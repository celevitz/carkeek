## Carly Levitz
## Written: June 24, 2023
## Modified: June 24, 2023
## Purpose: quarterly report.

## Step 1: set things up
rm(list=ls())

# Change this each quarter
  quarterName <- "2024Q2"
  quarterNameMid <- "Q2 2024"
  quarterNameLong <- "Q2 of 2024"
  previousquarterMid <- "Q1 2024"
  quartermin <- "2024-03-01"
  quartermax <- "2024-06-30"
  previousquartermin <- "2024-01-01"
  previousquartermax <- "2024-03-31"
  yeartodatemin <- "2024-01-01"
  yeartodatemax <- "2024-05-06"
  lastyearmin <- "2023-04-01"
  lastyearmax <- "2024-06-30"

library(ggplot2)
library(dplyr)
library(tidyr)
library(openxlsx)
library(rmarkdown)
library(sysfonts)
library(ggtext)
library(stringr)
library(ggpubr)
library(ggimage)
library(showtext)

font_add_google("Barlow", "bar")
showtext_auto()
ft <- "bar"

directory <- "/Users/carlylevitz/Documents/Data/carkeek/"
setwd(directory)

dark <- "#4b4f42"
mid2 <- "#6b625c"
mid <- "#9c8b94"
main <- "#7e7b3e"
light <- "#f0e0cc"
highlight <- "#46bdc6"

titlesize <- 20
subtitlesize <- 12
textsize <- 8
smalltextsize <- 5

logo <- paste(directory,"CWCAPlogo-1-white-1024x346.png",sep="")

## Step 2: Bring in data and clean it
clean <- read.csv(paste0(directory,"H20Data.csv"),stringsAsFactors = FALSE)

  clean <- clean %>%
  # as-of 2/2/24, drop the 2024 data
  #  filter(year != 2024) %>%
  # clean the date data
  mutate(dateTested = as.Date(dateTested)
  # Categorize the data into quarters
        ,quarter = case_when(quartermin <= dateTested & dateTested <= quartermax ~ "Current quarter"
                             ,previousquartermin <= dateTested & dateTested <=previousquartermax ~ "Previous quarter"
                             ,lastyearmin <= dateTested & dateTested <=lastyearmax ~ "Last year same quarter"
                             ,TRUE ~ "z")
        ,yeartodate = case_when(yeartodatemin <= dateTested & dateTested <= yeartodatemax ~ "include"
                                ,TRUE ~ "exclude")
  # were there one or two people volunteering?
        ,numberofvolunteers = case_when(is.na(tester2) ~ 1
                                        ,TRUE ~ 2))

## Step 3. Key metrics for the quarter:
quarter <- clean %>%
  filter(quarter == "Current quarter")

  # Testing information
    # Number of tests
    qNumberoftests <- nrow(quarter)

    # Number of tests with two people
    qNumberoftestsinpairs <- nrow(quarter[quarter$numberofvolunteers == 2,])

  # Volunteer information
    # Number of unique volunteers
    qNumVols <- length(unique(c(unique(quarter$tester1[!is.na(quarter$tester1)])
                                ,unique(quarter$tester2[!is.na(quarter$tester2)]))))

    # Number of volunteer hours
    qVolTime <- sum(quarter$numberofvolunteers)*2

    # Number of times each of them volunteered
    qAveragetimesvolunteered <- round(qNumberoftests/qNumVols,1)

## Step 4. previous quarter
previousq <- clean %>%
  filter(quarter == "Previous quarter")

  # Testing information
    # Number of tests
    pqNumberoftests <- nrow(previousq)

    # Number of tests with two people
    pqNumberoftestsinpairs <- nrow(previousq[previousq$numberofvolunteers==2,])

  # Volunteer information
    # Number of unique volunteers
    pqNumVols <- length(unique(c(unique(previousq$tester1[!is.na(previousq$tester1)])
                               ,unique(previousq$tester2[!is.na(previousq$tester2)]))))

    # Number of volunteer hours
    pqVolTime <- sum(previousq$numberofvolunteers)*2

    # Number of times each of them volunteered
    pqAveragetimesvolunteered <- round(pqNumberoftests/pqNumVols,1)


## Step 5. year to date
ytddata <- clean %>%
  filter(yeartodate == "include")

  # Testing information
    # Number of tests
    ytdNumberoftests <- nrow(ytddata)

    # Number of tests with two people
    ytdNumberoftestsinpairs <- nrow(ytddata[ytddata$numberofvolunteers==2,])

    # Volunteer information
    # Number of unique volunteers
    ytdNumVols <- length(unique(c(unique(ytddata$tester1[!is.na(ytddata$tester1)])
                               ,unique(ytddata$tester2[!is.na(ytddata$tester2)]))))

    # Number of volunteer hours
    ytdVolTime <- sum(ytddata$numberofvolunteers)*2

    # Number of times each of them volunteered
    ytdAveragetimesvolunteered <- round(ytdNumberoftests/ytdNumVols,1)

## Step 6. Create dataframe of this information
  db <- as.data.frame(matrix(c(c("Tests",qNumberoftests,pqNumberoftests
                                 ,ytdNumberoftests,5)
                         ,c("% of tests done in pairs",round(
                            qNumberoftestsinpairs/qNumberoftests*100,1)
                            ,round(pqNumberoftestsinpairs/pqNumberoftests*100,1)
                            ,round(ytdNumberoftestsinpairs/ytdNumberoftests*100
                                   ,1)
                            ,4)
                         ,c("Unique volunteers",qNumVols,pqNumVols
                            ,ytdNumVols,3)
                         ,c("Volunteer hours",qVolTime,pqVolTime,ytdVolTime,2)
                         ,c("Average times volunteered"
                            ,qAveragetimesvolunteered,pqAveragetimesvolunteered
                            ,ytdAveragetimesvolunteered,1))
                       ,ncol=5,byrow = TRUE
                       ,dimnames=list(NULL
                                  ,c("measure","current","previous","ytd"
                                     ,"y"))))

  db <- db %>%
    mutate(current = as.numeric(current)
           ,previous = as.numeric(previous)
           ,y = as.numeric(y)
          ,quarterlyChange = current-previous
          ,directionofchange = case_when(quarterlyChange < 0 ~ "negative"
                                         ,quarterlyChange == 0 ~ "no change"
                                         ,quarterlyChange > 0 ~ "positive"
                                         ,is.na(quarterlyChange) ~ "no change") )

### Step 8. Write a description
  # How many times was each site tested?
    numberoftestsbysite <- quarter %>%
      group_by(siteNumber) %>%
      summarise(n=n())

    twotestsinquarter <-
      paste("Site",numberoftestsbysite$siteNumber[numberoftestsbysite$n == 2]
            ,sep=" ")
    numberofonetests <- nrow(numberoftestsbysite[numberoftestsbysite$n == 1,])
      if (numberofonetests == 1) { onetestsPlural <- "1 site was"} else {
        onetestsPlural <- paste(numberofonetests,"sites were",sep=" ")
      }
    numberoftwotests <- nrow(numberoftestsbysite[numberoftestsbysite$n == 2,])
      if (numberoftwotests == 1) { twotestsPlural <- "1 site was"} else {
        twotestsPlural <- paste(numberoftwotests,"sites were",sep=" ")
      }
    numberofthreetests <- nrow(numberoftestsbysite[numberoftestsbysite$n == 3,])
      if (numberofthreetests == 1) { threetestsPlural <- "1 site was"} else {
        threetestsPlural <- paste(numberofthreetests,"sites were",sep=" ")
      }
    numberoffourtests <- nrow(numberoftestsbysite[numberoftestsbysite$n == 4,])
      if (numberoffourtests == 1) { fourtestsPlural <- "1 site was"} else {
        fourtestsPlural <- paste(numberoffourtests,"sites were",sep=" ")
      }
    numberoffivetests <- nrow(numberoftestsbysite[numberoftestsbysite$n > 4,])
      if (numberoffivetests == 1) { fivetestsPlural <- "1 site was"} else {
        fivetestsPlural <- paste(numberoffivetests,"sites were",sep=" ")
      }

  # How many times were volunteers unable to get data?
    if (nrow(quarter[is.na(quarter$pH),]) == 1 ) { missingpH <- "1 time"} else{
      missingpH <- paste(nrow(quarter[is.na(quarter$pH),])," times",sep=" ")
    }
    if (nrow(quarter[is.na(quarter$averageDo),]) == 1 ) {
      missingdo <- "1 time"} else {
      missingdo <- paste(nrow(quarter[is.na(quarter$averageDo),])," times"
                         ,sep=" ")
    }
    if (nrow(quarter[is.na(quarter$OxSat),]) == 1 ) {
      missingoxsat <- "1 time"} else{
      missingoxsat <- paste(nrow(quarter[is.na(quarter$OxSat),])," times"
                            ,sep=" ")
    }
    if (nrow(quarter[is.na(quarter$totalAlk),]) == 1 ) {
      missingAlk <- "1 time"} else{
      missingAlk <- paste(nrow(quarter[is.na(quarter$totalAlk),])," times"
                          ,sep=" ")
    }
    if (nrow(quarter[is.na(quarter$totalHardness),]) == 1 ) {
      missingHard <- "1 time"} else{
      missingHard <- paste(nrow(quarter[is.na(quarter$totalHardness),])," times"
                           ,sep=" ")
    }
    if (nrow(quarter[is.na(quarter$turbidityJTU),]) == 1 ) {
      missingTurb <- "1 time"} else{
      missingTurb <- paste(nrow(quarter[is.na(quarter$turbidityJTU),])," times"
                           ,sep=" ")
    }
    if (nrow(quarter[is.na(quarter$avgEcoli),]) == 1 ) {
      missingEcoli <- "1 time"} else{
      missingEcoli <- paste(nrow(quarter[is.na(quarter$avgEcoli),])," times"
                          ,sep=" ")
    }

  description <- str_wrap(str_glue("Out of the {nrow(quarter)} tests this
                                   quarter, data were missing for the following
                                   measures: pH was missing {missingpH},
                                   dissolved oxygen was missing {missingdo},
                                   oxygen saturation was missing {missingoxsat},
                                   alkalinity was missing {missingAlk},
                                   hardness was missing {missingHard},
                                   turbidity was missing {missingTurb}, and
                                   E Coli was missing {missingEcoli}."),105)
  missingdata <- data.frame(measure = c("pH","Dissolved oxygen"
                                        ,"Oxygen saturation","Alkalinity"
                                        ,"Hardness","Turbidity","E Coli")
                            ,value = c(missingpH,missingdo,missingoxsat
                                       ,missingAlk,missingHard,missingTurb
                                       ,missingEcoli))
  missingdata$y <- as.numeric(row.names(missingdata))

  ## Add the description to the dataframe in case it makes it print more
  ## nicely than simple using geom_text
    db <- db %>%
      mutate(current = as.character(current)
             ,previous = as.character(previous)
             ,y = as.character(y)
             ) %>%
      bind_rows( as.data.frame(matrix(c("description",description,NA,NA,-3)
                    ,ncol=5,byrow = TRUE
                    ,dimnames=list(NULL,c("measure","current","previous"
                                          ,"ytd","y"))))) %>%
      mutate(previous = as.numeric(previous)
             ,y = as.numeric(y))



### Step 9. Graph
  dashboard <-
  db %>%
    ggplot(aes(x=0,y=y)) +
    geom_text(data=db %>% filter(measure != "description")
              ,aes(x=0,y=y,label = measure),hjust=0,family = ft,color=dark) +
    # numbers
      geom_text(data=db %>% filter(measure != "% of tests done in pairs" &
                                     measure != "description")
                ,aes(x=20,y=y,label = current),hjust=0,family = ft,color=dark) +
      geom_text(data=db %>% filter(measure != "% of tests done in pairs" &
                                     measure != "description")
                ,aes(x=30,y=y,label = previous),hjust=0,family = ft
                ,color=dark) +
      geom_text(data=db %>% filter(measure != "% of tests done in pairs" &
                                     measure != "description")
                ,aes(x=41,y=y,label = quarterlyChange),hjust=0,family = ft
                ,color=dark) +
      geom_text(data=db %>% filter(measure != "% of tests done in pairs" &
                                     measure != "description")
                ,aes(x=51,y=y,label = ytd),hjust=0,family = ft,color=dark) +
    # percents
      geom_text(data=db %>% filter(measure == "% of tests done in pairs")
                ,aes(x=20,y=y,label = paste0(current,"%")),hjust=0
                ,family = ft,color=dark) +
      geom_text(data=db %>% filter(measure == "% of tests done in pairs")
                ,aes(x=30,y=y,label = paste0(previous,"%")),hjust=0
                ,family = ft,color=dark) +
      geom_text(data=db %>% filter(measure == "% of tests done in pairs")
                ,aes(x=41,y=y,label = paste0(quarterlyChange,"%")),hjust=0
                ,family = ft,color=dark) +
      geom_text(data=db %>% filter(measure == "% of tests done in pairs")
                ,aes(x=51,y=y,label = paste0(ytd,"%")),hjust=0
                ,family = ft,color=dark) +
    # instead of having x axis, plot the x axis as text
    # this way, I can plot other things ABOVE the x axis labels
    geom_text(aes(x=c(0,20,30,40,50,60),y = c(rep(6,6)))
                  ,label = c("Measure","Current\nquarter","Previous\nquarter"
                              ,"Change since\nlast quarter","Year-to-date","")
              ,family = ft, size = 4, color = dark,hjust=0) +
    coord_cartesian(clip = "off") +
    # then add the logo and title
    scale_y_continuous(lim=c(-10,10))+
    geom_rect(aes(xmin=0,xmax=60,ymin=7,ymax=10),color = main,fill = main) +
    geom_image(aes(x=30,y=9,image=logo),size=.2) +
    geom_text(aes(x=30,y=7.5),hjust=.5,size=8,color = "white",family = ft
              ,label=paste0("Quarterly report for ",quarterNameMid)) +
    # other graph stuff
    # instead of using geom_vline, I'm going to just make a line that only
    # is within the table
    geom_rect(aes(xmin=49,xmax=49,ymin=.8,ymax=6.2),color=dark,fill=dark)+
    geom_rect(aes(xmin=0,xmax=60,ymin=5.5,ymax=5.5),color=dark,fill=dark)+
    geom_point(data = db %>% filter(measure != "description")
               ,aes(x=40,y=y,shape = directionofchange,color=directionofchange
                   ,fill = directionofchange),size=3) +
    labs(caption = "For more details, please contact troy.beckner@gmail.com") +
    scale_shape_manual(values = c("negative"=25,"no change"=20,"positive"=24)) +
    scale_color_manual(values=c("negative"=mid,"no change"=mid2,"positive"=main)) +
    scale_fill_manual(values=c("negative"=mid,"no change"=mid2,"positive"=main)) +
    # Add information about # of times each site was tested this quarter
    geom_text(data=numberoftestsbysite
              ,aes(x=10,y=-siteNumber-1,label=n)
              ,family = ft, size = 4, color = dark) +
    geom_text(data=numberoftestsbysite
              ,aes(x=0,y=-siteNumber-1,label=paste0("Site ",siteNumber))
              ,family = ft, size = 4, color = dark,hjust=0) +
    geom_text(aes(x=0,y=-1,family = ft, size = 4, color = dark,hjust=0
                  ,label="Number of times each site was tested"))+
    # Add information about when data were missing
    geom_text(data=missingdata
              ,aes(x=50,y=-y-2,label=value)
              ,family = ft, size = 4, color = dark,hjust=0) +
    geom_text(data=missingdata
              ,aes(x=30,y=-y-2,label=measure)
              ,family = ft, size = 4, color = dark,hjust=0) +
    geom_text(aes(x=30,y=-2,family = ft, size = 4, color = dark,hjust=0
                  ,label="Number of times data were missing by measure"))+
    # Theme
    theme_minimal()+
    theme(panel.grid = element_blank()
          ,axis.title = element_blank()
          ,axis.text = element_blank()
          ,legend.position = "none"
          ,plot.caption = element_text(family = ft,hjust=0,color=dark))

## Step 10. Bring together
pdf(file = paste(directory,"CarkeekWatershedTesting",quarterName,".pdf",sep="")
    ,paper="letter",width=8,height=11)

  dashboard

dev.off()

