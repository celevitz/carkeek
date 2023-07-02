## Carly Levitz
## Written: June 24, 2023
## Modified: June 24, 2023
## Purpose: quarterly report.

## Step 1: set things up
rm(list=ls())

# Change this each quarter
  quarterName <- "2023Q2"
  quarterNameMid <- "Q2 2023"
  quarterNameLong <- "Q2 of 2023"
  previousquarterMid <- "Q1 2023"
  quartermin <- "2023-04-01"
  quartermax <- "2023-06-30"
  previousquartermin <- "2023-01-01"
  previousquartermax <- "2023-03-31"
  yeartodatemin <- "2022-07-01"
  yeartodatemax <- "2023-06-30"
  lastyearmin <- "2022-04-01"
  lastyearmax <- "2022-06-30"

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
    qNumVols <- length(unique(unique(quarter$tester1),unique(quarter$tester2)))

    # Number of volunteer hours
    qVolTime <- sum(quarter$numberofvolunteers)*2

    # Number of times each of them volunteered
    qAveragetimesvolunteered <- qNumberoftests/qNumVols

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
    pqNumVols <- length(unique(unique(previousq$tester1)
                               ,unique(previousq$tester2)))

    # Number of volunteer hours
    pqVolTime <- sum(previousq$numberofvolunteers)*2

    # Number of times each of them volunteered
    pqAveragetimesvolunteered <- pqNumberoftests/pqNumVols


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
    ytdNumVols <- length(unique(unique(ytddata$tester1)
                               ,unique(ytddata$tester2)))

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
                                         ,quarterlyChange > 0 ~ "positive"))

### Step 7. Graph
  dashboard <-
  db %>%
    ggplot(aes(x=0,y=y)) +
    geom_text(aes(x=0,y=y,label = measure),hjust=0,family = ft,color=dark) +
    # numbers
      geom_text(data=db %>% filter(measure != "% of tests done in pairs")
                ,aes(x=20,y=y,label = current),hjust=0,family = ft,color=dark) +
      geom_text(data=db %>% filter(measure != "% of tests done in pairs")
                ,aes(x=30,y=y,label = previous),hjust=0,family = ft
                ,color=dark) +
      geom_text(data=db %>% filter(measure != "% of tests done in pairs")
                ,aes(x=41,y=y,label = quarterlyChange),hjust=0,family = ft
                ,color=dark) +
      geom_text(data=db %>% filter(measure != "% of tests done in pairs")
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
    geom_text(aes(x=c(0,20,30,40,50),y = rep(6,5)
                  ,label = c("Measure","Current\nquarter","Previous\nquarter"
                              ,"Change since\nlast quarter","Year-to-date") )
              ,family = ft, size = 4, color = dark,hjust=0) +
    coord_cartesian(clip = "off") +
    # then add the logo and title
    scale_y_continuous(lim=c(0,10))+
    geom_rect(aes(xmin=0,xmax=60,ymin=7,ymax=10),color = main,fill = main) +
    geom_image(aes(x=30,y=9,image=logo),size=.5) +
    geom_text(aes(x=30,y=7.5),hjust=.5,size=8,color = "white",family = ft
              ,label=paste0("Quarterly report for ",quarterNameMid)) +
    # other graph stuff
    # instead of using geom_vline, I'm going to just make a line that only
    # is within the table
    geom_rect(aes(xmin=49,xmax=49,ymin=.8,ymax=6.2),color=dark,fill=dark)+
    geom_rect(aes(xmin=0,xmax=60,ymin=5.6,ymax=5.6),color=dark,fill=dark)+
    geom_point(aes(x=40,y=y,shape = directionofchange,color=directionofchange
                   ,fill = directionofchange),size=3) +
    labs(caption = "For more details, please contact troy.beckner@gmail.com") +
    scale_shape_manual(values = c(25,20,24)) +
    scale_color_manual(values=c(mid,mid2,main)) +
    scale_fill_manual(values=c(mid,mid2,main)) +
    theme_minimal()+
    theme(panel.grid = element_blank()
          ,axis.title = element_blank()
          ,axis.text = element_blank()
          ,legend.position = "none"
          ,plot.caption = element_text(family = ft,hjust=0,color=dark))

## Step 8g. Bring together
pdf(file = paste(directory,"CarkeekWatershedTesting",quarterName,".pdf",sep="")
    ,paper="letter",width=8,height=11)

  dashboard

dev.off()

