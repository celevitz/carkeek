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

## Step 5. Change between quarters
    # Number of tests
      if (qNumberoftests < pqNumberoftests) {
        chNumberoftests <- paste0(pqNumberoftests-qNumberoftests
                                  ," fewer tests than")
      } else if (qNumberoftests > pqNumberoftests) {
        chNumberoftests <- paste0(qNumberoftests-pqNumberoftests
                                  ," more tests than")
      } else {
        chNumberoftests <- " the same number of tests as"
      }

    # Number of unique volunteers
      if (qNumVols < pqNumVols) {
        chNumberofVols<- paste0(pqNumVols-qNumVols
                                  ," fewer volunteers than")
      } else if (qNumVols > pqNumVols) {
        chNumberofVols <- paste0(qNumVols-pqNumVols
                                  ," more volunteers than")
      } else {
        chNumberofVols <- " the same number of volunteers as"
      }

    # Number of volunteer hours
      if (qVolTime < pqVolTime) {
        chVolHrs <- paste0(pqVolTime-qVolTime
                                ," fewer hours than")
      } else if (qVolTime > pqVolTime) {
        chVolHrs <- paste0(qVolTime-pqVolTime
                                 ," more hours than")
      } else {
        chVolHrs <- " the same number of hours as"
      }

    # Avg number of times volunteered
      if (qAveragetimesvolunteered < pqAveragetimesvolunteered) {
        chAvgTimes <- paste0(pqAveragetimesvolunteered-qAveragetimesvolunteered
                           ," fewer times on average")
      } else if (qAveragetimesvolunteered > pqAveragetimesvolunteered) {
        chAvgTimes <- paste0(qAveragetimesvolunteered-pqAveragetimesvolunteered
                           ," more times on average")
      } else {
        chAvgTimes <- " the same number of times on average"
      }

## Step 6. year to date





### Step 7. Writing
intro <- str_glue("These data are for {quarterNameLong}. The eight sites were tested
                  a total of {qNumberoftests} times by {qNumVols} unique
                  volunteers (an average of {qAveragetimesvolunteered} times
                  volunteered). Because many of the tests (
                  {qNumberoftestsinpairs} out of {qNumberoftests}) were done by
                  two people, this was about {qVolTime} hours of volunteer
                  effort. Volunteers completed {chNumberoftests} (and
                  volunteered {chVolHrs}) last quarter, which was
                  {pqNumberoftests} tests and an average of
                  {pqAveragetimesvolunteered} times volunteered. There were
                  {chNumberofVols} last quarter.")

# clean %>%
#   ggplot(aes(x=dateTested,y=averageDo)) +
#   theme_nothing() +
#   labs(title=str_wrap(intro,90)) +
#   theme(plot.title = element_text(family=ft)
#         ,plot.margin = margin(10,10,10,30))

### Step 8. Quarterly dashboard
## Step 8a. Header
header <- clean %>%
  ggplot(aes(x=dateTested,y=averageDo)) +
  theme_nothing() +
  geom_image(aes(x=1,y=1,image=logo),size=2.2) +
  theme(panel.grid = element_blank()
        ,panel.background = element_rect(color=main,fill=main)
        ,plot.background = element_rect(color=main,fill=main) )

## Header part 2
header2 <- clean %>%
  ggplot(aes(x=dateTested,y=averageDo)) +
  theme_nothing() +
  labs(title=quarterNameMid) +
  theme(panel.grid = element_blank()
        ,panel.background = element_rect(color=main,fill=main)
        ,plot.background = element_rect(color=main,fill=main)
        ,plot.title = element_text(color="white",family=ft,hjust=.5,face="bold"
                                   ,size = 32,margin=margin(30,0,0,0)))

## Table Column Names
tablecolumnnamesA <- quarter %>%
  ggplot(aes(x=1,y=1)) +
  geom_text(aes(x=0,y=1,family=ft),color=dark,size=textsize
            ,label=paste0("\n\n\n",quarterNameMid," values") ) +
  theme_nothing() +
  geom_hline(aes(yintercept=0),color=dark)

tablecolumnnamesB <- quarter %>%
  ggplot(aes(x=1,y=1)) +
  geom_text(aes(x=1,y=1,family=ft),color=dark,size=textsize
            ,label=paste0("\n\nCompared to\n",previousquarterMid) ) +
  theme_nothing() +
  geom_hline(aes(yintercept=0),color=dark)

tablecolumnnames <- ggarrange(tablecolumnnamesA,tablecolumnnamesB,
                              nrow=1,ncol=2,widths=c(2,1))

## Step 8b. function for each measure
currentValue <- str_glue("{qNumberoftests}")
previousValue <- str_glue("{pqNumberoftests}")
mainlabel <- str_glue("tests")
positivechangelabel <- paste0("+",qNumberoftests-pqNumberoftests)
negativechangelabel <- paste0("-",pqNumberoftests-qNumberoftests)

  graphFunction <- function(currentValue,previousValue,mainlabel
                            ,positivechangelabel
                            ,negativechangelabel) {

    graphA <- quarter %>%
      ggplot(aes(x=1,y=1)) +
      geom_text(aes(x=1,y=1,family=ft),color=dark,size=titlesize
                ,label=currentValue ) +
      theme_nothing()

    graphB <- quarter %>%
      ggplot(aes(x=1,y=1)) +
      geom_text(aes(x=1,y=1,family=ft),color=dark,size=subtitlesize
                ,label=mainlabel) +
      theme_nothing()

    graphC <- previousq %>%
      ggplot(aes(x=1,y=1)) +
      theme_nothing() +
      if (currentValue < previousValue) {
          geom_point(aes(x=1,y=1),pch=25,color=mid,fill=mid,size=10)
      } else if (currentValue > previousValue) {
          geom_point(aes(x=1,y=1),pch=24,color=main,fill=main,size=10)
      } else {
          geom_point(aes(x=1,y=1),pch=20,color=mid2,fill=mid2,size=10)
      }

    graphD <- previousq %>%
      ggplot(aes(x=1,y=1)) +
      theme_nothing() +
      if (currentValue < previousValue) {
        geom_text(aes(x=1,y=1,family=ft),color=mid,size=textsize
                  ,label=negativechangelabel)
      } else if (currentValue > previousValue) {
        geom_text(aes(x=1,y=1,family=ft),color=main,size=textsize
                  ,label=positivechangelabel)
      } else {
        geom_text(aes(x=1,y=1,family=ft),color=mid2,size=textsize
                  ,label="+0")
      }

    ggarrange(graphA,graphB
              ,ggarrange(graphC,graphD,nrow=1,ncol=2)
              ,nrow=1,ncol=3)

  }

## Step 8c. Times tested
  timestested <- graphFunction(str_glue("{qNumberoftests}")
                               ,str_glue("{pqNumberoftests}")
                               ,str_glue("tests")
                            ,paste0("+",qNumberoftests-pqNumberoftests
                                    ," since last quarter")
                            ,paste0("-",pqNumberoftests-qNumberoftests
                                    ," since last quarter"))

## Step 8d. Number of tests with two people
  testswith2graph <- graphFunction(
    str_glue("{round(qNumberoftestsinpairs/qNumberoftests*100 ,1)}%")
    ,str_glue("{round(pqNumberoftestsinpairs/pqNumberoftests*100,1)}%")
    ,str_glue("done with\na partner")
    ,paste0("+",round((qNumberoftestsinpairs/qNumberoftests-
                         pqNumberoftestsinpairs/pqNumberoftests)*100,1),"%"
            ," since last quarter")
    ,paste0("-",round((pqNumberoftestsinpairs/pqNumberoftests-
                         qNumberoftestsinpairs/qNumberoftests)*100,1),"%"
            ," since last quarter")
  )

## Step 8e. Number of volunteers
  numvolsgraph <- graphFunction(
    str_glue("{qNumVols}")
    ,str_glue("{pqNumVols}")
    ,str_glue("unique\nvolunteers")
    ,paste0("+",qNumVols-pqNumVols," since last quarter")
    ,paste0("-",pqNumVols-qNumVols," since last quarter")
  )

## Step 8f. Number of hours by volunteers this quarters
  numHrsGraph <- graphFunction(
    str_glue("{qVolTime}")
    ,str_glue("{pqVolTime}")
    ,str_glue("hours")
    ,paste0("+",qVolTime-pqVolTime," since last quarter")
    ,paste0("-",pqVolTime-qVolTime," since last quarter")
  )

## Step 8g. Bring together
pdf(file = paste(directory,"CarkeekWatershedTesting",quarterName,".pdf",sep="")
    ,paper="letter",width=8,height=11)

ggarrange(ggarrange(header,header2,ncol=2,nrow=2,widths=c(2,1))
        #,tablecolumnnames
        ,timestested
        ,testswith2graph
        ,numvolsgraph
        ,numHrsGraph
        #,nrow=6,ncol=1
        ,nrow=5,ncol=1
)
dev.off()

