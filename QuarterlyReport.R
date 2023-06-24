## Carly Levitz
## Written: June 24, 2023
## Modified: June 24, 2023
## Purpose: quarterly report.

## Step 1: set things up
rm(list=ls())

# Change this each quarter
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

directory <- "/Users/carlylevitz/Documents/Data/carkeek/"
setwd(directory)

dark <- "#4b4f42"
mid2 <- "#6b625c"
mid <- "#9c8b94"
main <- "#7e7b3e"
light <- "#f0e0cc"
highlight <- "#46bdc6"

logo <- paste(directory,"CWCAPlogo-1-white-1024x346.png",sep="")

## Step 2: Bring in data and clean it
clean <- read.csv(paste0(directory,"H20Data.csv"),stringsAsFactors = FALSE)
clean$dateTested <-

  clean <- clean %>%
  # clean the date data
  mutate(dateTested = as.Date(clean$dateTested)
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

  # Volunteer information
    # Number of unique volunteers
    pqNumVols <- length(unique(unique(previousq$tester1)
                               ,unique(previousq$tester2)))

    # Number of volunteer hours
    pqVolTime <- sum(previousq$numberofvolunteers)*2

    # Number of times each of them volunteered
    pqAveragetimesvolunteered <- pqNumberoftests/pqNumVols

  # Change between quarters
    # Number of tests
      if (qNumberoftests < pqNumberoftests) {
        chNumberoftests <- paste0(pqNumberoftests-qNumberoftests
                                  ," fewer tests than")
      } else if (qNumberoftests > pqNumberoftests) {
        chNumberoftests <- paste0(qNumberoftests-pqNumberoftests
                                  ," more tests than")
      } else {
        chNumberoftests <- " the same number of tests as "
      }

    # Number of unique volunteers
      if (qNumVols < pqNumVols) {
        chNumberofVols<- paste0(pqNumVols-qNumVols
                                  ," fewer volunteers than")
      } else if (qNumVols > pqNumVols) {
        chNumberofVols <- paste0(qNumVols-pqNumVols
                                  ," more volunteers than")
      } else {
        chNumberofVols <- " the same number of volunteers as "
      }

    # Number of volunteer hours
      if (qVolTime < pqVolTime) {
        chVolHrs <- paste0(pqVolTime-qVolTime
                                ," fewer hours than")
      } else if (qVolTime > pqVolTime) {
        chVolHrs <- paste0(qVolTime-pqVolTime
                                 ," more hours than")
      } else {
        chVolHrs <- " the same number of hours as "
      }

    # Avg number of times volunteered
      if (qAveragetimesvolunteered < pqAveragetimesvolunteered) {
        chVolHrs <- paste0(pqAveragetimesvolunteered-qAveragetimesvolunteered
                           ," fewer times on average than")
      } else if (qAveragetimesvolunteered > pqAveragetimesvolunteered) {
        chVolHrs <- paste0(qAveragetimesvolunteered-pqAveragetimesvolunteered
                           ," more times on average than")
      } else {
        chVolHrs <- " the same number of times on average as "
      }

## Step 5. year to date





### Writing




