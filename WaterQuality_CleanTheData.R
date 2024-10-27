## Author: Carly Levitz
## Written: 2023-06-20
## Updated: 2024-10-27
## Purpose: clean the data

library(tidyr)
library(openxlsx)
library(tidyverse)

## Step 1: set things up
rm(list=ls())

directory <- "/Users/carlylevitz/Documents/Data/carkeek/"
setwd(directory)

importdataname <- "H20data_2024-10-27"
exportdataname  <- "H20Data"

## Step 2: Bring in data and clean it
### Step 2a. Bring in data
rawdata <- as_tibble(read.xlsx(paste(directory,importdataname,".xlsx"
                                     ,sep=""),sheet=2,startRow = 2)) %>%
  # drop the fake data
  filter(`Tester.#1` != "John Doe")

### Step 2b. Clean data
clean <- rawdata %>%
  # Fix date error: 12/13/2020 should be 12/13/2022
  mutate(Date.Tested = ifelse(Date.Tested == 44178 & !(is.na(Date.Tested))
                              ,44908,Date.Tested)
         # Fix the date and time
         ,Date.Tested = as.Date(as.numeric(Date.Tested), origin = "1899-12-30")
         # remove trailing spaces, extra quotation marks
         ,`Tester.#1` = trimws(gsub("\\\"","",`Tester.#1`),"both")
         ,`Tester.#2` = trimws(gsub("\\\"","",`Tester.#2`),"both")
         # combine testers that are the same
         ,`Tester.#1` = case_when(`Tester.#1` %in% c("Alice","ALice") ~
                                    "Alice Cottrell-Steen"
                                  ,`Tester.#1` %in% c("Sue") ~ "Sue Cottrell"
                                  ,`Tester.#1` %in% c("Natale") ~ "Natalie"
                                  ,`Tester.#1` %in% c("mike") ~ "Mike"
                                  ,`Tester.#1` %in% c("sean") ~ "Sean"
                                  ,TRUE ~ `Tester.#1`)
         ,`Tester.#2` = case_when(`Tester.#2` %in% c("Alice") ~
                                    "Alice Cottrell-Steen"
                                  ,`Tester.#2` %in% c("-  ","-") ~ NA
                                  ,`Tester.#2` %in% c("racheal") ~ "Racheal"
                                  ,`Tester.#2` %in% c("mike") ~ "Mike"
                                  ,`Tester.#2` %in% c("Sue") ~ "Sue Cottrell"
                                  ,TRUE ~ `Tester.#2`)
         # fix waterbody names
         ,Waterbody = ifelse(Waterbody == "venema","Venema",Waterbody)
         # If Turbidity is greater than 240, plot it at 241
         ,`Turbidity.(JTU)` = case_when(`Turbidity.(JTU)` %in% ">240 NCU"~"241"
                                        ,TRUE ~ trimws(`Turbidity.(JTU)`))
         # If Coliform is too numerous to count (TNTC), plot it at 350
         ,Coliform.1 = case_when(Coliform.1 %in% c("TNTC","TNTC ")~"350"
                                 ,Coliform.1 %in% c("-","n/a") ~NA
                                 ,TRUE ~ Coliform.1)
         ,Coliform.2 = case_when(Coliform.2 %in% c("TNTC","TNTC ")~"350"
                                 ,Coliform.2 %in% c("-","n/a") ~NA
                                 ,TRUE ~ Coliform.2)
         ,Coliform.3 = case_when(Coliform.3 %in% c("TNTC","TNTC ")~"350"
                                 ,Coliform.3 %in% c("-","n/a") ~NA
                                 ,TRUE ~ Coliform.3)
  )

# change characters variables that should be numeric, to numeric
for (charvar in c("Average.DO","%.Ox..Sat.","Total.ALK","Total.Hardness"
                  ,"Turbidity.(JTU)","E..coli.3","Avg..E..coli"
                  ,"Coliform.1","Coliform.2","Coliform.3","Avg..Coliform")) {
  print(charvar)
  clean[,charvar] <- as.numeric(unlist(clean[,charvar]))
}

## Data checks:
  # Is there a site number?
    clean[is.na(clean$`Site.#`),]

  # Is there a tester?
    clean[is.na(clean$`Tester.#1`),]

  # is the date appropriate?
    clean$year2 <- substr(clean$Date.Tested,1,4)
    table(clean$year2)
    table(clean$Year==clean$year2)
    clean$Year <- clean$year2
    clean$year2 <- NULL

  # DELETE later - 2023-11-09 testing by Mary and Sylvie should be site 8
  # Commented out 10/27/24 after confirming the data got updated
    # clean$`Site.#`[clean$`Tester.#1` == "Mary" & clean$Date.Tested == "2023-11-09"] <- 8
    # clean$`Site.#`[clean$`Tester.#1` == "Sean" & clean$Date.Tested == "2023-12-23"] <- 6

## Create an ID for each test time
clean <- clean %>%
  mutate(siteDateId = paste0("site",`Site.#`,"_",Date.Tested))

## rename the variables
names(clean) <- c("siteNumber","tester1","tester2","dateTested","month","year"
                  ,"timeTested","volunteerHours","watershed"
                  ,"waterbody","waterbodyCondition","weatherConditions"
                  ,"airTemp","waterTemp","pH","do1","do2","averageDo","OxSat"
                  ,"dropAlk","totalAlk","dropHardness","totalHardness"
                  ,"turbidityJTU","secchiDepth","ecoli1","ecoli2","ecoli3"
                  ,"avgEcoli","coliform1","coliform2","coliform3","avgColiform"
                  ,"publicInteractions","comments","siteDateId")

## Step 4. Export data
write.csv(clean,paste0(directory,exportdataname,".csv"),row.names=FALSE)


# Create quarterly & annual dataset
aggregated <- clean %>%
  mutate(quarter = case_when(month %in% c(1,2,3) ~ "Q1"
                              ,month %in% c(4,5,6) ~ "Q2"
                              ,month %in% c(7,8,9) ~ "Q3"
                              ,month %in% c(10,11,12) ~ "Q4"))

  # Number of tests
  numbertests <- aggregated %>%
    group_by(year,quarter,month) %>%
    summarise(Month = n()) %>%
    left_join(
      aggregated %>% group_by(year,quarter) %>%
        summarise(Quarter = n())
    ) %>%
    left_join(
      aggregated %>% group_by(year) %>%
        summarise(Annual = n())
    ) %>%
    pivot_longer(!c(year,quarter,month),names_to="time",values_to="numberoftests")

  # Number of volunteers
  numberofuniquevolunteers <- aggregated %>%
    select(year,tester1,tester2) %>%
    pivot_longer(!c(year),names_to="tester") %>%
    filter(!(is.na(value))) %>%
    select(!tester) %>%
    distinct() %>%
    group_by(year) %>%
    summarise(Annual = n()) %>%
    # quarterly
    left_join(aggregated %>%
                select(year,quarter,tester1,tester2) %>%
                pivot_longer(!c(year,quarter),names_to="tester") %>%
                filter(!(is.na(value))) %>%
                select(!tester) %>%
                distinct() %>%
                group_by(year,quarter) %>%
                summarise(Quarter = n())) %>%
    # monthly
    right_join(aggregated %>%
                select(year,quarter,month,tester1,tester2) %>%
                pivot_longer(!c(year,month,quarter),names_to="tester") %>%
                filter(!(is.na(value))) %>%
                select(!tester) %>%
                distinct() %>%
                group_by(year,quarter,month) %>%
                summarise(Month = n())) %>%
    pivot_longer(!c(year,quarter,month),names_to="time",values_to="uniquevolunteers")

  # Number completed by a pair
  completedbypair <- aggregated %>%
    mutate(numberoftesters = ifelse(is.na(tester2),0,1) ) %>%
    group_by(year,quarter) %>%
    summarise(Quarter = sum(numberoftesters)) %>%
    ungroup() %>% group_by(year) %>%
    mutate(Annual = sum(Quarter)) %>%
    # monthly
    right_join(aggregated %>%
                 mutate(numberoftesters = ifelse(is.na(tester2),0,1) ) %>%
                 group_by(year,quarter,month) %>%
                 summarise(Month = sum(numberoftesters)) ) %>%
    pivot_longer(!c(year,quarter,month),names_to="time",values_to="completedbypair")

  PeopleHours <- aggregated %>%
    mutate(numberoftesters = ifelse(is.na(tester2),1,2)
           ,peoplehours = numberoftesters*2)  %>%
    group_by(year,quarter) %>%
    summarise(Quarter = sum(peoplehours)) %>%
    ungroup() %>% group_by(year) %>%
    mutate(Annual = sum(Quarter)) %>%
    # monthly
    right_join(aggregated %>%
                 mutate(numberoftesters = ifelse(is.na(tester2),1,2)
                        ,peoplehours = numberoftesters*2)  %>%
                 group_by(year,quarter,month) %>%
                 summarise(Month = sum(peoplehours))) %>%
    pivot_longer(!c(year,quarter,month),names_to="time",values_to="peoplehours")

aggregatedataset <- numbertests %>%
  full_join(numberofuniquevolunteers) %>%
  full_join(completedbypair) %>%
  full_join(PeopleHours)

write.csv(aggregatedataset,paste0(directory,exportdataname,"_aggregated.csv"),row.names=FALSE)
