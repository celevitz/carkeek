## Salmon Survey data
## Date written: 2023-11-11
## Date updated: 2023-11-11
## By: Carly Levitz
## Combine all years of salmon survey data

library(tidyr)
library(openxlsx)
library(tidyverse)

## Step 1: set things up
rm(list=ls())

directory <- "/Users/carlylevitz/Documents/Data/carkeek/salmon/"
setwd(directory)

data2019 <- read.csv("salmon-survey-2019-csv/form-2__fish.csv")
data2020 <- read.csv("salmon-survey-2020-csv/form-2__fish.csv")
data2021 <- read.csv("salmon-survey-2021-csv/form-2__entry.csv")
data2022 <- read.csv("salmon-survey-2022-csv/form-2__entry.csv")
data2023 <- read.xlsx("CWCAP_Salmon_2023_2023-11-11.xlsx",sheet=1) %>%
  rename(Life_Stage=Life.Stage
         ,Width_Inches=`Width.(inches)`
         ,Length_Inches = `Length.(inches)`
         ,Adipose_Fin = Adipose.Fin
         ,Carcass_Age = Carcass.Age
         ,Spawning_Success = Spawned
         ,long_Location= `_Location_longitude`
         ,lat_Location = `_Location_latitude`
         ,Activity = Redd.Status) %>%
  mutate(Distance = as.numeric(Distance)
         ,lat_Location = as.numeric(lat_Location)
         ,long_Location = as.numeric(long_Location)
         ,Length_Inches = as.numeric(Length_Inches)
         ,Width_Inches = as.numeric(Width_Inches))

data <- data2019 %>%
  bind_rows(data2020) %>%
  bind_rows(data2021) %>%
  bind_rows(data2022) %>%
  mutate(Survey.Date = substr(created_at,1,10) ) %>%
  bind_rows(data2023) %>%
  mutate(Year = substr(Survey.Date,1,4))

## Data cleaning
  # PREDATION: There are differences in how the 2023 data are categorized
  # There's also a lot of missingness for this measure
    data$Predation[data$Predation == "No damage"] <- "No"
    data$Predation[data$Predation == "Predation"] <- "Yes"
    data$Predation[data$Predation == "Eye loss only"] <- "Eye loss"

  # Hours Since Death - categorize into Carcass Age

  ## Data questions for Spencer and Troy
    # Species: if it is blank, is it the same as if it's listed "unknown"?
    # Sex: if it is blank, is it the same as if it's listed "unknown"?
    # Predation: if it is blank, is it the same as if it's listed "unknown"?
    # Hours since death: data were captured in hours in 2019 & 2020, but in categories in 2021 onwards. Can I recategorize the old data?

write.csv(data,paste0(directory,"SalmonSurveyAllYears.csv"),row.names=FALSE)







