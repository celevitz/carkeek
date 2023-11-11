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
  bind_rows(data2023)

write.csv(data,paste0(directory,"SalmonSurveyAllYears.csv"),row.names=FALSE)







