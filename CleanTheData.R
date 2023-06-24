## Author: Carly Levitz
## Written: 2023-06-20
## Updated: 2023-06-24
## Purpose: clean the data

## Step 1: set things up
rm(list=ls())

directory <- "/Users/carlylevitz/Documents/Data/carkeek/"
setwd(directory)

importdataname <- "H20 Data as of May 2024"
exportdataname  <- "H20Data"

## Step 2: Bring in data and clean it
### Step 2a. Bring in data
rawdata <- as_tibble(read.xlsx(paste(directory,importdataname,".xlsx"
                                     ,sep=""),sheet=1,startRow = 2))

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
         ,`Tester.#1` = case_when(`Tester.#1` %in% c("Sue") ~ "Sue Cottrell"
                                  ,`Tester.#1` %in% c("Natale") ~ "Natalie"
                                  ,`Tester.#1` %in% c("mike") ~ "Mike"
                                  ,`Tester.#1` %in% c("sean") ~ "Sean"
                                  ,TRUE ~ `Tester.#1`)
         ,`Tester.#2` = case_when(`Tester.#2` %in% c("Alice") ~
                                    "Alice Cottrell-Steen"
                                  ,`Tester.#2` %in% c("-  ","-") ~ NA
                                  ,TRUE ~ `Tester.#2`)
         # If Turbidity is greater than 240, plot it at 241
         ,`Turbidity.(JTU)` = ifelse(">240 NCU","241",`Turbidity.(JTU)`)
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
  ) %>%
  # drop the fake data
  filter(`Tester.#1` != "John Doe")

# change characters variables that should be numeric, to numeric
for (charvar in c("Average.DO","%.Ox..Sat.","Total.ALK","Total.Hardness"
                  ,"Turbidity.(JTU)","E..coli.3","Avg..E..coli"
                  ,"Coliform.1","Coliform.2","Coliform.3","Avg..Coliform")) {
  print(charvar)
  clean[,charvar] <- as.numeric(unlist(clean[,charvar]))
}

## rename the data
names(clean) <- c("id","siteNumber","tester1","tester2","dateTested","month"
                  ,"timeTested","isThereASiteNumber","isThereATester"
                  ,"isTheDateAppropriate","exclude","siteDateId","year"
                  ,"previousYear","monthCalculated","previousMonth","watershed"
                  ,"waterbody","waterbodyCondition","weatherConditions"
                  ,"airTemp","waterTemp","pH","do1","do2","averageDo","OxSat"
                  ,"dropAlk","totalAlk","dropHardness","totalHardness"
                  ,"turbidityJTU","secchiDepth","ecoli1","ecoli2","ecoli3"
                  ,"avgEcoli","coliform1","coliform2","coliform3","avgColiform"
                  ,"comments")

## Step 4. Export data
write.csv(clean,paste0(directory,exportdataname,".csv"),row.names=FALSE)

