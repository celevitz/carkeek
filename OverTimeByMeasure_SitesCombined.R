## Carly Levitz
## Written: June 24, 2023
## Modified: June 24, 2023
## Purpose: quarterly report.

## Step 1: set things up
rm(list=ls())

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
         ,siteNumber = as.character(siteNumber)
         ,quarter = case_when(month %in% c(1,2,3) ~ "Q1"
                              ,month %in% c(4,5,6) ~ "Q2"
                              ,month %in% c(7,8,9) ~ "Q3"
                              ,month %in% c(10,11,12) ~ "Q4"))


var <- "OxSat"

###############################################
## Volunteer information over time: tests, unique volunteers, volunteer hours

quarterlyTests <- clean %>%
  group_by(year, quarter) %>%
  mutate(`Number of tests` = n()) %>%
  select(year,quarter,`Number of tests`) %>%
  distinct()

quarterlyVols <- clean %>%
  select(year,quarter,tester1,tester2) %>%
  pivot_longer(!c(year,quarter),names_to="tester") %>%
  filter(!(is.na(value))) %>%
  select(!tester) %>%
  group_by(year,quarter,value) %>%
  mutate(n=n()) %>%
  distinct() %>%
  ungroup() %>% group_by(year,quarter) %>%
  mutate(`Unique volunteers`=n()) %>%
  select(year,quarter,`Unique volunteers`) %>%
  distinct()

quarterlyPeopleHours <- clean %>%
  mutate(numberoftesters = ifelse(is.na(tester2),1,2)
         ,peoplehours = numberoftesters*2)  %>%
  group_by(year,quarter) %>%
  summarise(`Volunteer hours` = sum(peoplehours))

quarterly <- quarterlyTests %>%
  left_join(quarterlyVols) %>%
  left_join(quarterlyPeopleHours) %>%
  mutate(time = paste0(as.character(year)," ",quarter)) %>%
  pivot_longer(!c(year,quarter,time),names_to="variable",values_to= "value")

quarterlygraph <-

  quarterly %>%
  ggplot(aes(x=time,y=value)) +
  facet_wrap(~variable,ncol=1,scales="free_y")+
  geom_point(color=main) +
  geom_text(aes(label=value),nudge_y = 1,family = ft,color=main,size=smalltextsize) +
  ggtitle("Carkeek Watershed Community Action Project:\nQuarterly Volunteer Summary") +
  xlab("") + ylab("Quarterly value") +
  labs(caption = "Scales on y axes vary by graph") +
  theme_minimal() +
    theme(
      strip.background = element_rect(fill=light,color=light)
      ,strip.text = element_text(family=ft,color=dark,size=subtitlesize)
      ,title = element_text(family=ft,color=main,size=titlesize)
      ,axis.title = element_text(family=ft,color=main,size=subtitlesize)
      ,axis.text = element_text(family=ft,color=main,size=subtitlesize)
      ,plot.caption = element_text(family = ft,hjust=0,color=main,size=subtitlesize)
      ,panel.grid = element_blank()
      ,axis.line = element_line(color=dark)
      ,axis.ticks = element_line(color=dark)
    )





################################################
## Measures with values

ggplot(clean,aes(x=dateTested,y=OxSat,color=waterbody,shape=siteNumber)) +
  geom_point() +
  scale_shape_manual(values = c("1"=1,"2"=2,"3"=3,"4"=4
                                ,"5"=5,"6"=6,"7"=18,"8"=20)) +
  scale_y_continuous(lim=c(0,1.5)) +
  scale_color_manual(values=c("#E69F00","#56B4E9","#009E73",
                              "#F0E442","#0072B2","#D55E00","#CC79A7")) +
  #theme
  ggtitle("Option 1: Oxygen Saturation") +
  theme_minimal() +
  theme(#panel.grid = element_blank()
        #,axis.line = element_line(col="black")
        #,axis.tick = element_line(col=dark)
        #,plot.caption = element_text(family = ft,hjust=0,color=dark)
    )

ggplot(clean,aes(x=dateTested,y=OxSat,color=waterbody)) +
  geom_point() +
  facet_wrap(~siteNumber) +
  scale_y_continuous(lim=c(0,1.5)) +
  scale_color_manual(values=c("#E69F00","#56B4E9","#009E73",
                              "#F0E442","#0072B2","#D55E00","#CC79A7")) +
  #theme
  ggtitle("Option 2: Oxygen Saturation") +
  theme_minimal() +
  theme(#panel.grid = element_blank()
    #,axis.line = element_line(col="black")
    #,axis.tick = element_line(col=dark)
    #,plot.caption = element_text(family = ft,hjust=0,color=dark)
  )


###############################################################################
pdf(file = paste(directory,"CarkeekWatershedTesting_QuarterlyGraphs.pdf",sep="")
    ,paper="letter",width=8,height=11)

quarterlygraph

dev.off()




