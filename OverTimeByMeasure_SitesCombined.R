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
         ,siteNumber = as.character(siteNumber))


var <- "OxSat"

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



