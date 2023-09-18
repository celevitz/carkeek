## Author: Carly Levitz
## Written: 2023-09-17
## Updated: 2023-09-17
## Purpose: Compare Sites 6 and 7


## Step 1: set things up
  rm(list=ls())

  library(ggplot2)
  library(tidyverse)
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

  axistitlesize <- 20
  axissize <- 10
  titlesize <- 15
  subtitlesize <- 15
  captionsize <- 5
  textlabelsize <- 2
  titlemarginL <- 10
  titlemarginR <- 10
  titlemarginT <- 10
  titlemarginB <- 10

  logo <- paste(directory,"CWCAPlogo-1-white-1024x346.png",sep="")
  info <- data.frame(x = 1,y = 1,image = logo)

## Step 2: Bring in data and clean it
  ### Step 2a. Bring in data
  clean <- read.csv(paste0(directory,"H20Data.csv"),stringsAsFactors = FALSE)

  ### Step 2b. Clean data
  clean$dateTested <- as.Date(clean$dateTested)

  ### Step 2c. Keep just the two sites
  sites67 <- clean[clean$siteNumber %in% c(6,7),
                   c("siteNumber","dateTested","airTemp","waterTemp",
                     "pH","averageDo","OxSat","totalAlk","totalHardness"
                     ,"turbidityJTU","avgEcoli","avgColiform")]
  sites67$siteNumber <- as.factor(sites67$siteNumber)

##
  ## Do, pH, Water Temp: Values
  figs13 <- sites67 %>% select(siteNumber,dateTested
                     #,airTemp
                     ,waterTemp,pH,averageDo) %>%
  pivot_longer(names_to="variable",values_to="values"
               ,!c(siteNumber,dateTested)) %>%
  ggplot(aes(x=dateTested,y=values,col=siteNumber,shape=siteNumber)) +
    facet_wrap(~variable) +
    geom_point(size=3) +
    labs(title="Figures 1a, 2a, and 3a"
         ,subtitle="Comparing sites 6 and 7: actual values") +
    theme(axis.text = element_text(size=axissize,family=ft)
          ,axis.line.y = element_line(color=dark)
          ,axis.ticks.y = element_line(color=dark)
          ,axis.line.x = element_line(color=dark)
          ,axis.ticks.x = element_line(color=dark)
          ,axis.title.x = element_blank()
          ,plot.title = element_text(size=titlesize,face="bold",family=ft
                                     ,margin=margin(t=titlemarginT
                                                    ,r=titlemarginR
                                                    ,b=titlemarginB
                                                    ,l=titlemarginL))
          ,plot.subtitle = element_text(size=subtitlesize,family=ft)
          ,plot.caption = element_text(size=captionsize,family=ft))

  ## Do, pH, Water Temp: Differences
  figs13diffs <- sites67 %>% select(siteNumber,dateTested
                     ,waterTemp,pH,averageDo) %>%
    pivot_longer(names_to="variable",values_to="values"
                 ,!c(siteNumber,dateTested)) %>%
    pivot_wider(names_from=siteNumber,values_from=values) %>%
    mutate(diff=`7`-`6`) %>%
    ggplot(aes(x=dateTested,y=diff)) +
    facet_wrap(~variable) +
    geom_point(size=3) +
    geom_hline(yintercept=0) +
    labs(title="Figures 1b, 2b, and 3b: Difference between sites 6 and 7"
         ,subtitle="Comparing sites 6 and 7: negative values indicate site 7 has lower values\ncomparisons can only be calculated when the sites were tested on the same day") +
    theme(axis.text = element_text(size=axissize,family=ft)
          ,axis.line.y = element_line(color=dark)
          ,axis.ticks.y = element_line(color=dark)
          ,axis.line.x = element_line(color=dark)
          ,axis.ticks.x = element_line(color=dark)
          ,axis.title.x = element_blank()
          ,plot.title = element_text(size=titlesize,face="bold",family=ft
                                     ,margin=margin(t=titlemarginT
                                                    ,r=titlemarginR
                                                    ,b=titlemarginB
                                                    ,l=titlemarginL))
          ,plot.subtitle = element_text(size=subtitlesize,family=ft)
          ,plot.caption = element_text(size=captionsize,family=ft))


  # Air Temp
  airtemp <- sites67 %>%
    ggplot(aes(x=dateTested,y=airTemp,col=siteNumber,shape=siteNumber)) +
    geom_point(size=3) +
    labs(title="Figure 4: Air Temperature"
         ,subtitle="Comparing sites 6 and 7: actual values") +
    theme(axis.text = element_text(size=axissize,family=ft)
          ,axis.line.y = element_line(color=dark)
          ,axis.ticks.y = element_line(color=dark)
          ,axis.line.x = element_line(color=dark)
          ,axis.ticks.x = element_line(color=dark)
          ,axis.title.x = element_blank()
          ,plot.title = element_text(size=titlesize,face="bold",family=ft
                                     ,margin=margin(t=titlemarginT
                                                    ,r=titlemarginR
                                                    ,b=titlemarginB
                                                    ,l=titlemarginL))
          ,plot.subtitle = element_text(size=subtitlesize,family=ft)
          ,plot.caption = element_text(size=captionsize,family=ft))

  ## Air Temp: Difference
  airtempdiff <- sites67 %>% select(siteNumber,dateTested
                     ,airTemp) %>%
    pivot_wider(names_from=siteNumber,values_from=airTemp) %>%
    mutate(diff=`7`-`6`) %>%
    ggplot(aes(x=dateTested,y=diff)) +
    geom_point(size=3) +
    geom_hline(yintercept=0) +
    labs(title="Figures 4b: Air Temperature difference between sites 6 and 7"
         ,subtitle="Comparing sites 6 and 7: negative values indicate site 7 has lower values\ncomparisons can only be calculated when the sites were tested on the same day") +
    theme(axis.text = element_text(size=axissize,family=ft)
          ,axis.line.y = element_line(color=dark)
          ,axis.ticks.y = element_line(color=dark)
          ,axis.line.x = element_line(color=dark)
          ,axis.ticks.x = element_line(color=dark)
          ,axis.title.x = element_blank()
          ,plot.title = element_text(size=titlesize,face="bold",family=ft
                                     ,margin=margin(t=titlemarginT
                                                    ,r=titlemarginR
                                                    ,b=titlemarginB
                                                    ,l=titlemarginL))
          ,plot.subtitle = element_text(size=subtitlesize,family=ft)
          ,plot.caption = element_text(size=captionsize,family=ft))

  # Ox Sat (0 to 1)
  oxsat <- sites67 %>%
    ggplot(aes(x=dateTested,y=OxSat,col=siteNumber,shape=siteNumber)) +
    geom_point(size=3)  +
    labs(title="Figure 5: Oxygen Saturation"
         ,subtitle="Comparing sites 6 and 7: actual values") +
    theme(axis.text = element_text(size=axissize,family=ft)
          ,axis.line.y = element_line(color=dark)
          ,axis.ticks.y = element_line(color=dark)
          ,axis.line.x = element_line(color=dark)
          ,axis.ticks.x = element_line(color=dark)
          ,axis.title.x = element_blank()
          ,plot.title = element_text(size=titlesize,face="bold",family=ft
                                     ,margin=margin(t=titlemarginT
                                                    ,r=titlemarginR
                                                    ,b=titlemarginB
                                                    ,l=titlemarginL))
          ,plot.subtitle = element_text(size=subtitlesize,family=ft)
          ,plot.caption = element_text(size=captionsize,family=ft))

  ## Ox Sat: Difference
  oxsatdiff <- sites67 %>% select(siteNumber,dateTested
                     ,OxSat) %>%
    pivot_wider(names_from=siteNumber,values_from=OxSat) %>%
    mutate(diff=`7`-`6`) %>%
    ggplot(aes(x=dateTested,y=diff)) +
    geom_point(size=3) +
    geom_hline(yintercept=0) +
    labs(title="Figures 5b: Oxygen Saturation difference between sites 6 and 7"
         ,subtitle="Comparing sites 6 and 7: negative values indicate site 7 has lower values\ncomparisons can only be calculated when the sites were tested on the same day") +
    theme(axis.text = element_text(size=axissize,family=ft)
          ,axis.line.y = element_line(color=dark)
          ,axis.ticks.y = element_line(color=dark)
          ,axis.line.x = element_line(color=dark)
          ,axis.ticks.x = element_line(color=dark)
          ,axis.title.x = element_blank()
          ,plot.title = element_text(size=titlesize,face="bold",family=ft
                                     ,margin=margin(t=titlemarginT
                                                    ,r=titlemarginR
                                                    ,b=titlemarginB
                                                    ,l=titlemarginL))
          ,plot.subtitle = element_text(size=subtitlesize,family=ft)
          ,plot.caption = element_text(size=captionsize,family=ft))

  alk <- sites67 %>%
    ggplot(aes(x=dateTested,y=totalAlk,col=siteNumber,shape=siteNumber)) +
    geom_point(size=3) +
    labs(title="Figure 6: Alkalinity"
         ,subtitle="Comparing sites 6 and 7: actual values") +
    theme(axis.text = element_text(size=axissize,family=ft)
          ,axis.line.y = element_line(color=dark)
          ,axis.ticks.y = element_line(color=dark)
          ,axis.line.x = element_line(color=dark)
          ,axis.ticks.x = element_line(color=dark)
          ,axis.title.x = element_blank()
          ,plot.title = element_text(size=titlesize,face="bold",family=ft
                                     ,margin=margin(t=titlemarginT
                                                    ,r=titlemarginR
                                                    ,b=titlemarginB
                                                    ,l=titlemarginL))
          ,plot.subtitle = element_text(size=subtitlesize,family=ft)
          ,plot.caption = element_text(size=captionsize,family=ft))

  alkdiff <- sites67 %>% select(siteNumber,dateTested
                                   ,totalAlk) %>%
    pivot_wider(names_from=siteNumber,values_from=totalAlk) %>%
    mutate(diff=`7`-`6`) %>%
    ggplot(aes(x=dateTested,y=diff)) +
    geom_point(size=3) +
    geom_hline(yintercept=0) +
    labs(title="Figures 6b: Alkalinity difference between sites 6 and 7"
         ,subtitle="Comparing sites 6 and 7: negative values indicate site 7 has lower values\ncomparisons can only be calculated when the sites were tested on the same day") +
    theme(axis.text = element_text(size=axissize,family=ft)
          ,axis.line.y = element_line(color=dark)
          ,axis.ticks.y = element_line(color=dark)
          ,axis.line.x = element_line(color=dark)
          ,axis.ticks.x = element_line(color=dark)
          ,axis.title.x = element_blank()
          ,plot.title = element_text(size=titlesize,face="bold",family=ft
                                     ,margin=margin(t=titlemarginT
                                                    ,r=titlemarginR
                                                    ,b=titlemarginB
                                                    ,l=titlemarginL))
          ,plot.subtitle = element_text(size=subtitlesize,family=ft)
          ,plot.caption = element_text(size=captionsize,family=ft))

  hard <- sites67 %>%
    ggplot(aes(x=dateTested,y=totalHardness,col=siteNumber,shape=siteNumber)) +
    geom_point(size=3) +
    labs(title="Figure 7: Hardness"
         ,subtitle="Comparing sites 6 and 7: actual values") +
    theme(axis.text = element_text(size=axissize,family=ft)
          ,axis.line.y = element_line(color=dark)
          ,axis.ticks.y = element_line(color=dark)
          ,axis.line.x = element_line(color=dark)
          ,axis.ticks.x = element_line(color=dark)
          ,axis.title.x = element_blank()
          ,plot.title = element_text(size=titlesize,face="bold",family=ft
                                     ,margin=margin(t=titlemarginT
                                                    ,r=titlemarginR
                                                    ,b=titlemarginB
                                                    ,l=titlemarginL))
          ,plot.subtitle = element_text(size=subtitlesize,family=ft)
          ,plot.caption = element_text(size=captionsize,family=ft))

  harddiff <- sites67 %>% select(siteNumber,dateTested
                                 ,totalHardness) %>%
    pivot_wider(names_from=siteNumber,values_from=totalHardness) %>%
    mutate(diff=`7`-`6`) %>%
    ggplot(aes(x=dateTested,y=diff)) +
    geom_point(size=3) +
    geom_hline(yintercept=0) +
    labs(title="Figures 7b: Hardness difference between sites 6 and 7"
         ,subtitle="Comparing sites 6 and 7: negative values indicate site 7 has lower values\ncomparisons can only be calculated when the sites were tested on the same day") +
    theme(axis.text = element_text(size=axissize,family=ft)
          ,axis.line.y = element_line(color=dark)
          ,axis.ticks.y = element_line(color=dark)
          ,axis.line.x = element_line(color=dark)
          ,axis.ticks.x = element_line(color=dark)
          ,axis.title.x = element_blank()
          ,plot.title = element_text(size=titlesize,face="bold",family=ft
                                     ,margin=margin(t=titlemarginT
                                                    ,r=titlemarginR
                                                    ,b=titlemarginB
                                                    ,l=titlemarginL))
          ,plot.subtitle = element_text(size=subtitlesize,family=ft)
          ,plot.caption = element_text(size=captionsize,family=ft))

  turb <- sites67 %>%
    ggplot(aes(x=dateTested,y=turbidityJTU,col=siteNumber,shape=siteNumber)) +
    geom_point(size=3) +
    labs(title="Figure 8: Turbidity"
         ,subtitle="Comparing sites 6 and 7: actual values") +
    theme(axis.text = element_text(size=axissize,family=ft)
          ,axis.line.y = element_line(color=dark)
          ,axis.ticks.y = element_line(color=dark)
          ,axis.line.x = element_line(color=dark)
          ,axis.ticks.x = element_line(color=dark)
          ,axis.title.x = element_blank()
          ,plot.title = element_text(size=titlesize,face="bold",family=ft
                                     ,margin=margin(t=titlemarginT
                                                    ,r=titlemarginR
                                                    ,b=titlemarginB
                                                    ,l=titlemarginL))
          ,plot.subtitle = element_text(size=subtitlesize,family=ft)
          ,plot.caption = element_text(size=captionsize,family=ft))

  turbdiff  <- sites67 %>% select(siteNumber,dateTested
                                  ,turbidityJTU) %>%
    pivot_wider(names_from=siteNumber,values_from=turbidityJTU) %>%
    mutate(diff=`7`-`6`) %>%
    ggplot(aes(x=dateTested,y=diff)) +
    geom_point(size=3) +
    geom_hline(yintercept=0) +
    labs(title="Figures 8b: Turbidity difference between sites 6 and 7"
         ,subtitle="Comparing sites 6 and 7: negative values indicate site 7 has lower values\ncomparisons can only be calculated when the sites were tested on the same day") +
    theme(axis.text = element_text(size=axissize,family=ft)
          ,axis.line.y = element_line(color=dark)
          ,axis.ticks.y = element_line(color=dark)
          ,axis.line.x = element_line(color=dark)
          ,axis.ticks.x = element_line(color=dark)
          ,axis.title.x = element_blank()
          ,plot.title = element_text(size=titlesize,face="bold",family=ft
                                     ,margin=margin(t=titlemarginT
                                                    ,r=titlemarginR
                                                    ,b=titlemarginB
                                                    ,l=titlemarginL))
          ,plot.subtitle = element_text(size=subtitlesize,family=ft)
          ,plot.caption = element_text(size=captionsize,family=ft))

  ecoli <- sites67 %>%
    ggplot(aes(x=dateTested,y=avgEcoli,col=siteNumber,shape=siteNumber)) +
    geom_point(size=3) +
    labs(title="Figure 9: Average eColi"
         ,subtitle="Comparing sites 6 and 7: actual values") +
    theme(axis.text = element_text(size=axissize,family=ft)
          ,axis.line.y = element_line(color=dark)
          ,axis.ticks.y = element_line(color=dark)
          ,axis.line.x = element_line(color=dark)
          ,axis.ticks.x = element_line(color=dark)
          ,axis.title.x = element_blank()
          ,plot.title = element_text(size=titlesize,face="bold",family=ft
                                     ,margin=margin(t=titlemarginT
                                                    ,r=titlemarginR
                                                    ,b=titlemarginB
                                                    ,l=titlemarginL))
          ,plot.subtitle = element_text(size=subtitlesize,family=ft)
          ,plot.caption = element_text(size=captionsize,family=ft))

  ecolidiff <- sites67 %>% select(siteNumber,dateTested
                                  ,avgEcoli) %>%
    pivot_wider(names_from=siteNumber,values_from=avgEcoli) %>%
    mutate(diff=`7`-`6`) %>%
    ggplot(aes(x=dateTested,y=diff)) +
    geom_point(size=3) +
    geom_hline(yintercept=0) +
    labs(title="Figures 9b: eColi difference between sites 6 and 7"
         ,subtitle="Comparing sites 6 and 7: negative values indicate site 7 has lower values\ncomparisons can only be calculated when the sites were tested on the same day") +
    theme(axis.text = element_text(size=axissize,family=ft)
          ,axis.line.y = element_line(color=dark)
          ,axis.ticks.y = element_line(color=dark)
          ,axis.line.x = element_line(color=dark)
          ,axis.ticks.x = element_line(color=dark)
          ,axis.title.x = element_blank()
          ,plot.title = element_text(size=titlesize,face="bold",family=ft
                                     ,margin=margin(t=titlemarginT
                                                    ,r=titlemarginR
                                                    ,b=titlemarginB
                                                    ,l=titlemarginL))
          ,plot.subtitle = element_text(size=subtitlesize,family=ft)
          ,plot.caption = element_text(size=captionsize,family=ft))

  coliform <- sites67 %>%
    ggplot(aes(x=dateTested,y=avgColiform,col=siteNumber,shape=siteNumber)) +
    geom_point(size=3) +
    labs(title="Figure 10: Average Coliform"
         ,subtitle="Comparing sites 6 and 7: actual values") +
    theme(axis.text = element_text(size=axissize,family=ft)
          ,axis.line.y = element_line(color=dark)
          ,axis.ticks.y = element_line(color=dark)
          ,axis.line.x = element_line(color=dark)
          ,axis.ticks.x = element_line(color=dark)
          ,axis.title.x = element_blank()
          ,plot.title = element_text(size=titlesize,face="bold",family=ft
                                     ,margin=margin(t=titlemarginT
                                                    ,r=titlemarginR
                                                    ,b=titlemarginB
                                                    ,l=titlemarginL))
          ,plot.subtitle = element_text(size=subtitlesize,family=ft)
          ,plot.caption = element_text(size=captionsize,family=ft))

  coliformdiff <- sites67 %>% select(siteNumber,dateTested
                                     ,avgColiform) %>%
    pivot_wider(names_from=siteNumber,values_from=avgColiform) %>%
    mutate(diff=`7`-`6`) %>%
    ggplot(aes(x=dateTested,y=diff)) +
    geom_point(size=3) +
    geom_hline(yintercept=0) +
    labs(title="Figures 10b: Coliform difference between sites 6 and 7"
         ,subtitle="Comparing sites 6 and 7: negative values indicate site 7 has lower values\ncomparisons can only be calculated when the sites were tested on the same day") +
    theme(axis.text = element_text(size=axissize,family=ft)
          ,axis.line.y = element_line(color=dark)
          ,axis.ticks.y = element_line(color=dark)
          ,axis.line.x = element_line(color=dark)
          ,axis.ticks.x = element_line(color=dark)
          ,axis.title.x = element_blank()
          ,plot.title = element_text(size=titlesize,face="bold",family=ft
                                     ,margin=margin(t=titlemarginT
                                                    ,r=titlemarginR
                                                    ,b=titlemarginB
                                                    ,l=titlemarginL))
          ,plot.subtitle = element_text(size=subtitlesize,family=ft)
          ,plot.caption = element_text(size=captionsize,family=ft))


titleplot <-
  ggplot(info, aes(x=x,y=y)) +
  geom_image(aes(x=1,y=1,image=logo),size=1) +
  theme(plot.background = element_blank()
        ,panel.background = element_rect(color="#7e7b3e",fill="#7e7b3e")
        ,panel.grid = element_blank()
        ,axis.line = element_blank()
        ,axis.text = element_blank()
        ,axis.ticks = element_blank()
        ,axis.title = element_blank()) +
  labs(title = "Carkeek Watershed Community Action Project: Comparison between sites 6 and 7"
       ,subtitle = "Water quality data collected by Natalie, Sean, Sue, Alice, and Racheal between 10/29/22 and 08/02/23"
       ,caption = "Analysis by Carly Levitz")






pdf(file = paste(directory,"CarkeekWatershedTesting_Sites6and7.pdf",sep="")
    ,paper="letter",width=11,height=8)

  titleplot
  ggarrange(figs13,figs13diffs,ncol=1,nrow=2)
  ggarrange(airtemp,airtempdiff,ncol=1,nrow=2)
  ggarrange(oxsat,oxsatdiff,ncol=1,nrow=2)
  ggarrange(alk,alkdiff,ncol=1,nrow=2)
  ggarrange(hard,harddiff,ncol=1,nrow=2)
  ggarrange(turb,turbdiff,ncol=1,nrow=2)
  ggarrange(ecoli,ecolidiff,ncol=1,nrow=2)
  ggarrange(coliform,coliformdiff,ncol=1,nrow=2)
dev.off()




