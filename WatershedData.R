## Carly Levitz
## Written: 2023-06-20
## Purpose: create visualizations of Carkeek Watershed data

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

directory <- "/Users/carlylevitz/Documents/Data/carkeek/"
setwd(directory)

dark <- "#4b4f42"
mid2 <- "#6b625c"
mid <- "#9c8b94"
main <- "#7e7b3e"
light <- "#f0e0cc"
highlight <- "#46bdc6"

axistitlesize <- 20
axissize <- 18
titlesize <- 10
subtitlesize <- 8
captionsize <- 8
textlabelsize <- 2
plotmarginL <- 10
plotmarginR <- 30
plotmarginT <- 10
plotmarginB <- 10
titlemarginL <- 10
titlemarginR <- 10
titlemarginT <- 10
titlemarginB <- 10



rawdata <- as_tibble(read.xlsx(paste(directory,"H20 Data as of May 2024.xlsx",sep=""),sheet=1,startRow = 2))

# Clean data
clean <- rawdata %>%
  # I'll need to fix the date and time
  # remove trailing spaces, extra quotation marks
  # combine testers that are the same
  # drop the fake data
  filter(`Tester.#1` != "John Doe")



sitechosen <- 1
## The idea is: for each site, have the logo and a general description at the
## top. Then, 6 graphs - temperature, ph, total hardness, total alkalinity,
## dissolved oxygen, and turbidity

temptitle <- str_glue("Water and air temperature over time")
tempsubtitle <- str_glue("Water temperature can affect the breeding and feeding
                         of aquatic<br>animals. It also affects how much dissolved
                         oxygen the water can hold.<br>Air temperature affects
                         water temperature. The National Wildlife<br>Federation
                         says that the optimum water temperature<br>range for
                         chinook salmon is 12.8 to 17.8 degrees Celsius.")

sitespecificdata <- clean %>% filter(`Site.#` == sitechosen)

temperature <-
  sitespecificdata %>%
  ggplot(aes(x=Date.Tested,y=Water.Temp)) +
  coord_cartesian(clip="off") +
  geom_rect(xmin=min(sitespecificdata$Date.Tested)
            ,xmax=max(sitespecificdata$Date.Tested)
            ,ymin=12.8,ymax=17.8
            ,color=light,fill=light)+
  geom_point(aes(x=Date.Tested,y=Water.Temp),color=main) +
  geom_point(aes(x=Date.Tested,y=Air.Temp),color=mid2) +
  geom_line(aes(x=Date.Tested,y=Water.Temp),color=main) +
  geom_line(aes(x=Date.Tested,y=Air.Temp),color=mid2) +
  geom_text(data = sitespecificdata %>% filter(Date.Tested == max(Date.Tested))
             ,aes(x=Date.Tested+1,y=Water.Temp),color=main
            ,label="Water temperature",hjust=0,size=textlabelsize) +
  geom_text(data = sitespecificdata %>% filter(Date.Tested == max(Date.Tested))
            ,aes(x=Date.Tested+1,y=Air.Temp),color=mid2
            ,label="Air temperature",hjust=0,size=textlabelsize) +
  scale_y_continuous(lim=c(0,27)) +
  xlab("Date") + ylab("Temperature (Celsius)") +
  labs(title=temptitle
       ,subtitle=tempsubtitle) +
  theme_minimal() +
  theme(panel.grid = element_blank()
        ,panel.background = element_blank()
        ,plot.background = element_blank()
        ,axis.text = element_markdown()
        ,plot.title = element_markdown(size=titlesize,face="bold",margin=margin(t=titlemarginT,r=titlemarginR,b=titlemarginB,l=titlemarginL))
        ,plot.subtitle = element_markdown(size=subtitlesize)
        ,plot.caption = element_markdown(size=captionsize)
        ,plot.margin = margin(t=plotmarginT,r=plotmarginR,b=plotmarginB,l=plotmarginL))





pdf(file = paste(directory,"Site",sitechosen,"_OnePager.pdf",sep="")
    ,paper="letter",width=8,height=11)
ggarrange(temperature,temperature
          ,temperature,temperature
          ,temperature,temperature
          ,nrow=3,ncol=2)

dev.off()










