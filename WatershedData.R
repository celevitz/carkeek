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
library(ggimage)

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

logo <- paste(directory,"CWCAPlogo-1-white-1024x346.png",sep="")

rawdata <- as_tibble(read.xlsx(paste(directory,"H20 Data as of May 2024.xlsx",sep=""),sheet=1,startRow = 2))

# Clean data
clean <- rawdata %>%
  # I'll need to fix the date and time
  # remove trailing spaces, extra quotation marks
  # combine testers that are the same
  # drop the fake data
  filter(`Tester.#1` != "John Doe")
  # change numeric variables to numeric

  for (charvar in c("Average.DO","%.Ox..Sat.","Total.ALK","Total.Hardness"
                    ,"Turbidity.(JTU)","E..coli.3","Avg..E..coli"
                    ,"Coliform.1","Coliform.2","Coliform.3","Avg..Coliform")) {
    print(charvar)
    clean[,charvar] <- as.numeric(unlist(clean[,charvar]))
  }



sitechosen <- 1
## The idea is: for each site, have the logo and a general description at the
## top. Then, 6 graphs - overall statement, temperature, ph, total hardness &
## total alkalinity, dissolved oxygen, and turbidity.
## Also need to put in ecoli & coliform
sitespecificdata <- clean %>% filter(`Site.#` == sitechosen) %>%
  arrange(Date.Tested)

nameofsite <- unique(sitespecificdata$Waterbody)
numberoftests <- length(unique(sitespecificdata$Date.Tested))
numberofvolunteers <- length(unique(c(unique(sitespecificdata$`Tester.#1`)
                                      ,unique(sitespecificdata$`Tester.#2`))))
overalltitle <- str_glue("{nameofsite} (Site #{sitechosen}) has been tested
                        {numberoftests} times by {numberofvolunteers}
                         volunteers.")

temptitle <- str_glue("Water and air temperature")
tempsubtitle <- str_glue("Water temperature can affect the breeding and feeding
                         of aquatic<br>
                         animals. It also affects how much dissolved oxygen the
                         water can hold.<br>
                         Air temperature affects water temperature. The National
                         Wildlife<br>
                         Federation says that the optimum water temperature
                         range for chinook<br>
                         salmon is 12.8 to 17.8 degrees Celsius.")
phtitle <- str_glue("pH levels")
phsubtitle <- str_glue("pH measures how acidic or basic water is. A value of 7
                        is neutral.<br>
                        Less than 7 is acidic, and more than 7 is basic. pH can
                        change over<br>the course of a season (or a day). A pH
                        less than 4.0 or more than<br>
                        11.0 is usually lethal to fish and other organisms. pH
                       <br>between 6 and 8.5 is usually ideal.")

hardnessalkalinitytitle <- str_glue("Hardness and Alkilinity")
hAndASubtitle <- str_glue("Higher alkalinity provides a buffer against changes
                          in pH, making it<br>
                          more stable for aquatic life. Hardness is primarily
                          the concentration<br>
                          of calcium and magnesium ions in water.")

info <- data.frame(x = 1,
                y = 1,
                image = logo)
firstgraphA <-
  ggplot(info, aes(x=x,y=y)) +
  geom_image(aes(x=1,y=1,image=logo),size=2.2) +
  theme_minimal() +
  theme(panel.grid = element_blank()
        ,panel.background = element_rect(color=main,fill=main)
        ,plot.background = element_rect(color=main,fill=main)
        ,axis.text = element_blank()
        ,axis.line = element_blank()
        ,axis.ticks = element_blank()
        ,axis.title = element_blank()
        ,plot.title = element_markdown(size=titlesize,face="bold"
                                       ,margin=margin(t=titlemarginT
                                                      ,r=titlemarginR
                                                      ,b=titlemarginB
                                                      ,l=titlemarginL))
        ,plot.subtitle = element_markdown(size=subtitlesize)
        ,plot.caption = element_markdown(size=captionsize)
        ,plot.margin = margin(t=plotmarginT,r=plotmarginR,b=plotmarginB
                              ,l=plotmarginL))

firstgraphB <-
  ggplot(info,aes(x=x,y=y)) +
  labs(title=overalltitle) +
  theme_minimal() +
  theme(panel.grid = element_blank()
        ,panel.background = element_blank()
        ,plot.background = element_blank()
        ,axis.text = element_blank()
        ,axis.line = element_blank()
        ,axis.ticks = element_blank()
        ,axis.title = element_blank()
        ,plot.title = element_markdown(size=titlesize,face="bold"
                                       ,margin=margin(t=titlemarginT
                                                      ,r=titlemarginR
                                                      ,b=titlemarginB
                                                      ,l=titlemarginL)
                                       ,hjust=.5,color=main)
        ,plot.subtitle = element_markdown(size=subtitlesize)
        ,plot.caption = element_markdown(size=captionsize)
        ,plot.margin = margin(t=plotmarginT,r=plotmarginR,b=plotmarginB
                              ,l=plotmarginL))


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
  ylab("Temperature (Celsius)") +
  labs(title=temptitle
       ,subtitle=tempsubtitle) +
  theme_minimal() +
  theme(panel.grid = element_blank()
        ,panel.background = element_blank()
        ,plot.background = element_blank()
        ,axis.text = element_markdown()
        ,axis.line.y = element_line(color=dark)
        ,axis.ticks.y = element_line(color=dark)
        ,axis.title.x = element_blank()
        ,plot.title = element_markdown(size=titlesize,face="bold",margin=margin(t=titlemarginT,r=titlemarginR,b=titlemarginB,l=titlemarginL))
        ,plot.subtitle = element_markdown(size=subtitlesize)
        ,plot.caption = element_markdown(size=captionsize)
        ,plot.margin = margin(t=plotmarginT,r=plotmarginR,b=plotmarginB,l=plotmarginL))

phGraph <-
  sitespecificdata %>%
    ggplot(aes(x=Date.Tested,y=pH)) +
    coord_cartesian(clip="off") +
    geom_rect(xmin=min(sitespecificdata$Date.Tested)
              ,xmax=max(sitespecificdata$Date.Tested)
              ,ymin=6,ymax=8.5
              ,color=light,fill=light)+
    geom_point(aes(x=Date.Tested,y=pH),color=main) +
    geom_line(aes(x=Date.Tested,y=pH),color=main) +
    geom_text(data = sitespecificdata %>% filter(Date.Tested == max(Date.Tested))
              ,aes(x=Date.Tested+1,y=pH),color=main
              ,label="pH",hjust=0,size=textlabelsize) +
    scale_y_continuous(lim=c(0,14),breaks=seq(0,14,1),labels=seq(0,14,1)) +
    ylab("pH") +
    labs(title=phtitle
         ,subtitle=phsubtitle) +
    theme_minimal() +
    theme(panel.grid = element_blank()
          ,panel.background = element_blank()
          ,plot.background = element_blank()
          ,axis.text = element_markdown()
          ,axis.title.x = element_blank()
          ,axis.line.y = element_line(color=dark)
          ,axis.ticks.y = element_line(color=dark)
          ,plot.title = element_markdown(size=titlesize,face="bold",margin=margin(t=titlemarginT,r=titlemarginR,b=titlemarginB,l=titlemarginL))
          ,plot.subtitle = element_markdown(size=subtitlesize)
          ,plot.caption = element_markdown(size=captionsize)
          ,plot.margin = margin(t=plotmarginT,r=plotmarginR,b=plotmarginB,l=plotmarginL))

hAndAGraph <-   sitespecificdata %>%
  ggplot(aes(x=Date.Tested,y=Total.Hardness)) +
  coord_cartesian(clip="off") +
  geom_point(aes(x=Date.Tested,y=Total.Hardness),color=main) +
  geom_point(aes(x=Date.Tested,y=Total.ALK),color=mid2) +
  geom_line(aes(x=Date.Tested,y=Total.Hardness),color=main) +
  geom_line(aes(x=Date.Tested,y=Total.ALK),color=mid2) +
  geom_text(data = sitespecificdata %>% filter(Date.Tested == max(Date.Tested))
            ,aes(x=Date.Tested+1,y=Total.Hardness),color=main
            ,label="Total Hardness",hjust=0,size=textlabelsize) +
  geom_text(data = sitespecificdata %>% filter(Date.Tested == max(Date.Tested))
            ,aes(x=Date.Tested+1,y=Total.ALK),color=mid2
            ,label="Total Alkalinity",hjust=0,size=textlabelsize) +
  scale_y_continuous(lim=c(0,max(clean$Total.ALK,na.rm=T))) +
  ylab("mg/L") +
  labs(title=hardnessalkalinitytitle
       ,subtitle=hAndASubtitle) +
  theme_minimal() +
  theme(panel.grid = element_blank()
        ,panel.background = element_blank()
        ,plot.background = element_blank()
        ,axis.text = element_markdown()
        ,axis.line.y = element_line(color=dark)
        ,axis.ticks.y = element_line(color=dark)
        ,axis.title.x = element_blank()
        ,plot.title = element_markdown(size=titlesize,face="bold",margin=margin(t=titlemarginT,r=titlemarginR,b=titlemarginB,l=titlemarginL))
        ,plot.subtitle = element_markdown(size=subtitlesize)
        ,plot.caption = element_markdown(size=captionsize)
        ,plot.margin = margin(t=plotmarginT,r=plotmarginR,b=plotmarginB,l=plotmarginL))





pdf(file = paste(directory,"Site",sitechosen,"_OnePager.pdf",sep="")
    ,paper="letter",width=8,height=11)
ggarrange(ggarrange(firstgraphA,firstgraphB,ncol=1,nrow=2),temperature
          ,phGraph,hAndAGraph
          ,phGraph,hAndAGraph
          ,nrow=3,ncol=2)

dev.off()










