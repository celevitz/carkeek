## Salmon Survey data
## Date written: 2023-11-11
## Date updated: 2023-11-11
## By: Carly Levitz
## Analyze

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


directory <- "/Users/carlylevitz/Documents/Data/carkeek/salmon/"
setwd(directory)

dark <- "#4b4f42"
mid2 <- "#6b625c"
mid <- "#9c8b94"
main <- "#7e7b3e"
light <- "#f0e0cc"
highlight <- "#46bdc6"

axistitlesize <- 20
axissize <- 7
titlesize <- 10
subtitlesize <- 8
captionsize <- 5
textlabelsize <- 2
plotmarginL <- 10
plotmarginR <- 40
plotmarginT <- 10
plotmarginB <- 10
titlemarginL <- 10
titlemarginR <- 10
titlemarginT <- 10
titlemarginB <- 10

logo <- paste(directory,"CWCAPlogo-1-white-1024x346.png",sep="")

## Step 2. Bring in the data
clean <- read.csv(paste0(directory,"SalmonSurveyAllYears.csv"),stringsAsFactors = FALSE)


## Step 3. Survey totals
totals <- clean %>%
  mutate(category = paste(Species,Type,sep="-")) %>%
  group_by(Year,category) %>%
  summarise(n=n())

totalstacked <- totals %>%
  ggplot(aes(x=as.factor(Year),y=n,color=category,fill=category)) +
  geom_bar(stat="identity")+
  ylab("number of fish") +
  labs(title="Number of fish by species and status over time")+
  theme_minimal() +
  theme(panel.grid = element_blank()
        ,panel.background = element_blank()
        ,plot.background = element_blank()
        ,axis.text = element_text(size=axissize,family=ft)
        ,axis.line.y = element_line(color=dark)
        ,axis.ticks.y = element_line(color=dark)
        ,axis.title.x = element_blank()
        ,axis.title.y = element_text(size=axissize,family=ft)
        ,plot.title = element_text(size=titlesize,face="bold",family=ft
                                   ,margin=margin(t=titlemarginT
                                                  ,r=titlemarginR
                                                  ,b=titlemarginB
                                                  ,l=titlemarginL))
        ,plot.subtitle = element_text(size=subtitlesize,family=ft)
        ,plot.caption = element_text(size=captionsize,family=ft)
        ,plot.margin = margin(t=plotmarginT,r=plotmarginR,b=plotmarginB
                              ,l=plotmarginL))

## Step 4. Count by time of year
weekly <- clean %>%
  mutate(date=as.Date(substr(Survey.Date,6,10),format="%m-%d")
         ,Year=as.factor(Year)) %>%
  group_by(Year,date) %>%
  summarise(n=n()) %>%
  ggplot(aes(x=date,y=n,color=Year)) +
  geom_line() +
  ylab("total number of fish") +
  labs(title="Total number of fish, dead and alive, each year")+
  theme_minimal() +
  theme(panel.grid = element_blank()
        ,panel.background = element_blank()
        ,plot.background = element_blank()
        ,axis.text = element_text(size=axissize,family=ft)
        ,axis.line.y = element_line(color=dark)
        ,axis.ticks.y = element_line(color=dark)
        ,axis.title.x = element_blank()
        ,axis.title.y = element_text(size=axissize,family=ft)
        ,plot.title = element_text(size=titlesize,face="bold",family=ft
                                   ,margin=margin(t=titlemarginT
                                                  ,r=titlemarginR
                                                  ,b=titlemarginB
                                                  ,l=titlemarginL))
        ,plot.subtitle = element_text(size=subtitlesize,family=ft)
        ,plot.caption = element_text(size=captionsize,family=ft)
        ,plot.margin = margin(t=plotmarginT,r=plotmarginR,b=plotmarginB
                              ,l=plotmarginL))

## Step 8. Predation
predationdata <- clean %>%
  filter(Species %in% c("Coho","Chum")) %>%
  mutate(Year = as.factor(Year)
         ,Predation = ifelse(is.na(Predation),"",Predation)) %>%
  group_by(Year,Predation,Species) %>%
  mutate(n=n()) %>%
  ungroup() %>% group_by(Year,Species) %>%
  mutate(N=n(),percent=n/N) %>%
  select(Year,Predation,Species,n,N,percent) %>%
  mutate(label=paste0(round(percent*100,1),"% (",n,")")) %>%
  distinct()

predationgraph1 <-
  predationdata %>%
  ggplot(aes(x=as.factor(Year),y=percent,color=Predation,fill=Predation,label=label)) +
  facet_wrap(~Species) +
  geom_bar(stat="identity")+
  geom_text(size = 2, position = position_stack(vjust = 0.5),color="black") +
  ylab("% of fish") +
  scale_y_continuous(lim=c(0,1),breaks=c(0,.25,.5,.75,1)
                     ,labels = c("0%","25%","50%","75%","100%")) +
  labs(title="Predation of each species over time: Percentages"
       ,subtitle = "Only chum and coho salmon")+
  theme_minimal() +
  theme(panel.grid = element_blank()
        ,panel.background = element_blank()
        ,plot.background = element_blank()
        ,axis.text = element_text(size=axissize,family=ft)
        ,axis.line.y = element_line(color=dark)
        ,axis.ticks.y = element_line(color=dark)
        ,axis.title.x = element_blank()
        ,axis.title.y = element_text(size=axissize,family=ft)
        ,plot.title = element_text(size=titlesize,face="bold",family=ft
                                   ,margin=margin(t=titlemarginT
                                                  ,r=titlemarginR
                                                  ,b=titlemarginB
                                                  ,l=titlemarginL))
        ,plot.subtitle = element_text(size=subtitlesize,family=ft)
        ,plot.caption = element_text(size=captionsize,family=ft)
        ,plot.margin = margin(t=plotmarginT,r=plotmarginR,b=plotmarginB
                              ,l=plotmarginL))

predationgraph2 <-
  predationdata %>%
  ggplot(aes(x=as.factor(Year),y=n,color=Predation,fill=Predation,label=n)) +
  facet_wrap(~Species) +
  geom_bar(stat="identity")+
  geom_text(size = 2, position = position_stack(vjust = 0.5),color="black") +
  ylab("% of fish") +
  #scale_y_continuous(lim=c(0,1),breaks=c(0,.25,.5,.75,1)
  #                   ,labels = c("0%","25%","50%","75%","100%")) +
  labs(title="Predation of each species over time: N"
       ,subtitle = "Only chum and coho salmon")+
  theme_minimal() +
  theme(panel.grid = element_blank()
        ,panel.background = element_blank()
        ,plot.background = element_blank()
        ,axis.text = element_text(size=axissize,family=ft)
        ,axis.line.y = element_line(color=dark)
        ,axis.ticks.y = element_line(color=dark)
        ,axis.title.x = element_blank()
        ,axis.title.y = element_text(size=axissize,family=ft)
        ,plot.title = element_text(size=titlesize,face="bold",family=ft
                                   ,margin=margin(t=titlemarginT
                                                  ,r=titlemarginR
                                                  ,b=titlemarginB
                                                  ,l=titlemarginL))
        ,plot.subtitle = element_text(size=subtitlesize,family=ft)
        ,plot.caption = element_text(size=captionsize,family=ft)
        ,plot.margin = margin(t=plotmarginT,r=plotmarginR,b=plotmarginB
                              ,l=plotmarginL))

## Step 10. Size data
sizedata <- clean %>%
  filter(Species %in% c("Chum","Coho") & Sex %in% c("Female","Male")) %>%
  mutate(category = paste(Species," (",Sex,")",sep=""))



Length<- ggplot(sizedata, aes(x=as.factor(Year), y=Length_Inches)) +
  facet_wrap(~category) +
  geom_boxplot() +
  ylab("Length (inches)") +
  labs(title="Length of fish by sex and species"
       ,subtitle="Only chum and coho salmon") +
  theme_minimal() +
  theme(panel.grid = element_blank()
        ,panel.background = element_blank()
        ,plot.background = element_blank()
        ,axis.text = element_text(size=axissize,family=ft)
        ,axis.line.y = element_line(color=dark)
        ,axis.ticks.y = element_line(color=dark)
        ,axis.title.x = element_blank()
        ,axis.title.y = element_text(size=axissize,family=ft)
        ,plot.title = element_text(size=titlesize,face="bold",family=ft
                                   ,margin=margin(t=titlemarginT
                                                  ,r=titlemarginR
                                                  ,b=titlemarginB
                                                  ,l=titlemarginL))
        ,plot.subtitle = element_text(size=subtitlesize,family=ft)
        ,plot.caption = element_text(size=captionsize,family=ft)
        ,plot.margin = margin(t=plotmarginT,r=plotmarginR,b=plotmarginB
                              ,l=plotmarginL))

Width <- ggplot(sizedata, aes(x=as.factor(Year), y=Width_Inches)) +
  facet_wrap(~category) +
  geom_boxplot()+
  ylab("Width (inches)") +
  labs(title="Width of fish by sex and species over time"
       ,subtitle="Only chum and coho salmon") +
  theme_minimal() +
  theme(panel.grid = element_blank()
        ,panel.background = element_blank()
        ,plot.background = element_blank()
        ,axis.text = element_text(size=axissize,family=ft)
        ,axis.line.y = element_line(color=dark)
        ,axis.ticks.y = element_line(color=dark)
        ,axis.title.x = element_blank()
        ,axis.title.y = element_text(size=axissize,family=ft)
        ,plot.title = element_text(size=titlesize,face="bold",family=ft
                                   ,margin=margin(t=titlemarginT
                                                  ,r=titlemarginR
                                                  ,b=titlemarginB
                                                  ,l=titlemarginL))
        ,plot.subtitle = element_text(size=subtitlesize,family=ft)
        ,plot.caption = element_text(size=captionsize,family=ft)
        ,plot.margin = margin(t=plotmarginT,r=plotmarginR,b=plotmarginB
                              ,l=plotmarginL))


## Step 11. Print the graphs
pdf(file = paste(directory,"SalmonSurveyAnalysis.pdf",sep="")
    ,paper="letter",width=8,height=11)

ggarrange(totalstacked,weekly,ncol=1,nrow=2)
ggarrange(predationgraph1,predationgraph2,ncol=1,nrow=2)
ggarrange(Length,Width,ncol=1,nrow=2)

dev.off()

