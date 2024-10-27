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

################################################################################
## Volunteer information over time: tests, unique volunteers, volunteer hours
################################################################################

quarterlyTests <- clean %>%
  group_by(year, quarter) %>%
  mutate(`Number of tests` = n()) %>%
  select(year,quarter,`Number of tests`) %>%
  distinct()

quarterlyTests %>% ungroup()  %>% summarise(average = mean(`Number of tests`))

quarterlyVols <- clean %>%
  select(year,quarter,tester1,tester2) %>%
  pivot_longer(!c(year,quarter),names_to="tester") %>%
  filter(!(is.na(value))) %>%
  select(!tester) %>%
  group_by(year,quarter,value) %>%
  mutate(n=n()) %>%
  distinct() %>%
  ungroup() %>% group_by(year,quarter) %>%
  mutate(`Number of unique volunteers`=n()) %>%
  select(year,quarter,`Number of unique volunteers`) %>%
  distinct()

clean %>%
  select(year,quarter,tester1,tester2) %>%
  pivot_longer(!c(year,quarter),names_to="tester") %>%
  filter(!(is.na(value))) %>%
  select(!tester) %>%
  group_by(year,value) %>%
  mutate(n=n()) %>%
  distinct()  %>%
  ungroup() %>% group_by(year) %>%
  mutate(`Number of unique volunteers`=n()) %>%
  select(year,`Number of unique volunteers`) %>%
  distinct()


quarterly2testers <- clean %>%
  mutate(numberoftesters = ifelse(is.na(tester2),0,1) ) %>%
  group_by(year,quarter) %>%
  summarise(`Number of tests completed by a pair` = sum(numberoftesters))

quarterlyPeopleHours <- clean %>%
  mutate(numberoftesters = ifelse(is.na(tester2),1,2)
         ,peoplehours = numberoftesters*2)  %>%
  group_by(year,quarter) %>%
  summarise(`Total volunteer hours (assuming two hours per person per test)` = sum(peoplehours))

quarterly <- quarterlyTests %>%
  left_join(quarterly2testers) %>%
  left_join(quarterlyVols) %>%
  left_join(quarterlyPeopleHours) %>%
  mutate(time = paste0(as.character(year)," ",quarter)) %>%
  pivot_longer(!c(year,quarter,time),names_to="variable",values_to= "value")

quarterlyVolunteergraph <-

  quarterly %>%
  ggplot(aes(x=time,y=value)) +
  facet_wrap(~variable,ncol=1,scales="free_y")+
  geom_point(color=main) +
  geom_text(aes(label=value),nudge_y = 1,family = ft,color=main,size=smalltextsize) +
  ggtitle("Carkeek Watershed Community Action Project:\nQuarterly Water Testing Volunteer Summary") +
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


################################################################################
## Site information
################################################################################
# how many times was each site tested?
siteTests <- clean %>%
  group_by(year,quarter,siteNumber) %>%
  summarise(`Number of times this site was tested`=n()) %>%
  mutate(time = paste0(as.character(year)," ",quarter)
         ,label=paste0("Site ",siteNumber))

numberoftimessitesweretested <-
  siteTests %>%
  ggplot(aes(x=time,y=`Number of times this site was tested`)) +
  facet_wrap(~label,ncol=2 )+
  geom_point(color=main) +
  #geom_text(aes(label=`Number of times this site was tested`)
  #          ,nudge_y = 1,family = ft,color=main,size=smalltextsize) +
  ggtitle("Number of tests done by site") +
  xlab("") +
  scale_y_continuous(lim=c(0,7)) +
  theme_minimal() +
  theme(
    strip.background = element_rect(fill=light,color=light)
    ,strip.text = element_text(family=ft,color=dark,size=subtitlesize)
    ,title = element_text(family=ft,color=main,size=titlesize)
    ,axis.title = element_text(family=ft,color=main,size=subtitlesize)
    ,axis.text.y = element_text(family=ft,color=main,size=subtitlesize)
    ,axis.text.x = element_text(family=ft,color=main,size=textsize)
    ,plot.caption = element_text(family = ft,hjust=0
                                 ,color=main,size=subtitlesize)
    ,panel.grid = element_blank()
    ,axis.line = element_line(color=dark)
    ,axis.ticks = element_line(color=dark)
  )



################################################
## Measures with values

# remove waterbody na
nonawaterbody <- clean %>% filter(!(is.na(waterbody)))

# Ox Sat
oxsat <-
  ggplot(nonawaterbody,aes(x=dateTested,y=OxSat,color=siteNumber,shape=siteNumber)) +
  geom_point() +
  geom_line() +
  facet_wrap(~waterbody) +
  scale_shape_manual(values = c("1"=1,"2"=2,"3"=3,"4"=4
                                ,"5"=5,"6"=6,"7"=18,"8"=20)) +
  scale_y_continuous(lim=c(0.55,1.3)) +
  scale_color_manual(values=c("#E69F00","#56B4E9","#009E73","gray74"
                              ,"#F0E442","#0072B2","#D55E00","#CC79A7")) +
  #theme
  ggtitle("Oxygen Saturation by Waterbody") +
  xlab("Date tested") + ylab("Oxygen saturation") +
  theme_minimal() +
  guides(shape=guide_legend(title="Site")
         ,color=guide_legend(title="Site")) +
  theme(panel.grid = element_blank()
        ,axis.text.x=element_text(family=ft,size=smalltextsize)
        #,axis.line = element_line(col="black")
        #,axis.tick = element_line(col=dark)
        #,plot.caption = element_text(family = ft,hjust=0,color=dark)
  )

# pH
pHgraph <-
  ggplot(nonawaterbody,aes(x=dateTested,y=as.numeric(pH),color=siteNumber,shape=siteNumber)) +
  geom_point() +
  geom_line() +
  facet_wrap(~waterbody) +
  scale_shape_manual(values = c("1"=1,"2"=2,"3"=3,"4"=4
                                ,"5"=5,"6"=6,"7"=18,"8"=20)) +
  scale_y_continuous(lim=c(6.5,9)) +
  scale_color_manual(values=c("#E69F00","#56B4E9","#009E73","gray74"
                              ,"#F0E442","#0072B2","#D55E00","#CC79A7")) +
  #theme
  ggtitle("pH by Waterbody") +
  xlab("Date tested") + ylab("pH") +
  theme_minimal() +
  guides(shape=guide_legend(title="Site")
         ,color=guide_legend(title="Site")) +
  theme(panel.grid = element_blank()
        ,axis.text.x=element_text(family=ft,size=smalltextsize)
        #,axis.line = element_line(col="black")
        #,axis.tick = element_line(col=dark)
        #,plot.caption = element_text(family = ft,hjust=0,color=dark)
  )

averagedograph <-
  ggplot(nonawaterbody,aes(x=dateTested,y=averageDo,color=siteNumber,shape=siteNumber)) +
  geom_point() +
  geom_line() +
  facet_wrap(~waterbody) +
  scale_shape_manual(values = c("1"=1,"2"=2,"3"=3,"4"=4
                                ,"5"=5,"6"=6,"7"=18,"8"=20)) +
  scale_y_continuous(lim=c(6.5,15)) +
  scale_color_manual(values=c("#E69F00","#56B4E9","#009E73","gray74"
                              ,"#F0E442","#0072B2","#D55E00","#CC79A7")) +
  #theme
  ggtitle("Average Dissolved Oxygen (DO) by Waterbody") +
  xlab("Date tested") + ylab("Average DO") +
  theme_minimal() +
  guides(shape=guide_legend(title="Site")
         ,color=guide_legend(title="Site")) +
  theme(panel.grid = element_blank()
        ,axis.text.x=element_text(family=ft,size=smalltextsize)
        #,axis.line = element_line(col="black")
        #,axis.tick = element_line(col=dark)
        #,plot.caption = element_text(family = ft,hjust=0,color=dark)
  )

alkalinitygraph <-
  ggplot(nonawaterbody,aes(x=dateTested,y=totalAlk,color=siteNumber,shape=siteNumber)) +
  geom_point() +
  geom_line() +
  facet_wrap(~waterbody) +
  scale_shape_manual(values = c("1"=1,"2"=2,"3"=3,"4"=4
                                ,"5"=5,"6"=6,"7"=18,"8"=20)) +
  scale_y_continuous(lim=c(20,300)) +
  scale_color_manual(values=c("#E69F00","#56B4E9","#009E73","gray74"
                              ,"#F0E442","#0072B2","#D55E00","#CC79A7")) +
  #theme
  ggtitle("Alkalinity by Waterbody") +
  xlab("Date tested") + ylab("Alkalinity") +
  theme_minimal() +
  guides(shape=guide_legend(title="Site")
         ,color=guide_legend(title="Site")) +
  theme(panel.grid = element_blank()
        ,axis.text.x=element_text(family=ft,size=smalltextsize)
        #,axis.line = element_line(col="black")
        #,axis.tick = element_line(col=dark)
        #,plot.caption = element_text(family = ft,hjust=0,color=dark)
  )

hardnessgraph <-
  ggplot(nonawaterbody,aes(x=dateTested,y=totalHardness,color=siteNumber,shape=siteNumber)) +
  geom_point() +
  geom_line() +
  facet_wrap(~waterbody) +
  scale_shape_manual(values = c("1"=1,"2"=2,"3"=3,"4"=4
                                ,"5"=5,"6"=6,"7"=18,"8"=20)) +
  scale_y_continuous(lim=c(35,155)) +
  scale_color_manual(values=c("#E69F00","#56B4E9","#009E73","gray74"
                              ,"#F0E442","#0072B2","#D55E00","#CC79A7")) +
  #theme
  ggtitle("Hardness by Waterbody") +
  xlab("Date tested") + ylab("Hardness") +
  theme_minimal() +
  guides(shape=guide_legend(title="Site")
         ,color=guide_legend(title="Site")) +
  theme(panel.grid = element_blank()
        ,axis.text.x=element_text(family=ft,size=smalltextsize)
        #,axis.line = element_line(col="black")
        #,axis.tick = element_line(col=dark)
        #,plot.caption = element_text(family = ft,hjust=0,color=dark)
  )

ecoligraph <-
  ggplot(nonawaterbody,aes(x=dateTested,y=avgEcoli,color=siteNumber,shape=siteNumber)) +
  geom_point() +
  geom_line() +
  facet_wrap(~waterbody) +
  scale_shape_manual(values = c("1"=1,"2"=2,"3"=3,"4"=4
                                ,"5"=5,"6"=6,"7"=18,"8"=20)) +
  scale_y_continuous(lim=c(0,155)) +
  scale_color_manual(values=c("#E69F00","#56B4E9","#009E73","gray74"
                              ,"#F0E442","#0072B2","#D55E00","#CC79A7")) +
  #theme
  ggtitle("E Coli by Waterbody") +
  xlab("Date tested") + ylab("Average E Coli") +
  theme_minimal() +
  guides(shape=guide_legend(title="Site")
         ,color=guide_legend(title="Site")) +
  theme(panel.grid = element_blank()
        ,axis.text.x=element_text(family=ft,size=smalltextsize)
        #,axis.line = element_line(col="black")
        #,axis.tick = element_line(col=dark)
        #,plot.caption = element_text(family = ft,hjust=0,color=dark)
  )

turbiditygraph <-
  ggplot(nonawaterbody,aes(x=dateTested,y=turbidityJTU,color=siteNumber,shape=siteNumber)) +
  geom_point() +
  geom_line() +
  facet_wrap(~waterbody) +
  scale_shape_manual(values = c("1"=1,"2"=2,"3"=3,"4"=4
                                ,"5"=5,"6"=6,"7"=18,"8"=20)) +
  scale_y_continuous(lim=c(0,245)) +
  scale_color_manual(values=c("#E69F00","#56B4E9","#009E73","gray74"
                              ,"#F0E442","#0072B2","#D55E00","#CC79A7")) +
  #theme
  ggtitle("Turbidity by Waterbody") +
  xlab("Date tested") + ylab("Turbidity JTU") +
  theme_minimal() +
  guides(shape=guide_legend(title="Site")
         ,color=guide_legend(title="Site")) +
  theme(panel.grid = element_blank()
        ,axis.text.x=element_text(family=ft,size=smalltextsize)
        #,axis.line = element_line(col="black")
        #,axis.tick = element_line(col=dark)
        #,plot.caption = element_text(family = ft,hjust=0,color=dark)
  )

airtempgraph <-
  ggplot(nonawaterbody,aes(x=dateTested,y=airTemp,color=siteNumber,shape=siteNumber)) +
  geom_point() +
  geom_line() +
  facet_wrap(~waterbody) +
  scale_shape_manual(values = c("1"=1,"2"=2,"3"=3,"4"=4
                                ,"5"=5,"6"=6,"7"=18,"8"=20)) +
  scale_y_continuous(lim=c(-5,27)) +
  scale_color_manual(values=c("#E69F00","#56B4E9","#009E73","gray74"
                              ,"#F0E442","#0072B2","#D55E00","#CC79A7")) +
  #theme
  ggtitle("Air temperature by Waterbody") +
  xlab("Date tested") + ylab("Air temperature (celsius)") +
  theme_minimal() +
  guides(shape=guide_legend(title="Site")
         ,color=guide_legend(title="Site")) +
  theme(panel.grid = element_blank()
        ,axis.text.x=element_text(family=ft,size=smalltextsize)
        #,axis.line = element_line(col="black")
        #,axis.tick = element_line(col=dark)
        #,plot.caption = element_text(family = ft,hjust=0,color=dark)
  )

watertempgraph <-
  ggplot(nonawaterbody,aes(x=dateTested,y=waterTemp,color=siteNumber,shape=siteNumber)) +
  geom_point() +
  geom_line() +
  facet_wrap(~waterbody) +
  scale_shape_manual(values = c("1"=1,"2"=2,"3"=3,"4"=4
                                ,"5"=5,"6"=6,"7"=18,"8"=20)) +
  scale_y_continuous(lim=c(-5,27)) +
  scale_color_manual(values=c("#E69F00","#56B4E9","#009E73","gray74"
                              ,"#F0E442","#0072B2","#D55E00","#CC79A7")) +
  #theme
  ggtitle("Water temperature by Waterbody") +
  xlab("Date tested") + ylab("Water temperature (celsius)") +
  theme_minimal() +
  guides(shape=guide_legend(title="Site")
         ,color=guide_legend(title="Site")) +
  theme(panel.grid = element_blank()
        ,axis.text.x=element_text(family=ft,size=smalltextsize)
        #,axis.line = element_line(col="black")
        #,axis.tick = element_line(col=dark)
        #,plot.caption = element_text(family = ft,hjust=0,color=dark)
  )

###############################################################################
pdf(file = paste(directory,"CarkeekWatershedTesting_QuarterlyGraphs.pdf",sep="")
    ,paper="letter",width=8,height=11)

quarterlyVolunteergraph
numberoftimessitesweretested

ggarrange(oxsat,pHgraph,averagedograph,nrow=3,ncol=1)
ggarrange(alkalinitygraph,hardnessgraph,ecoligraph,nrow=3,ncol=1)
ggarrange(turbiditygraph,airtempgraph,watertempgraph,nrow=3,ncol=1)

dev.off()
