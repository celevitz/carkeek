## Author: Carly Levitz
## Written: 2023-06-20
## Updated: 2023-06-23
## Purpose: create visualizations of Carkeek Watershed data


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

directory <- "/Users/carlylevitz/Documents/Data/carkeek/"
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

## Step 2: Bring in data and clean it
### Step 2a. Bring in data
clean <- read.csv(paste0(directory,"H20Data.csv"),stringsAsFactors = FALSE)

### Step 2b. Clean data
  clean$dateTested <- as.Date(clean$dateTested)

## Step 3: set up things for visualizations

# For testing
  sitechosen <- 5

# write the info about each measure:
overallsubtitle <- str_wrap(str_glue("If you are interested in volunteering,
                          have questions, or would like to donate, please email:
                          info (at) CarkeekWatershed.org"),80)
overallcaption <- str_glue("Analyzed by Carly Levitz and supported by
                         Troy Beckner")

temptitle <- str_glue("Air temperature")
tempsubtitle <- str_wrap(str_glue("Air temperature affects water temperature.
                                  (This graph will show other weather when
                                  data on weather are captured.)"),65)
phtitle <- str_glue("pH levels")
phsubtitle <- str_wrap(str_glue("pH measures how acidic or basic water is. A
                              value of 7 is neutral. Less than 7 is acidic,
                              and more than 7 is basic. pH can change over
                              the course of a season (or a day). A pH less
                              than 4.0 or more than 11.0 is usually lethal to
                              fish and other organisms. pH between 6 and 8.5
                              is usually ideal."),65)

hardnessalkalinitytitle <- str_glue("Hardness and Alkilinity")
hAndASubtitle <- str_wrap(str_glue("Higher alkalinity provides a buffer against
                                 changes in pH, making it more stable for
                                 aquatic life. Hardness is primarily the
                                 concentration of calcium and magnesium ions
                                 in water."),65)

DOtitle <- str_wrap("Dissolved Oxygen (DO) and Water Temperature",40)
DOsubtitle <- str_wrap(str_glue("Water temperature can affect the breeding and
                        feeding of aquatic animals. It also affects how much
                        dissolved oxygen the water can hold. Aquatic animals and
                        plants need oxygen to survive. Dissolved oxygen (DO)
                        measures how much oxygen is in the water. A value of 5
                        parts per million (ppm) is considered 'good' and values
                        between 7 and 11 are 'very good.' DO decreases with
                        higher temperatures."),65)

bacteriaTitle <- str_wrap("Bacteria levels",40)
bacteriaSubtitle <- str_wrap(str_glue("E Coli & Coliform"),65)

sitepdf <- function(sitechosen) {

## The idea is: for each site, have the logo and a general description at the
## top. Then, 6 graphs - overall statement, temperature, ph, total hardness &
## total alkalinity, dissolved oxygen, and turbidity.
## Also need to put in ecoli & coliform

# Step 3a. Create the data for just the specific site
  # and then write a little bit about the site
sitespecificdata <- clean %>% filter(siteNumber == sitechosen) %>%
  arrange(dateTested)

volunteers <- sitespecificdata %>%
  select(tester1,tester2,dateTested) %>%
  mutate(id = row.names(sitespecificdata)) %>%
  pivot_longer(!c(id,dateTested),names_to = "volunteerNumber"
               ,values_to="volunteer") %>%
  filter(!(is.na(volunteer))) %>%
  group_by(volunteer) %>%
  mutate(count = n() ) %>%
  ungroup() %>%
  mutate(min = min(count),max=max(count),avg=round(mean(count),1)) %>%
  select(min,max,avg) %>%
  distinct()
volunteercountrange <-str_glue("Individuals volunteered between {volunteers$min}
                              and {volunteers$max} times, with an average of
                              {volunteers$avg}.")

numberoftimeswithtwopeople <- str_glue("{nrow(sitespecificdata %>%
  filter(!(is.na(tester1)) & !(is.na(tester2))) )} of these times had
                                     two volunteers, which is best practice.")

volunteerhours <- sitespecificdata %>%
  select(tester1,tester2,dateTested) %>%
  mutate(id = row.names(sitespecificdata)) %>%
  pivot_longer(!c(id,dateTested),names_to = "volunteerNumber"
               ,values_to="volunteer") %>%
  filter(!(is.na(volunteer))) %>%
  group_by(volunteer) %>%
  summarise(count = n() ,hours=2*count) %>%
  ungroup() %>%
  summarise(totalhours = sum(hours))
volunteertime <- str_glue("This is an estimated {volunteerhours$totalhours}
                          hours of volunteer time.")

# Step 3b. Write titles and such for each graph
nameofsite <- unique(sitespecificdata$waterbody)
numberoftests <- nrow(sitespecificdata)
numberofvolunteers <- length(unique(c(unique(sitespecificdata$tester1[
                                      !(is.na(sitespecificdata$tester1))])
                                  ,unique(sitespecificdata$tester2[
                                    !(is.na(sitespecificdata$tester2))]))))

overalltitle <- str_wrap(str_glue("{nameofsite} (Site #{sitechosen}) has been
                        tested {numberoftests} times by {numberofvolunteers}
                        volunteers. {numberoftimeswithtwopeople} Thank you,
                                  volunteers!" ),40)

# Step 3c. first plot is a combined plot: logo + description
# create a placeholder dataframe for the logo so we can use it firstgraphB
info <- data.frame(x = 1,y = 1,image = logo)

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
  labs(title=str_wrap(overalltitle,width=50)
       ,subtitle=overallsubtitle
       ,caption=overallcaption) +
  theme_minimal() +
  theme(panel.grid = element_blank()
        ,panel.background = element_blank()
        ,plot.background = element_blank()
        ,axis.text = element_blank()
        ,axis.line = element_blank()
        ,axis.ticks = element_blank()
        ,axis.title = element_blank()
        ,plot.title = element_text(size=titlesize,face="bold"
                                       ,margin=margin(t=titlemarginT
                                                      ,r=titlemarginR
                                                      ,b=titlemarginB
                                                      ,l=titlemarginL)
                                       ,hjust=0,color=main)
        ,plot.subtitle = element_text(size=subtitlesize
                                          ,hjust=0,color=mid2)
        ,plot.caption = element_markdown(size=captionsize,color=mid2)
        ,plot.margin = margin(t=plotmarginT,r=plotmarginR,b=plotmarginB
                              ,l=plotmarginL))

# Step 3d. Temperature graph
temperature <-
  sitespecificdata %>%
  ggplot(aes(x=dateTested,y=waterTemp)) +
  coord_cartesian(clip="off") +
  # geom_rect(xmin=min(sitespecificdata$dateTested)
  #           ,xmax=max(sitespecificdata$dateTested)
  #           ,ymin=12.8,ymax=17.8
  #           ,color=light,fill=light)+
  # geom_point(aes(x=dateTested+1,y=waterTemp),color=main) +
  geom_point(aes(x=dateTested,y=airTemp),color=mid2) +
  # geom_line(aes(x=dateTested+1,y=waterTemp),color=main) +
  geom_line(aes(x=dateTested,y=airTemp),color=mid2) +
  # geom_text(data = sitespecificdata %>% filter(dateTested == max(dateTested))
  #            ,aes(x=dateTested+1,y=waterTemp),color=main
  #           ,label="Water temperature",hjust=0,size=textlabelsize) +
  geom_text(data = sitespecificdata %>% filter(dateTested == max(dateTested))
            ,aes(x=dateTested+1,y=airTemp),color=mid2
            ,label="Air temperature",hjust=0,size=textlabelsize) +
  scale_y_continuous(lim=c(0,27)) +
  scale_x_date(date_labels = "%b %y",date_breaks = "month") +
  ylab("Temperature (Celsius)") +
  labs(title=temptitle
       ,subtitle=tempsubtitle) +
  theme_minimal() +
  theme(panel.grid = element_blank()
        ,panel.background = element_blank()
        ,plot.background = element_blank()
        ,axis.text = element_markdown(size=axissize)
        ,axis.line.y = element_line(color=dark)
        ,axis.ticks.y = element_line(color=dark)
        ,axis.title.x = element_blank()
        ,plot.title = element_text(size=titlesize,face="bold"
                                       ,margin=margin(t=titlemarginT
                                                      ,r=titlemarginR
                                                      ,b=titlemarginB
                                                      ,l=titlemarginL))
        ,plot.subtitle = element_text(size=subtitlesize)
        ,plot.caption = element_markdown(size=captionsize)
        ,plot.margin = margin(t=plotmarginT,r=plotmarginR,b=plotmarginB
                              ,l=plotmarginL))

# Step 3e. pH graph
phGraph <-
  sitespecificdata %>%
    ggplot(aes(x=dateTested,y=pH)) +
    coord_cartesian(clip="off") +
    geom_rect(xmin=min(sitespecificdata$dateTested)
              ,xmax=max(sitespecificdata$dateTested)
              ,ymin=6,ymax=8.5
              ,color=light,fill=light)+
    geom_point(aes(x=dateTested,y=pH),color=main) +
    geom_line(aes(x=dateTested,y=pH),color=main) +
    scale_y_continuous(lim=c(0,14),breaks=seq(0,14,1),labels=seq(0,14,1)) +
    scale_x_date(date_labels = "%b %y",date_breaks = "month") +
    ylab("pH") +
    labs(title=phtitle
         ,subtitle=phsubtitle) +
    theme_minimal() +
    theme(panel.grid = element_blank()
          ,panel.background = element_blank()
          ,plot.background = element_blank()
          ,axis.text = element_markdown(size=axissize)
          ,axis.title.x = element_blank()
          ,axis.line.y = element_line(color=dark)
          ,axis.ticks.y = element_line(color=dark)
          ,plot.title = element_text(size=titlesize
                                         ,face="bold"
                                         ,margin=margin(t=titlemarginT
                                                        ,r=titlemarginR
                                                        ,b=titlemarginB
                                                        ,l=titlemarginL))
          ,plot.subtitle = element_text(size=subtitlesize)
          ,plot.caption = element_markdown(size=captionsize)
          ,plot.margin = margin(t=plotmarginT,r=plotmarginR,b=plotmarginB
                                ,l=plotmarginL))

# Step 3f. Hardness & Alkalinity graph
hAndAGraph <-   sitespecificdata %>%
  ggplot(aes(x=dateTested,y=totalHardness)) +
  coord_cartesian(clip="off") +
  geom_point(aes(x=dateTested,y=totalHardness),color=main) +
  geom_point(aes(x=dateTested,y=totalAlk),color=mid2) +
  geom_line(aes(x=dateTested,y=totalHardness),color=main) +
  geom_line(aes(x=dateTested,y=totalAlk),color=mid2) +
  geom_text(data = sitespecificdata %>% filter(dateTested == max(dateTested))
            ,aes(x=dateTested+1,y=totalHardness),color=main
            ,label="Total Hardness",hjust=0,size=textlabelsize) +
  geom_text(data = sitespecificdata %>% filter(dateTested == max(dateTested))
            ,aes(x=dateTested+1,y=totalAlk),color=mid2
            ,label="Total Alkalinity",hjust=0,size=textlabelsize) +
  scale_y_continuous(lim=c(0,max(clean$totalAlk,na.rm=T))) +
  scale_x_date(date_labels = "%b %y",date_breaks = "month") +
  ylab("mg/L") +
  labs(title=hardnessalkalinitytitle
       ,subtitle=hAndASubtitle) +
  theme_minimal() +
  theme(panel.grid = element_blank()
        ,panel.background = element_blank()
        ,plot.background = element_blank()
        ,axis.text = element_markdown(size=axissize)
        ,axis.line.y = element_line(color=dark)
        ,axis.ticks.y = element_line(color=dark)
        ,axis.title.x = element_blank()
        ,plot.title = element_text(size=titlesize,face="bold"
                                       ,margin=margin(t=titlemarginT
                                                      ,r=titlemarginR
                                                      ,b=titlemarginB
                                                      ,l=titlemarginL))
        ,plot.subtitle = element_text(size=subtitlesize)
        ,plot.caption = element_markdown(size=captionsize)
        ,plot.margin = margin(t=plotmarginT,r=plotmarginR,b=plotmarginB
                              ,l=plotmarginL))

# Step 3g. Bacteria levels
bacteriaGraph <- sitespecificdata %>%
  ggplot(aes(x=dateTested)) +
  coord_cartesian(clip="off") +
  geom_point(aes(x=dateTested,y=avgColiform/10),color=main) +
  geom_line(aes(x=dateTested,y=avgColiform/10),color=main) +
  geom_point(aes(x=dateTested,y=avgEcoli),color=mid2) +
  geom_line(aes(x=dateTested,y=avgEcoli),color=mid2) +
  scale_y_continuous(
    # Features of the first axis
    name = "Average E Coli",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~., name="Average Coliform"
                        ,breaks=seq(0,25,5)
                        ,label=seq(0,250,50))  ) +
  scale_x_date(date_labels = "%b %y",date_breaks = "month") +
  labs(title=bacteriaTitle,subtitle=bacteriaSubtitle) +
  theme_minimal() +
  theme(panel.grid = element_blank()
        ,panel.background = element_blank()
        ,plot.background = element_blank()
        ,axis.title.x = element_blank()
        ,axis.text = element_markdown(size=axissize)
        ,axis.line.y.left = element_line(color=dark)
        ,axis.ticks.y.left = element_line(color=dark)
        ,axis.title.y.left = element_text(color=main)
        ,axis.title.y.right = element_text(color=mid2)
        ,axis.line.y.right = element_line(color=mid2)
        ,axis.ticks.y.right = element_line(color=mid2)
        ,plot.title = element_text(size=titlesize,face="bold"
                                   ,margin=margin(t=titlemarginT
                                                  ,r=titlemarginR
                                                  ,b=titlemarginB
                                                  ,l=titlemarginL))
        ,plot.subtitle = element_text(size=subtitlesize)
        ,plot.caption = element_markdown(size=captionsize)
        ,plot.margin = margin(t=plotmarginT,r=plotmarginR,b=plotmarginB
                              ,l=plotmarginL))


# Step 3h. dissolved oxygen
DOgraph <- sitespecificdata %>%
  ggplot(aes(x=dateTested,y=averageDo)) +
  coord_cartesian(clip="off") +
  geom_rect(xmin=min(sitespecificdata$dateTested)
            ,xmax=max(sitespecificdata$dateTested)
            ,ymin=7,ymax=11
            ,color=light,fill=light)+
  geom_point(aes(x=dateTested,y=averageDo),color=main) +
  #geom_point(aes(x=dateTested,y=OxSat),color=mid2) +
  geom_point(aes(x=dateTested,y=waterTemp),color=mid2) +
  geom_line(aes(x=dateTested,y=averageDo),color=main) +
  #geom_line(aes(x=dateTested,y=OxSat),color=mid2) +
  geom_line(aes(x=dateTested,y=waterTemp),color=mid2) +
  geom_text(data = sitespecificdata %>% filter(dateTested == max(dateTested))
            ,aes(x=dateTested+1,y=averageDo),color=main
            ,label="Average dissolved\noxygen",hjust=0,size=textlabelsize) +
  # geom_text(data = sitespecificdata %>% filter(dateTested == max(dateTested))
  #           ,aes(x=dateTested,y=OxSat),color=mid2
  #           ,label="% oxygen saturation",hjust=0,size=textlabelsize) +
  geom_text(data = sitespecificdata %>% filter(dateTested == max(dateTested))
            ,aes(x=dateTested+1,y=waterTemp),color=mid2
            ,label="Water temperature",hjust=0,size=textlabelsize) +
  scale_y_continuous(lim=c(0,max(clean$averageDo,na.rm=T))) +
  scale_x_date(date_labels = "%b %y",date_breaks = "month") +
  ylab("DO parts per million\nWater temperature (Celsius)") +
  labs(title=DOtitle
       ,subtitle=DOsubtitle) +
  theme_minimal() +
  theme(panel.grid = element_blank()
        ,panel.background = element_blank()
        ,plot.background = element_blank()
        ,axis.text = element_markdown(size=axissize)
        ,axis.line.y = element_line(color=dark)
        ,axis.ticks.y = element_line(color=dark)
        ,axis.title.x = element_blank()
        ,plot.title = element_text(size=titlesize,face="bold"
                                   ,margin=margin(t=titlemarginT
                                                  ,r=titlemarginR
                                                  ,b=titlemarginB
                                                  ,l=titlemarginL))
        ,plot.subtitle = element_text(size=subtitlesize)
        ,plot.caption = element_markdown(size=captionsize)
        ,plot.margin = margin(t=plotmarginT,r=plotmarginR,b=plotmarginB
                              ,l=plotmarginL))

# Step 3i. Bring all the graphs together
ggarrange(ggarrange(firstgraphA,firstgraphB,ncol=1,nrow=2),temperature
          ,phGraph,DOgraph
          ,hAndAGraph,bacteriaGraph
          ,nrow=3,ncol=2)

}

## Step 4: loop through the sites and print the PDFs

for (sitechosen in seq(1,8,1)) {

  pdf(file = paste(directory,"Site",sitechosen,"_OnePager.pdf",sep="")
      ,paper="letter",width=8,height=11)

  sitepdf(sitechosen)

  dev.off()
}









