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
rawdata <- as_tibble(read.xlsx(paste(directory,"H20 Data as of May 2024.xlsx"
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


## Step 3: set up things for visualizations
## For now, just do it for site 1.

sitechosen <- 5
#pdf(file = paste(directory,"Site",sitechosen,"_OnePager.pdf",sep=""),paper="letter",width=8,height=11)

sitepdf <- function(sitechosen) {

## The idea is: for each site, have the logo and a general description at the
## top. Then, 6 graphs - overall statement, temperature, ph, total hardness &
## total alkalinity, dissolved oxygen, and turbidity.
## Also need to put in ecoli & coliform

# Step 3a. Create the data for just the specific site
  # and then write a little bit about the site
sitespecificdata <- clean %>% filter(`Site.#` == sitechosen) %>%
  arrange(Date.Tested)

volunteers <- sitespecificdata %>%
  select(`Tester.#1`,`Tester.#2`,Date.Tested) %>%
  mutate(id = row.names(sitespecificdata)) %>%
  pivot_longer(!c(id,Date.Tested),names_to = "volunteerNumber"
               ,values_to="volunteer") %>%
  filter(!(is.na(volunteer))) %>%
  group_by(volunteer) %>%
  mutate(count = n() ) %>%
  ungroup() %>%
  mutate(min = min(count),max=max(count),avg=round(mean(count),1)) %>%
  select(min,max,avg) %>%
  distinct()
volunteercountrange <- str_glue("Individuals volunteered between {volunteers$min}
                              and {volunteers$max} times, with an<br>average of
                              {volunteers$avg}.")

numberoftimeswithtwopeople <- str_glue("{nrow(sitespecificdata %>%
  filter(!(is.na(`Tester.#1`)) & !(is.na(`Tester.#2`))) )} of these times had
                                       two volunteers,<br>which is best practice.")

volunteerhours <- sitespecificdata %>%
  select(`Tester.#1`,`Tester.#2`,Date.Tested) %>%
  mutate(id = row.names(sitespecificdata)) %>%
  pivot_longer(!c(id,Date.Tested),names_to = "volunteerNumber"
               ,values_to="volunteer") %>%
  filter(!(is.na(volunteer))) %>%
  group_by(volunteer) %>%
  summarise(count = n() ,hours=2*count) %>%
  ungroup() %>%
  summarise(totalhours = sum(hours))
volunteertime <- str_glue("This is an estimated {volunteerhours$totalhours}
                          hours of<br>volunteer time.")

# Step 3b. Write titles and such for each graph
nameofsite <- unique(sitespecificdata$Waterbody)
numberoftests <- length(unique(sitespecificdata$Date.Tested))
numberofvolunteers <- length(unique(c(unique(sitespecificdata$`Tester.#1`[
                                      !(is.na(sitespecificdata$`Tester.#1`))])
                                  ,unique(sitespecificdata$`Tester.#2`[
                                    !(is.na(sitespecificdata$`Tester.#2`))]))))


overalltitle <- str_glue("{nameofsite} (Site #{sitechosen}) has been tested
                        {numberoftests} times<br>
                        by {numberofvolunteers} volunteers.
                         {numberoftimeswithtwopeople}<br><br>
                        {volunteercountrange}
                        {volunteertime}<br><br>
                        Thank you, volunteers!" )
overallsubtitle <- str_glue("If you are interested in volunteering, have
                            questions, or would like<br>to donate: email
                            info at CarkeekWatershed.org")
overallcaption <- str_glue("Analyzed by Carly Levitz and supported by
                           Troy Beckner")

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
        ,plot.title = element_markdown(size=titlesize,face="bold"
                                       ,margin=margin(t=titlemarginT
                                                      ,r=titlemarginR
                                                      ,b=titlemarginB
                                                      ,l=titlemarginL)
                                       ,hjust=0,color=main)
        ,plot.subtitle = element_markdown(size=subtitlesize
                                          ,hjust=0,color=mid2)
        ,plot.caption = element_markdown(size=captionsize,color=mid2)
        ,plot.margin = margin(t=plotmarginT,r=plotmarginR,b=plotmarginB
                              ,l=plotmarginL))

# Step 3d. Temperature graph
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
        ,plot.title = element_markdown(size=titlesize,face="bold"
                                       ,margin=margin(t=titlemarginT
                                                      ,r=titlemarginR
                                                      ,b=titlemarginB
                                                      ,l=titlemarginL))
        ,plot.subtitle = element_markdown(size=subtitlesize)
        ,plot.caption = element_markdown(size=captionsize)
        ,plot.margin = margin(t=plotmarginT,r=plotmarginR,b=plotmarginB
                              ,l=plotmarginL))

# Step 3e. pH graph
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
          ,plot.title = element_markdown(size=titlesize
                                         ,face="bold"
                                         ,margin=margin(t=titlemarginT
                                                        ,r=titlemarginR
                                                        ,b=titlemarginB
                                                        ,l=titlemarginL))
          ,plot.subtitle = element_markdown(size=subtitlesize)
          ,plot.caption = element_markdown(size=captionsize)
          ,plot.margin = margin(t=plotmarginT,r=plotmarginR,b=plotmarginB
                                ,l=plotmarginL))

# Step 3f. Hardness & Alkalinity graph
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
        ,plot.title = element_markdown(size=titlesize,face="bold"
                                       ,margin=margin(t=titlemarginT
                                                      ,r=titlemarginR
                                                      ,b=titlemarginB
                                                      ,l=titlemarginL))
        ,plot.subtitle = element_markdown(size=subtitlesize)
        ,plot.caption = element_markdown(size=captionsize)
        ,plot.margin = margin(t=plotmarginT,r=plotmarginR,b=plotmarginB
                              ,l=plotmarginL))

# Step 3g. Bacteria levels


# Step 3h. dissolved oxygen

# Step 3i. Bring all the graphs together
ggarrange(ggarrange(firstgraphA,firstgraphB,ncol=1,nrow=2),temperature
          ,phGraph,hAndAGraph
          # when I've created the final graphs, I'll update these two things
          ,phGraph,hAndAGraph
          ,nrow=3,ncol=2)

}

## Step 4: loop through the sites and print the PDFs

for (sitechosen in seq(1,8,1)) {

  pdf(file = paste(directory,"Site",sitechosen,"_OnePager.pdf",sep="")
      ,paper="letter",width=8,height=11)

  sitepdf(sitechosen)

  dev.off()
}









