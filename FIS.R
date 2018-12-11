#clear environment
#rm(list=ls(all=TRUE))
library(tidyverse)
library(broom)
library(lubridate)
library(GGally)
library(scales)
library(gdata)
library(effsize)
library(openxlsx)


source("C:/GitCode/AbResearch/getSeason.r")
fis <- read.xlsx(
 "C:/CloudStor/Shared/Fisheries Research/Abalone/AbResearchData/pop/ResearchSurveys_Dec2018_JM.xlsx",
 sheet = "FIS",
 detectDates = TRUE)

## data cleaning ####
summary(fis)

## convert var names to lowor case
colnames(fis) <- tolower(colnames(fis))
fis <- rename(fis, survdate = date)
fis <- rename(fis, ab_sl = length)
fis$string <- as.factor(fis$string)

## remove characters from site names
fis$site <- gsub(' ', '', fis$site)
fis$site <- gsub('_', '', fis$site)
fis$site <- gsub('Telopea', 'TE', fis$site)

## remove data with no site name
fis <- filter(fis, !is.na(site))

## rename east and west levels
unique(fis$eastwest)
fis$eastwest <- gsub('w', 'W', fis$eastwest)
fis$eastwest <- gsub('N', 'E', fis$eastwest)
fis$eastwest <- gsub('S', 'W', fis$eastwest)

### Prepare dataframes for length frequency and abundance analyses ----

## A. Extract records with abs for length frequency analysis ----
fis.sl <- filter(fis, !is.na(ab_sl))

## construct  date, quarter and season variables ----
#juv.sl$q <- quarter(juv.sl$survdate, with_year = TRUE)
fis.sl$sampyear <- as.factor(year(fis.sl$survdate)) 
fis.sl$season <- getSeason(fis.sl$survdate) 
## recode autumn samples as summer
fis.sl$season <- gsub( "Autumn", "Summer", fis.sl$season)
fis.sl$season <- as.factor(fis.sl$season)
fis.sl$season <- ordered(fis.sl$season, levels=c("Summer","Winter","Spring"))
fis.sl$yr.season <- interaction(fis.sl$sampyear,fis.sl$season)
levels(fis.sl$yr.season)
fis.sl$yr.season <-
 ordered(fis.sl$yr.season, levels = c("2015.Summer", "2015.Winter", "2015.Spring", "2016.Summer", "2016.Winter", "2016.Spring", "2017.Summer", "2017.Winter", "2017.Spring", "2018.Summer", "2018.Winter", "2018.Spring"))
pick <- which(fis.sl$site == "TG")
fis.sl$yr.season[pick] <- gsub( "2015.Summer", "2015.Spring", fis.sl$yr.season[pick])
fis.sl$yr.season <- droplevels(fis.sl$yr.season)

unique(fis.sl$survdate)
unique(fis.sl$yr.season)

## B. Extract and prepare records with abs for abundance analyses ----
#juv$ab_sl <- NAToUnknown(x = juv$ab_sl, unknown = 0)
## NOTE:  sites were surveyed on different days, and not always entirely in the one season

platearea <- 0.503 #  var for planar area of reef covered by juvenile collector


fis$survindex <- as.factor(paste(fis$site, fis$survdate, fis$string, fis$transect, fis$eastwest, fis$startpoint, sep="_"))

dat <- filter(fis, !is.na(ab_sl))  %>%
 group_by(survindex) %>%
 summarise(ab_n=n()) %>%  #as.data.frame()
 complete(survindex, fill = list(ab_n = 0)) %>%
 as.data.frame()


