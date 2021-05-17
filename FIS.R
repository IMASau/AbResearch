## Script name: Abalone Fishery Independant Survey (FIS) Analysis

## Purpose of script: Compilation and analysis of Abalone Recuitment Module (ARM) and Length
## Evaluation and Growth (LEG) survey data.

## Author: Jaime McAllister and Craig Mundy

## Date Created: 2020-05-11

## Copyright (c) Jaime McAllister, 2020
## Email: jaime.mcallister@utas.edu.au

##--------------------------------------------------------------------------------------##
## set working directory
setwd('C:/CloudStor/R_Stuff/FIS')

##--------------------------------------------------------------------------------------##
## Load libraries ####
## load library packages
suppressPackageStartupMessages({
library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)
library(gdata)
library(openxlsx)
library(lubridate)
library(reshape)
library(gridExtra)
library(ggpubr)
library(readxl)
library(tibble)
library(data.table)
library(ggpmisc)
})

##--------------------------------------------------------------------------------------##
## Load functions ####
source("C:/GitCode/AbResearch/getSeason.r")
source("C:/GitCode/AbResearch/errorUpper2.r")
source("C:/GitCode/AbResearch/stderr.r")

##--------------------------------------------------------------------------------------## 
## LEG data ####

## load LEG data
xl_data <- 'R:/TAFI/TAFI_MRL_Sections/Abalone/Section Shared/Abalone_databases/Data/Data for Transfer/2018/Ab_pop_bio_Lenght_density_2016.xlsx'

##--------------------------------------------------------------------------------------## 
## LEG compile and clean data ####

## LEG data is first compiled into a single dataframe which enables some cleaning of data 
## to occur prior to being seperated into abundance and size structure data for analysis.

## identify sheets in excel workbook
tab_names <- excel_sheets(path = xl_data)

## create list from seperate sheets
list_all <- lapply(tab_names, function(x) read_excel(path = xl_data, sheet = x))

## create dataframe from seperate sheets
legs.df <- rbindlist(list_all, fill = T)

## convert varible names to lower case and compile data for estimates and comments 
## columns, removing additional columns from the Excel import (i.e. each sheet contained 
## different column names for these variables)
colnames(legs.df) <- tolower(colnames(legs.df))
names(legs.df) <- gsub('/', '', names(legs.df), fixed = T)
names(legs.df) <- gsub(' ', '', names(legs.df), fixed = T)
names(legs.df) <- gsub('=', '', names(legs.df), fixed = T)
names(legs.df) <- gsub('comments', 'comments.1', names(legs.df), fixed = T)
names(legs.df) <- gsub('...8', 'comments.2', names(legs.df), fixed = T)
names(legs.df) <- gsub('...9', 'comments.3', names(legs.df), fixed = T)
names(legs.df) <- gsub('...10', 'comments.4', names(legs.df), fixed = T)
names(legs.df) <- gsub('eestimate', 'comments.5', names(legs.df), fixed = T)
colnames(legs.df) <- make.unique(names(legs.df))
legs.df <- dplyr::rename(legs.df, survdate = date)
legs.df <- dplyr::rename(legs.df, sllength = length)
legs.df$string <- as.factor(legs.df$string)

legs.df.2 <- legs.df %>%
  # select(-comments.3) %>%
  unite('all_comments', 'comments.1','comments.2', 'comments.4', 
        'comments.5', sep = ',') %>%
  mutate(all_comments = gsub('NA', '', all_comments),
         all_comments = gsub(',', '', all_comments),
         all_comments = gsub('^$', NA, all_comments)) %>%
  mutate(estimate.2 = estimate) %>%
  mutate(estimate = if_else(is.na(all_comments) & estimate.2 %in% c('E', 'e'), estimate.2, 
                              if_else(all_comments %in% c('E', 'e'), 
                                      all_comments, NA_character_))) %>%
  mutate(comments = if_else(is.na(estimate), all_comments, NA_character_),
         estimate = gsub('e', 'E', estimate)) %>%
  dplyr::select(-c(estimate.2, all_comments)) %>%
  as.data.frame()

## remove any characters or obvious errors from shell length (e.g. where 'estimate (E)' 
## or 0 has been entered in the raw data)
str(legs.df.2$sllength) #check
legs.df.2 <- legs.df.2 %>% 
  mutate(estimate = if_else(grepl('E', sllength), 'E', estimate),
         sllength = as.numeric(gsub('E', '', sllength))) %>% 
  filter(sllength != 0)
# str(legs.df.2$sllength) #check

## remove data with no site name or shell length
legs.df.2 <- filter(legs.df.2, !is.na(site))
legs.df.2 <- filter(legs.df.2, !is.na(sllength))

## remove characters from site names and rename sites to a three letter acronym
# unique(bigabs$site)
legs.df.2$site <- gsub(' ', '', legs.df.2$site)
legs.df.2$site <- gsub('_', '', legs.df.2$site)
legs.df.2$site <- gsub('Telopea', 'TEL', legs.df.2$site)
legs.df.2$site <- gsub('SP', 'SEY', legs.df.2$site)
legs.df.2$site <- gsub('\\bT\\b', 'THU', legs.df.2$site)
legs.df.2$site <- gsub('BI', 'BET', legs.df.2$site)
legs.df.2$site <- gsub('TG', 'GAR', legs.df.2$site)
legs.df.2$site <- gsub('GIII', 'GEO', legs.df.2$site)
legs.df.2$site <- gsub('MB', 'MUN', legs.df.2$site)
legs.df.2$site <- gsub('MP', 'INN', legs.df.2$site)
legs.df.2$site <- gsub('LB', 'LOU', legs.df.2$site)
legs.df.2$site <- gsub('OB', 'OUT', legs.df.2$site)

## rename string names from earlier sampling periods
# table(legs.df.2$site, legs.df.2$string)
legs.df.2$string  <- gsub( "Kar", "1", legs.df.2$string )
legs.df.2$string  <- gsub( "Juv", "2", legs.df.2$string )
legs.df.2$string  <- gsub( "N", "1", legs.df.2$string )
legs.df.2$string  <- gsub( "S", "2", legs.df.2$string )
legs.df.2$string  <- gsub( "North", "1", legs.df.2$string )
legs.df.2$string  <- gsub( "South", "2", legs.df.2$string )

## rename east and west transect directions
# unique(legs.df.2$eastwest)
legs.df.2$eastwest <- gsub('w', 'W', legs.df.2$eastwest)
legs.df.2$eastwest <- gsub('N', 'E', legs.df.2$eastwest)
legs.df.2$eastwest <- gsub('S', 'W', legs.df.2$eastwest)
legs.df.2$eastwest <- gsub('L', 'W', legs.df.2$eastwest)
legs.df.2$eastwest <- gsub('R', 'E', legs.df.2$eastwest)

## add unique identifier for each measurement
legs.df.2$survindex <- as.factor(paste(legs.df.2$site, legs.df.2$survdate, legs.df.2$string, 
                                    legs.df.2$transect, sep="_"))

##--------------------------------------------------------------------------------------##
## LEG density data ####

## To generate density estimates for LEG data, individual length data first need to be 
## converted to counts then density.

## LEG ALL count data ##

## filter for ALL abalone
legs.dat <- legs.df.2 %>%
 group_by(survindex) %>%
 summarise(ab_n = n()) %>%  #as.data.frame()
 complete(survindex, fill = list(ab_n = 0)) %>%
 as.data.frame()

## calculate abs per square metre 
legs.dat$absm <- legs.dat$ab_n / 15

## unpack survindex variables and create new dataframe
leg.counts <- data.frame(separate(legs.dat, survindex, sep = "_", 
                                   into = c("site", "survdate", 
                                            "string","transect"), convert = TRUE), 
                          legs.dat$survindex, legs.dat$ab_n, legs.dat$absm)

## set string as a factor
leg.counts$string <- as.factor(leg.counts$string)

## construct date and season variables
leg.counts$survdate <- as.Date(strptime(leg.counts$survdate, "%Y-%m-%d"))
leg.counts$sampyear <- year(leg.counts$survdate)
leg.counts$season <- getSeason(leg.counts$survdate)

## recode autumn samples as summer
leg.counts$season <- gsub( "Autumn", "Summer", leg.counts$season)

## create year.season variable and arrange in order (i.e. summer, winter, spring)
leg.counts$season <- as.factor(leg.counts$season)
leg.counts$season <- ordered(leg.counts$season, levels=c("Summer","Winter","Spring"))
leg.counts$yr.season <- interaction(leg.counts$sampyear,leg.counts$season)
# sort(unique(leg.counts$yr.season))
leg.counts$yr.season <-
 ordered(leg.counts$yr.season, levels = c("2015.Summer", "2015.Winter", "2015.Spring", 
                                           "2016.Summer", "2016.Winter", "2016.Spring", 
                                           "2017.Summer", "2017.Winter", "2017.Spring", 
                                           "2018.Summer", "2018.Winter", "2018.Spring", 
                                           "2019.Summer", '2019.Winter', '2019.Spring',
                                          '2020.Summer', "2020.Spring", "2021.Summer"))

## adjust misclassified seasons for The Gardens
pick <- which(leg.counts$site == "GAR")
leg.counts$yr.season[pick] <- gsub( "2015.Summer", "2015.Spring", 
                                     leg.counts$yr.season[pick])
leg.counts$yr.season <- droplevels(leg.counts$yr.season)

## save a copy of the R files
saveRDS(leg.counts, 'C:/CloudStor/R_Stuff/FIS/leg.counts.RDS')

##--------------------------------------------------------------------------------------##

## OPTION: the dataframe can also be seperated into different size classes to generate 
##plots of legal and sub-legal abalone counts (see LEG plots)

## LEG SIZE count data

## Filter for sub-legal abalone
legs.dat.sub <- legs.df.2 %>% 
 filter(sllength <= 137) %>%
 # bigabdat <- bigabs %>%
 group_by(survindex) %>%
 summarise(ab_n = n()) %>%  #as.data.frame()
 complete(survindex, fill = list(ab_n = 0)) %>%
 as.data.frame()

## Filter for legal abalone
legs.dat.leg <- legs.df.2 %>% 
 filter(sllength >= 138) %>%
 group_by(survindex) %>%
 summarise(ab_n = n()) %>%  #as.data.frame()
 complete(survindex, fill = list(ab_n = 0)) %>%
 as.data.frame()

## Filter for sub-legal abalone <10 mm
legs.dat.sub.ten <- legs.df.2 %>% 
 filter(sllength >= 128 & sllength < 138) %>%
 group_by(survindex) %>%
 summarise(ab_n = n()) %>%  #as.data.frame()
 complete(survindex, fill = list(ab_n = 0)) %>%
 as.data.frame()

## Filter for legal abalone +10 mm
legs.dat.leg.ten <- legs.df.2 %>% 
 filter(sllength >= 139 & sllength <= 148) %>%
 group_by(survindex) %>%
 summarise(ab_n = n()) %>%  #as.data.frame()
 complete(survindex, fill = list(ab_n = 0)) %>%
 as.data.frame()

## join dataframes created above for survindex
legs.dat.join <- left_join(legs.dat.sub, legs.dat.leg, by = 'survindex') %>%
 left_join(., legs.dat.sub.ten, by = 'survindex') %>%
 left_join(., legs.dat.leg.ten, by = 'survindex') %>%
 left_join(., legs.dat, by = 'survindex')

## rename variables to identify abcounts from each size class
legs.dat.join <- dplyr::rename(legs.dat.join, ab_n_leg = ab_n.y)
legs.dat.join <- dplyr::rename(legs.dat.join, ab_n_sub = ab_n.x)
legs.dat.join <- dplyr::rename(legs.dat.join, ab_n_sub_ten = ab_n.x.x)
legs.dat.join <- dplyr::rename(legs.dat.join, ab_n_leg_ten = ab_n.y.y)

## calculate abs per square metre for joint dataframe
legs.dat.join$absm_sub <- legs.dat.join$ab_n_sub /15
legs.dat.join$absm_leg <- legs.dat.join$ab_n_leg /15
legs.dat.join$absm_sub_ten <- legs.dat.join$ab_n_sub_ten /15
legs.dat.join$absm_leg_ten <- legs.dat.join$ab_n_leg_ten /15
legs.dat.join$absm <- legs.dat.join$ab_n /15

## unpack survindex variables and create new dataframe for the joint dataframe
legs.counts.join <- data.frame(separate(legs.dat.join, survindex, sep = "_",
                                   into = c("site", "survdate", "string","transect"), 
                                   convert = TRUE), 
                          legs.dat.join$survindex, legs.dat.join$ab_n, 
                          legs.dat.join$absm, legs.dat.join$ab_n_sub, 
                          legs.dat.join$absm_sub, legs.dat.join$ab_n_leg, 
                          legs.dat.join$absm_leg,
                          legs.dat.join$ab_n_sub_ten, legs.dat.join$absm_sub_ten,
                          legs.dat.join$ab_n_leg_ten, legs.dat.join$absm_leg_ten)

## set string as a factor
legs.counts.join$string <- as.factor(legs.counts.join$string)

## construct date and season variables
legs.counts.join$survdate <- as.Date(strptime(legs.counts.join$survdate, "%Y-%m-%d"))
legs.counts.join$sampyear <- year(legs.counts.join$survdate)
legs.counts.join$season <- getSeason(legs.counts.join$survdate)

## recode autumn samples as summer
legs.counts.join$season <- gsub( "Autumn", "Summer", legs.counts.join$season)

## create year.season variable and arrange in order (i.e. summer, winter, spring)
legs.counts.join$season <- as.factor(legs.counts.join$season)
legs.counts.join$season <- ordered(legs.counts.join$season, 
                                   levels=c("Summer","Winter","Spring"))
legs.counts.join$yr.season <- interaction(legs.counts.join$sampyear,
                                          legs.counts.join$season)
# unique(legs.counts.join$yr.season)
legs.counts.join$yr.season <-
 ordered(legs.counts.join$yr.season, 
         levels = c("2015.Summer", "2015.Winter", "2015.Spring", 
                    "2016.Summer", "2016.Winter", "2016.Spring", 
                    "2017.Summer", "2017.Winter", "2017.Spring", 
                    "2018.Summer", "2018.Winter", "2018.Spring", 
                    "2019.Summer", '2019.Winter', '2019.Spring',
                    '2020.Summer', "2020.Spring", "2021.Summer"))

## adjust misclassified seasons
pick <- which(legs.counts.join$site == "GAR")
legs.counts.join$yr.season[pick] <- gsub( "2015.Summer", "2015.Spring", 
                                          legs.counts.join$yr.season[pick])
legs.counts.join$yr.season <- droplevels(legs.counts.join$yr.season)

## save a copy of the R files
saveRDS(legs.counts.join, 'C:/CloudStor/R_Stuff/FIS/legs.counts.join.RDS')

##--------------------------------------------------------------------------------------##

## OPTION: the dataframe can also be seperated into individual LEG sampling sites

## LEG SITE count data

## subset data to individual LEG sampling sites
list_legscounts.site <- split(legs.counts.join, legs.counts.join$site)
names(list_legscounts.site)
legscounts.sites <- c("legscounts.BET", "legscounts.BRB", "legscounts.BRS", 
                       "legscounts.GAR", "legscounts.GEO", "legscounts.INN", 
                       "legscounts.LOU", "legscounts.MUN", "legscounts.OUT", 
                       "legscounts.SEY", "legscounts.TEL",  "legscounts.THU")
for (i in 1:length(list_legscounts.site)) {
 assign(legscounts.sites[i], list_legscounts.site[[i]])
}

## save a copy of the R files
saveRDS(list_legscounts.site, 'C:/CloudStor/R_Stuff/FIS/list_legscounts.site.RDS')

##--------------------------------------------------------------------------------------##
## LEG size data ####

## create a copy of the original LEG dataframe
legs.sl <- legs.df.2

## construct date and season variables
legs.sl$sampyear <- year(legs.sl$survdate)
legs.sl$season <- getSeason(legs.sl$survdate)

## recode autumn samples as summer
legs.sl$season <- gsub( "Autumn", "Summer", legs.sl$season)

## create year.season variable and arrange in order (i.e. summer, winter, spring)
legs.sl$season <- as.factor(legs.sl$season)
legs.sl$season <- ordered(legs.sl$season, levels=c("Summer","Winter","Spring"))
legs.sl$yr.season <- interaction(legs.sl$sampyear,legs.sl$season)
legs.sl$yr.season <-
 ordered(legs.sl$yr.season, levels = c("2015.Summer", "2015.Winter", "2015.Spring", 
                                         "2016.Summer", "2016.Winter", "2016.Spring", 
                                         "2017.Summer", "2017.Winter", "2017.Spring", 
                                         "2018.Summer", "2018.Winter", "2018.Spring", 
                                         "2019.Summer", '2019.Winter', '2019.Spring',
                                       '2020.Summer', '2020.Spring', '2021.Summer'))

## adjust misclassified seasons for The Gardens
pick <- which(legs.sl$site == "GAR")
legs.sl$yr.season[pick] <- gsub( "2015.Summer", "2015.Spring", legs.sl$yr.season[pick])
legs.sl$yr.season <- droplevels(legs.sl$yr.season)

## save a copy of the R files
saveRDS(legs.sl, 'C:/CloudStor/R_Stuff/FIS/legs.sl.RDS')

##--------------------------------------------------------------------------------------##

## OPTION: the dataframe can also be seperated into individual LEG sampling sites

## LEG size SITE frequency data

## subset data into sites
list_legs.sl.site <- split(legs.sl, legs.sl$site)
names(list_legs.sl.site)
legs.sl.sites <- c("legs.sl.BET", "legs.sl.BRB", "legs.sl.BRS", "legs.sl.GAR",
                     "legs.sl.GEO", "legs.sl.INN", "legs.sl.LOU", "legs.sl.MUN",
                     "legs.sl.OUT", "legs.sl.SEY", "legs.sl.TEL", "legs.sl.THU")

for (i in 1:length(list_legs.sl.site)) {
 assign(legs.sl.sites[i], list_legs.sl.site[[i]])
}

## save a copy of the R files
saveRDS(list_legs.sl.site, 'C:/CloudStor/R_Stuff/FIS/list_legs.sl.site.RDS')

##--------------------------------------------------------------------------------------##
## ARM data ####

## ARM data is loaded from one Excel file containing seperate sheets for each site.

## Load ARM data
xl_arm_data <- 'R:/TAFI/TAFI_MRL_Sections/Abalone/Section Shared/Abalone_databases/Data/Data for Transfer/2018/Juvenile_data_2016.xlsx'

##--------------------------------------------------------------------------------------##
## ARM compile and clean data ####

## identify sheets in excel workbook
tab_names_arms <- excel_sheets(path = xl_arm_data)

## create list from seperate sheets
list_all_arms <- lapply(tab_names_arms, function(x) read_excel(path = xl_arm_data, 
                                                               sheet = x, guess_max = 10000))

## remove list items (i.e. sheets) that are irrelevant
list_all_arms_2 <- list_all_arms[-c(2, 10)]

## create dataframe from seperate sheets
arms.df <- rbindlist(list_all_arms_2, fill = T)

## convert varible names to lower case, compile data comments columns and clean data,
## removing additional columns from the Excel import (i.e. each sheet contained 
## different column names for these variables)

names(arms.df) <- gsub('/', '', names(arms.df), fixed = T)
names(arms.df) <- gsub(' ', '', names(arms.df), fixed = T)
arms.df <- dplyr::rename(arms.df, rock.plate.3 = RockorPlate)
colnames(arms.df) <- tolower(colnames(arms.df))
colnames(arms.df) <- make.unique(names(arms.df))
names(arms.df) <- gsub('comments', 'comments.1', names(arms.df), fixed = T)
names(arms.df) <- gsub('...8', 'comments.2', names(arms.df), fixed = T)
arms.df <- dplyr::rename(arms.df, site = location)
arms.df <- dplyr::rename(arms.df, survdate = date)
arms.df <- dplyr::rename(arms.df, sllength = ab_sl)
arms.df <- dplyr::rename(arms.df, rock.plate.1 = rp)
arms.df <- dplyr::rename(arms.df, rock.plate.2 = rockorplate)
arms.df <- dplyr::rename(arms.df, tag.col = tagcolour)
arms.df <- dplyr::rename(arms.df, tag.id.col = printcolour)
arms.df <- dplyr::rename(arms.df, tag.id = tag_id)
arms.df <- dplyr::rename(arms.df, tag.recap = tr)
arms.df$string <- as.factor(arms.df$string)
arms.df$rock.plate.3 <- gsub('r', 'R', arms.df$rock.plate.3)
arms.df$string = gsub('North', 1, arms.df$string)
arms.df$plate = gsub(8.5, 8, arms.df$plate)
arms.df <- arms.df %>% 
 mutate(comments.1 = if_else(sllength == 'OFF', 'plate off', comments.1),
        sllength = gsub('OFF', NA, sllength))

## combine duplicate columns and compile dataframe
arms.df.2 <- arms.df %>%
  unite('comments', 'comments.1', 'comments.2', sep = ',') %>% 
  mutate(comments = gsub('NA', '', comments),
         comments = gsub(',', '', comments),
         comments = gsub('^$', NA, comments)) %>% 
  unite('rock.plate', 'rock.plate.1', 'rock.plate.2', 'rock.plate.3', sep = ',') %>% 
  mutate(rock.plate = gsub('NA', '', rock.plate),
        rock.plate = gsub(',', '', rock.plate),
        rock.plate = gsub('^$', NA, rock.plate))

## remove data with no site name or shell length
arms.df.2 <- filter(arms.df.2, !is.na(site))
arms.df.2 <- filter(arms.df.2, !is.na(sllength))

## remove characters from site names and rename sites to a three letter acronym
# unique(arms.df.2$site)
arms.df.2$site <- gsub(' ', '', arms.df.2$site)
arms.df.2$site <- gsub('_', '', arms.df.2$site)
arms.df.2$site <- gsub('Telopea', 'TEL', arms.df.2$site)
arms.df.2$site <- gsub('SP', 'SEY', arms.df.2$site)
arms.df.2$site <- gsub('\\bT\\b', 'THU', arms.df.2$site)
arms.df.2$site <- gsub('BI', 'BET', arms.df.2$site)
arms.df.2$site <- gsub('TG', 'GAR', arms.df.2$site)
arms.df.2$site <- gsub('GIII', 'GEO', arms.df.2$site)
arms.df.2$site <- gsub('MB', 'MUN', arms.df.2$site)
arms.df.2$site <- gsub('SB', 'INN', arms.df.2$site)
arms.df.2$site <- gsub('LB', 'LOU', arms.df.2$site)
arms.df.2$site <- gsub('OS', 'OUT', arms.df.2$site)
arms.df.2$site <- gsub('G3', 'GEO', arms.df.2$site)
arms.df.2$site <- gsub('G4', 'GEO', arms.df.2$site)
arms.df.2$site <- gsub('G5', 'GEO', arms.df.2$site)
arms.df.2$site <- gsub('G6', 'GEO', arms.df.2$site)
arms.df.2$site <- gsub('G7', 'GEO', arms.df.2$site)

##--------------------------------------------------------------------------------------##
## ARM size data ####

## A. Extract records with abs for length frequency analysis
arms.sl <- arms.df.2 %>% 
 mutate(sllength = as.numeric(as.character(sllength))) %>%
 filter(is.nan(sllength) | !is.na(sllength))

## construct  date and season variables
arms.sl$sampyear <- as.factor(year(arms.sl$survdate)) 
arms.sl$season <- getSeason(arms.sl$survdate) 

## recode autumn samples as summer
arms.sl$season <- gsub( "Autumn", "Summer", arms.sl$season)
arms.sl$season <- as.factor(arms.sl$season)
arms.sl$season <- ordered(arms.sl$season, levels=c("Summer","Winter","Spring"))

## extract year.season and arrange in order (i.e. summer, winter, spring)
arms.sl$yr.season <- interaction(arms.sl$sampyear,arms.sl$season)
# levels(arms.sl$yr.season)
arms.sl$yr.season <-
ordered(arms.sl$yr.season, levels = c("2015.Summer", "2015.Winter", "2015.Spring", 
                                     "2016.Summer", "2016.Winter", "2016.Spring", 
                                     "2017.Summer", "2017.Winter", "2017.Spring", 
                                     "2018.Summer", "2018.Winter", "2018.Spring",
                                     "2019.Summer", "2019.Winter", "2019.Spring",
                                     '2020.Summer', '2020.Spring', '2021.Summer'))

## recode Gardens 2015.summer samples as 2015.spring
pick <- which(arms.sl$site == "GAR")
arms.sl$yr.season[pick] <- gsub("2015.Summer", "2015.Spring", arms.sl$yr.season[pick])
arms.sl$yr.season <- droplevels(arms.sl$yr.season)

## save a copy of the R files
saveRDS(arms.sl, 'C:/CloudStor/R_Stuff/FIS/arms.sl.RDS')

##--------------------------------------------------------------------------------------##

## OPTION: the dataframe can also be seperated into individual ARM sampling sites

## ARM size SITE frequency data

## subset data into ARM sampling sites
list_arms.sl.site <- split(arms.sl, arms.sl$site)
names(list_arms.sl.site)
arms.sl.sites <- c("arms.sl.BET",   "arms.sl.BRB",    "arms.sl.BRS",   "arms.sl.GAR",
                     "arms.sl.GEO", "arms.sl.INN", "arms.sl.OUT", "arms.sl.SEY")
for (i in 1:length(list_arms.sl.site)) {
 assign(arms.sl.sites[i], list_arms.sl.site[[i]])
}

## save a copy of the R files
saveRDS(list_arms.sl.site, 'C:/CloudStor/R_Stuff/FIS/list_arms.sl.site.RDS')

##--------------------------------------------------------------------------------------##
## ARM density data ####

## determine the surface area of the ARM (diameter = 400 mm)
platearea <- pi*0.2^2

## create unique ID/index for each ARM and survdate combination
arms.df.2$survindex <- as.factor(paste(arms.df.2$site, 
                                       arms.df.2$survdate, 
                                       arms.df.2$string, 
                                       arms.df.2$plate, sep="_"))

## subset and count number of animals per ARM by survdate
dat <- arms.df.2 %>% 
 mutate(sllength = as.numeric(as.character(sllength))) %>%
 filter(is.nan(sllength) | !is.na(sllength)) %>%
 group_by(survindex) %>%
 summarise(ab_n=n()) %>%
 complete(survindex, fill = list(ab_n = 0)) %>%
 as.data.frame()

## calculate abs per square metre 
dat$absm <- dat$ab_n * (1/platearea)

## unpack survindex variables and create new dataframe
arm.counts <- data.frame(separate(dat, survindex, sep = "_", 
                                  into = c("site", "survdate", "string","plate"), 
                                  convert = TRUE), dat$survindex, dat$ab_n, dat$absm)

## format date variable and add year/season variables
arm.counts$survdate <- as.Date(strptime(arm.counts$survdate, "%Y-%m-%d"))
arm.counts$sampyear <- as.factor(year(arm.counts$survdate)) 
arm.counts$season <- getSeason(arm.counts$survdate) 

## recode autumn samples as summer
arm.counts$season <- gsub( "Autumn", "Summer", arm.counts$season)
arm.counts$season <- as.factor(arm.counts$season)
arm.counts$season <- ordered(arm.counts$season, levels=c("Summer","Winter","Spring"))

## create variable identifying year and season
arm.counts$yr.season <- interaction(arm.counts$sampyear,arm.counts$season)
arm.counts$yr.season <-
 ordered(arm.counts$yr.season, levels = c("2015.Summer", "2015.Winter", "2015.Spring", 
                                        "2016.Summer", "2016.Winter", "2016.Spring", 
                                        "2017.Summer", "2017.Winter", "2017.Spring", 
                                        "2018.Summer", "2018.Winter", "2018.Spring",
                                        "2019.Summer", "2019.Winter", "2019.Spring",
                                        '2020.Summer', '2020.Spring', '2021.Summer'))

## recode Gardens 2015.summer samples as 2015.spring
pick <- which(arm.counts$site == "GAR")
arm.counts$yr.season[pick] <- gsub( "2015.Summer", "2015.Spring", 
                                    arm.counts$yr.season[pick])
arm.counts$yr.season <- droplevels(arm.counts$yr.season)

## save a copy of the R files
saveRDS(arm.counts, 'C:/CloudStor/R_Stuff/FIS/arm.counts.RDS')

##--------------------------------------------------------------------------------------##

## OPTION: the dataframe can also be seperated into different size classes to generate 
## lagged association plots with adults abalone counts (see ARM and LEG plots)

## Legal and sub-legal leg counts 2

## Filter for juvenile abalone approx. 6-months old (i.e. <25 mm)
arms.dat.juv <- arms.df.2 %>% 
 filter(sllength <= 25) %>%
 group_by(survindex) %>%
 summarise(ab_n = n()) %>%  #as.data.frame()
 complete(survindex, fill = list(ab_n = 0)) %>%
 as.data.frame()

## join dataframes created above for survindex
arms.counts.join <- dplyr::rename(arm.counts, survindex = dat.survindex) %>% 
  left_join(., arms.dat.juv, by = 'survindex')

## rename variables to identify abcounts from each size class
arms.counts.join <- dplyr::rename(arms.counts.join, ab_n = ab_n.x)
arms.counts.join <- dplyr::rename(arms.counts.join, ab_n_juv = ab_n.y)

## calculate abs per square metre for joint dataframe
arms.counts.join$absm_juv <- arms.counts.join$ab_n_juv/platearea

## save a copy of the R files
saveRDS(arms.counts.join, 'C:/CloudStor/R_Stuff/FIS/arms.counts.join.RDS')

##--------------------------------------------------------------------------------------##
## ARM:LEG combine data ####

## For the 2018 Abalone Stock Assessment Report ARM and LEG data were combined so that both 
## sources of size data could be displayed on the one figure/plot.

## ARM:LEG density data combined ####

## load most recent juvenile and adult data sets
leg.counts <- readRDS('C:/CloudStor/R_Stuff/FIS/leg.counts.RDS')
arm.counts <- readRDS('C:/CloudStor/R_Stuff/FIS/arm.counts.RDS')

## add column to identify FIS and ARM data
leg.counts$sampmethod <- 'LEG'
arm.counts$sampmethod <- 'ARM'

## rename absm columns to make unique for ARM or LEG
arm.counts <- dplyr::rename(arm.counts, absm.arm = absm)
leg.counts <- dplyr::rename(leg.counts, absm.leg = absm)

## convert arm.counts survdate to POSIXct
arm.counts$survdate <- as.Date(strptime(arm.counts$survdate, "%Y-%m-%d"))

## convert sampyear to factor from leg.counts df
leg.counts$sampyear <- as.factor(leg.counts$sampyear)
arm.counts$string <- as.factor(arm.counts$string)

## join FIS and ARM data

leg.counts <- leg.counts %>% 
  dplyr::select(-yr.season)

arm.counts <- arm.counts %>% 
  dplyr::select(-yr.season)

arm.leg.counts <- dplyr::bind_rows(leg.counts, arm.counts)

arm.leg.counts <- arm.leg.counts %>% 
  mutate(yr.season = paste(sampyear, season, sep = '.'))


## save a copy of the R files
saveRDS(arm.leg.counts, 'C:/CloudStor/R_Stuff/FIS/arm.leg.counts.RDS')

##--------------------------------------------------------------------------------------##

## ARM:LEG size data combined ####

## load most recent ARM and LEG size frequency data sets
legs.sl <- readRDS('C:/CloudStor/R_Stuff/FIS/legs.sl.RDS')
arms.sl <- readRDS('C:/CloudStor/R_Stuff/FIS/arms.sl.RDS')

## convert sampyear to factor
arms.sl$sampyear <- as.factor(arms.sl$sampyear)
legs.sl$sampyear <- as.factor(legs.sl$sampyear)

# add column to identify ARM and LEG data
legs.sl$sampmethod <- 'LEG'
arms.sl$sampmethod <- 'ARM'

## rename shell length columns to make unique for ARM or LEG
arms.sl <- dplyr::rename(arms.sl, sllength.arm = sllength)
legs.sl <- dplyr::rename(legs.sl, sllength.leg = sllength)

## convert abcounts survdate to POSIXct
# legs.sl$survdate <- as.Date(strptime(legs.sl$survdate, "%Y-%m-%d"))
# legs.sl$survdate <- as.Date(strptime(legs.sl$survdate, "%Y-%m-%d"))

# join ARM and LEG data
legs.sl <- legs.sl %>% 
  dplyr::select(-yr.season)

arms.sl <- arms.sl %>% 
  dplyr::select(-yr.season)

arm.leg.sl <- bind_rows(legs.sl, arms.sl)

arm.leg.sl <- arm.leg.sl %>% 
  mutate(yr.season = paste(sampyear, season, sep = '.'))

## save a copy of the R files
saveRDS(arm.leg.sl, 'C:/CloudStor/R_Stuff/FIS/arm.leg.sl.RDS')

##--------------------------------------------------------------------------------------##

## Plots ####

##--------------------------------------------------------------------------------------##
## Create some abbreviated x-axis labels for plots where there 
## is a long time series of data and potential for labels to appear squashed.

## create short label names for plot facets and axis 
season_labels <- c("2015.Summer" = '2015.Su',
                   "2015.Winter" = '2015.Wi', 
                   "2015.Spring" = '2015.Sp', 
                   "2016.Summer" = '2016.Su', 
                   "2016.Winter" = '2016.Wi', 
                   "2016.Spring" = '2016.Sp', 
                   "2017.Summer" = '2017.Su', 
                   "2017.Winter" = '2017.Wi', 
                   "2017.Spring" = '2017.Sp', 
                   "2018.Summer" = '2018.Su', 
                   "2018.Winter" = '2018.Wi', 
                   "2018.Spring" = '2018.Sp',
                   "2019.Summer" = '2019.Su',
                   "2019.Winter" = '2019.Wi',
                   "2019.Spring" = '2019.Sp',
                   "2020.Summer" = '2020.Su',
                   "2020.Winter" = '2020.Wi',
                   "2020.Spring" = '2020.Sp',
                   "2021.Summer" = '2021.Su')

##--------------------------------------------------------------------------------------##
## Size freq plot ####

## load most recent combined ARM and LEG size frequency data set
arm.leg.sl <- readRDS('C:/CloudStor/R_Stuff/FIS/arm.leg.sl.RDS')
  
## exract unique sites and yr.seasons
arm.leg.sites <- unique(arm.leg.sl$site)
arm.leg.seasons <- data.frame(yr.season = unique(arm.leg.sl$yr.season)) %>% 
  add_row(yr.season = '2020.Winter')

plot.seasons <- c("2017.Spring", "2019.Winter", 
                  "2018.Summer", "2019.Spring",
                  "2018.Winter", "2020.Summer", 
                  "2018.Spring", "2020.Winter",
                  "2019.Summer", "2020.Spring")

plot.sites <- c("BRS", "BRB", "GEO", "BET")


## loop through sites to generate arm and leg plots autonomously
for (i in plot.sites){

## subset site data
arm.leg.site <- subset(arm.leg.sl, site == i &
                         yr.season %in% plot.seasons &
                         site %in% plot.sites)

## re-order data so that facet plots in vertical order of two columns
arm.leg.site$yr.season <- factor(arm.leg.site$yr.season, 
                                 levels = c("2017.Spring", "2019.Winter", 
                                            "2018.Summer", "2019.Spring",
                                            "2018.Winter", "2020.Summer", 
                                            "2018.Spring", "2020.Winter",
                                            "2019.Summer", "2020.Spring"))


## generate a summary table for chosen site to add counts to plots (i.e. n = xxx)
plot.n.LEG <- arm.leg.site %>% 
 filter(sampmethod == 'LEG') %>%
 group_by(yr.season) %>%
 summarise(n = paste('n =', n()))

plot.n.ARM <- arm.leg.site %>% 
 filter(sampmethod == 'ARM') %>%
 group_by(yr.season) %>%
 summarise(n = paste('n =', n()))

## generate dataframe to annotate 'no data' for missing seasons
arm.leg.summary <- arm.leg.site %>%
 group_by(yr.season, sampmethod) %>%
 summarise(n.sl = n()) %>%
 as.data.frame()

ann_text <- left_join(arm.leg.seasons, arm.leg.summary, by = 'yr.season') %>% 
  filter(yr.season %in% plot.seasons) %>% 
 mutate(lab = if_else(is.na(n.sl), 'NO DATA', NA_character_),
        x = 90,
        y = 40) %>% 
 filter(!is.na(lab)) %>% 
        mutate(yr.season = factor(yr.season)) %>% 
 dplyr::select(c(yr.season, lab, x, y)) 

## re-order and convert yr.season to factor
ann_text$yr.season <- factor(ann_text$yr.season, 
                                 levels = c("2017.Spring", "2019.Winter", 
                                            "2018.Summer", "2019.Spring",
                                            "2018.Winter", "2020.Summer", 
                                            "2018.Spring", "2020.Winter",
                                            "2019.Summer", "2020.Spring"))

## generate plot using 'if...else' statement to determine plot type depending on 
## whether a site has ARMs installed
arm.leg.plot <- if((nrow(plot.n.ARM) == 0 & length(names(plot.n.ARM)) == 0)){
ggplot(data = arm.leg.site)+
 geom_histogram(aes(x = sllength.leg, y = ..count..), binwidth = 2, fill = 'blue')+
 geom_histogram(aes(x = sllength.arm, y = -..count..), binwidth = 2, fill = 'red')+
 facet_wrap(. ~ yr.season, ncol = 2, drop = F)+
 theme_bw()+
 ylab("Frequency") +
 xlab("Shell Length (mm)")+
 coord_cartesian(ylim = c(-25, 50), xlim = c(0, 180))+
 geom_hline(yintercept = 0, size = 0.1)+
 geom_text(data = ann_text, aes(x = x, y = y, label = lab))+
 geom_text(data = plot.n.LEG, aes(x = 160, y = 50, label = n),
           colour = 'black', inherit.aes = F, parse = F, size = 3.5)+
 geom_vline(aes(xintercept = 138),colour = 'red', linetype = 'dashed', size = 0.5)+
 theme(legend.position = 'none')
        } else {
ggplot(data = arm.leg.site)+
        geom_histogram(aes(x = sllength.leg, y = ..count..), binwidth = 2, fill = 'blue')+
        geom_histogram(aes(x = sllength.arm, y = -..count..), binwidth = 2, fill = 'red')+
        facet_wrap(. ~ yr.season, ncol = 2, drop = F)+
        theme_bw()+
        ylab("Frequency") +
        xlab("Shell Length (mm)")+
        coord_cartesian(ylim = c(-25, 50), xlim = c(0, 180))+
        geom_hline(yintercept = 0, size = 0.1)+
        geom_text(data = ann_text, aes(x = x, y = y, label = lab))+
        geom_text(data = plot.n.LEG, aes(x = 160, y = 50, label = n),
                  colour = 'black', inherit.aes = F, parse = F, size = 3.5)+
        geom_text(data = plot.n.ARM, aes(x = 10, y = -25, label = if_else(n == 'n = 0', '', n)),
                  colour = 'black', inherit.aes = F, parse = F, size = 3.5, na.rm = F)+
        geom_vline(aes(xintercept = 138),colour = 'red', linetype = 'dashed', size = 0.5)+
        theme(legend.position = 'none')
         }

 # print(arm.leg.plot)

setwd('C:/CloudStor/R_Stuff/FIS/FIS_2020')
ggsave(filename = paste('ARM_LEG_LF_', i, '_2mm', '.pdf', sep = ''),
       plot = arm.leg.plot, units = 'mm', width = 190, height = 250)
ggsave(filename = paste('ARM_LEG_LF_', i, '_2mm', '.png', sep = ''),
       plot = arm.leg.plot, units = 'mm', width = 190, height = 250)
}

##--------------------------------------------------------------------------------------##
## Size freq powerpoint ####

## load most recent combined ARM and LEG size frequency data set
arm.leg.sl <- readRDS('C:/CloudStor/R_Stuff/FIS/arm.leg.sl.RDS')

## subset chosen site and last four sampling seasons for powerpoint presentation
unique(arm.leg.sl$site)
selected.site <- 'BRS'
selected.season <- c('2019.Summer', '2019.Winter', '2019.Spring', '2020.Summer')

arm.leg.site.lastfour <- arm.leg.sl %>% 
  filter(site %in% selected.site & yr.season %in% selected.season)

## re-order data so that facet plots in vertical order of two columns
arm.leg.site.lastfour$yr.season <- factor(arm.leg.site.lastfour$yr.season, 
                                 levels = c("2019.Summer", 
                                            "2019.Winter",
                                            "2019.Spring",
                                            "2020.Summer"))

## generate a summary table for chosen site to add counts to plots (i.e. n = xxx)
plot.n.LEG.lastfour <- arm.leg.site.lastfour %>% 
 filter(sampmethod == 'LEG') %>%
 group_by(yr.season) %>%
 summarise(n = paste('n =', n()))

plot.n.ARM.lastfour <- arm.leg.site.lastfour %>% 
 filter(sampmethod == 'ARM') %>%
 group_by(yr.season) %>%
 summarise(n = paste('n =', n()))

## manually generate dataframe to annotate 'no data' for missing seasons
# ann_text <- data.frame(x = 90, y = 40, 
#                        lab = 'NO DATA', 
#                        yr.season = c('2018.Spring', '2019.Summer'))

arm.leg.plot.lastfour <- ggplot(data = arm.leg.site.lastfour)+
 geom_histogram(aes(x = sllength.leg, y = ..count..), binwidth = 10, fill = 'blue')+
 geom_histogram(aes(x = sllength.arm, y = -..count..), binwidth = 10, fill = 'red')+
 facet_wrap(. ~ yr.season, ncol = 1, drop = F)+
 theme_bw()+
 ylab("Frequency") +
 xlab("Shell Length (mm)")+
 coord_cartesian(ylim = c(-50, 115), xlim = c(0, 180))+
 geom_hline(yintercept = 0, size = 0.1)+
 # geom_text(data = ann_text, aes(x = x, y = y, label = lab))+
 geom_text(data = plot.n.LEG.lastfour, aes(x = 160, y = 50, label = n), 
           colour = 'black', inherit.aes = F, parse = F, size = 3.5)+
 geom_text(data = plot.n.ARM.lastfour, aes(x = 10, y = -30, label = n), 
           colour = 'black', inherit.aes = F, parse = F, size = 3.5)+
 geom_vline(aes(xintercept = 138),colour = 'red', linetype = 'dashed', size = 0.5)

# print(arm.leg.plot.lastfour)

#setwd('C:/CloudStor/R_Stuff/FIS/FIS_2020')
ggsave(filename = paste('ARM_LEG_LF_LASTFOUR_', selected.site, '.pdf', sep = ''), 
       plot = arm.leg.plot.lastfour, units = 'mm', width = 190, height = 250)
ggsave(filename = paste('ARM_LEG_LF_LASTFOUR_', selected.site, '.wmf', sep = ''), 
       plot = arm.leg.plot.lastfour, units = 'mm', width = 190, height = 250)
ggsave(filename = paste('ARM_LEG_LF_LASTFOUR_', selected.site, '.png', sep = ''), 
       plot = arm.leg.plot.lastfour, units = 'mm', width = 190, height = 250)

##--------------------------------------------------------------------------------------##
## Density plot ####

## load most recent combined ARM and LEG size frequency data set
arm.leg.counts <- readRDS('C:/CloudStor/R_Stuff/FIS/arm.leg.counts.RDS')

## exract unique sites and yr.seasons
arm.leg.sites <- unique(arm.leg.counts$site)
arm.leg.seasons <- data.frame(yr.season = unique(arm.leg.counts$yr.season))

## loop through sites to generate arm and leg plots autonomously
for (i in plot.sites){

## subset site data
arm.leg.site.den <- subset(arm.leg.counts, site == i)

# ## subset chosen site
# unique(arm.leg.counts$site)
# selected.site <- 'GAR'
# arm.leg.site.den <- subset(arm.leg.counts, site %in% selected.site)

## re-order data so that yr.season is in sequence

arm.leg.site.den$string <- factor(as.integer(arm.leg.site.den$string), levels = c(1,2))
arm.leg.site.den$yr.season <-
 ordered(arm.leg.site.den$yr.season, levels = c("2015.Summer", "2015.Winter", 
                                                "2015.Spring", "2016.Summer", 
                                                "2016.Winter", "2016.Spring", 
                                                "2017.Summer", "2017.Winter", 
                                                "2017.Spring", "2018.Summer", 
                                                "2018.Winter", "2018.Spring",
                                                "2019.Summer", "2019.Winter",
                                                '2019.Spring', '2020.Summer',
                                                '2020.Winter', '2020.Spring',
                                                '2021.Summer'))

## summarise data for arm and leg density
leg.summ <- arm.leg.site.den %>%
 filter(sampmethod == 'LEG') %>%
 group_by(string, yr.season) %>%
 summarise(leg_mean = mean(absm.leg),
           leg_n = n(),
          leg_se = sd(absm.leg)/sqrt(leg_n))

arm.summ <- arm.leg.site.den %>%
 filter(sampmethod == 'ARM') %>%
 group_by(string, yr.season) %>%
 summarise(arm_mean = mean(absm.arm),
           arm_n = n(),
           arm_se = sd(absm.arm)/sqrt(arm_n))

## arm density plot
arm_den <- ggplot()+
 geom_line(data = arm.summ, aes(x = yr.season, y = arm_mean, group = factor(string), 
                                linetype = string), position = position_dodge(0.5), colour = 'red')+
 geom_point(data = arm.summ, aes(x = yr.season, y = arm_mean, group = factor(string), 
                                 colour = string), size = 3, position = position_dodge(0.5), 
            colour = 'red')+
 geom_errorbar(data = arm.summ, aes(x = yr.season, 
                                    ymin = arm_mean - arm_se, ymax = arm_mean + arm_se, 
                                    group = factor(string), colour = string), 
               position = position_dodge(0.5), width = 0.1, colour = 'red')+
 # ylab(bquote('ARM Density ('*~m^2*')'))+
  ylab(bquote('ARM Density (no. '*~m^-2*')')) +
 scale_x_discrete(labels = season_labels, drop = F)+
 scale_color_manual(values = c('red'))+
 theme_bw()+
 #theme(legend.position = c(0.1, 0.9), legend.direction = 'vertical')+
 theme(legend.position = 'none')+
 labs(col = 'String')+
 #xlab("Season")+
 xlab(NULL)+
 coord_cartesian(ylim = c(0, 85))

## leg density plot
leg_den <- ggplot()+
 geom_line(data = leg.summ, aes(x = yr.season, y = leg_mean, group = factor(string), 
                                linetype = string), position = position_dodge(0.5), colour = 'blue')+
 geom_point(data = leg.summ, aes(x = yr.season, y = leg_mean, group = factor(string), 
                                 colour = string), size = 3, position = position_dodge(0.5), 
            colour = 'blue')+
 geom_errorbar(data = leg.summ, aes(x = yr.season, 
                                    ymin = leg_mean - leg_se, ymax = leg_mean + leg_se, group = factor(string), colour = string), position = position_dodge(0.5), width = 0.1, colour = 'blue')+
 # ylab(bquote('LEG Density ('*~m^2*')'))+
  ylab(bquote('LEG Density (no. '*~m^-2*')')) +
  scale_x_discrete(labels = season_labels, drop = F)+
 scale_color_manual(values = c('blue'))+
 theme_bw()+
 theme(legend.position = 'none')+
 labs(col = 'String')+
 xlab("Season")+
 coord_cartesian(ylim = c(0, 3))+
 theme(axis.text.x = element_text(angle = 30, hjust = 1))

## combine arm and leg plot on the same page
arm.leg.den <- grid.arrange(
 arrangeGrob(cowplot::plot_grid(arm_den + rremove('x.text'), leg_den, align = 'v', 
                                ncol = 1), ncol = 1))

## save combined plot to file 
setwd('C:/CloudStor/R_Stuff/FIS/FIS_2020')
ggsave(filename = paste('ARM_LEG_DENSITY_', i, '.pdf', sep = ''), 
       plot = arm.leg.den)
ggsave(filename = paste('ARM_LEG_DENSITY_', i, '.wmf', sep = ''), 
       plot = arm.leg.den)
ggsave(filename = paste('ARM_LEG_DENSITY_', i, '.png', sep = ''), 
       plot = arm.leg.den)
}

##--------------------------------------------------------------------------------------##
## Association plot ####

## load data sets
legs.counts.join <- readRDS('C:/CloudStor/R_Stuff/FIS/legs.counts.join.RDS')
arms.counts.join <- readRDS('C:/CloudStor/R_Stuff/FIS/arms.counts.join.RDS')

## summarise mean density for juvenile abalone from ARM data
juvmeans <- arm.leg.counts %>%
 filter(sampmethod == 'ARM') %>% 
 group_by(site, yr.season) %>%
 summarise(Juvenile = mean(absm.arm)) 

## summarise mean density for juvenile abalone from ARM data (i.e. <25 mm)
juvmeans <- arms.counts.join %>%
 group_by(site, yr.season) %>%
 summarise(Juvenile = mean(absm_juv))

## summarise mean density for sub-Legal (i.e. <138 mm) abalone from LEG data
bigabsubmeans <- legs.counts.join %>% 
 group_by(site, yr.season) %>%
 summarise(SubLegal = mean(absm_sub)) 

## summarise mean density for Legal (i.e. >138 mm) abalone from LEG data
bigablegalmeans <- legs.counts.join %>% 
 group_by(site, yr.season) %>%
 summarise(Legal = mean(absm_leg)) 

##--------------------------------------------------------------------------------------##
## Juv vs legal association plot ####

## plot total juveniles against total sublegal and legal
pooled <- left_join(juvmeans, bigabsubmeans, by = c("site", "yr.season"))
pooled <- left_join(pooled, bigablegalmeans, by = c("site", "yr.season")) %>% 
 filter(site %in% c('BET', 'BRB', 'BRS', 'GEO'))

GGally::ggpairs(pooled, columns = 3:5,  aes(colour = site), diag='blank') +
 theme(axis.text.x = element_text(angle = 0, hjust = 0.5),legend.position="right") +
 xlab(bquote('Abalone density ('*~m^2*')')) + 
 ylab(bquote('Abalone density ('*~m^2*')')) 

##--------------------------------------------------------------------------------------##
## Lagged association ####

lag1.l <- bigablegalmeans %>% 
 filter(yr.season == "2015.Spring" & site %in% c('BET', 'BRB', 'BRS', 'GEO')) %>% 
 select(site, Legal) %>% 
 dplyr::rename(Legal.2015.Spring = Legal)

lag2.l <- bigablegalmeans %>%
 filter(yr.season == "2016.Spring" & site %in% c('BET', 'BRB', 'BRS', 'GEO')) %>%
 select(site, Legal) %>%
 dplyr::rename(Legal.2016.Spring = Legal)
 
lag3.l <- bigablegalmeans %>%
 filter(yr.season == "2017.Spring" & site %in% c('BET', 'BRB', 'BRS', 'GEO')) %>%
 select(site, Legal) %>%
 dplyr::rename(Legal.2017.Spring = Legal)

lag4.l <- bigablegalmeans %>%
 filter(yr.season == "2018.Spring" & site %in% c('BET', 'BRB', 'BRS', 'GEO')) %>%
 select(site, Legal) %>%
 dplyr::rename(Legal.2018.Spring = Legal)

lag5.l <- bigablegalmeans %>%
 filter(yr.season == "2019.Spring" & site %in% c('BET', 'BRB', 'BRS', 'GEO')) %>%
 select(site, Legal) %>%
 dplyr::rename(Legal.2019.Spring = Legal)

lag1.j <- juvmeans %>% 
 filter(yr.season == "2016.Spring" & site %in% c('BET', 'BRB', 'BRS', 'GEO')) %>%
 select(site, Juvenile) %>%
 dplyr::rename(Juv.2016.Spring = Juvenile)

lag2.j <- juvmeans %>%
 filter(yr.season == "2017.Summer" & site %in% c('BET', 'BRB', 'BRS', 'GEO')) %>%
 select(site, Juvenile) %>%
 dplyr::rename(Juv.2017.Summer = Juvenile)

lag3.j <- juvmeans %>%
 filter(yr.season == "2017.Spring" & site %in% c('BET', 'BRB', 'BRS', 'GEO')) %>%
 select(site, Juvenile) %>%
 dplyr::rename(Juv.2017.Spring = Juvenile)

lag4.j <- juvmeans %>%
 filter(yr.season == "2018.Summer" & site %in% c('BET', 'BRB', 'BRS', 'GEO')) %>%
 select(site, Juvenile) %>%
 dplyr::rename(Juv.2018.Summer = Juvenile)

lag5.j <- juvmeans %>%
 filter(yr.season == "2018.Spring" & site %in% c('BET', 'BRB', 'BRS', 'GEO')) %>%
 select(site, Juvenile) %>%
 dplyr::rename(Juv.2018.Spring = Juvenile)

lag6.j <- juvmeans %>%
 filter(yr.season == "2019.Summer" & site %in% c('BET', 'BRB', 'BRS', 'GEO')) %>%
 select(site, Juvenile) %>%
 dplyr::rename(Juv.2019.Summer = Juvenile)

lag7.j <- juvmeans %>%
  filter(yr.season == "2019.Spring" & site %in% c('BET', 'BRB', 'BRS', 'GEO')) %>%
  select(site, Juvenile) %>%
  dplyr::rename(Juv.2019.Spring = Juvenile)

lag8.j <- juvmeans %>%
  filter(yr.season == "2020.Summer" & site %in% c('BET', 'BRB', 'BRS', 'GEO')) %>%
  select(site, Juvenile) %>%
  dplyr::rename(Juv.2020.Summer = Juvenile)

lag <- left_join(lag1.l, lag2.l, by = c("site"))
lag <- left_join(lag, lag3.l, by = c("site"))
lag <- left_join(lag, lag4.l, by = c("site"))
lag <- left_join(lag, lag5.l, by = c("site"))
lag <- left_join(lag, lag1.j, by = c("site"))
lag <- left_join(lag, lag2.j, by = c("site"))
lag <- left_join(lag, lag3.j, by = c("site"))
lag <- left_join(lag, lag4.j, by = c("site"))
lag <- left_join(lag, lag5.j, by = c("site"))
lag <- left_join(lag, lag6.j, by = c("site"))
lag <- left_join(lag, lag7.j, by = c("site"))
lag <- left_join(lag, lag8.j, by = c("site"))

my_fn <- function(data, mapping, ...){
 p <- ggplot(data = data, mapping = mapping) + 
  geom_point() + 
  # geom_smooth(method=loess, fill="red", color="red", ...) +
  geom_smooth(method=lm, fill="blue", color="blue", ...)
 p
}

GGally::ggpairs(lag, columns = 2:11, aes(colour = site), diag='blank', upper = 'blank', lower = list(continuous = my_fn)) +
 theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5),legend.position="right") +
 xlab(bquote('Abalone Abundance ('*~m^2*')')) + 
 ylab(bquote('Abalone Abundance ('*~m^2*')'))

##--------------------------------------------------------------------------------------##
## LEG plots ####

## Density stacked bar plot ####

## load most recent combined legs.counts.join data set
legs.counts.join <- readRDS('C:/CloudStor/R_Stuff/FIS/legs.counts.join.RDS')

## seperate dataframe into legal and sub-legal abalone to plot above or below the x-axis
sub.leg <- melt(legs.counts.join,id.vars=c('legs.dat.join.survindex', 'site', 'yr.season'), 
              measure.vars=c('absm_sub','absm_leg','absm', 'absm_sub_ten', 'absm_leg_ten'))

sub.leg.2 <- melt(legs.counts.join,id.vars=c('legs.dat.join.survindex', 'site', 'yr.season'), 
                measure.vars=c('absm_sub','absm_leg'))

sub.leg.3 <- melt(legs.counts.join,id.vars=c('legs.dat.join.survindex', 'site', 'yr.season'), 
                measure.vars=c('absm_sub_ten', 'absm_leg_ten'))

## select site
unique(sub.leg$site)
selected.site <- 'BRS'

#create negative values for sub-legal animals
sub.leg.dat <- sub.leg.2 %>%
 filter(site %in% selected.site) %>% #only use filter for individuals sites otherwise remove filter and use facet_grid
 mutate(value = case_when(variable == 'absm_sub' ~ -value, TRUE ~ value)) #%>%
 #mutate(value = case_when(variable == 'absm_sub_ten' ~ -value, TRUE ~ value))#make sub-legal abs negative

sub.leg.dat.2 <- sub.leg.3 %>%
 filter(site %in% selected.site) %>% #only use filter for individuals sites otherwise remove filter and use facet_grid
 mutate(value = case_when(variable == 'absm_sub_ten' ~ -value, TRUE ~ value))

sub.leg.colours.2 = c("grey", "white")
sub.leg.colours = c("white", "black")
         
leg.den.bar <- ggplot(sub.leg.dat, aes(x = yr.season, y = value, fill = variable))+
 stat_summary(geom = 'bar', position = 'identity', fun.data = my.stderr, colour = 'black')+
 stat_summary(fun.ymax = errorUpper2, fun.ymin = mean,
               geom = 'errorbar', position = 'identity', colour = 'black', width = 0.2)+
 stat_summary(fun.ymax = errorUpper2, fun.ymin = errorUpper2,
              geom = 'linerange', position = 'identity', colour = 'black')+
 scale_colour_manual()+
 theme_bw()+
 #xlab('Season')+
 xlab(NULL)+
 ylab(bquote('Abalone Density ('*~m^2*')'))+
 labs(fill = 'Size')+
 scale_fill_manual(values = sub.leg.colours,
  name = 'Size class', breaks = c('absm', 'absm_leg', 'absm_sub', "absm_sub_ten"),
                     labels = c(' All BL', ' Legal ', ' Sub-legal', " <10 mm legal"))+
 theme(legend.title = element_blank())+
 theme(axis.text.x = element_text(angle = 0, vjust = 0.5))+
 theme(legend.justification = c(0, 1), legend.position = c(0, 1), 
       legend.direction = 'horizontal')+
 #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
 theme(legend.background = element_blank())+
 coord_cartesian(ylim = c(-2, 2))+
 annotate('text', x = c(1, 2, 11), y = 0.6, label = 'NO DATA', angle = 90)+ 
 #enter manually but work on conditional statement
 scale_x_discrete(breaks = c("2015.Summer", "2015.Winter", "2015.Spring", 
                             "2016.Summer", "2016.Winter", "2016.Spring", 
                             "2017.Summer", "2017.Winter", "2017.Spring", 
                             "2018.Summer", "2018.Winter", "2018.Spring", 
                             "2019.Summer", "2019.Winter", "2019.Spring",
                             "2020.Summer"), labels = season_labels, drop = F)

leg.den.bar.2 <- ggplot(sub.leg.dat.2, aes(x = yr.season, y = value, fill = variable))+
 stat_summary(geom = 'bar', position = 'identity', fun.data = my.stderr, colour = 'black')+
 stat_summary(fun.ymax = errorUpper2, fun.ymin = mean,
              geom = 'errorbar', position = 'identity', colour = 'black', width = 0.2)+
 stat_summary(fun.ymax = errorUpper2, fun.ymin = errorUpper2,
              geom = 'linerange', position = 'identity', colour = 'black')+
 scale_colour_manual()+
 theme_bw()+
 xlab('Season')+
 ylab(bquote('Abalone Density ('*~m^2*')'))+
 labs(fill = 'Size')+
 scale_fill_manual(values = sub.leg.colours.2,
                   name = 'Size class', breaks = c("absm_sub_ten","absm_leg_ten"),
                   labels = c(" <10 mm legal ", " >10 mm legal"))+
 theme(legend.title = element_blank())+
 theme(legend.justification = c(0, 1), legend.position = c(0, 1), 
       legend.direction = 'horizontal')+
 #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
 theme(legend.background = element_blank())+
 coord_cartesian(ylim = c(-0.5, 0.5))+
 annotate('text', x = c(1, 2, 11), y = 0.15, label = 'NO DATA', angle = 90)+ #manually add no data to missing season data 
 #enter manually but work on conditional statement
 scale_x_discrete(breaks = c("2015.Summer", "2015.Winter", "2015.Spring", 
                             "2016.Summer", "2016.Winter", "2016.Spring", 
                             "2017.Summer", "2017.Winter", "2017.Spring", 
                             "2018.Summer", "2018.Winter", "2018.Spring", 
                             "2019.Summer", "2019.Winter", "2019.Spring",
                             "2020.Summer"), labels = season_labels, drop = F)+
 theme(axis.text.x = element_text(angle = 30, vjust = 0.5))

leg.density <- grid.arrange(
 arrangeGrob(cowplot::plot_grid(leg.den.bar + rremove('x.text'), leg.den.bar.2, 
                                align = 'v', ncol = 1), ncol = 1))

## save plot to file 
setwd('C:/CloudStor/R_Stuff/FIS/FIS_2020')
ggsave(filename = paste('LEG_DENSITY_', selected.site, '.pdf', sep = ''), plot = leg.density)
ggsave(filename = paste('LEG_DENSITY_', selected.site, '.wmf', sep = ''), plot = leg.density)

##--------------------------------------------------------------------------------------##
plot.leg.den <- legs.counts.join %>% 
  filter(site == 'GEO' &
           sampyear != 2021) %>% 
  group_by(string, yr.season) %>%
  summarise(leg_mean = mean(absm_leg),
            leg_n = n(),
            leg_se = sd(absm_leg)/sqrt(leg_n)) %>% 
  ggplot(aes(x = yr.season, y = leg_mean, group = string, colour = string))+
  geom_line(aes(x = yr.season, y = leg_mean, group = factor(string), 
                                 linetype = string), position = position_dodge(0.5), colour = 'blue')+
  geom_point(aes(x = yr.season, y = leg_mean, group = factor(string), 
                                  colour = string), size = 3, position = position_dodge(0.5), 
             colour = 'blue')+
  geom_errorbar(aes(x = yr.season, ymin = leg_mean - leg_se, ymax = leg_mean + leg_se, group = factor(string), colour = string), position = position_dodge(0.5), width = 0.1, colour = 'blue')+
  geom_smooth(aes(x = yr.season, y = leg_mean, group = string), method = 'lm', formula = y~x, se = T, size = 1)+
  ggpmisc::stat_poly_eq(formula = y~x, aes(label = paste(..rr.label.., p.value.label, sep = "~~~")),
               parse = TRUE, label.x = 'left', label.y = 'top')+
  ylab(bquote('Legal >138 mm LEG Density (no. '*~m^-2*')')) +
  # scale_x_discrete(labels = season_labels, drop = F)+
  scale_color_manual(values = c('blue', 'red'))+
  theme_bw()+
  theme(legend.position = 'none')+
  labs(col = 'String')+
  xlab("Season")+
  coord_cartesian(ylim = c(0, 0.35))+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(
  filename = paste('GEO_LegalDensity_Summer2021', '.pdf', sep = ''),
  plot = plot.leg.den,
  width = 7.4,
  height = 5.57,
  units = 'in'
)

ggsave(
  filename = paste('GEO_LegalDensity_Summer2021', '.png', sep = ''),
  plot = plot.leg.den,
  width = 7.4,
  height = 5.57,
  units = 'in'
)

