#clear environment
#rm(list=ls(all=TRUE))

##--------------------------------------------------------------------------------------##
## Load libaries ####
library(tidyverse)
library(broom)
library(lubridate)
library(GGally)
library(scales)
library(gdata)
library(effsize)
library(openxlsx)
library(Hmisc)
library(reshape2)
library(dplyr)
library(ggsci)
library(effsize)

##--------------------------------------------------------------------------------------##
## Functions ####

## Create function to assign sampling records to a season
getSeason.reef <- function(DATES) {
        Wi <- as.Date("2012-06-01", format = "%Y-%m-%d") # Winter 
        Sp <- as.Date("2012-09-01",  format = "%Y-%m-%d") # Spring 
        Su <- as.Date("2012-12-01",  format = "%Y-%m-%d") # Summer 
        # Looks like autumn threshold date is a month out so I have changed - Jaime
        #Au <- as.Date("2012-04-01",  format = "%Y-%m-%d") # Autumn 
        Au <- as.Date("2012-03-01",  format = "%Y-%m-%d") # Autumn
        
        # Convert dates from any year to 2012 dates
        d <- as.Date(strftime(DATES, format="2012-%m-%d"))
        
        ifelse (d >= Wi & d < Sp, "Winter",
                ifelse (d >= Sp & d < Su, "Spring",
                        ifelse (d >= Su | d < Au, "Summer", "Autumn")))
}

## Create function for standard error calculations
stderr.reef <- function(x) {
        sqrt(var(x[!is.na(x)]) / length(x[!is.na(x)]))
}

my.stderr.reef <- function(x) {
        meany <- mean(x)
        ymin  <- mean(x) - stderr.reef(x)
        ymax  <- mean(x) + stderr.reef(x)
        # assemble the named output
        out <- c(y = meany, ymin = ymin, ymax = ymax)
        return(out)
}

## Load RI data ####

juv_lip <- read.xlsx(
        "R:/TAFI/TAFI_MRL_Sections/Marine Environment/Section Shared/2015 FRDC 024 Environmental Interactions/Reef interactions/Abalone plates/Lippies/Data/Abalone plates_Juv Abs & Inv assemblages_DataLippies Cleaned.xlsx",
        sheet = "Juvenile Abalone",
        detectDates = TRUE)

juv_trump <- read.xlsx(
        "R:/TAFI/TAFI_MRL_Sections/Marine Environment/Section Shared/2015 FRDC 024 Environmental Interactions/Reef interactions/Abalone plates/Trumpeter/Data/Abalone plates_Juv Abs & Inv assemblages_DataTrumpeter Cleaned.xlsx",
        sheet = "Juvenile Abalone",
        detectDates = TRUE)

juv_lip_resurvey <-read.xlsx(
        "R:/TAFI/TAFI_MRL_Sections/Marine Environment/Section Shared/2018 RVA method validation/Abalone plate resurvey/Abalone Plate Recon Data.xlsx",
        sheet = "Sheet1",
        detectDates = TRUE)

##--------------------------------------------------------------------------------------##
## set working directory
setwd('R:/TAFI/TAFI_MRL_Sections/Marine Environment/Section Shared/2018 RVA method validation/Abalone plate resurvey')

##--------------------------------------------------------------------------------------##
## Compile RI data ####

## check structure of raw data to ensure bind_rows will work properly
str(juv_lip)
str(juv_trump)
str(juv_lip_resurvey)

## fix juv_lip$SIZE.AB1 variable where 'Plate missing' is in wrong column and convert from 
## character to numeric
juv_lip <- juv_lip %>% 
        mutate(NO.ABS = if_else(SIZE.AB1 == 'Plate missing', SIZE.AB1, NO.ABS),
               SIZE.AB1 = as.numeric(gsub('Plate missing', NA_character_, SIZE.AB1)))

## merge raw data sets
juv <- bind_rows(juv_lip, juv_trump, juv_lip_resurvey)

##--------------------------------------------------------------------------------------##
## Clean RI data ####

## convert varible names to lower case
colnames(juv) <- tolower(colnames(juv))

## convert juv data from wide to long format
juv_long <- melt(juv, id.var = c('site', 'date', 'diver', 'collector', 'no.abs'), value.name = 'length')

## seperate string number from site
juv_long_split <- juv_long %>%
 separate(site, 
          into = c("site", "string"), 
          sep = "(?<=[A-Za-z])(?=[0-9])")

## rename column variables
names(juv_long_split)[names(juv_long_split)=="collector"] <- "plate"
names(juv_long_split)[names(juv_long_split)=="length"] <- "ab_sl"
names(juv_long_split)[names(juv_long_split)=="date"] <- "survdate"

## convert string names to only 1 and 2
juv_long_split <- juv_long_split %>%
 mutate(string = gsub('3', '1', string),
        string = gsub('4', '2', string),
        string = gsub('5', '1', string),
        string = gsub('6', '2', string))

## convert variables to factors
juv_long_split$string <- as.factor(juv_long_split$string)
juv_long_split$plate <- as.factor(juv_long_split$plate)

## convert site names to match abalone assessment terminology
juv_long_split <- juv_long_split %>% 
        mutate(site = gsub('GIII', 'GEO', site))

##--------------------------------------------------------------------------------------##
## RI abalone data ####

juv.df <- juv_long_split %>% 
        select(site, survdate, diver, string, plate, ab_sl)

## save a copy of the R files
saveRDS(juv.df, 'R:/TAFI/TAFI_MRL_Sections/Marine Environment/Section Shared/2018 RVA method validation/Abalone plate resurvey/juv.df.RDS')

##--------------------------------------------------------------------------------------##
## RI Length data ####
juv.sl <- filter(juv.df, !is.na(ab_sl))

## add quarter, year and season variables
#juv.sl$q <- quarter(juv.sl$survdate, with_year = TRUE)
juv.sl$sampyear <- as.factor(year(juv.sl$survdate)) 
juv.sl$season <- getSeason.reef(juv.sl$survdate) 

## add yr.season variable and arrange in order
juv.sl$season <- as.factor(juv.sl$season)
juv.sl$season <- ordered(juv.sl$season, levels=c("Summer","Winter","Spring", 'Autumn'))
juv.sl$yr.season <- interaction(juv.sl$sampyear,juv.sl$season)
# levels(juv.sl$yr.season)
juv.sl$yr.season <-
 ordered(juv.sl$yr.season, levels = c("2016.Winter", "2016.Spring", "2016.Summer", 
                                      "2017.Autumn", "2017.Winter", "2017.Spring", "2017.Summer", 
                                      "2018.Autumn", "2018.Winter", '2018.Spring', "2018.Summer",
                                      "2019.Autumn", "2019.Winter", "2019.Spring"))

## save a copy of the R files
saveRDS(juv.sl, 'R:/TAFI/TAFI_MRL_Sections/Marine Environment/Section Shared/2018 RVA method validation/Abalone plate resurvey/juv.sl.RDS')

##--------------------------------------------------------------------------------------##
## RI density data ####

## plate area for calculating density
platearea <- 0.126

## make copy of juv.df
juv.den <- juv.df

## create unique ID/index for each ARM and survdate combination
juv.den$survindex <- as.factor(paste(juv$site, juv$survdate, juv$string, juv$plate, sep="_"))

## subset and count number of animals per ARM by survdate (subset by size class if required)
juv.dat <- filter(juv.den, !is.na(ab_sl))  %>%
 #dat <- filter(juv.den, ab_sl >=25 & ab_sl < 100) %>%
 #dat <- filter(juv.den, ab_sl <25 ) %>% 
 #dat <- filter(juv.den, ab_sl <= 100) %>%
 group_by(survindex) %>%
 summarise(ab_n=n()) %>%  #as.data.frame()
 complete(survindex, fill = list(ab_n = 0)) %>%
 as.data.frame()

## calculate abalone per square metre 
juv.dat$absm <- juv.dat$ab_n * (1/platearea)

## unpack survindex variables and create new dataframe
juv.abcounts <- data.frame(separate(juv.dat, survindex, sep = "_", 
                                into = c("site", "survdate", "string", "plate"), 
                                convert = TRUE), juv.dat$survindex, juv.dat$ab_n, juv.dat$absm)

## format date variable and add year and season variables
juv.abcounts$survdate <- as.Date(strptime(juv.abcounts$survdate, "%Y-%m-%d"))
juv.abcounts$sampyear <- as.factor(year(juv.abcounts$survdate)) 
juv.abcounts$season <- getSeason.reef(juv.abcounts$survdate) 

## add yr.season variable and arrange in order
juv.abcounts$yr.season <- interaction(juv.abcounts$sampyear,juv.abcounts$season)
juv.abcounts$yr.season <-
 ordered(juv.abcounts$yr.season, levels = c("2016.Winter", "2016.Spring", "2016.Summer", 
                                            "2017.Autumn", "2017.Winter", "2017.Spring", "2017.Summer", 
                                            "2018.Autumn", "2018.Winter", '2018.Spring', "2018.Summer",
                                            "2019.Autumn", "2019.Winter", "2019.Spring"))

juv.abcounts <- juv.abcounts %>% 
        select(-c(juv.dat.ab_n, juv.dat.absm)) %>% 
        rename(survindex = juv.dat.survindex)

## save a copy of the R files
saveRDS(juv.abcounts, 'R:/TAFI/TAFI_MRL_Sections/Marine Environment/Section Shared/2018 RVA method validation/Abalone plate resurvey/juv.abcounts.RDS')

##--------------------------------------------------------------------------------------##
## AB abalone data ####
## load most recent compiled ARM size and density data from Abalone Fishery Independant Surveys

arms.sl <- readRDS('C:/CloudStor/R_Stuff/FIS/arms.sl.RDS')

arm.counts <- readRDS('C:/CloudStor/R_Stuff/FIS/arm.counts.RDS')

# re-classify abalone data seasons to match Reef Interactions seasons
arms.sl <- arms.sl %>%
        select(-season, yr.season) %>% 
        mutate(season = getSeason.reef(survdate),
               yr.season = interaction(sampyear, season))

arm.counts <- arm.counts %>%
        select(-season, yr.season) %>% 
        mutate(season = getSeason.reef(survdate),
               yr.season = interaction(sampyear, season))

##--------------------------------------------------------------------------------------##
# Join density data ####
# join abalone density data to reef interactions density data

# filter abalone data for matching sites and seasons spanning the reef interactions study
arm.abcounts <- arm.counts %>% 
        filter(site %in% c('BET', 'GEO', 'BRS') &
                       yr.season %in% c('2016.Summer',
                                        '2016.Spring',
                                        '2016.Winter',
                                        '2017.Autumn',
                                        '2017.Winter',
                                        '2017.Spring',
                                        '2017.Summer',
                                        '2018.Autumn',
                                        '2018.Winter',
                                        '2018.Spring',
                                        '2019.Summer',
                                        '2019.Autumn'))

# remove repeated variables and rename variables to match reef interactions data
arm.abcounts <- arm.abcounts %>% 
        select(-c(dat.ab_n, dat.absm)) %>% 
        rename(survindex = dat.survindex) %>% 
        mutate(string = as.factor(string))

## combine reef interactions and abalone dataframes noting that there are 12 records where the abalone counts 
## vary between the two datasets by <3 abalone for GEO and BRS. 
## decision made to remove duplicate records by using only data for GEO and BRS abalone database.

# # indentify difference in counts between data sets and retain only mis-matched records
# juv.abcounts.mismatch <- left_join(juv.abcounts, arm.abcounts, by = "survindex") %>% 
#         mutate(ab_n.diff = ab_n.x - ab_n.y) %>%
#         filter(!is.na(ab_n.diff) & 
#                        ab_n.diff != 0) %>%
#         # mutate(ab_n.diff = 1) %>% 
#         select(survindex, ab_n.diff) %>% 
#         as.data.frame()
# 
# # join mis-matched records to reef interactions data and remove mis-matched data before binding to abalone data
# # and removing remaining duplicate records
# arm.abcounts.df <- left_join(juv.abcounts, juv.abcounts.mismatch, by = 'survindex') %>% 
#         filter(is.na(ab_n.diff)) %>% 
#         bind_rows(arm.abcounts) %>% 
#         distinct(survindex, .keep_all = T) %>% 
#         select(-ab_n.diff)

# filter for unique reef interactions data and join to abalone data
arm.abcounts.df <- juv.abcounts %>% 
        filter(!site %in% c('GEO', 'BRS')) %>% 
        # mutate(survdate = as.POSIXct(survdate)) %>% 
        bind_rows(arm.abcounts)

# convert summer to autumn for all data and place in order
arm.abcounts.df <- arm.abcounts.df %>% 
        mutate(season = gsub('Summer', 'Autumn', season),
               yr.season = gsub('Summer', 'Autumn', yr.season))

arm.abcounts.df$yr.season <-
        ordered(df.2$yr.season, levels = c("2016.Autumn",
                                                "2016.Winter",
                                                "2016.Spring",
                                                "2017.Autumn",
                                                "2017.Winter", 
                                                "2017.Spring",
                                                "2018.Autumn",
                                                "2018.Winter",
                                                "2018.Spring",
                                                "2019.Autumn"))

## save a copy of the R files
saveRDS(arm.abcounts.df, 'R:/TAFI/TAFI_MRL_Sections/Marine Environment/Section Shared/2018 RVA method validation/Abalone plate resurvey/arm.abcounts.df.RDS')

##--------------------------------------------------------------------------------------##
# Join length data ####
# join abalone size data to reef interaction size data

# filter abalone data for matching sites and seasons spanning the reef interactions study
arms.absl <- arms.sl %>% 
        filter(site %in% c('BET', 'GEO', 'BRS') &
                       yr.season %in% c('2016.Summer',
                                        '2016.Spring',
                                        '2016.Winter',
                                        '2017.Autumn',
                                        '2017.Winter',
                                        '2017.Spring',
                                        '2017.Summer',
                                        '2018.Autumn',
                                        '2018.Winter',
                                        '2018.Spring',
                                        '2019.Summer',
                                        '2019.Autumn'))

# select variables to match reef interactions data
arms.absl.match <- arms.absl %>% 
        select(c(site, survdate, string, plate, sllength, sampyear, season, yr.season)) %>% 
        rename(ab_sl = sllength)

# filter for unique reef interactions data and join to abalone data
arm.sl.df <- juv.sl %>% 
        filter(!site %in% c('GEO', 'BRS')) %>% 
        mutate(survdate = as.POSIXct(survdate)) %>% 
        bind_rows(arms.absl.match)

# convert summer to autumn for all data and place in order
arm.sl.df <- arm.sl.df %>% 
        mutate(season = gsub('Summer', 'Autumn', season),
               yr.season = gsub('Summer', 'Autumn', yr.season))

arm.sl.df$yr.season <-
        ordered(arm.sl.df$yr.season, levels = c("2016.Autumn",
                                                "2016.Winter",
                                                "2016.Spring",
                                                "2017.Autumn",
                                                "2017.Winter", 
                                                "2017.Spring",
                                                "2018.Autumn",
                                                "2018.Winter",
                                                "2018.Spring",
                                                "2019.Autumn"))

## save a copy of the R files
saveRDS(arm.sl.df, 'R:/TAFI/TAFI_MRL_Sections/Marine Environment/Section Shared/2018 RVA method validation/Abalone plate resurvey/arm.sl.df.RDS')

##--------------------------------------------------------------------------------------##
## Plots: data ####

## load latest RDS data
arm.sl.df <- readRDS('R:/TAFI/TAFI_MRL_Sections/Marine Environment/Section Shared/2018 RVA method validation/Abalone plate resurvey/arm.sl.df.RDS')
arm.abcounts.df <- readRDS('R:/TAFI/TAFI_MRL_Sections/Marine Environment/Section Shared/2018 RVA method validation/Abalone plate resurvey/arm.abcounts.df.RDS')

# create short label names for plot facets  
season_labels <- c("2016.Autumn" = '2016.Au',
                   "2016.Winter" = '2016.Wi',
                   "2016.Spring" = '2016.Sp',
                   "2017.Autumn" = '2017.Au',
                   "2017.Winter" = '2017.Wi', 
                   "2017.Spring" = '2017.Sp',
                   "2018.Autumn" = '2018.Au',
                   "2018.Winter" = '2018.Wi',
                   "2018.Spring" = '2018.Sp',
                   "2019.Autumn" = '2019.Au')

# dataframe for plot colours when split by region (from scales package pal_jco)
plot.colours <- c(
        "SIS" = '#8F7700FF',
        "LIP" = '#003C67FF',
        "CQE" = '#CD534CFF',
        "BBS" = '#0073C2FF',
        "TBN" = '#3B3B3BFF',
        "BET" = '#EFC000FF',
        "BRS" = '#868686FF',
        "GEO" = '#7AA6DCFF' 
)

# plot.colours.2 <- data.frame(site = c("SIS", "LIP", "CQE", "BBS", "TBN", "BET", "BRS", "GEO"),
#                             fill.color = c('#8F7700FF', '#003C67FF', '#CD534CFF', '#0073C2FF', '#3B3B3BFF', '#EFC000FF', '#868686FF', '#7AA6DCFF'),
#                             fill.alpha = 0.6)

##--------------------------------------------------------------------------------------##
## Plot 1: LF histogram ####
## length frequency plot of all sites x year.season

# create geom_text label for number of samples 
plot.n.ARM <- arm.sl.df %>% 
        group_by(site, yr.season) %>%
        summarise(n = paste('n =', n()))

# create plot
arm.size.plot <- ggplot(arm.sl.df, aes(x = ab_sl, color = site, fill = site)) + 
 ylab("Frequency") +
 xlab("Shell Length (mm)")+
 theme_bw()+
 geom_histogram(binwidth = 10) +
 geom_text(data = plot.n.ARM, aes(x = 125, y = 35, label = n), color = 'black', size = 3)+
 scale_fill_jco(alpha = 0.6)+
 scale_color_jco()+
 theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) + 
 theme(legend.position="none") +
 facet_grid(yr.season ~ site, labeller = labeller(yr.season = season_labels))

# print(arm.size.plot)

setwd('R:/TAFI/TAFI_MRL_Sections/Marine Environment/Section Shared/2018 RVA method validation/Abalone plate resurvey')
ggsave(
        filename = paste('Reef Interactions_AbaloneResurvey_SizeFrequency_Au2016_Au2019', '.pdf', sep = ''),
        plot = arm.size.plot,
        width = 8.3,
        height = 11.7,
        units = 'in'
)

ggsave(
        filename = paste('Reef Interactions_AbaloneResurvey_SizeFrequency_Au2016_Au2019', '.png', sep = ''),
        plot = arm.size.plot,
        width = 8.3,
        height = 11.7,
        units = 'in'
)
##--------------------------------------------------------------------------------------##

## create summary table of abalone per squre meter for site and year.season
ab_n.summary <- arm.abcounts.df %>% 
 group_by(site, yr.season) %>%
 summarise(ab_n = sum(ab_n))

# ## boxplot showing year x season abundance
# 
# ggplot(arm.abcounts.df, aes(y = absm, x = site))+
#  geom_boxplot(outlier.colour = "orange", outlier.size = 1.5)+
#  theme_bw()+
#  facet_grid(season ~ sampyear)+
#  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
#  xlab('Site')+
#  ylab(bquote('Abalone Abundance ('*~m^2*')'))

##--------------------------------------------------------------------------------------##
## Plot 2: BP length ####
## boxplot of regional lengths vs year.season

# create geom_text label for number of samples 
plot.n.ARM2 <- arm.sl.df %>% 
        group_by(site, yr.season) %>%
        summarise(ab_n = n())

# split survey sites into regional values
trump.sl <- c('TBN', 'CQE', 'BBS', 'BET')
lipp.sl <- c('LIP', 'BRS', 'GEO', 'SIS')

# create plot for Lippies survey
lipp.sl.bp <- arm.sl.df %>% 
        filter(site %in% lipp.sl) %>% 
        ggplot(aes(y = ab_sl, x = yr.season, fill = site))+
        geom_boxplot(outlier.colour = "orange", outlier.size = 1.5, alpha = 0.8)+
        stat_summary(fun.y = mean, geom = 'point', size = 1.5, 
                     colour = 'red', fill = 'red')+
        theme_bw()+
        scale_x_discrete(labels = season_labels, drop = F)+
        theme(axis.text.x = element_text(angle = 0, hjust = 0.5))+
        xlab('Year.Season')+
        ylab(bquote('Shell length (mm)'))+
        geom_hline(aes(yintercept = 25),colour = 'red', linetype = 'dashed', size = 0.5)+
        theme(legend.position = 'none')+
        scale_fill_manual(values = plot.colours)+
        geom_text(data = plot.n.ARM2 %>% filter(site %in% lipp.sl), 
                  aes(x = yr.season, y = 150, label = ab_n), 
                  colour = 'black', inherit.aes = F, parse = F, size = 3)+
        facet_grid(site ~ .)

# print(lipp.sl.bp)

trump.sl.bp <- arm.sl.df %>% 
        filter(site %in% trump.sl) %>% 
        ggplot(aes(y = ab_sl, x = yr.season, fill = site))+
        geom_boxplot(outlier.colour = "orange", outlier.size = 1.5, alpha = 0.8)+
        stat_summary(fun.y = mean, geom = 'point', size = 1.5, 
                     colour = 'red', fill = 'red')+
        theme_bw()+
        scale_x_discrete(labels = season_labels, drop = F)+
        theme(axis.text.x = element_text(angle = 0, hjust = 0.5))+
        xlab('Year.Season')+
        ylab(bquote('Shell length (mm)'))+
        geom_hline(aes(yintercept = 25),colour = 'red', linetype = 'dashed', size = 0.5)+
        theme(legend.position = 'none')+
        scale_fill_manual(values = plot.colours)+
        geom_text(data = plot.n.ARM2 %>% filter(site %in% trump.sl), 
                  aes(x = yr.season, y = 150, label = ab_n), 
                  colour = 'black', inherit.aes = F, parse = F, size = 3)+
        facet_grid(site ~ .)

# print(trump.sl.bp)

setwd('R:/TAFI/TAFI_MRL_Sections/Marine Environment/Section Shared/2018 RVA method validation/Abalone plate resurvey')
ggsave(
        filename = paste('Reef Interactions_AbaloneResurvey_LippiesSizeBoxPlot_Au2016_Au2019', '.pdf', sep = ''),
        plot = lipp.sl.bp
)

ggsave(
        filename = paste('Reef Interactions_AbaloneResurvey_LippiesSizeBoxPlot_Au2016_Au2019', '.png', sep = ''),
        plot = lipp.sl.bp
)
ggsave(
        filename = paste('Reef Interactions_AbaloneResurvey_TrumpeterSizeBoxPlot_Au2016_Au2019', '.pdf', sep = ''),
        plot = trump.sl.bp
)

ggsave(
        filename = paste('Reef Interactions_AbaloneResurvey_TrumpeterSizeBoxPlot_Au2016_Au2019', '.png', sep = ''),
        plot = trump.sl.bp
)

##--------------------------------------------------------------------------------------##        
## frequency distribution of number per arm x yr.season

ggplot(arm.abcounts.df, aes(x = ab_n, fill = site, color = site)) + #can change fill = string
 ylab("Frequency") +
 xlab("no. abalone per ARM")+
 geom_histogram(alpha = 0.2, binwidth = 1)+
 #ggtitle(paste(dum$SubBlockNo, FishYear))+
 #labs(title= Yeardum$SubBlockNo, size=10)+
 #geom_histogram(binwidth=50)+
 theme_bw()+
 scale_fill_jco(alpha = 0.6)+
 scale_color_jco()+
 theme(legend.position="none") +
 facet_grid(yr.season ~ site, labeller = labeller(yr.season = season_labels))

##--------------------------------------------------------------------------------------##
# ggplot(juv.sl, aes(x=ab_sl, group=as.factor(site), color=as.factor(site))) +
#  geom_histogram(stat = "bin", colour="grey", binwidth = 5)+
#  scale_x_continuous(limits=c(0, 150))

## juvenile abundance/m2 plot of year.season x site

# abcounts$string <- factor(as.integer(abcounts$string), levels = c(1,2))
# ## juveniles as abalone/m2
# ggplot(abcounts, aes(x=yr.season, y=absm, group = string)) +
#  aes(colour = string) +  theme_bw() +
#  #xlab("Season") + #ggtitle("Shell length 0mm to 100mm") +
#  ylab(bquote('Abalone Abundance ('*~m^2*')')) +
#  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
# # coord_cartesian(ylim = c(0, 15)) +
#  stat_summary(geom="line", position=position_dodge(0.2), fun.data=my.stderr, size=1) + #fun.y=mean, linetype="dashed")+
#  stat_summary(geom="point", position=position_dodge(0.2), fun.data=my.stderr) +
#  stat_summary(geom="errorbar", position=position_dodge(0.2), fun.data=my.stderr, width = 0.125, size = 1) +
#  facet_grid(site ~ ., scales = "free_y" )
##--------------------------------------------------------------------------------------##
## Plot 3: ARM density #### 
## ARM density line plot per site x string x yr.season

# ensure string is a factor
arm.abcounts.df$string <- factor(as.integer(arm.abcounts.df$string), levels = c(1,2))

# create line plot
arm.abcount.plot <- ggplot(arm.abcounts.df, aes(x = yr.season, y = absm, group = string)) + 
 aes(colour = site) +
 scale_color_jco(alpha = 1)+
 theme_bw() +
 xlab("Year.Season") + #ggtitle("Shell length 0mm to 100mm") +
 ylab(bquote('ARM Density (no. '*~m^-2*')')) +
        scale_x_discrete(labels = season_labels, drop = F)+
 theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
 # coord_cartesian(ylim = c(0, 30)) +
 stat_summary(geom = "line", position = position_dodge(0.2), fun.data = my.stderr.reef, size = 0.5, aes(linetype = string)) + #fun.y=mean, linetype="dashed")+
 stat_summary(geom = "point", position = position_dodge(0.2), fun.data = my.stderr.reef) +
 stat_summary(geom = "errorbar", position = position_dodge(0.2), fun.data = my.stderr.reef, width = 0.125, size = 0.5) +
 theme(legend.position = 'none')+
 facet_grid(site ~ ., scales = "free_y")

# print(arm.abcount.plot)

# save plot
setwd('R:/TAFI/TAFI_MRL_Sections/Marine Environment/Section Shared/2018 RVA method validation/Abalone plate resurvey')
ggsave(
        filename = paste('Reef Interactions_AbaloneResurvey_ARMDensity_Au2016_Au2019', '.pdf', sep = ''),
        plot = arm.abcount.plot,
        width = 8.3,
        height = 11.7,
        units = 'in'
)

ggsave(
        filename = paste('Reef Interactions_AbaloneResurvey_ARMDensity_Au2016_Au2019', '.png', sep = ''),
        plot = arm.abcount.plot,
        width = 8.3,
        height = 11.7,
        units = 'in'
)
##--------------------------------------------------------------------------------------##
## Plot 4: ARM density #### 
## ARM regional density line plot per site x string x yr.season

# split survey sites into regional values
trump.counts <- c('TBN', 'CQE', 'BBS', 'BET')
lipp.counts <- c('LIP', 'BRS', 'GEO', 'SIS')

# ensure string is a factor
arm.abcounts.df$string <- factor(as.integer(arm.abcounts.df$string), levels = c(1,2))

# create Lippies line plot
arm.lipp.abcount.plot <- arm.abcounts.df %>% 
        filter(site %in% lipp.counts) %>% 
        ggplot(aes(x = yr.season, y = absm, group = string)) + 
        aes(colour = site) +
        scale_color_manual(values = plot.colours, aes(alpha = 1))+
        theme_bw() +
        xlab("Year.Season") + #ggtitle("Shell length 0mm to 100mm") +
        ylab(bquote('ARM Density (no. '*~m^-2*')')) +
        scale_x_discrete(labels = season_labels, drop = F)+
        theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
        # coord_cartesian(ylim = c(0, 30)) +
        stat_summary(geom = "line", position = position_dodge(0.2), fun.data = my.stderr.reef, size = 0.5, aes(linetype = string)) + #fun.y=mean, linetype="dashed")+
        stat_summary(geom = "point", position = position_dodge(0.2), fun.data = my.stderr.reef) +
        stat_summary(geom = "errorbar", position = position_dodge(0.2), fun.data = my.stderr.reef, width = 0.125, size = 0.5) +
        theme(legend.position = 'none')+
        facet_grid(site ~ ., scales = "free_y")

# create Trumpeter line plot
arm.trump.abcount.plot <- arm.abcounts.df %>% 
        filter(site %in% trump.counts) %>% 
        ggplot(aes(x = yr.season, y = absm, group = string)) + 
        aes(colour = site) +
        scale_color_manual(values = plot.colours, aes(alpha = 1))+
        theme_bw() +
        xlab("Year.Season") + #ggtitle("Shell length 0mm to 100mm") +
        ylab(bquote('ARM Density (no. '*~m^-2*')')) +
        scale_x_discrete(labels = season_labels, drop = F)+
        theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
        # coord_cartesian(ylim = c(0, 30)) +
        stat_summary(geom = "line", position = position_dodge(0.2), fun.data = my.stderr.reef, size = 0.5, aes(linetype = string)) + #fun.y=mean, linetype="dashed")+
        stat_summary(geom = "point", position = position_dodge(0.2), fun.data = my.stderr.reef) +
        stat_summary(geom = "errorbar", position = position_dodge(0.2), fun.data = my.stderr.reef, width = 0.125, size = 0.5) +
        theme(legend.position = 'none')+
        facet_grid(site ~ ., scales = "free_y")

# print(arm.lipp.abcount.plot)

# save plot
setwd('R:/TAFI/TAFI_MRL_Sections/Marine Environment/Section Shared/2018 RVA method validation/Abalone plate resurvey')
ggsave(
        filename = paste('Reef Interactions_AbaloneResurvey_LippiesARMDensity_Au2016_Au2019', '.pdf', sep = ''),
        plot = arm.lipp.abcount.plot
)

ggsave(
        filename = paste('Reef Interactions_AbaloneResurvey_LippiesARMDensity_Au2016_Au2019', '.png', sep = ''),
        plot = arm.lipp.abcount.plot
)

ggsave(
        filename = paste('Reef Interactions_AbaloneResurvey_TrumpeterARMDensity_Au2016_Au2019', '.pdf', sep = ''),
        plot = arm.trump.abcount.plot
)

ggsave(
        filename = paste('Reef Interactions_AbaloneResurvey_TrumpeterARMDensity_Au2016_Au2019', '.png', sep = ''),
        plot = arm.trump.abcount.plot
)

##--------------------------------------------------------------------------------------##
## Plot 5: ARM density #### 
## ARM density of site strings combined x yr.season

arm.abcount.plot.combined <- ggplot(arm.abcounts.df, aes(x = yr.season, y = absm, group = site)) + 
        aes(colour = site) +
        scale_color_jco(alpha = 1)+
        theme_bw() +
        xlab("Year.Season") + #ggtitle("Shell length 0mm to 100mm") +
        ylab(bquote('ARM Density (no. '*~m^-2*')')) +
        scale_x_discrete(labels = season_labels, drop = F)+
        theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
        # coord_cartesian(ylim = c(0, 30)) +
        stat_summary(geom = "line", position = position_dodge(0.2), fun.data = my.stderr.reef, size = 0.5, aes(linetype = string)) + #fun.y=mean, linetype="dashed")+
        stat_summary(geom = "point", position = position_dodge(0.2), fun.data = my.stderr.reef) +
        stat_summary(geom = "errorbar", position = position_dodge(0.2), fun.data = my.stderr.reef, width = 0.125, size = 0.5)+
        theme(legend.position = c(0.5, 0.92), 
              legend.title = element_blank(),
              legend.background = element_blank())+
        guides(col = guide_legend(nrow = 2, byrow = TRUE))

print(arm.abcount.plot.combined)

setwd('R:/TAFI/TAFI_MRL_Sections/Marine Environment/Section Shared/2018 RVA method validation/Abalone plate resurvey')
ggsave(
        filename = paste('Reef Interactions_AbaloneResurvey_ARMDensityCombined_Au2016_Au2019', '.pdf', sep = ''),
        plot = arm.abcount.plot.combined,
        width = 200,
        height = 150,
        units = 'mm'
)

ggsave(
        filename = paste('Reef Interactions_AbaloneResurvey_ARMDensityCombined_Au2016_Au2019', '.png', sep = ''),
        plot = arm.abcount.plot.combined,
        width = 200,
        height = 150,
        units = 'mm'
)
##--------------------------------------------------------------------------------------##
## box plot of abalone counts for individual ARMs

filter(arm.abcounts.df, site == 'BRS', !is.na(plate)) %>%
 group_by(string, yr.season, plate) %>%
 summarise(cnts = sum(ab_n)) %>%
 mutate(stringdex = paste0(as.character(string),'_',as.character(plate))) %>%
 transform(stringdex=reorder(stringdex, cnts) ) %>%
 ggplot(aes(x=stringdex, y= cnts)) +
 geom_boxplot() + 
 theme(axis.text.x = element_text(angle = 90, hjust = 1))+
 ylab('Abalone Abundance (count/ARM)')+
 xlab('ARM (string_ARM number)')+
 # ggtitle(mysite)+
 theme(plot.title = element_text(hjust = 0.5))
##--------------------------------------------------------------------------------------##
## Analysis: ANOVA ####

## prepare data by adding size class variable to combined abalone and reef interactions ARM desnity data

## add column to identify likely settlers (<25 mm) and possible migrants (>25 mm)
arm.sl.df.2 <- arm.sl.df
arm.sl.df.2$size_class <- ifelse(arm.sl.df$ab_sl <= 25, 1, 2)

## create unique ID/index for each ARM and survdate combination
arm.sl.df.2$survindex <- as.factor(paste(arm.sl.df.2$site, 
                                         arm.sl.df.2$yr.season, 
                                         arm.sl.df.2$survdate, 
                                         arm.sl.df.2$string, 
                                         arm.sl.df.2$plate, sep="_"))

## subset and count number of animals per ARM by survindex
arm.dat <- filter(arm.sl.df.2, !is.na(ab_sl))  %>%
        group_by(size_class, survindex) %>%
        summarise(ab_n=n()) %>%  #as.data.frame()
        complete(survindex, fill = list(ab_n = 0)) %>%
        as.data.frame()

## calculate abalone per square metre 
platearea <- pi*(0.2^2)
arm.dat$absm <- arm.dat$ab_n * (1/platearea)

## unpack survindex variables and create new dataframe
arm.abcounts <- data.frame(separate(arm.dat, survindex, sep = "_", 
                                    into = c("site", "yr.season", "survdate", "string", "plate"), 
                                    convert = TRUE), arm.dat$survindex, arm.dat$ab_n, arm.dat$absm)


arm.abcounts.sizeclass.df <- arm.abcounts %>% 
        select(-c(arm.dat.ab_n, arm.dat.absm)) %>% 
        rename(survindex = arm.dat.survindex)

## save a copy of the R files
saveRDS(arm.abcounts.sizeclass.df, 'R:/TAFI/TAFI_MRL_Sections/Marine Environment/Section Shared/2018 RVA method validation/Abalone plate resurvey/arm.abcounts.sizeclass.df.RDS')

## load data
arm.abcounts.sizeclass.df <- readRDS('R:/TAFI/TAFI_MRL_Sections/Marine Environment/Section Shared/2018 RVA method validation/Abalone plate resurvey/arm.abcounts.sizeclass.df.RDS')

# For the alpha() function (transparency)
arm.abcounts.sizeclass.df$site <- as.factor(arm.abcounts.sizeclass.df$site)
arm.abcounts.sizeclass.df$survdate <- as.Date(arm.abcounts.sizeclass.df$survdate)
arm.abcounts.sizeclass.df$survtime <- as.numeric(with(arm.abcounts.sizeclass.df, survdate - min(survdate)))
library(scales)
# Set colours, transparent
palette(alpha(c("blue","red","forestgreen","darkorange"), 0.5))
with(arm.abcounts.sizeclass.df[sample(nrow(arm.abcounts.sizeclass.df)),],
     plot(jitter(survtime,3), absm, col=site, pch=19,
          xlab="Year Season", ylab="Abalone (no. m-2)"))
legend("topleft", levels(arm.abcounts.sizeclass.df$site), pch=19, col=palette())

##--------------------------------------------------------------------------------------##
## Analysis: MDD ####
# https://www.r-bloggers.com/power-analysis-and-sample-size-calculation-for-agriculture/
# 
## power analysis and sample size
## 



# dat1 <- droplevels(filter(arm.abcounts.df, site==mysite & yr.season == "2016.Summer" & string == mystring)) %>%
#  as.data.frame()
# 
# dat2 <- droplevels(filter(arm.abcounts.df, site==mysite & yr.season == "2017.Summer" & string == mystring)) %>%
#  as.data.frame()

# dat.ef <- droplevels(filter(abcounts, site==mysite & yr.season %in% c("2016.Summer", "2017.Summer") & string ==1)) %>%
#  as.data.frame()

# select site and string 
mysite <- 'LIP'
mystring <- 2

dat1 <- arm.abcounts.df %>% 
        filter(site == mysite &
                       yr.season == '2018.Autumn' &
                       string == mystring)

dat2 <- arm.abcounts.df %>% 
        filter(site == mysite &
                       yr.season == '2019.Autumn' &
                       string == mystring)

dat.ef <- arm.abcounts.df %>% 
        filter(site == mysite &
                       string == mystring &
                       yr.season %in% c('2018.Autumn', '2019.Autumn')) %>% 
        mutate(yr.season = factor(yr.season, levels = c('2019.Autumn', '2018.Autumn'))) %>% 
        as_data_frame()

group_by(dat.ef, yr.season) %>%
 summarise(mnabs = mean(absm),
           sdabs = sd(absm),
           n=n())


## Using the effsize package

t.test(dat1$absm, dat2$absm)
cohen.d(dat2$absm, dat1$absm, pooled=TRUE, conf.level=0.95)

# dat.ef$yr.season <- as.factor(dat.ef$yr.season)

coh.d <- cohen.d(dat.ef$absm ~ dat.ef$yr.season, pooled=TRUE, conf.level=0.95)
coh.d

tidy(coh.d$conf.int)




## http://genomicsclass.github.io/book/pages/power_calculations.html
m1 <- mean(dat1$absm) 
m2 <- mean(dat2$absm)
round(c(m1, m2),2)

diff <- m1 - m2 
diff


t.test(dat1$absm, dat2$absm)
t.test(dat1$absm, dat2$absm)$conf.int / mean(dat2$absm) * 100
N <- 20

# Cohens D
sd(dat1$absm)
sd_pool <- sqrt(((N-1)*var(dat1$absm) + (N-1)*var(dat2$absm))/(2*N - 2))
sd_pool
diff / sd_pool

## https://www.math.wustl.edu/~victor/classes/ma322/r-eg-06.txt
# Find the minimum detectable difference with 50 samples, same distribution
# parameters, alpha=0.05 and power=0.95:
mdd <- power.t.test(n=N, delta=NULL, sd=sd_pool, sig.level=0.05, power=0.95, type="two.sample", alternative = "two.sided")
mdd

## https://stats.idre.ucla.edu/r/dae/power-analysis-for-two-group-independent-sample-t-test/
deltaseq <- data.frame(iter = seq(2.0, 5.0, 0.1))

test <- deltaseq %>% group_by(iter) %>%
 do(pwrt = power.t.test(n=NULL, delta = .$iter, sd=sd_pool, sig.level=0.05, power=0.95, type="two.sample", alternative = "two.sided")) %>%
 tidy(pwrt)

plot.text <- paste0("MDD  = ",round(mdd$delta,2),", pooled sd = ",round(sd_pool,2))
## with Cohen's D if thats what we want
#plot.text <- paste0("MDD  = ",round(mdd$delta,2),", pooled sd = ",round(sd_pool,2),"; Cohen's D  = ",round(coh.d$estimate,2))

ggplot(test) + geom_point(aes(x=delta, y=n))  +
 annotate("text", -Inf, Inf, label= plot.text, hjust = -1.25, vjust =3, size = 5) +
 theme_bw() +
 theme(text = element_text(size=16))

## plot power and sample size for given effect
pwrt<-power.t.test(d=3,n= seq(50,1000,100),sd=sd_pool,sig.level=0.05,type="two.sample",alternative="two.sided")
plot(pwrt$n,pwrt$power,type="b",xlab="sample size",ylab="power")

http://sphweb.bumc.bu.edu/otlt/MPH-Modules/BS/R/R-Manual/R-Manual17.html
http://online.sfsu.edu/efc/classes/biol458/labs/lab5/Lab-5-R-version.pdf
https://link.springer.com/content/pdf/10.1007%2Fs11356-014-3398-2.pdf

# ## data and factor
# cohen.d(d,f)
# ## formula interface
# cohen.d(d ~ f)
# ## compute Hedges'g
# cohen.d(d,f,hedges.correction=TRUE)

## looks to be a good option
library(powerAnalysis)
## mean, sd, n -> d
ES.t.two(m1=13.5,m2=5.5,sd1=4.1833,sd2=3.02765,n1=14,n2=10)
## mean se, n -> d
ES.t.two(m1=13.5,m2=5.5,se1=1.118034,se2=0.9574271,n1=14,n2=10)
## t and n -> d
ES.t.two(n1=14,n2=10,t=5.4349)


## t, df and n -> d
ES.t.two(t = 3.123, df = 37.759,n1=20,n2=20)


##using library pwr

cohen.ES(test = "t",)



pwr.r.test(r=cohen.ES(test="r",size="medium")$effect.size,
           power=0.90, sig.level=0.05, alternative="two.sided")

group_by(dat.ef, yr.season) %>% 
 summarise(t.means = mean(absm), sd=sd(absm)) %>%
 as.data.frame()


library(bootES)

treats <- sprintf(unique(as.character(dat.ef$yr.season)))
paste(unique(as.character(dat.ef$yr.season)), collapse="," )
bootES(
 dat.ef,
 data.col = "absm",
 group.col = "yr.season",
 contrast = c("2017.Autumn", "2019.Autumn"),
 effect.type = "hedges.g")



## also see
### https://statistics.berkeley.edu/computing/r-t-tests

###


cohen.d(dat1$absm,dat2$absm)

#library(pwr)
pwr.t.test(power =0.9, n=20, sig.level=.05,type="two.sample",alternative="two.sided")

## https://www.math.wustl.edu/~victor/classes/ma322/r-eg-06.txt
## 

# Find the minimum sample size, if the desired power level is 95%
power.t.test(n=NULL,delta=2, sd=sd_pool, sig.level=0.05, power=0.95)

# Find the minimum detectable difference with 50 samples, same distribution
# parameters, alpha=0.05 and power=0.95:
power.t.test(n=20, delta=NULL, sd=sd_pool, sig.level=0.05, power=0.95)








