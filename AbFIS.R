## clear environment
#rm(list=ls(all=TRUE))

## load library packages
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

## load custom functions
source("C:/GitCode/AbResearch/getSeason.r")
source("C:/GitCode/AbResearch/errorUpper2.r")
source("C:/GitCode/AbResearch/stderr.r")

## source original LEGs data in seperate Excel sheets and combine

# load excel workbook containing data in seperate sheets
xl_data <- 'R:/TAFI/TAFI_MRL_Sections/Abalone/Section Shared/Abalone_databases/Data/Data for Transfer/2018/Ab_pop_bio_Lenght_density_2016.xlsx'

# identify sheets in excel workbook
tab_names <- excel_sheets(path = xl_data)

# create list from seperate sheets
list_all <- lapply(tab_names, function(x) read_excel(path = xl_data, sheet = x))

# create dataframe from seperate sheets
legs.df <- rbindlist(list_all, fill = T)

# ## source data for LEGs 
# bigabs <- read.xlsx("C:/CloudStor/Shared/Fisheries/Research/Abalone/AbResearchData/pop/ResearchSurveys_May2019_JM.xlsx",
#                     sheet = "FIS",
#                     detectDates = TRUE)

## Data cleaning ####

## convert varible names to lower case and compile data for estimates and comments columns, removing additional
#  columns from the Excel import (i.e. each sheet contained different column names for these variables)

# colnames(bigabs) <- tolower(colnames(bigabs))
# bigabs <- dplyr::rename(bigabs, survdate = date)
# bigabs <- dplyr::rename(bigabs, sllength = length)
# bigabs$string <- as.factor(bigabs$string)
# #bigabs$transect <- as.factor(bigabs$transect)

colnames(legs.df) <- tolower(colnames(legs.df))
names(legs.df) <- gsub('/', '', names(legs.df), fixed = T)
names(legs.df) <- gsub(' ', '', names(legs.df), fixed = T)
names(legs.df) <- gsub('=', '', names(legs.df), fixed = T)
names(legs.df) <- gsub('comments', 'comments.1', names(legs.df), fixed = T)
names(legs.df) <- gsub('...8', 'comments.2', names(legs.df), fixed = T)
names(legs.df) <- gsub('...9', 'comments.3', names(legs.df), fixed = T)
names(legs.df) <- gsub('...10', 'comments.4', names(legs.df), fixed = T)
names(legs.df) <- gsub('eestimate', 'comments.5', names(legs.df), fixed = T)
legs.df <- dplyr::rename(legs.df, survdate = date)
legs.df <- dplyr::rename(legs.df, sllength = length)
legs.df$string <- as.factor(legs.df$string)


bigabs <- legs.df %>%
  select(-comments.3) %>%
  unite('all_comments', 'comments.1','comments.2', 'comments.4', 'comments.5', sep = ',') %>%
  mutate(all_comments = gsub('NA', '', all_comments),
         all_comments = gsub(',', '', all_comments),
         all_comments = gsub('^$', NA, all_comments)) %>%
  mutate(estimate.2 = estimate) %>%
  mutate(estimate = if_else(is.na(all_comments) & estimate.2 == 'E', estimate.2, 
                              if_else(all_comments == 'E', all_comments, NA_character_))) %>%
  mutate(comments = if_else(is.na(estimate), all_comments, NA_character_)) %>%
  select(-c(estimate.2, all_comments)) %>%
  as.data.frame()


#bigabs$site <- recode(bigabs$site, BR_S = "BRS", BR_B = "BRB", .default = bigabs$site)
#bigabs$site <- recode(bigabs$site, GIII = "G3", .default = bigabs$site)

## remove data with no site name or shell length
bigabs <- filter(bigabs, !is.na(site))
bigabs <- filter(bigabs, !is.na(sllength))

## remove characters from site names and rename sites to a three letter acronym
unique(bigabs$site)
bigabs$site <- gsub(' ', '', bigabs$site)
bigabs$site <- gsub('_', '', bigabs$site)
bigabs$site <- gsub('Telopea', 'TEL', bigabs$site)
bigabs$site <- gsub('SP', 'SEY', bigabs$site)
bigabs$site <- gsub('\\bT\\b', 'THU', bigabs$site)
bigabs$site <- gsub('BI', 'BET', bigabs$site)
bigabs$site <- gsub('TG', 'GAR', bigabs$site)
bigabs$site <- gsub('GIII', 'GEO', bigabs$site)
bigabs$site <- gsub('MB', 'MUN', bigabs$site)
bigabs$site <- gsub('MP', 'INN', bigabs$site)
bigabs$site <- gsub('LB', 'LOU', bigabs$site)
bigabs$site <- gsub('OB', 'OUT', bigabs$site)


## rename string names from earlier sampling periods
table(bigabs$site, bigabs$string)
bigabs$string  <- gsub( "Kar", "1", bigabs$string )
bigabs$string  <- gsub( "Juv", "2", bigabs$string )
bigabs$string  <- gsub( "N", "1", bigabs$string )
bigabs$string  <- gsub( "S", "2", bigabs$string )
bigabs$string  <- gsub( "North", "1", bigabs$string )
bigabs$string  <- gsub( "South", "2", bigabs$string )

## rename east and west transect directions
unique(bigabs$eastwest)
bigabs$eastwest <- gsub('w', 'W', bigabs$eastwest)
bigabs$eastwest <- gsub('N', 'E', bigabs$eastwest)
bigabs$eastwest <- gsub('S', 'W', bigabs$eastwest)
bigabs$eastwest <- gsub('L', 'W', bigabs$eastwest)
bigabs$eastwest <- gsub('R', 'E', bigabs$eastwest)

## inspect data for outliers
filter(bigabs, !is.na(sllength)) %>%
ggplot() +
 geom_histogram(mapping = aes(x = sllength), binwidth = 5)

filter(bigabs, !is.na(sllength)) %>%
 count(cut_width(sllength, 5))

bigabs.2 <- subset(bigabs, is.na(estimate))
summary(bigabs$sllength)

filter(bigabs, !is.na(sllength)) %>%
 ggplot(aes(x=site, y=sllength)) +
geom_boxplot()

filter(bigabs, !is.na(sllength), is.na(estimate)) %>%
 ggplot(aes(x=site, y=sllength)) +
 geom_boxplot()

## add unique identifier for each measurement
bigabs$survindex <- as.factor(paste(bigabs$site, bigabs$survdate, bigabs$string, bigabs$transect, sep="_"))

### Prepare dataframes for size frequency and abundance analyses ####

## A. Extract abundance data ####

## Filter for all abalone
bigabdat <- bigabs %>%
 group_by(survindex) %>%
 summarise(ab_n = n()) %>%  #as.data.frame()
 complete(survindex, fill = list(ab_n = 0)) %>%
 as.data.frame()

## Filter for sub-legal abalone
# bigabdat.sub <- bigabs %>% filter(sllength <= 137) %>%
# # bigabdat <- bigabs %>%
#   group_by(survindex) %>%
#   summarise(ab_n = n()) %>%  #as.data.frame()
#   complete(survindex, fill = list(ab_n = 0)) %>%
#   as.data.frame()

bigabdat.sub <- bigabs %>% 
 filter(sllength <= 137) %>%
 # bigabdat <- bigabs %>%
 group_by(survindex) %>%
 summarise(ab_n = n()) %>%  #as.data.frame()
 complete(survindex, fill = list(ab_n = 0)) %>%
 as.data.frame()

## Filter for legal abalone
# bigabdat.leg <- bigabs %>% filter(sllength >= 138) %>%
#   group_by(survindex) %>%
#   summarise(ab_n = n()) %>%  #as.data.frame()
#   complete(survindex, fill = list(ab_n = 0)) %>%
#   as.data.frame()

bigabdat.leg <- bigabs %>% 
 filter(sllength >= 138) %>%
 group_by(survindex) %>%
 summarise(ab_n = n()) %>%  #as.data.frame()
 complete(survindex, fill = list(ab_n = 0)) %>%
 as.data.frame()

## Filter for sub-legal abalone <10 mm
# bigabdat.sub.ten <- bigabs %>% filter(sllength >= 128 & sllength < 138) %>%
#  group_by(survindex) %>%
#  summarise(ab_n = n()) %>%  #as.data.frame()
#  complete(survindex, fill = list(ab_n = 0)) %>%
#  as.data.frame()

bigabdat.sub.ten <- bigabs %>% 
 filter(sllength >= 128 & sllength < 138) %>%
 group_by(survindex) %>%
 summarise(ab_n = n()) %>%  #as.data.frame()
 complete(survindex, fill = list(ab_n = 0)) %>%
 as.data.frame()

bigabdat.leg.ten <- bigabs %>% 
 filter(sllength >= 139 & sllength <= 148) %>%
 group_by(survindex) %>%
 summarise(ab_n = n()) %>%  #as.data.frame()
 complete(survindex, fill = list(ab_n = 0)) %>%
 as.data.frame()
 
## Filter for non-estimates abalone (for length frequency analysis)
# bigabdat.meas <- subset(bigabs, is.na(estimate)) %>%
#   group_by(survindex) %>%
#   summarise(ab_n = n()) %>%  #as.data.frame()
#   complete(survindex, fill = list(ab_n = 0)) %>%
#   as.data.frame()

## join dataframes created above for survindex
bigabdat.join <- left_join(bigabdat.sub, bigabdat.leg, by = 'survindex') %>%
 left_join(., bigabdat.sub.ten, by = 'survindex') %>%
 left_join(., bigabdat.leg.ten, by = 'survindex') %>%
 left_join(., bigabdat, by = 'survindex')

## rename variables to identify abcounts from each size class
names(bigabdat.join)
bigabdat.join <- dplyr::rename(bigabdat.join, ab_n_leg = ab_n.y)
bigabdat.join <- dplyr::rename(bigabdat.join, ab_n_sub = ab_n.x)
bigabdat.join <- dplyr::rename(bigabdat.join, ab_n_sub_ten = ab_n.x.x)
bigabdat.join <- dplyr::rename(bigabdat.join, ab_n_leg_ten = ab_n.y.y)

## rename dataframes created above for legal and sub-legal counts for coding to follow
## and rename dataframe at the end to the original filtered name
#bigabdat <- bigabdat.sub
#bigabdat <- bigabdat.leg
 
## calculate abs per square metre 
bigabdat$absm <- bigabdat$ab_n / 15

## calculate abs per square metre for joint dataframe
bigabdat.join$absm_sub <- bigabdat.join$ab_n_sub /15
bigabdat.join$absm_leg <- bigabdat.join$ab_n_leg /15
bigabdat.join$absm_sub_ten <- bigabdat.join$ab_n_sub_ten /15
bigabdat.join$absm_leg_ten <- bigabdat.join$ab_n_leg_ten /15
bigabdat.join$absm <- bigabdat.join$ab_n /15

## unpack survindex variables and create new dataframe
bigabcounts <- data.frame(separate(bigabdat, survindex, sep = "_", into = c("site", "survdate", "string","transect"), convert = TRUE), bigabdat$survindex, bigabdat$ab_n, bigabdat$absm)

## unpack survindex variables and create new dataframe for the joint dataframe
bigabcounts.join <- data.frame(separate(bigabdat.join, survindex, sep = "_",
                                   into = c("site", "survdate", "string","transect"), convert = TRUE), 
                          bigabdat.join$survindex, bigabdat.join$ab_n, bigabdat.join$absm, bigabdat.join$ab_n_sub, 
                          bigabdat.join$absm_sub, bigabdat.join$ab_n_leg, bigabdat.join$absm_leg,
                          bigabdat.join$ab_n_sub_ten, bigabdat.join$absm_sub_ten,
                          bigabdat.join$ab_n_leg_ten, bigabdat.join$absm_leg_ten)

## set string as a factor
bigabcounts$string <- as.factor(bigabcounts$string)

## construct  date, quarter and season variables
bigabcounts$survdate <- as.Date(strptime(bigabcounts$survdate, "%Y-%m-%d"))
bigabcounts$sampyear <- year(bigabcounts$survdate)
bigabcounts$season <- getSeason(bigabcounts$survdate)

bigabcounts.join$survdate <- as.Date(strptime(bigabcounts.join$survdate, "%Y-%m-%d"))
bigabcounts.join$sampyear <- year(bigabcounts.join$survdate)
bigabcounts.join$season <- getSeason(bigabcounts.join$survdate)

## recode autumn samples as summer
#table(bigabcounts$site,bigabcounts$season)
#table(bigabcounts$sampyear,bigabcounts$season)
bigabcounts$season <- gsub( "Autumn", "Summer", bigabcounts$season)

bigabcounts.join$season <- gsub( "Autumn", "Summer", bigabcounts.join$season)

## extract year.season and arrange in order (i.e. summer, winter, spring)
bigabcounts$season <- as.factor(bigabcounts$season)
bigabcounts$season <- ordered(bigabcounts$season, levels=c("Summer","Winter","Spring"))
bigabcounts$yr.season <- interaction(bigabcounts$sampyear,bigabcounts$season)
sort(unique(bigabcounts$yr.season))
bigabcounts$yr.season <-
 ordered(bigabcounts$yr.season, levels = c("2015.Summer", "2015.Winter", "2015.Spring", 
                                           "2016.Summer", "2016.Winter", "2016.Spring", 
                                           "2017.Summer", "2017.Winter", "2017.Spring", 
                                           "2018.Summer", "2018.Winter", "2018.Spring", 
                                           "2019.Summer", '2019.Winter'))

bigabcounts.join$season <- as.factor(bigabcounts.join$season)
bigabcounts.join$season <- ordered(bigabcounts.join$season, levels=c("Summer","Winter","Spring"))
bigabcounts.join$yr.season <- interaction(bigabcounts.join$sampyear,bigabcounts.join$season)
unique(bigabcounts.join$yr.season)
bigabcounts.join$yr.season <-
 ordered(bigabcounts.join$yr.season, levels = c("2015.Summer", "2015.Winter", "2015.Spring", 
                                                "2016.Summer", "2016.Winter", "2016.Spring", 
                                                "2017.Summer", "2017.Winter", "2017.Spring", 
                                                "2018.Summer", "2018.Winter", "2018.Spring", 
                                                "2019.Summer", '2019.Winter'))

## adjust misclassified seasons
pick <- which(bigabcounts$site == "GAR")
bigabcounts$yr.season[pick] <- gsub( "2015.Summer", "2015.Spring", bigabcounts$yr.season[pick])
bigabcounts$yr.season <- droplevels(bigabcounts$yr.season)

pick <- which(bigabcounts.join$site == "GAR")
bigabcounts.join$yr.season[pick] <- gsub( "2015.Summer", "2015.Spring", bigabcounts.join$yr.season[pick])
bigabcounts.join$yr.season <- droplevels(bigabcounts.join$yr.season)

## subset data to include only seasonal routine sampling sites (i.e. BI, BRB, BRS, GIII, SP, TG)
#bigabcounts.seasonal <- subset(bigabcounts, site %in% c("BI","BRB","BRS","GIII", "SP", "TG"))
#bigabcounts.join.seasonal <- subset(bigabcounts.join, site %in% c("BI","BRB","BRS","GIII", "SP", "TG"))

## rename dataframes for legal and sub-legal counts
#bigabcounts.sub.seasonal <- bigabcounts.seasonal
#bigabcounts.leg.seasonal <- bigabcounts.seasonal

## subset data to individual ARM sampling sites
list_bigabcounts.site <- split(bigabcounts.join, bigabcounts.join$site)
names(list_bigabcounts.site)
bigabcounts.sites <- c("bigabcounts.BET",   "bigabcounts.BRB",    "bigabcounts.BRS",   "bigabcounts.GEO",
                       "bigabcounts.LOU", "bigabcounts.MUN", "bigabcounts.INN", "bigabcounts.OUT", "bigabcounts.SEY",
                     "bigabcounts.THU", "bigabcounts.TEL",  "bigabcounts.GAR")
for (i in 1:length(list_bigabcounts.site)) {
 assign(bigabcounts.sites[i], list_bigabcounts.site[[i]])
}

## save a copy of the R files
saveRDS(list_bigabcounts.site, 'C:/CloudStor/R_Stuff/FIS/list_bigabcounts.site.RDS')
saveRDS(bigabcounts.join, 'C:/CloudStor/R_Stuff/FIS/bigabcounts.RDS')


## B. Extract size frequency data ####

# ## check for outliers
# summary(bigabs$sllength)
# hist(bigabs$sllength)
# subset(bigabs, sllength > 200)

bigabs.sl <- bigabs

## construct  date, quarter and season variables
#juv.sl$q <- quarter(juv.sl$survdate, with_year = TRUE)
bigabs.sl$sampyear <- year(bigabs.sl$survdate)
bigabs.sl$season <- getSeason(bigabs.sl$survdate)

## recode autumn samples as summer
bigabs.sl$season <- gsub( "Autumn", "Summer", bigabs.sl$season)

## extract year.season and arrange in order (i.e. summer, winter, spring)
bigabs.sl$season <- as.factor(bigabs.sl$season)
bigabs.sl$season <- ordered(bigabs.sl$season, levels=c("Summer","Winter","Spring"))
bigabs.sl$yr.season <- interaction(bigabs.sl$sampyear,bigabs.sl$season)
bigabs.sl$yr.season <-
 ordered(bigabs.sl$yr.season, levels = c("2015.Summer", "2015.Winter", "2015.Spring", 
                                         "2016.Summer", "2016.Winter", "2016.Spring", 
                                         "2017.Summer", "2017.Winter", "2017.Spring", 
                                         "2018.Summer", "2018.Winter", "2018.Spring", 
                                         "2019.Summer", '2019.Winter'))

## adjust misclassified seasons
pick <- which(bigabs.sl$site == "GAR")
bigabs.sl$yr.season[pick] <- gsub( "2015.Summer", "2015.Spring", bigabs.sl$yr.season[pick])
bigabs.sl$yr.season <- droplevels(bigabs.sl$yr.season)

## subset data to include only seasonal routine ARM sampling sites (i.e. BI, BRB, BRS, GIII, SP, TG)
bigabs.sl.seasonal <- subset(bigabs.sl, site %in% c("BET","BRB","BRS","GEO", "SEY", "GAR"))

## subset data into routine FIS sampling sites
list_bigabs.sl.site <- split(bigabs.sl, bigabs.sl$site)
#list2env(list_bigabs.sl.site, envir = .GlobalEnv) #splits list into each site but not well labelled
names(list_bigabs.sl.site)
bigabs.sl.sites <- c("bigabs.sl.BET",   "bigabs.sl.BRB",    "bigabs.sl.BRS",   "bigabs.sl.GEO",
                     "bigabs.sl.LOU", "bigabs.sl.MUN",  "bigabs.sl.INN",  "bigabs.sl.OUT",
                     "bigabs.sl.SEY", "bigabs.sl.THU", "bigabs.sl.TEL", "bigabs.sl.GAR")
for (i in 1:length(list_bigabs.sl.site)) {
 assign(bigabs.sl.sites[i], list_bigabs.sl.site[[i]])
}

saveRDS(list_bigabs.sl.site, 'C:/CloudStor/R_Stuff/FIS/list_bigabs.sl.site.RDS')
saveRDS(bigabs.sl, 'C:/CloudStor/R_Stuff/FIS/bigabs.sl.RDS')


#**************************************************************************************************#
## Abundance plots and summaries ####

## load most recent RDS file of FIS abcounts and shell length data
list_bigabcounts.site <- readRDS('C:/CloudStor/R_Stuff/FIS/list_bigabcounts.site.RDS')
list_bigabs.sl.site <- readRDS('C:/CloudStor/R_Stuff/FIS/list_bigabs.sl.site.RDS')
list_juv.sl.site <- readRDS('C:/CloudStor/R_Stuff/FIS/list_juv.sl.site.RDS')
list_abcounts <- readRDS('C:/CloudStor/R_Stuff/ARMs/abcounts.RDS')

## subset data into routine FIS sampling sites
names(list_bigabs.sl.site)
bigabs.sl.sites <- c("bigabs.sl.BET",   "bigabs.sl.BRB",    "bigabs.sl.BRS",   "bigabs.sl.GEO",
                     "bigabs.sl.LOU", "bigabs.sl.MUN",  "bigabs.sl.INN",  "bigabs.sl.OUT",
                     "bigabs.sl.SEY", "bigabs.sl.THU", "bigabs.sl.TEL", "bigabs.sl.GAR")
for (i in 1:length(list_bigabs.sl.site)) {
 assign(bigabs.sl.sites[i], list_bigabs.sl.site[[i]])
}

## subset data to individual ARM sampling sites
names(list_bigabcounts.site)
bigabcounts.sites <- c("bigabcounts.BET",   "bigabcounts.BRB",    "bigabcounts.BRS",   "bigabcounts.GEO",
                       "bigabcounts.LOU", "bigabcounts.MUN", "bigabcounts.INN", "bigabcounts.OUT", "bigabcounts.SEY",
                       "bigabcounts.THU", "bigabcounts.TEL",  "bigabcounts.GAR")
for (i in 1:length(list_bigabcounts.site)) {
 assign(bigabcounts.sites[i], list_bigabcounts.site[[i]])
}

## set the colour scheme for FIS strings so they contrast with ARM strings when plotting
fis.col <- c('#7CAE00', '#C77CFF')

## create short label names for plot facets 
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
                   "2019.Winter" = '2019.Win')

## adult abunance/m2 plot of year.season x site
ggplot(bigabcounts, aes(x=yr.season, y=absm, group = string)) + 
 aes(colour = string) +  theme_bw() +
 scale_color_manual(values = fis.col)+
 xlab("Year.Season") + #ggtitle("Abalone (>=138) observed during transect surveys") +
 ylab(bquote('Abalone Abundance ('*~m^2*')')) +
 theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
# coord_cartesian(ylim = c(0, 5)) +
 stat_summary(geom="line", position=position_dodge(0.2), fun.data=my.stderr, size=1) + #fun.y=mean, linetype="dashed")+
 stat_summary(geom="point", position=position_dodge(0.2), fun.data=my.stderr) +
 stat_summary(geom="errorbar", position=position_dodge(0.2), fun.data=my.stderr, width = 0.125, size = 1) +
 facet_grid(site ~ . , scales = "free")+
 labs(col = 'String')

## line plot of abaundance year x site x string x season
ggplot(bigabcounts, aes(y=absm, x=sampyear, group=season))+
 aes(colour = season)+scale_colour_brewer(palette = 'Set1')+
 theme_bw()+
 facet_grid(site ~ string, scales = "free_y" )+
 theme(axis.text.x = element_text(angle = 0, hjust = 0.5))+
 stat_summary(geom="line", position=position_dodge(0.2), fun.data=my.stderr, size=1, linetype = 'dashed') + #fun.y=mean, linetype="dashed")+
 stat_summary(geom="point", position=position_dodge(0.2), fun.data=my.stderr) +
 stat_summary(geom="errorbar", position=position_dodge(0.2), fun.data=my.stderr, width = 0.125, size = 1) +
 xlab('Year')+
 ylab(bquote('Abalone Abundance ('*~m^2*')'))+
 ggtitle('Fishery Independant Surveys (FIS)')+
 theme(plot.title = element_text(hjust = 0.5))


## stacked bar plot of abundance for legal and sub-legal abalone

unlist_bigabcounts <- bind_rows(list_bigabcounts.site, .id = 'column_label')

sub.leg <- melt(unlist_bigabcounts,id.vars=c('bigabdat.join.survindex', 'site', 'yr.season'), 
              measure.vars=c('absm_sub','absm_leg','absm', 'absm_sub_ten', 'absm_leg_ten'))

sub.leg.2 <- melt(unlist_bigabcounts,id.vars=c('bigabdat.join.survindex', 'site', 'yr.season'), 
                measure.vars=c('absm_sub','absm_leg'))

sub.leg.3 <- melt(unlist_bigabcounts,id.vars=c('bigabdat.join.survindex', 'site', 'yr.season'), 
                measure.vars=c('absm_sub_ten', 'absm_leg_ten'))
# select site
unique(sub.leg$site)
selected.site <- 'BI'

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

# sub.leg.dat$variable <-
#  ordered(sub.leg.dat$variable, levels = c("absm", "absm_leg", "absm_sub", "absm_sub_ten")) #re-order variables for plot
         
fis.abund.bar <- ggplot(sub.leg.dat, aes(x = yr.season, y = value, fill = variable))+
 stat_summary(geom = 'bar', position = 'identity', fun.data = my.stderr, colour = 'black')+
 stat_summary(fun.ymax = errorUpper2, fun.ymin = mean,
               geom = 'errorbar', position = 'identity', colour = 'black', width = 0.2)+
 stat_summary(fun.ymax = errorUpper2, fun.ymin = errorUpper2,
              geom = 'linerange', position = 'identity', colour = 'black')+
 scale_colour_manual()+
 theme_bw()+
 #xlab('Season')+
 xlab(NULL)+
 ylab(bquote('Abalone Abundance ('*~m^2*')'))+
 labs(fill = 'Size')+
 scale_fill_manual(values = sub.leg.colours,
  name = 'Size class', breaks = c('absm', 'absm_leg', 'absm_sub', "absm_sub_ten"),
                     labels = c(' All BL', ' Legal ', ' Sub-legal', " <10 mm legal"))+
 theme(legend.title = element_blank())+
 theme(axis.text.x = element_text(angle = 0, vjust = 0.5))+
 theme(legend.justification = c(0, 1), legend.position = c(0, 1), legend.direction = 'horizontal')+
 #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
 theme(legend.background = element_blank())+
 coord_cartesian(ylim = c(-2, 2))+
 annotate('text', x = c(1), y = 0.6, label = 'NO DATA', angle = 90)+ #enter manually but work on conditional statement
 scale_x_discrete(breaks = c("2015.Summer", "2015.Winter", "2015.Spring", "2016.Summer", "2016.Winter",
                  "2016.Spring", "2017.Summer", "2017.Winter",
                  "2017.Spring", "2018.Summer", "2018.Winter", "2018.Spring", "2019.Summer"), labels = season_labels, drop = F)

fis.abund.bar.2 <- ggplot(sub.leg.dat.2, aes(x = yr.season, y = value, fill = variable))+
 stat_summary(geom = 'bar', position = 'identity', fun.data = my.stderr, colour = 'black')+
 stat_summary(fun.ymax = errorUpper2, fun.ymin = mean,
              geom = 'errorbar', position = 'identity', colour = 'black', width = 0.2)+
 stat_summary(fun.ymax = errorUpper2, fun.ymin = errorUpper2,
              geom = 'linerange', position = 'identity', colour = 'black')+
 scale_colour_manual()+
 theme_bw()+
 xlab('Season')+
 ylab(bquote('Abalone Abundance ('*~m^2*')'))+
 labs(fill = 'Size')+
 scale_fill_manual(values = sub.leg.colours.2,
                   name = 'Size class', breaks = c("absm_sub_ten","absm_leg_ten"),
                   labels = c(" <10 mm legal ", " >10 mm legal"))+
 theme(legend.title = element_blank())+
 theme(axis.text.x = element_text(angle = 0, vjust = 0.5))+
 theme(legend.justification = c(0, 1), legend.position = c(0, 1), legend.direction = 'horizontal')+
 #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
 theme(legend.background = element_blank())+
 coord_cartesian(ylim = c(-0.5, 0.5))+
 annotate('text', x = c(1), y = 0.15, label = 'NO DATA', angle = 90)+ #enter manually but work on conditional statement
 scale_x_discrete(breaks = c("2015.Summer", "2015.Winter", "2015.Spring", "2016.Summer", "2016.Winter",
                             "2016.Spring", "2017.Summer", "2017.Winter",
                             "2017.Spring", "2018.Summer", "2018.Winter", "2018.Spring", "2019.Summer"), labels = season_labels, drop = F)

FIS_ABUND <- grid.arrange(
 arrangeGrob(cowplot::plot_grid(fis.abund.bar + rremove('x.text'), fis.abund.bar.2, align = 'v', ncol = 1),
             ncol = 1))


ggsave(filename = paste('FIS_ABUND_', selected.site, '.pdf', sep = ''), plot = FIS_ABUND)
ggsave(filename = paste('FIS_ABUND_', selected.site, '.wmf', sep = ''), plot = FIS_ABUND)

# dd <- sub.leg.dat %>%
#  group_by(variable, yr.season) %>%
#  summarise(
#   mean.x = mean(value),
#   sd.x = sd(value),
#   se.x = sd.x/sqrt(length(value)),
#   error.up1 = mean.x + se.x,
#   error.up2 = mean.x - se.x,
#   error.up3 = errorUpper(value),
#   error.up4 = errorUpper2(value))



## quick summary data of proportion measured that were legal-sized
dat.1 <- sub.leg.dat %>%
 filter(variable == 'absm') %>%
 group_by(yr.season, variable) %>%
 summarise(mean = mean(value))
dat.2 <- sub.leg.dat %>%
 filter(variable == 'absm_leg') %>%
 group_by(yr.season, variable) %>%
 summarise(mean = mean(value))
dat.3 <- sub.leg.dat %>%
 filter(variable == 'absm_sub_ten') %>%
 group_by(yr.season, variable) %>%
 summarise(mean = mean(value))

left_join(dat.1, dat.2, by = c('yr.season')) %>%
 left_join(dat.3, by = c('yr.season')) %>%
 summarise(prop.legal = (mean.y/mean.x)*100,
           prop.sublegal10 = abs((mean/mean.x)*100),
           prop.sublegal = 100 - prop.legal - prop.sublegal10)

## Size frequency plots ####

## adult size frequency plot of year.season x site
# option: replace bigabs.sl.seasonal with site dataframes (e.g. bigabs.sl.BRB)
ggplot(bigabs.sl.seasonal, aes(x=sllength, color=site, fill = site))+
 ylab("Frequency") +
 xlab("Shell Length (mm)")+
 geom_histogram(alpha = 0.2, binwidth = 5)+
 theme_bw()+
 theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
 #theme(strip.text.y = element_text(size = 5))+
 #theme(axis.text.y = element_text(size = 7))+
 #ggtitle(paste(dum$SubBlockNo, FishYear))+
 #labs(title= Yeardum$SubBlockNo, size=10)+
 #geom_histogram(binwidth=50)+
 theme(legend.position = 'none')+
 facet_grid(yr.season ~ site, labeller = labeller(yr.season = season_labels))+
 geom_vline(aes(xintercept = 138),colour = 'red', linetype = 'dashed', size = .5)

## adult size frequency density plot of year.season x site
ggplot(bigabs.sl.seasonal, aes(x=sllength, color=site)) + 
 ylab("Frequency") +
 xlab("Shell Length (mm)")+
 geom_histogram(aes(y=..density..), alpha = 0.2, binwidth = 10)+
 geom_density(alpha=.2) +
 theme_bw()+
 facet_grid(yr.season ~ site, labeller = labeller(yr.season = season_labels))

# ggplot(mydatsl, aes(x=sllength, color=site)) + 
#  ylab("Frequency") +
#  xlab("Shell Length (mm)")+
#  geom_histogram(alpha = 0.2, binwidth = 10)+
#  theme_bw()+
#  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#  #ggtitle(paste(dum$SubBlockNo, FishYear))+
#  #labs(title= Yeardum$SubBlockNo, size=10)+
#  #geom_histogram(binwidth=50)+
#  facet_grid(site ~ yr.season)
# 
# ggplot(mydatsl, aes(x=sllength, color=site)) + 
#  ylab("Frequency") +
#  xlab("Shell Length (mm)")+
#  geom_histogram(aes(y=..density..), alpha = 0.2, binwidth = 10)+
#  geom_density(alpha=.2) +
#  theme_bw()+
#  facet_grid(site ~ yr.season)

# ggplot(bigabs.sl.seasonal, aes(x=sllength)) + 
#  ylab("Frequency") +
#  xlab("Shell Length (mm)")+
#  geom_histogram(aes(y=..density..), alpha = 0.2, binwidth = 10)+
#  geom_density(alpha=.2) +
#  theme_bw()+
#  facet_grid( ~ yr.season)


## Site plots ####

## size frequency by site

# as of April 2019 there are 12 sampling periods, therfore rearrange yr.season to spread plots over
# two columns and in vertical order (i.e. 2 x 6 facet grid)
unique(bigabs.sl$yr.season)
plot.order <- c("2015.Summer", "2017.Summer", "2015.Winter", "2017.Winter", "2015.Spring", "2017.Spring", "2016.Summer",
  "2018.Summer", "2016.Winter", "2018.Winter",  "2016.Spring", "2018.Spring")

# generate a summary table for chosen site to add counts to plots (i.e. n = xxx)
plot.n <- bigabs.sl.BRB %>% 
 group_by(yr.season) %>%
 summarise(n = paste('n =', n()))

ggplot(transform(bigabs.sl.BRB, yr.season = factor(yr.season, levels = plot.order)), aes(x=sllength)) + 
 ylab("Frequency") +
 xlab("Shell Length (mm)")+
 geom_histogram(alpha = 0.5, binwidth = 5, fill = "blue3", col=I("black"))+
 #ggtitle(paste(dum$SubBlockNo, FishYear))+
 #labs(title= Yeardum$SubBlockNo, size=10)+
 #geom_histogram(binwidth=50)+
 theme_bw()+
 facet_wrap(. ~ yr.season, ncol = 2, drop = F)+
 geom_vline(aes(xintercept = 138),colour = 'red', linetype = 'dashed', size = .5)+
 #ggtitle('GIII FIS 2015-2018')+
 geom_text(data = plot.n, aes(x = 30, y = 15, label = n), colour = 'black', inherit.aes = F, parse = F, size = 3.5)

## abundance by site

# convert string to factor so that stings are plotted as two unique colours
bigabcounts.BRS$string <- factor(as.integer(bigabcounts.BRS$string), levels = c(1,2))

ggplot(bigabcounts.BRS, aes(x=yr.season, y=absm, group = string)) + 
 aes(colour = string) +  
 theme_bw() +
 xlab("Season") + 
 #ggtitle("Abalone observed during transect surveys") +
 ylab(bquote('MB Abalone Abundance ('*~m^2*')')) +
 #coord_cartesian(ylim = c(0, 2.5))
 stat_summary(geom="line", position=position_dodge(0.2), fun.data=my.stderr, size=1.5) +
 #stat_summary(geom="point", position=position_dodge(0.2), fun.data=my.stderr, size = 3) +
 #stat_summary(geom="errorbar", position=position_dodge(0.2), fun.data=my.stderr, width = 0.125, size = 1)+
 stat_summary(fun.y = mean, geom = 'line', group = 'string', size = 1, aes(colour = '1+2'))+
 stat_summary(fun.y = mean, group = 'string', geom = 'point', aes(colour = '1+2'), size = 3)+
 stat_summary(fun.data = my.stderr, aes(colour = '1+2'), group = 'string', geom = 'errorbar',  width = 0.125, size = 1)+
 theme(axis.text.x = element_text(angle = 0, vjust = 0.5))+
 labs(col = 'String')+
 scale_color_manual(values = c('red', 'black', 'blue'))+
 scale_x_discrete(labels = season_labels, drop = F)+
 theme(legend.position = c(0.95, 0.9), legend.direction = 'vertical')
 #geom_smooth(aes(group = 1), size = 2, method = 'lm')

## abundance by season and year by site
ggplot(bigabcounts.MB, aes(x = sampyear, y=absm, group = interaction(sampyear, season))) + 
 aes(fill = season) +  
 theme_bw() +
 xlab("Year") +
 ylab(bquote('Abalone Abundance ('*~m^2*')')) +
 geom_boxplot(alpha = 0.6, position = position_dodge(0.85, preserve = 'single'), outlier.shape = NA)+
 scale_fill_grey()+
 theme(legend.title = element_blank())+
 theme(axis.text.x = element_text(angle = 0, vjust = 0.5))+
 theme(legend.justification = c(0, 1), legend.position = c(0, 1), legend.direction = 'vertical')+
 theme(legend.background = element_blank())+
 ylim(0, 0.5)
 #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

## ARM.FIS plots ####

## inverted size frequency plot with ARM and FIS data

# combine individual sites from R data file list into a single dataframe
# unlist_bigabs.sl <- bind_rows(list_bigabs.sl.site, .id = 'column_label')
# unlist_bigabs.sl$sampyear <- as.factor(unlist_bigabs.sl$sampyear)
# 
# juv.sl <- bind_rows(list_juv.sl.site, .id = 'column_label')
# juv.sl$sampyear <- as.factor(juv.sl$sampyear)

## load most recent juvenile and adult data sets
bigabs.sl <- readRDS('C:/CloudStor/R_Stuff/FIS/bigabs.sl.RDS')
juv.sl <- readRDS('C:/CloudStor/R_Stuff/FIS/juv.sl.RDS')

## convert sampyear to factor
juv.sl$sampyear <- as.factor(juv.sl$sampyear)
bigabs.sl$sampyear <- as.factor(bigabs.sl$sampyear)

# add column to identify FIS and ARM data
bigabs.sl$sampmethod <- 'FIS'
juv.sl$sampmethod <- 'ARM'

# join FIS and ARM data
fisarm <- bind_rows(bigabs.sl, juv.sl)

# subset chosen site
unique(fisarm$site)
selected.site <- 'BRS'
fisarm.site <- subset(fisarm, site %in% selected.site)

# re-order data so that facet plots in vertical order of two columns
fisarm.site$yr.season <- factor(fisarm.site$yr.season, levels = c("2015.Summer", "2017.Summer", "2015.Winter", "2017.Winter", "2015.Spring", "2017.Spring", "2016.Summer",
                                                                  "2018.Summer", "2016.Winter", "2018.Winter",  "2016.Spring", "2018.Spring", "2019.Summer"))

# generate a summary table for chosen site to add counts to plots (i.e. n = xxx)
plot.n.FIS <- fisarm.site %>% 
 filter(sampmethod == 'FIS') %>%
 group_by(yr.season) %>%
 summarise(n = paste('n =', n()))

plot.n.ARM <- fisarm.site %>% 
 filter(sampmethod == 'ARM') %>%
 group_by(yr.season) %>%
 summarise(n = paste('n =', n()))

# generate dataframe to annotate 'no data' for missing seasons
ann_text <- data.frame(x = 90, y = 40, 
                       lab = 'NO DATA', 
                       yr.season = c("2015.Summer", '2015.Winter', '2018.Spring'))

ARM_FIS <- ggplot(data = fisarm.site)+
 geom_histogram(aes(x = sllength, y = ..count..), binwidth = 10, fill = 'blue')+
 geom_histogram(aes(x = ab_sl, y = -..count..), binwidth = 10, fill = 'red')+
 facet_wrap(. ~ yr.season, ncol = 2, drop = F)+
 theme_bw()+
 ylab("Frequency") +
 xlab("Shell Length (mm)")+
 coord_cartesian(ylim = c(-40, 115), xlim = c(0, 180))+
 geom_hline(yintercept = 0, size = 0.1)+
 geom_text(data = ann_text, aes(x = x, y = y, label = lab))+
 geom_text(data = plot.n.FIS, aes(x = 160, y = 50, label = n), 
           colour = 'black', inherit.aes = F, parse = F, size = 3.5)+
 geom_text(data = plot.n.ARM, aes(x = 10, y = -30, label = n), 
           colour = 'black', inherit.aes = F, parse = F, size = 3.5)+
 geom_vline(aes(xintercept = 138),colour = 'red', linetype = 'dashed', size = 0.5)

print(ARM_FIS)

#setwd('C:/CloudStor/R_Stuff/FIS')
ggsave(filename = paste('ARM_FIS_LF_', selected.site, '.pdf', sep = ''), plot = ARM_FIS, units = 'mm', width = 190, height = 250)
ggsave(filename = paste('ARM_FIS_LF_', selected.site, '.wmf', sep = ''), plot = ARM_FIS, units = 'mm', width = 190, height = 250)



## abundance plot with ARM and FIS data

# # combine individual sites from R data file list into a single dataframe
# unlist_bigabcounts <- bind_rows(list_bigabcounts.site, .id = 'column_label')
# unlist_bigabcounts$sampyear <- as.factor(unlist_bigabcounts$sampyear)
# 

## load most recent juvenile and adult data sets
bigabcounts <- readRDS('C:/CloudStor/R_Stuff/FIS/bigabcounts.RDS')
abcounts <- readRDS('C:/CloudStor/R_Stuff/ARMs/abcounts.RDS')

## add column to identify FIS and ARM data
bigabcounts$sampmethod <- 'FIS'
abcounts$sampmethod <- 'ARM'

## convert sampyear to factor from bigabcounts df
bigabcounts$sampyear <- as.factor(bigabcounts$sampyear)

# join FIS and ARM data
fisarm.abund <- bind_rows(bigabcounts, abcounts)

# subset chosen site
unique(fisarm.abund$site)
selected.site <- 'GIII'
fisarm.site.abund <- subset(fisarm.abund, site %in% selected.site)

# re-order data so that facet plots in vertical order of two columns

fisarm.site.abund$string <- factor(as.integer(fisarm.site.abund$string), levels = c(1,2))
fisarm.site.abund$yr.season <-
 ordered(fisarm.site.abund$yr.season, levels = c("2015.Summer", "2015.Winter", "2015.Spring", 
                                                 "2016.Summer", "2016.Winter", "2016.Spring", 
                                                 "2017.Summer", "2017.Winter", "2017.Spring", 
                                                 "2018.Summer", "2018.Winter", "2018.Spring",
                                                 "2019.Summer"))

# ARM_FIS_ABUND <- ggplot(fisarm.site.abund, aes(x=yr.season, y=absm, group = interaction(sampmethod, string))) + 
#  aes(colour = string) +  
#  theme_bw() +
#  xlab("Season") + 
#  ylab(bquote('Abalone Abundance ('*~m^2*')')) +
#  stat_summary(geom="line", position=position_dodge(0.2), fun.data=my.stderr, size=1, aes(linetype = sampmethod)) +
#  stat_summary(geom="point", position=position_dodge(0.2), fun.data=my.stderr, size = 3) +
#  stat_summary(geom="errorbar", position=position_dodge(0.2), fun.data=my.stderr, width = 0.125, size = 0.5)+
#  #stat_summary(fun.y = mean, geom = 'line', group = 'string', size = 1, aes(colour = '1+2'))+
#  #stat_summary(fun.y = mean, group = 'string', geom = 'point', aes(colour = '1+2'), size = 3)+
#  #stat_summary(fun.data = my.stderr, aes(colour = '1+2'), group = 'string', geom = 'errorbar',  width = 0.125, size = 1)+
#  theme(axis.text.x = element_text(angle = 0, vjust = 0.5))+
#  labs(col = 'String')+
#  scale_color_manual(values = c('black', 'darkblue'))+
#  scale_x_discrete(labels = season_labels, drop = F)+
#  guides(linetype=F)+
#  theme(legend.position = c(0.1, 0.9), legend.direction = 'vertical')+
#  coord_cartesian(ylim = c(0, 60))+
#  geom_line()
# print(ARM_FIS_ABUND)

# attempt to plot abundance on same plot with second y-axis
fis.summ <- fisarm.site.abund %>%
 filter(sampmethod == 'FIS') %>%
 group_by(string, yr.season) %>%
 summarise(fis_mean = mean(absm),
           fis_n = n(),
           fis_se = sd(absm)/sqrt(fis_n))

arm.summ <- fisarm.site.abund %>%
 filter(sampmethod == 'ARM') %>%
 group_by(string, yr.season) %>%
 summarise(arm_mean = mean(absm),
           arm_n = n(),
           arm_se = sd(absm)/sqrt(arm_n))

# ARM_FIS_ABUND <- ggplot()+
#  geom_line(data = arm.summ, aes(x = yr.season, y = arm_mean/10, group = factor(string), linetype = string), position = position_dodge(0.5), colour = 'red')+
#  geom_point(data = arm.summ, aes(x = yr.season, y = arm_mean/10, group = factor(string), colour = string), size = 3, position = position_dodge(0.5), colour = 'red')+
#  geom_errorbar(data = arm.summ, aes(x = yr.season, 
#                                     ymin = arm_mean/10 - arm_se/10, ymax = arm_mean/10 + arm_se/10, group = factor(string), colour = string), position = position_dodge(0.5), width = 0.1, colour = 'red')+
#  geom_line(data = fis.summ, aes(x = yr.season, y = fis_mean, group = factor(string), linetype = string), position = position_dodge(1), colour = 'blue')+
#  geom_point(data = fis.summ, aes(x = yr.season, y = fis_mean, group = factor(string), colour = string), size = 3, position = position_dodge(1), colour = 'blue')+
#  geom_errorbar(data = fis.summ, aes(x = yr.season, 
#                                     ymin = fis_mean - fis_se, ymax = fis_mean + fis_se, group = factor(string), colour = string), width = 0.1, position = position_dodge(1), colour = 'blue')+
#  scale_y_continuous(sec.axis = sec_axis(~.*10, name = bquote('ARM Abalone Abundance ('*~m^2*')')))+
#  ylab(bquote('FIS Abalone Abundance ('*~m^2*')'))+
#  scale_x_discrete(labels = season_labels, drop = F)+
#  scale_color_manual(values = c('blue', 'red'))+
#  theme_bw()+
#  #theme(legend.position = c(0.1, 0.9), legend.direction = 'vertical')+
#  theme(legend.position = 'none')+
#  labs(col = 'String')+
#  xlab("Season")+
#  coord_cartesian(ylim = c(0, 6))

arm_abund <- ggplot()+
 geom_line(data = arm.summ, aes(x = yr.season, y = arm_mean, group = factor(string), linetype = string), position = position_dodge(0.5), colour = 'red')+
 geom_point(data = arm.summ, aes(x = yr.season, y = arm_mean, group = factor(string), colour = string), size = 3, position = position_dodge(0.5), colour = 'red')+
 geom_errorbar(data = arm.summ, aes(x = yr.season, 
                                    ymin = arm_mean - arm_se, ymax = arm_mean + arm_se, group = factor(string), colour = string), position = position_dodge(0.5), width = 0.1, colour = 'red')+
 ylab(bquote('ARM Abalone Abundance ('*~m^2*')'))+
 scale_x_discrete(labels = season_labels, drop = F)+
 scale_color_manual(values = c('red'))+
 theme_bw()+
 #theme(legend.position = c(0.1, 0.9), legend.direction = 'vertical')+
 theme(legend.position = 'none')+
 labs(col = 'String')+
 #xlab("Season")+
 xlab(NULL)+
 coord_cartesian(ylim = c(0, 60))

fis_abund <- ggplot()+
 geom_line(data = fis.summ, aes(x = yr.season, y = fis_mean, group = factor(string), linetype = string), position = position_dodge(0.5), colour = 'blue')+
 geom_point(data = fis.summ, aes(x = yr.season, y = fis_mean, group = factor(string), colour = string), size = 3, position = position_dodge(0.5), colour = 'blue')+
 geom_errorbar(data = fis.summ, aes(x = yr.season, 
                                    ymin = fis_mean - fis_se, ymax = fis_mean + fis_se, group = factor(string), colour = string), position = position_dodge(0.5), width = 0.1, colour = 'blue')+
 ylab(bquote('FIS Abalone Abundance ('*~m^2*')'))+
 #scale_y_continuous(position = 'right')+
 # theme(axis.title.y = element_blank(),
 #       axis.text.y = element_blank(),
 #       axis.ticks.y = element_blank())+
 #ylab(NULL)+
 #theme_minimal()+
 #theme(axis.text.y = element_blank())+
 #scale_y_continuous(sec.axis = sec_axis(~.*1, name = bquote('FIS Abalone Abundance ('*~m^2*')')))+
 scale_x_discrete(labels = season_labels, drop = F)+
 scale_color_manual(values = c('blue'))+
 theme_bw()+
 #theme(legend.position = c(0.1, 0.9), legend.direction = 'vertical')+
 theme(legend.position = 'none')+
 labs(col = 'String')+
 xlab("Season")+
 coord_cartesian(ylim = c(0, 3))


# print(arm_abund)
# print(fis_abund)
# print(ARM_FIS_ABUND)


ARM_FIS_ABUND <- grid.arrange(
 arrangeGrob(cowplot::plot_grid(arm_abund + rremove('x.text'), fis_abund, align = 'v', ncol = 1),
             ncol = 1))
 
#setwd('C:/CloudStor/R_Stuff/FIS')
ggsave(filename = paste('ARM_FIS_ABUNDANCE_', selected.site, '.pdf', sep = ''), plot = ARM_FIS_ABUND)
ggsave(filename = paste('ARM_FIS_ABUNDANCE_', selected.site, '.wmf', sep = ''), plot = ARM_FIS_ABUND)

#add width and height to change y axis scale and stretch out


################################ old stuff


dat <- temp %>% group_by(String, Date, Transect) %>%
 summarise(count = n()) %>% 
 complete(String, Date, Transect, fill = list(count = 0)) %>% data.frame()

dat$abs <- dat$count/15


datmns <- dat %>%
 group_by(Date, String) %>%
 summarise(N = n(), mnabs=mean(abs), sd=sd(abs)) %>% data.frame()


datmns$se <- datmns$sd / sqrt(datmns$N)  # Calculate standard error of the mean

# Confidence interval multiplier for standard error
# Calculate t-statistic for confidence interval: 
# e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
ciMult <- qt(.95/2 + .5, datmns$N-1)
datmns$ci <- datmns$se * ciMult


datmns$String <- as.factor(datmns$String)


pd <- position_dodge(0.1) # move them .05 to the left and right

ggplot(datmns, aes(x=Date, y=mnabs, colour=String)) + 
 geom_errorbar(aes(ymin=mnabs-ci, ymax=mnabs+ci), width=5, position=pd) +
 geom_line(position=pd) +
 geom_point(position=pd, size=2)


datmns$Date <- as.factor(datmns$Date)

# Error bars represent standard error of the mean
ggplot(datmns, aes(x=Date, y=mnabs, fill=String)) + 
 geom_bar(position=position_dodge(), stat="identity") +
 geom_errorbar(aes(ymin=mnabs-se, ymax=mnabs+se),
               width=.2,                    # Width of the error bars
               position=position_dodge(.9))


# Use 95% confidence intervals instead of SEM
ggplot(datmns, aes(x=Date, y=mnabs, fill=String)) + 
 geom_bar(position=position_dodge(), stat="identity") +
 geom_errorbar(aes(ymin=mnabs-ci, ymax=mnabs+ci),
               width=.2,                    # Width of the error bars
               position=position_dodge(.9))

131722 * 5 / 10000


bm <- 0.07*10000*.6
bm
bm*.15

149/9/0.15


# Actaeons
mins <- 135855
mins * 2/10000
mins * 5/10000

ha <- 831


# Betsey 

264/1.7/.15



