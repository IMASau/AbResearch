## clear environment
#rm(list=ls(all=TRUE))

## load library packages
library(tidyverse)
library(broom)
library(lubridate)
library(GGally)
library(scales)
library(gdata)
library(effsize)
library(openxlsx)
library(Hmisc)
library(ggsci)

## load season recode function
source("C:/GitCode/AbResearch/getSeason.r")

## load raw data from Excel spreadsheet (Note: data is entered for each site in different sheets 
## and needs to be compiled into a single sheet before import)
juv <- read.xlsx(
 "C:/CloudStor/Shared/Fisheries/Research/Abalone/AbResearchData/pop/ResearchSurveys_May2019_JM.xlsx",
 sheet = "ARM",
 detectDates = TRUE)

## Data cleaning ####

## convert var names to lower case
colnames(juv) <- tolower(colnames(juv))
juv <- dplyr::rename(juv, survdate = date)
juv <- dplyr::rename(juv, site = location)
juv$string <- as.factor(juv$string)
juv$plate <- as.factor(juv$plate)

## fix site names for several records from GIII on 2017-03-22 which have clearly been filled down in sequence 
## in the raw data.
juv <- juv %>%
 mutate(site = gsub('G4', 'GIII', site),
        site = gsub('G5', 'GIII', site),
        site = gsub('G6', 'GIII', site),
        site = gsub('G7', 'GIII', site),
        site = gsub('G3', 'GIII', site))

## remove characters from site names. 
juv$site <- gsub( "_", "", juv$site)

## check site names.
unique(juv$site)
unique(juv$ab_sl)

## checking for outliers
## most animals should be < 140 mm with the majority < 80 mm
juv %>% 
 mutate(ab_sl = as.numeric(as.character(ab_sl))) %>%
 filter(is.nan(ab_sl) | !is.na(ab_sl)) %>%
 ggplot() +
 geom_histogram(mapping = aes(x = ab_sl), binwidth = 5)

juv %>% 
 mutate(ab_sl = as.numeric(as.character(ab_sl))) %>%
 filter(is.nan(ab_sl) | !is.na(ab_sl)) %>%
 count(cut_width(ab_sl, 5))

juv %>% 
 mutate(ab_sl = as.numeric(as.character(ab_sl))) %>%
 filter(is.nan(ab_sl) | !is.na(ab_sl)) %>%
 ggplot(aes(x = site, y = ab_sl)) +
 geom_boxplot()

### Prepare dataframes for length frequency and abundance analyses ####

## A. Extract records with abs for length frequency analysis ####
juv.sl <- juv %>% 
 mutate(ab_sl = as.numeric(as.character(ab_sl))) %>%
 filter(is.nan(ab_sl) | !is.na(ab_sl))

## construct  date, quarter and season variables
#juv.sl$q <- quarter(juv.sl$survdate, with_year = TRUE)
juv.sl$sampyear <- as.factor(year(juv.sl$survdate)) 
juv.sl$season <- getSeason(juv.sl$survdate) 

## recode autumn samples as summer
juv.sl$season <- gsub( "Autumn", "Summer", juv.sl$season)
juv.sl$season <- as.factor(juv.sl$season)
juv.sl$season <- ordered(juv.sl$season, levels=c("Summer","Winter","Spring"))

## extract year.season and arrange in order (i.e. summer, winter, spring)
juv.sl$yr.season <- interaction(juv.sl$sampyear,juv.sl$season)
levels(juv.sl$yr.season)
juv.sl$yr.season <-
ordered(juv.sl$yr.season, levels = c("2015.Summer", "2015.Winter", "2015.Spring", 
                                     "2016.Summer", "2016.Winter", "2016.Spring", 
                                     "2017.Summer", "2017.Winter", "2017.Spring", 
                                     "2018.Summer", "2018.Winter", "2018.Spring",
                                     "2019.Summer", "2019.Winter", "2019.Spring"))

## recode Gardens 2015.summer samples as 2015.spring
pick <- which(juv.sl$site == "TG")
juv.sl$yr.season[pick] <- gsub( "2015.Summer", "2015.Spring", juv.sl$yr.season[pick])
juv.sl$yr.season <- droplevels(juv.sl$yr.season)

unique(juv.sl$survdate)
unique(juv.sl$yr.season)

## subset data to include only seasonal routine ARM sampling sites (i.e. BI, BRB, BRS, GIII, SP, TG)
juv.sl.seasonal <- subset(juv.sl, site %in% c("BI","BRB","BRS","GIII", "SP", "TG"))

## subset data into ARM sampling sites
list_juv.sl.site <- split(juv.sl, juv.sl$site)
#list2env(list_bigabs.sl.site, envir = .GlobalEnv) #splits list into each site but not well labelled
names(list_juv.sl.site)
juv.sl.sites <- c("juv.sl.BI",   "juv.sl.BRB",    "juv.sl.BRS",   "juv.sl.GIII",
                     "juv.sl.OS", "juv.sl.SB", "juv.sl.SP", "juv.sl.TG")
for (i in 1:length(list_juv.sl.site)) {
 assign(juv.sl.sites[i], list_juv.sl.site[[i]])
}

saveRDS(list_juv.sl.site, 'C:/CloudStor/R_Stuff/FIS/list_juv.sl.site.RDS')
saveRDS(juv.sl, 'C:/CloudStor/R_Stuff/FIS/juv.sl.RDS')

## B. Extract and prepare records with abs for abundance analyses ####

#juv$ab_sl <- NAToUnknown(x = juv$ab_sl, unknown = 0)
## NOTE:  sites were surveyed on different days, and not always entirely in the one season


# In the report the platearea or planar area of reef covered by the collector = 0.503 0.126 m2.
# However, this is incorrect as it refers to a plate with a diameter of 800 mm. 
# It looks like 0.4 m was used in the equation pi*r2 rather than 0.2 m. Therefore the correct plate area is 0.126 m2. 
platearea <- 0.126

## create unique ID/index for each ARM and survdate combination
juv$survindex <- as.factor(paste(juv$site, juv$survdate, juv$string, juv$plate, sep="_"))

## subset and count number of animals per ARM by survdate (subset by size class if required)
dat <- juv %>% 
 mutate(ab_sl = as.numeric(as.character(ab_sl))) %>%
 filter(is.nan(ab_sl) | !is.na(ab_sl)) %>%
#dat <- filter(juv, ab_sl >=25 & ab_sl < 100) %>%
#dat <- filter(juv, ab_sl <25 ) %>% 
#dat <- filter(juv, ab_sl <= 100) %>%
 group_by(survindex) %>%
 summarise(ab_n=n()) %>%
 complete(survindex, fill = list(ab_n = 0)) %>%
 as.data.frame()

## calculate abs per square metre 
dat$absm <- dat$ab_n * (1/platearea)

## unpack survindex variables and create new dataframe
abcounts <- data.frame(separate(dat, survindex, sep = "_", into = c("site", "survdate", "string","plate"), convert = TRUE), dat$survindex, dat$ab_n, dat$absm)

## format date variable and add year/season variables
abcounts$survdate <- as.Date(strptime(abcounts$survdate, "%Y-%m-%d"))
abcounts$sampyear <- as.factor(year(abcounts$survdate)) 
abcounts$season <- getSeason(abcounts$survdate) 

## recode autumn samples as summer
abcounts$season <- gsub( "Autumn", "Summer", abcounts$season)
abcounts$season <- as.factor(abcounts$season)
abcounts$season <- ordered(abcounts$season, levels=c("Summer","Winter","Spring"))

## create variable identifying year and season
abcounts$yr.season <- interaction(abcounts$sampyear,abcounts$season)
abcounts$yr.season <-
 ordered(abcounts$yr.season, levels = c("2015.Summer", "2015.Winter", "2015.Spring", 
                                        "2016.Summer", "2016.Winter", "2016.Spring", 
                                        "2017.Summer", "2017.Winter", "2017.Spring", 
                                        "2018.Summer", "2018.Winter", "2018.Spring",
                                        "2019.Summer", "2019.Winter", "2019.Spring"))
## recode Gardens 2015.summer samples as 2015.spring
pick <- which(abcounts$site == "TG")
abcounts$yr.season[pick] <- gsub( "2015.Summer", "2015.Spring", abcounts$yr.season[pick])
abcounts$yr.season <- droplevels(abcounts$yr.season)

saveRDS(abcounts, 'C:/CloudStor/R_Stuff/ARMs/abcounts.RDS')

## Size frequency plots ####

## create short label names for plot facets  
levels(juv.sl$yr.season)
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
                   "2019.Summer" = '2019.Su')

## subset data to include only seasonal routine sampling sites (i.e. BI, BRB, BRS, GIII, SP, TG)
juv.sl.seasonal <- subset(juv.sl, site %nin% c('OS', 'SB'))

## length frequency distribution plot of year.season x site
ggplot(juv.sl.seasonal, aes(x=ab_sl, color=site)) +
 ylab("Frequency") +
 xlab("Shell Length (mm)")+
 theme_bw()+
 geom_histogram(binwidth = 10) +
 theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
 theme(legend.position="none") +
 facet_grid(site ~ yr.season)
 #facet_grid(site ~ yr.season, scales = "free_y")

## length frequency density plot of year.season x site
ggplot(juv.sl.seasonal, aes(x=ab_sl, color=site)) +
 ylab("Frequency") +
 xlab("Shell Length (mm)")+
 geom_histogram(aes(y=..density..), alpha=.2, binwidth = 10)+
 geom_density(alpha = .2) +
 theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
 theme(legend.position="none") +
  facet_grid(site ~ yr.season)

## length frequency distribution plot of site x year.season
ggplot(juv.sl.seasonal, aes(x=ab_sl, color=site)) + 
 ylab("Frequency") +
 xlab("Shell Length (mm)")+
 theme_bw()+
 geom_histogram(binwidth = 10) +
 theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
 theme(legend.position="none") +
 facet_grid(yr.season ~ site, labeller = labeller(yr.season = season_labels))
 #facet_grid(yr.season ~ site, scales = "free_y")

## length frequency density plot of year.season x site
ggplot(juv.sl.seasonal, aes(x=ab_sl, color=site)) + 
 ylab("Frequency") +
 xlab("Shell Length (mm)")+
 geom_histogram(aes(y=..density..), alpha=.2, binwidth = 10)+
 geom_density(alpha = .2) +
 theme_bw()+
 theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
 theme(legend.position="none") +
 facet_grid(yr.season ~ site, labeller = labeller(yr.season = season_labels))

## Abundance plots and summaries ####

# **NOTE: the following plots by default are for ALL shell lengths**
#         To plot data for select shell lengths extract records for the 'dat' 
#         dataframe and generate the 'abcounts' dataframes (see above code).

## subset data to include only seasonal routine sampling sites (i.e. BI, BRB, BRS, GIII, SP, TG)
abcounts.seasonal <- subset(abcounts, site %nin% c('OS', 'SB'))

## create summary table of abalone per squre meter
ab_n.summary <- abcounts.seasonal %>%
 group_by(site, string, sampyear, survdate, season) %>%
 summarise(absm_mean = mean(absm),
           ab_string = sum(ab_n),
           absm_se = sd(absm)/sqrt(ab_string),
           ab_ARM = mean(ab_n),
           ab_ARM_se = sd(ab_n)/sqrt(ab_string))

## mean abalone abundance by year x season for each site
ggplot(abcounts.seasonal, aes(y=absm, x=yr.season))+
 geom_bar(stat = 'summary', fun.y = 'mean')+
 theme_bw()+
 theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
 facet_grid(. ~ site)+
 xlab('Year.Season')+
 ylab(bquote('Abalone Abundance ('*~m^2*')'))

## boxplot showing year x season abundance
ggplot(abcounts.seasonal, aes(y=absm, x=site))+
 geom_boxplot(outlier.colour = "orange", outlier.size = 1.5)+
 theme_bw()+
 facet_grid(season ~ sampyear)+
 theme(axis.text.x = element_text(angle = 90, hjust = 1))+
 xlab('Site')+
 ylab(bquote('Abalone Abundance ('*~m^2*')'))

## line plot showing mean abalone abundance (year x site x string)
ggplot(abcounts.seasonal, aes(y=absm, x=sampyear, group=season))+
 aes(colour = season)+
 scale_colour_brewer(palette = 'Set1')+
 theme_bw()+
 facet_grid(site ~ string, scales = "free_y" )+
 theme(axis.text.x = element_text(angle = 0, hjust = 0.5))+
 stat_summary(geom="line", position=position_dodge(0.2), fun.data=my.stderr, size=1) + #fun.y=mean, linetype="dashed")+
 stat_summary(geom="point", position=position_dodge(0.2), fun.data=my.stderr) +
 stat_summary(geom="errorbar", position=position_dodge(0.2), fun.data=my.stderr, width = 0.125, size = 1) +
 xlab('Year')+
 ylab(bquote('Abalone Abundance ('*~m^2*')'))+
 theme(plot.title = element_text(hjust = 0.5))+
 theme(legend.title = element_blank())+
 theme(legend.position = 'top')

## frequency distribution plot of n/ARM (site x year.season)
ggplot(abcounts.seasonal, aes(x = ab_n, fill = site, color = site)) + 
 ylab("Frequency") +
 xlab("N")+
 geom_histogram(alpha = 0.2, binwidth = 1)+
 theme_bw()+
 facet_grid(yr.season ~ site, scales = 'free_y', labeller = labeller(yr.season = season_labels))+
 theme(legend.position = 'none')

## juvenile abundance/m2 plot of year.season x site x string)
abcounts.seasonal$string <- factor(as.integer(abcounts.seasonal$string), levels = c(1,2))
plot.colours <- c("#0072B2", "#D55E00")
ggplot(abcounts.seasonal, aes(x=yr.season, y=absm, group = string)) + 
 aes(colour = string) + 
 scale_colour_manual(values = plot.colours)+
 theme_bw() +
 xlab("Year.Season") + 
 #ggtitle("Shell length 0mm to 100mm") +
 ylab(bquote('Abalone Abundance ('*~m^2*')')) +
 theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
 # coord_cartesian(ylim = c(0, 15)) +
 stat_summary(geom="line", position=position_dodge(0.2), fun.data=my.stderr, size=1) +
 stat_summary(geom="point", position=position_dodge(0.2), fun.data=my.stderr) +
 stat_summary(geom="errorbar", position=position_dodge(0.2), fun.data=my.stderr, width = 0.125, size = 1) +
 facet_grid(site ~ ., scales = "free_y" )+
 labs(col = 'String')

## juvenile abundance/m2 plot of site x year.season
abcounts.seasonal$string <- factor(as.integer(abcounts.seasonal$string), levels = c(1,2))
plot.colours <- c("#0072B2", "#D55E00")
ggplot(abcounts.seasonal, aes(x=site, y=absm, group = string)) +
 scale_colour_manual(values = plot.colours)+
 aes(colour = string) +  theme_bw() +
 xlab("Year.Season") + 
 #ggtitle("Shell length 0mm to 100mm") +
 ylab(bquote('Abalone Abundance ('*~m^2*')')) +
 theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
 # coord_cartesian(ylim = c(0, 15)) +
 #stat_summary(geom="line", position=position_dodge(0.2), fun.data=my.stderr, size=1) + #fun.y=mean, linetype="dashed")+
 stat_summary(geom="point", position=position_dodge(0.2), fun.data=my.stderr) +
 stat_summary(geom="errorbar", position=position_dodge(0.2), fun.data=my.stderr, width = 0.125, size = 1) +
 facet_grid(yr.season ~ ., scales = "free_y", labeller = labeller(yr.season = season_labels))+
 labs(col = 'String')

## juvenile n/ARM plot of year.season x site
plot.colours <- c("#0072B2", "#D55E00")
ggplot(abcounts.seasonal, aes(x=yr.season, y=ab_n, group = string)) +
 scale_colour_manual(values = plot.colours)+
 aes(colour = string) +  
 theme_bw() +
 xlab("Season") + 
 #ggtitle("Shell length 0mm to 100mm") +
 theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
 # coord_cartesian(ylim = c(0, 15)) +
 stat_summary(geom="line", position=position_dodge(0.2), fun.data=my.stderr, size=1) + #fun.y=mean, linetype="dashed")+
 stat_summary(geom="point", position=position_dodge(0.2), fun.data=my.stderr) +
 stat_summary(geom="errorbar", position=position_dodge(0.2), fun.data=my.stderr, width = 0.125, size = 1) +
 facet_grid(site ~ ., scales = "free_y", labeller = labeller(yr.season = season_labels))+
 labs(col = 'String', y = expression(Abalone~x~ARM^{-1}))

## Habitat complexity figures for abalone abundance/m2 ####

# add variable for site habitat complexity
pick1 <- which(abcounts$site %in% c("TG", "SP", "BRB"))
abcounts$habitat[pick1] <- "high"

pick2 <- which(abcounts$site %in% c("BI", "GIII"))
abcounts$habitat[pick2] <- "medium"

pick3 <- which(abcounts$site %in% c("BRS"))
abcounts$habitat[pick3] <- "low"

mydataset <- droplevels(subset(abcounts, yr.season=="2015.Spring"))

mydataset.seasonal <- abcounts %>% filter(site %nin% c('OS', 'SB'))

mydataset.seasonal$habitat <- as.factor(mydataset.seasonal$habitat)
mydataset.seasonal$habitat <-
 ordered(mydataset.seasonal$habitat, levels = c("low","medium", "high"))

mydataset.seasonal$site <- factor(mydataset.seasonal$site, levels = c("BRS", "GIII", "BRB", "TG", "BI", "SP"))

mydataset.seasonal$string <- factor(as.integer(mydataset.seasonal$string), levels = c(1,2))

plot.colours <- c("#0072B2", "#D55E00")    
ggplot(mydataset.seasonal, aes(x=site, y=absm, group = as.factor(string))) + 
 aes(colour = string) + 
 scale_colour_manual(values = plot.colours)+
 theme_bw() +
 xlab("Site") + #ggtitle("Shell length 0mm to 100mm") +
 ylab(bquote('Abalone Abundance ('*~m^2*')')) +
 theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
 # coord_cartesian(ylim = c(0, 15)) +
 #stat_summary(geom="line", position=position_dodge(0.2), fun.data=my.stderr, size=1) + #fun.y=mean, linetype="dashed")+
 stat_summary(geom="point", position=position_dodge(0.2), fun.data=my.stderr, size = 2) +
 stat_summary(geom="errorbar", position=position_dodge(0.2), fun.data=my.stderr, width = 0.25, size = 1) +
 facet_grid( ~ habitat , scales = "free_x")+
 labs(col = 'String')

##------------------------------------------------------##

#~~~~~~~~~~~~~~~~~~~~~~~~~BY SITE ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# see values shown in ?stat_summary

stderr <- function(x) {
 sqrt(var(x[!is.na(x)]) / length(x[!is.na(x)]))
}

my.stderr <- function(x) {
 meany <- mean(x)
 ymin  <- mean(x) - stderr(x)
 ymax  <- mean(x) + stderr(x)
 # assemble the named output
 out <- c(y = meany, ymin = ymin, ymax = ymax)
 return(out)
}


unique(abcounts$site)

subdat <- filter(abcounts, site =='BI')
subdat$string <- as.factor(subdat$string)

ggplot(subdat, aes(y=ab_n, x=season, fill=string)) +
  ggtitle("Betsey Island")+
  xlab("Sample date") + 
  ylab("Blacklip Abalone Abundance") +
  geom_bar(stat="identity")+
  scale_fill_grey(start = 0.3, end = 0.7)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust=0.2), 
        text = element_text(size=16),
        legend.position=c(.95,.88)) +
 facet_grid(sampyear ~.)

#  +scale_x_discrete(limits=c("27 Jun", "28 Jul", "18 Aug", "25 Sep", "08 Oct", "09 Oct"))

Pick <- abcounts
GIII <- droplevels(subset(Pick, Pick$site=="GIII"))

ggplot(GIII, aes(y=ab_n, x=survdate, fill=string)) +
  ggtitle("George 3rd Rock")+
  xlab("Sample date") + 
  ylab("Blacklip Abalone Abundance") +
  geom_bar(stat="identity")+
  ylim(0,50)+
  scale_fill_grey(start = 0.3, end = 0.7)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust=0.2), 
        text = element_text(size=16),
        legend.position=c(.95,.88))

# scale_x_discrete(limits=c("11 Aug", "21 Sep", "13 Oct"))

BR_B<-droplevels(subset(Pick, Pick$site=="BRB"))

ggplot(BR_B, aes(y=ab_n, x=survdate, fill=string)) +
  ggtitle("Black Reef Boulder")+
  xlab("Sample date") + 
  ylab("Blacklip Abalone Abundance") +
  geom_bar(stat="identity")+
  ylim(0,50)+
  scale_fill_grey(start = 0.3, end = 0.7)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust=0.2), 
        text = element_text(size=16),
        legend.position=c(.95,.88))+
  scale_x_discrete(limits=c("12 Aug", "30 Sep", "13 Oct"))



BR_S<-droplevels(subset(Pick, Pick$Site=="BR_S"))

ggplot(BR_S, aes(y=Ab_Sum, x=survdate, fill=string)) +
  ggtitle("Black Reef Slab")+
  xlab("Sample date") + 
  ylab("Blacklip Abalone Abundance") +
  geom_bar(stat="identity")+
  ylim(0,50)+
  scale_fill_grey(start = 0.3, end = 0.7)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust=0.2), 
        text = element_text(size=16),
        legend.position=c(.1,.87))+
  scale_x_discrete(limits=c("24 Jul", "11 Aug", "30 Sep", "13 Oct"))

SP<-droplevels(subset(Pick, Pick$Site=="SP"))

ggplot(SP, aes(y=Ab_Sum, x=survdate, fill=string)) +
  ggtitle("Seymour Point")+
  xlab("Sample date") + 
  ylab("Blacklip Abalone Abundance") +
  geom_bar(stat="identity")+
  ylim(0,50)+
  scale_fill_grey(start = 0.3, end = 0.7)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust=0.2), 
        text = element_text(size=16),
        legend.position=c(.1,.87))+
  scale_x_discrete(limits=c("29 Jul", "20 Aug", "09 Sep"))

TG<-droplevels(subset(Pick, Pick$Site=="TG"))

ggplot(TG, aes(y=Ab_Sum, x=survdate, fill=string)) +
  ggtitle("The Gardens")+
  xlab("Sample date") + 
  ylab("Blacklip Abalone Abundance") +
  geom_bar(stat="identity")+
  ylim(0,50)+
  scale_fill_grey(start = 0.3, end = 0.7)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust=0.2), 
        text = element_text(size=16),
        legend.position=c(.1,.87))+
  scale_x_discrete(limits=c("29 Jul", "20 Aug", "09 Sep"))

## Length frequency plots by site ####

TG.sl <- droplevels(subset(juv.sl, site=="TG"))


ggplot(TG.sl, aes(x=ab_sl, color=site)) + 
 ylab("Frequency") +
 xlab("Shell Length (mm)")+
 geom_histogram(alpha = 0.2, binwidth = 5)+
 #ggtitle(paste(dum$SubBlockNo, FishYear))+
 #labs(title= Yeardum$SubBlockNo, size=10)+
 #geom_histogram(binwidth=50)+
 theme_bw()+
 facet_grid(yr.season ~ .)


BI.sl <- droplevels(subset(juv.sl, site=="BI"))

ggplot(BI.sl, aes(x=ab_sl, color=site)) + 
 ylab("Frequency") +
 xlab("Shell Length (mm)")+
 geom_histogram(alpha = 0.2, binwidth = 5)+
 #ggtitle(paste(dum$SubBlockNo, FishYear))+
 #labs(title= Yeardum$SubBlockNo, size=10)+
 #geom_histogram(binwidth=50)+
 theme_bw()+
 facet_grid(sampyear ~ season)


BRS.sl <- droplevels(subset(juv.sl, site=="BRS"))

ggplot(BRS.sl, aes(x=ab_sl, color=site)) + 
 ylab("Frequency") +
 xlab("Shell Length (mm)")+
 geom_histogram(alpha = 0.5, binwidth = 10, fill = "red", col=I("black"))+
 #ggtitle(paste(dum$SubBlockNo, FishYear))+
 #labs(title= Yeardum$SubBlockNo, size=10)+
 #geom_histogram(binwidth=50)+
 theme_bw()+
 facet_grid(sampyear ~ season)

BRB.sl <- droplevels(subset(juv.sl, site=="BRB"))

ggplot(BRB.sl, aes(x=ab_sl, color=site)) + 
 ylab("Frequency") +
 xlab("Shell Length (mm)")+
 geom_histogram(alpha = 0.5, binwidth = 10, fill = "red", col=I("black"))+
 #ggtitle(paste(dum$SubBlockNo, FishYear))+
 #labs(title= Yeardum$SubBlockNo, size=10)+
 #geom_histogram(binwidth=50)+
 theme_bw()+
 facet_grid(sampyear ~ season)


GIII.sl <- droplevels(subset(juv.sl, site=="G3"))

ggplot(GIII.sl, aes(x=ab_sl)) + 
 ylab("Frequency") +
 xlab("Shell Length (mm)")+
 geom_histogram(alpha = 0.5, binwidth = 10, fill = "red", col=I("black"))+
 #ggtitle(paste(dum$SubBlockNo, FishYear))+
 #labs(title= Yeardum$SubBlockNo, size=10)+
 #geom_histogram(binwidth=50)+
 theme_bw()+
 facet_grid(sampyear ~ season)

SP.sl <- droplevels(subset(juv.sl, site=="SP"))

ggplot(SP.sl, aes(x=ab_sl, color=site)) + 
 ylab("Frequency") +
 xlab("Shell Length (mm)")+
 geom_histogram(alpha = 0.2, binwidth = 5)+
 #ggtitle(paste(dum$SubBlockNo, FishYear))+
 #labs(title= Yeardum$SubBlockNo, size=10)+
 #geom_histogram(binwidth=50)+
 theme_bw()+
 facet_grid(sampyear ~ season)

## plots of abalone counts per ARM ####

unique(abcounts$site)
subset(abcounts, !is.na(plate))

mysite <- "SP"
plotdat <- filter(abcounts, site==mysite & !is.na(plate)) %>%
 mutate(string = factor(string)) %>%
  group_by(string, yr.season, plate) %>%
 summarise(cnts = sum(ab_n)) %>%
 spread(yr.season, cnts)

pairs(plotdat[3:10],panel=panel.smooth,main = paste0("Site: ",mysite))

ggpairs(plotdat, columns = 3:12,  aes(colour = string)) +
 theme(axis.text.x = element_text(angle = 90, hjust = 1),
       legend.position = "right") +
 xlab(bquote('Abalone Abundance (count/plate)')) +
 ylab(bquote('Abalone Abundance (count/plate)'))


plotdat2 <- filter(abcounts, site==mysite & !is.na(plate)) %>%
 mutate(stringdex = paste0(string,'_',plate)) %>%
 group_by(stringdex) %>%
 summarise(cnts = sum(ab_n)) 
 
#hist(plotdat2$cnts, breaks = 20)

## box plot of abalone counts for individual ARMs

filter(abcounts, site==mysite, !is.na(plate)) %>%
 group_by(string, yr.season, plate) %>%
 summarise(cnts = sum(ab_n)) %>%
 mutate(stringdex = paste0(as.character(string),'_',as.character(plate))) %>%
  transform(stringdex=reorder(stringdex, cnts) ) %>%
 ggplot(aes(x=stringdex, y= cnts)) +
 geom_boxplot() + 
 theme(axis.text.x = element_text(angle = 90, hjust = 1))+
 ylab('Abalone Abundance (count/ARM)')+
 xlab('ARM (string_ARM number)')+
 ggtitle(mysite)+
 theme(plot.title = element_text(hjust = 0.5))
 


##-----------------------------------------------------------------##
## MDD ####
# https://www.r-bloggers.com/power-analysis-and-sample-size-calculation-for-agriculture/
# 
## power analysis and sample size
## 

mysite <- "BRB"
dat1 <- droplevels(filter(abcounts, site==mysite & yr.season == "2016.Summer" & string ==1)) %>%
  as.data.frame()

dat2 <- droplevels(filter(abcounts, site==mysite & yr.season == "2017.Summer" & string ==1)) %>%
  as.data.frame()

dat.ef <- droplevels(filter(abcounts, site==mysite & yr.season %in% c("2016.Summer", "2017.Summer") & string ==1)) %>%
 as.data.frame()

group_by(dat.ef, yr.season) %>%
 summarise(mnabs = mean(absm),
           sdabs = sd(absm),
           n=n())


## Using the effsize package
t.test(dat1$absm, dat2$absm)
cohen.d(dat2$absm, dat1$absm, pooled=TRUE, conf.level=0.95)
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
 contrast = c("2016.Winter", "2016.Spring"),
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








## Boulder Rolling ####

boulders <- read.xlsx(
 "D:/OneDrive - University of Tasmania/Fisheries Research/Abalone/AbResearchData/pop/ResearchSurveys.xlsx",
 sheet = "boulders",
 detectDates = TRUE)


## A. Extract records with abs for length frequency analysis ----
boulder.sl <- filter(boulders, ab_sl > 0 & sampleperiod == 2)

filter(boulders, ab_sl > 0 & sampleperiod == 2) %>%
 ggplot(aes(x=interaction(patch, site ), y=ab_sl)) +
 geom_boxplot() +
 theme_bw() +
 theme(text = element_text(size=16)) + 
 scale_y_continuous(breaks = seq(0,160, 20)) +
 ylab("Shell length") +
 xlab("Site")

filter(boulders, ab_sl > 0 & sampleperiod == 2) %>%
 group_by(site, patch) %>%
 summarise(ab_n =n(), med = median(ab_sl)) %>%  #as.data.frame()
 complete(site, patch, fill = list(ab_n = 0)) %>%
 as.data.frame()

filter(juv.sl, yr.season == "2015.Spring") %>%
 group_by(site) %>%
 summarise(ab_n =n(), med = median(ab_sl)) %>%  #as.data.frame()
 complete(site, fill = list(ab_n = 0)) %>%
 as.data.frame()


bld.dat <- filter(boulders, ab_sl > 0 & sampleperiod == 2) %>%
 group_by(site, patch, quadrat) %>%
 summarise(ab_n =n()) %>%  #as.data.frame()
 complete(site, patch, quadrat, fill = list(ab_n = 0)) %>%
 as.data.frame()


ggplot(bld.dat, aes(y=ab_n, x=interaction(site, patch))) +
 geom_bar(stat="identity")+
 theme_bw()+
 theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
#  facet_grid(. ~ site)
#  
#  

