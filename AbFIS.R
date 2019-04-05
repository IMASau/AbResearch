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

## load function to generate season for observations
source("C:/GitCode/AbResearch/getSeason.r")
source("C:/GitCode/AbResearch/errorUpper.r")

## source data for FIS 
bigabs <- read.xlsx("C:/CloudStor/Shared/Fisheries Research/Abalone/AbResearchData/pop/ResearchSurveys_Dec2018_JM.xlsx",
                    sheet = "FIS",
                    detectDates = TRUE)

## Data cleaning ####

## convert varible names to lowor case
colnames(bigabs) <- tolower(colnames(bigabs))
bigabs <- rename(bigabs, survdate = date)
bigabs <- rename(bigabs, sllength = length)
bigabs$string <- as.factor(bigabs$string)
#bigabs$transect <- as.factor(bigabs$transect)

#bigabs$site <- recode(bigabs$site, BR_S = "BRS", BR_B = "BRB", .default = bigabs$site)
#bigabs$site <- recode(bigabs$site, GIII = "G3", .default = bigabs$site)

## remove data with no site name or shell length
bigabs <- filter(bigabs, !is.na(site))
bigabs <- filter(bigabs, !is.na(sllength))

## remove characters from site names
bigabs$site <- gsub(' ', '', bigabs$site)
bigabs$site <- gsub('_', '', bigabs$site)
bigabs$site <- gsub('Telopea', 'TE', bigabs$site)

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
bigabdat.sub <- bigabs %>% filter(sllength <= 137) %>%
# bigabdat <- bigabs %>%
  group_by(survindex) %>%
  summarise(ab_n = n()) %>%  #as.data.frame()
  complete(survindex, fill = list(ab_n = 0)) %>%
  as.data.frame()

## Filter for legal abalone
bigabdat.leg <- bigabs %>% filter(sllength >= 138) %>%
  group_by(survindex) %>%
  summarise(ab_n = n()) %>%  #as.data.frame()
  complete(survindex, fill = list(ab_n = 0)) %>%
  as.data.frame()
 
## Filter for non-estimates abalone (for length frequency analysis)
bigabdat.meas <- subset(bigabs, is.na(estimate)) %>%
  group_by(survindex) %>%
  summarise(ab_n = n()) %>%  #as.data.frame()
  complete(survindex, fill = list(ab_n = 0)) %>%
  as.data.frame()

## join dataframes created above for survindex
bigabdat.join <- left_join(bigabdat.sub, bigabdat.leg, by = 'survindex') %>%
 left_join(., bigabdat, by = 'survindex')

## rename variables to identify abcounts from each size class
names(bigabdat.join)
bigabdat.join <- rename(bigabdat.join, ab_n_leg = ab_n.y)
bigabdat.join <- rename(bigabdat.join, ab_n_sub = ab_n.x)

## rename dataframes created above for legal and sub-legal counts for coding to follow
## and rename dataframe at the end to the original filtered name
#bigabdat <- bigabdat.sub
#bigabdat <- bigabdat.leg
 
## calculate abs per square metre 
bigabdat$absm <- bigabdat$ab_n / 15

## calculate abs per square metre for joint dataframe
bigabdat.join$absm_sub <- bigabdat.join$ab_n_sub /15
bigabdat.join$absm_leg <- bigabdat.join$ab_n_leg /15
bigabdat.join$absm <- bigabdat.join$ab_n /15

## unpack survindex variables and create new dataframe
bigabcounts <- data.frame(separate(bigabdat, survindex, sep = "_", into = c("site", "survdate", "string","transect"), convert = TRUE), bigabdat$survindex, bigabdat$ab_n, bigabdat$absm)

## unpack survindex variables and create new dataframe for the joint dataframe
bigabcounts.join <- data.frame(separate(bigabdat.join, survindex, sep = "_",
                                   into = c("site", "survdate", "string","transect"), convert = TRUE), 
                          bigabdat.join$survindex, bigabdat.join$ab_n, bigabdat.join$absm, bigabdat.join$ab_n_sub, 
                          bigabdat.join$absm_sub, bigabdat.join$ab_n_leg, bigabdat.join$absm_leg)

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
unique(bigabcounts$yr.season)
bigabcounts$yr.season <-
 ordered(bigabcounts$yr.season, levels = c("2015.Summer", "2015.Winter", "2015.Spring", "2016.Summer", "2016.Winter", "2016.Spring", "2017.Summer", "2017.Winter", "2017.Spring", "2018.Summer", "2018.Winter", "2018.Spring"))

bigabcounts.join$season <- as.factor(bigabcounts.join$season)
bigabcounts.join$season <- ordered(bigabcounts.join$season, levels=c("Summer","Winter","Spring"))
bigabcounts.join$yr.season <- interaction(bigabcounts.join$sampyear,bigabcounts.join$season)
unique(bigabcounts.join$yr.season)
bigabcounts.join$yr.season <-
 ordered(bigabcounts.join$yr.season, levels = c("2015.Summer", "2015.Winter", "2015.Spring", "2016.Summer", "2016.Winter", "2016.Spring", "2017.Summer", "2017.Winter", "2017.Spring", "2018.Summer", "2018.Winter", "2018.Spring"))

## adjust misclassified seasons
pick <- which(bigabcounts$site == "TG")
bigabcounts$yr.season[pick] <- gsub( "2015.Summer", "2015.Spring", bigabcounts$yr.season[pick])
bigabcounts$yr.season <- droplevels(bigabcounts$yr.season)

pick <- which(bigabcounts.join$site == "TG")
bigabcounts.join$yr.season[pick] <- gsub( "2015.Summer", "2015.Spring", bigabcounts.join$yr.season[pick])
bigabcounts.join$yr.season <- droplevels(bigabcounts.join$yr.season)

## subset data to include only seasonal routine sampling sites (i.e. BI, BRB, BRS, GIII, SP, TG)
bigabcounts.seasonal <- subset(bigabcounts, site %in% c("BI","BRB","BRS","GIII", "SP", "TG"))

bigabcounts.join.seasonal <- subset(bigabcounts.join, site %in% c("BI","BRB","BRS","GIII", "SP", "TG"))

## rename dataframes for legal and sub-legal counts
#bigabcounts.sub.seasonal <- bigabcounts.seasonal
#bigabcounts.leg.seasonal <- bigabcounts.seasonal

## subset data to individual ARM sampling sites
list_bigabcounts.site <- split(bigabcounts.join, bigabcounts.join$site)
names(list_bigabcounts.site)
bigabcounts.sites <- c("bigabcounts.BI",   "bigabcounts.BRB",    "bigabcounts.BRS",   "bigabcounts.GIII",
                       "bigabcounts.LB", "bigabcounts.MB", "bigabcounts.MP", "bigabcounts.OP", "bigabcounts.SP",
                     "bigabcounts.T", "bigabcounts.TE",  "bigabcounts.TG")
for (i in 1:length(list_bigabcounts.site)) {
 assign(bigabcounts.sites[i], list_bigabcounts.site[[i]])
}

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
 ordered(bigabs.sl$yr.season, levels = c("2015.Summer", "2015.Winter", "2015.Spring", "2016.Summer", "2016.Winter", "2016.Spring", "2017.Summer", "2017.Winter", "2017.Spring", "2018.Summer", "2018.Winter", "2018.Spring"))

## adjust misclassified seasons
pick <- which(bigabs.sl$site == "TG")
bigabs.sl$yr.season[pick] <- gsub( "2015.Summer", "2015.Spring", bigabs.sl$yr.season[pick])
bigabs.sl$yr.season <- droplevels(bigabs.sl$yr.season)

## subset data to include only seasonal routine ARM sampling sites (i.e. BI, BRB, BRS, GIII, SP, TG)
bigabs.sl.seasonal <- subset(bigabs.sl, site %in% c("BI","BRB","BRS","GIII", "SP", "TG"))

## subset data into routine FIS sampling sites
list_bigabs.sl.site <- split(bigabs.sl, bigabs.sl$site)
#list2env(list_bigabs.sl.site, envir = .GlobalEnv) #splits list into each site but not well labelled
names(list_bigabs.sl.site)
bigabs.sl.sites <- c("bigabs.sl.BI",   "bigabs.sl.BRB",    "bigabs.sl.BRS",   "bigabs.sl.GIII",
                     "bigabs.sl.LB", "bigabs.sl.MB",  "bigabs.sl.MP",  "bigabs.sl.OP",
                     "bigabs.sl.SP", "bigabs.sl.T", "bigabs.sl.TE", "bigabs.sl.TG")
for (i in 1:length(list_bigabs.sl.site)) {
 assign(bigabs.sl.sites[i], list_bigabs.sl.site[[i]])
}

#**************************************************************************************************#
## Abundance plots and summaries ####

## set the colour scheme for FIS strings so they contrast with ARM strings when plotting
fis.col <- c('#7CAE00', '#C77CFF')

## create short label names for plot facets 
levels(bigabcounts.seasonal$yr.season)
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
                   "2018.Spring" = '2018.Sp')

## adult abunance/m2 plot of year.season x site
ggplot(bigabcounts.seasonal, aes(x=yr.season, y=absm, group = string)) + 
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
ggplot(bigabcounts.seasonal, aes(y=absm, x=sampyear, group=season))+
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
sub.leg <- melt(bigabcounts.join.seasonal,id.vars=c('bigabdat.join.survindex', 'site', 'yr.season'), 
              measure.vars=c('absm_sub','absm_leg','absm'))

unique(sub.leg$site)
sub.leg.dat <- sub.leg %>%
 filter(site == 'BRS') %>% #only use filter for individuals sites otherwise remove filter and use facet_grid
 mutate(value = case_when(variable == 'absm_sub' ~ -value, TRUE ~ value)) #make sub-legal abs negative

sub.leg.colours = c("white", "black", "grey")

sub.leg.dat$variable <-
 ordered(sub.leg.dat$variable, levels = c("absm", "absm_leg", "absm_sub")) #re-order variables for plot
         
ggplot(sub.leg.dat, aes(x = yr.season, y = value, fill = variable))+
 stat_summary(geom = 'bar', position = 'identity', fun.data = my.stderr, colour = 'black')+
 stat_summary(fun.ymax = errorUpper, fun.ymin = errorUpper,
              geom = 'errorbar', position = 'identity', colour = 'black', width = 0.2)+
 stat_summary(fun.ymax = errorUpper, fun.ymin = mean,
              geom = 'linerange', position = 'identity', colour = 'black')+
 scale_colour_manual()+
 theme_bw()+
 xlab('Season')+
 ylab(bquote('Abalone Abundance ('*~m^2*')'))+
 labs(fill = 'Size')+
 scale_fill_manual(values = sub.leg.colours,
  name = 'Size class', breaks = c('absm', 'absm_leg', 'absm_sub'),
                     labels = c(' All BL', ' Legal', ' Sub-legal'))+
 theme(legend.title = element_blank())+
 theme(axis.text.x = element_text(angle = 0, vjust = 0.5))+
 theme(legend.justification = c(0, 1), legend.position = c(0, 1), legend.direction = 'horizontal',
       panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
 theme(legend.background = element_blank())+
 scale_x_discrete(breaks = c("2015.Summer", "2015.Winter", "2015.Spring", "2016.Summer", "2016.Winter",
                  "2016.Spring", "2017.Summer", "2017.Winter",
                  "2017.Spring", "2018.Summer", "2018.Winter", "2018.Spring"), labels = season_labels)

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

unique(bigabs.sl.SP$yr.season)

plot.order <- c("2015.Summer", "2017.Summer", "2015.Winter", "2017.Winter", "2015.Spring", "2017.Spring", "2016.Summer",
  "2018.Summer", "2016.Winter", "2018.Winter",  "2016.Spring", "2018.Spring")

ggplot(transform(bigabs.sl.SP, yr.season = factor(yr.season, levels = plot.order)), aes(x=sllength)) + 
 ylab("Frequency") +
 xlab("Shell Length (mm)")+
 geom_histogram(alpha = 0.5, binwidth = 5, fill = "blue", col=I("black"))+
 #ggtitle(paste(dum$SubBlockNo, FishYear))+
 #labs(title= Yeardum$SubBlockNo, size=10)+
 #geom_histogram(binwidth=50)+
 theme_bw()+
 facet_wrap(. ~ yr.season, ncol = 2, drop = F)+
 geom_vline(aes(xintercept = 138),colour = 'red', linetype = 'dashed', size = .5)

## size frequency by site
ggplot(bigabs.sl.SP, aes(x=sllength)) + 
 ylab("Frequency") +
 xlab("Shell Length (mm)")+
 geom_histogram(alpha = 0.5, binwidth = 5, fill = "blue", col=I("black"))+
 #ggtitle(paste(dum$SubBlockNo, FishYear))+
 #labs(title= Yeardum$SubBlockNo, size=10)+
 #geom_histogram(binwidth=50)+
 theme_bw()+
 facet_wrap(. ~ yr.season, ncol = 2, drop = F)+
 geom_vline(aes(xintercept = 138),colour = 'red', linetype = 'dashed', size = .5)



ggplot(bigabcounts.SP, aes(x=yr.season, y=absm))+# group = string)) + 
 aes(colour = string) +  theme_bw() +
 xlab("Season") + ggtitle("Abalone observed during transect surveys") +
 ylab(bquote('Abalone Abundance ('*~m^2*')')) +
 coord_cartesian(ylim = c(0, 2.5)) +
 stat_summary(geom="line", position=position_dodge(0.2), fun.data=my.stderr, size=1) + #fun.y=mean, linetype="dashed")+
 stat_summary(geom="point", position=position_dodge(0.2), fun.data=my.stderr) +
 stat_summary(geom="errorbar", position=position_dodge(0.2), fun.data=my.stderr, width = 0.125, size = 1)







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



