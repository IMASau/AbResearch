#clear environment
#rm(list=ls(all=TRUE))
library(dplyr)
library(ggplot2)
library(scales)
library(scales)
library(tidyr)
library(gdata)
library(xlsx)
library(lubridate)

source("C:/GitCode/AbResearch/getSeason.r")

juv <- read.xlsx(
 "D:/Fisheries Research/Abalone/AbResearchData/pop/2017/Juvenile_data_2016.xlsx",
 sheetName = "AllSites", col.names = TRUE
)

## convert var names to lowor case
colnames(juv) <- tolower(colnames(juv))
juv <- rename(juv, survdate = date)
juv$string <- as.factor(juv$string)
juv$plate <- as.factor(juv$plate)


juv$location <- gsub( "_", "", juv$location)
unique(juv$location)

### Prepare dataframes for length frequency and abundance analyses ----

## A. Extract records with abs for length frequency analysis ----
juv.sl <- filter(juv, !is.na(ab_sl))

## construct  date, quarter and season variables ----
#juv.sl$q <- quarter(juv.sl$survdate, with_year = TRUE)
juv.sl$sampyear <- year(juv.sl$survdate) 
juv.sl$season <- getSeason(juv.sl$survdate) 
## recode autumn samples as summer
juv.sl$season <- gsub( "Autumn", "Summer", juv.sl$season)
juv.sl$season <- as.factor(juv.sl$season)
juv.sl$season <- ordered(juv.sl$season, levels=c("Summer","Winter","Spring"))
juv.sl$yr.season <- interaction(juv.sl$sampyear,juv.sl$season)
juv.sl$yr.season <-
 ordered(juv.sl$yr.season, levels = c("2015.Summer", "2015.Winter", "2015.Spring", "2016.Summer", "2016.Winter", "2016.Spring"))
pick <- which(juv.sl$location == "TG")
juv.sl$yr.season[pick] <- gsub( "2015.Summer", "2015.Spring", juv.sl$yr.season[pick])
juv.sl$yr.season <- droplevels(juv.sl$yr.season)



## B. Extract and prepare records with abs for abudnance analyses ----
#juv$ab_sl <- NAToUnknown(x = juv$ab_sl, unknown = 0)
## NOTE:  sites were surveyed on different days, and not always entirely in the one season


platearea <- 0.503 #  var for planar area of reef covered by juvenile collector

juv$survindex <- as.factor(paste(juv$location, juv$survdate, juv$string, juv$plate, sep="_"))

#dat <- filter(juv, ab_sl >=75 & ab_sl < 100) %>%
#dat <- filter(juv, ab_sl <25 ) %>% 
dat <- filter(juv, ab_sl <= 100) %>% 
 group_by(survindex) %>%
 summarise(ab_n =n()) %>%  #as.data.frame()
 complete(survindex, fill = list(ab_n = 0)) %>%
 as.data.frame()

## calculate abs per square metre 
dat$absm <- dat$ab_n * (1/platearea)

## unpack survindex var
abcounts <- data.frame(separate(dat, survindex, sep = "_", into = c("location", "survdate", "string","plate"), convert = TRUE), ab_count$survindex, ab_count$ab_n, ab_count$absm)

abcounts$survdate <- as.Date(strptime(abcounts$survdate, "%Y-%m-%d"))
abcounts$sampyear <- year(abcounts$survdate) 
abcounts$season <- getSeason(abcounts$survdate) 
## recode autumn samples as summer
abcounts$season <- gsub( "Autumn", "Summer", abcounts$season)
abcounts$season <- as.factor(abcounts$season)
abcounts$season <- ordered(abcounts$season, levels=c("Summer","Winter","Spring"))
abcounts$yr.season <- interaction(abcounts$sampyear,abcounts$season)
abcounts$yr.season <-
 ordered(abcounts$yr.season, levels = c("2015.Summer", "2015.Winter", "2015.Spring", "2016.Summer", "2016.Winter", "2016.Spring"))
pick <- which(abcounts$location == "TG")
abcounts$yr.season[pick] <- gsub( "2015.Summer", "2015.Spring", abcounts$yr.season[pick])
abcounts$yr.season <- droplevels(abcounts$yr.season)

levels(abcounts$yr.season)

# Betsey<-droplevels(subset(juv, location== "BI" & string== "2"))
# Betsey10<-droplevels(subset(Betsey, plate >=5))
# Betsey10<-droplevels(subset(Betsey10, plate <=7))

# #remove NAs
# Betsey10[complete.cases(Betsey10[,5]),]

##------------------------------------------------------##
##------------------------------------------------------##


## Figures for length frequency analyses ####

ggplot(ab_count, aes(y=ab_ct, x=survdate)) +
 geom_bar(stat="identity")+
 theme_bw()+
 theme(axis.text.x = element_text(angle = 90, hjust = 1))+
 facet_grid(. ~ location)

#ggplot(Pick, aes(Ab_Sum, fill=survdate)) + geom_bar(position="dodge")


ggplot(juv.sl, aes(x=ab_sl, group=location, color=location))+
 geom_histogram(stat = "bin", colour="grey", binwidth = 5, stack=FALSE)+
 scale_x_continuous(limits=c(0, 150))


ggplot(juv.sl, aes(x=ab_sl, color=location)) + 
 ylab("Frequency") +
 xlab("Shell Length (mm)")+
 geom_histogram(alpha = 0.2, binwidth = 5)+
 #ggtitle(paste(dum$SubBlockNo, FishYear))+
 #labs(title= Yeardum$SubBlockNo, size=10)+
 #geom_histogram(binwidth=50)+
 theme_bw()+
 facet_grid(location ~ yr.season)

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


unique(abcounts$location)

subdat <- filter(abcounts, location =='SP')
subdat$string <- as.factor(subdat$string)

abcounts$string <- as.factor(abcounts$string)

ggplot(abcounts, aes(x=yr.season, y=absm, group = string)) + 
 aes(colour = string) +  theme_bw() +
 xlab("Season") + ggtitle("Shell length 0mm to 100mm") +
 ylab(bquote('Abalone Abundance ('*~m^2*')')) +
 coord_cartesian(ylim = c(0, 15)) +
 stat_summary(geom="line", position=position_dodge(0.2), fun.data=my.stderr, size=1) + #fun.y=mean, linetype="dashed")+
 stat_summary(geom="point", position=position_dodge(0.2), fun.data=my.stderr) +
 stat_summary(geom="errorbar", position=position_dodge(0.2), fun.data=my.stderr, width = 0.125, size = 1) +
 facet_grid(location ~ . )



ggplot(betsey, aes(y=absm, x=yr.season, fill=string)) +
 ggtitle("Betsey Island")+
 xlab("Sample date") + 
 ylab("Blacklip Abalone Abundance") +
 geom_bar(stat="identity")+
 scale_fill_grey(start = 0.3, end = 0.7)+
 theme_bw()+
 theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust=0.2), 
       text = element_text(size=16),
       legend.position=c(.95,.88))

+
 # scale_x_discrete(limits=c("27 Jun", "28 Jul", "18 Aug", "25 Sep", "08 Oct", "09 Oct"))
 
 GIII<-droplevels(subset(Pick, Pick$Site=="GIII"))

ggplot(GIII, aes(y=Ab_Sum, x=survdate, fill=string)) +
 ggtitle("George 3rd Rock")+
 xlab("Sample date") + 
 ylab("Blacklip Abalone Abundance") +
 geom_bar(stat="identity")+
 ylim(0,50)+
 scale_fill_grey(start = 0.3, end = 0.7)+
 theme_bw()+
 theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust=0.2), 
       text = element_text(size=16),
       legend.position=c(.95,.88))+
 scale_x_discrete(limits=c("11 Aug", "21 Sep", "13 Oct"))

BR_B<-droplevels(subset(Pick, Pick$Site=="BR_B"))

ggplot(BR_B, aes(y=Ab_Sum, x=survdate, fill=string)) +
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

