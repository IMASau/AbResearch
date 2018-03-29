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


source("D:/GitCode/AbResearch/getSeason.r")
juv <- read.xlsx(
 "D:/OneDrive - University of Tasmania/Fisheries Research/Abalone/AbResearchData/pop/ResearchSurveys.xlsx",
 sheet = "Juv",
 detectDates = TRUE)

## convert var names to lowor case
colnames(juv) <- tolower(colnames(juv))
juv <- rename(juv, survdate = date)
juv <- rename(juv, site = location)
juv$string <- as.factor(juv$string)
juv$plate <- as.factor(juv$plate)


juv$site <- gsub( "_", "", juv$site)
unique(juv$site)

## Checking for outliers ####
filter(juv, !is.na(ab_sl)) %>%
 ggplot() +
 geom_histogram(mapping = aes(x = ab_sl), binwidth = 5)

filter(juv, !is.na(ab_sl)) %>%
 count(cut_width(ab_sl, 5))

filter(juv, !is.na(ab_sl)) %>%
 ggplot(aes(x=site, y=ab_sl)) +
 geom_boxplot()


### Prepare dataframes for length frequency and abundance analyses ----

## A. Extract records with abs for length frequency analysis ----
juv.sl <- filter(juv, !is.na(ab_sl))

## construct  date, quarter and season variables ----
#juv.sl$q <- quarter(juv.sl$survdate, with_year = TRUE)
juv.sl$sampyear <- as.factor(year(juv.sl$survdate)) 
juv.sl$season <- getSeason(juv.sl$survdate) 
## recode autumn samples as summer
juv.sl$season <- gsub( "Autumn", "Summer", juv.sl$season)
juv.sl$season <- as.factor(juv.sl$season)
juv.sl$season <- ordered(juv.sl$season, levels=c("Summer","Winter","Spring"))
juv.sl$yr.season <- interaction(juv.sl$sampyear,juv.sl$season)
levels(juv.sl$yr.season)
juv.sl$yr.season <-
 ordered(juv.sl$yr.season, levels = c("2015.Summer", "2015.Winter", "2015.Spring", "2016.Summer", "2016.Winter", "2016.Spring", "2017.Summer", "2017.Winter", "2017.Spring"))
pick <- which(juv.sl$site == "TG")
juv.sl$yr.season[pick] <- gsub( "2015.Summer", "2015.Spring", juv.sl$yr.season[pick])
juv.sl$yr.season <- droplevels(juv.sl$yr.season)

unique(juv.sl$survdate)
unique(juv.sl$yr.season)

## B. Extract and prepare records with abs for abundance analyses ----
#juv$ab_sl <- NAToUnknown(x = juv$ab_sl, unknown = 0)
## NOTE:  sites were surveyed on different days, and not always entirely in the one season


platearea <- 0.503 #  var for planar area of reef covered by juvenile collector

juv$survindex <- as.factor(paste(juv$site, juv$survdate, juv$string, juv$plate, sep="_"))

## Subset by size ------------------##
dat <- filter(juv, !is.na(ab_sl))  %>%
#dat <- filter(juv, ab_sl >=25 & ab_sl < 100) %>%
#dat <- filter(juv, ab_sl <25 ) %>% 
#dat <- filter(juv, ab_sl <= 100) %>%
 group_by(survindex) %>%
 summarise(ab_n =n()) %>%  #as.data.frame()
 complete(survindex, fill = list(ab_n = 0)) %>%
 as.data.frame()

# ## USe all data -------------------##
# dat <- group_by(juv, survindex) %>%
#  summarise(ab_n =n()) %>%  #as.data.frame()
#  complete(survindex, fill = list(ab_n = 0)) %>%
#  as.data.frame()


## calculate abs per square metre 
dat$absm <- dat$ab_n * (1/platearea)

## unpack survindex var
abcounts <- data.frame(separate(dat, survindex, sep = "_", into = c("site", "survdate", "string","plate"), convert = TRUE), dat$survindex, dat$ab_n, dat$absm)

abcounts$survdate <- as.Date(strptime(abcounts$survdate, "%Y-%m-%d"))
abcounts$sampyear <- as.factor(year(abcounts$survdate)) 
abcounts$season <- getSeason(abcounts$survdate) 
## recode autumn samples as summer
abcounts$season <- gsub( "Autumn", "Summer", abcounts$season)
abcounts$season <- as.factor(abcounts$season)
abcounts$season <- ordered(abcounts$season, levels=c("Summer","Winter","Spring"))
abcounts$yr.season <- interaction(abcounts$sampyear,abcounts$season)
abcounts$yr.season <-
 ordered(abcounts$yr.season, levels = c("2015.Summer", "2015.Winter", "2015.Spring", "2016.Summer", "2016.Winter", "2016.Spring", "2017.Summer", "2017.Winter", "2017.Spring"))
pick <- which(abcounts$site == "TG")
abcounts$yr.season[pick] <- gsub( "2015.Summer", "2015.Spring", abcounts$yr.season[pick])
abcounts$yr.season <- droplevels(abcounts$yr.season)

levels(abcounts$yr.season)

# Betsey<-droplevels(subset(juv, site== "BI" & string== "2"))
# Betsey10<-droplevels(subset(Betsey, plate >=5))
# Betsey10<-droplevels(subset(Betsey10, plate <=7))

# #remove NAs
# Betsey10[complete.cases(Betsey10[,5]),]

##------------------------------------------------------##
##------------------------------------------------------##


## Figures for length frequency analyses ####

ggplot(abcounts, aes(y=absm, x=yr.season)) +
  geom_bar(stat="identity")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_grid(. ~ site)

#ggplot(Pick, aes(Ab_Sum, fill=survdate)) + geom_bar(position="dodge")


## Frequency distribution of N by plate 
cnt.dat <- droplevels(subset(abcounts, site=="BRB"))

ggplot(cnt.dat, aes(x=ab_n, color=site)) + 
 ylab("Frequency") +
 xlab("N")+
 geom_histogram(alpha = 0.2, binwidth = 1)+
 #ggtitle(paste(dum$SubBlockNo, FishYear))+
 #labs(title= Yeardum$SubBlockNo, size=10)+
 #geom_histogram(binwidth=50)+
 theme_bw()+
 facet_grid(yr.season ~ string)



ggplot(juv.sl, aes(x=ab_sl, group=as.factor(site), color=as.factor(site))) +
 geom_histogram(stat = "bin", colour="grey", binwidth = 5)+
 scale_x_continuous(limits=c(0, 150))
 

ggplot(juv.sl, aes(x=ab_sl, color=site)) + 
 ylab("Frequency") +
 xlab("Shell Length (mm)")+
 theme_bw()+
 geom_histogram(binwidth = 10) +
 theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
 theme(legend.position="none") +
 #ggtitle(paste(dum$SubBlockNo, FishYear))+
 #labs(title= Yeardum$SubBlockNo, size=10)+
 facet_grid(site ~ yr.season)
#facet_grid(site ~ yr.season, scales = "free_y")


ggplot(juv.sl, aes(x=ab_sl, color=site)) + 
 ylab("Frequency") +
 xlab("Shell Length (mm)")+
 geom_histogram(aes(y=..density..), alpha=.2, binwidth = 10)+
 #stat_density(geom = "line", position = "identity") +
 geom_density(alpha = .2) +
 theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
 theme(legend.position="none") +
  facet_grid(site ~ yr.season)

ggplot(juv.sl, aes(x=ab_sl)) + 
 ylab("Frequency") +
 xlab("Shell Length (mm)")+
 geom_histogram(aes(y=..density..),alpha=.2, binwidth = 10)+
 geom_density(alpha=.2) +
 theme_bw()+
 facet_grid( ~ yr.season)



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

subdat <- filter(abcounts, site =='SP')
subdat$string <- as.factor(subdat$string)

abcounts$string <- factor(as.integer(abcounts$string), levels = c(1,2))

## juveniles as abalone/m2
ggplot(abcounts, aes(x=yr.season, y=absm, group = string)) + 
 aes(colour = string) +  theme_bw() +
 #xlab("Season") + #ggtitle("Shell length 0mm to 100mm") +
 ylab(bquote('Abalone Abundance ('*~m^2*')')) +
 theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
# coord_cartesian(ylim = c(0, 15)) +
 stat_summary(geom="line", position=position_dodge(0.2), fun.data=my.stderr, size=1) + #fun.y=mean, linetype="dashed")+
 stat_summary(geom="point", position=position_dodge(0.2), fun.data=my.stderr) +
 stat_summary(geom="errorbar", position=position_dodge(0.2), fun.data=my.stderr, width = 0.125, size = 1) +
 facet_grid(site ~ ., scales = "free_y" )


## juveniles as counts
ggplot(abcounts, aes(x=yr.season, y=ab_n, group = string)) + 
 aes(colour = string) +  theme_bw() +
 #xlab("Season") + #ggtitle("Shell length 0mm to 100mm") +
 ylab(bquote('Abalone Abundance (abalone/plate)')) +
 theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
 # coord_cartesian(ylim = c(0, 15)) +
 stat_summary(geom="line", position=position_dodge(0.2), fun.data=my.stderr, size=1) + #fun.y=mean, linetype="dashed")+
 stat_summary(geom="point", position=position_dodge(0.2), fun.data=my.stderr) +
 stat_summary(geom="errorbar", position=position_dodge(0.2), fun.data=my.stderr, width = 0.125, size = 1) +
 facet_grid(site ~ ., scales = "free_y" )

## recode by habitat complexity
pick1 <- which(abcounts$site %in% c("TG", "SP", "BRB"))
abcounts$habitat[pick1] <- "high"

pick2 <- which(abcounts$site %in% c("BI", "G3"))
abcounts$habitat[pick2] <- "medium"

pick3 <- which(abcounts$site %in% c("BRS"))
abcounts$habitat[pick3] <- "low"

mydataset <- droplevels(subset(abcounts, yr.season=="2015.Spring"))
mydataset$habitat <- as.factor(mydataset$habitat)
mydataset$habitat <-
 ordered(mydataset$habitat, levels = c("low","medium", "high"))

mydataset$site <- factor(mydataset$site, levels = c("BRS", "G3", "BRB", "TG", "BI", "SP"))

mydataset$string <- factor(as.integer(mydataset$string), levels = c(1,2))
    
ggplot(mydataset, aes(x=site, y=absm, group = as.factor(string))) + 
 aes(colour = string) +  theme_bw() +
 #xlab("Season") + #ggtitle("Shell length 0mm to 100mm") +
 ylab(bquote('Abalone Abundance ('*~m^2*')')) +
 theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
 # coord_cartesian(ylim = c(0, 15)) +
 #stat_summary(geom="line", position=position_dodge(0.2), fun.data=my.stderr, size=1) + #fun.y=mean, linetype="dashed")+
 stat_summary(geom="point", position=position_dodge(0.2), fun.data=my.stderr, size = 2) +
 stat_summary(geom="errorbar", position=position_dodge(0.2), fun.data=my.stderr, width = 0.25, size = 1) +
 facet_grid( ~ habitat , scales = "free_x" )

ggplot(betsey, aes(y=ab_n, x=yr.season, fill=string)) +
  ggtitle("Betsey Island")+
  xlab("Sample date") + 
  ylab("Blacklip Abalone Abundance") +
  geom_bar(stat="identity")+
  scale_fill_grey(start = 0.3, end = 0.7)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust=0.2), 
        text = element_text(size=16),
        legend.position=c(.95,.88)) +
 facet_grid(sampyear ~ season)

#  +scale_x_discrete(limits=c("27 Jun", "28 Jul", "18 Aug", "25 Sep", "08 Oct", "09 Oct"))

GIII<-droplevels(subset(Pick, Pick$Site=="G3"))

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
        legend.position=c(.95,.88))

# scale_x_discrete(limits=c("11 Aug", "21 Sep", "13 Oct"))

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

## Length frequency by site

TG.sl <- droplevels(subset(juv.sl, site=="TG"))


ggplot(TG.sl, aes(x=ab_sl, color=site)) + 
 ylab("Frequency") +
 xlab("Shell Length (mm)")+
 geom_histogram(alpha = 0.2, binwidth = 5)+
 #ggtitle(paste(dum$SubBlockNo, FishYear))+
 #labs(title= Yeardum$SubBlockNo, size=10)+
 #geom_histogram(binwidth=50)+
 theme_bw()+
 facet_grid(sampyear ~ season)


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

##-----------------------------------------------------------------##
## AbCounts by plate

mysite <- "BI"
plotdat <- filter(abcounts, site==mysite) %>%
 mutate(string = factor(string)) %>%
  group_by(string, yr.season, plate) %>%
 summarise(cnts = sum(ab_n)) %>%
 spread(yr.season, cnts)

pairs(plotdat[3:10],panel=panel.smooth,main = paste0("Site: ",mysite))

ggpairs(plotdat, columns = 3:10,  aes(colour = string)) +
 theme(axis.text.x = element_text(angle = 90, hjust = 1),
       legend.position = "right") +
 xlab(bquote('Abalone Abundance (count/plate)')) +
 ylab(bquote('Abalone Abundance (count/plate)'))


plotdat2 <- filter(abcounts, site==mysite) %>%
 mutate(stringdex = paste0(string,'_',plate)) %>%
 group_by(stringdex) %>%
 summarise(cnts = sum(ab_n)) 
 
#hist(plotdat2$cnts, breaks = 20)


filter(abcounts, site==mysite) %>%
 group_by(string, yr.season, plate) %>%
 summarise(cnts = sum(ab_n)) %>%
 mutate(stringdex = paste0(as.character(string),'_',as.character(plate))) %>%
  transform(stringdex=reorder(stringdex, cnts) ) %>%
 ggplot(aes(x=stringdex, y= cnts)) +
 geom_boxplot() + 
 theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
 

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

