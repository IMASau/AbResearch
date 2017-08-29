library(dplyr)
library(ggplot2)
library(scales)
library(scales)
library(tidyr)
library(gdata)
library(openxlsx)
library(lubridate)
bigabs <- read.xlsx(
 "D:/Owncloud/Fisheries Research/Abalone/AbResearchData/pop/2017/Ab_pop_bio_Lenght_density_working.xlsx",
 sheet = "AllSites",
 detectDates = TRUE)

source("D:/GitCode/AbResearch/getSeason.r")

## convert var names to lowor case
colnames(bigabs) <- tolower(colnames(bigabs))
bigabs <- rename(bigabs, survdate = date)
bigabs <- rename(bigabs, sllength = length)
bigabs$string <- as.factor(bigabs$string)
#bigabs$transect <- as.factor(bigabs$transect)

## Filter for legal biomass
#dat <- filter(temp, sllength > 137) %>%
bigabs <- filter(bigabs, !is.na(sllength))

bigabs$survindex <- as.factor(paste(bigabs$site, bigabs$survdate, bigabs$string, bigabs$transect, sep="_"))

#dat <- filter(juv, ab_sl >=75 & ab_sl < 100) %>%
#dat <- filter(juv, ab_sl <25 ) %>% 
#dat <- filter(juv, ab_sl <= 100) %>% 
 
 bigabdat <- group_by(bigabs, survindex) %>%
 summarise(ab_n =n()) %>%  #as.data.frame()
 complete(survindex, fill = list(ab_n = 0)) %>%
 as.data.frame()

## calculate abs per square metre 
 bigabdat$absm <- bigabdat$ab_n / 15

## unpack survindex var
bigabcounts <- data.frame(separate(bigabdat, survindex, sep = "_", into = c("site", "survdate", "string","transect"), convert = TRUE), bigabdat$survindex, bigabdat$ab_n, bigabdat$absm)

bigabcounts$survdate <- as.Date(strptime(bigabcounts$survdate, "%Y-%m-%d"))
bigabcounts$sampyear <- year(bigabcounts$survdate) 
bigabcounts$season <- getSeason(bigabcounts$survdate) 
## recode autumn samples as summer
bigabcounts$season <- gsub( "Autumn", "Summer", bigabcounts$season)
bigabcounts$season <- as.factor(bigabcounts$season)
bigabcounts$season <- ordered(bigabcounts$season, levels=c("Summer","Winter","Spring"))
bigabcounts$yr.season <- interaction(bigabcounts$sampyear,bigabcounts$season)
bigabcounts$yr.season <-
 ordered(bigabcounts$yr.season, levels = c("2015.Summer", "2015.Winter", "2015.Spring", "2016.Summer", "2016.Winter", "2016.Spring", "2017.Summer", "2017.Winter"))
pick <- which(bigabcounts$location == "TG")
bigabcounts$yr.season[pick] <- gsub( "2015.Summer", "2015.Spring", bigabcounts$yr.season[pick])
bigabcounts$yr.season <- droplevels(bigabcounts$yr.season) 



unique(bigabcounts$site)
mydat <- subset(bigabcounts, site %in% c("BI","BRB","BRS","GIII", "SP", "TG"))


mydat$string <- as.factor(mydat$string)

ggplot(mydat, aes(x=yr.season, y=absm, group = string)) + 
 aes(colour = string) +  theme_bw() +
 xlab("Season") + ggtitle("Abalone observed during transsect surveys") +
 ylab(bquote('Abalone Abundance ('*~m^2*')')) +
 coord_cartesian(ylim = c(0, 5)) +
 stat_summary(geom="line", position=position_dodge(0.2), fun.data=my.stderr, size=1) + #fun.y=mean, linetype="dashed")+
 stat_summary(geom="point", position=position_dodge(0.2), fun.data=my.stderr) +
 stat_summary(geom="errorbar", position=position_dodge(0.2), fun.data=my.stderr, width = 0.125, size = 1) +
 facet_grid(site ~ . )



mydatsl <- subset(bigabs, site %in% c("BI","BRB","BRS","GIII", "SP", "TG"))
## construct  date, quarter and season variables ----
#juv.sl$q <- quarter(juv.sl$survdate, with_year = TRUE)
mydatsl$sampyear <- year(mydatsl$survdate) 
mydatsl$season <- getSeason(mydatsl$survdate) 
## recode autumn samples as summer
mydatsl$season <- gsub( "Autumn", "Summer", mydatsl$season)
mydatsl$season <- as.factor(mydatsl$season)
mydatsl$season <- ordered(mydatsl$season, levels=c("Summer","Winter","Spring"))
mydatsl$yr.season <- interaction(mydatsl$sampyear,mydatsl$season)
mydatsl$yr.season <-
 ordered(mydatsl$yr.season, levels = c("2015.Summer", "2015.Winter", "2015.Spring", "2016.Summer", "2016.Winter", "2016.Spring", "2017.Summer", "2017.Winter"))
pick <- which(mydatsl$site == "TG")
mydatsl$yr.season[pick] <- gsub( "2015.Summer", "2015.Spring", mydatsl$yr.season[pick])
mydatsl$yr.season <- droplevels(mydatsl$yr.season)




ggplot(mydatsl, aes(x=sllength, color=site)) + 
 ylab("Frequency") +
 xlab("Shell Length (mm)")+
 geom_histogram(aes(y=..density..), alpha = 0.2, binwidth = 5)+
 geom_density(alpha=.2) +
 theme_bw()+
 facet_grid(site ~ yr.season)



## Length frequency by site

TG.sl.big <- droplevels(subset(mydatsl, site=="BRS"))


ggplot(TG.sl.big, aes(x=sllength)) + 
 ylab("Frequency") +
 xlab("Shell Length (mm)")+
 geom_histogram(alpha = 0.5, binwidth = 10, fill = "blue", col=I("black"))+
 #ggtitle(paste(dum$SubBlockNo, FishYear))+
 #labs(title= Yeardum$SubBlockNo, size=10)+
 #geom_histogram(binwidth=50)+
 theme_bw()+
 facet_grid(sampyear ~ season)





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



