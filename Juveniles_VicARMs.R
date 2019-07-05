#clear environment
rm(list=ls(all=TRUE))
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
library(plotrix)
library(multcomp)
library(DescTools)
library(RVAideMemoire)
library(kSamples)
library(PMCMRplus)

setwd('C:/Users/jaimem/Documents/Abalone/VicARMs')
## import raw data from Excel for Lippies and Trumpeter surveys
juv <- read.xlsx(
 "C:/Users/jaimem/Documents/Abalone/VicARMs/VicARMS_ReSurveyData_March2019.xlsx",
 sheet = "ARM",
 detectDates = TRUE)

## convert var names to lower case
colnames(juv) <- tolower(colnames(juv))

## convert data values to factors
juv <- rename(juv, survdate = date)
juv <- rename(juv, site = location)
juv$string <- as.factor(juv$string)
juv$plate <- as.factor(juv$plate)

## data cleaning ####

## remove characters from site names. 
juv$site <- gsub( "-", "", juv$site)

## check site names.
unique(juv$site)

## Checking for outliers ####
## most animals should be < 140 mm with the majority < 80 mm
filter(juv, !is.na(ab_sl)) %>%
 ggplot() +
 geom_histogram(mapping = aes(x = ab_sl), binwidth = 5)

filter(juv, !is.na(ab_sl)) %>%
 count(cut_width(ab_sl, 5))

### Prepare dataframes for length frequency and abundance analyses ----

## add column to identify likely settlers (<25 mm) and possible migrants (>25 mm)
juv$size_class <- ifelse(juv$ab_sl <= 25, 1, 2)

## A. Extract records with abs for length frequency analysis ----
juv.sl <- filter(juv, !is.na(ab_sl))

## construct  date, quarter and season variables ----

#http://stackoverflow.com/questions/9500114/find-which-season-a-particular-date-belongs-to

## Define function to assign records to a season
getSeason <- function(DATES) {
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

## add column for year quarter, sample year, and season
juv.sl$q <- quarter(juv.sl$survdate, with_year = TRUE)
juv.sl$sampyear <- as.factor(year(juv.sl$survdate)) 
juv.sl$season <- getSeason(juv.sl$survdate) 
juv.sl$season <- as.factor(juv.sl$season)
juv.sl$season <- ordered(juv.sl$season, levels=c("Summer","Winter","Spring", 'Autumn'))
juv.sl$yr.season <- interaction(juv.sl$sampyear,juv.sl$season)
levels(juv.sl$yr.season)
juv.sl$yr.season <-
 ordered(juv.sl$yr.season, levels = c("2019.Summer", "2019.Winter", "2019.Spring", "2019.Autumn"))
unique(juv.sl$survdate)
unique(juv.sl$yr.season)

## calculate planar area of reef covered by juvenile collector that is 400 mm diameter
platearea <- pi*(0.2^2)

## create unique ID/index for each ARM and survdate combination
juv$survindex <- as.factor(paste(juv$site, juv$survdate, juv$string, juv$plate, sep="_"))

## subset and count number of animals per ARM by survdate (subset by size class if required)
dat <- filter(juv, !is.na(ab_sl))  %>%
 #dat <- filter(juv, ab_sl >=25 & ab_sl < 100) %>%
 #dat <- filter(juv, ab_sl <25 ) %>% 
 #dat <- filter(juv, ab_sl <= 100) %>%
 group_by(survindex) %>%
 summarise(ab_n=n()) %>%
 complete(survindex, fill = list(ab_n = 0)) %>%
 as.data.frame()

dat.2 <- filter(juv, !is.na(ab_sl))  %>%
 group_by(size_class, survindex) %>%
 summarise(ab_n=n()) %>%
 complete(survindex, fill = list(ab_n = 0)) %>%
 as.data.frame()

## calculate abs per square metre 
dat$absm <- dat$ab_n * (1/platearea)
dat.2$absm <- dat.2$ab_n * (1/platearea)

## unpack survindex variables and create new dataframe
abcounts <- data.frame(separate(dat, survindex, sep = "_", into = c("site", "survdate", "string","plate"), convert = TRUE), dat$survindex, dat$ab_n, dat$absm)
abcounts.2 <- data.frame(separate(dat.2, survindex, sep = "_", into = c("site", "survdate", "string","plate"), convert = TRUE), dat.2$survindex, dat.2$ab_n, dat.2$absm)

## format date variable and add year/season variables
abcounts$survdate <- as.Date(strptime(abcounts$survdate, "%Y-%m-%d"))
abcounts$sampyear <- as.factor(year(abcounts$survdate)) 
abcounts$season <- getSeason(abcounts$survdate) 

abcounts.2$survdate <- as.Date(strptime(abcounts.2$survdate, "%Y-%m-%d"))
abcounts.2$sampyear <- as.factor(year(abcounts.2$survdate)) 
abcounts.2$season <- getSeason(abcounts.2$survdate)

## create variable identifying year and season
abcounts$yr.season <- interaction(abcounts$sampyear,abcounts$season)
abcounts$yr.season <-
 ordered(abcounts$yr.season, levels = c("2019.Summer", "2019.Winter", "2019.Spring", "2019.Autumn"))

abcounts.2$yr.season <- interaction(abcounts.2$sampyear,abcounts.2$season)
abcounts.2$yr.season <-
 ordered(abcounts.2$yr.season, levels = c("2019.Summer", "2019.Winter", "2019.Spring", "2019.Autumn"))

## Figures and summaries for analyses ####

## create summary table of abalone per squre meter for site and year.season
ab_n.summary <- abcounts %>% 
 group_by(site, sampyear, survdate, season) %>%
 summarise(ab_n = sum(ab_n))

ab_n.summary.sizeclass <- abcounts.2 %>% 
 group_by(site, sampyear, survdate, season, size_class) %>%
 summarise(ab_n = sum(ab_n))

juv.size <- juv.sl %>% 
 group_by(site) %>% 
 summarise(ab_n = sum(!is.na(ab_sl)),
           mean_sl = mean(ab_sl, na.rm = T),
           se_sl = sd(ab_sl, na.rm = T)/sqrt(ab_n),
           min_sl = min(ab_sl, na.rm = T),
           max_sl = max(ab_sl, na.rm = T),
           n.plate = (sum(rock.or.plate == 'P', na.rm = T)/ab_n)*100,
           n.rock = (sum(rock.or.plate == 'R', na.rm = T)/ab_n)*100)

juv.density <- abcounts %>%
 group_by(site) %>%
 summarise(absm_mean = mean(absm),
           ab_string = sum(ab_n),
           absm_se = sd(absm)/sqrt(ab_string),
           ab_ARM = mean(ab_n),
           ab_ARM_se = sd(ab_n)/sqrt(ab_string))

juv.density.size <- abcounts.2 %>%
 group_by(site, size_class) %>%
 summarise(absm_mean = mean(absm),
           ab_string = sum(ab_n),
           absm_se = sd(absm)/sqrt(ab_string),
           ab_ARM = mean(ab_n),
           ab_ARM_se = sd(ab_n)/sqrt(ab_string))

juv.summary <- left_join(juv.density, juv.size, 'site')
write.table(juv.summary, file = "juv.summary.txt", sep = ",", quote = FALSE, row.names = F)

## quick ANOVA of density differences between sites
fit1 <- lm(absm ~ site, abcounts)
summary(fit1)

juv.small <- abcounts.2 %>%
 filter(size_class == 1)

fit3 <- lm(absm ~ site, juv.small)
summary(fit3)

juv.large <- abcounts.2 %>%
 filter(size_class == 2)

fit4 <- lm(absm ~ site, juv.large)
summary(fit4)

## G-test for difference in size ratio between sites

# http://www.biostathandbook.com/gtestind.html
# https://rcompanion.org/rcompanion/b_06.html

abcounts.2$size_class <- as.factor(abcounts.2$size_class)
abcounts.2$site <- as.factor(abcounts.2$site)
df.2 <- abcounts.2 %>%
        group_by(site, size_class) %>%
        summarise(numbers = sum(ab_n))

size.ratio.plot <- ggplot(data = df.2, aes(x = site, y = numbers, fill = factor(size_class, levels = c('2', '1'))))+
        geom_bar(stat = 'identity', position = 'fill', colour = 'black')+
        scale_y_continuous(labels = scales::percent_format())+
        theme_bw()+
        ylab('Percentage')+
        xlab('Site')+
        theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
        scale_fill_manual(values = c("gray","white"), name = 'Size class', labels = c('>25 mm', '<25 mm'))


ggsave(filename = 'size_ratio_plot.wmf', 
       plot = size.ratio.plot, width = 15.9, height = 9.6, units = 'cm')
ggsave(filename = 'size_ratio_plot.pdf', 
       plot = size.ratio.plot, width = 15.9, height = 9.6, units = 'cm')
ggsave(filename = 'size_ratio_plot.png', 
       plot = size.ratio.plot, width = 15.9, height = 9.6, units = 'cm')
        
abcounts.2.contingency <- abcounts.2 %>%
        group_by(size_class, site) %>%
        summarise(n = sum(ab_n)) %>%
        spread(size_class, n) %>%
        as.data.frame()

abcounts.2.sizeclass <- abcounts.2.contingency %>%
        remove_rownames() %>%
        column_to_rownames(var = 'site') %>%
        rename(small = 1, large = 2) %>%
        filter(small != 0 | large != 0)

df.1 <- as.matrix(abcounts.2.sizeclass)

G.test(df.1)

pairwise.G.test(df.1, 
                p.method = "none")

# # alternative GTest and pairwise comparisons
# GTest(df.1, correct = 'none')
# FUN <- function(i,j){     
#         GTest(matrix(c(matriz[i,1], matriz[i,2], 
#                        matriz[j,1], matriz[j,2]),
#                      nrow=2,
#                      byrow=TRUE),
#               correct="none")$ p.value
# }
# 
# pairwise.table(FUN,
#                rownames(matriz),
#                p.adjust.method="none")

# ggplot()+
#         geom_bar(data = abcounts.2, aes(x = dat.2.survindex, y = ab_n, fill = size_class), stat = 'identity', position = 'fill')+
#         scale_y_continuous(labels = scales::percent_format())+
#         theme(axis.text.x = element_text(angle = 90, hjust = 1))

# abcounts.2.contingency <- abcounts.2 %>%
#         group_by(size_class, dat.2.survindex) %>%
#         summarise(n = sum(ab_n)) %>%
#         spread(size_class, n) %>%
#         as.data.frame()

# abcounts.2.sizeclass <- abcounts.2.contingency %>%
#         remove_rownames() %>%
#         column_to_rownames(var = 'dat.2.survindex') %>%
#         rename(small = 1, large = 2) %>%
#         filter(small != 0 | large != 0)


## Anderson-Darling test of size structure difference between sites
df.3 <- juv.sl %>%
        dplyr::select(site, ab_sl)

ad.test(df.3$ab_sl~df.3$site, method = 'exact', dist = F, Nsim = 1000)

## size frequency distribution figure by site

juv.sl$site <- factor(juv.sl$site, levels = c("IPB", "PPCNT", "IPCT","PPC"))
size.frequency.plot <- ggplot(juv.sl, aes(x=ab_sl, fill=site, color=site)) + 
 ylab("Frequency") +
 xlab("Shell Length (mm)")+
 theme_bw()+
 geom_histogram(alpha = 0.2, binwidth = 5) +
 xlim(0, 100)+
 ylim(0, 7)+
 theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) + 
 theme(legend.position="none") +
 facet_grid(site ~ ., scales = "free_y")

ggsave(filename = 'size_frequency_plot.wmf', 
       plot = size.frequency.plot, width = 15.9, height = 9.6, units = 'cm')
ggsave(filename = 'size_frequency_plot.pdf', 
       plot = size.frequency.plot, width = 15.9, height = 9.6, units = 'cm')
ggsave(filename = 'size_frequency_plot.png', 
       plot = size.frequency.plot, width = 15.9, height = 9.6, units = 'cm')

## size structure boxplot figure by site

juv.sl$site <- factor(juv.sl$site, levels = c("IPB", "PPCNT", "IPCT","PPC"))
ab_abund_label <- c('a', 'a', 'a', 'b')

size.boxplot.plot <- filter(juv.sl, !is.na(ab_sl)) %>%
 ggplot(aes(x=site, y=ab_sl)) +
 geom_boxplot(outlier.colour = "orange", outlier.size = 1.5)+
 theme_bw()+
 theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
 theme(axis.text.x = element_text(angle = 0, hjust = 0.5))+
 xlab('Site')+
 ylab(bquote('Shell length ('*mm*')'))+
 geom_text(data = juv %>% group_by(site) %>% summarise(ab_n = sum(!is.na(ab_sl))), 
           aes(y = 100, label = ab_n), angle = 0)
 #geom_text(data = juv.sl %>% group_by(site) %>% summarise(sl_max = max(ab_sl)), 
 #          aes(y = sl_max+3, label = ab_abund_label), angle = 0)

ggsave(filename = 'size_boxplot_plot.wmf', 
       plot = size.boxplot.plot, width = 15.9, height = 9.6, units = 'cm')
ggsave(filename = 'size_boxplot_plot.pdf', 
       plot = size.boxplot.plot, width = 15.9, height = 9.6, units = 'cm')
ggsave(filename = 'size_boxplot_plot.png', 
       plot = size.boxplot.plot, width = 15.9, height = 9.6, units = 'cm')

## size structure density plot to identify modes

ggplot(juv.sl, aes(x=ab_sl, color=site)) + 
 ylab("Frequency") +
 xlab("Shell Length (mm)")+
 geom_histogram(aes(y=..density..), alpha=.2, binwidth = 5)+
 #stat_density(geom = "line", position = "identity") +
 geom_density(alpha = .2) +
 theme_bw()+
 theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
 theme(legend.position="none")+
 facet_grid(site~.)+
 xlim(0, 100)+
 ylim(0, 0.1)

## abundance figure by site
ab_abund_label_dens <- c('a', 'a', 'b', 'a')
juv.density$site <- factor(juv.density$site, levels = c("IPB", "PPCNT", "IPCT","PPC"))

density.overall.plot <- ggplot(juv.density, aes(x = site, y = absm_mean))+
 geom_point(size = 2)+
 aes(colour = site)+
 theme_bw()+
 theme(axis.text.x = element_text(angle = 0, hjust = 0.5))+
 geom_errorbar(aes(ymin = absm_mean - absm_se, ymax = absm_mean + absm_se), width = 0.125, size = 1)+
 xlab('Site')+
 ylab(bquote('Abalone Abundance ('*~m^2*')'))+
 coord_cartesian(ylim=c(0, 30))+
 theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
 theme(legend.position="none")+
 geom_text(aes(label=ab_abund_label_dens, y=absm_mean+(absm_se/1.8)), vjust=-1.5, colour = 'black') 

ggsave(filename = 'density_overall_plot.wmf', 
       plot = density.overall.plot, width = 15.9, height = 9.6, units = 'cm')
ggsave(filename = 'density_overall_plot.pdf', 
       plot = density.overall.plot, width = 15.9, height = 9.6, units = 'cm')
ggsave(filename = 'density_overall_plot.png', 
       plot = density.overall.plot, width = 15.9, height = 9.6, units = 'cm')

## abundance figure by site and size class to demostrate recruits and/or migrants
juv.density.size$size_class <- factor(as.integer(juv.density.size$size_class), levels = c(1,2))
juv.density.size$site <- factor(juv.density.size$site, levels = c("IPB", "PPCNT", "IPCT","PPC"))
stats_labs <- c('a', 'a', 'a', 'a', 'a', 'b', 'a', 'a')

ggplot(juv.density.size, aes(x = site, y = absm_mean, group = as.factor(size_class)))+
 geom_point(aes(shape=size_class, colour=site), position=position_dodge(0.5),size = 3)+
 scale_shape_manual(values=c(17, 16))+
 theme_bw()+
 theme(axis.text.x = element_text(angle = 0, hjust = 0.5))+
 geom_errorbar(aes(ymin = absm_mean - absm_se, ymax = absm_mean + absm_se, colour = site), width = 0.125, size = 1, position=position_dodge(0.5))+
 xlab('Site')+
 ylab(bquote('Abalone Abundance ('*~m^2*')'))+
 coord_cartesian(ylim=c(0, 30))+
 theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
 theme(legend.position="none")
 #geom_text(aes(label=stats_labs, y=absm_mean+(absm_se/1.8)), vjust=-1.5, colour = 'black', position=position_dodge(0.5)) 


## Frequency distribution plot of N by plate 
abcounts$site <- factor(abcounts$site, levels = c("IPB", "PPCNT", "IPCT","PPC"))
arm.count.plot <- ggplot(abcounts, aes(x=ab_n, fill=site, color=site)) + 
 ylab("Frequency") +
 xlab("N")+
 geom_histogram(alpha = 0.2, binwidth = 1)+
 theme_bw()+
 facet_grid(site ~.)+
        scale_x_continuous(breaks = seq(0, 10, 2))+
 theme(legend.position="none")+
 theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

ggsave(filename = 'arm_count_plot.wmf', 
       plot = arm.count.plot, width = 15.9, height = 9.6, units = 'cm')
ggsave(filename = 'arm_count_plot_plot.pdf', 
       plot = arm.count.plot, width = 15.9, height = 9.6, units = 'cm')
ggsave(filename = 'arm_count_plot_plot.png', 
       plot = arm.count.plot, width = 15.9, height = 9.6, units = 'cm')

## boxplot showing year x season abundance

ggplot(abcounts, aes(y=absm, x=site))+
 geom_boxplot(outlier.colour = "orange", outlier.size = 1.5)+
 theme_bw()+
 facet_grid(season ~ sampyear)+
 theme(axis.text.x = element_text(angle = 90, hjust = 1))+
 xlab('Site')+
 ylab(bquote('Abalone Abundance ('*~m^2*')'))+
 geom_text(data = ab_n.summary, aes(y = 75, label = ab_n), size = 3, angle = 0)+
 stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="black", fill="black")


 

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

abcounts$string <- factor(as.integer(abcounts.2$string), levels = c(1,2))
ggplot(abcounts, aes(x=site, y=absm, group = string)) + 
 aes(colour = site) +  theme_bw() +
 xlab("Year.Season") + #ggtitle("Shell length 0mm to 100mm") +
 ylab(bquote('Abalone Abundance ('*~m^2*')')) +
 theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
 # coord_cartesian(ylim = c(0, 15)) +
 stat_summary(geom="line", position=position_dodge(0.2), fun.data=my.stderr, size=1) + #fun.y=mean, linetype="dashed")+
 stat_summary(geom="point", position=position_dodge(0.2), fun.data=my.stderr) +
 stat_summary(geom="errorbar", position=position_dodge(0.2), fun.data=my.stderr, width = 0.125, size = 1)
 facet_grid(site ~ ., scales = "free_y" )

## juvenile abundance/m2 plot of site x year.season

# abcounts.2$string <- factor(as.integer(abcounts.2$string), levels = c(1,2))
# ggplot(abcounts.2, aes(x=site, y=absm, group = string)) + 
#  aes(colour = string) +  theme_bw() +
#  xlab("Year.Season") + #ggtitle("Shell length 0mm to 100mm") +
#  ylab(bquote('Abalone Abundance ('*~m^2*')')) +
#  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#  # coord_cartesian(ylim = c(0, 15)) +
#  stat_summary(geom="line", position=position_dodge(0.2), fun.data=my.stderr, size=1) + #fun.y=mean, linetype="dashed")+
#  stat_summary(geom="point", position=position_dodge(0.2), fun.data=my.stderr) +
#  stat_summary(geom="errorbar", position=position_dodge(0.2), fun.data=my.stderr, width = 0.125, size = 1) +
#  facet_grid(yr.season ~ ., scales = "free_y" )

## juvenile abunance n/ARM plot of year.season x site

# ggplot(abcounts, aes(x=yr.season, y=ab_n, group = string)) + 
#  aes(colour = string) +  theme_bw() +
#  #xlab("Season") + #ggtitle("Shell length 0mm to 100mm") +
#  ylab(bquote('Abalone Abundance (abalone/plate)')) +
#  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#  # coord_cartesian(ylim = c(0, 15)) +
#  stat_summary(geom="line", position=position_dodge(0.2), fun.data=my.stderr, size=1) + #fun.y=mean, linetype="dashed")+
#  stat_summary(geom="point", position=position_dodge(0.2), fun.data=my.stderr) +
#  stat_summary(geom="errorbar", position=position_dodge(0.2), fun.data=my.stderr, width = 0.125, size = 1) +
#  facet_grid(site ~ ., scales = "free_y" )

ggplot(abcounts, aes(x=yr.season, y=ab_n, group = string)) + 
 aes(colour = string) +  theme_bw() +
 xlab("Year.Season") + #ggtitle("Shell length 0mm to 100mm") +
 ylab(bquote('Abalone Abundance (abalone/plate)')) +
 theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
 # coord_cartesian(ylim = c(0, 15)) +
 stat_summary(geom="line", position=position_dodge(0.2), fun.data=my.stderr, size=1) + #fun.y=mean, linetype="dashed")+
 stat_summary(geom="point", position=position_dodge(0.2), fun.data=my.stderr) +
 stat_summary(geom="errorbar", position=position_dodge(0.2), fun.data=my.stderr, width = 0.125, size = 1)+
 facet_grid(site ~ ., scales = "free_y" )

## Figures and summaries for habitat complexity abalone abundance/m2 ####

unique(abcounts$site)

# add variable for site habitat complexity
pick1 <- which(abcounts$site %in% c("BBS"))
abcounts$habitat[pick1] <- "high"

pick2 <- which(abcounts$site %in% c("LIP", "SIS", "CQE", "TBN", "GIII"))
abcounts$habitat[pick2] <- "medium"

pick3 <- which(abcounts$site %in% c("BRS"))
abcounts$habitat[pick3] <- "low"

#mydataset <- droplevels(subset(abcounts, yr.season=="2015.Spring"))
mydataset <- abcounts
mydataset$habitat <- as.factor(mydataset$habitat)
mydataset$habitat <-
 ordered(mydataset$habitat, levels = c("low","medium", "high"))

mydataset$site <- factor(mydataset$site, levels = c("IPB", "PPCNT", "IPCT","PPC"))

ggplot(mydataset, aes(x=site, y=absm)) + 
 aes(colour = site) +  theme_bw() +
 theme(legend.position="none") +
 xlab("Site") + #ggtitle("Shell length 0mm to 100mm") +
 ylab(bquote('Abalone Abundance ('*~m^2*')')) +
 theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
 # coord_cartesian(ylim = c(0, 15)) +
 #stat_summary(geom="line", position=position_dodge(0.2), fun.data=my.stderr, size=1) + #fun.y=mean, linetype="dashed")+
 stat_summary(geom="point", position=position_dodge(0.2), fun.data=my.stderr, size = 2) +
 stat_summary(geom="errorbar", position=position_dodge(0.2), fun.data=my.stderr, width = 0.25, size = 1)
 #facet_grid( ~ habitat , scales = "free_x" )

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

subdat <- filter(abcounts, site =='IPB')
subdat$string <- as.factor(subdat$string)

ggplot(subdat, aes(y=ab_n, x=yr.season, fill=string)) +
 ggtitle("Cape Queen Elizabeth")+
 xlab("Sample date") + 
 ylab("Blacklip Abalone Abundance") +
 geom_bar(stat="identity")+
 scale_fill_grey(start = 0.3, end = 0.7)+
 theme_bw()+
 theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust=0.2), 
       text = element_text(size=16),
       legend.position=c(.95,.88)) +
 facet_grid(sampyear ~ season)

# #  +scale_x_discrete(limits=c("27 Jun", "28 Jul", "18 Aug", "25 Sep", "08 Oct", "09 Oct"))
# 
# Pick <- abcounts
# GIII <- droplevels(subset(Pick, Pick$Site=="CQE"))
# 
# ggplot(GIII, aes(y=Ab_Sum, x=survdate, fill=string)) +
#  ggtitle("George 3rd Rock")+
#  xlab("Sample date") + 
#  ylab("Blacklip Abalone Abundance") +
#  geom_bar(stat="identity")+
#  ylim(0,50)+
#  scale_fill_grey(start = 0.3, end = 0.7)+
#  theme_bw()+
#  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust=0.2), 
#        text = element_text(size=16),
#        legend.position=c(.95,.88))
# 
# # scale_x_discrete(limits=c("11 Aug", "21 Sep", "13 Oct"))
# 
# BR_B<-droplevels(subset(Pick, Pick$Site=="BR_B"))
# 
# ggplot(BR_B, aes(y=Ab_Sum, x=survdate, fill=string)) +
#  ggtitle("Black Reef Boulder")+
#  xlab("Sample date") + 
#  ylab("Blacklip Abalone Abundance") +
#  geom_bar(stat="identity")+
#  ylim(0,50)+
#  scale_fill_grey(start = 0.3, end = 0.7)+
#  theme_bw()+
#  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust=0.2), 
#        text = element_text(size=16),
#        legend.position=c(.95,.88))+
#  scale_x_discrete(limits=c("12 Aug", "30 Sep", "13 Oct"))
# 
# 
# 
# BR_S<-droplevels(subset(Pick, Pick$Site=="BR_S"))
# 
# ggplot(BR_S, aes(y=Ab_Sum, x=survdate, fill=string)) +
#  ggtitle("Black Reef Slab")+
#  xlab("Sample date") + 
#  ylab("Blacklip Abalone Abundance") +
#  geom_bar(stat="identity")+
#  ylim(0,50)+
#  scale_fill_grey(start = 0.3, end = 0.7)+
#  theme_bw()+
#  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust=0.2), 
#        text = element_text(size=16),
#        legend.position=c(.1,.87))+
#  scale_x_discrete(limits=c("24 Jul", "11 Aug", "30 Sep", "13 Oct"))
# 
# SP<-droplevels(subset(Pick, Pick$Site=="SP"))
# 
# ggplot(SP, aes(y=Ab_Sum, x=survdate, fill=string)) +
#  ggtitle("Seymour Point")+
#  xlab("Sample date") + 
#  ylab("Blacklip Abalone Abundance") +
#  geom_bar(stat="identity")+
#  ylim(0,50)+
#  scale_fill_grey(start = 0.3, end = 0.7)+
#  theme_bw()+
#  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust=0.2), 
#        text = element_text(size=16),
#        legend.position=c(.1,.87))+
#  scale_x_discrete(limits=c("29 Jul", "20 Aug", "09 Sep"))
# 
# TG<-droplevels(subset(Pick, Pick$Site=="TG"))
# 
# ggplot(TG, aes(y=Ab_Sum, x=survdate, fill=string)) +
#  ggtitle("The Gardens")+
#  xlab("Sample date") + 
#  ylab("Blacklip Abalone Abundance") +
#  geom_bar(stat="identity")+
#  ylim(0,50)+
#  scale_fill_grey(start = 0.3, end = 0.7)+
#  theme_bw()+
#  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust=0.2), 
#        text = element_text(size=16),
#        legend.position=c(.1,.87))+
#  scale_x_discrete(limits=c("29 Jul", "20 Aug", "09 Sep"))
# 
# ## Length frequency plots by site ####
# 
# TG.sl <- droplevels(subset(juv.sl, site=="CQE"))
# 
# 
# ggplot(TG.sl, aes(x=ab_sl, color=site)) + 
#  ylab("Frequency") +
#  xlab("Shell Length (mm)")+
#  geom_histogram(alpha = 0.2, binwidth = 5)+
#  #ggtitle(paste(dum$SubBlockNo, FishYear))+
#  #labs(title= Yeardum$SubBlockNo, size=10)+
#  #geom_histogram(binwidth=50)+
#  theme_bw()+
#  facet_grid(sampyear ~ season)
# 
# 
# BI.sl <- droplevels(subset(juv.sl, site=="BI"))
# 
# ggplot(BI.sl, aes(x=ab_sl, color=site)) + 
#  ylab("Frequency") +
#  xlab("Shell Length (mm)")+
#  geom_histogram(alpha = 0.2, binwidth = 5)+
#  #ggtitle(paste(dum$SubBlockNo, FishYear))+
#  #labs(title= Yeardum$SubBlockNo, size=10)+
#  #geom_histogram(binwidth=50)+
#  theme_bw()+
#  facet_grid(sampyear ~ season)
# 
# 
# BRS.sl <- droplevels(subset(juv.sl, site=="BRS"))
# 
# ggplot(BRS.sl, aes(x=ab_sl, color=site)) + 
#  ylab("Frequency") +
#  xlab("Shell Length (mm)")+
#  geom_histogram(alpha = 0.5, binwidth = 10, fill = "red", col=I("black"))+
#  #ggtitle(paste(dum$SubBlockNo, FishYear))+
#  #labs(title= Yeardum$SubBlockNo, size=10)+
#  #geom_histogram(binwidth=50)+
#  theme_bw()+
#  facet_grid(sampyear ~ season)
# 
# BRB.sl <- droplevels(subset(juv.sl, site=="BRB"))
# 
# ggplot(BRB.sl, aes(x=ab_sl, color=site)) + 
#  ylab("Frequency") +
#  xlab("Shell Length (mm)")+
#  geom_histogram(alpha = 0.5, binwidth = 10, fill = "red", col=I("black"))+
#  #ggtitle(paste(dum$SubBlockNo, FishYear))+
#  #labs(title= Yeardum$SubBlockNo, size=10)+
#  #geom_histogram(binwidth=50)+
#  theme_bw()+
#  facet_grid(sampyear ~ season)
# 
# 
# GIII.sl <- droplevels(subset(juv.sl, site=="G3"))
# 
# ggplot(GIII.sl, aes(x=ab_sl)) + 
#  ylab("Frequency") +
#  xlab("Shell Length (mm)")+
#  geom_histogram(alpha = 0.5, binwidth = 10, fill = "red", col=I("black"))+
#  #ggtitle(paste(dum$SubBlockNo, FishYear))+
#  #labs(title= Yeardum$SubBlockNo, size=10)+
#  #geom_histogram(binwidth=50)+
#  theme_bw()+
#  facet_grid(sampyear ~ season)
# 
# SP.sl <- droplevels(subset(juv.sl, site=="SP"))
# 
# ggplot(SP.sl, aes(x=ab_sl, color=site)) + 
#  ylab("Frequency") +
#  xlab("Shell Length (mm)")+
#  geom_histogram(alpha = 0.2, binwidth = 5)+
#  #ggtitle(paste(dum$SubBlockNo, FishYear))+
#  #labs(title= Yeardum$SubBlockNo, size=10)+
#  #geom_histogram(binwidth=50)+
#  theme_bw()+
#  facet_grid(sampyear ~ season)

## plots of abalone counts per ARM ####

unique(abcounts$site)
#abcounts.test <- subset(abcounts, !is.na(plate))

# Flora - change mysite names here to generate ab counts per string/plate combination
# "BBS"  "BRS"  "CQE"  "GIII" "LIP"  "SIS"  "TBN"

mysite <- "PPC"

# plotdat <- filter(abcounts, site==mysite & !is.na(plate)) %>%
#  mutate(string = factor(string)) %>%
#  group_by(string, yr.season, plate) %>%
#  summarise(cnts = sum(ab_n)) %>%
#  spread(yr.season, cnts)
# 
# pairs(plotdat[3:10],panel=panel.smooth,main = paste0("Site: ",mysite))
# 
# ggpairs(plotdat, columns = 3:12,  aes(colour = string)) +
#  theme(axis.text.x = element_text(angle = 90, hjust = 1),
#        legend.position = "right") +
#  xlab(bquote('Abalone Abundance (count/plate)')) +
#  ylab(bquote('Abalone Abundance (count/plate)'))
# 
# 
# plotdat2 <- filter(abcounts, site==mysite & !is.na(plate)) %>%
#  mutate(stringdex = paste0(string,'_',plate)) %>%
#  group_by(stringdex) %>%
#  summarise(cnts = sum(ab_n)) 

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

## STOP HERE FLORA

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