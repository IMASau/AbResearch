#clear environment
rm(list=ls(all=TRUE))
library(lattice)
library(latticeExtra)
library(grid)
library(gridBase)
library(plyr)
library(dplyr)
library(ggplot2)
library(scales)
library(extrafont)
library(extrafontdb)
library(scales)
library(reshape2)

setwd('D:/R_Stuff')

Density<-read.csv('BR_DensityData.csv', header = T )

# #replace NA with 0
# Density[is.na(Density)] <- 0
# summary(Density)


#remove NA's
#Density<-Density[complete.cases(Density[,5]),]
summary(Density)
Density$String<-as.factor(Density$String)
Density$Transect<-as.factor(Density$Transect)
# SORTING OUT THE DATES SO THEY ARE IN USEFUL FORMAT
Density$Date<-as.Date(Density$Date, format="%d/%m/%Y")
Density$newDate<-as.character(Density$Date)
Density$newDate<-format(Density$Date, "%d %b %Y")
Density$Date<-Density$newDate
Density<-Density[,-9]

ggplot(Density, aes(x=Length, fill=Site)) +
  #ggtitle("Black Reef Boulder")+
  ylim(0,250)+
  xlim(0,200)+
  xlab("Shell Length (mm)") + 
  ylab("n") +
  geom_histogram(stat = "bin",  binwidth = 5, alpha=0.5)+
  #ylim(0,50)+
  #scale_fill_grey(start = 0.3, end = 0.7)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust=0.2), 
        text = element_text(size=16),
        legend.position=c(.8,.8))
  
Slab<-droplevels(subset(Density, Density$Site=="BR_S"))

ggplot(Slab, aes(x=Length, fill = Date)) +
  geom_histogram(size=1.5, alpha= 0.5, binwidth = 5)+
  ylim(0,150)+
  xlim(0,200)+
  ylab("n") +
  xlab("Shell Length (mm)")+
  #ggtitle(dum$SubBlockNo)+
  theme_bw()+#white background
  theme(legend.position=c(0.2, 0.70), #legend.direction = "horizontal",
        legend.title=element_text(size=14),
        legend.text = element_text(size=14),
        axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=14))

Blder<-droplevels(subset(Density, Density$Site=="BR_B"))

ggplot(Blder, aes(x=Length, fill = Date)) +
  geom_histogram(size=1.5, alpha= 0.5, binwidth = 5)+
  ylim(0,150)+
  xlim(0,200)+
  ylab("n") +
  xlab("Shell Length (mm)")+
  #ggtitle(dum$SubBlockNo)+
  theme_bw()+#white background
  theme(legend.position=c(0.2, 0.70), #legend.direction = "horizontal",
        legend.title=element_text(size=14),
        legend.text = element_text(size=14),
        axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=14))



#+++++++++++++++++++++++++++

ab_m2<-ddply(Density,.(Site, Date, String), summarize,  n = length(Length), mean_SL=mean(Length) )
ab_m2$density<-ab_m2$n/150 #15m  by 1m transects *10
ab_m2

Slab_ab_m2<-droplevels(subset(ab_m2, ab_m2$Site=="BR_S"))

ggplot(ab_m2, aes(y=density, x=Date, fill=String)) +
  ggtitle("Slab")+
  ylim(0,5)+
  xlab("Sample Date") + 
  ylab("Blacklip Abalone density m2") +
  geom_bar(stat="identity")+
  scale_fill_grey(start = 0.3, end = 0.7)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust=0.2), 
        text = element_text(size=16),
        legend.position=c(.95,.88))+
  scale_x_discrete(limits=c("09 Oct 2015", "20 Oct 2015", "03 Mar 2016"))

blder_ab_m2<-droplevels(subset(ab_m2, ab_m2$Site=="BR_B"))

ggplot(blder_ab_m2, aes(y=density, x=Date, fill=String)) +
  ggtitle("Boulder")+
  ylim(0,5)+
  xlab("Sample Date") + 
  ylab("Blacklip Abalone density m2") +
  geom_bar(stat="identity")+
  scale_fill_grey(start = 0.3, end = 0.7)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust=0.2), 
        text = element_text(size=16),
        legend.position=c(.95,.88))+
  scale_x_discrete(limits=c("14 Oct 2015", "20 Oct 2015", "04 Mar 2016"))


#save Rfile
outFile <- paste(myWorkPath,"/BR_Density",format(Sys.time(), "%Y-%m-%d"),".RData",sep ="")
save.image(file = outFile)
