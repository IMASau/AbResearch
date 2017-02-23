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

setwd('D:/Port Davey/Port Davey')


saddle1<-read.csv('LF_saddleBight_1.csv', header = T )
saddle1<-saddle1[,1:3]
saddle2<-read.csv('LF_saddleBight_2.csv', header = T )
gulch<-read.csv('LF_saddle_Gulch.csv', header = T )
gulch<-gulch[,c(1:3,18)]
outSaddle1<-read.csv('LF_OuterSaddle.csv', header = T )
outSaddle1<-outSaddle1[,c(1:3,11)]
outSaddle2<-read.csv('LF_OuterSaddle2.csv', header = T )
outSaddle2<-outSaddle2[,c(1:3,6)]
saddle1$Site<-"Saddle1"
saddle2$Site<-"Saddle2"

outSaddle2$Site<-"OutSaddle2"
outSaddle1$Site<-"OutSaddle1"
gulch$Site<-"InnerSaddle_Gulch"

LF<-rbind(outSaddle1, outSaddle2, gulch, saddle2, saddle1)

# #replace NA with 0
# PD[is.na(PD)] <- 0
# summary(PD)

PD<-droplevels(subset(dat, dat$SPC_Id!= 1327))
PD<-droplevels(subset(dat, dat$SPC_Id!= 1323))

PD<-droplevels(subset(PD, select=c("site", "Processed.Date", "Sex", "Shell.Length")))
colnames(PD) <- c("Site", "Date", "Sex", "SL")
#remove NA's
#PD<-PD[complete.cases(PD[,5]),]
summary(PD)

# SORTING OUT THE DATES SO THEY ARE IN USEFUL FORMAT
PD$Date<-as.Date(PD$Date, format="%d/%m/%Y")
PD$newDate<-as.character(PD$Date)
PD$newDate<-format(PD$Date, "%d %b")
#PD$Date<-PD$newDate
PD$FishYear<-format(PD$Date, "%Y")
PD$FishYear<-as.factor(PD$FishYear)



# Betsey<-droplevels(subset(PD, Location== "BI" & String== "2"))
# Betsey10<-droplevels(subset(Betsey, Plate >=5))
# Betsey10<-droplevels(subset(Betsey10, Plate <=7))

# #remove NAs
# Betsey10[complete.cases(Betsey10[,5]),]


ggplot(LF, aes(x=Length, fill=Site))+
  geom_density(stat = "bin",  binwidth = 5, alpha=0.3)+
  scale_x_continuous(limits=c(0, 200))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Shell Length (mm)") + ylab("Frequency") +
  theme_bw()

ggplot(PD, aes(x=SL, fill=FishYear)) + 
  #ylab(expression(paste("GeoMean", phantom(.)%+-%phantom(.), "se"))) +
  geom_density(alpha=0.5)+
  #geom_histogram(stat = "bin", binwidth = 5, alpha=0.5)+
  #ggtitle(paste(dum$SubBlockNo, FishYear))+
  #labs(title= Yeardum$SubBlockNo, size=10)+
  #geom_histogram(binwidth=50)+
  theme_bw()+
  facet_grid(. ~ Site)

#Ab_count <- ddply(PD, "Date", summarise, Ab_ct = length(Ab_SL))

#rearregage DF giving number of PDeniles by location, date and String.
Pick<-aggregate(x = PD, by = list(PD$Site, PD$Date), FUN = "length")
Pick<-Pick[1:4]
names(Pick)[names(Pick)=="Group.2"] <- "Date"
names(Pick)[names(Pick)=="Group.1"] <- "Site"
names(Pick)[names(Pick)=="Group.3"] <- "String"
names(Pick)[names(Pick)=="Location"] <- "Ab_Sum"

ggplot(Pick, aes(y=Ab_Sum, x=Date)) +
  geom_bar(stat="identity")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_grid(. ~ Site)

#ggplot(Pick, aes(Ab_Sum, fill=Date)) + geom_bar(position="dodge")


#~~~~~~~~~~~~~~~~~~~~~~~~~BY SITE ~~~~~~~~~~~~~~~~~~~~~~~~~~~
#rearregage DF giving number of PDeniles by location and date.
BI<-droplevels(subset(Pick, Pick$Site=="BI"))

ggplot(BI, aes(y=Ab_Sum, x=Date, fill=String)) +
  ggtitle("Betsey Island")+
  xlab("Sample Date") + 
  ylab("Blacklip Abalone Abundance") +
  geom_bar(stat="identity")+
  scale_fill_grey(start = 0.3, end = 0.7)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust=0.2), 
        text = element_text(size=16),
        legend.position=c(.95,.88))+
  scale_x_discrete(limits=c("27 Jun", "28 Jul", "18 Aug", "25 Sep", "08 Oct", "09 Oct"))

GIII<-droplevels(subset(Pick, Pick$Site=="GIII"))

ggplot(GIII, aes(y=Ab_Sum, x=Date, fill=String)) +
  ggtitle("George 3rd Rock")+
  xlab("Sample Date") + 
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

ggplot(BR_B, aes(y=Ab_Sum, x=Date, fill=String)) +
  ggtitle("Black Reef Boulder")+
  xlab("Sample Date") + 
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

ggplot(BR_S, aes(y=Ab_Sum, x=Date, fill=String)) +
  ggtitle("Black Reef Slab")+
  xlab("Sample Date") + 
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

ggplot(SP, aes(y=Ab_Sum, x=Date, fill=String)) +
  ggtitle("Seymour Point")+
  xlab("Sample Date") + 
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

ggplot(TG, aes(y=Ab_Sum, x=Date, fill=String)) +
  ggtitle("The Gardens")+
  xlab("Sample Date") + 
  ylab("Blacklip Abalone Abundance") +
  geom_bar(stat="identity")+
  ylim(0,50)+
  scale_fill_grey(start = 0.3, end = 0.7)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust=0.2), 
        text = element_text(size=16),
        legend.position=c(.1,.87))+
  scale_x_discrete(limits=c("29 Jul", "20 Aug", "09 Sep"))

