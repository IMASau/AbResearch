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
library(gdata)

setwd('F:/FRDC 7. Abalone recruitment/Data')

JUV<-read.csv('Juvenile_data_19_08_2016.csv', header = T )


JUV <- read.xlsx(
 "D:/Fisheries Research/Abalone/AbResearchData/pop/2017/Juvenile_data_2016.xlsx",
 sheetName = "AllSites",
 col.names = TRUE
)

JUV$Ab_SL <- NAToUnknown(x = JUV$Ab_SL, unknown = 0)

# #replace NA with 0
# JUV[is.na(JUV)] <- 0
# summary(JUV)

JUV<-JUV[1:6]
#remove NA's
JUV<-JUV[complete.cases(JUV[,5]),]
summary(JUV)
JUV$String<-as.factor(JUV$String)
JUV$Plate<-as.factor(JUV$Plate)
# SORTING OUT THE DATES SO THEY ARE IN USEFUL FORMAT
#JUV$Date<-as.Date(JUV$Date, origin, format="%d/%m/%y")
JUV$newDate<-strptime(as.character(JUV$Date), "%d/%m/%Y")
JUV$newDate<-format(JUV$newDate, "%d/%b/%Y")
JUV$newDate<-as.Date(JUV$newDate, format="%d/%b/%Y")



# Betsey<-droplevels(subset(JUV, Location== "BI" & String== "2"))
# Betsey10<-droplevels(subset(Betsey, Plate >=5))
# Betsey10<-droplevels(subset(Betsey10, Plate <=7))

# #remove NAs
# Betsey10[complete.cases(Betsey10[,5]),]


ggplot(JUV, aes(x=Ab_SL, color=Location))+
  geom_histogram(stat = "bin", colour="grey", binwidth = 5)+
  scale_x_continuous(limits=c(0, 150))+
  
  xlab("Shell Length (mm)") + ylab("Frequency") +
  theme_bw()

ggplot(JUV, aes(x=Ab_SL)) + 
  ylab("Frequency") +
  xlab("Shell Length (mm)")+
  geom_histogram(alpha = 0.2, binwidth = 5)+
  #ggtitle(paste(dum$SubBlockNo, FishYear))+
  #labs(title= Yeardum$SubBlockNo, size=10)+
  #geom_histogram(binwidth=50)+
  theme_bw()+
  facet_grid(. ~ Location)

#Ab_count <- ddply(JUV, "Date", summarise, Ab_ct = length(Ab_SL))

#rearregage DF giving number of juveniles by location, date and String.
Pick<-aggregate(x = JUV, by = list(JUV$Location, JUV$newDate, JUV$String), FUN = "length")
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
#rearregage DF giving number of juveniles by location and date.
BI<-droplevels(subset(Pick, Pick$Site=="BI"))
BI$Date<-as.factor(BI$Date)

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
 # scale_x_discrete(limits=c("27 Jun", "28 Jul", "18 Aug", "25 Sep", "08 Oct", "09 Oct"))

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

