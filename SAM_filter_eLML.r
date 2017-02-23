# Version 05/06/2014  

rm(list=ls(all=TRUE))
## SET THE WORKING AND RESULTS DIRECTORIES
wkdir <- "D:/R_Stuff/SAM"
setwd(wkdir)


library(car)
library(MASS)
library(boot)
library(dplyr)
library(plyr)
library(gdata)
library(ggplot2)
library(multcompView)
library(devtools)
#sources

load ('D:/R_Stuff/SAM/SAM2016_April.RData') #SAM2016_April
# keep(SamResults, sure =T)

#rename columns
#SamResults<-rename(SamResults, c("SIT_Latitude"="Latitude", "SIT_Longitude"="Longitude", "SIT_StatBlock"="BlockNo", "SIT_SubBlock"="SubBlockNo"))
names(SamResults)[names(SamResults)=='SIT_Latitude']<-"Latitude"
names(SamResults)[names(SamResults)=='SIT_Longitude']<-"Longitude"
names(SamResults)[names(SamResults)=='SIT_StatBlock']<-"BlockNo"
names(SamResults)[names(SamResults)=='SIT_SubBlock']<-"SubBlockNo"

#Add columns for sample size (n), and % of sample <L05 <L50 and >L95
SamResults$n<-SamResults$I+SamResults$M
SamResults$PctL05<-SamResults$N.underLD05/SamResults$n*100
SamResults$PctL50<-SamResults$N.underLD50/SamResults$n*100
SamResults$PctL95<-SamResults$N.overLD95/SamResults$n*100

#add column for year of sample
SamResults$FishYear<-format(SamResults$SAM_Date,'%Y')
SamResults$FishYear<-as.factor(SamResults$FishYear)

#Reformat SublockNo
SamResults$SubBlockNo<-paste(SamResults$BlockNo,SamResults$SubBlockNo, sep="")

#Add zone 
SamResults$Zone[SamResults$BlockNo %in%  c(seq(14,30,1)) | SamResults$SubBlockNo %in% c("13C", "13D", "13E", "31A")] <- "E"
SamResults$Zone[SamResults$BlockNo %in%  c(seq(7,12,1)) | SamResults$SubBlockNo %in% c("13A", "13B", "06D", "6D")] <- "W"
SamResults$Zone[SamResults$SubBlockNo %in% c("6A","6B", "6C")] <- "CW"
SamResults$Zone[SamResults$SubBlockNo %in% c("5A", "5B", "5C")] <- "N"
SamResults$Zone[SamResults$BlockNo %in%  c(1, 2, 3, 4,47, 48, 49,39, 40) | SamResults$SubBlockNo %in% c("31B")] <- "N" 
SamResults$Zone[SamResults$BlockNo %in% c(seq(32, 38,1),seq(41,46,1), seq(50,57,1))] <- "BS"

#Add LML 
SamResults$LML[SamResults$BlockNo %in%  c(14:26,29:30) | SamResults$SubBlockNo %in% c("13C", "13D", "13E", "31A")] <- 138
SamResults$LML[SamResults$BlockNo %in%  c(seq(7,12,1)) | SamResults$SubBlockNo %in% c("13A", "13B", "06D", "6D")] <- 140
SamResults$LML[SamResults$SubBlockNo %in% c("6A","6B", "6C", "5D")] <- 132
SamResults$LML[SamResults$BlockNo %in%  c(1, 2, 3, 4,39, 40) |SamResults$SubBlockNo %in% c("5A", "5B", "5C", "31B", "49D")] <- 127
SamResults$LML[SamResults$BlockNo %in% c("27", "28")] <- 145
SamResults$LML[SamResults$BlockNo %in% c(seq(32, 38,1),seq(41,46,1), seq(50,57,1))] <- 110
SamResults$LML[SamResults$BlockNo %in%  c(47,48) |SamResults$SubBlockNo %in% c("49A", "49B","49C")] <- 120


#Ld bootstrap range
SamResults$Ld50BootRange<-SamResults$Ld50BootU95-SamResults$Ld50BootL95
SamResults$Ld95BootRange<-SamResults$Ld95BootU95-SamResults$Ld95BootL95

#remove files which break metarules of <5% or >95% mature
pick <- which(SamResults$PctL05 <5)
SamResults <- SamResults[-pick,]
SamResults <- droplevels(SamResults)
pick <- which(SamResults$PctL95 <5)
SamResults <- SamResults[-pick,]
SamResults <- droplevels(SamResults)

#DT Seasonal SAM MSc used the same sites for SAM work between 1999-2001. There is some concern that by resampling the same sites continuously for 
#for 2.5 years there might be effects on the L50 outside of those found at all other sites. Therefore all but the 1st samples in 1999 and 2000 are removed.

DT_MSc<-c(171, 172)
pick <- which(SamResults$SIT_Id == DT_MSc)
DT_MSc<-SamResults[pick,]

pick <- which(DT_MSc$FishYear == 2001)
DT_MSc<-DT_MSc[-pick,]
DT_MSc[order(as.Date(DT_MSc$SAM_Date, format="%d/%m/%Y")),]


Sites<-unique(DT_MSc$SIT_Name)
if (exists("DT_out")) 
  rm(DT_out)

for(d in Sites){
  choice<-subset(DT_MSc, SIT_Name == d)
  pick<-ddply(choice,.(FishYear),function(x) head(x,1))
  if (exists("DT_out"))
    DT_out <- rbind(DT_out, pick)
  else
    DT_out <- pick
}

#Remove all DT_MSc sites and replace with DT_out

SamFilter<-droplevels(subset(SamResults, SIT_Id != 171))
SamFilter<-droplevels(subset(SamFilter, SIT_Id != 172))

SamFilter<-rbind(SamFilter, DT_out)

# save(SamResults, file="SamResults.Rdata")
# write.csv(SamResults, file='SamResultsLatLong.csv')

rm(choice, DT_MSc, DT_out, pick)

#remove duplicate records

SamFilter<-SamFilter[!duplicated(SamFilter[,1]),]

#Add SamNIQR to SamFilter
load('D:/R_Stuff/SAM/Logistic/SamN_IQR.RData')
SamFilter<-left_join(SamFilter, SamNIQR, by = "SiteCode") 
SamFilter$PctIQR<-SamFilter$N.IQR/SamFilter$n*100


ddply(SamResults,.(Zone), summarize,  n = length(SiteCode), 
      mn.LD50 = mean(LD50, na.rm=T), mn.LCI50 = mean(Ld50BootL95, na.rm=T), mn.UCI50 = mean(Ld50BootU95, na.rm=T)
      , LD50Range = mean(Ld50BootRange, na.rm=T))

keep(SamResults, SamFilter, sure =T)

#Filter by CIrange to mean of dataset
L50RangeMean<-mean(SamFilter$Ld50BootRange, na.rm=T)

pick <- which(SamFilter$Ld50BootRange <= L50RangeMean)
SamFilterCI <- SamFilter[pick,]
SamFilterCI <- droplevels(SamFilterCI)


