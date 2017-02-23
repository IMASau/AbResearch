# Version 05/06/2014  

rm(list=ls(all=TRUE))
## SET THE WORKING AND RESULTS DIRECTORIES
wkdir <- "D:/R_Stuff/SAM"
setwd(wkdir)

## Load raw csv file
#infile <- "Basic Sam Query.csv"

library(car)
library(MASS)
library(boot)
library(dplyr)
library(plyr)
library(gdata)
library(ggplot2)
library(multcompView)
#sources
source("D:/GitCode/r-AbSpatialAnalyses/GraphsUtils.r")

source("D:/GitCode/AbResearch/SAM_utils_TechReport.R")


# load D:\R_Stuff\SAM    SAM2016_April  SAM_TechReport230816.RData
keep(SamResults, sure =T)

#rename columns
colnames(SamResults)[21] <- "Latitude"
colnames(SamResults)[22] <- "Longitude"
colnames(SamResults)[23] <- "BlockNo"
colnames(SamResults)[24] <- "SubBlockNo"

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

DT_MSc<-c("Sterile Island, tag site", "Actaeon Island, tag site")
pick <- which(SamResults$SIT_Name == DT_MSc)
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
DT_MSc<-c("Sterile Island, tag site", "Actaeon Island, tag site")
pick <- which(SamResults$SIT_Name == DT_MSc)
SamResults<-SamResults[-pick,]

SamResults<-rbind(SamResults, DT_out)

save(SamResults, file="SamResults.Rdata")
write.csv(SamResults, file='SamResultsLatLong.csv')


#remove duplicate records

SamResults<-SamResults[!duplicated(SamResults[,1]),]


BlockSumStats<-ddply(SamResults,.(BlockNo, Zone), summarize,  n = length(SiteCode), 
                        mn.L50 = mean(LD50, na.rm=T), mn.LCI50 = mean(Ld50BootL95, na.rm=T), mn.UCI50 = mean(Ld50BootU95, na.rm=T),
                        mn.L95 = mean(LD95, na.rm=T), mn.LCI95 = mean(Ld95BootL95, na.rm=T), mn.UCI95 = mean(Ld95BootU95, na.rm=T),
                        mn.IQR = mean(IQR, na.rm=T), sd.IQR = sd(IQR, na.rm=T),
                        mn.pct.L50 = mean(PctL50, na.rm=T), sd.pct.L50 = mean(PctL50, na.rm=T),
                        mn.Bootrange.L50 = mean(Ld50BootRange, na.rm=T), sd.Bootrange.L50 = mean(Ld50BootRange, na.rm=T))
                        #mn.MLStc = mean(MLStc, na.rm=T) , sd.MLStc = sd(MLStc, na.rm=T))
write.csv(BlockSumStats, file='SAMBlockSummaryStats.csv')

