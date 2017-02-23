# Version 10/11/2016

rm(list=ls(all=TRUE))
## SET THE WORKING AND RESULTS DIRECTORIES
myWorkDrive <- "D:/"  ## Craig and Hugh


myWorkFolder <- "R_Stuff/SAM/Logistic"
myWorkPath <- paste(myWorkDrive,myWorkFolder,sep="")
setwd(myWorkPath)

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

load ("D:/R_Stuff/SAM/Logistic/SamResults131016.RData")

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
SamResults$LML[SamResults$BlockNo %in% c(seq(32, 38,1), seq(50,57,1))] <- 114
SamResults$LML[SamResults$BlockNo %in% c(seq(41,46,1))]<-110
SamResults$LML[SamResults$BlockNo %in%  c(47,48) |SamResults$SubBlockNo %in% c("49A", "49B","49C")] <- 120

#Ld bootstrap range
SamResults$Ld50BootRange<-SamResults$Ld50BootU95-SamResults$Ld50BootL95
SamResults$Ld75BootRange<-SamResults$Ld75BootU95-SamResults$Ld75BootL95
SamResults$Ld85BootRange<-SamResults$Ld85BootU95-SamResults$Ld85BootL95
SamResults$Ld90BootRange<-SamResults$Ld90BootU95-SamResults$Ld90BootL95
SamResults$Ld95BootRange<-SamResults$Ld95BootU95-SamResults$Ld95BootL95

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
SamFilter<-droplevels(subset(SamResults, SIT_Id != 172))

SamFilter<-rbind(SamFilter, DT_out)

# save(SamResults, file="SamResults.Rdata")
# write.csv(SamResults, file='SamResultsLatLong.csv')

rm(choice, DT_MSc, DT_out, pick)

#remove duplicate records

SamFilter<-SamFilter[!duplicated(SamFilter[,1]),]

ddply(SamFilter,.(Zone), summarize,  n = length(SiteCode), 
      mn.LD50 = mean(LD50, na.rm=T), mn.LCI50 = mean(Ld50BootL95, na.rm=T), mn.UCI50 = mean(Ld50BootU95, na.rm=T)
      , LD50Range = mean(Ld50BootRange, na.rm=T))

# # LOAD IL_SAM_GIS match DATA
GIS.IL<-read.csv('D:/Fisheries Research/Abalone/SAM/IL_GIS_291016.csv', header =T)
# 
# 
#load('D:/R_Stuff/SAM/Logistic/SamFilter131016.RData')
# 
SamFilterIL<-join(SamFilter, GIS.IL, by='SiteCode')


#recode database subblock errors
SamFilterIL$SubBlockNo[SamFilterIL$SubBlockNo==11] <- "11A"
keep(SamFilterIL, SamFilter, SamResults, GIS.IL, sure=T)

# add % under Lm 50
SamFilterIL$PctL50<-SamFilterIL$N.underLD50/SamFilterIL$n*100

# add % under LM05
SamFilterIL$PctLM05<-SamFilterIL$N.underLD05/SamFilterIL$n*100

# % I  and % M
SamFilterIL$Pct.I<-SamFilterIL$I/SamFilterIL$n*100
SamFilterIL$Pct.M<-SamFilterIL$M/SamFilterIL$n*100

# % IQR
SamFilter$PctIQR<-SamFilter$N.IQR/SamFilter$n*100

#-------------------------------
#FILTERS
#-------------------------------
#remove LD50 complete cases.
which( colnames(SamFilterIL)=="Ld50BootRange" )
SamFilterIL<-SamFilterIL[complete.cases(SamFilterIL[,42]),]

#remove outliers
q975<-quantile(SamFilterIL$Ld50BootRange, 0.975, na.rm=T)
SamFilterIL<-subset(SamFilterIL, Ld50BootRange <=q975)
#remove outliers
pick <- which(SamFilterIL$Pct.M <85 & SamFilterIL$Ld50BootRange >10)
SamFilterIL <- SamFilterIL[-pick,]
#remove outliers
pick <- which(SamFilterIL$Pct.M <30) 
SamFilterIL <- SamFilterIL[-pick,]


#Build in cuts to bin data by 5% in % mature
brks<-seq(30,100,by=5)
SamFilterIL$Pct.M.cut<-cut(SamFilterIL$Pct.M, 
                           breaks = seq(30,100,by=5), 
                           labels = brks[-length(brks)] + diff(brks), 
                           right = FALSE)


#anova L50 by Zone
boxcox(SamFilterIL$Ld50BootRange~SamFilterIL$Pct.M.cut)
fit<-aov(log(Ld50BootRange)~Pct.M.cut, data=SamFilterIL)
anova(fit)
par(mfrow = c(2,2))
plot(fit)
par(mfrow = c(1,1))
tHSDlm<- TukeyHSD(fit, ordered = FALSE, conf.level = 0.95)
tHSDlm

#set working dataframe for Tukey label function
ASM<-SamFilterIL


source("D:/GitCode/AbResearch/SAMTechReportFigures.R") # source of mean boxplots in ggplots


ggplot(SamFilterIL, aes(x=Pct.M.cut, y=Ld50BootRange)) + 
 xlab("% mature") + ylab(bquote(~CI['Range']~'LM'['50%']))+ #geom_boxplot(outlier.colour = "black", outlier.size = 3)+
 stat_summary(fun.data = min.mean.sd.max, geom = "boxplot", outlier.colour = "black", outlier.size = 3)+ 
 stat_summary(fun.y=mean, colour="black", geom="point", size=2)+
 theme_bw()+#white background
 theme(legend.position="none",
       axis.title.x = element_text(size=14),
       axis.text.x  = element_text(size=14),
       axis.title.y = element_text(size=14),
       axis.text.y  = element_text(size=14))+
 geom_text(data = generate_label_df(tHSDlm, "Pct.M.cut"), aes(x = plot.labels, y = 1, label = labels))

#Keep a copy for figures before final filter
SAMPreFilter<-SamFilterIL
#remove top and bottom 5%
pick <- which(SamFilterIL$Pct.M >85)
SamFilterIL <- SamFilterIL[-pick,]


SamFilterIL$Pct.M.cut<-NULL
rm(ASM)

#save Rfile
outFile <- paste(myWorkPath,"/SAM_FILTER_",format(Sys.time(), "%Y-%m-%d"),".RData",sep ="")
save.image(file = outFile)

# 
# # 
# # mean(SamFilterIL$Ld50BootRange)
# range(SamFilterIL$Ld50BootRange)
# ggplot(data = SamFilterIL, aes(x=Pct.M,  y=Ld50BootRange)) +
#  geom_point()+
#  xlab("% mature") + ylab(bquote(~CI['Range']~'LM'['50%']))+
#  # geom_hline(yintercept=median(SamFilterIL2$Ld50BootRange), colour = 'red', linetype= 3, size=1.5)+
#  # geom_hline(yintercept=mean(SamFilterIL2$Ld50BootRange), colour = 'green', linetype= 3, size=1.5)+
#  #
#  # geom_hline(yintercept=SAMLd50SD, colour = 'blue', linetype= 3, size=1.5)+
#   theme_bw()+
#  theme(legend.position=c(0.9, 0.8))+
#  theme(legend.title=element_blank())+
#  theme(legend.text = element_text(size=14))+
#  theme(axis.title.x = element_text(size=14),
#        axis.text.x  = element_text(size=14))+
#  theme(axis.title.y = element_text(size=14),
#        axis.text.y  = element_text(size=14))
# 
# 
