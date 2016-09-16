#use SamFilter as precusror to this file 
# use SamFilter030816.RData 
#   package mirrors if install fails install.packages('package name', repo='http://cran.csiro.au/')

library(car)
library(MASS)
library(boot)
library(dplyr)
library(plyr)
library(gdata)
library(ggplot2)
library(multcompView)
library(devtools)
library(gdata)
library(reshape)
library(reshape2)
library(lubridate)
#######RBIND ALL SUBSETS (function below for unmatch columns)

rbind.match.columns <- function(input1, input2) {
  n.input1 <- ncol(input1)
  n.input2 <- ncol(input2)
  
  if (n.input2 < n.input1) {
    TF.names <- which(names(input2) %in% names(input1))
    column.names <- names(input2[, TF.names])
  } else {
    TF.names <- which(names(input1) %in% names(input2))
    column.names <- names(input1[, TF.names])
  }
  
  return(rbind(input1[, column.names], input2[, column.names]))
}

#################################################################

keep(SamFilter, SamResults, sure=T)

#recode database subblock errors
SamFilter$SubBlockNo[SamFilter$SubBlockNo==11] <- "11A"


SamFilterIL<-SamFilter

# #================================================================
# #
# #                         ADD IN SL DATA from raw SAM database file to SAMfilter
# #
# #================================================================


infileNew <- "D:/R_Stuff/SAM/Logistic/SAMExport2016.csv"
BlckPopNew <- read.csv(infileNew, header=TRUE, sep=',', dec='.', as.is=TRUE)
BlckPopNew$SAM_Date <- as.Date(strptime(as.character(BlckPopNew$SAM_Date), "%d/%m/%Y", tz='AUSTRALIA/HOBART'))
BlckPopNew$SiteCode <- paste(BlckPopNew$SIT_Id,'_',year(BlckPopNew$SAM_Date),'_',month(BlckPopNew$SAM_Date), sep="")
BlckPopNew <- BlckPopNew[order(BlckPopNew$SiteCode,BlckPopNew$SPC_ShellLength),]

BlckPop <- BlckPopNew

SiteCodes<-unique(SamFilter$SiteCode)
pick <- which(BlckPop$SiteCode %in% SiteCodes)
BlckPopFilter<-BlckPop[pick,]

names(BlckPopFilter)[names(BlckPopFilter)=='SIT_StatBlock']<-"BlockNo"
names(BlckPopFilter)[names(BlckPopFilter)=='SIT_SubBlock']<-"SubBlockNo"
#Reformat SublockNo
BlckPopFilter$SubBlockNo<-paste(BlckPopFilter$BlockNo,BlckPopFilter$SubBlockNo, sep="")

SLSubBlockSum<-ddply(BlckPopFilter,.(SubBlockNo), summarize,  
                  SLmax = max(SPC_ShellLength, na.rm=T), SLq95 = quantile(SPC_ShellLength, 0.95, na.rm=T))
#
#Match SLBlockSum to SamFilter by subblock
#
SamFilterIL<-left_join(SamFilterIL,SLSubBlockSum, by = 'SubBlockNo')
rm(BlckPop, BlckPopNew, SLSubBlockSum)


#drop SAM observations with odd low LD50
OddLook<-subset(SamFilterIL, LD50 < 100 & SLmax > 183)
OddSiteCodes<-unique(OddLook$SiteCode)
pick <- which(SamFilterIL$SiteCode %in% OddSiteCodes)
SamFilterIL<-SamFilterIL[-pick,]


setwd("D:/Fisheries Research/Abalone/SAM")
IL.info<-read.csv("Inv.Log.Data.csv", header = T)
summary(IL.info)

names(IL.info)[names(IL.info)=='Site_code']<-"Growth_Id"
names(IL.info)[names(IL.info)=='Site.ID']<-"SIT_Id"
names(IL.info)[names(IL.info)=='Site']<-"Site_Growth"
#Add zone 
IL.info$Zone[IL.info$BlockNo %in%  c(seq(14,30,1)) | IL.info$SubBlockNo %in% c("13C", "13D", "13E", "31A")] <- "E"
IL.info$Zone[IL.info$BlockNo %in%  c(seq(7,12,1)) | IL.info$SubBlockNo %in% c("13A", "13B", "06D", "6D")] <- "W"
IL.info$Zone[IL.info$SubBlockNo %in% c("6A","6B", "6C")] <- "CW"
IL.info$Zone[IL.info$SubBlockNo %in% c("5A", "5B", "5C")] <- "N"
IL.info$Zone[IL.info$BlockNo %in%  c(1, 2, 3, 4,47, 48, 49,39, 40) | IL.info$SubBlockNo %in% c("31B")] <- "N" 
IL.info$Zone[IL.info$BlockNo %in% c(seq(32, 38,1),seq(41,46,1), seq(50,57,1))] <- "BS"

IL.info<-droplevels(subset(IL.info, SIT_Id != "480"))
IL.info<-droplevels(subset(IL.info, SIT_Id != "266"))
IL.info<-droplevels(subset(IL.info, SIT_Id != "349"))

#match SAM data to Growth data by site ID
SAM.IL<-left_join(SamFilterIL,IL.info[,4:9], by = 'SIT_Id')
Unpick<-subset(SAM.IL, is.na(MaxDL))
Unpick<-Unpick[,1:34]
SIT_ID<-subset(SAM.IL, !is.na(MaxDL))
rm(SAM.IL)
IL<-unique(SIT_ID$SIT_Id)


#LM 
boxcox(SIT_ID$LD50~SIT_ID$L50)
fit<-lm(L50~LD50, data=SIT_ID)
summary(fit)
anova(fit)
par(mfrow = c(2,2))
plot(fit)
par(mfrow = c(1,1))

# view the data l50 by ld50
ggplot(data = SIT_ID, aes(x=L50,  y=LD50)) + 
  geom_point()+
  xlab(bquote(''~LM['50%']~'(mm)')) + ylab(bquote(''~L['50']~'(mm)'))+
  geom_smooth(method=lm, se=F, fill='Black', fullrange=F, size=1.2, color='black')+
  #ggtitle(paste(dum$SubBlockNo, FishYear))+
  #labs(title= Yeardum$SubBlockNo, size=10)+
  #geom_histogram(binwidth=50)+
  theme_bw()+
  scale_color_identity()+ #this makes sure the color follows the color argument above in aes()
  theme(legend.position=c(0.9, 0.8))+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(size=14))+
  theme(axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=14))+
  theme(axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=14))

ggplot(data = SIT_ID, aes(x=SLmax,  y=LD50)) + 
 geom_point()+
 xlab(bquote(''~MaxSL['']~'(mm)')) + ylab(bquote(''~L['50']~'(mm)'))+
 geom_smooth(method=lm, se=F, fill='Black', fullrange=F, size=1.2, color='black')+
 #ggtitle(paste(dum$SubBlockNo, FishYear))+
 #labs(title= Yeardum$SubBlockNo, size=10)+
 #geom_histogram(binwidth=50)+
 theme_bw()+
 scale_color_identity()+ #this makes sure the color follows the color argument above in aes()
 theme(legend.position=c(0.9, 0.8))+
 theme(legend.title=element_blank())+
 theme(legend.text = element_text(size=14))+
 theme(axis.title.x = element_text(size=14),
       axis.text.x  = element_text(size=14))+
 theme(axis.title.y = element_text(size=14),
       axis.text.y  = element_text(size=14))


#
fitcoef<-coef(fit[1])
IL.info$LD50T<-(fitcoef[1]+fitcoef[2]*IL.info$L50)

#drop SAM observations with odd low LD50
OddLook<-subset(SIT_ID, LD50 < 110 & SLmax > 180)
OddSiteCodes<-unique(OddLook$SiteCode)
pick <- which(SIT_ID$SiteCode %in% OddSiteCodes)
SIT_ID<-SIT_ID[-pick,]

#  Load package
#require( data.table )

# #  Make data.frames into data.tables with a key column
# 
# unpickdt <- data.table(Unpick , key = list("LD50","SLmax"))
# 
# SIT_IDdt <- data.table(SIT_ID , key = list("LD50","SLmax"))
# 
# #  Join based on the key column of the two tables (time & time1)
# #  roll = "nearest" gives the desired behaviour
# #  list( obs , time1 , temp ) gives the columns you want to return from dt
# test<-SIT_IDdt[ unpickdt , list(SiteCode) , roll = "nearest" , by=list()]
# 
# #Function from solution
# func = function(x,v){
#   vec = with(SIT_IDdt, which.min(abs(x - LD50)) + which.min(abs(v - LD50)))
#   unpickdt[which.min(vec),]$Value
# }
# 
# test = transform(SIT_IDdt, Value=apply(SIT_IDdt, 1, function(u) func(u[1], u[2])))
# 
# #================================================================
# #
# #                         Growth data allocation with zone 
# #
# #================================================================
#############################
UnpickBS<-droplevels(subset(Unpick, Zone == 'BS'))
UnpickZ<-droplevels(subset(Unpick, Zone != 'BS'))

Zones<-unique(Unpick$Zone)
#####
if (exists("SAMjoin")) 
 rm(SAMjoin)

for(z in Zones){
 Samchoice<-subset(Unpick, Zone == z)
 LIchoice<-subset(IL.info, Zone == z)
 Samchoice$match<-sapply(Samchoice$LD50,function(x)which.min(abs(x - LIchoice$LD50T)))
 Samchoice$match<-as.numeric(Samchoice$match)
 LIchoice$match<-1:nrow(LIchoice)
 
 Zonejoin<-left_join(Samchoice,LIchoice[,c(4:5,7:9,12:13)], by = 'match')
 
        if (exists("SAMjoin"))
  SAMjoin <- rbind(SAMjoin, Zonejoin)
 else
  SAMjoin <- Zonejoin
}
names(SAMjoin)[names(SAMjoin)=='LD50.x']<-"LD50"
#names(SAMjoin)[names(SAMjoin)=='LD50.y']<-"GrowthLD50"
names(SAMjoin)[names(SAMjoin)=='SLmax.y']<-"SLmax.Growth"
names(SAMjoin)[names(SAMjoin)=='SLq95.y']<-"SLq95.Growth"
names(SAMjoin)[names(SAMjoin)=='SLmax.x']<-"SLmax"
names(SAMjoin)[names(SAMjoin)=='SLq95.x']<-"SLq95"


#TREAT BSZ grwth data as mean values as no direct matches by SIT_ID
BS.IL.Info<-droplevels(subset(IL.info, Zone == 'BS'))
UnpickBS$match<-sapply(UnpickBS$BlockNo,function(x)which.min(abs(x - BS.IL.Info$BlockNo)))
BS.IL.Info$match<-1:nrow(BS.IL.Info)
BSjoin<-left_join(UnpickBS,BS.IL.Info[,c(4:5, 7:9, 12)], by = 'match')

BSjoin$GrowthLD50<-NA
BSjoin$SLmax.Growth<-NA
BSjoin$SLq95.Growth<-NA

SAMjoin<-rbind.match.columns(SAMjoin, BSjoin)

#  Match by all sites
#  
# Unpick$match<-sapply(Unpick$LD50,function(x)which.min(abs(x - SIT_ID$LD50)))
# 
# SIT_ID$match<-c(1:28)
# SAMjoin<-left_join(Unpick,SIT_ID[,c(6,33:40)], by = 'match')
# names(SAMjoin)[names(SAMjoin)=='LD50.x']<-"LD50"
# names(SAMjoin)[names(SAMjoin)=='LD50.y']<-"GrowthLD50"
# names(SAMjoin)[names(SAMjoin)=='SLmax.y']<-"SLmax.Growth"
# names(SAMjoin)[names(SAMjoin)=='SLq95.y']<-"SLq95.Growth"
# names(SAMjoin)[names(SAMjoin)=='SLmax.x']<-"SLmax"
# names(SAMjoin)[names(SAMjoin)=='SLq95.x']<-"SLq95"

#LM LM50 to Grwoth LM50
boxcox(SAMjoin$LD50^3.2~SAMjoin$GrowthLD50)
fit<-lm(LD50^3.2~GrowthLD50, data=SAMjoin)
summary(fit)
anova(fit)
par(mfrow = c(2,2))
plot(fit)
par(mfrow = c(1,1))

ggplot(data = SAMjoin, aes(x=LD50,  y=LD50T)) + 
  geom_point()+
  xlab(bquote(''~LM['50%']~'(mm)')) + ylab(bquote('Growth Site'~LM['50']~'(mm)'))+
  geom_smooth(method=lm, se=F, fill='Black', fullrange=F, size=1.2, color='black')+
  #ggtitle(paste(dum$SubBlockNo, FishYear))+
  #labs(title= Yeardum$SubBlockNo, size=10)+
  #geom_histogram(binwidth=50)+
  theme_bw()+
  scale_color_identity()+ #this makes sure the color follows the color argument above in aes()
  theme(legend.position=c(0.9, 0.8))+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(size=14))+
  theme(axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=14))+
  theme(axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=14))

#A look at the allocations
Sites<-SAMjoin[,c(6,18,22,29,34:42)]

SAMILResults<-rbind.match.columns(SIT_ID,SAMjoin)


# 
# 
ggplot(data = SAMjoin, aes(x=SLmax,  y=SLmax.Growth)) +
  geom_point()+
  xlab('Max SL (mm)') + ylab('Growth Site Max SL (mm)')+
  geom_smooth(method=lm, se=F, fill='black', fullrange=F, size=1.2, color='black')+
  #ggtitle(paste(dum$SubBlockNo, FishYear))+
  #labs(title= Yeardum$SubBlockNo, size=10)+
  #geom_histogram(binwidth=50)+
  theme_bw()+
    theme(legend.position=c(0.9, 0.2))+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(size=14))+
  theme(axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=14))+
  theme(axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=14))
# 
# 
# #Fit LM50 to l50
# boxcox(SAMjoin$SLmax^3~SAMjoin$SLmax.Growth)
# fit<-lm(LD50^3~L50, data=SAMjoin)
# summary(fit)
# anova(fit)
# par(mfrow = c(2,2))
# plot(fit)
# par(mfrow = c(1,1))
# 
# 
# 
# #Fit LM50 to l50
# boxcox(SAMjoin$LD50^3~SAMjoin$L50)
# fit<-lm(LD50^3~L50, data=SAMjoin)
# summary(fit)
# anova(fit)
# par(mfrow = c(2,2))
# plot(fit)
# par(mfrow = c(1,1))
# 
# ggplot(data = SAMjoin, aes(x=LD50,  y=L50)) + 
#   geom_point()+
#   xlab(bquote(''~LM['50%']~'(mm)')) + ylab(bquote(''~L['50']~'(mm)'))+
#   geom_smooth(method=lm, se=F, fill='Black', fullrange=F, size=1.2, color='black')+
#   #ggtitle(paste(dum$SubBlockNo, FishYear))+
#   #labs(title= Yeardum$SubBlockNo, size=10)+
#   #geom_histogram(binwidth=50)+
#   theme_bw()+
#   scale_color_identity()+ #this makes sure the color follows the color argument above in aes()
#   theme(legend.position=c(0.9, 0.8))+
#   theme(legend.title=element_blank())+
#   theme(legend.text = element_text(size=14))+
#   theme(axis.title.x = element_text(size=14),
#         axis.text.x  = element_text(size=14))+
#   theme(axis.title.y = element_text(size=14),
#         axis.text.y  = element_text(size=14))




# #================================================================
# #
# #                         Alternative method for alloctaions
# #
# #================================================================

#Below is the initial method for allocating the grwoth data by a step down process :
#1.	Match by SIT_ID 
#2.	Match by Site name (e.g. 'Black Reef, east side in shallows' is allocated 'Black Reef Tag data')
#3.	Match by Subblock
#4.	Match by Block
#5.	Match by Block + 1 (e.g. no growth data for block 29 use growth data in block 30)

#This is only partial complete as it was abandoned based on poor matches of LM50 to L50 which was choosen as the preferred method
# 
# #================================================================
# #
# #                           EAST
# #
# #================================================================
# East.IL<-droplevels(subset(IL.info, Zone == 'E'))
# East.SamF<-droplevels(subset(Unpick, Zone == 'E'))
# 
# #++++++++++++++++++++++++++++++++++++++++
# #  Dealing with mutliple growth data allocations in subblocks
# #++++++++++++++++++++++++++++++++++++++++
# SubBlockDupes<-c('13D', '13E', '14A', '16D')
# East_SBDupes_IL<-droplevels(subset(East.IL, SubBlockNo %in% SubBlockDupes))
# East_SBDupes_IL<-East_SBDupes_IL[!duplicated(East_SBDupes_IL[,12]),]
# 
# #++++++++++++++++++++++++++++++++++++++++
# # Take the unmatched obs from East.Unpick and extract all obs from SubBlockDupes
# East_SBDupes_SAM<-droplevels(subset(East.SamF, SubBlockNo %in% SubBlockDupes))
# 
# #new column with start of site name
# 
# East_Sitebind<-left_join(East_SBDupes_SAM,East_SBDupes_IL[,c(4:5,7:9,12)], by = 'Part_Name')
# East.Unpick<-subset(East_Sitebind, is.na(MaxDL))
# East.Unpick<-East.Unpick[,1:32]
# 
# #       OUTPUT  1
# East_Sitebind<-subset(East_Sitebind, !(is.na(MaxDL)))
# 
# SAM_Out<-rbind.match.columns(SIT_ID, East_Sitebind)
# 
# 
# #++++++++++++++++++++++++++++++++++++++++
# # Take the unmatched obs from East.Unpick and extract 13D and 13E for remaining SAM in those subblocks
# Site_IDs<-c(815, 879) # this selects appropriate IL from 13D and 13E for remaining SAM in those subblocks
# East_Site_ID_IL<-droplevels(subset(East.IL, SIT_Id %in% Site_IDs))
# 
# East_SiteID<-left_join(East.Unpick,East_Site_ID_IL[,c(3:5, 7:9)], by = 'SubBlockNo')
# 
# East.Unpick<-subset(East_SiteID, is.na(MaxDL))
# East.Unpick<-East.Unpick[,1:32]
# 
# #       OUTPUT  3
# East_ID<-subset(East_SiteID, !(is.na(MaxDL)))
# names(East_ID)[names(East_ID)=='SIT_Id.x']<-"SIT_Id"
# 
# 
# #++++++++++++++++++++++++++++++++++++++++
# # Take the unmatched obs from blocks 14 and 16 and use average grwoth in those subblocks
# ###########
# sb<-c(14,16)
# pick <- which(East_SBDupes_IL$BlockNo %in% sb)
# East.BLK<-East_SBDupes_IL[pick,]
# SB_mean<-ddply(East.BLK,.(BlockNo), summarize,  MaxDL = mean(MaxDL, na.rm=T), L50 = mean(L50), L95 = mean(L95))
# SB_mean$Growth_Id<-'BlockAvg'
# SB_mean$Site_Growth<-'Site_Growth'
# 
# #       OUTPUT  4
# East_dupes<-left_join(East.Unpick,SB_mean, by = 'BlockNo')
# 
# 
# #rm(SB_mean,East.Unpick,East_Site_ID_IL, Site_IDs)
# 
# #++++++++++++++++++++++++++++++++++++++++
# # Take the unmatched obs from east unmatched 2ith unique IL data but no site_ID match
# 
# pick <- which(East_UnMatched$SubBlockNo %in% SubBlockDupes)
# East_UnMatched<-East_UnMatched[-pick,] # remove all those subblocks already dealt with above
# East_UnMatched<-East_UnMatched[,1:32]
# 
# #match Growth and SAM by subblock
# East.SBMatch<-left_join(East_UnMatched,East.IL[,3:9], by = 'SubBlockNo')
# East.Unpick<-subset(East.SBMatch, is.na(MaxDL))
# East.Unpick<-East.Unpick[,1:32]
# 
# #       OUTPUT  5  - matched subblocks
# East.SBMatch<-subset(East.SBMatch, !(is.na(MaxDL)))
# names(East.SBMatch)[names(East.SBMatch)=='SIT_Id.x']<-"SIT_Id"
# 
# #match Growth and SAM by block
# East.BlkMatch<-left_join(East.Unpick,East.IL[,c(2,4:9)], by = 'BlockNo')
# East.Unpick<-subset(East.BlkMatch, is.na(MaxDL))
# East.Unpick<-East.Unpick[,1:32]
# 
# #       OUTPUT  6- matched blocks
# East.BlkMatch<-subset(East.BlkMatch, !(is.na(MaxDL)))
# names(East.BlkMatch)[names(East.BlkMatch)=='SIT_Id.x']<-"SIT_Id"
# 
# #++++++++++++++++++++++++++++++++++++++++
# # Take the unmatched obs from east East.Unpick and match by block +1 for closest growth data
# East.Unpick$Blk1<-East.Unpick$BlockNo+1
# East.BlkPlus1<-merge(East.Unpick,East.IL[,c(2,4:9)], by.x = c("Blk1"), by.y = c('BlockNo'))
# 
# #       OUTPUT  7
# East.BlkPlus1<-subset(East.BlkPlus1, !(is.na(MaxDL)))
# names(East.BlkPlus1)[names(East.BlkPlus1)=='SIT_Id.x']<-"SIT_Id"
# 
# 
# 
# SAM_Out<-rbind.match.columns(East_SIT_ID, East_Sitebind)
# SAM_Out<-rbind.match.columns(SAM_Out, East_ID)
# SAM_Out<-rbind.match.columns(SAM_Out, East_dupes)
# SAM_Out<-rbind.match.columns(SAM_Out, East.SBMatch)
# SAM_Out<-rbind.match.columns(SAM_Out, East.BlkMatch)
# SAM_Out<-rbind.match.columns(SAM_Out, East.BlkPlus1)
# 
# 
# SamOut<-rbind(East.BlkPlus1[,2:39], East.BlkMatch, East.SBMatch, East_dupes, East_ID, East_Sitebind[,-33], East_SIT_ID)
# SamOutEast<-SAM_Out[!duplicated(SAM_Out[,1]),]
# 
# keep(SamFilter, SamResults, SamOutEast, IL.info, sure=T)
# 
# #--------------------------------------------------END EAST
# 
# #================================================================
# #
# #                           WEST
# #
# #================================================================
# 
# W.IL<-droplevels(subset(IL.info, Zone == 'W'))
# W.SamF<-droplevels(subset(SamFilter, Zone == 'W'))
# 
# W.SAM.IL<-left_join(W.SamF,W.IL[,4:9], by = 'SIT_Id')
# # look at unallocated SiteCodes
# W.Unpick<-subset(W.SAM.IL, is.na(MaxDL))
# W.Unpick<-W.Unpick[,1:32]
# 
# W_UnMatched<-subset(W.SAM.IL, is.na(MaxDL)) # used in output 5 onwards
# 
# #       OUTPUT  1
# W_SIT_ID<-subset(W.SAM.IL, !is.na(MaxDL))
# 
# #++++++++++++++++++++++++++++++++++++++++
# #  Dealing with mutliple growth data allocations in subblocks
# #++++++++++++++++++++++++++++++++++++++++
# SubBlockDupes<-c('11A')
# W_SBDupes_IL<-droplevels(subset(W.IL, SubBlockNo %in% SubBlockDupes))
# 
# #++++++++++++++++++++++++++++++++++++++++
# # Take the unmatched obs from W.Unpick and extract all obs from SubBlockDupes
# W_SBDupes_SAM<-droplevels(subset(W.Unpick, SubBlockNo %in% SubBlockDupes))
# 
# #new column with start of site name
# W_SBDupes_SAM$Part_Name<-sapply(strsplit(W_SBDupes_SAM$SIT_Name, "\\ "), `[[`, 1)
# W_SBDupes_IL$Part_Name<-sapply(strsplit(as.character(W_SBDupes_IL$Site_Growth), "\\ "), `[[`, 1)
# 
# W_Sitebind<-left_join(W_SBDupes_SAM,W_SBDupes_IL[,c(4:5,7:9,12)], by = 'Part_Name')
# W.Unpick<-subset(W_Sitebind, is.na(MaxDL))
# W.Unpick<-W.Unpick[,1:32]
# 
# #       OUTPUT  2
# W_Sitebind<-subset(W_Sitebind, !(is.na(MaxDL)))
# 
# #++++++++++++++++++++++++++++++++++++++++
# # Manually assign growth within subblock 
# W.Unpick$Growth_Id<-c("GI", "GI", "GI","TR")
# 
# #       OUTPUT  3
# W_SiteID<-left_join(W.Unpick,W_SBDupes_IL[,c(4:5, 7:9)], by = 'Growth_Id')
# 
# #++++++++++++++++++++++++++++++++++++++++
# # Take the unmatched obs from W unmatched 2ith unique IL data but no site_ID match
# 
# pick <- which(W_UnMatched$SubBlockNo %in% SubBlockDupes)
# W_UnMatched<-W_UnMatched[-pick,] # remove all those subblocks already dealt with above
# W_UnMatched<-W_UnMatched[,1:32]
# 
# #match Growth and SAM by subblock
# W.SBMatch<-left_join(W_UnMatched,W.IL[,3:9], by = 'SubBlockNo')
# W.Unpick<-subset(W.SBMatch, is.na(MaxDL))
# W.Unpick<-W.Unpick[,1:32]
# 
# #       OUTPUT  5  - matched subblocks
# W.SBMatch<-subset(W.SBMatch, !(is.na(MaxDL)))
# names(W.SBMatch)[names(W.SBMatch)=='SIT_Id.x']<-"SIT_Id"
# 
# 
# # allocate 11E grwoth data from 11c
# W.pick<-subset(W.Unpick, SubBlockNo == '11E')
# W.pickIL<-subset(W.IL, SubBlockNo == '11C')
# 
# W.SB11E<-left_join(W.pick,W.pickIL[,c(2,4:9)], by = 'BlockNo')
# 
# 
# W.pick<-subset(W.Unpick, SubBlockNo != '11E')
# 
# 
# 
# #match Growth and SAM by block
# W.BlkMatch<-left_join(W.Unpick,W.IL[,c(2,4:9)], by = 'BlockNo')
# W.Unpick<-subset(W.BlkMatch, is.na(MaxDL))
# W.Unpick<-W.Unpick[,1:32]
# 
# #       OUTPUT  6- matched blocks
# W.BlkMatch<-subset(W.BlkMatch, !(is.na(MaxDL)))
# names(W.BlkMatch)[names(W.BlkMatch)=='SIT_Id.x']<-"SIT_Id"
# 
# #++++++++++++++++++++++++++++++++++++++++
# # Take the unmatched obs from W W.Unpick and match by block +1 for closest growth data
# W.Unpick$Blk1<-W.Unpick$BlockNo+1
# W.BlkPlus1<-merge(W.Unpick,W.IL[,c(2,4:9)], by.x = c("Blk1"), by.y = c('BlockNo'))
# 
# #       OUTPUT  7
# W.BlkPlus1<-subset(W.BlkPlus1, !(is.na(MaxDL)))
# names(W.BlkPlus1)[names(W.BlkPlus1)=='SIT_Id.x']<-"SIT_Id"
# 
# #######RBIND ALL SUBSETS
# IL_out<-rbind(Out1, Out2, Out3, Out4, Out5, Out6, Out7)
# 
# rbind.match.columns <- function(input1, input2) {
#   n.input1 <- ncol(input1)
#   n.input2 <- ncol(input2)
#   
#   if (n.input2 < n.input1) {
#     TF.names <- which(names(input2) %in% names(input1))
#     column.names <- names(input2[, TF.names])
#   } else {
#     TF.names <- which(names(input1) %in% names(input2))
#     column.names <- names(input1[, TF.names])
#   }
#   
#   return(rbind(input1[, column.names], input2[, column.names]))
# }
# 
# SAM_Out<-rbind.match.columns(W_SIT_ID, W_Sitebind)
# SAM_Out<-rbind.match.columns(SAM_Out, W_ID)
# SAM_Out<-rbind.match.columns(SAM_Out, W_dupes)
# SAM_Out<-rbind.match.columns(SAM_Out, W.SBMatch)
# SAM_Out<-rbind.match.columns(SAM_Out, W.BlkMatch)
# SAM_Out<-rbind.match.columns(SAM_Out, W.BlkPlus1)
# 
# 
# SamOut<-rbind(W.BlkPlus1[,2:39], W.BlkMatch, W.SBMatch, W_dupes, W_ID, W_Sitebind[,-33], W_SIT_ID)
# SamOutW<-SAM_Out[!duplicated(SAM_Out[,1]),]
# 
# keep(SamFilter, SamResults, SamOutW, sure=T)
# 
# #================================================================
# #
# #                           NORTH
# #
# #================================================================
# 
# N.IL<-droplevels(subset(IL.info, Zone == 'N'))
# N.SamF<-droplevels(subset(SamFilter, Zone == 'N'))
# 
# N.SAM.IL<-left_join(N.SamF,N.IL[,4:9], by = 'SIT_Id')
# # look at unallocated SiteCodes
# N.Unpick<-subset(N.SAM.IL, is.na(MaxDL))
# N.Unpick<-N.Unpick[,1:32]
# 
# N_UnMatched<-subset(N.SAM.IL, is.na(MaxDL)) # used in output 5 onwards
# 
# #       OUTPUT  1
# N_SIT_ID<-subset(N.SAM.IL, !is.na(MaxDL))
# 
