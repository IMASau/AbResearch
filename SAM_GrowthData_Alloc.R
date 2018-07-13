#use latest SamFilter as precusror to this file 
# currently SamFilter050916.RData 
library(car)
library(MASS)
library(boot)
library(dplyr)
library(plyr)
library(gdata)
library(ggplot2)
library(ggrepel)
library(multcompView)
library(devtools)
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


# LOAD SAM DATA
load('c:/CloudStor/R_Stuff/SAM/Logistic/SamFilter190916.RData ')
keep(SamFilterCI, SamFilter, SamResults, sure=T)
#recode database subblock errors
SamFilter$SubBlockNo[SamFilter$SubBlockNo==11] <- "11A"
SamFilterCI$SubBlockNo[SamFilterCI$SubBlockNo==11] <- "11A"

#
###### CHOOSE OPTION ########
SamFilterIL<-SamFilter
#SamFilterIL<-SamFilterCI
# #================================================================
# #                         ADD IN SL DATA from raw SAM database file to SAMfilter
# #================================================================

infileNew <- "c:/CloudStor/R_Stuff/SAM/Logistic/SAMExport2016.csv"
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
# OddLook<-subset(SamFilterIL, LD50 < 100 & SLmax > 183)
# OddSiteCodes<-unique(OddLook$SiteCode)
# pick <- which(SamFilterIL$SiteCode %in% OddSiteCodes)
# SamFilterIL<-SamFilterIL[-pick,]

# #================================================================
# #                         load Growth parameters
# #================================================================
load('c:/CloudStor/R_Stuff/SAM/Logistic/ILResults.RData')

IL.info<-ILResults
names(IL.info)[names(IL.info)=='Latitude']<-"Latitude.IL"
names(IL.info)[names(IL.info)=='Longitude']<-"Longitude.IL"
names(IL.info)[names(IL.info)=='StatBlock']<-"BlockNo.IL"
names(IL.info)[names(IL.info)=='SiteName']<-"GrowthSite"
names(IL.info)[names(IL.info)=='SiteId']<-"SIT_Id"


#Add zone 
IL.info$Zone[IL.info$BlockNo %in%  c(seq(13,30,1)) | IL.info$SIT_Id %in% c("588")]<- "E"
IL.info$Zone[IL.info$BlockNo %in%  c(seq(6,12,1))]  <- "CW"
IL.info$Zone[IL.info$BlockNo %in%  c(seq(7,12,1))]  <- "W"
IL.info$Zone[IL.info$BlockNo %in% c("5") | IL.info$SIT_Id %in% c("316")]<- "N"
IL.info$Zone[IL.info$BlockNo %in%  c(1, 2, 3, 4,47, 48, 49,39, 40)] <- "N" 
IL.info$Zone[IL.info$BlockNo %in% c(seq(32, 38,1),seq(41,46,1), seq(50,57,1))] <- "BS"

IL.info<-droplevels(subset(IL.info, SIT_Id != "266"))#Louisa Bay
IL.info<-droplevels(subset(IL.info, SIT_Id != "171"))#Sterile Island
IL.info<-droplevels(subset(IL.info, SIT_Id != "172"))#Actaeon Island
# IL.info<-droplevels(subset(IL.info, SIT_Id != "461"))# One Tree Point
# IL.info<-droplevels(subset(IL.info, SIT_Id != "764"))#Duck holes
IL.info<-droplevels(subset(IL.info, SIT_Id != "480"))#Gagens Point
IL.info<-droplevels(subset(IL.info, SIT_Id != "478"))#Middle Grounds
IL.info<-droplevels(subset(IL.info, SIT_Id != "337"))#Southerly Bottom
IL.info<-droplevels(subset(IL.info, SIT_Id != "813"))#George III

IL.info$L95<-as.numeric(as.character(IL.info$L95))
IL.info$L50<-as.numeric(as.character(IL.info$L50))
IL.info$MaxDL<-as.numeric(as.character(IL.info$MaxDL))
IL.info$MaxSig<-as.numeric(as.character(IL.info$MaxSig))

# #================================================================
# #                         Match SAM and Growth ID's
# #================================================================
SamFilterIL$SIT_Id<-as.character(SamFilterIL$SIT_Id)
SAM.IL<-left_join(SamFilterIL,IL.info[,c(1:6)], by = 'SIT_Id')
SAMSites<-subset(SAM.IL, is.na(MaxDL))
SAMSites<-SAMSites[,1:34]
SAMGwthSites<-subset(SAM.IL, !is.na(MaxDL))
rm(SAM.IL)
Gwth.Id<-unique(SAMGwthSites$SIT_Id)
combinedDF<-left_join(SAMGwthSites[,c(6,17,21,22,24,28:31,33,35:39)],IL.info[,c(1:11)], by = 'SIT_Id')
write.csv(combinedDF, file='GrwthSAMmatched.csv')

# # ==================     LM     ================================= 
boxcox(SAMGwthSites$LD50~SAMGwthSites$L50)
fit<-lm(LD50~L50, data=SAMGwthSites)
summary(fit)

anova(fit)
par(mfrow = c(2,2))
plot(fit)
par(mfrow = c(1,1))

# view the data l50 by ld50
ggplot(data = SAMGwthSites, aes(x=L50,  y=LD50)) + 
 xlab(bquote(''~L50['']~'(mm)')) + ylab(bquote(''~LM['50%']~'(mm)'))+
 geom_smooth(method=lm, se=F, color='grey', fullrange=F, size=1.2, color='black')+
 geom_text_repel(aes(label=GrowthSite), size=3)+
 geom_errorbar(aes(ymin=SAMGwthSites$Ld50BootL95, ymax=SAMGwthSites$Ld50BootU95),
               width=.2)+
 geom_point(aes(colour=Zone), size=3)+
 #ggtitle(paste(dum$SubBlockNo, FishYear))+
 #labs(title= Yeardum$SubBlockNo, size=10)+
 #geom_histogram(binwidth=50)+
 theme_bw()+
 #scale_color_identity()+ #this makes sure the color follows the color argument above in aes()
 theme(legend.position=c(0.1, 0.8))+
 theme(legend.title=element_blank())+
 theme(legend.text = element_text(size=14))+
 theme(axis.title.x = element_text(size=14),
       axis.text.x  = element_text(size=14))+
 theme(axis.title.y = element_text(size=14),
       axis.text.y  = element_text(size=14))


# #drop SAM observations with odd low LD50
# OddLook<-subset(SAMGwthSites, L50 < 110 & LD50 > 120)
# OddSiteCodes<-unique(OddLook$SiteCode)
# pick <- which(SamFilterIL$SiteCode %in% OddSiteCodes)
# SamFilterIL<-SamFilterIL[-pick,]
#
# #================================================================
# #
# #                         Growth data allocation with zone 
# #
# #================================================================
#############################
SAMSite.BS<-droplevels(subset(SAMSites, Zone == 'BS'))
SAMSite.Z<-droplevels(subset(SAMSites, Zone != 'BS'))

Zones<-unique(SAMSite.Z$Zone)
#####
if (exists("SAMjoin")) 
 rm(SAMjoin)

for(z in Zones){
 Samchoice<-subset(SAMSites, Zone == z)
 ILchoice<-subset(SAMGwthSites, Zone == z)
 Samchoice$match<-sapply(Samchoice$LD50,function(x)which.min(abs(x - ILchoice$LD50)))
 Samchoice$match<-as.numeric(Samchoice$match)
 ILchoice$match<-1:nrow(ILchoice)
 
 Zonejoin<-left_join(Samchoice,ILchoice[,c(6,37:42)], by = 'match')
 
 if (exists("SAMjoin"))
  SAMjoin <- rbind(SAMjoin, Zonejoin)
 else
  SAMjoin <- Zonejoin
}
names(SAMjoin)[names(SAMjoin)=='LD50.x']<-"LD50"
names(SAMjoin)[names(SAMjoin)=='LD50.y']<-"GrowthLD50"

#TREAT BSZ grwth data as mean values as no direct matches by SIT_ID
BS.IL.Info<-droplevels(subset(IL.info, SIT_Id == "315"))
BSjoin<-left_join(SAMSite.BS, BS.IL.Info[,c(2:6,12)], by = 'Zone')

BSjoin$GrowthLD50<-NA


SAMGwthOutPut<-rbind.match.columns(SAMjoin, BSjoin)

SAMILResults<-rbind.match.columns(SAMGwthSites,SAMGwthOutPut)


# GrwthAllocations<-SAMjoin[,c(6, 18, 21, 22,29, 36,41)]
# E_GrwthAllocations<-droplevels(subset(GrwthAllocations, Zone =='E'))
# 
# write.csv(GrwthAllocations, file='GrwthAllocations.csv')

########GROWTH ALLOCATION OPTIONS NOT USED###############


#                      1. #ALLOCATE THE GROWTH DATA BY LD50 without zone retrictions

# SAMSite.Z$match<-sapply(SAMSite.Z$LD50,function(x)which.min(abs(x - SAMGwthSites$LD50)))
# SAMGwthSites$match<-c(1:20)
# SAMjoin<-left_join(SAMSite.Z,SAMGwthSites[,c(6,35:41)], by = 'match')
# names(SAMjoin)[names(SAMjoin)=='LD50.x']<-"LD50"
# names(SAMjoin)[names(SAMjoin)=='LD50.y']<-"GrowthLD50"


#                      2. #ALLOCATE GROWTH DATA ONLY KEEPING MAXIMUM REULST FOR EACH MATCH OF GROWTH AND SAM

#SAMGwthSites<-do.call(rbind,lapply(split(SAMGwthSites,SAMGwthSites$SIT_Id),function(chunk) chunk[which.min(chunk$LD50),]))


#                      3. #ALLOCATE GROWTH DATA VIA Producing LD50t for each of the growth data from Growth data with matching LD50range < 5 mm

# fit2<-lm(LD50~L50, data=SAMGwthSites)
# summary(fit2)
# 
# anova(fit2)
# par(mfrow = c(2,2))
# plot(fit2)
# par(mfrow = c(1,1))
# #shapiro.test(fit2)
# 
# 
# IL.info$LD50t<-fit2$coef[1]+fit2$coef[2]*IL.info$L50
# 
# # #================================================================
# # #
# # #                         Growth data allocation with zone 
# # #
# # #================================================================
# #############################
# SAMSite.BS<-droplevels(subset(SamFilter, Zone == 'BS'))
# SAMSite.Z<-droplevels(subset(SamFilter, Zone != 'BS'))
# 
# Zones<-unique(SAMSite.Z$Zone)
# #####
# if (exists("SAMjoin")) 
#  rm(SAMjoin)
# 
# for(z in Zones){
#  Samchoice<-subset(SamFilter, Zone == z)
#  ILchoice<-subset(IL.info, Zone == z)
#  Samchoice$match<-sapply(Samchoice$LD50,function(x)which.min(abs(x - ILchoice$LD50t)))
#  Samchoice$match<-as.numeric(Samchoice$match)
#  ILchoice$match<-1:nrow(ILchoice)
#  
#  Zonejoin<-left_join(Samchoice,ILchoice[,c(2:5,7,14)], by = 'match')
#  
#  if (exists("SAMjoin"))
#   SAMjoin <- rbind(SAMjoin, Zonejoin)
#  else
#   SAMjoin <- Zonejoin
# }
# 
# #TREAT BSZ grwth data as mean values as no direct matches by SIT_ID
# BS.IL.Info<-droplevels(subset(IL.info, SIT_Id == "315"))
# BSjoin<-left_join(SAMSite.BS, BS.IL.Info[,c(2:6,12)], by = 'Zone')
# 
# 
# 
# SAMGwthOutPut<-rbind.match.columns(SAMjoin, BSjoin)
# 
# SAMILResults<-SAMGwthOutPut
# 
