#use latest SamFilter as precusror to this file 
# currently SamFilter050916.RData 
library(car)
library(MASS)
library(boot)
library(plyr)
library(dtplyr)
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
# # LOAD IL_SAM_GIS match DATA
Sam.GIS.IL<-read.csv('D:/R_Stuff/SAM/Logistic/Sam_ILRefined_GIS_Match_131016.csv', header =T)

Sam.match.IL<-Sam.GIS.IL[,c(2,3)]


load('D:/R_Stuff/SAM/Logistic/SamFilter131016.RData')

SamFilterIL<-join(SamFilter, Sam.match.IL, by='SiteCode')
keep(SamFilterIL, SamFilter, SamResults, BlckPop, Sam.GIS.IL, rbind.match.columns, sure=T)

SamFilterIL<-subset(SamFilterIL, Ld50BootRange <=20)

#recode database subblock errors
SamFilterIL$SubBlockNo[Sam.GIS.IL$SubBlockNo==11] <- "11A"

SamFilterIL$Ld50BootRangeLog<-log(SamFilterIL$Ld50BootRange)
hist(SamFilterIL$Ld50BootRangeLog)
mean(SamFilterIL$Ld50BootRangeLog)
sd(SamFilterIL$Ld50BootRangeLog)
SAMLd50SD<-mean(SamFilterIL$Ld50BootRangeLog, na.rm = T)+ sd(SamFilterIL$Ld50BootRangeLog, na.rm = T)

SamFilterIL<-subset(SamFilterIL, Ld50BootRangeLog <= SAMLd50SD)
hist(SamFilterIL$Ld50BootRange)
max(SamFilterIL$Ld50BootRange)

SamFilterIL$Ld50BootRangeLog<-NULL

# # #================================================================
# # #                         ADD IN SL DATA from raw SAM database file to SAMfilter
# # #================================================================
SiteCodes<-unique(SamFilterIL$SiteCode)
pick <- which(BlckPop$SiteCode %in% SiteCodes)
BlckPopFilter<-BlckPop[pick,]

names(BlckPopFilter)[names(BlckPopFilter)=='SIT_StatBlock']<-"BlockNo"
names(BlckPopFilter)[names(BlckPopFilter)=='SIT_SubBlock']<-"SubBlockNo"
#Reformat SublockNo
BlckPopFilter$SubBlockNo<-paste(BlckPopFilter$BlockNo,BlckPopFilter$SubBlockNo, sep="")

maxSLsitecode<-ddply(BlckPopFilter,.(SiteCode), summarize,
                     SLmax = max(SPC_ShellLength, na.rm=T), SLq95 = quantile(SPC_ShellLength, 0.95, na.rm=T))
#
#Match SLBlockSum to SamFilter by subblock
#
SamFilterIL<-left_join(SamFilterIL,maxSLsitecode, by = 'SiteCode')
rm(maxSLsitecode)

#drop SAM observations with odd low LD50
# OddLook<-subset(SamFilterIL, LD50 < 100 & SLmax > 183)
# OddSiteCodes<-unique(OddLook$SiteCode)
# pick <- which(SamFilterIL$SiteCode %in% OddSiteCodes)
# SamFilterIL<-SamFilterIL[-pick,]

# #================================================================
# #                         load Growth parameters
# #================================================================
load('D:/R_Stuff/SAM/Logistic/ILResults191016.Rdata')

IL.info<-ILResults
names(IL.info)[names(IL.info)=='Latitude']<-"Latitude.IL"
names(IL.info)[names(IL.info)=='Longitude']<-"Longitude.IL"
names(IL.info)[names(IL.info)=='StatBlock']<-"BlockNo.IL"
names(IL.info)[names(IL.info)=='NameSh']<-"GrowthSite"
names(IL.info)[names(IL.info)=='SiteId']<-"SIT_Id"

#Add zone 
IL.info$Zone[IL.info$BlockNo %in%  c(seq(13,30,1))] <- "E"
IL.info$Zone[IL.info$BlockNo %in%  c(seq(7,12,1))] <- "W"
IL.info$Zone[IL.info$BlockNo %in% c(seq(6,6,1))] <- "CW"
IL.info$Zone[IL.info$BlockNo %in% c(seq(5,5,1))] <- "N"
IL.info$Zone[IL.info$BlockNo %in%  c(1, 2, 3, 4,47, 48, 49,39, 40)] <- "N" 
IL.info$Zone[IL.info$BlockNo %in% c(seq(32, 38,1),seq(41,46,1), seq(50,57,1))] <- "BS"

IL.exclude<- c(172,663,466,815,170,438,480,813,570,470,59,252,266,159,171,872,459) # see following file for exmaplations of exclusions D:\Fisheries Research\Abalone\SAM\Growth code\ILResultsOutliersEXCLUDE191016.csv
IL.info<-IL.info[!is.element(IL.info$SIT_Id,IL.exclude),]


#IL.info<-droplevels(subset(IL.info, SIT_Id != "266"))#Louisa Bay

IL.info$L95<-as.numeric(as.character(IL.info$L95))
IL.info$L50<-as.numeric(as.character(IL.info$L50))
IL.info$MaxDL<-as.numeric(as.character(IL.info$MaxDL))
IL.info$MaxSig<-as.numeric(as.character(IL.info$MaxSig))

# #================================================================
# #                         Match SAM and Growth ID's
# #================================================================
SAM.IL<-left_join(SamFilterIL,IL.info[,c(2:5,11,13)], by = 'GrowthSite')
SAMSites<-subset(SAM.IL, is.na(MaxDL))
SAMSites<-SAMSites[,1:50]
SAMGwthSitesRAW<-subset(SAM.IL, !is.na(MaxDL))
rm(SAM.IL)
#write.csv(SAMGwthSites, file='GrwthSAMmatched.csv')


SAMGwthSites<-subset(SAMGwthSitesRAW, LD50>=90)

# # ==================     LM     ================================= 
boxcox(SAMGwthSites$LD50~SAMGwthSites$L50)
fit<-lm(log(LD50)~L50, data=SAMGwthSites)
summary(fit)

anova(fit)
par(mfrow = c(2,2))
plot(fit)
par(mfrow = c(1,1))

# view the data l50 by ld50
ggplot(data = SAMGwthSites, aes(x=L50,  y=LD50)) + 
 xlab(bquote(''~L50['']~'(mm)')) + ylab(bquote(''~LM['50%']~'(mm)'))+
 geom_smooth(method=lm, se=F, color='grey', fullrange=F, size=1.2)+
 geom_text_repel(aes(label=GrowthSite), size=3)+
 geom_errorbar(aes(ymin=SAMGwthSites$Ld50BootL95, ymax=SAMGwthSites$Ld50BootU95),
               width=.2, colour = 'grey')+
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

######################
#                  MATCHING remaining SAM TO IL/SAM data
#
######################
ILchoice<-SAMGwthSites
Samchoice<-subset(SAMSites, LD50 >=90)
Samchoice$match<-sapply(Samchoice$LD50,function(x)which.min(abs(x - ILchoice$LD50)))
Samchoice$match<-as.numeric(Samchoice$match)
ILchoice$match<-1:nrow(ILchoice)

ILchoice$GwthLD50<-ILchoice$LD50
SAMjoin<-left_join(Samchoice,ILchoice[,c(48,51:53,55:57)], by = 'match')

# view the data l50 by ld50
ggplot(data = SAMjoin, aes(x=L50,  y=LD50)) + 
 xlab(bquote(''~L50['']~'(mm)')) + ylab(bquote(''~LM['50%']~'(mm)'))+
 geom_smooth(method=lm, se=F, color='grey', fullrange=F, size=1.2)+
 #geom_text_repel(aes(label=GrowthSite.y), size=3)+
 geom_errorbar(aes(ymin=SAMjoin$Ld50BootL95, ymax=SAMjoin$Ld50BootU95),
               width=.2, colour = 'grey')+
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

boxcox(SAMjoin$LD50^1.7~SAMjoin$L50)
fit<-lm(LD50^1.7~L50, data=SAMjoin)
summary(fit)

anova(fit)
par(mfrow = c(2,2))
plot(fit)
par(mfrow = c(1,1))

SAMGwthSites$GwthLD50<-SAMGwthSites$LD50
SAMILResults<-rbind.match.columns(SAMGwthSites,SAMjoin)

