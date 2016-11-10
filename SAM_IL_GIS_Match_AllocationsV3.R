#use latest SamFilter as precusror to this file 
# currently SamFilter050916.RData ## SET THE WORKING AND RESULTS DIRECTORIES
myWorkDrive <- "D:/"  ## Craig and Hugh


myWorkFolder <- "R_Stuff/SAM/Logistic"
myWorkPath <- paste(myWorkDrive,myWorkFolder,sep="")
setwd(myWorkPath)

load("D:/R_Stuff/SAM/Logistic/SAM_FILTER_2016-11-10.RData")

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
library(dplyr)

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

# # #================================================================
# # #                         ADD IN SL DATA from raw SAM database file to SAMfilter
# # #================================================================
load('D:/R_Stuff/SAM/Logistic/BlckPop.RData')

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

# #================================================================
# #                         load Growth parameters
# #================================================================
load('D:/R_Stuff/SAM/Logistic/ILResults271016.RData')

IL.info<-ILResults
names(IL.info)[names(IL.info)=='Latitude']<-"Latitude"
names(IL.info)[names(IL.info)=='Longitude']<-"Longitude"
names(IL.info)[names(IL.info)=='Stat.Block']<-"BlockNo"
names(IL.info)[names(IL.info)=='Sub.Block']<-"SubBlockNo"
names(IL.info)[names(IL.info)=='NameSh']<-"GrowthSite"
names(IL.info)[names(IL.info)=='SiteId']<-"SIT_Id"

#Add zone 
IL.info$Zone[IL.info$BlockNo %in%  c(seq(13,30,1))] <- "E"
IL.info$Zone[IL.info$BlockNo %in%  c(seq(7,12,1))] <- "W"
IL.info$Zone[IL.info$BlockNo %in% c(seq(6,6,1))] <- "CW"
IL.info$Zone[IL.info$BlockNo %in% c(seq(5,5,1))] <- "N"
IL.info$Zone[IL.info$BlockNo %in%  c(1, 2, 3, 4,47, 48, 49,39, 40)] <- "N" 
IL.info$Zone[IL.info$BlockNo %in% c(seq(32, 38,1),seq(41,46,1), seq(50,57,1))] <- "BS"

IL.exclude<- c(172,663,466,815,438,480,813,570,470,252,266,170,171,159, 59, 872,459,662) # see following file for exmaplations of exclusions D:\Fisheries Research\Abalone\SAM\Growth code\ILResultsOutliersEXCLUDE191016.csv
IL.info<-IL.info[!is.element(IL.info$SIT_Id,IL.exclude),]


#IL.info<-droplevels(subset(IL.info, SIT_Id != "266"))#Louisa Bay

IL.info$L95<-as.numeric(as.character(IL.info$L95))
IL.info$L50<-as.numeric(as.character(IL.info$L50))
IL.info$MaxDL<-as.numeric(as.character(IL.info$MaxDL))
IL.info$MaxSig<-as.numeric(as.character(IL.info$MaxSig))

# #================================================================
# #                         Match SAM and Growth ID's EZ and WZ
# #================================================================
ZoneEW<-c("E", "W", "CW")
SamFilterEW<-subset(SamFilterIL, Zone %in% ZoneEW )
IL.infoEW<-subset(IL.info, Zone %in% ZoneEW )
SAM.IL.EW<-left_join(SamFilterEW,IL.infoEW[,c(2:5,12)], by = 'GrowthSite')
SAMSitesEW<-subset(SAM.IL.EW, is.na(MaxDL))
SAMSitesEW<-SAMSitesEW[,c(1:52)]
SAMGwthSitesEW<-subset(SAM.IL.EW, !is.na(MaxDL))
rm(SAM.IL.EW)

# # ==================     LM     ================================= 
boxcox(SAMGwthSitesEW$LD50^3~SAMGwthSitesEW$L50)
fit<-lm(LD50^3~L50, data=SAMGwthSitesEW)
summary(fit)

anova(fit)
par(mfrow = c(2,2))
plot(fit)
par(mfrow = c(1,1))

# view the data l50 by ld50
ggplot(data = SAMGwthSitesEW, aes(x=L50,  y=LD50)) + 
 xlab(bquote(''~L50['']~'(mm)')) + ylab(bquote(''~LM['50%']~'(mm)'))+
 geom_smooth(method=lm, se=F, color='grey', fullrange=F, size=1.2)+
 geom_text_repel(aes(label=GrowthSite), size=3)+
 geom_errorbar(aes(ymin=SAMGwthSitesEW$Ld50BootL95, ymax=SAMGwthSitesEW$Ld50BootU95),
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
#                  MATCHING remaining SAM TO IL/SAM data EZ and WZ
#
######################
ILchoice<-SAMGwthSitesEW
Samchoice<-SAMSitesEW
#Samchoice<-subset(SAMSitesEW, LD50 >=90)
Samchoice$match<-sapply(Samchoice$LD50,function(x)which.min(abs(x - ILchoice$LD50)))
Samchoice$match<-as.numeric(Samchoice$match)
ILchoice$match<-1:nrow(ILchoice)

ILchoice$GwthLD50<-ILchoice$LD50
SAMjoinEW<-left_join(Samchoice[,c(1,6,9,16:17,29:33,39:42,46,47,51:53)],ILchoice[,c(53:55,57:58)], by = 'match')

# view the data l50 by ld50
ggplot(data = SAMjoinEW, aes(x=L50,  y=LD50)) + 
 xlab(bquote(''~L50['']~'(mm)')) + ylab(bquote(''~LM['50%']~'(mm)'))+
 geom_smooth(method=lm, se=F, color='grey', fullrange=F, size=1.2)+
 #geom_text_repel(aes(label=GrowthSite), size=3)+
 geom_errorbar(aes(ymin=SAMjoinEW$Ld50BootL95, ymax=SAMjoinEW$Ld50BootU95),
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

boxcox(SAMjoinEW$LD50^3~SAMjoinEW$L50)
fit<-lm(LD50^3~L50, data=SAMjoinEW)
summary(fit)

anova(fit)
par(mfrow = c(2,2))
plot(fit)
par(mfrow = c(1,1))

# SAMGwthSites$GwthLD50<-SAMGwthSites$LD50
SAMGwthSitesEW$GwthLD50<-SAMGwthSitesEW$LD50

# SAMILResults<-rbind.match.columns(SAMGwthSites,SAMjoin)
SAMILResultsEW<-rbind.match.columns(SAMGwthSitesEW,SAMjoinEW)

#===================================================================
# #                                BS AND NZ   
# #                         Match SAM and Growth ID's
#===================================================================
ZoneNBS<-c("N", "BS")
SamFilterNBS<-subset(SamFilterIL, Zone %in% ZoneNBS )
IL.infoNBS<-subset(IL.info, Zone %in% ZoneNBS )
SAM.IL.NBS<-left_join(SamFilterNBS,IL.infoNBS[,c(2:5,12)], by = 'GrowthSite')
SAMSitesNBS<-subset(SAM.IL.NBS, is.na(MaxDL))
SAMSitesNBS<-SAMSitesNBS[,c(1:52)]
SAMGwthSitesNBS<-subset(SAM.IL.NBS, !is.na(MaxDL))
rm(SAM.IL.NBS)

# # ==================     LM     ================================= 
boxcox(SAMGwthSitesNBS$LD50^-1~SAMGwthSitesNBS$L50)
fit<-lm(LD50^-1~L50, data=SAMGwthSitesEW)
summary(fit)

anova(fit)
par(mfrow = c(2,2))
plot(fit)
par(mfrow = c(1,1))

# view the data l50 by ld50
ggplot(data = SAMGwthSitesNBS, aes(x=L50,  y=LD50)) + 
 xlab(bquote(''~L50['']~'(mm)')) + ylab(bquote(''~LM['50%']~'(mm)'))+
 geom_smooth(method=lm, se=F, color='grey', fullrange=F, size=1.2)+
 geom_text_repel(aes(label=GrowthSite), size=3)+
 geom_errorbar(aes(ymin=SAMGwthSitesNBS$Ld50BootL95, ymax=SAMGwthSitesNBS$Ld50BootU95),
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


#ADD EW SAMGwthSites to a maximum LM50% of 120 mm which is the max found in NZ BSZ
range(SAMSitesNBS$LD50)
SAMGwthSitesSub<-subset(SAMGwthSitesEW, LD50 <121)

SAMGwthSitesNBS$GwthLD50<-SAMGwthSitesNBS$LD50

SAMGwthSitesMix<-rbind(SAMGwthSitesNBS, SAMGwthSitesSub)
######################
#                  MATCHING remaining SAM TO IL/SAM data in BSZ NZ
#
######################
ILchoice<-SAMGwthSitesMix
Samchoice<-SAMSitesNBS
#Samchoice<-subset(SAMSitesEW, LD50 >=90)
Samchoice$match<-sapply(Samchoice$LD50,function(x)which.min(abs(x - ILchoice$LD50)))
Samchoice$match<-as.numeric(Samchoice$match)
ILchoice$match<-1:nrow(ILchoice)

ILchoice$GwthLD50<-ILchoice$LD50
SAMjoinNBS<-left_join(Samchoice[,c(1,6,9,16:17,29:33,39:42,46,47,51:53)],ILchoice[,c(53:55,57:58)], by = 'match')


# view the data l50 by ld50
ggplot(data = SAMjoinNBS, aes(x=L50,  y=LD50)) + 
 xlab(bquote(''~L50['']~'(mm)')) + ylab(bquote(''~LM['50%']~'(mm)'))+
 geom_smooth(method=lm, se=F, color='grey', fullrange=F, size=1.2)+
 #geom_text_repel(aes(label=SIT_Name), size=3)+
 geom_errorbar(aes(ymin=SAMjoinNBS$Ld50BootL95, ymax=SAMjoinNBS$Ld50BootU95),
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



# SAMILResults<-rbind.match.columns(SAMGwthSites,SAMjoin)
SAMILResultsNBS<-rbind.match.columns(SAMGwthSitesNBS,SAMjoinNBS)
SAMILResults<-rbind(SAMILResultsEW,SAMILResultsNBS)



######################
#                  MATCHING remaining SAM TO IL/SAM data all data
######################

SAM.IL<-left_join(SamFilterIL, IL.info[,c(2:5,12)], by = 'GrowthSite')
SAMGwthSites<-subset(SAM.IL, !is.na(MaxDL))
rm(SAM.IL)

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
 geom_text_repel(aes(label=GrowthSite), size=4, colour= 'grey')+
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


#filter out match which are > confidnece interval range.

SAMjoin<-rbind(SAMjoinEW, SAMjoinNBS)
SAMjoin$LD50Diff<-SAMjoin$LD50-SAMjoin$GwthLD50
range(SAMjoin$LD50Diff)
hist(SAMjoin$LD50Diff)
limit<-0-mean(SAMjoin$Ld50BootRange)/2

SAMjoin<-subset(SAMjoin, LD50Diff >= limit)
hist(SAMjoin$LD50Diff)

mean(SAMjoin$LD50Diff)
sd(SAMjoin$LD50Diff)


ggplot(data = SAMjoin, aes(x=L50,  y=LD50)) + 
 xlab(bquote(''~L50['']~'(mm)')) + ylab(bquote(''~LM['50%']~'(mm)'))+
 #geom_smooth(method=lm, se=F, color='grey', fullrange=F, size=1.2)+
 #geom_text_repel(aes(label=GrowthSite), size=4, colour= 'grey')+
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

boxcox(SAMjoin$LD50~SAMjoin$L50)
fit<-lm(LD50~L50, data=SAMjoin)
summary(fit)

anova(fit)
par(mfrow = c(2,2))
plot(fit)

par(mfrow = c(1,1))

#save Rfile
outFile <- paste(myWorkPath,"/SAMILRESULTS_",format(Sys.time(), "%Y-%m-%d"),".RData",sep ="")
save.image(file = outFile)


####Histogram 
ggplot(data = F.GwthResults, aes(x=LD50Diff)) + 
 geom_histogram(bins = 30)+
 xlab(expression(paste('LM'['50%']~'Differences (mm)')))+
 #ylim(0,120)+
 #geom_vline(xintercept=mean(SamResults$Ld50BootRange, na.rm=T), colour = 'red', linetype= 3, size=1.5)+
 #geom_vline(xintercept=max(SamFilterIL$Ld50BootRange, na.rm=T),  linetype= 3, size=1.2)+
 #geom_vline(xintercept=Lsd_ld50ci,  linetype= 3, size=1.2)+
 theme_bw()+
 #scale_fill_identity()+ #this makes sure the color follows the color argument above in aes()
 theme(legend.position=c(0.9, 0.8))+
 theme(legend.title=element_blank())+
 theme(legend.text = element_text(size=14))+
 theme(axis.title.x = element_text(size=14),
       axis.text.x  = element_text(size=14))+
 theme(axis.title.y = element_text(size=14),
       axis.text.y  = element_text(size=14))