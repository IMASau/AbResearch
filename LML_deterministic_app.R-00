

# Version 1   26/08/2016

rm(list=ls(all=TRUE))
## SET THE WORKING AND RESULTS DIRECTORIES
wkdir <- "D:/R_Stuff/SAM"
setwd(wkdir)


# load D:\R_Stuff\SAM    SAM2016_April  SAM_TechReport230816.RData

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


LML<-SamResults[,c(1,21:22,24, 26:27,6,8,13:16)]

LM.value<-c("LD50", "LD95","Ld50BootU95", "Ld50BootL95", "Ld95BootU95", "Ld95BootL95")
####################     Theortical LML  L50% #######################

  LML$L50T<-1.1539*LML[,LM.value]-15.335
  LML$L95T<-1.0862*LML[,LM.value]+32.461
  LML$MaxDLt<-0.46095*LML$L95T-0.46856*LML$L50T+5.58943
  LML$DL<-LML$MaxDLt/(1+exp((log(19)*((LML$LD50-LML$L50T)/(LML$L95T-LML$L50T)))))
  LML$Yr1<-LML$LD50+LML$DL
  LML$DL2<-LML$MaxDLt/(1+exp((log(19)*((LML$LD50-LML$L50T)/(LML$L95T-LML$L50T)))))
  LML$LMLt<-LML$Yr1+LML$DL2
  LML$LMLtc<-(LML$LMLt-16.8868)/0.8946
  


LMLtc<-cbind(LML[,c(1:6)],as.data.frame(LML$LMLt))
LMLtc<-rename(LMLtc, c("LD50" ="LMLt.LD50" , "LD95"="LMLt.LD95", "Ld50BootU95" = "LMLt.50UCI", "Ld50BootL95" = "LMLt.50LCI", 
                "Ld95BootU95" = "LMLt.95UCI", "Ld95BootL95" = "LMLt.95LCI"))
LMLtc<-cbind(LMLtc,as.data.frame(LML$LMLtc))
LMLtc<-rename(LMLtc, c("LD50" ="LMLtc.LD50" , "LD95"="LMLtc.LD95", "Ld50BootU95" = "LMLtc.50UCI", "Ld50BootL95" = "LMLtc.50LCI", 
                       "Ld95BootU95" = "LMLtc.95UCI", "Ld95BootL95" = "LMLtc.95LCI"))


names(LMLtc)
BlockLMLStats<-ddply(LMLtc,.(BlockNo, Zone), summarize,  n = length(SiteCode), 
                        L50 = mean(LMLtc.LD50, na.rm=T), LCI50 = mean(LMLtc.50LCI, na.rm=T), UCI50 = mean(LMLtc.50UCI, na.rm=T),
                        L95 = mean(LMLtc.LD95, na.rm=T), LCI95 = mean(LMLtc.95LCI, na.rm=T), UCI95 = mean(LMLtc.95UCI, na.rm=T),
                        sd.L50 = sd(LMLtc.LD50, na.rm=T), sd.LCI50 = sd(LMLtc.50LCI, na.rm=T), sd.UCI50 = sd(LMLtc.50UCI, na.rm=T),
                        sd.L95 = sd(LMLtc.LD95, na.rm=T), sd.LCI95 = sd(LMLtc.95LCI, na.rm=T), sd.UCI95 = sd(LMLtc.95UCI, na.rm=T))


#####
# Plots
#####
doPlot = function(LFPlot) {
  dum = subset(BlockLMLStats, Zone == LFPlot)
  ggobj = ggplot(data = dum, aes(y=L50, x=as.factor(BlockNo))) + 
    xlab("BlockNo") +
    ylab("LMLtc (mm)") +
    labs(title= dum$Zone, size=10)+
    geom_boxplot() +
    geom_boxplot(aes(y=LCI50, x=as.factor(BlockNo), colour = "red"))+
    geom_boxplot(aes(y=UCI50, x=as.factor(BlockNo), colour = "red"))+
    scale_fill_grey(start = 0.3, end = 0.7)+
    theme_bw()+
    theme(legend.title=element_blank(),
          legend.text = element_text(size=14),
          axis.title.x = element_text(size=14),
          axis.text.x  = element_text(size=14),
          axis.title.y = element_text(size=14),
          axis.text.y  = element_text(size=14),
          legend.position="none")
  #ggsave(sprintf("%s_LFplot.tiff", LFPlot))
  print(ggobj)
}
lapply(unique(BlockLMLStats$Zone), doPlot)

doPlot = function(LFPlot) {
  dum = subset(BlockLMLStats, Zone == LFPlot)
  ggobj = ggplot(data = dum, aes(y=L50, x=as.factor(BlockNo))) + 
    xlab("BlockNo") +
    ylab("LMLtc (mm)") +
    labs(title= dum$Zone, size=10)+
    ylim(min(dum$LCI50-5), max(dum$UCI50+5))+
    geom_point(position=position_dodge(), stat="identity", size =3) +
    geom_errorbar(aes(ymin=LCI50, ymax=UCI50),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9))+
    theme_bw()+
    theme(legend.title=element_blank(),
          legend.text = element_text(size=14),
          axis.title.x = element_text(size=14),
          axis.text.x  = element_text(size=14),
          axis.title.y = element_text(size=14),
          axis.text.y  = element_text(size=14),
          legend.position="none")
  #ggsave(sprintf("%s_LFplot.tiff", LFPlot))
  print(ggobj)
}
lapply(unique(BlockLMLStats$Zone), doPlot)

