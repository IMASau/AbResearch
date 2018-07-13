library(car)
library(MASS)
library(boot)
library(dplyr)
library(plyr)
library(gdata)
library(ggplot2)
library(multcompView)
#sources
source("C:/GitCode/r-AbSpatialAnalyses/GraphsUtils.r") # source of the TukeyHSD letters in ggplots

#########
# LOAD      GwthResults050916.RData from R_stuff/SAM/Logistic
#########

####RESULTS ANALYSIS

GwthResults$PctLessLML<-100-GwthResults$PctU.LML

#############################
#  l50% by zone figure and ANOVA
#############################

#anova L50 by Zone
boxcox(GwthResults$LD50^3.6~GwthResults$Zone)
fit<-aov(LD50^3.6~Zone, data=GwthResults)
anova(fit)
par(mfrow = c(2,2))
plot(fit)
par(mfrow = c(1,1))
tHSDlm<- TukeyHSD(fit, ordered = FALSE, conf.level = 0.95)
tHSDlm

#set working dataframe for Tukey label function
ASM<-GwthResults


#Boxplot by maturity L%
ggplot(GwthResults, aes(x=Zone, y=LD50)) + 
  xlab("Zone") +  
  ylab(expression(paste(LM['50%']~'(mm)')))+
  geom_boxplot(outlier.colour = "black", outlier.size = 3)+
  theme_bw()+#white background
  theme(legend.position="none",
        axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=14))+
  geom_text(data = generate_label_df(tHSDlm, "Zone"), aes(x = plot.labels, y = 60, label = labels))


ddply(GwthResults,.(Zone), summarize,  mnLD50 = median(LD50, na.rm=T), number = length(LD50))



################
# SUMSTATS by Block
################
BlockSumStats<-ddply(GwthResults,.(BlockNo, Zone), summarize,  n = length(SiteCode), 
                     mn.L50 = mean(LD50, na.rm=T), mn.LCI50 = mean(Ld50BootL95, na.rm=T), mn.UCI50 = mean(Ld50BootU95, na.rm=T),
                     mn.L95 = mean(LD95, na.rm=T), mn.LCI95 = mean(Ld95BootL95, na.rm=T), mn.UCI95 = mean(Ld95BootU95, na.rm=T),
                     mn.IQR = mean(IQR, na.rm=T), sd.IQR = sd(IQR, na.rm=T),
                     mn.pct.L50 = mean(PctL50, na.rm=T), sd.pct.L50 = mean(PctL50, na.rm=T),
                     mn.Bootrange.L50 = mean(Ld50BootRange, na.rm=T), sd.Bootrange.L50 = mean(Ld50BootRange, na.rm=T),
                     mn.eLML = mean(eLML, na.rm=T) , sd.eLML = sd(eLML, na.rm=T), se.eLML = sd(eLML, na.rm=T)/sqrt(length(eLML)),
                     mn.SLmax = mean(SLmax, na.rm=T),
                     mn.PctU.LML = mean(PctU.LML, na.rm =T), sd.PctU.LML = sd(PctU.LML, na.rm =T))
                                          # mn.eLMLbootL95 = mean(eLMLbootL95, na.rm=T) , sd.eLMLbootL95 = sd(eLMLbootL95, na.rm=T),
                     # mn.eLMLbootU95 = mean(eLMLbootU95, na.rm=T) , sd.eLMLbootU95 = sd(eLMLbootU95, na.rm=T),
                     # diffLML = mean(LMLDiff, na.rm=T))
write.csv(BlockSumStats, file= "blockSAMstats.csv")

#difference between maxSL and LD50
BlockSumStats$LD50SLmaxDiff<-BlockSumStats$mn.SLmax-BlockSumStats$mn.L50
mean(BlockSumStats$LD50SLmaxDiff)
range(BlockSumStats$LD50SLmaxDiff)

#############################
#   (%<L50%)  comparison at l50 
#############################

#Range of IQ against CIrangeL50%
plot(GwthResults$PctL50, GwthResults$Ld50BootRange)

# 
#remove outliers from dataset and run lm
pick <- which(GwthResults$Ld50BootRange >10)
IQRlm <- GwthResults[-pick,]
IQRlm <- droplevels(IQRlm)

#anova diferences L%
boxcox(log(IQRlm$Ld50BootRange)~IQRlm$PctL50)
fit<-lm(log(Ld50BootRange)~IQR, data=IQRlm)
summary(fit)
anova(fit)
par(mfrow = c(2,2))
plot(fit)
par(mfrow = c(1,1))


ggplot(data = GwthResults, aes(x=PctL50,  y=Ld50BootRange, color=ifelse(Ld50BootRange>10, 'red', 'black'))) + 
  geom_point()+
  xlab(bquote('Proportion <'~LM['50%']~'(%)')) + ylab(bquote('C.I. Range'~LM['50%']~'(mm)'))+
  #geom_smooth(method=lm, se=F, fill='Black', fullrange=F, size=1.2, color='black')+
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
#############################
#  END-  (%<L50%)  comparison at l50 
#############################


#############################
#   IQrange (L75-L25) CIrange comparison at l50 
#############################
#PERCENTAGE IQR and bootstrapped CI of LM50
ggplot(data=GwthResults, aes(x=PctIQR,  y=Ld50BootRange, color=ifelse(Ld50BootRange>10, 'red', 'black'))) + 
 geom_point()+
 xlab(bquote(~IQ['Range']~'%')) + 
 ylab(bquote(~C.I.['Range']~LM['50%']~'(mm)'))+
 #geom_smooth(method=lm, se=F, fill='Black', fullrange=F, size=1.2, color='black')+
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

plot(GwthResults$PctIQR, GwthResults$Ld50BootRange)
#anova diferences L%
boxcox(log(GwthResultsLM50.10MM$Ld50BootRange)~GwthResultsLM50.10MM$PctIQR)
fit<-lm(log(GwthResultsLM50.10MM$Ld50BootRange)~GwthResultsLM50.10MM$PctIQR)
summary(fit)
anova(fit)
par(mfrow = c(2,2))
plot(fit)
par(mfrow = c(1,1))

#Range of IQ against CIrangeL50%
plot(GwthResults$IQR, GwthResults$Ld50BootRange)

# 
#remove outliers from dataset and run lm
pick <- which(GwthResults$Ld50BootRange >10)
IQRlm <- GwthResults[-pick,]
IQRlm <- droplevels(IQRlm)

#anova diferences L%
boxcox(IQRlm$Ld50BootRange^-0.7~IQRlm$IQR)
fit<-lm(Ld50BootRange^-0.7~IQR, data=IQRlm)
summary(fit)
anova(fit)
par(mfrow = c(2,2))
plot(fit)
par(mfrow = c(1,1))


ggplot(data = GwthResults, aes(x=IQR,  y=Ld50BootRange, color=ifelse(Ld50BootRange>10, 'red', 'black'))) + 
  geom_point()+
  xlab(bquote(~IQ['range']~'(mm)')) + ylab(bquote('C.I. Range'~LM['50%']~'(mm)'))+
  #geom_smooth(method=lm, se=F, fill='Black', fullrange=F, size=1.2, color='black')+
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


#                    HISTOGRAM CI RANGE

mn_ld50ci<-mean(GwthResults$Ld50BootRange, na.rm=T)
#md_ld50ci<-median(GwthResults$Ld50BootRange, na.rm=T)
Usd_ld50ci<-mn_ld50ci+sd(GwthResults$Ld50BootRange, na.rm=T)
Lsd_ld50ci<-mn_ld50ci-sd(GwthResults$Ld50BootRange, na.rm=T)

####Histogram of CIrangeL50
ggplot(data = GwthResults, aes(x=Ld50BootRange)) + 
  geom_histogram(bins = 30)+
  xlab(bquote(~CI['range']~'LM'['50%']))+
  #ylim(0,120)+
 geom_vline(xintercept=mn_ld50ci, colour = 'red', linetype= 3, size=1.5)+
 geom_vline(xintercept=Usd_ld50ci,  linetype= 3, size=1.2)+
 geom_vline(xintercept=Lsd_ld50ci,  linetype= 3, size=1.2)+
  theme_bw()+
  #scale_fill_identity()+ #this makes sure the color follows the color argument above in aes()
  theme(legend.position=c(0.9, 0.8))+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(size=14))+
  theme(axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=14))+
  theme(axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=14))



#############################
#  END-  IQrange (L75-L25) CIrange comparison at l50 
#############################

#
#############################
#    Figure 6 L50 and L95 CIrange comparison
#############################

rangeCI95<-as.data.frame(GwthResults$Ld95BootRange)
colnames(rangeCI95)[1] <- "CIRange"
rangeCI95$LM<-"LM95%"

rangeCI50<-as.data.frame(GwthResults$Ld50BootRange)
colnames(rangeCI50)[1] <- "CIRange"
rangeCI50$LM<-"LM50%"
CIRange<-rbind(rangeCI50, rangeCI95)
rm(rangeCI50, rangeCI95)


#anova diferences L%
boxcox(CIRange$CIRange^-0.35~CIRange$LM)
fit<-aov(CIRange^-0.35~LM, data=CIRange)
anova(fit)
par(mfrow = c(2,2))
plot(fit)
par(mfrow = c(1,1))
tHSDlm<- TukeyHSD(fit, ordered = FALSE, conf.level = 0.95)
tHSDlm

#set working dataframe for Tukey label function
ASM<-CIRange


#Boxplot by maturity L%
ggplot(CIRange, aes(x=LM, y=CIRange)) + 
  xlab("Maturity Estimate") + ylab(bquote(~CI['range']~'LM'))+
  geom_boxplot(outlier.colour = "black", outlier.size = 3)+
  theme_bw()+#white background
  theme(legend.position="none",
        axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=14))
#geom_text(data = generate_label_df(tHSDlm, "LM"), aes(x = plot.labels, y = 0.2, label = labels))
ddply(CIRange,.(LM), summarize,  M = median(CIRange, na.rm=T))

#############################
#   END -  Figure 6 L50 and L95 CIrange comparison
#############################


#############################
#   Mrange (L95-L05) CIrange comparison at l50 (figure not currently used)
#############################
#Range of maturity onset (l95-L05) against CIraangeL50%
GwthResults$Mrange<-GwthResults$LD95-GwthResults$LD05
plot(GwthResults$Mrange, GwthResults$Ld50BootRange)
#anova diferences L%
boxcox(GwthResults$Ld50BootRange^-0.7~GwthResults$Mrange)
fit<-lm(GwthResults$Ld50BootRange^-0.7~GwthResults$Mrange)
summary(fit)
anova(fit)
par(mfrow = c(2,2))
plot(fit)
par(mfrow = c(1,1))


ggplot(data=GwthResults, aes(x=Mrange,  y=Ld50BootRange, color=ifelse(Ld50BootRange>10, 'red', 'black'))) + 
  geom_point()+
  xlab(bquote(~M['range']~'(mm)')) + ylab(bquote('C.I. Range'~LM['50%']~'(mm)'))+
  #geom_smooth(method=lm, se=F, fill='Black', fullrange=F, size=1.2, color='black')+
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
#############################
#   END- Mrange (L95-L05) CIrange comparison at l50 (figure not currently used)
#############################




#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

#############################
#  CIrange perecentages
#############################

#% below thresholds
pick <- which(GwthResults$Ld50BootRange <5)
ci5less <- GwthResults[pick,]
ci5less <- droplevels(ci5less)

dim(ci5less)
94/260*100



#Boxplot by MLStc by zone
ggplot(GwthResults, aes(x=Zone, y=eLML)) + 
  xlab("Zone") +  ylab("eLML")+
  geom_boxplot(outlier.colour = "black", outlier.size = 3)+
  theme_bw()+#white background
  theme(legend.position="none",
        axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=14))
  #geom_text(data = generate_label_df(tHSDlm, "Zone"), aes(x = plot.labels, y = 60, label = labels))





#
#########################
#           Plots for eLML L50%
#########################
#
# add ylimits for plots
Zone<-c('BS', "CW", "E", "N", "W")
Top<-c(112, 135, 150, 135, 150)
Bottom<-c(95, 110, 130, 90, 135)

ylimits<-data.frame(Zone, Top, Bottom)
GwthResults<-left_join(GwthResults, ylimits, by = "Zone")


setwd("D:/Fisheries Research/Abalone/SAM")

doPlot = function(LFPlot) {
 dum = subset(GwthResults, Zone == LFPlot)
 ggobj = ggplot(data = dum, aes(y=eLML, x=as.factor(BlockNo))) + 
  xlab("BlockNo") +
  ylab("eLML (mm)") +
  labs(title= dum$Zone, size=10)+
  ylim(min(dum$Bottom), max(dum$Top))+
  geom_boxplot(outlier.colour = "black", outlier.size = 3)+
  geom_hline(yintercept=dum$LML, colour = 'red', linetype= 3)+
  theme_bw()+
  theme(legend.title=element_blank(),
        legend.text = element_text(size=14),
        axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=14),
        legend.position="none")
 ggsave(sprintf("%s_eLMLplot.tiff", LFPlot, width = 6, height = 8, units = "cm"))
 print(ggobj)
}
lapply(unique(GwthResults$Zone), doPlot)



################
# SUMSTATS by Block
################

BlockSumStats<-ddply(GwthResults,.(BlockNo, Zone), summarize,  n = length(SiteCode), 
                     mn.L50 = mean(LD50, na.rm=T), mn.LCI50 = mean(Ld50BootL95, na.rm=T), mn.UCI50 = mean(Ld50BootU95, na.rm=T),
                     mn.L95 = mean(LD95, na.rm=T), mn.LCI95 = mean(Ld95BootL95, na.rm=T), mn.UCI95 = mean(Ld95BootU95, na.rm=T),
                     mn.IQR = mean(IQR, na.rm=T), sd.IQR = sd(IQR, na.rm=T),
                     mn.pct.L50 = mean(PctL50, na.rm=T), sd.pct.L50 = mean(PctL50, na.rm=T),
                     mn.Bootrange.L50 = mean(Ld50BootRange, na.rm=T), sd.Bootrange.L50 = mean(Ld50BootRange, na.rm=T),
                     mn.eLML = mean(eLML, na.rm=T) , sd.eLML = sd(eLML, na.rm=T), se.eLML = sd(eLML, na.rm=T)/sqrt(length(eLML)))


doPlot = function(LFPlot) {
 dum = subset(GwthResults, Zone == LFPlot)
 ggobj = ggplot(data = dum, aes(x=LD50,  y=eLML)) + 
 geom_point(aes(colour=GrowthSite), size=3)+
 xlab(bquote(''~LM['50%']~'(mm)')) + ylab(bquote(''~eLML['']~'(mm)'))+
 geom_smooth(method=lm, se=F, color='grey', fullrange=F, size=1.2, color='black')+
 #geom_text_repel(aes(label=BlockNo), size=3)+
 #ggtitle(paste(dum$SubBlockNo, FishYear))+
 #labs(title= Yeardum$SubBlockNo, size=10)+
 #geom_histogram(binwidth=50)+
 theme_bw()+
 #scale_color_identity()+ #this makes sure the color follows the color argument above in aes()
# theme(legend.position=c(0.1, 0.8))+
 theme(legend.title=element_blank())+
 theme(legend.text = element_text(size=14))+
 theme(axis.title.x = element_text(size=14),
       axis.text.x  = element_text(size=14))+
 theme(axis.title.y = element_text(size=14),
       axis.text.y  = element_text(size=14))
}
lapply(unique(GwthResults$Zone), doPlot)

#
##                   PCT LML protection
#
doPlot = function(LFPlot) {
  dum = subset(GwthResults, Zone == LFPlot)
  ggobj = ggplot(data = dum, aes(y=PctU.LML, x=as.factor(BlockNo))) + 
    xlab("BlockNo") +
    ylab("Percentage Population Protected") +
    labs(title= dum$Zone, size=10)+
    #ylim(min(dum$sd.eLML-10), max(dum$sd.eLML+10))+
    geom_boxplot(outlier.colour = "black", outlier.size = 3)+
   #geom_hline(yintercept=dum$LML, colour = 'red', linetype= 3)+
        theme_bw()+
    theme(legend.title=element_blank(),
          legend.text = element_text(size=14),
          axis.title.x = element_text(size=14),
          axis.text.x  = element_text(size=14),
          axis.title.y = element_text(size=14),
          axis.text.y  = element_text(size=14),
          legend.position="none")
  ggsave(sprintf("%s_PctProtected.tiff", LFPlot))
  print(ggobj)
}
lapply(unique(BlockSumStats$Zone), doPlot)

#
#########################
#           Plots for eLML L50% on LM50 < 5mm
#########################
#
#
################
# SUMSTATS by Block
################
GwthResultsLM50.10MM<-droplevels(subset(GwthResults, Ld50BootRange <= 10))
GwthResultsLM50.5MM<-droplevels(subset(GwthResults, Ld50BootRange <= 5))

L50RangeMean<-mean(GwthResults$Ld50BootRange, na.rm=T)

pick <- which(GwthResults$Ld50BootRange <= L50RangeMean)
GwthRltLD50Mn <- GwthResults[pick,]
GwthRltLD50Mn <- droplevels(GwthRltLD50Mn)

doPlot = function(LFPlot) {
 dum = subset(GwthResultsLM50.5MM, Zone == LFPlot)
 ggobj = ggplot(data = dum, aes(y=eLML, x=as.factor(BlockNo))) + 
  xlab("BlockNo") +
  ylab("eLML (mm)") +
  labs(title= dum$Zone, size=10)+
  ylim(min(dum$Bottom), max(dum$Top))+
  geom_boxplot(outlier.colour = "black", outlier.size = 3)+
  geom_hline(yintercept=dum$LML, colour = 'red', linetype= 3)+
  theme_bw()+
  theme(legend.title=element_blank(),
        legend.text = element_text(size=14),
        axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=14),
        legend.position="none")
 ggsave(sprintf("%s_eLML_5mmplot.tiff", LFPlot, width = 6, height = 8, units = "cm"))
 print(ggobj)
}
lapply(unique(GwthResultsLM50.5MM$Zone), doPlot)


BlockSumStats5mm<-ddply(GwthResultsLM50.5MM,.(BlockNo, Zone), summarize,  n = length(SiteCode), 
                        mn.L50 = mean(LD50, na.rm=T), mn.LCI50 = mean(Ld50BootL95, na.rm=T), mn.UCI50 = mean(Ld50BootU95, na.rm=T),
                        mn.L95 = mean(LD95, na.rm=T), mn.LCI95 = mean(Ld95BootL95, na.rm=T), mn.UCI95 = mean(Ld95BootU95, na.rm=T),
                        mn.IQR = mean(IQR, na.rm=T), sd.IQR = sd(IQR, na.rm=T),
                        mn.pct.L50 = mean(PctL50, na.rm=T), sd.pct.L50 = mean(PctL50, na.rm=T),
                        mn.Bootrange.L50 = mean(Ld50BootRange, na.rm=T), sd.Bootrange.L50 = mean(Ld50BootRange, na.rm=T),
                        mn.eLML = mean(eLML, na.rm=T) , sd.eLML = sd(eLML, na.rm=T), se.eLML = sd(eLML, na.rm=T)/sqrt(length(eLML)))

doPlot = function(LFPlot) {
 dum = subset(GwthResults, Zone == LFPlot)
 ggobj = ggplot(data = dum, aes(x=LD50,  y=eLML)) + 
  geom_point(aes(colour=GrowthSite), size=3)+
  xlab(bquote(''~LM['50%']~'(mm)')) + ylab(bquote(''~eLML['']~'(mm)'))+
  geom_smooth(method=lm, se=F, color='grey', fullrange=F, size=1.2, color='black')+
  #geom_text_repel(aes(label=BlockNo), size=3)+
  #ggtitle(paste(dum$SubBlockNo, FishYear))+
  #labs(title= Yeardum$SubBlockNo, size=10)+
  #geom_histogram(binwidth=50)+
  theme_bw()+
  #scale_color_identity()+ #this makes sure the color follows the color argument above in aes()
  # theme(legend.position=c(0.1, 0.8))+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(size=14))+
  theme(axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=14))+
  theme(axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=14))
}
lapply(unique(GwthResults$Zone), doPlot)

#
##                   PCT LML protection
#
doPlot = function(LFPlot) {
 dum = subset(GwthResultsLM50.5MM, Zone == LFPlot)
 ggobj = ggplot(data = dum, aes(y=Pctless.LML) + 
  xlab("BlockNo") +
  ylab("Percentage Population Protected") +
  labs(title= dum$Zone, size=10)+
  #ylim(min(dum$sd.eLML-10), max(dum$sd.eLML+10))+
  geom_boxplot(outlier.colour = "black", outlier.size = 3)+
  #geom_hline(yintercept=dum$LML, colour = 'red', linetype= 3)+
  theme_bw()+
  theme(legend.title=element_blank(),
        legend.text = element_text(size=14),
        axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=14),
        legend.position="none")
 ggsave(sprintf("%s_PctProtected.tiff", LFPlot))
 print(ggobj)
}
lapply(unique(GwthResultsLM50.5MM$Zone), doPlot)


#HISTO of percentage protection
ggplot(data = GwthResults, aes(x=Pctless.LML)) + 
 xlab("Percent") +
 ylab("Number of sites") +
 xlim(0,50)+
 #labs(title= dum$Zone, size=10)+
 #ylim(min(dum$sd.eLML-10), max(dum$sd.eLML+10))+
 geom_histogram(breaks=seq(0, 50, by = 5), binwidth=5)+
 #geom_hline(yintercept=dum$LML, colour = 'red', linetype= 3)+
 theme_bw()+
 theme(legend.title=element_blank(),
       legend.text = element_text(size=14),
       axis.title.x = element_text(size=14),
       axis.text.x  = element_text(size=14),
       axis.title.y = element_text(size=14),
       axis.text.y  = element_text(size=14),
       legend.position="none")+
 facet_grid(Zone~ .)





