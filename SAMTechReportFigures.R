library(car)
library(MASS)
library(boot)
library(dplyr)
library(plyr)
library(gdata)
library(ggplot2)
library(multcompView)
#sources
source("D:/GitCode/r-AbSpatialAnalyses/GraphsUtils.r") # source of the TukeyHSD letters in ggplots

#########
# LOAD      GwthResults050916.RData from R_stuff/SAM/Logistic
#########
keep(GwthResults, SamFilter, eLMLResults, eLMLResults75,  SAMILResults, sure = T)

GwthResults$PctL50<-GwthResults$N.underLD50/GwthResults$n*100

GwthResults$Zone<-as.factor(GwthResults$Zone)

####RESULTS ANALYSIS

#                    HISTOGRAM CI RANGE
# SAMfilter contains the full dataset prior to removing the datasets with Ld50 bootrange > 1 s.d.
mn_ld50ci<-mean(GwthResults$Ld50BootRange, na.rm=T)
#md_ld50ci<-median(GwthResults$Ld50BootRange, na.rm=T)
Usd_ld50ci<-mn_ld50ci+sd(GwthResults$Ld50BootRange, na.rm=T)
Lsd_ld50ci<-mn_ld50ci-sd(GwthResults$Ld50BootRange, na.rm=T)

#computation of the standard error of the mean
sem<-sd(SamFilter$Ld50BootRange, na.rm=T)/sqrt(length(SamFilter$Ld50BootRange))
#95% confidence intervals of the mean
c(mn_ld50ci-2*sem,mn_ld50ci+2*sem)


####Histogram of CIrangeL50
ggplot(data = GwthResults, aes(x=Ld50BootRange)) + 
 geom_histogram(bins = 30)+
 xlab(bquote(~CI['Range']~'LM'['50%']))+
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


max_ld50ci<-max(GwthResults$Ld50BootRange, na.rm=T)

####Histogram of CIrangeL50
ggplot(data = SamResults, aes(x=Ld50BootRange)) + 
 geom_histogram(bins = 30)+
 xlab(bquote(~CI['Range']~'LM'['50%']))+
 #ylim(0,120)+
 geom_vline(xintercept=mean(SamResults$Ld50BootRange, na.rm=T), colour = 'red', linetype= 3, size=1.5)+
 geom_vline(xintercept=max(GwthResults$Ld50BootRange, na.rm=T),  linetype= 3, size=1.2)+
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

#############################
#  l50% by zone figure and ANOVA
#############################

#anova L50 by Zone
boxcox(SamFilterIL$LD50^2~SamFilterIL$Zone)
fit<-aov(LD50^2~Zone, data=SamFilterIL)
anova(fit)
par(mfrow = c(2,2))
plot(fit)
par(mfrow = c(1,1))
tHSDlm<- TukeyHSD(fit, ordered = FALSE, conf.level = 0.95)
tHSDlm

#set working dataframe for Tukey label function
ASM<-SamFilterIL


#Boxplot by maturity L%
ggplot(SamFilterIL, aes(x=Zone, y=LD50)) + 
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
BlockSumStats<-ddply(SamFilterIL,.(BlockNo, Zone), summarize,  n = length(SiteCode), 
                     mn.L50 = mean(LD50, na.rm=T), mn.LCI50 = mean(Ld50BootL95, na.rm=T), mn.UCI50 = mean(Ld50BootU95, na.rm=T),
                     mn.L95 = mean(LD95, na.rm=T), mn.LCI95 = mean(Ld95BootL95, na.rm=T), mn.UCI95 = mean(Ld95BootU95, na.rm=T),
                     mn.IQR = mean(IQR, na.rm=T), sd.IQR = sd(IQR, na.rm=T),
                     mn.pct.L50 = mean(PctL50, na.rm=T), sd.pct.L50 = mean(PctL50, na.rm=T),
                     mn.Bootrange.L50 = mean(Ld50BootRange, na.rm=T), sd.Bootrange.L50 = mean(Ld50BootRange, na.rm=T),
                     mn.SLmax = mean(SLmax, na.rm=T))
zoneSumStats<-ddply(GwthResults,.(Zone), summarize,  n = length(SiteCode), 
                     mn.L50 = mean(LD50, na.rm=T), mn.LCI50 = mean(Ld50BootL95, na.rm=T), mn.UCI50 = mean(Ld50BootU95, na.rm=T),
                     rangeL50 = max(LD50, na.rm=T) - min(LD50, na.rm=T),
                     mn.L95 = mean(LD95, na.rm=T), mn.LCI95 = mean(Ld95BootL95, na.rm=T), mn.UCI95 = mean(Ld95BootU95, na.rm=T),
                     mn.IQR = mean(IQR, na.rm=T), sd.IQR = sd(IQR, na.rm=T),
                     mn.pct.L50 = mean(PctL50, na.rm=T), sd.pct.L50 = mean(PctL50, na.rm=T),
                     mn.Bootrange.L50 = mean(Ld50BootRange, na.rm=T), sd.Bootrange.L50 = mean(Ld50BootRange, na.rm=T),
                     mn.SLmax = mean(SLmax, na.rm=T))
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
# pick <- which(GwthResults$Ld50BootRange >10)
# IQRlm <- GwthResults[-pick,]
# IQRlm <- droplevels(IQRlm)


#anova diferences L%
boxcox(GwthResults$Ld50BootRange~GwthResults$PctL50)
fit<-lm(Ld50BootRange~IQR, data=GwthResults)
summary(fit)
anova(fit)
par(mfrow = c(2,2))
plot(fit)
par(mfrow = c(1,1))


ggplot(data = GwthResults, aes(x=PctL50,  y=Ld50BootRange))+#, color=ifelse(Ld50BootRange>10, 'red', 'black'))) + 
  geom_point()+
  xlab(bquote('Proportion <'~LM['50%']~'(%)')) + ylab(bquote(~CI['Range']~'LM'['50%']~'(mm)'))+
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
ggplot(data=GwthResults, aes(x=PctIQR,  y=Ld50BootRange))+#, color=ifelse(Ld50BootRange>10, 'red', 'black'))) + 
 geom_point()+
 xlab(bquote(~IQ['Range']~'%')) + 
 ylab(bquote(~CI['Range']~'LM'['50%']~'(mm)'))+
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
boxcox(GwthResults$Ld50BootRange~GwthResults$PctIQR)
fit<-lm(GwthResults$Ld50BootRange~GwthResults$PctIQR)
summary(fit)
anova(fit)
par(mfrow = c(2,2))
plot(fit)
par(mfrow = c(1,1))

#Range of IQ against CIrangeL50%
plot(GwthResults$IQR, GwthResults$Ld50BootRange)



#anova diferences L%
boxcox(GwthResults$Ld50BootRange~GwthResults$IQR)
fit<-lm(Ld50BootRange~IQR, data=GwthResults)
summary(fit)
anova(fit)
par(mfrow = c(2,2))
plot(fit)
par(mfrow = c(1,1))


ggplot(data = GwthResults, aes(x=IQR,  y=Ld50BootRange))+#, color=ifelse(Ld50BootRange>10, 'red', 'black'))) + 
  geom_point()+
  xlab(bquote(~IQ['range']~'(mm)')) + ylab(bquote(~CI['Range']~'LM'['50%']~'(mm)'))+
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
#  END-  IQrange (L75-L25) CIrange comparison at l50 
#############################

#
#############################
#    Figure 6 L50 and L95 CIrange comparison
#############################
#Ld bootstrap range

rangeCI95<-as.data.frame(GwthResults$Ld95BootRange)
colnames(rangeCI95)[1] <- "CIRange"
rangeCI95$LM<-"LM95%"

rangeCI50<-as.data.frame(GwthResults$Ld50BootRange)
colnames(rangeCI50)[1] <- "CIRange"
rangeCI50$LM<-"LM50%"

rangeCI75<-as.data.frame(GwthResults$Ld75BootRange)
colnames(rangeCI75)[1] <- "CIRange"
rangeCI75$LM<-"LM75%"

rangeCI85<-as.data.frame(GwthResults$Ld85BootRange)
colnames(rangeCI85)[1] <- "CIRange"
rangeCI85$LM<-"LM85%"

rangeCI90<-as.data.frame(GwthResults$Ld90BootRange)
colnames(rangeCI90)[1] <- "CIRange"
rangeCI90$LM<-"LM90%"


CIRange<-rbind(rangeCI50, rangeCI75, rangeCI85, rangeCI90, rangeCI95)
rm(rangeCI50, rangeCI75, rangeCI85, rangeCI90, rangeCI95)


#anova diferences L%
boxcox(CIRange$CIRange~CIRange$LM)
fit<-aov(log(CIRange)~LM, data=CIRange)
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
  xlab("Maturity Estimate") + ylab(expression(paste(CI['Range']~'(mm)')))+
 ylim(3,25)+
  geom_boxplot(outlier.colour = "black", outlier.size = 3)+
  theme_bw()+#white background
  theme(legend.position="none",
        axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=14, angle = 90, hjust = 1),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=14))+
geom_text(data = generate_label_df(tHSDlm, "LM"), aes(x = plot.labels, y = 3, label = labels))
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
#$$$$$$$$$$                   ELML                             $$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$



#Boxplot by MLStc by zone
ggplot(GwthResults, aes(x=Zone, y=eLML50)) + 
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
#           Plots for     eLML   
#########################
#
## Routine Outlier Removal: Unusual small LM50
pick <- which(GwthResults$SiteCode =='780_2005_11')
outlier <- GwthResults[pick,]
GwthResults <- GwthResults[-pick,]



# add ylimits for plots
BlkeLMLmin<-ddply(GwthResults,.(BlockNo), summarize,  eLMLmin = min(eLML50.1y, na.rm=T))
GwthResultsX<-left_join(GwthResults, BlkeLMLmin, by = 'BlockNo')

BlkSLmax<-ddply(GwthResults,.(BlockNo), summarize,  BlkSLmax = max(SLq95, na.rm=T))
GwthResultsX<-left_join(GwthResultsX, BlkSLmax, by = 'BlockNo')

setwd("D:/Fisheries Research/Abalone/SAM")

#
#                         LM50      1YR
#
doPlot = function(LFPlot) {
 dum = subset(GwthResultsX, Zone == LFPlot)
 ggobj = ggplot(data = dum, aes(y=eLML50.1y, x=as.factor(BlockNo))) + 
  xlab("Block") +
  ylab(expression(paste('LM'['50%']~'+1 yrs (mm)')))+ 
  labs(title= dum$Zone, size=10)+
  ylim(min(dum$eLMLmin-2), max(dum$BlkSLmax+2))+
  geom_boxplot(outlier.colour = "black", outlier.size = 3)+
  geom_point(data = dum, aes(y=BlkSLmax, x=as.factor(BlockNo)), shape =6)+
  geom_hline(yintercept=dum$LML, colour = 'red', linetype= 3)+
  theme_bw()+
  theme(legend.title=element_blank(),
        legend.text = element_text(size=14),
        axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=14),
        legend.position="none")
 ggsave(sprintf("50.1y_%s_eLMLplot.tiff", LFPlot, width = 4, height = 6, units = "cm"))
print(ggobj)
}
lapply(unique(GwthResultsX$Zone), doPlot)

#
#                         LM50      2YR
#
doPlot = function(LFPlot) {
  dum = subset(GwthResultsX, Zone == LFPlot)
  ggobj = ggplot(data = dum, aes(y=eLML50.2y, x=as.factor(BlockNo))) + 
    xlab("Block") +
    ylab(expression(paste('LM'['50%']~'+2 yrs (mm)')))+ 
    labs(title= dum$Zone, size=10)+
    ylim(min(dum$eLMLmin-2), max(dum$BlkSLmax+2))+
    geom_boxplot(outlier.colour = "black", outlier.size = 3)+
    geom_point(data = dum, aes(y=BlkSLmax, x=as.factor(BlockNo)), shape =6)+
    geom_hline(yintercept=dum$LML, colour = 'red', linetype= 3)+
    theme_bw()+
    theme(legend.title=element_blank(),
          legend.text = element_text(size=14),
          axis.title.x = element_text(size=14),
          axis.text.x  = element_text(size=14),
          axis.title.y = element_text(size=14),
          axis.text.y  = element_text(size=14),
          legend.position="none")
  ggsave(sprintf("50.2y_%s_eLMLplot.tiff", LFPlot, width = 4, height = 6, units = "cm"))
  print(ggobj)
}
lapply(unique(GwthResultsX$Zone), doPlot)

#
#                         LM50      3YR
#
doPlot = function(LFPlot) {
  dum = subset(GwthResultsX, Zone == LFPlot)
  ggobj = ggplot(data = dum, aes(y=eLML50.3y, x=as.factor(BlockNo))) + 
    xlab("Block") +
    ylab(expression(paste('LM'['50%']~'+3 yrs (mm)')))+ 
    labs(title= dum$Zone, size=10)+
    ylim(min(dum$eLMLmin-2), max(dum$BlkSLmax+2))+
    geom_boxplot(outlier.colour = "black", outlier.size = 3)+
    geom_point(data = dum, aes(y=BlkSLmax, x=as.factor(BlockNo)), shape =6)+
    geom_hline(yintercept=dum$LML, colour = 'red', linetype= 3)+
    theme_bw()+
    theme(legend.title=element_blank(),
          legend.text = element_text(size=14),
          axis.title.x = element_text(size=14),
          axis.text.x  = element_text(size=14),
          axis.title.y = element_text(size=14),
          axis.text.y  = element_text(size=14),
          legend.position="none")
  ggsave(sprintf("50.3y_%s_eLMLplot.tiff", LFPlot, width = 4, height = 6, units = "cm"))
  print(ggobj)
}
lapply(unique(GwthResultsX$Zone), doPlot)
##########################################################################################
#
#                         LM90      1YR
#
doPlot = function(LFPlot) {
  dum = subset(GwthResultsX, Zone == LFPlot)
  ggobj = ggplot(data = dum, aes(y=eLML90.1y, x=as.factor(BlockNo))) + 
    xlab("Block") +
    ylab(expression(paste('LM'['90%']~'+1 yrs (mm)')))+ 
    labs(title= dum$Zone, size=10)+
    ylim(min(dum$eLMLmin-2), max(dum$BlkSLmax+2))+
    geom_boxplot(outlier.colour = "black", outlier.size = 3)+
    geom_point(data = dum, aes(y=BlkSLmax, x=as.factor(BlockNo)), shape =6)+
    geom_hline(yintercept=dum$LML, colour = 'red', linetype= 3)+
    theme_bw()+
    theme(legend.title=element_blank(),
          legend.text = element_text(size=14),
          axis.title.x = element_text(size=14),
          axis.text.x  = element_text(size=14),
          axis.title.y = element_text(size=14),
          axis.text.y  = element_text(size=14),
          legend.position="none")
  ggsave(sprintf("90.1y_%s_eLMLplot.tiff", LFPlot, width = 4, height = 6, units = "cm"))
  print(ggobj)
}
lapply(unique(GwthResultsX$Zone), doPlot)

#
#                         LM90      2YR
#
doPlot = function(LFPlot) {
  dum = subset(GwthResultsX, Zone == LFPlot)
  ggobj = ggplot(data = dum, aes(y=eLML90.2y, x=as.factor(BlockNo))) + 
    xlab("Block") +
    ylab(expression(paste('LM'['90%']~'+2 yrs (mm)')))+ 
    labs(title= dum$Zone, size=10)+
    ylim(min(dum$eLMLmin-2), max(dum$BlkSLmax+2))+
    geom_boxplot(outlier.colour = "black", outlier.size = 3)+
    geom_point(data = dum, aes(y=BlkSLmax, x=as.factor(BlockNo)), shape =6)+
    geom_hline(yintercept=dum$LML, colour = 'red', linetype= 3)+
    theme_bw()+
    theme(legend.title=element_blank(),
          legend.text = element_text(size=14),
          axis.title.x = element_text(size=14),
          axis.text.x  = element_text(size=14),
          axis.title.y = element_text(size=14),
          axis.text.y  = element_text(size=14),
          legend.position="none")
  ggsave(sprintf("90.2y_%s_eLMLplot.tiff", LFPlot, width = 4, height = 6, units = "cm"))
  print(ggobj)
}
lapply(unique(GwthResultsX$Zone), doPlot)

#
#                         LM90      3YR
#
doPlot = function(LFPlot) {
  dum = subset(GwthResultsX, Zone == LFPlot)
  ggobj = ggplot(data = dum, aes(y=eLML90.3y, x=as.factor(BlockNo))) + 
    xlab("Block") +
    ylab(expression(paste('LM'['90%']~'+3 yrs (mm)')))+ 
    labs(title= dum$Zone, size=10)+
    ylim(min(dum$eLMLmin-2), max(dum$BlkSLmax+2))+
    geom_boxplot(outlier.colour = "black", outlier.size = 3)+
    geom_point(data = dum, aes(y=BlkSLmax, x=as.factor(BlockNo)), shape =6)+
    geom_hline(yintercept=dum$LML, colour = 'red', linetype= 3)+
    theme_bw()+
    theme(legend.title=element_blank(),
          legend.text = element_text(size=14),
          axis.title.x = element_text(size=14),
          axis.text.x  = element_text(size=14),
          axis.title.y = element_text(size=14),
          axis.text.y  = element_text(size=14),
          legend.position="none")
  ggsave(sprintf("90.3y_%s_eLMLplot.tiff", LFPlot, width = 4, height = 6, units = "cm"))
  print(ggobj)
}
lapply(unique(GwthResultsX$Zone), doPlot)
#
##                   PCT LML protection
#

################
# LM50
################

#HISTO of percentage protection

#                           1Yr

ggplot(data = GwthResults, aes(x=PPLM50.1yr)) + 
 xlab(expression(paste('Pct. Popn. Prt. LM'['50%'])))+
 ylab("Number of sites") +
 xlim(0,50)+
 ylim(0,70)+
 geom_histogram(breaks=seq(0, 50, by = 5), binwidth=5)+
 theme_bw()+
 theme(legend.title=element_blank(),
       legend.text = element_text(size=14),
       axis.title.x = element_text(size=14),
       axis.text.x  = element_text(size=14),
       axis.title.y = element_text(size=14),
       axis.text.y  = element_text(size=14),
       legend.position="none")+
 facet_grid(Zone~ .)

#                           2Yr

ggplot(data = GwthResults, aes(x=PPLM50.2yr)) + 
  xlab(expression(paste('Pct. Popn. Prt. LM'['50%'])))+
  ylab("Number of sites") +
  xlim(0,50)+
  ylim(0,70)+
  geom_histogram(breaks=seq(0, 50, by = 5), binwidth=5)+
  theme_bw()+
  theme(legend.title=element_blank(),
        legend.text = element_text(size=14),
        axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=14),
        legend.position="none")+
  facet_grid(Zone~ .)

#                           3Yr

ggplot(data = GwthResults, aes(x=PPLM50.3yr)) + 
  xlab(expression(paste('Pct. Popn. Prt. LM'['50%'])))+
  ylab("Number of sites") +
  xlim(0,50)+
  ylim(0,70)+
  geom_histogram(breaks=seq(0, 50, by = 5), binwidth=5)+
  theme_bw()+
  theme(legend.title=element_blank(),
        legend.text = element_text(size=14),
        axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=14),
        legend.position="none")+
  facet_grid(Zone~ .)


################
# LM90
################

#HISTO of percentage protection

#                           1Yr

ggplot(data = GwthResults, aes(x=PPLM90.1yr)) + 
  xlab(expression(paste('Pct. Popn. Prt. LM'['90%'])))+
  ylab("Number of sites") +
  xlim(0,50)+
  ylim(0,70)+
  geom_histogram(breaks=seq(0, 50, by = 5), binwidth=5)+
  theme_bw()+
  theme(legend.title=element_blank(),
        legend.text = element_text(size=14),
        axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=14),
        legend.position="none")+
  facet_grid(Zone~ .)

#                           2Yr

ggplot(data = GwthResults, aes(x=PPLM90.2yr)) + 
  xlab(expression(paste('Pct. Popn. Prt. LM'['90%'])))+
  ylab("Number of sites") +
  xlim(0,50)+
  ylim(0,70)+
  geom_histogram(breaks=seq(0, 50, by = 5), binwidth=5)+
  theme_bw()+
  theme(legend.title=element_blank(),
        legend.text = element_text(size=14),
        axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=14),
        legend.position="none")+
  facet_grid(Zone~ .)

#                           3Yr

ggplot(data = GwthResults, aes(x=PPLM90.3yr)) + 
  xlab(expression(paste('Pct. Popn. Prt. LM'['90%'])))+
  ylab("Number of sites") +
  xlim(0,50)+
  ylim(0,70)+
  geom_histogram(breaks=seq(0, 50, by = 5), binwidth=5)+
  theme_bw()+
  theme(legend.title=element_blank(),
        legend.text = element_text(size=14),
        axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=14),
        legend.position="none")+
  facet_grid(Zone~ .)



#BY Block



doPlot = function(LFPlot) {
 dum = subset(GwthResults, Zone == LFPlot)
 ggobj = ggplot(data = dum, aes(x=PPLM50)) + 
  xlab(expression(paste('Pct Popn. Prt. LM'['50%'])))+
  ylab("Number of sites") +
  xlim(0,50)+
  ylim(0,12)+
  #ylim(min(length(dum$PPLM50))+
  geom_histogram(breaks=seq(0, 50, by = 5), binwidth=5)+
  theme_bw()+
  theme(legend.title=element_blank(),
        legend.text = element_text(size=14),
        axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=14),
        legend.position="none")+
  facet_grid(BlockNo~ .)
 print(ggobj)
}
lapply(unique(GwthResults$Zone), doPlot)

#EZ



################
# LM90
################

#HISTO of percentage protection
ggplot(data = GwthResults, aes(x=PPLM90)) + 
 xlab(expression(paste('Pct. Popn. Prt. LM'['90%'])))+
 ylab("Number of sites") +
 xlim(0,50)+
 ylim(0,60)+
 geom_histogram(breaks=seq(0, 50, by = 5), binwidth=5)+
 theme_bw()+
 theme(legend.title=element_blank(),
       legend.text = element_text(size=14),
       axis.title.x = element_text(size=14),
       axis.text.x  = element_text(size=14),
       axis.title.y = element_text(size=14),
       axis.text.y  = element_text(size=14),
       legend.position="none")+
 facet_grid(Zone~ .)


