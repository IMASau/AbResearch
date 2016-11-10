library(car)
library(MASS)
library(boot)
library(dplyr)
library(plyr)
library(gdata)
library(grid)
library(ggplot2)
library(multcompView)
#sources
source("D:/GitCode/r-AbSpatialAnalyses/GraphsUtils.r") # source of the TukeyHSD letters in ggplots

# function for computing mean, DS, max and min values in boxplots
min.mean.sd.max <- function(x) {
 r <- c(min(x), mean(x) - sd(x), mean(x), mean(x) + sd(x), max(x))
 names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
 r
}

#########
# LOAD      SAMILResults271016.RData from R_stuff/SAM/Logistic
#########
#keep(GwthResults, SAMILResults, SamFilterIL, SamFilter,sure = T)



GwthResults$Zone<-as.factor(GwthResults$Zone)
####RESULTS ANALYSIS

#                    HISTOGRAM CI RANGE
# SAMfilter contains the full dataset prior to removing the datasets with Ld50 bootrange > 1 s.d.
mn_ld50ci<-mean(GwthResults$Ld50BootRange, na.rm=T)
#md_ld50ci<-median(GwthResults$Ld50BootRange, na.rm=T)
Usd_ld50ci<-mn_ld50ci+sd(GwthResults$Ld50BootRange, na.rm=T)
Lsd_ld50ci<-mn_ld50ci-sd(GwthResults$Ld50BootRange, na.rm=T)

#computation of the standard error of the mean
sem<-sd(GwthResults$Ld50BootRange, na.rm=T)/sqrt(length(GwthResults$Ld50BootRange))
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
  #geom_boxplot(outlier.colour = "black", outlier.size = 3)+
 stat_summary(fun.data = min.mean.sd.max, geom = "boxplot", outlier.colour = "black", outlier.size = 3)+ 
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
                     Pct.M = mean(Pct.M, na.rm=T),
                     mn.LM50 = mean(LD50, na.rm=T), #mn.LCI50 = mean(Ld50BootL95, na.rm=T), mn.UCI50 = mean(Ld50BootU95, na.rm=T),
                     mn.L90 = mean(LD90, na.rm=T), #mn.LCI950 = mean(Ld90BootL95, na.rm=T), mn.UCI90 = mean(Ld90BootU95, na.rm=T),
                     mn.IQR = mean(IQR, na.rm=T), sd.IQR = sd(IQR, na.rm=T),
                     mn.pct.L50 = mean(PctL50, na.rm=T), #sd.pct.L50 = mean(PctL50, na.rm=T),
                     mn.Bootrange.L50 = mean(Ld50BootRange, na.rm=T), #sd.Bootrange.L50 = mean(Ld50BootRange, na.rm=T),
                     mn.Bootrange.L90 = mean(Ld90BootRange, na.rm=T), #sd.Bootrange.L50 = mean(Ld50BootRange, na.rm=T),
                     mn.SLmax.q95 = mean(SLq95, na.rm=T))
zoneSumStats<-ddply(SamFilterIL,.(Zone), summarize,  n = length(SiteCode), 
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
plot(SamFilterIL$PctL50, SamFilterIL$Ld50BootRange)

# 
#remove outliers from dataset and run lm
pick <- which(SamFilterIL$PctL50 >70)
IQRlm <- SamFilterIL[-pick,]
IQRlm <- droplevels(IQRlm)


#anova diferences L%
boxcox(IQRlm$Ld50BootRange~IQRlm$PctL50)
fit<-lm(Ld50BootRange~IQR, data=IQRlm)
summary(fit)
anova(fit)
par(mfrow = c(2,2))
plot(fit)
par(mfrow = c(1,1))


ggplot(data = SamFilterIL, aes(x=PctL50,  y=Ld50BootRange, color=ifelse(PctL50>70, 'red', 'black'))) + 
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
ggplot(data=SamFilterIL, aes(x=PctIQR,  y=Ld50BootRange, color=ifelse(PctIQR>30, 'red', 'black'))) + 
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
boxcox(IQRlm$Ld50BootRange~IQRlm$PctIQR)
fit<-lm(log(IQRlm$Ld50BootRange)~IQRlm$PctIQR)
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


ggplot(data = SamFilterIL, aes(x=IQR,  y=Ld50BootRange))+#, color=ifelse(Ld50BootRange>10, 'red', 'black'))) + 
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

rangeCI95<-as.data.frame(SamFilterIL$Ld95BootRange)
colnames(rangeCI95)[1] <- "CIRange"
rangeCI95$LM<-"LM95%"

rangeCI50<-as.data.frame(SamFilterIL$Ld50BootRange)
colnames(rangeCI50)[1] <- "CIRange"
rangeCI50$LM<-"LM50%"

rangeCI75<-as.data.frame(SamFilterIL$Ld75BootRange)
colnames(rangeCI75)[1] <- "CIRange"
rangeCI75$LM<-"LM75%"

rangeCI85<-as.data.frame(SamFilterIL$Ld85BootRange)
colnames(rangeCI85)[1] <- "CIRange"
rangeCI85$LM<-"LM85%"

rangeCI90<-as.data.frame(SamFilterIL$Ld90BootRange)
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
 ylim(0,20)+
 stat_summary(fun.data = min.mean.sd.max, geom = "boxplot", outlier.colour = "black", outlier.size = 3)+ 
  theme_bw()+#white background
  theme(legend.position="none",
        axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=14, angle = 90, hjust = 1),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=14))+
geom_text(data = generate_label_df(tHSDlm, "LM"), aes(x = plot.labels, y = 1, label = labels))
ddply(CIRange,.(LM), summarize,  M = mean(CIRange, na.rm=T))

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
#ADD MArket measure data MaxSL to gwthdata.
MMMaxSL<-read.csv('D:/Fisheries Research/Abalone/SAM/MMBlockMaxSL.csv')
colnames(MMMaxSL)[3]<-"MM_MaxSL"
colnames(MMMaxSL)[4]<-"MM_q95SL"
MMMaxSL<-MMMaxSL[,2:4]

GwthResults<-left_join(GwthResults,MMMaxSL, by = 'BlockNo')


#filter out match which are > confidnece interval range.
GwthResults$LD50Diff<-GwthResults$LD50-GwthResults$GwthLD50
range(GwthResults$LD50Diff)
hist(GwthResults$LD50Diff)
limit<-0-mean(GwthResults$Ld50BootRange)/2

F.GwthResults<-subset(GwthResults, LD50Diff >= limit)
mean(F.GwthResults$LD50Diff)
sd(F.GwthResults$LD50Diff)

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


#########################
#           Plots for     eLML   
#########################
#

# add ylimits for plots
BlkeLMLmin<-ddply(F.GwthResults,.(BlockNo), summarize,  eLMLmin = min(eLML50.1y, na.rm=T))
GwthResultsX<-left_join(F.GwthResults, BlkeLMLmin, by = 'BlockNo')

BlkSLmax<-ddply(F.GwthResults,.(BlockNo), summarize,  Blk_SLmax = max(SLq95, na.rm=T))
GwthResultsX<-left_join(GwthResultsX, BlkSLmax, by = 'BlockNo')

setwd("D:/Fisheries Research/Abalone/SAM/eLML figures")

#
#                         LM50      1YR
#
doPlot = function(LFPlot) {
 dum = subset(GwthResultsX, Zone == LFPlot)
 ggobj = ggplot(data = dum, aes(y=eLML50.1y, x=as.factor(BlockNo))) + 
  xlab("Block") +
  ylab(expression(paste('LM'['50%']~'+1 yrs (mm)')))+ 
  #labs(title= dum$Zone, size=10)+
  ylim(min(dum$eLMLmin-2), max(dum$MM_q95SL+2))+
  geom_boxplot()+
  stat_summary(fun.y=mean, colour="blue", geom="point", 
               shape=1, size=3)+
    geom_point(data = dum, aes(y=Blk_SLmax, x=as.factor(BlockNo)), shape =6)+
  geom_point(data = dum, aes(y=MM_q95SL, x=as.factor(BlockNo)), shape =6, colour = "red")+
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
    #xlab("Block") +
    ylab(expression(paste('LM'['50%']~'+2 yrs (mm)')))+ 
    #labs(title= dum$Zone, size=10)+
   ylim(min(dum$eLMLmin-2), max(dum$MM_q95SL+2))+
   geom_boxplot()+
   stat_summary(fun.y=mean, colour="blue", geom="point", 
                shape=1, size=3)+
   geom_point(data = dum, aes(y=Blk_SLmax, x=as.factor(BlockNo)), shape =6)+
   geom_point(data = dum, aes(y=MM_q95SL, x=as.factor(BlockNo)), shape =6, colour = "red")+
   geom_hline(yintercept=dum$LML, colour = 'red', linetype= 3)+
   theme_bw()+
   theme(legend.title=element_blank(),
         legend.text = element_text(size=14),
         axis.title.x = element_blank(),
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
    #xlab("Block") +
    ylab(expression(paste('LM'['50%']~'+3 yrs (mm)')))+ 
    #labs(title= dum$Zone, size=10)+
   ylim(min(dum$eLMLmin-2), max(dum$MM_q95SL+2))+
   geom_boxplot()+
   stat_summary(fun.y=mean, colour="blue", geom="point", 
                shape=1, size=3)+
   geom_point(data = dum, aes(y=Blk_SLmax, x=as.factor(BlockNo)), shape =6)+
   geom_point(data = dum, aes(y=MM_q95SL, x=as.factor(BlockNo)), shape =6, colour = "red")+
   geom_hline(yintercept=dum$LML, colour = 'red', linetype= 3)+
   theme_bw()+
   theme(legend.title=element_blank(),
         legend.text = element_text(size=14),
         axis.title.x = element_blank(),
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
    #labs(title= dum$Zone, size=10)+
   ylim(min(dum$eLMLmin-2), max(dum$MM_q95SL+2))+
   geom_boxplot()+
   stat_summary(fun.y=mean, colour="blue", geom="point", 
                shape=1, size=3)+
   geom_point(data = dum, aes(y=Blk_SLmax, x=as.factor(BlockNo)), shape =6)+
   geom_point(data = dum, aes(y=MM_q95SL, x=as.factor(BlockNo)), shape =6, colour = "red")+
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
    #xlab("Block") +
    ylab(expression(paste('LM'['90%']~'+2 yrs (mm)')))+ 
    #labs(title= dum$Zone, size=10)+
   ylim(min(dum$eLMLmin-2), max(dum$MM_q95SL+2))+
   geom_boxplot()+
   stat_summary(fun.y=mean, colour="blue", geom="point", 
                shape=1, size=3)+
   geom_point(data = dum, aes(y=Blk_SLmax, x=as.factor(BlockNo)), shape =6)+
   geom_point(data = dum, aes(y=MM_q95SL, x=as.factor(BlockNo)), shape =6, colour = "red")+
   geom_hline(yintercept=dum$LML, colour = 'red', linetype= 3)+
   theme_bw()+
   theme(legend.title=element_blank(),
         legend.text = element_text(size=14),
         axis.title.x = element_blank(),
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
    #xlab("Block") +
    ylab(expression(paste('LM'['90%']~'+3 yrs (mm)')))+ 
    #labs(title= dum$Zone, size=10)+
   ylim(min(dum$eLMLmin-2), max(dum$MM_q95SL+2))+
   geom_boxplot()+
   stat_summary(fun.y=mean, colour="blue", geom="point", 
                shape=1, size=3)+
   geom_point(data = dum, aes(y=Blk_SLmax, x=as.factor(BlockNo)), shape =6)+
   geom_point(data = dum, aes(y=MM_q95SL, x=as.factor(BlockNo)), shape =6, colour = "red")+
   geom_hline(yintercept=dum$LML, colour = 'red', linetype= 3)+
   theme_bw()+
   theme(legend.title=element_blank(),
         legend.text = element_text(size=14),
         axis.title.x = element_blank(),
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
Zone<-c('BS', "CW", "E", "N", "W")
Ylim<-c(4, 4,80, 20, 15)
PPlimits<-data.frame(Zone, Ylim)
GwthResultsT<-left_join(GwthResultsX, PPlimits, by ='Zone')


doPlot = function(LFPlot) {
 dum = subset(GwthResultsT, Zone == LFPlot)
 ggobj1 = ggplot(data = dum, aes(x=PPLM50.1yr)) + 
  xlab('Pct Protection')+
  ylab("        Number of") +
  ylim(0,max(dum$Ylim[1]))+
  xlim(0,50)+
  geom_histogram(breaks=seq(0, 50, by = 2), binwidth=5)+
  annotate("text", x = 1, y = dum$Ylim[1]*0.95, label = " 1Yr")+
  theme_bw()+
  theme(legend.title=element_blank(),
        legend.text = element_text(size=14),
        axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=10),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=10),
        legend.position="none")
 ggobj2 = ggplot(data = dum, aes(x=PPLM50.2yr)) + 
  ylim(0,max(dum$Ylim[1]))+
  xlim(0,50)+
  geom_histogram(breaks=seq(0, 50, by = 2), binwidth=5)+
  ylab(expression(paste('    LM'['50%']~'datasets')))+ 
  theme_bw()+
  annotate("text", x = 1, y = dum$Ylim[1]*0.95, label = " 2Yr")+
  theme(legend.title=element_blank(),
        legend.text = element_text(size=14),
        axis.title.x = element_blank(),
        axis.text.x  = element_text(size=10),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=10),
        legend.position="none")
 ggobj3 = ggplot(data = dum, aes(x=PPLM50.3yr)) + 
  #ylab(expression(paste('LM'['50%']~'datasets')))+ 
  ylim(0,max(dum$Ylim[1]))+
  xlim(0,50)+
  labs(title= dum$Zone, size=10)+
  geom_histogram(breaks=seq(0, 50, by = 2), binwidth=5)+
  annotate("text", x = 1, y = dum$Ylim[1]*0.95, label = " 3Yr")+
  theme_bw()+
  theme(legend.title=element_blank(),
        legend.text = element_text(size=14),
        axis.title.x = element_blank(),
        axis.text.x  = element_text(size=10),
        axis.title.y = element_blank(),
        axis.text.y  = element_text(size=10),
        legend.position="none")
 grid.newpage()
 ggobj<-grid.draw(rbind(ggplotGrob(ggobj3), ggplotGrob(ggobj2), ggplotGrob(ggobj1), size = "last"))
 print(ggobj)
 #ggsave(sprintf("50_%s_mmplot.tiff", LFPlot, width = 4, height = 6, units = "cm"))
}
lapply(unique(GwthResultsT$Zone), doPlot)



################
# LM90
################

doPlot = function(LFPlot) {
 dum = subset(GwthResultsT, Zone == LFPlot)
 ggobj1 = ggplot(data = dum, aes(x=PPLM90.1yr)) + 
  xlab('Pct Protection')+
  ylab("        Number of") +
  ylim(0,max(dum$Ylim[1]))+
  geom_histogram(breaks=seq(0, 50, by = 2), binwidth=5)+
  annotate("text", x = 1, y = dum$Ylim[1]*0.95, label = " 1Yr")+
  theme_bw()+
  theme(legend.title=element_blank(),
        legend.text = element_text(size=14),
        axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=10),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=10),
        legend.position="none")
 ggobj2 = ggplot(data = dum, aes(x=PPLM90.2yr)) + 
  ylim(0,max(dum$Ylim[1]))+
  geom_histogram(breaks=seq(0, 50, by = 2), binwidth=5)+
  ylab(expression(paste('    LM'['90%']~'datasets')))+ 
  theme_bw()+
  annotate("text", x = 1, y = dum$Ylim[1]*0.95, label = " 2Yr")+
  theme(legend.title=element_blank(),
        legend.text = element_text(size=14),
        axis.title.x = element_blank(),
        axis.text.x  = element_text(size=10),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=10),
        legend.position="none")
 ggobj3 = ggplot(data = dum, aes(x=PPLM90.3yr)) + 
  #ylab(expression(paste('LM'['50%']~'datasets')))+ 
  ylim(0,max(dum$Ylim[1]))+
  labs(title= dum$Zone, size=10)+
  geom_histogram(breaks=seq(0, 50, by = 2), binwidth=5)+
  annotate("text", x = 1, y = dum$Ylim[1]*0.95, label = " 3Yr")+
  theme_bw()+
  theme(legend.title=element_blank(),
        legend.text = element_text(size=14),
        axis.title.x = element_blank(),
        axis.text.x  = element_text(size=10),
        axis.title.y = element_blank(),
        axis.text.y  = element_text(size=10),
        legend.position="none")
 grid.newpage()
 ggobj<-grid.draw(rbind(ggplotGrob(ggobj3), ggplotGrob(ggobj2), ggplotGrob(ggobj1), size = "last"))
 print(ggobj)
 #ggsave(sprintf("90_%s_mmplot.tiff", LFPlot, width = 4, height = 6, units = "cm"))
}
lapply(unique(GwthResultsT$Zone), doPlot)



GwthSumStats<-ddply(GwthResultsX,.(BlockNo, Zone), summarize,  n = length(SiteCode), 
                     eLML50.1y = mean(eLML50.1y, na.rm=T), sd.eLML50.1y = sd(eLML50.1y, na.rm=T),
                     eLML50.2y = mean(eLML50.2y, na.rm=T), sd.eLML50.2y = sd(eLML50.2y, na.rm=T),
                     eLML50.3y = mean(eLML50.3y, na.rm=T), sd.eLML50.3y = sd(eLML50.3y, na.rm=T),
                     eLML90.1y = mean(eLML90.1y, na.rm=T), sd.eLML90.1y = sd(eLML90.1y, na.rm=T),
                     eLML90.2y = mean(eLML90.2y, na.rm=T), sd.eLML90.2y = sd(eLML90.2y, na.rm=T),
                     eLML90.3y = mean(eLML90.3y, na.rm=T), sd.eLML90.3y = sd(eLML90.3y, na.rm=T),
                     
                     PPLM50.1y = mean(PPLM50.1yr, na.rm=T), sd.PPLM50.1y = sd(PPLM50.1yr, na.rm=T),
                     PPLM50.2y = mean(PPLM50.2yr, na.rm=T), sd.PPLM50.2y = sd(PPLM50.2yr, na.rm=T),
                     PPLM50.3y = mean(PPLM50.3yr, na.rm=T), sd.PPLM50.3y = sd(PPLM50.3yr, na.rm=T),
                     PPLM90.1y = mean(PPLM90.1yr, na.rm=T), sd.PPLM90.1y = sd(PPLM90.1yr, na.rm=T),
                     PPLM90.2y = mean(PPLM90.2yr, na.rm=T), sd.PPLM90.2y = sd(PPLM90.2yr, na.rm=T),
                     PPLM90.3y = mean(PPLM90.3yr, na.rm=T), sd.PPLM90.3y = sd(PPLM90.3yr, na.rm=T),
                     SLmax.q95 = max(SLq95, na.rm=T),
                     MMSLmax.q95 = max(MM_q95SL, na.rm=T),
                    LML = max(LML))

GwthSumStats$MAxSLDiff<-GwthSumStats$MMSLmax.q95-GwthSumStats$SLmax.q95

GwthSumStats$LMLdiff2y<-GwthSumStats$eLML50.2y-GwthSumStats$LML


GwthSumStats[,c(1,2,6,28,29,30,31,32)]
