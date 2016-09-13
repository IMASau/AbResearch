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
  xlab("Zone") +  ylab(bquote(~L['50%']~'(mm)'))+
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
                     mn.eLML = mean(eLML, na.rm=T) , sd.eLML = sd(eLML, na.rm=T), se.eLML = sd(eLML, na.rm=T)/sqrt(length(eLML)))
                     # mn.eLMLbootL95 = mean(eLMLbootL95, na.rm=T) , sd.eLMLbootL95 = sd(eLMLbootL95, na.rm=T),
                     # mn.eLMLbootU95 = mean(eLMLbootU95, na.rm=T) , sd.eLMLbootU95 = sd(eLMLbootU95, na.rm=T),
                     # diffLML = mean(LMLDiff, na.rm=T))
write.csv(BlockSumStats, file= "blockSAMstats.csv")

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
  xlab(bquote('Percentage sample <'~L['50%']~'.')) + ylab(bquote('C.I. Range'~L['50%']~'(mm)'))+
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
  xlab(bquote(~IQ['range']~'(mm)')) + ylab(bquote('C.I. Range'~L['50%']~'(mm)'))+
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


####Histogram of CIrangeL50
ggplot(data = GwthResults, aes(x=Ld50BootRange)) + 
  geom_histogram(bins = 30)+
  xlab(bquote(~CI['range']~'L'['50%']))+
  #ylim(0,120)+
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
rangeCI95$LM<-"L95%"

rangeCI50<-as.data.frame(GwthResults$Ld50BootRange)
colnames(rangeCI50)[1] <- "CIRange"
rangeCI50$LM<-"L50%"
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
  xlab("Maturity Estimate") + labs(y=expression(paste("C.I. Range (mm)")))+
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
  xlab(bquote(~M['range']~'(mm)')) + ylab(bquote('C.I. Range'~L['50%']~'(mm)'))+
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
#
doPlot = function(LFPlot) {
  dum = subset(BlockSumStats, Zone == LFPlot)
  ggobj = ggplot(data = dum, aes(y=mn.eLML, x=as.factor(BlockNo))) + 
    xlab("BlockNo") +
    ylab("eLML (mm)") +
    labs(title= dum$Zone, size=10)+
    #ylim(min(dum$sd.eLML-10), max(dum$sd.eLML+10))+
    geom_point(position=position_dodge(), stat="identity", size =3) +
    geom_errorbar(aes(ymin=dum$mn.eLML-dum$se.eLML, ymax=dum$mn.eLML+dum$se.eLML),
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
lapply(unique(BlockSumStats$Zone), doPlot)

doPlot = function(LFPlot) {
  dum = subset(GwthResults, Zone == LFPlot)
  ggobj = ggplot(data = dum, aes(y=eLML, x=as.factor(BlockNo))) + 
    xlab("BlockNo") +
    ylab("eLML (mm)") +
    labs(title= dum$Zone, size=10)+
    #ylim(min(dum$sd.eLML-10), max(dum$sd.eLML+10))+
    geom_boxplot(outlier.colour = "black", outlier.size = 3)+
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
lapply(unique(BlockSumStats$Zone), doPlot)


#eLML boxplot from the BootU95 of L50%
doPlot = function(LFPlot) {
  dum = subset(GwthResults, Zone == LFPlot)
  ggobj = ggplot(data = dum, aes(y=eLMLbootU95, x=as.factor(BlockNo))) + 
    xlab("BlockNo") +
    ylab("eLML (UCI) (mm)") +
    labs(title= dum$Zone, size=10)+
    #ylim(min(dum$sd.eLML-10), max(dum$sd.eLML+10))+
    geom_boxplot(outlier.colour = "black", outlier.size = 3)+
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
lapply(unique(BlockSumStats$Zone), doPlot)

