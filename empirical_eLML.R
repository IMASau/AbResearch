s


# # empirical LML
# # Example R code to conduct such a calculation using inverse logistic growth
invlog <- function(x,y,z,L) { # calculates the expected growth increment for length L
  ans <- x/(1+exp(log(19)*(L-y)/(z-y)))
  return(ans)
}

Sites<-unique(SAMILResults$SiteCode)

#####
#     L50%
#####
if (exists("D.eLMLRslts")) 
 rm(D.eLMLRslts)

for(i in Sites){
 choice<-subset(SAMILResults, SiteCode == i)
 param <- c(choice$MaxDL,choice$L50,choice$L95,choice$SigMaxLD50) # MaxDL, L50, L95, SigMax
 choice$eLML.LM50.1 <- choice$LD50  + invlog(param[1], param[2], param[3], choice$LD50)
 choice$eLML.LM50.2 <- choice$eLML.LM50.1 + invlog(param[1], param[2], param[3], choice$eLML.LM50.1)
 choice$eLML.LM50.3 <- choice$eLML.LM50.2 + invlog(param[1], param[2], param[3], choice$eLML.LM50.2)
 
 pick<-choice[,c(2,51:53)]  
 if (exists("D.eLMLRslts"))
  D.eLMLRslts <- rbind(D.eLMLRslts, pick)
 else
  D.eLMLRslts <- pick
}
#pass back to GwthResults
Pick.GwthRsts<-join(SAMILResults, D.eLMLRslts, by='SiteCode')

#####
#     L90%
#####
if (exists("D.eLMLRslts")) 
 rm(D.eLMLRslts)

for(i in Sites){
 choice<-subset(SAMILResults, SiteCode == i)
 param <- c(choice$MaxDL,choice$L50,choice$L95,choice$SigMaxLD50) # MaxDL, L50, L95, SigMax
 choice$eLML.LM90.1 <- choice$LD90 + invlog(param[1], param[2], param[3], choice$LD90)
 choice$eLML.LM90.2 <- choice$eLML.LM90.1 + invlog(param[1], param[2], param[3], choice$eLML.LM90.1)
 choice$eLML.LM90.3 <- choice$eLML.LM90.2 + invlog(param[1], param[2], param[3], choice$eLML.LM90.2)
 
 pick<-choice[,c(2,51:53)]  
 if (exists("D.eLMLRslts"))
  D.eLMLRslts <- rbind(D.eLMLRslts, pick)
 else
  D.eLMLRslts <- pick
}
#pass back to GwthResults
Pick.GwthRsts<-join(Pick.GwthRsts, D.eLMLRslts, by='SiteCode')



#
#########################
#           Plots for eLML L50%
#########################
#
# 
# Pick.GwthRsts<-Pick.GwthRsts[,c(1:56)]
# Pick.GwthRsts<-left_join(Pick.GwthRsts,GwthLD50, by = 'SiteCode')
# plot(Pick.GwthRsts$LD50, Pick.GwthRsts$GrowthLD50)
# Pick.GwthRsts$DiffLD50<-Pick.GwthRsts$LD50-Pick.GwthRsts$GrowthLD50
# hist(Pick.GwthRsts$DiffLD50)

BSZ.GwthResults<-subset(D.GwthResults, Zone == "BS")
Sub.GwthRsts<-subset(D.GwthResults, Zone != "BS")
mean(Sub.GwthRsts$DiffLD50)
sd(Sub.GwthRsts$DiffLD50)
q95<-quantile(Sub.GwthRsts$DiffLD50, 0.975)
q05<-quantile(Sub.GwthRsts$DiffLD50, 0.025)
Pick.GwthRsts<-subset(Sub.GwthRsts, DiffLD50 <=q95 & DiffLD50 >=q05)

Pick.GwthRsts<-rbind(Pick.GwthRsts, BSZ.GwthResults)



####Histogram of CIrangeL50
ggplot(data = Pick.GwthRsts, aes(x=DiffLD50)) + 
 geom_histogram(bins = 10)+
 xlab(bquote(~Difference~'LM'['50%']))+
 scale_x_continuous(breaks=c(-30,-20,-10, 0, 10,20)) +
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

ggplot(data = SubPick.GwthRsts, aes(x=LD50,  y=GrowthLD50)) + 
 geom_point()+
 xlab(bquote('Growth Data LM'['50%']~'(mm)')) + ylab(bquote('LM'['50%']~'(mm)'))+
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
# SubPick.GwthRsts<-subset(Pick.GwthRsts, DiffLD50 <=10 & DiffLD50 >=-10)
# 

# add ylimits for plots
Zone<-c('BS', "CW", "E", "N", "W")
Top<-c(120, 135, 155, 140, 155)
Bottom<-c(95, 110, 110, 90, 130)

ylimits<-data.frame(Zone, Top, Bottom)
Pick.GwthRsts<-left_join(Pick.GwthRsts, ylimits, by = "Zone")
#Pick.GwthRsts<-Pick.GwthRsts[,c(1:58)]

setwd("D:/Fisheries Research/Abalone/SAM/eLML figures")
#YEAR 1
doPlot = function(LFPlot) {
 dum = subset(Pick.GwthRsts, Zone == LFPlot)
 ggobj = ggplot(data = dum, aes(y=eLML.LM50.1, x=as.factor(BlockNo))) + 
  xlab("BlockNo") +
  ylab(expression(paste('1 Year eLML LM'['50%']~'(mm)')))+ 
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
 ggsave(sprintf("50_%s_eLMLplot_Yr1.tiff", LFPlot, width = 4, height = 6, units = "cm"))
 print(ggobj)
}
lapply(unique(Pick.GwthRsts$Zone), doPlot)

#YEAR 2
doPlot = function(LFPlot) {
 dum = subset(Pick.GwthRsts, Zone == LFPlot)
 ggobj = ggplot(data = dum, aes(y=eLML.LM50.2, x=as.factor(BlockNo))) + 
  xlab("BlockNo") +
  ylab(expression(paste('2 Year eLML LM'['50%']~'(mm)')))+ 
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
 ggsave(sprintf("50_%s_eLMLplot_Yr2.tiff", LFPlot, width = 4, height = 6, units = "cm"))
 print(ggobj)
}
lapply(unique(Pick.GwthRsts$Zone), doPlot)

#YEAR 3
doPlot = function(LFPlot) {
 dum = subset(Pick.GwthRsts, Zone == LFPlot)
 ggobj = ggplot(data = dum, aes(y=eLML.LM50.3, x=as.factor(BlockNo))) + 
  xlab("BlockNo") +
  ylab(expression(paste('3 Year eLML LM'['50%']~'(mm)')))+ 
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
 ggsave(sprintf("50_%s_eLMLplot_Yr3.tiff", LFPlot, width = 4, height = 6, units = "cm"))
 print(ggobj)
}
lapply(unique(Pick.GwthRsts$Zone), doPlot)

#################
#              LM90
#################

#YEAR 1
doPlot = function(LFPlot) {
 dum = subset(Pick.GwthRsts, Zone == LFPlot)
 ggobj = ggplot(data = dum, aes(y=eLML.LM90.1, x=as.factor(BlockNo))) + 
  xlab("BlockNo") +
  ylab(expression(paste('1 Year eLML LM'['90%']~'(mm)')))+ 
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
 ggsave(sprintf("90_%s_eLMLplot_Yr1.tiff", LFPlot, width = 4, height = 6, units = "cm"))
 print(ggobj)
}
lapply(unique(Pick.GwthRsts$Zone), doPlot)

#YEAR 2
doPlot = function(LFPlot) {
 dum = subset(Pick.GwthRsts, Zone == LFPlot)
 ggobj = ggplot(data = dum, aes(y=eLML.LM90.2, x=as.factor(BlockNo))) + 
  xlab("BlockNo") +
  ylab(expression(paste('2 Year eLML LM'['90%']~'(mm)')))+ 
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
 ggsave(sprintf("90_%s_eLMLplot_Yr2.tiff", LFPlot, width = 4, height = 6, units = "cm"))
 print(ggobj)
}
lapply(unique(Pick.GwthRsts$Zone), doPlot)

#YEAR 3
doPlot = function(LFPlot) {
 dum = subset(Pick.GwthRsts, Zone == LFPlot)
 ggobj = ggplot(data = dum, aes(y=eLML.LM90.3, x=as.factor(BlockNo))) + 
  xlab("BlockNo") +
  ylab(expression(paste('3 Year eLML LM'['90%']~'(mm)')))+ 
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
 ggsave(sprintf("90_%s_eLMLplot_Yr3.tiff", LFPlot, width = 4, height = 6, units = "cm"))
 print(ggobj)
}
lapply(unique(Pick.GwthRsts$Zone), doPlot)


save(Pick.GwthRsts, file='c:/CloudStor/R_Stuff/SAM/Logistic/DeterGwthRslts.RData')

eLMLBlockSumStats<-ddply(Pick.GwthRsts,.(BlockNo, Zone), summarize,  n = length(SiteCode), 
                         eLML.LM90.3 = mean(eLML.LM90.3, na.rm=T), eLML90.3.IQ = quantile(eLML.LM90.3, 0.75, na.rm=T)- quantile(eLML.LM90.3, 0.25, na.rm=T),
                         eLML.LM50.1 = mean(eLML.LM50.1, na.rm=T), eLML50.1.IQ = quantile(eLML.LM50.1, 0.75, na.rm=T)- quantile(eLML.LM50.1, 0.25, na.rm=T))

Pick.GwthRsts$LMLDiffLD50<-Pick.GwthRsts$LML-Pick.GwthRsts$eLML.LM50.2


write.csv(Pick.GwthRsts, file='Pick.GwthRsts.csv')
