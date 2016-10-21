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
 ggobj = ggplot(data = dum, aes(y=eLML50, x=as.factor(BlockNo))) + 
  xlab("BlockNo") +
  ylab(expression(paste('eLML LM'['50%']~'(mm)')))+ 
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
 ggsave(sprintf("50_%s_eLMLplot.tiff", LFPlot, width = 4, height = 6, units = "cm"))
 print(ggobj)
}
lapply(unique(GwthResults$Zone), doPlot)