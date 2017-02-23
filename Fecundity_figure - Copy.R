library(car)
library(MASS)
library(boot)
library(dplyr)
library(plyr)
library(gdata)
library(ggplot2)
library(multcompView)
library(devtools)

setwd('D:/Fisheries Research/Abalone')
Fcd<-read.csv('Fecundity_GIII_050916.csv')

Fcd$CountsM<-Fcd$count/1000000
Fcd$Se.M<-Fcd$SE/1000000


boxcox(Fcd$count~Fcd$SL)

#Build size class intervals into dataframe
brks<-seq(90,150,by=5)
Fcd$SLclass <- cut(Fcd$SL, 
                   breaks = seq(90,150,by=5), 
                   labels = brks[-length(brks)] + diff(brks), 
                   right = FALSE)


fit<-lm(count~SL, data=Fcd)
summary(fit)
anova(fit)
par(mfrow = c(2,2))
plot(fit)
par(mfrow = c(1,1))

lm.out1 = lm(log(Fcd$count) ~ Fcd$SL) #exp model
plot(lm.out1,1)
summary(lm.out1)

lm.out2 = lm(log(Fcd$count)  ~ log(Fcd$SL))  # the power model
lm.out2
summary(lm.out2)
plot(lm.out2$fitted, lm.out2$resid)



ggplot(data = Fcd, aes(x=SL,  y=CountsM)) + 
  geom_point()+
  geom_errorbar(aes(ymin=Fcd$CountsM-Fcd$Se.M, ymax=Fcd$CountsM+Fcd$Se.M),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  xlab("Shell length (mm)") + ylab("Number of eggs (millions)")+
  ylim(0,6)+
  #geom_smooth(method=lm, se=T, fill='Black', fullrange=F, size=1.2, color='grey50', linetype=2)+
  theme_bw()+
  theme(legend.position=c(0.9, 0.8))+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(size=14))+
  theme(axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=14))+
  theme(axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=14))


maxFcd<-ddply(Fcd,.(SLclass), summarize,  CountsM = max(CountsM))
maxFcd$SLclass<-as.character(maxFcd$SLclass)
maxFcd$SLclass<-as.numeric(maxFcd$SLclass)
maxFcd<-left_join(maxFcd, Fcd[,c(1,5)], by='CountsM')


fit<-lm(CountsM~SL, data=maxFcd)
summary(fit)
anova(fit)
par(mfrow = c(2,2))
plot(fit)
par(mfrow = c(1,1))

lm.out1 = lm(log(maxFcd$CountsM) ~ maxFcd$SL) #exp model
plot(lm.out1,1)
summary(lm.out1)

lm.out2 = lm(log(maxFcd$CountsM)  ~ log(maxFcd$SL))  # simple  power model
lm.out2
summary(lm.out2)
plot(lm.out2$fitted, lm.out2$resid)

prd <- data.frame(x = seq(90,150, by=0.5))
result <- prd
result$mdl1 <- lm.out2$coefficients[1]+lm.out2$coefficients[2]*result$x
plot(result$x, result$mdl1)
result$mdl2<-exp(result$mdl1)
plot(result$x, result$mdl2)


#print(getInitial(CountsM ~ SSmicmen(SL, max(CountsM), 1), data = maxFcd), digits = 3)
#better power model
mdl1<- nls(CountsM ~ a*SL^b, start = list(a=0.3, b=1), data = maxFcd, control = list(maxiter=200, warnOnly=TRUE))
  summary(mdl1)

#Figure from http://stackoverflow.com/questions/18305852/power-regression-in-r-similar-to-excel
  
ggplot(data = maxFcd, aes(x=SL,  y=CountsM)) + 
  geom_point()+
  xlab("Shell length (mm)") + ylab("Number of eggs (millions)")+
  scale_x_continuous(trans = 'log', breaks=c(100,110,120,130,140,150)) + 
  scale_y_continuous(trans = 'log', breaks=c(0,1,3,5,7)) + 
  stat_smooth(method = 'lm', se = FALSE, colour='black') + 
  coord_trans(y = 'exp', x = 'exp')+
   geom_errorbar(data = Fcd, aes(ymin=CountsM-Se.M, ymax=CountsM+Se.M),
                                # Width of the error bars
                position=position_dodge(.9))+
  geom_point(data = Fcd, aes(x=SL,  y=CountsM, colour = 'blue')) + 
  geom_point(size = 2)+
  geom_segment(aes(x = 138, y = 3.8, xend = 138, yend = 3.1, colour = 'red'), size = 1.2, arrow = arrow(length = unit(0.2, "cm")))+
  geom_segment(aes(x = gIIILD50, y = 0, xend = gIIILD50, yend = 0.2, colour = 'blue'), size = 1.2, arrow = arrow(length = unit(0.2, "cm")))+
  geom_segment(aes(x = gIIIeLML, y = 5, xend = gIIIeLML, yend = 4.3, colour = 'green'), size = 1.2, arrow = arrow(length = unit(0.2, "cm")))+
  
  # geom_vline(xintercept = 138,  colour = "Grey", linetype = 3, size =1.2)+#LML
  # geom_vline(xintercept = gIIILD50,  colour = "blue", linetype = 3, size =1.2)+#SAM
  # geom_vline(xintercept = gIIIeLML,  colour = "red", linetype = 3, size =1.2)+#eLML
    theme_bw()+
  theme(legend.position='none')+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(size=14))+
  theme(axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=14))+
  theme(axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=14))

# 
# load("D:/R_Stuff/SAM/Logistic/GrthResults130916.RData")
# geoIIIsites<-c('813','418', '37', '420', '247', '39')
# geoIII<-subset(GwthResults, SIT_Id %in% geoIIIsites)
# gIIIeLML<-mean(geoIII$eLML)
# gIIILD50<-mean(geoIII$LD50)


