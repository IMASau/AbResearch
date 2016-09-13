library(car)
library(MASS)
library(boot)
library(dplyr)
library(plyr)
library(gdata)
library(ggplot2)
library(multcompView)

setwd('D:/Fisheries Research/Abalone')
Fcd<-read.csv('Fecundity_GIII_050916.csv')

Fcd$CountsM<-Fcd$count/1000000
Fcd$Se.M<-Fcd$SE/1000000


boxcox(Fcd$count~Fcd$SL)


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

lm.out3 = lm(Fcd$count ~ Fcd$SL + I(Fcd$SL^2))# + I(Fcd$SL^3)) # the poly model
summary(lm.out3)



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