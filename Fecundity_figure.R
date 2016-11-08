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
maxFcd<-left_join(maxFcd, Fcd[,c(1,2,7,11)], by='CountsM')


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
maxFcdSix<-maxFcd[1:6,]
# mdl1 <- nls(CountsM ~ a*SL^b, start = list(a=1, b=0.9), data = maxFcdSix, control = list(maxiter=500, warnOnly=TRUE))
# summary(mdl1)
# coef(mdl1)
#better exp model
mdl2 <- nls(CountsM ~ exp(a+SL*b), start = list(a=0, b=0), data = maxFcdSix, control = list(maxiter=500, warnOnly=TRUE))
summary(mdl2)
coef(mdl2)
  
  # plot(maxFcdSix$SL, maxFcdSix$CountsM)
  # lines(maxFcdSix$SL, predict(mdl1, list(x = maxFcdSix$SL)))
  # lines(maxFcdSix$SL,  coef(mdl1)[1]*maxFcdSix$SL^coef(mdl1)[2], col='red')
  # lines(maxFcdSix$SL,  exp(coef(mdl2)[1]+maxFcdSix$SL*coef(mdl2)[2]), col='blue')

# 
eSL<-seq(105,151,2)
num<-unique(eSL)
if (exists("y1")) 
  rm(y1)
for(n in num){
  y<-exp(coef(mdl2)[1]+n*coef(mdl2)[2])
  pick<-y  
  if (exists("y1"))
    y1 <- rbind(y1, y)
  else
    y1 <- y}
FCD_xy<-data.frame(eSL, y1)
colnames(FCD_xy) <- c("eSL", "FcD")

load('D:/R_Stuff/SAM/Logistic/GeoIIIGTM021116.RData')
#keep(George, Fcd, sure=T)
eLML50.1<-mean(George$eLML50.1y)
eLML50.2<-mean(George$eLML50.2y)
eLML50.3<-mean(George$eLML50.3y)

LD50<-mean(George$LD50)
#LD90<-mean(geoIII$LD90)


F.eLML50.1<-exp(coef(mdl2)[1]+eLML50.1*coef(mdl2)[2])
F.eLML50.2<-exp(coef(mdl2)[1]+eLML50.2*coef(mdl2)[2])
F.eLML50.3<-exp(coef(mdl2)[1]+eLML50.3*coef(mdl2)[2])

F.LML<-exp(coef(mdl2)[1]+138*coef(mdl2)[2])
F.LM50<-exp(coef(mdl2)[1]+LD50*coef(mdl2)[2])

#Figure from http://stackoverflow.com/questions/18305852/power-regression-in-r-similar-to-excel

ggplot(data = maxFcdSix, aes(x=SL,  y=CountsM)) + 
  xlab("Shell length (mm)") + ylab("Number of eggs (millions)")+
  # scale_x_continuous(trans = 'log', breaks=c(100,110,120,130,140,150)) + 
  # scale_y_continuous(trans = 'log', breaks=c(0,1,3,5,7)) + 
  # # stat_smooth(method = 'lm', se = FALSE, colour='black') + 
  # # coord_trans(y = 'exp', x = 'exp')+
    geom_errorbar(data = Fcd, aes(ymin=CountsM-Se.M, ymax=CountsM+Se.M),
                                # Width of the error bars
                position=position_dodge(.9))+
  geom_point(data = Fcd, aes(x=SL,  y=CountsM), colour = 'red') +  
  geom_point(size = 2, colour ='black')+
  #geom_smooth(data = maxFcdSix, aes(x=SL, y= exp(coef(mdl1)[1]+maxFcdSix$SL*coef(mdl1)[2])), colour ='black', size=1, linetype =3)+
  geom_smooth(data = FCD_xy, aes(x=eSL, y=FcD), colour ='black', size=1, linetype =3)+
  # 
  geom_segment(aes(x = 138, y = F.LML+0.7, xend = 138, yend = F.LML),colour = 'lightblue', size = 1.2, arrow = arrow(length = unit(0.2, "cm")))+
  geom_segment(aes(x = LD50, y = F.LM50+0.7, xend = LD50, yend = F.LM50), colour = 'grey', size = 1.2, arrow = arrow(length = unit(0.2, "cm")))+
  geom_segment(aes(x = eLML50.1, y = F.eLML50.1+0.7, xend = eLML50.1, yend = F.eLML50.1), colour = 'red', size = 1.2, arrow = arrow(length = unit(0.2, "cm")))+
  geom_segment(aes(x = eLML50.2, y = F.eLML50.2+0.7, xend = eLML50.2, yend = F.eLML50.2), colour = 'orange', size = 1.2, arrow = arrow(length = unit(0.2, "cm")))+
  geom_segment(aes(x = eLML50.3, y = F.eLML50.3+0.7, xend = eLML50.3, yend = F.eLML50.3), colour = 'green', size = 1.2, arrow = arrow(length = unit(0.2, "cm")))+
  
  #geom_segment(aes(x = eLML90, y = F.eLMLm90+0.7, xend = eLML90, yend = F.eLMLm90), colour = 'green', size = 1.2, arrow = arrow(length = unit(0.2, "cm")))+
  # geom_segment(aes(x = eLML85, y = F.eLMLm85+0.7, xend = eLML85, yend = F.eLMLm85), colour = 'green', size = 1.2, arrow = arrow(length = unit(0.2, "cm")))+
  # 
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


##
##  ============================= Length - Weight ====================
##
setwd('D:/Fisheries Research/Abalone')

slswt<-read.csv('Length_weight_GIII.csv')

plot(slswt$SL, slswt$TWt)
plot(slswt$SL, slswt$VWt)
plot(slswt$SL, slswt$MWt)
plot(slswt$SL, slswt$SWt)

lm.out1 = lm(log(slswt$TWt) ~ slswt$SL) #exp model
plot(lm.out1,1)
summary(lm.out1)

loess.out2 = loess(slswt$TWt ~ slswt$SL, ) #loess model use the coefs as starting values for power model
plot(loess.out2,1)
summary(loess.out2)

#POwer model
mdl3 <- nls(TWt ~ a*SL^b, start = list(a=-361.961, b=5.05), data = slswt, control = list(maxiter=500, warnOnly=TRUE))
summary(mdl3)
coef(mdl3)
#exp model
mdl4 <- nls(TWt ~ exp(a+SL*b), start = list(a=-1.9, b=0.03), data = slswt, control = list(maxiter=500, warnOnly=TRUE))
summary(mdl4)
coef(mdl4)

# plot(slswt$SL, slswt$TWt)
# #lines(slswt$SL, predict(mdl3, list(x = slswt$SL)))
# lines(slswt$SL,  coef(mdl3)[1]*slswt$SL^coef(mdl3)[2], col='red')
# lines(slswt$SL,  exp(coef(mdl4)[1]+slswt$SL*coef(mdl4)[2]), col='blue')

#Power modelled
eSL<-seq(50,150,2)
num<-unique(eSL)
if (exists("y1")) 
  rm(y1)
for(n in num){
  y<-coef(mdl3)[1]*n^coef(mdl3)[2]
  pick<-y  
  if (exists("y1"))
    y1 <- rbind(y1, y)
  else
    y1 <- y}
pwr_xy<-data.frame(eSL, y1)
colnames(pwr_xy) <- c("eSL", "SLSWt")

#Exp modelled
eSL<-seq(50,150,2)
num<-unique(eSL)
if (exists("y1"))
  rm(y1)
for(n in num){
  y<-exp(coef(mdl4)[1]+n*coef(mdl4)[2])
  pick<-y
  if (exists("y1"))
    y1 <- rbind(y1, y)
  else
    y1 <- y}
exp_xy<-data.frame(eSL, y1)
colnames(exp_xy) <- c("eSL", "SLSWt")

ggplot(data = slswt, aes(x=SL,  y=TWt)) + 
  xlab("Shell length (mm)") + ylab("Total Weight (g)")+
  geom_point(colour="grey")+
  #geom_smooth(data = exp_xy, aes(x=eSL, y=SLSWt), colour ='blue', size=1, linetype =1)+
  geom_smooth(data = pwr_xy, aes(x=eSL, y=SLSWt), colour ='black', size=1, linetype =1)+
        theme_bw()+
  theme(legend.position='none')+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(size=14))+
  theme(axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=14))+
  theme(axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=14))


##
##  ============================= log SLfecundity and log SLWt  ====================
##
#Power modelled
eSL<-seq(105,150,2)
num<-unique(eSL)
if (exists("y1")) 
  rm(y1)
for(n in num){
  y<-coef(mdl3)[1]*n^coef(mdl3)[2]
  pick<-y  
  if (exists("y1"))
    y1 <- rbind(y1, y)
  else
    y1 <- y}
pwr_xy<-data.frame(eSL, y1)
colnames(pwr_xy) <- c("eSL", "SLSWt")

pwrFcdTWt<-join(pwr_xy, FCD_xy, by = 'eSL')

lm.out3<-lm(log(FcD)~log(SLSWt), data = pwrFcdTWt)
lm.out3
anova(lm.out3)
plot(lm.out3$fitted, lm.out3$resid)




ggplot(data = pwrFcdTWt, aes(x=log(SLSWt),  y=log(FcD))) + 
  xlab("log(total weight at length)") + ylab("log(fecundity at length)")+
  geom_point()+
  #geom_smooth(data = SLTWt_xy, aes(x=eSL, y=a), colour ='black', size=1.2, linetype =1)+
  theme_bw()+
  theme(legend.position='none')+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(size=14))+
  theme(axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=14))+
  theme(axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=14))

