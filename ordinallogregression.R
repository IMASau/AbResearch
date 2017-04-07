require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)
library(lattice)

load("D:/GitCode/AbResearch/OrdLogRegData.RData")

###########################################################COMMERCIAL FISH SEASON
# 
# d1<-read.csv("20_02_17_commseason.csv")
cor(d1[,c(8:13,21)])

w1.fit1 <-glm(colour ~ age + macro + gonad_index + month2 + test_diameter + gut_weight + spine_length + water_loss, data=d1)
op <-par(mfrow=c(2,2))
plot(w1.fit1)
par(op)

# #factors (categorical)
# d1$colour0 <- as.factor(as.numeric(d1$colour)-1)
# d1$colour <-factor(d1$colour, levels=c("1", "2","3","4","5"))
# d1$habitat1<-factor(d1$habitat1, levels=c("1", "2", "3"))
# d1$month2<-factor(d1$month2, levels=c("1", "2","3","4","5","6","7","8","9"))
# d1$month3<-factor(d1$month3, levels=c("1", "2","3","4","5","6","7","8","9"))
# #1$month <- ordered(month, levels = c("a_December", "b_January" "c_February" d_March e_April f_May g_June h_July i_August))

head(d1)
lapply(d1[, c("colour","month2","habitat1")], table)
ftable(xtabs(~habitat+colour+month,data=d1))
summary(d1$age)
sd(d1$age)
ggplot(d1,aes(x=colour, y=age))+geom_boxplot(size = .1)+geom_jitter(alpha = .5)+facet_grid(habitat1~month2, margins=TRUE)+theme(axis.text.x = element_text(angle=45,hjust=1,vjust=1))

m<-polr(colour ~  month + age + macro + habitat1 + gonad_index + test_diameter + gut_weight + spine_length + water_loss, data=d1, Hess=TRUE)
summary(m)

m<-polr(colour ~  month + habitat1 + gonad_index + test_diameter + gut_weight, data=d1, Hess=TRUE)
summary(m)

ctable<-coef(summary(m))

p<-pnorm(abs(ctable[,"t value"]), lower.tail=FALSE)*2
(ctable<-cbind(ctable, "pvalue"=p)) #p values

(ci<-confint(m))

confint.default(m)
exp(coef(m))
exp(cbind(OR=coef(m),ci)) #odds ratio

sf <-
  function(y) {
    c(
      'Y>=1' = qlogis(mean(y >= 1)),
      'Y>=2' = qlogis(mean(y >= 2)),
      'Y>=3' = qlogis(mean(y >= 3)),
      'Y>=4' = qlogis(mean(y >= 4)),
      'Y>=5' = qlogis(mean(y >= 5))
    )
  }
sf


s <-
  with(
    d1,
    summary(
      as.numeric(colour) ~ month + habitat1 + gonad_index + test_diameter + gut_weight,
      fun = sf
    )
  )
s


glm(I(as.numeric(colour)>=1)~month, family="binomial",data=d1)
glm(I(as.numeric(colour)>=2)~month, family="binomial",data=d1)
glm(I(as.numeric(colour)>=3)~month, family="binomial",data=d1)
glm(I(as.numeric(colour)>=4)~month, family="binomial",data=d1)


s[, 4] <- s[, 4] - s[, 3]
s[, 3] <- s[, 3] - s[, 3]
s # print


plot(s, which=1:5, pch=1:3, xlab='logit', main=' ', xlim=range(s[,4:5]))


d1$quality_index1 <-factor(d1$quality_index, levels=c("1", "2","3","4","5","6","7","8","9","10","11","12","13","14","15"))


#full model   fit_A <- glm(gran ~ habitat age + macro + gonad_index + month + test_diameter + gut_weight + spine_length + water_loss, data=d1)





m<-polr(quality_sum ~ age + macro + gonad_index + test_diameter + gut_weight + spine_length + water_loss, data=d1, Hess=TRUE)



#w1.fit1 <-glm(quality_sum ~ age + macro + gonad_index + month + test_diameter + gut_weight + spine_length + water_loss, data=d1)
















library(lattice)
library(relaimpo)
cor(d1[,2:17])

w1.fit1 <-glm(quality_sum ~ age + macro + gonad_index + month + test_diameter + gut_weight + spine_length + water_loss, data=d1)
op <-par(mfrow=c(2,2))
plot(w1.fit1)
par(op)



d1$month2 <-factor(d1$month, levels=c("1", "2","3","4","5","6","7","8","9","10","11","12"))

####SUBSETTING
d1<-read.csv("16_6_16_data ALL.csv")

jan<-subset(d1, month == 1)
feb<-subset(d1, month == 2)
mar<-subset(d1, month == 3)
apr<-subset(d1, month == 4)
may<-subset(d1, month == 5)
jun<-subset(d1, month == 6)
jul<-subset(d1, month == 7)
aug<-subset(d1, month == 8)
sep<-subset(d1, month == 9)
oct<-subset(d1, month == 10)
nov<-subset(d1, month == 11)
dec<-subset(d1, month == 12)



cor(dec[,6:36])

################################## model for peak season

fit_A <- glm(gran ~ age + macro + gonad_index + month + test_diameter + gut_weight + spine_length + water_loss, data=d1)
summary(fit_A)
metrics <- calc.relimp(fit_A, type = c("lmg", "first", "last", "betasq", "pratt"))
metrics



#################################### model for pre season

fit_A <- glm(gran ~ age + macro + gonad_index + month + total_weight + spine_length + water_loss, data=d1)
summary(fit_A)
metrics <- calc.relimp(fit_A, type = c("lmg", "first", "last", "betasq", "pratt"))
metrics


















