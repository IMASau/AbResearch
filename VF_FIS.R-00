library(xlsx)
library(ggplot2)
library(dplyr)
library(lme4)

dat <- read.xlsx(
 "D:/OwnCloud/Contracts/VICFish/FISData.xlsx",
 sheetName = "Data",
 col.names = TRUE
)

infile <- "D:/OwnCloud/Contracts/VICFish/1. FIS Review/FISData.csv"
dat <- read.csv(infile,header=T) 

yearlist <- sample(unique(dat$QuotaYear), 6)
pick <- which(dat$QuotaYear %in% yearlist)
indat <- dat[pick,]



data.nest.lmer <- lmer(Recruits~QuotaYear+(1|SiteID), indat, REML=TRUE)
data.nest.lmer1 <- lmer(Recruits~QuotaYear+(QuotaYear|SiteID), indat, REML=TRUE)
anova(data.nest.lmer)
anova(data.nest.lmer1)
anova(data.nest.lmer, data.nest.lmer1)
VarCorr(data.nest.lmer)

print(VarCorr(data.nest.lmer1),comp=c("Variance"))

as.data.frame(data.nest.lmer1)

indat1 <- subset(dat, QuotaYear == 2012)

vc <- lmer(Recruits~(1|SiteID), indat1, REML=TRUE)
VarCorr(vc)


mod1<-lmer(Recruits ~ QuotaYear + SiteID, indat, REML=TRUE)
VarCorr(mod1)
anova(mod1)

dat$abs <- dat$Recruits/30

out <- dat %>% group_by(QuotaYear, SiteID) %>%
  summarise(mn=mean(abs), std=sd(abs))


