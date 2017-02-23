# Version 05/06/2014  

rm(list=ls(all=TRUE))
## SET THE WORKING AND RESULTS DIRECTORIES
wkdir <- "D:/Fisheries Research/Abalone"
setwd(wkdir)

## Load raw csv file
infile <- "DataForExport.csv"

library(car)
library(MASS)
library(boot)
source("D:/GitCode/AbResearch/SAM_utils.R")

samdata <- read.csv(infile,header=T)
dim(samdata)
head(samdata)

samdata <- subset(samdata, Sex %in% c("I", "M", "F"))

#Option - remove Wineglass Bay Site for Season * region analysis
pick <- which(samdata$Site_Name == "Wineglass Bay")
samdata <- samdata[-pick,]
samdata <- droplevels(samdata)

# Simplify Maturity classes
samdata$Mat <- NA
samdata$Mat <- ifelse(samdata$Sex=="I", c("I"), c("M"))

# Code Season and Site
samdata$Season <- samdata$SamplePeriod
samdata$Region <- NA

pick <- which(samdata$Site_Name == "Gardens")
samdata$Region[pick] <- "North"
pick <- which(samdata$Site_Name == "George III Rock")
samdata$Region[pick] <- "South"

## Create new Season* Region interaction variable
samdata$RegionSeason <- interaction(samdata$Region,samdata$Season)

# Characterize Sites
Sites <- unique(samdata$Site_Name); Sites
NS <- length(Sites)

RegionSeasonVec <- unique(samdata$RegionSeason); RegionSeasonVec
NSR <-  length(RegionSeasonVec)


## Outlier Removal: Unusual large immature animals
pick <- which((samdata$Length >139) & (samdata$Mat=="I"))
outlier <- samdata[pick,]
samdata <- samdata[-pick,]

## Outlier Removal: Unusual small mature animals
pick <- which((samdata$Length <60) & (samdata$Mat=="M"))
outlier <- rbind(outlier,samdata[pick,])
samdata <- samdata[-pick,]

## Allocate into Size Classes

samdata$SizeC <- NA
pick <- which(samdata$Length <= 90)
samdata$SizeC[pick] <- "Small"
pick <- which((samdata$Length > 90) & (samdata$Length <=130))
samdata$SizeC[pick] <- "Medium"
pick <- which(samdata$Length > 130)
samdata$SizeC[pick] <- "Large"

## Calculate Base cases for each site.
columns <- c("LM50", "LM75", "LM95","IQ","a","b","smallN","mediumN","largeN","totalN")
BaseResults <- matrix(0,nrow=NSR,ncol=length(columns),dimnames=list(RegionSeasonVec,columns))
for (pSite in 1:NSR) {
 Site <- RegionSeasonVec[pSite]
 pick <- which(samdata$RegionSeason == Site)
 if (length(pick) > 0) {
  subdata <- samdata[pick,]
   } else { "Oops something has gone wrong"
    }

  ## Create required fields for logistic regression
 SizeMat <- table(subdata$Length, subdata$Mat)
 SizeMat <- as.data.frame(rbind(SizeMat))
 SizeMat$Length <- as.numeric(rownames(SizeMat))
 head(SizeMat)
 SizeMat$Total <- NA
 SizeMat$Total <- SizeMat$I + SizeMat$M
 SizeMat$MatRatio <- NA
 SizeMat$MatRatio <- SizeMat$M/SizeMat$Total
  out <- doLogistic(SizeMat)
 BaseResults[pSite,1] <- out$LM50
 BaseResults[pSite,2] <- out$LM75
 BaseResults[pSite,3] <- out$LM95
 BaseResults[pSite,4] <- out$IQ
 model <- out$Model
 BaseResults[pSite,5:6] <- model$coef
 scN <- numeric(3)
 pick <- which(SizeMat$Length <= 90)
 scN[1] <- sum(SizeMat$Total[pick],na.rm=T)
 pick <- which((SizeMat$Length > 90) & (SizeMat$Length <= 130))
 scN[2] <- sum(SizeMat$Total[pick],na.rm=T)
 pick <- which(SizeMat$Length > 130)
 scN[3] <- sum(SizeMat$Total[pick],na.rm=T)
 BaseResults[pSite,7:10] <- c(scN,sum(scN))
 plotgraph(SizeMat,out$LM50,Site,scN,savefile=F)
}
BaseResults

#Combine all sites and Seasons
# USe function allSites  - needs full data set as input 
processed <- allSites(samdata)
#Run full model
outCompare<- doLogisticOrth(processed)
outCompare$ModelSummary
outCompare$ModelSummary$coefficients
outCompare$SigSeason
outCompare$Sigregion
outCompare$SigInteract



source("D:\\Students\\Carly Giosio/carly_utils.R")
plotgraph(SizeMat,out$LM50,Site,scN,savefile=F)




## Do the Monte Carlo Testing of reducing the sample size

## Calculate Base cases for each site.
starttime <- unclass(Sys.time())

sizeCl <- c("Small","Medium","Large")
as.character(RegionSeasonVec)
# 1: George III Rock April
# 2: George III Rock January
# 3: St Helens April
# 4: St Helens January
# 5: Wineglass Bay January
pSite <- 1            #  CHANGE THIS TO SELECT WHICH Location TO USE
# in the Monte Carlo pSite <- 1  will analyse George III
#  pSite <- 3    would analyse St Helens
Site1 <- Sites[pSite]
pick <- which(samdata$Site_Name == Site1)
if (length(pick) > 0) {
 subdata <- samdata[pick,]
} else { "Oops something has gone wrong"
}

## Data choice 
ThinFactorSmall <- 1
ThinFactorMedium <- 2 
ThinFactorLarge <-1 


reps <- 200
columns <- c("LM50","IQ","a","b","smallN","mediumN","largeN","totalN")
Results <- matrix(0,nrow=reps,ncol=length(columns),dimnames=list(seq(1,reps,1),columns))
# pick <- which(subdata$SizeC == SizeClass)
# tmp <- subdata[-pick,]            #  data from the other two size classes
# classdat <- subdata[pick,]        # data from teh selected size class
# categorysize <- length(pick)
# pickfrom <- seq(1,categorysize,1)   # this will be sampled to decide with rows to use
# samplesize <- trunc(categorysize/ThinFactor)  # This takes 50% of the original sample size


#  if you change this you need to change
# line 200 below
for (iter in 1:reps) {
 ## do the random sampling
 #input required indat,SizeClass,ThinFactor 
 thinned <- dataThinningOneSite(subdata,"Medium", ThinFactorMedium)
 thinned <- dataThinningOneSite(thinned,"Small", ThinFactorSmall)
 thinned <- dataThinningOneSite(thinned,"Large", ThinFactorLarge)
 ## Create required fields for logistic regression
 SizeMat <- table(thinned$Length, thinned$Mat)
 SizeMat <- as.data.frame(rbind(SizeMat))
 SizeMat$Length <- as.numeric(rownames(SizeMat))
 head(SizeMat)
 SizeMat$Total <- NA
 SizeMat$Total <- SizeMat$I + SizeMat$M
 SizeMat$MatRatio <- NA
 SizeMat$MatRatio <- SizeMat$M/SizeMat$Total
 out <- doLogistic(SizeMat)
 Results[iter,1] <- out$LM50
 Results[iter,2] <- out$IQ
 model <- out$Model
 Results[iter,3:4] <- model$coef
 scN <- numeric(3)
 pick <- which(SizeMat$Length <= 90)
 scN[1] <- sum(SizeMat$Total[pick],na.rm=T)
 pick <- which((SizeMat$Length > 90) & (SizeMat$Length <= 130))
 scN[2] <- sum(SizeMat$Total[pick],na.rm=T)
 pick <- which(SizeMat$Length > 130)
 scN[3] <- sum(SizeMat$Total[pick],na.rm=T)
 Results[iter,5:8] <- c(scN,sum(scN))
 #  plotgraph(SizeMat,out$LM50,Site1,scN,savefile=T)
}

pSite2 <- pSite + 1    #  THIS WILL SELECT the complementary sample to the one above
Site2 <- Sites[pSite2]
pick <- which(samdata$Site_Name == Site2)
if (length(pick) > 0) {
 subdata <- samdata[pick,]
} else { "Oops something has gone wrong"
}


reps <- 200
columns <- c("LM50","IQ","a","b","smallN","mediumN","largeN","totalN")
Results2 <- matrix(0,nrow=reps,ncol=length(columns),dimnames=list(seq(1,reps,1),columns))
# pick <- which(subdata$SizeC == SizeClass)
# tmp <- subdata[-pick,]            #  data from the other two size classes
# classdat <- subdata[pick,]        # data from teh selected size class
# categorysize <- length(pick)
# pickfrom <- seq(1,categorysize,1)   # this will be sampled to decide with rows to use
# samplesize <- trunc(categorysize/ThinFactor)  # CHANGE THE 4 TO GIVE WHATEVER PROPORTION
# OF THE ORIGINAL YOU WANT
for (iter in 1:reps) {
 ## do the random sampling
#  adddata <- sort(sample(pickfrom,samplesize,replace=FALSE))
#  subdata2 <- rbind(tmp,classdat[adddata,])    # combine the bits
 thinned <- dataThinningOneSite(subdata,"Medium", ThinFactorMedium)
 thinned <- dataThinningOneSite(thinned,"Small", ThinFactorSmall)
 thinned <- dataThinningOneSite(thinned,"Large", ThinFactorLarge)
 
 ## Create required fields for logistic regression
 SizeMat <- table(thinned$Length, thinned$Mat)
 SizeMat <- as.data.frame(rbind(SizeMat))
 SizeMat$Length <- as.numeric(rownames(SizeMat))
 head(SizeMat)
 SizeMat$Total <- NA
 SizeMat$Total <- SizeMat$I + SizeMat$M
 SizeMat$MatRatio <- NA
 SizeMat$MatRatio <- SizeMat$M/SizeMat$Total
 out <- doLogistic(SizeMat)
 Results2[iter,1] <- out$LM50
 Results2[iter,2] <- out$IQ
 model <- out$Model
 Results2[iter,3:4] <- model$coef
 scN <- numeric(3)
 pick <- which(SizeMat$Length <= 90)
 scN[1] <- sum(SizeMat$Total[pick],na.rm=T)
 pick <- which((SizeMat$Length > 90) & (SizeMat$Length <= 130))
 scN[2] <- sum(SizeMat$Total[pick],na.rm=T)
 pick <- which(SizeMat$Length > 130)
 scN[3] <- sum(SizeMat$Total[pick],na.rm=T)
 Results2[iter,5:8] <- c(scN,sum(scN))
 #  plotgraph(SizeMat,out$LM50,Site2,scN,savefile=T)
}

Results
Results2

endtime <- unclass(Sys.time())
secs <- endtime-starttime
cat("Procedure took ",secs," seconds or ",secs/60," minutes \n")

# Plots up the results from the first Monte Carlo Check the Site number
par(mfrow = c(2,1))
par(mai=c(0.4,0.4,0.1,0.05),oma=c(0,0,0.0,0.0))
par(cex=0.75, mgp=c(1.35,0.35,0), font.axis=2,font=2)
hist(Results[,1],main="")
abline(v=BaseResults[1,1],col=2,lwd=2)
x <- seq(60,160,1)
y <- exp(Results[1,3]+Results[1,4]*x)/(1+exp(Results[1,3]+Results[1,4]*x))
plot(x,y,type="l",col="grey")
for (i in 2:reps) {
 y <- exp(Results[i,3]+Results[i,4]*x)/(1+exp(Results[i,3]+Results[i,4]*x))
 lines(x,y,col="grey")
}
y <- exp(BaseResults[pSite,3]+BaseResults[pSite,4]*x)/(1+exp(BaseResults[pSite,3]+BaseResults[pSite,4]*x))
lines(x,y,col="red",lwd=3)


# Plots both the trajectories with Monte Carlo replicates
par(mfrow = c(1,1))
par(mai=c(0.4,0.4,0.1,0.05),oma=c(0,0,0.0,0.0))
par(cex=0.75, mgp=c(1.35,0.35,0), font.axis=2,font=2)
x <- seq(60,160,1)
y <- exp(Results[1,3]+Results[1,4]*x)/(1+exp(Results[1,3]+Results[1,4]*x))
plot(x,y,type="l",col="grey",xlab="",ylab="")
for (i in 2:reps) {
 y <- exp(Results[i,3]+Results[i,4]*x)/(1+exp(Results[i,3]+Results[i,4]*x))
 lines(x,y,col="grey")
}
for (i in 1:reps) {
 y2 <- exp(Results2[i,3]+Results2[i,4]*x)/(1+exp(Results2[i,3]+Results2[i,4]*x))
 lines(x,y2,col="lightblue")
}
y <- exp(BaseResults[pSite2,3]+BaseResults[pSite2,4]*x)/(1+exp(BaseResults[pSite2,3]+BaseResults[pSite2,4]*x))
lines(x,y,col="red",lwd=3)

y3 <- exp(BaseResults[pSite,3]+BaseResults[pSite,4]*x)/(1+exp(BaseResults[pSite,3]+BaseResults[pSite,4]*x))
lines(x,y3,col=4,lwd=3)
abline(v=c(BaseResults[pSite:pSite2,1]),col=3,lwd=1)
title(ylab=list("Proportion Mature",cex=1.0,font=2),
      xlab=list("Shell Length mm",cex=1.0,font=2))
legend(55,1.0,c(as.character(Site2),as.character(Site1)),col=c(2,4),lwd=3,bty="n",cex=1.0)

#abline(h=c(0.25,0.75),col="grey")


p <- c(2.5, 50, 97.5)/100
Results.df <- as.data.frame(Results)
droplevels(Site1)
quantile(Results.df$LM50, p)
mean(Results.df$LM50)
unique(Results[,5:8])
#quantile(Results.df$IQ, p)

Results2.df <- as.data.frame(Results2)
droplevels(Site2)
quantile(Results2.df$LM50,p)
mean(Results2.df$LM50)
unique(Results2[,5:8])
#quantile(Results2.df$IQ, p)
##---------------------------------------------------------------------------------------##

source("D:\\Students\\Carly Giosio/carly_utils.R")


#Thin data in desired size class
reps <- 200
columns <- c("SigSeason","Sigregion","SigInteract","TotalN")
GLMResults <- matrix(0,nrow=reps,ncol=length(columns),dimnames=list(seq(1,reps,1),columns))
 
for (iter in 1:reps) {
#input required indat,SizeClass,ThinFactor 
 thinned <- dataThinning(samdata,"Medium", 4)
 #thinned <- dataThinning(thinned,"Small", 5)
 #thinned <- dataThinning(thinned,"Large", 1.25)
 
 processedThin <- allSites(thinned)
 outCompare<- doLogisticOrth(processedThin)
 GLMResults[iter,1] <- outCompare$SigSeason
 GLMResults[iter,2] <- outCompare$Sigregion
 GLMResults[iter,3] <- outCompare$SigInteract
 GLMResults[iter,4] <- outCompare$TotalN
}

GLMResults.df <- as.data.frame(GLMResults)

length(which(GLMResults.df$SigSeason <= 0.05))
length(which(GLMResults.df$Sigregion <= 0.05))
length(which(GLMResults.df$SigInteract <= 0.05))
trunc(mean(GLMResults.df$TotalN))


dataThinningOnSite <- function(indat2,SizeClass,ThinFactor, pSite savefile=F) {
 







