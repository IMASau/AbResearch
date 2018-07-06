# Version 05/06/2014  

rm(list=ls(all=TRUE))
## SET THE WORKING AND RESULTS DIRECTORIES
wkdir <- "D:/R_Stuff/SAM"
setwd(wkdir)

## Load raw csv file
infile <- "Basic Sam Query.csv"

library(car)
library(MASS)
library(boot)
library(dplyr)
library(plyr)
source("D:/GitCode/AbResearch/SAM_utils.R")

alldata <- read.csv(infile,header=T)
dim(alldata)
head(alldata)

n.per.sample<-ddply(alldata,.(Seq.Num), summarize,  n = length(Length))

subdata<-droplevels(subset(n.per.sample, n>=49))

samdata<-join(alldata, subdata, by = "Seq.Num", type = "inner")

samdata<-samdata[, c(1,5:8,13,17)]
#recode date

samdata$NewDate <- as.character(samdata$Date)
samdata$NewDate <- substr(samdata$NewDate, 1, 10)
samdata$FishYear<-substr(samdata$NewDate, 4, 10)
#samdata$NewDate<-as.Date(samdata$NewDate, "%m/%d/%Y")
samdata$FishYear<-sapply(strsplit(samdata$FishYear, "/"), "[", 2)
samdata$FishYear<-as.numeric(samdata$FishYear)

samdata$FishMonth<-sapply(strsplit(samdata$NewDate, "/"), "[", 2)
samdata$FishMonth<-as.numeric(samdata$FishMonth)
samdata$FishQtr<-samdata$FishMonth
samdata$FishQtr[samdata$FishQtr %in% c("1", "2", "3")] <- "Q1"
samdata$FishQtr[samdata$FishQtr %in% c("4", "5", "6")] <- "Q2"
samdata$FishQtr[samdata$FishQtr %in% c("7", "8", "9")] <- "Q3"
samdata$FishQtr[samdata$FishQtr %in% c("10", "11", "12")] <- "Q4"
samdata$FishQtr<-as.factor(samdata$FishQtr)
summary(samdata)

samdata <- subset(samdata, Sex %in% c("I", "M", "F"))

samdata$SubBlockNo <- paste(samdata$Stat.Block,samdata$Sub.Block, sep="")
samdata$new_zone[samdata$Stat.Block %in%  c(seq(14,30,1)) | samdata$SubBlockNo %in% c("13C", "13D", "13E", "31A")] <- "E"
samdata$new_zone[samdata$Stat.Block %in%  c(seq(7,12,1)) | samdata$SubBlockNo %in% c("13A", "13B", "06D", "6D")] <- "W"
samdata$new_zone[samdata$SubBlockNo %in% c("6A", "6C")] <- "CW"
samdata$new_zone[samdata$SubBlockNo %in% c("5A", "5B", "5C")] <- "N"
samdata$new_zone[samdata$Stat.Block %in%  c(1, 2, 3, 4,47, 48, 49,39, 40) | samdata$SubBlockNo %in% c("31 B")] <- "N" 
samdata$new_zone[samdata$Stat.Block %in% c(seq(32, 38,1),seq(41,46,1), seq(50,57,1))] <- "BS"

summary(samdata)



n.SubBlockNo<-ddply(samdata,.(SubBlockNo, FishYear), summarize,  n = length(Length))

samdata$Mat <- NA
samdata$Mat <- ifelse(samdata$Sex=="I", c("I"), c("M"))
# Characterize Sites
samdata$Region<-samdata$Region_Name
samdata$Site_Name<-samdata$Region

## Allocate into Size Classes

samdata$SizeC <- NA
pick <- which(samdata$Length <= 90)
samdata$SizeC[pick] <- "Small"
pick <- which((samdata$Length > 90) & (samdata$Length <=130))
samdata$SizeC[pick] <- "Medium"
pick <- which(samdata$Length > 130)
samdata$SizeC[pick] <- "Large"


## Calculate Base cases for each site.
starttime <- unclass(Sys.time())
sizeCl <- c("Small","Medium","Large")
as.character(Sites)

## Create new FishYear*SubBlock interaction variable
samdata$SubBlockNoYear <- interaction(samdata$SubBlockNo,samdata$FishYear)

# Characterize Sites
Sites <- unique(samdata$Site_Name); Sites
NS <- length(Sites)

SubBlockNoYearVec <- unique(samdata$SubBlockNoYear); SubBlockNoYearVec
NSR <-  length(SubBlockNoYearVec)
as.character(SubBlockNoYearVec)

Regions<-unique(samdata$Region)


## Calculate Base cases for each site.
columns <- c("LM05", "LM50", "LCI50", "UCI50", "LM75", "LCI75", "UCI75", "LM95","LCI95", "UCI95", "IQ","a","b","smallN","mediumN","largeN","totalN", "PctL50", "LMrange")
BaseResults <- matrix(0,nrow=NSR,ncol=length(columns),dimnames=list(SubBlockNoYearVec,columns))

for (pSite in 1:NSR) {
  Site <- SubBlockNoYearVec[pSite]
  pick <- which(samdata$SubBlockNoYearVec == Site)
  if (length(pick) > 0) {
    subdata <- samdata[pick,]
  } else { "Oops something has gone wrong"
  }
  ##
  ## Step 3: Run from here to the very bottom
  reps <- 1000
  columns <- c("LM05","IQ","a","b","smallN","mediumN","largeN","totalN")
  bootResults <- matrix(0,nrow=reps,ncol=length(columns),dimnames=list(seq(1,reps,1),columns))
  
  sampledDat <- split(subdata,subdata$SizeC)
  catSizeLarge <- nrow(sampledDat[[1]])
  catSizeMedium <- nrow(sampledDat[[2]])
  catSizeSmall <- nrow(sampledDat[[3]])
  pickfrom1 <- seq(1,catSizeLarge,1)
  pickfrom2 <- seq(1,catSizeMedium,1)
  pickfrom3 <- seq(1,catSizeSmall,1)
  
  for (iter in 1:reps) {
    ## do the random sampling
    adddata1 <- sort(sample(pickfrom1,catSizeLarge,replace=TRUE))
    adddata2 <- sort(sample(pickfrom2,catSizeMedium,replace=TRUE))
    adddata3 <- sort(sample(pickfrom3,catSizeSmall,replace=TRUE))
    
    subdata2 <- rbind(sampledDat[[1]][adddata1,],sampledDat[[2]][adddata2,])    # combine the bits
    subdata2 <- rbind(subdata2,sampledDat[[3]][adddata3,])    # combine the bits
    #   pickfrom <- seq(1,catSize,1)
    #    picked <- sort(sample(pickfrom,catSize,replace=TRUE))
    #   subdata2 <-subdata[picked,]
    ## Create required fields for logistic regression
    SizeMat <- table(subdata2$Length, subdata2$Mat)
    SizeMat <- as.data.frame(rbind(SizeMat))
    SizeMat$Length <- as.numeric(rownames(SizeMat))
    head(SizeMat)
    SizeMat$Total <- NA
    SizeMat$Total <- SizeMat$I + SizeMat$M
    SizeMat$MatRatio <- NA
    SizeMat$MatRatio <- SizeMat$M/SizeMat$Total
    out <- doLogistic(SizeMat)
    bootResults[iter,1] <- out$LM05
    bootResults[iter,2] <- out$IQ
    model <- out$Model
    bootResults[iter,3:4] <- model$coef
    scN <- numeric(3)
    pick <- which(SizeMat$Length <= 90)
    scN[1] <- sum(SizeMat$Total[pick],na.rm=T)
    pick <- which((SizeMat$Length > 90) & (SizeMat$Length <= 130))
    scN[2] <- sum(SizeMat$Total[pick],na.rm=T)
    pick <- which(SizeMat$Length > 130)
    scN[3] <- sum(SizeMat$Total[pick],na.rm=T)
    bootResults[iter,5:8] <- c(scN,sum(scN))
    #  plotgraph(SizeMat,out$LM50,Site1,scN,savefile=T)
  }
  droplevels(Sites[pSite])
  p <- c(2.5, 50, 97.5)/100
  bootResults <- as.data.frame(bootResults)
  BaseResults[pSite,1]<-quantile(bootResults$LM05,0.5)    
  
  ###################################################
  ##L50
  ## Step 3: Run from here to the very bottom
  reps <- 1000
  columns <- c("LM50","IQ","a","b","smallN","mediumN","largeN","totalN")
  bootResults <- matrix(0,nrow=reps,ncol=length(columns),dimnames=list(seq(1,reps,1),columns))
  
  sampledDat <- split(subdata,subdata$SizeC)
  catSizeLarge <- nrow(sampledDat[[1]])
  catSizeMedium <- nrow(sampledDat[[2]])
  catSizeSmall <- nrow(sampledDat[[3]])
  pickfrom1 <- seq(1,catSizeLarge,1)
  pickfrom2 <- seq(1,catSizeMedium,1)
  pickfrom3 <- seq(1,catSizeSmall,1)
  
  for (iter in 1:reps) {
    ## do the random sampling
    adddata1 <- sort(sample(pickfrom1,catSizeLarge,replace=TRUE))
    adddata2 <- sort(sample(pickfrom2,catSizeMedium,replace=TRUE))
    adddata3 <- sort(sample(pickfrom3,catSizeSmall,replace=TRUE))
    
    subdata2 <- rbind(sampledDat[[1]][adddata1,],sampledDat[[2]][adddata2,])    # combine the bits
    subdata2 <- rbind(subdata2,sampledDat[[3]][adddata3,])    # combine the bits
    #   pickfrom <- seq(1,catSize,1)
    #    picked <- sort(sample(pickfrom,catSize,replace=TRUE))
    #   subdata2 <-subdata[picked,]
    ## Create required fields for logistic regression
    SizeMat <- table(subdata2$Length, subdata2$Mat)
    SizeMat <- as.data.frame(rbind(SizeMat))
    SizeMat$Length <- as.numeric(rownames(SizeMat))
    head(SizeMat)
    SizeMat$Total <- NA
    SizeMat$Total <- SizeMat$I + SizeMat$M
    SizeMat$MatRatio <- NA
    SizeMat$MatRatio <- SizeMat$M/SizeMat$Total
    out <- doLogistic(SizeMat)
    bootResults[iter,1] <- out$LM50
    bootResults[iter,2] <- out$IQ
    model <- out$Model
    bootResults[iter,3:4] <- model$coef
    scN <- numeric(3)
    pick <- which(SizeMat$Length <= 90)
    scN[1] <- sum(SizeMat$Total[pick],na.rm=T)
    pick <- which((SizeMat$Length > 90) & (SizeMat$Length <= 130))
    scN[2] <- sum(SizeMat$Total[pick],na.rm=T)
    pick <- which(SizeMat$Length > 130)
    scN[3] <- sum(SizeMat$Total[pick],na.rm=T)
    bootResults[iter,5:8] <- c(scN,sum(scN))
    #  plotgraph(SizeMat,out$LM50,Site1,scN,savefile=T)
  }
  
  
  droplevels(Sites[pSite])
  p <- c(2.5, 50, 97.5)/100
  bootResults <- as.data.frame(bootResults)
  BaseResults[pSite,2]<-quantile(bootResults$LM50,0.5)
  BaseResults[pSite,3]<-quantile(bootResults$LM50,0.05)
  BaseResults[pSite,4]<-quantile(bootResults$LM50,0.95)
  BaseResults[pSite,11] <- mean(out$IQ)
  BaseResults[pSite,12] <- mean(bootResults[pSite,3])
  BaseResults[pSite,13] <- mean(bootResults[pSite,4])
  BaseResults[pSite,14] <- mean(bootResults[pSite,5])
  BaseResults[pSite,15] <- mean(bootResults[pSite,6])
  BaseResults[pSite,16] <- mean(bootResults[pSite,7])
  BaseResults[pSite,17] <- mean(bootResults[pSite,8])
  #Calucltae the % of shells in sample below the l50%
  pick <- which(subdata$Length <= BaseResults[pSite,2])
  Lpick<-length(pick)
  BaseResults[pSite,18]<-(Lpick/length(subdata$Length))*100
  # # Obtain the 95% CI of Lm50 - Lower CI=2.5% & upper CI=97.5% 
  # LM50boot<-as.data.frame(quantile(bootResults$LM50, p))
  
  ##
  
  #####LM75
  
  
  ## Step 3: Run from here to the very bottom
  reps <- 1000
  columns <- c("LM75","IQ","a","b","smallN","mediumN","largeN","totalN")
  bootResults <- matrix(0,nrow=reps,ncol=length(columns),dimnames=list(seq(1,reps,1),columns))
  
  sampledDat <- split(subdata,subdata$SizeC)
  catSizeLarge <- nrow(sampledDat[[1]])
  catSizeMedium <- nrow(sampledDat[[2]])
  catSizeSmall <- nrow(sampledDat[[3]])
  pickfrom1 <- seq(1,catSizeLarge,1)
  pickfrom2 <- seq(1,catSizeMedium,1)
  pickfrom3 <- seq(1,catSizeSmall,1)
  
  for (iter in 1:reps) {
    ## do the random sampling
    
    
    adddata1 <- sort(sample(pickfrom1,catSizeLarge,replace=TRUE))
    adddata2 <- sort(sample(pickfrom2,catSizeMedium,replace=TRUE))
    adddata3 <- sort(sample(pickfrom3,catSizeSmall,replace=TRUE))
    
    subdata2 <- rbind(sampledDat[[1]][adddata1,],sampledDat[[2]][adddata2,])    # combine the bits
    subdata2 <- rbind(subdata2,sampledDat[[3]][adddata3,])    # combine the bits
    #   pickfrom <- seq(1,catSize,1)
    #    picked <- sort(sample(pickfrom,catSize,replace=TRUE))
    #   subdata2 <-subdata[picked,]
    ## Create required fields for logistic regression
    SizeMat <- table(subdata2$Length, subdata2$Mat)
    SizeMat <- as.data.frame(rbind(SizeMat))
    SizeMat$Length <- as.numeric(rownames(SizeMat))
    head(SizeMat)
    SizeMat$Total <- NA
    SizeMat$Total <- SizeMat$I + SizeMat$M
    SizeMat$MatRatio <- NA
    SizeMat$MatRatio <- SizeMat$M/SizeMat$Total
    out <- doLogistic(SizeMat)
    bootResults[iter,1] <- out$LM75
    bootResults[iter,2] <- out$IQ
    model <- out$Model
    bootResults[iter,3:4] <- model$coef
    scN <- numeric(3)
    pick <- which(SizeMat$Length <= 90)
    scN[1] <- sum(SizeMat$Total[pick],na.rm=T)
    pick <- which((SizeMat$Length > 90) & (SizeMat$Length <= 130))
    scN[2] <- sum(SizeMat$Total[pick],na.rm=T)
    pick <- which(SizeMat$Length > 130)
    scN[3] <- sum(SizeMat$Total[pick],na.rm=T)
    bootResults[iter,5:8] <- c(scN,sum(scN))
    #  plotgraph(SizeMat,out$LM50,Site1,scN,savefile=T)
  }
  
  
  droplevels(Sites[pSite])
  p <- c(2.5, 50, 97.5)/100
  bootResults <- as.data.frame(bootResults)
  BaseResults[pSite,5]<-quantile(bootResults$LM75,0.5)
  BaseResults[pSite,6]<-quantile(bootResults$LM75,0.05)
  BaseResults[pSite,7]<-quantile(bootResults$LM75,0.95)
  # # Obtain the 95% CI of Lm50 - Lower CI=2.5% & upper CI=97.5% 
  # LM75boot<-as.data.frame(quantile(bootResults$LM75, p))
  
  ##
  ## Step 3: Run from here to the very bottom
  reps <- 1000
  columns <- c("LM95","IQ","a","b","smallN","mediumN","largeN","totalN")
  bootResults <- matrix(0,nrow=reps,ncol=length(columns),dimnames=list(seq(1,reps,1),columns))
  
  sampledDat <- split(subdata,subdata$SizeC)
  catSizeLarge <- nrow(sampledDat[[1]])
  catSizeMedium <- nrow(sampledDat[[2]])
  catSizeSmall <- nrow(sampledDat[[3]])
  pickfrom1 <- seq(1,catSizeLarge,1)
  pickfrom2 <- seq(1,catSizeMedium,1)
  pickfrom3 <- seq(1,catSizeSmall,1)
  
  for (iter in 1:reps) {
    ## do the random sampling
    
    
    adddata1 <- sort(sample(pickfrom1,catSizeLarge,replace=TRUE))
    adddata2 <- sort(sample(pickfrom2,catSizeMedium,replace=TRUE))
    adddata3 <- sort(sample(pickfrom3,catSizeSmall,replace=TRUE))
    
    subdata2 <- rbind(sampledDat[[1]][adddata1,],sampledDat[[2]][adddata2,])    # combine the bits
    subdata2 <- rbind(subdata2,sampledDat[[3]][adddata3,])    # combine the bits
    #   pickfrom <- seq(1,catSize,1)
    #    picked <- sort(sample(pickfrom,catSize,replace=TRUE))
    #   subdata2 <-subdata[picked,]
    ## Create required fields for logistic regression
    SizeMat <- table(subdata2$Length, subdata2$Mat)
    SizeMat <- as.data.frame(rbind(SizeMat))
    SizeMat$Length <- as.numeric(rownames(SizeMat))
    head(SizeMat)
    SizeMat$Total <- NA
    SizeMat$Total <- SizeMat$I + SizeMat$M
    SizeMat$MatRatio <- NA
    SizeMat$MatRatio <- SizeMat$M/SizeMat$Total
    out <- doLogistic(SizeMat)
    bootResults[iter,1] <- out$LM95
    bootResults[iter,2] <- out$IQ
    model <- out$Model
    bootResults[iter,3:4] <- model$coef
    scN <- numeric(3)
    pick <- which(SizeMat$Length <= 90)
    scN[1] <- sum(SizeMat$Total[pick],na.rm=T)
    pick <- which((SizeMat$Length > 90) & (SizeMat$Length <= 130))
    scN[2] <- sum(SizeMat$Total[pick],na.rm=T)
    pick <- which(SizeMat$Length > 130)
    scN[3] <- sum(SizeMat$Total[pick],na.rm=T)
    bootResults[iter,5:8] <- c(scN,sum(scN))
    #  plotgraph(SizeMat,out$LM50,Site1,scN,savefile=F)
  }
  
  
  droplevels(Sites[pSite])
  p <- c(2.5, 50, 97.5)/100
  bootResults <- as.data.frame(bootResults)
  BaseResults[pSite,8]<-quantile(bootResults$LM95,0.5)
  BaseResults[pSite,9]<-quantile(bootResults$LM95,0.05)
  BaseResults[pSite,10]<-quantile(bootResults$LM95,0.95)
  BaseResults[pSite,19]<-BaseResults[pSite,8]-BaseResults[pSite,1]
  
  plotgraph(SizeMat,out$LM50,out$LM75, out$LM95,Site,scN,savefile=F)
  
  # # Obtain the 95% CI of Lm50 - Lower CI=2.5% & upper CI=97.5% 
  # LM95boot<-as.data.frame(quantile(bootResults$LM95, p))
  # 
}