## CarlyNewBoot.R
# Version 23/06/05  

rm(list=ls(all=TRUE))
## SET THE WORKING AND RESULTS DIRECTORIES
wkdir <- "D:\\Students\\Carly Giosio"
setwd(wkdir)
resdir <- "D:\\Students\\Carly Giosio/Results/"

## Load raw csv file
infile <- "D:\\Students\\Carly Giosio\\DataForExport.csv"

library(MASS)
library(boot)
library(car)
source("D:\\Students\\Carly Giosio/carly_utils.R")

samdata <- read.csv(infile,header=T)
dim(samdata)
head(samdata)

#Option - remove Wineglass Bay Site for Season * region analysis
pick <- which(samdata$Site_Name == "Wineglass Bay January")
samdata <- samdata[-pick,]
samdata <- droplevels(samdata)

# Simplify Maturity classes
samdata$Mat <- NA
samdata$Mat <- ifelse(samdata$Sex=="I", c("I"), c("M"))

# Characterize Sites
Sites <- unique(samdata$Site_Name); Sites
NS <- length(Sites)

# Code Season and Site
samdata$Season <- NA
samdata$Region <- NA
pick <- which(samdata$Site_Name == "St Helens January")
samdata$Season[pick] <- "January"
samdata$Region[pick] <- "St Helens"


pick <- which(samdata$Site_Name == "St Helens April")
samdata$Season[pick] <- "April"
samdata$Region[pick] <- "St Helens"

pick <- which(samdata$Site_Name == "George III Rock January")
samdata$Season[pick] <- "January"
samdata$Region[pick] <- "George III Rock"

pick <- which(samdata$Site_Name == "George III Rock April")
samdata$Season[pick] <- "April"
samdata$Region[pick] <- "George III Rock"

## Outlier Removal: St Helens January is the only Site with these outliers
pick <- which((samdata$Length >139) & (samdata$Mat=="I") &
               (samdata$Site_Name == "St Helens January"))
outlier <- samdata[pick,]
samdata <- samdata[-pick,]

pick <- which((samdata$Length < 103) & (samdata$Mat=="M") & (as.character(samdata$Site_Name) == "St Helens January"))
outlier <- rbind(outlier,samdata[pick,])
samdata <- samdata[-pick,]

pick <- which((samdata$Length >119) & (samdata$Mat=="I") &
               (as.character(samdata$Site_Name) == "St Helens April"))
outlier <- rbind(outlier,samdata[pick,])
samdata <- samdata[-pick,]

pick <- which((samdata$Length <92) & (samdata$Mat=="M") &
               (as.character(samdata$Site_Name) == "St Helens April"))
outlier <- rbind(outlier,samdata[pick,])
samdata <- samdata[-pick,]

## Outlier Removal: Wineglass Bay


## Outlier Removal: George III Rock
#George III Rock
pick <- which((samdata$Length < 60) & (samdata$Mat=="M") &
               (as.character(samdata$Site_Name) == "George III Rock January"))
outlier <- rbind(outlier,samdata[pick,])
samdata <- samdata[-pick,]

pick <- which((samdata$Length > 140) & (samdata$Mat=="I") &
               (as.character(samdata$Site_Name) == "George III Rock January"))
outlier <- rbind(outlier,samdata[pick,])
samdata <- samdata[-pick,]

#April sample
pick <- which((samdata$Length < 91) & (samdata$Mat=="M") &
               (as.character(samdata$Site_Name) == "George III Rock April"))
outlier <- rbind(outlier,samdata[pick,])
samdata <- samdata[-pick,]

pick <- which((samdata$Length > 109) & (samdata$Mat=="I") &
               (as.character(samdata$Site_Name) == "George III Rock April"))
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


source("D:\\Students\\Carly Giosio/carly_utils.R")

## Calculate Base cases for each site.
starttime <- unclass(Sys.time())
sizeCl <- c("Small","Medium","Large")
as.character(Sites)


# 1: George III Rock April
# 2: George III Rock January
# 3: St Helens April
# 4: St Helens January
# 5: Wineglass Bay January

##
## Step 1: Choose a site based on the number index above
pSite <- 2            #  CHANGE THIS TO SELECT WHICH Location TO USE
#  pSite <- 1  will analyse George III
#  pSite <- 3  will analyse St Helens

Site1 <- Sites[pSite]
pick <- which(samdata$Site_Name == Site1)
if (length(pick) > 0) {
 subdata <- samdata[pick,]
} else { "Oops something has gone wrong"
}


##
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
# Obtain the 95% CI of Lm50 - Lower CI=2.5% & upper CI=97.5% 
quantile(bootResults$LM50, p)


 

