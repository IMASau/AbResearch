library(MASS)
library(gdata)
library(doBy)
library(xlsx)
library(reshape)
library(boot)
library(car)
library(plyr)


##-----------------------------
## Need to work out a way to combine the full sample with the top up sample (good grief)
##-----------------------------


setwd('D:\\R_Stuff\\Logistic')
infile <- "D:\\R_Stuff\\Logistic/BlacklipSAM.txt"
#Load raw csv file
BlckPop <- read.csv(infile, header=TRUE, sep=',', dec='.', as.is=TRUE)
BlckPop <- BlckPop[order(BlckPop$SiteCode,BlckPop$SPC_ShellLength),]

## Note SiteCode is a single field that combines SiteNumber, Year & Month
## This effectively pools all data colelcted from a site in the same month,
## to address sites where sampling was collected over multiple days, but have 
## different sample codes for some reason. May have some unintended side effects


Sex <- as.data.frame(table(BlckPop$SPC_Sex))
SAM_Type <- as.data.frame(table(BlckPop$SAM_Type))
GndStg <- as.data.frame(table(BlckPop$SPC_GonadStage))

Trem <- subset(BlckPop, BlckPop$SPC_Sex =="T") # Remove Trematode records

## Subset
samdata <- subset(BlckPop, SPC_Sex %in% c("I", "M", "F"))

## re-code Male and Female as M for Mature (I = Imature)
samdata$Mat <- ifelse(samdata$SPC_Sex=="I", c("I"), c("M")) 
#samdata$Mat[samdata$SPC_GonadStage %in% c("0")] <- "I"

#samdata.subdata <- droplevels(subset(samdata, SiteCode =="217_1999_10"))
sites <- as.data.frame(table(samdata$SiteCode))
sites

## Routine Outlier Removal: Unusual large immature animals
pick <- which((samdata$SPC_ShellLength >139) & (samdata$Mat=="I"))
outlier <- samdata[pick,]
samdata <- samdata[-pick,]

## Routine Outlier Removal: Unusual small mature animals
pick <- which((samdata$SPC_ShellLength <60) & (samdata$Mat=="M"))
outlier <- rbind(outlier,samdata[pick,])
samdata <- samdata[-pick,]


Mat <- as.data.frame(table(samdata$Mat))
Mat
Mat <- as.data.frame(table(samdata$SPC_Sex))
Mat
SamList <- as.data.frame(table(samdata$SiteCode))
colnames(SamList) <- c("SiteCode", "N")


SamList$LD05 <- as.numeric(NA)
SamList$LD25 <- as.numeric(NA)
SamList$LD50 <- as.numeric(NA)
SamList$LD75 <- as.numeric(NA)
SamList$LD95 <- as.numeric(NA)
SamList$IQR <- as.numeric(NA)
SamList$N.underLD05 <- as.numeric(NA)
SamList$N.underLD50 <- as.numeric(NA)
SamList$N.overLD95 <- as.numeric(NA)
SamList$Ld50BootU95 <- as.numeric(NA)
SamList$Ld50BootL95 <- as.numeric(NA)
SamList$Ld95BootU95 <- as.numeric(NA)
SamList$Ld95BootL95 <- as.numeric(NA)

NumSites <- nrow(SamList)
NumSites
#SiteIDList <- droplevels(SiteIDList[-117,])

SamList <- subset(SamList, N> 150, drop=TRUE)
SamList[117,]
SamList <- subset(SamList, SiteCode!="841_2009_9", drop=TRUE)
SamList <- subset(SamList, SiteCode!="216_1999_10", drop=TRUE)
SamList <- subset(SamList, SiteCode!="155_2007_8", drop=TRUE)
SamList <- subset(SamList, SiteCode!="155_2008_8", drop=TRUE)
SamList <- subset(SamList, SiteCode!="161_1998_5", drop=TRUE)
SamList <- subset(SamList, SiteCode!="285_2000_2", drop=TRUE)
SamList <- subset(SamList, SiteCode!="293_2000_2", drop=TRUE)
SamList <- subset(SamList, SiteCode!="358_1994_6", drop=TRUE)
SamList <- subset(SamList, SiteCode!="473_2010_10", drop=TRUE)

NumSites <- nrow(SamList); NumSites


i <- 108
i <- 400
#Loop through unique DiveId's
for (i in 1:NumSites) {
  subdat <- droplevels(subset(samdata, samdata$SiteCode == SamList[i,1]))
  SizeMat <- table(subdat$SPC_ShellLength, subdat$Mat)
  SizeMat <- as.data.frame(rbind(SizeMat))
  SizeMat$ShellLength <- as.numeric(rownames(SizeMat))
  head(SizeMat)
  SizeMat$Total <- SizeMat$I + SizeMat$M
  SizeMat$MatRatio <- SizeMat$M/SizeMat$Total
  r <- glm(MatRatio ~ ShellLength, family=binomial(link = "logit"), data = SizeMat, weights = Total)

  ld05 <- dose.p(r, p = 0.05); 
  SamList$LD05[i] <-  as.vector(ld05)
  ld25 <- dose.p(r, p = 0.25); 
  SamList$LD25[i] <-  as.vector(ld25)
  ld50 <- dose.p(r, p = 0.5); 
  SamList$LD50[i] <-  as.numeric(ld50)
  ld75 <- dose.p(r, p = 0.75); 
  SamList$LD75[i] <-  as.vector(ld75)
  ld95 <- dose.p(r, p = 0.95); 
  SamList$LD95[i] <-  as.numeric(ld95)
  SamList$IQR[i] <-  SamList$LD75[i] - SamList$LD25[i]
  
  
  N.underLD05 <-  nrow(droplevels(subset(subdat, subdat$SPC_ShellLength < as.integer(as.vector(ld05)))))
  SamList$N.underLD05[i] <-  as.numeric(N.underLD05)
  N.underLD50 <-  nrow(droplevels(subset(subdat, subdat$SPC_ShellLength < as.integer(as.vector(ld50)))))
  SamList$N.underLD50[i] <-  as.numeric(N.underLD50)
   N.overLD95 <-  nrow(droplevels(subset(subdat, subdat$SPC_ShellLength >= as.integer(as.vector(ld95)))))
  SamList$N.overLD95[i] <-  as.numeric(N.overLD95)

  
  ## Bootstrap the ld50 paramater ####
  BootSAM50 <- function(data, indices) {
   bootdata <- data[indices, ]
   SizeMat <- table(bootdata$SPC_ShellLength, bootdata$Mat)
   SizeMat <- as.data.frame(rbind(SizeMat))
   SizeMat$ShellLength <- as.numeric(rownames(SizeMat))
   SizeMat$Total <- SizeMat$I + SizeMat$M
   SizeMat$MatRatio <- SizeMat$M/SizeMat$Total
   model <-  glm(MatRatio ~ ShellLength, family=binomial(link = "logit"), data = SizeMat, weights = Total)
   #conRD <- -coef(model)[1]/coef(model)[2]
   ld50 <- dose.p(model, p = 0.5)
   return(ld50)
  }
  try({
    
  booted.SAM <- boot(subdat,statistic=BootSAM50, R= 1000)
  booted.SAM.CI <- boot.ci(booted.SAM,type="bca",conf = c(0.95))
  boot95.lower <- format(booted.SAM.CI$bca[4],nsmall = 2)
  boot95.upper <- format(booted.SAM.CI$bca[5],nsmall = 2)
  
  SamList$Ld50BootU95[i] <-  as.numeric(boot95.upper)
  SamList$Ld50BootL95[i] <-  as.numeric(boot95.lower)
  
  })
  
  ## Bootstrap the ld95 paramater ####
  BootSAM95 <- function(data, indices) {
   bootdata <- data[indices, ]
   SizeMat <- table(bootdata$SPC_ShellLength, bootdata$Mat)
   SizeMat <- as.data.frame(rbind(SizeMat))
   SizeMat$ShellLength <- as.numeric(rownames(SizeMat))
   SizeMat$Total <- SizeMat$I + SizeMat$M
   SizeMat$MatRatio <- SizeMat$M/SizeMat$Total
   model <-  glm(MatRatio ~ ShellLength, family=binomial(link = "logit"), data = SizeMat, weights = Total)
   #conRD <- -coef(model)[1]/coef(model)[2]
   ld95 <- dose.p(model, p = 0.95)
   return(ld95)
  }
  try({
    
  booted.SAM <- boot(subdat,statistic=BootSAM95, R= 1000)
  booted.SAM.CI <- boot.ci(booted.SAM,type="bca",conf = c(0.95))
  boot95.lower <- format(booted.SAM.CI$bca[4],nsmall = 2)
  boot95.upper <- format(booted.SAM.CI$bca[5],nsmall = 2)
  
  SamList$Ld95BootU95[i] <-  as.numeric(boot95.upper)
  SamList$Ld95BootL95[i] <-  as.numeric(boot95.lower)
  
  })

}
SiteNames <- unique(samdata[,1:8])
SamResults <- merge(SamList, SiteNames, by.x="SiteCode", by.Y="SiteCode", all.y=FALSE)

write.xlsx(SamResults, "D:\\R_Stuff\\Logistic\\SamResultsBoot.xlsx", sheetName="SAM",  col.names=TRUE, row.names=TRUE, append=FALSE)


##Size at Emergence ####

Shell <- as.data.frame(table(BlckPop$SPC_ShellCover))

shelldata <- subset(BlckPop, BlckPop$SPC_ShellCover %in% c("A","P","0","1","2","3"))
shelldata$Shell <- ifelse(shelldata$SPC_ShellCover %in% c("A","0"), c("C"), c("E")) 

SizeShell <- table(shelldata$SiteCode, shelldata$Shell)

Shell <- as.data.frame(table(shelldata$Shell))
Shell

#shelldata.subdata <- droplevels(subset(shelldata, SiteCode =="268_2000_2"))

## Outlier Removal: Unusual large immature animals
pick <- which((shelldata$SPC_ShellLength >150) & (shelldata$Shell=="C"))
outlier <- shelldata[pick,]
shelldata <- shelldata[-pick,]

## Outlier Removal: Unusual small mature animals
pick <- which((shelldata$SPC_ShellLength <60) & (shelldata$Shell=="E"))
outlier <- rbind(outlier,shelldata[pick,])
shelldata <- shelldata[-pick,]

Shell <- as.data.frame(table(shelldata$Shell))
Shell


ShellList <- as.data.frame(table(shelldata$SiteCode))
colnames(ShellList) <- c("SiteCode", "N")

ShellList$LD05 <- as.numeric(NA)
ShellList$LD25 <- as.numeric(NA)
ShellList$LD50 <- as.numeric(NA)
ShellList$LD75 <- as.numeric(NA)
ShellList$LD95 <- as.numeric(NA)
ShellList$IQR <- as.numeric(NA)
ShellList$N.underLD05 <- as.numeric(NA)
ShellList$N.underLD50 <- as.numeric(NA)
ShellList$N.overLD95 <- as.numeric(NA)
ShellList$Ld50BootU95 <- as.numeric(NA)
ShellList$Ld50BootL95 <- as.numeric(NA)
ShellList$Ld95BootU95 <- as.numeric(NA)
ShellList$Ld95BootL95 <- as.numeric(NA)

NumSites <- nrow(ShellList)
NumSites
ShellList <- subset(ShellList, N> 150, drop=TRUE)
ShellList[417,]
ShellList <- subset(ShellList, SiteCode!="841_2009_9", drop=TRUE)
ShellList <- subset(ShellList, SiteCode!="268_2000_2", drop=TRUE)
ShellList <- subset(ShellList, SiteCode!="285_2000_2", drop=TRUE)
ShellList <- subset(ShellList, SiteCode!="290_2000_2", drop=TRUE)
ShellList <- subset(ShellList, SiteCode!="483_2002_11", drop=TRUE)
ShellList <- subset(ShellList, SiteCode!="649_1994_1", drop=TRUE)
ShellList <- subset(ShellList, SiteCode!="821_2009_11", drop=TRUE)


NumSites <- nrow(ShellList); NumSites
NumSites

i <- 450
#Loop through unique DiveId's
for (i in 1:NumSites) {
  subdat <- droplevels(subset(shelldata, shelldata$SiteCode == ShellList[i,1]))
  SizeShell <- table(subdat$SPC_ShellLength, subdat$Shell)
  SizeShell <- as.data.frame(rbind(SizeShell))
  SizeShell$ShellLength <- as.numeric(rownames(SizeShell))
  head(SizeShell)
  SizeShell$Total <- SizeShell$C + SizeShell$E
  SizeShell$ShellRatio <- SizeShell$E/SizeShell$Total
  r <- glm(ShellRatio ~ ShellLength, family=binomial(link = "logit"), data = SizeShell, weights = Total)
  
    ld05 <- dose.p(r, p = 0.05); 
  ShellList$LD05[i] <-  as.vector(ld05)
  ld25 <- dose.p(r, p = 0.25); 
  ShellList$LD25[i] <-  as.vector(ld25)
  ld50 <- dose.p(r, p = 0.5); 
  ShellList$LD50[i] <-  as.numeric(ld50)
  ld75 <- dose.p(r, p = 0.75); 
  ShellList$LD75[i] <-  as.vector(ld75)
  ld95 <- dose.p(r, p = 0.95); 
  ShellList$LD95[i] <-  as.numeric(ld95)
  ShellList$IQR[i] <-  ShellList$LD75[i] - ShellList$LD25[i]
  
  N.underLD05 <-  nrow(droplevels(subset(subdat, subdat$SPC_ShellLength < as.integer(as.vector(ld05)))))
  ShellList$N.underLD05[i] <-  as.numeric(N.underLD05)
  N.underLD50 <-  nrow(droplevels(subset(subdat, subdat$SPC_ShellLength < as.integer(as.vector(ld50)))))
  ShellList$N.underLD50[i] <-  as.numeric(N.underLD50)
  N.overLD95 <-  nrow(droplevels(subset(subdat, subdat$SPC_ShellLength >= as.integer(as.vector(ld95)))))
  ShellList$N.overLD95[i] <-  as.numeric(N.overLD95)
  
  
  ## Bootstrap the ld50 paramater
  BootSEM50 <- function(data, indices) {
   bootdata <- data[indices, ]
   SizeShell <- table(bootdata$SPC_ShellLength, bootdata$Shell)
   SizeShell <- as.data.frame(rbind(SizeShell))
   SizeShell$ShellLength <- as.numeric(rownames(SizeShell))
   SizeShell$Total <- SizeShell$C + SizeShell$E
   SizeShell$ShellRatio <- SizeShell$E/SizeShell$Total
   model <-  glm(ShellRatio ~ ShellLength, family=binomial(link = "logit"), data = SizeShell, weights = Total)
   #conRD <- -coef(model)[1]/coef(model)[2]
   ld50 <- dose.p(model, p = 0.5)
   return(ld50)
  }
 try({
  booted.SEM <- boot(subdat,statistic=BootSEM50, R= 1000)
  booted.SEM.CI <- boot.ci(booted.SEM,type="bca",conf = c(0.95))
  boot95.lower <- format(booted.SEM.CI$bca[4],nsmall = 2)
  boot95.upper <- format(booted.SEM.CI$bca[5],nsmall = 2)
  ShellList$Ld50BootU95[i] <-  as.numeric(boot95.upper)
  ShellList$Ld50BootL95[i] <-  as.numeric(boot95.lower)
  
})

    ## Bootstrap the ld95 paramater
  BootSEM95 <- function(data, indices) {
   bootdata <- data[indices, ]
   SizeShell <- table(bootdata$SPC_ShellLength, bootdata$Shell)
   SizeShell <- as.data.frame(rbind(SizeShell))
   SizeShell$ShellLength <- as.numeric(rownames(SizeShell))
   SizeShell$Total <- SizeShell$C + SizeShell$E
   SizeShell$ShellRatio <- SizeShell$E/SizeShell$Total
   model <-  glm(ShellRatio ~ ShellLength, family=binomial(link = "logit"), data = SizeShell, weights = Total)
   #conRD <- -coef(model)[1]/coef(model)[2]
   ld95 <- dose.p(model, p = 0.95)
   return(ld95)
  }
 try({
  booted.SEM <- boot(subdat,statistic=BootSEM95, R= 1000)
  booted.SEM.CI <- boot.ci(booted.SEM,type="bca",conf = c(0.95))
  boot95.lower <- format(booted.SEM.CI$bca[4],nsmall = 2)
  boot95.upper <- format(booted.SEM.CI$bca[5],nsmall = 2)
  ShellList$Ld95BootU95[i] <-  as.numeric(boot95.upper)
  ShellList$Ld95BootL95[i] <-  as.numeric(boot95.lower)
  
})
  

}
SiteNames <- unique(shelldata[,1:8])
ShellResults <- merge(ShellList, SiteNames, by.x="SiteCode", by.Y="SiteCode", all.y=FALSE)

write.xlsx(ShellResults, "D:\\R_Stuff\\Logistic\\SamResultsBoot.xlsx", sheetName="SEM",  col.names=TRUE, row.names=TRUE, append=TRUE)

##Filter datasets to include only sites where there is a good spread of data
samfilt <- subset(SamList, N.underLD05 >15 & N.overLD95 > 15)
#shellfilt <- subset(ShellList, N.underLD05 >15 & N.overLD95 > 15)



colnames(samfilt) <- paste("SAM", colnames(samfilt), sep = ".")
shllListCols <- colnames(ShellList)
colnames(ShellList) <- paste("SEM", colnames(ShellList), sep = ".")
samfilt <- rename(samfilt, c("SAM.SiteCode"="SiteCode"))
ShellList <- rename(ShellList, c("SEM.SiteCode"="SiteCode"))

matched <- join(samfilt,ShellList,by="SiteCode",type="inner")
plot(matched$SAM.IQR ~ matched$SEM.IQR)
plot(matched$SAM.LD95 ~ matched$SEM.LD95)

write.xlsx(matched, "D:\\R_Stuff\\Logistic\\SamResultsBoot.xlsx", sheetName="SAM_SEM",  col.names=TRUE, row.names=TRUE, append=TRUE)



