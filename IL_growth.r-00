library(grDevices)
library(data.table)
library(dplyr)
rm(list=ls())   # Cleanup the R console if required

setwd("D:/Fisheries Research/Abalone/SAM/Growth code")
#source("C:/A_CSIRO/Rcode/CEUtils/lm_utils.r")
#source("C:/A_CSIRO/Rcode/CEUtils/abalone_utils.r")
#source("C:/A_CSIRO/Rcode/CEUtils/abdata_utils.r")
source("D:/GitCode/AbResearch/ab_funs1.r")
source("D:/GitCode/AbResearch/IL_funs.r")
#source("D:/Fisheries Research/Abalone/SAM/MH growth code/abalonesubblks.r")
#source(paste("D:/Fisheries Research/Abalone/SAM/MH growth code/fishMH.r",sep=""))

#load("D:/R_Stuff/SAM/Logistic/IL_output200916.RData")

GwthRaw<-read.csv("GwthDatablacklip.csv", header=TRUE)
dim(GwthRaw)
head(GwthRaw,20)
summary(GwthRaw)

SiteDrop<-c(802)
pick <- which(GwthRaw$SiteId %in% SiteDrop)
Gwth<-GwthRaw[-pick,]
GwthRaw<-subset(GwthRaw)

sitenames <-  read.csv("sitenames.csv", header=TRUE)
sitenos <- sitenames$SiteId
Nsites <- length(sitenos)
GwthRaw$SiteN <- NA

for (i in 1:Nsites) {
   pick <- which(Gwth$SiteId == sitenos[i])
   GwthRaw$SiteN[pick] <- as.character(sitenames$NameSh[i])
}
unique(GwthRaw$SiteN)


#Gwth<-droplevels(subset(GwthRaw, Dt >= 0.25 & Dt < 5))


sitechar <- as.data.frame(matrix(0,nrow=Nsites,ncol=7,dimnames=list(sitenos,c("SiteId","Name",
                  "Longitude","Latitude","StatBlock","Recap_Year","Records"))))
for (i in 1:Nsites) {
   pick <- which(Gwth$SiteId == sitenos[i])
   sitechar[i,] <- c(sitenos[i],as.character(Gwth$SiteN[pick[1]]),Gwth$Long[pick[1]],Gwth$Lat[pick[1]],
                     Gwth$StatBlock[pick[1]],Gwth$Recap_Year[pick[1]],length(pick))
}
sitechar
sitechar<-sitechar[complete.cases(sitechar),]
sitechar
sitechar <- sitechar[order(sitechar[,5]),]


  Nsites <- length(sitechar$SiteId)
  sitenos<-unique(as.numeric(sitechar$SiteId))
  
  # subs<-sitechar[2:4,]
  # sitenos<-unique(as.numeric(subs$SiteId))
  

## Calculate Base cases for each site.
  ILcolumns <- c("L50", "L95", "MaxDL", "MaxSig", "SiteName")
  ILOutput <- matrix(0,nrow=Nsites,ncol=length(ILcolumns),dimnames=list(sitenos,ILcolumns))
  
 
  for (p in sitenos) {
  absite <- getsitedata(p,Gwth)
   bins <- seq(min(absite$Dt),max(absite$Dt),0.1)
   #hist(absite$Dt,breaks=bins)
   nP <- length(absite$Dt)
   columns <- c("Lt","Dt","MaxDL","L50","L95","MaxSig","-veLL","Influence")
   numcol <- length(columns)
   rows <- 1:nP
   influen <- matrix(0,nrow=nP,ncol=numcol,dimnames=list(rows,columns))
   model <- fitIL(absite$Lt,absite$DL,SiteId=p,outliers=T,sitename=as.character(absite$SiteN[1]))
   BaseCase <- c(NA,NA,model$model$estimate, model$model$minimum,NA)
   LtDL <- c(13,15)
   for (i in 1:nP) {
      model <- fitIL(absite$Lt[-i],absite$DL[-i],SiteId=p,outliers=F,sitename=as.character(absite$SiteN[1]))
      ans <- unlist(c(absite[i,LtDL],model$model$estimate, model$model$minimum,BaseCase[numcol-1]-model$model$minimum))
      influen[i,] <- ans
   }
   impact <- influen[order(influen[,numcol],decreasing=TRUE),]
   impact <- rbind(BaseCase,impact)
   head(impact,30)
   plot(model)
   q<-as.character(p)
   ILOutput[q,1]<-model$L50
   ILOutput[q,2]<-model$L95
   ILOutput[q,3]<-model$MaxDL
   ILOutput[q,4]<-model$MaxSig
   ILOutput[q,5]<-absite$SiteN[1]
   
   # jpeg(filename = paste("ILOut",absite$SiteN[1],".jpeg", sep='_'))
   # print<-plotted
   # dev.off()
   
   # pick <- 6
   # hist(BaseCase[pick]-impact[,pick],breaks="Sturges")

  plotsingleIL(model)
  # jpeg(filename = paste("ILCurve",absite$SiteN[1],".jpeg", sep='_'))
  # print<-plotted
  # dev.off()
}
ILResults<-as.data.frame(ILOutput)
ILResults<-setDT(ILResults, keep.rownames =T[])
names(ILResults)[names(ILResults)=='rn']<-"SiteId"
ILResults<-as.data.frame(ILResults)

ILResults<-left_join(ILResults, sitechar, by = "SiteId")

save(ILResults, file='D:/R_Stuff/SAM/Logistic/ILResults.RData')
