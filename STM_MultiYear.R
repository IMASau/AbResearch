###############################################
#Calculate the Size transition matrix and eLML
###############################################
#
# #==============================================================
# #
# #                 Calculate the Size transition matrix and eLML
# #
# #==============================================================
library(tidyverse)


myWorkDrive <- "C:/"  ## Craig and Hugh
myWorkFolder <- "CloudStor/R_Stuff/Logistic"
myWorkPath <- paste(myWorkDrive,myWorkFolder,sep="")
setwd(myWorkPath)

#load("c:/CloudStor/R_Stuff/Logistic/SAMILRESULTS.RData")

source("C:/GitCode/AbResearch/Grwth_matrix.r")

#Calculate SigMaxLD50 for each site
SAMILResults$SigMaxLD50<-SAMILResults$MaxDL/(1+exp((log(19)*(SAMILResults$LD50-SAMILResults$L50)/(SAMILResults$L95-SAMILResults$L50))))

#SAMILResults$SigMaxLD50<-SAMILResults$MaxDL/(1+exp((log(19)*(2-SAMILResults$L50)/(SAMILResults$L95-SAMILResults$L50))))

#Calculate SigMaxL95 for each site
SAMILResults$SigMaxLD95<-SAMILResults$MaxDL/(1+exp((log(19)*(SAMILResults$LD95-SAMILResults$L50)/(SAMILResults$L95-SAMILResults$L50))))

Sites <- unique(SAMILResults$SiteCode)
# i <- "125_1988_8" # Gardens Site 8
# 
# i <- "610_2003_11" # Mary Anne Reef, Recercher Bay
# i <- "272_2001_3" # Point Hibbs
# i <- "458_2002_2" # Black ISland, West Coast
# 
#####
#     L50% multiyear
#####
if (exists("eLMLResults")) 
  rm(eLMLResults)

for(i in Sites){
  choice <- subset(SAMILResults, SiteCode == i)
  param <- c(choice$MaxDL,choice$L50,choice$L95,choice$SigMaxLD50) # MaxDL, L50, L95, SigMax
  Lm50 <- choice$LD50 # estimated size at 50% maturity
  LML <- choice$LML
  #eLML from L50
  midpts <- seq(2,210,2)
  G <- STM(param,midpts)
  Nt <- numeric(105)
  Nt[trunc(Lm50/2)] <- 1000
  Nt1 <- G %*% Nt  #1 year growth post LM50
  Nt2 <-G %*% (G %*% Nt) #2 year growth post LM50
  Nt3 <-G %*% (G %*% (G %*% Nt))  #3 year growth post LM50
  choice$eLML50.1y<-(findmedL(Nt1))
  choice$eLML50.2y<-(findmedL(Nt2))
  choice$eLML50.3y<-(findmedL(Nt3))
  #%pp 1yr
  Nt1df<-as.data.frame(Nt1)
  Nt1df$Length<-midpts
  pick<-which(Nt1df$V1 > 0)
  Nt1df <- Nt1df[pick,]
  pick<-which(Nt1df$Length <= LML)
  L.LML1 <- Nt1df[pick,]
  choice$PPLM50.1yr<-sum(L.LML1$V1/10)/2  ## ? why the /2???
  #% pp 2yr
  Nt2df<-as.data.frame(Nt2)
  Nt2df$Length<-midpts
  pick<-which(Nt2df$V1 > 0)
  Nt2df <- Nt2df[pick,]
  pick<-which(Nt2df$Length <= LML)
  L.LML2 <- Nt2df[pick,]
  choice$PPLM50.2yr<-sum(L.LML2$V1/10)/2
  #% pp 3yr
  Nt3df<-as.data.frame(Nt3)
  Nt3df$Length<-midpts
  pick<-which(Nt3df$V1 > 0)
  Nt3df <- Nt3df[pick,]
  pick<-which(Nt3df$Length <= LML)
  L.LML3 <- Nt3df[pick,]
  choice$PPLM50.3yr<-sum(L.LML3$V1/10)/2
  
  #pick<-choice[,c(1,25:30)]  
  if (exists("eLMLResults"))
    eLMLResults <- rbind(eLMLResults, choice)
  else
    eLMLResults <- choice
}
#pass back to GwthResults
GwthResults<-left_join(SAMILResults, eLMLResults, by='SiteCode')


#####
#     L90% multiyear
#####
if (exists("eLMLResults")) 
  rm(eLMLResults)

for(i in Sites){
  choice<-subset(SAMILResults, SiteCode == i)
  param <- c(choice$MaxDL,choice$L50,choice$L95,choice$SigMaxLD95) # MaxDL, L50, L95, SigMax
  Lm50 <- choice$LD95 # estimated size at 50% maturity
  LML <- choice$LML
  #eLML from L50
  midpts <- seq(2,210,2)
  G <- STM(param,midpts)
  Nt <- numeric(105)
  Nt[trunc(Lm50/2)] <- 1000
  Nt1 <- G %*% Nt  #1 year growth post LM50
  Nt2 <-G %*% (G %*% Nt) #2 year Grwoth post LM50
  Nt3 <-G %*% (G %*% (G %*% Nt))  #3 year Grwoth post LM50
  choice$eLML90.1y<-(findmedL(Nt1))
  choice$eLML90.2y<-(findmedL(Nt2))
  choice$eLML90.3y<-(findmedL(Nt3))
  #%pp 1yr
  Nt1df<-as.data.frame(Nt1)
  Nt1df$Length<-midpts
  pick<-which(Nt1df$V1 > 0)
  Nt1df <- Nt1df[pick,]
  pick<-which(Nt1df$Length <= LML)
  L.LML1 <- Nt1df[pick,]
  choice$PPLM90.1yr<-sum(L.LML1$V1/10)/2
  #% pp 2yr
  Nt2df<-as.data.frame(Nt2)
  Nt2df$Length<-midpts
  pick<-which(Nt2df$V1 > 0)
  Nt2df <- Nt2df[pick,]
  pick<-which(Nt2df$Length <= LML)
  L.LML2 <- Nt2df[pick,]
  choice$PPLM90.2yr<-sum(L.LML2$V1/10)/2
  #% pp 3yr
  Nt3df<-as.data.frame(Nt3)
  Nt3df$Length<-midpts
  pick<-which(Nt3df$V1 > 0)
  Nt3df <- Nt3df[pick,]
  pick<-which(Nt3df$Length <= LML)
  L.LML3 <- Nt3df[pick,]
  choice$PPLM90.3yr<-sum(L.LML3$V1/10)/2
  
  pick<-choice[,c(1,25:30)]
  if (exists("eLMLResults"))
    eLMLResults <- rbind(eLMLResults, pick)
  else
    eLMLResults <- pick
}
#pass back to GwthResults
GwthResults<-join(GwthResults, eLMLResults, by='SiteCode')

#save Rfile
outFile <- paste(myWorkPath,"/GWTHRESULTS_",format(Sys.time(), "%Y-%m-%d"),".RData",sep ="")
save.image(file = outFile)


## CM Mucking about - growing from 2mm

Nt1 <- G %*% Nt  #1 year growth
Nt2 <-G %*% (G %*% Nt) #2 year Growth
Nt3 <-G %*% (G %*% (G %*% Nt))  #3 year Growth
Nt4 <-G %*% (G %*% (G %*% (G %*% Nt)))  #4 year Growth
Nt5 <-G %*% (G %*% (G %*% (G %*% (G %*% Nt))))  #5 year Growth
Nt6 <-G %*% (G %*% (G %*% (G %*% (G %*% (G %*% Nt)))))  #6 year Growth
Nt7 <-G %*% (G %*% (G %*% (G %*% (G %*% (G %*% (G %*% Nt))))))  #7 year Growth
Nt8 <-G %*% (G %*% (G %*% (G %*% (G %*% (G %*% (G %*% (G %*% Nt)))))))  #8 year Growth
Nt9 <-G %*% (G %*% (G %*% (G %*% (G %*% (G %*% (G %*% (G %*% (G %*% Nt))))))))  #9 year Growth
Nt10 <-G %*% (G %*% (G %*% (G %*% (G %*% (G %*% (G %*% (G %*% (G %*% (G %*% Nt)))))))))  #10 year Growth
(findmedL(Nt1))
(findmedL(Nt2))
(findmedL(Nt3))
(findmedL(Nt4))
(findmedL(Nt5))
(findmedL(Nt6))
(findmedL(Nt7))
(findmedL(Nt8))
(findmedL(Nt9))
(findmedL(Nt10))

Nt1df<-as.data.frame(Nt1)
Nt1df$Length<-midpts

Nt2df<-as.data.frame(Nt2)
Nt2df$Length<-midpts

Nt3df<-as.data.frame(Nt3)
Nt3df$Length<-midpts

Nt4df<-as.data.frame(Nt4)
Nt4df$Length<-midpts

Nt5df<-as.data.frame(Nt5)
Nt5df$Length<-midpts

Nt6df<-as.data.frame(Nt6)
Nt6df$Length<-midpts

Nt7df<-as.data.frame(Nt7)
Nt7df$Length<-midpts

Nt8df<-as.data.frame(Nt8)
Nt8df$Length<-midpts

Nt9df<-as.data.frame(Nt9)
Nt9df$Length<-midpts

Nt10df<-as.data.frame(Nt10)
Nt10df$Length<-midpts




ggplot(Nt2df) + geom_line(aes(x = Length, y = V1), size = 1) +   geom_vline(
 xintercept = Lm50,
 colour = 'red',
 size = 1,
 linetype = 3
) + xlim(100, 200)

