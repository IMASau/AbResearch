###############################################
#Calculate the Size transition matrix and eLML
###############################################
#
#================================================================
# #
# #                           Uses outputs from Growth_data_Allocation_SAM
# #
# #================================================================
source("D:/GitCode/AbResearch/Grwth_matrix.r")

#Calculate SigMaxLD50 for each site
SAMILResults$SigMaxLD50<-SAMILResults$MaxDL/(1+exp((log(19)*(SAMILResults$LD50-SAMILResults$L50)/(SAMILResults$L95-SAMILResults$L50))))
#Calculate SigMaxL90 for each site
SAMILResults$SigMaxLD90<-SAMILResults$MaxDL/(1+exp((log(19)*(SAMILResults$LD90-SAMILResults$L50)/(SAMILResults$L95-SAMILResults$L50))))

Sites<-unique(SAMILResults$SiteCode)

#####
#     L50% multiyear
#####
if (exists("eLMLResults")) 
  rm(eLMLResults)

for(i in Sites){
  choice<-subset(SAMILResults, SiteCode == i)
  param <- c(choice$MaxDL,choice$L50,choice$L95,choice$SigMaxLD50) # MaxDL, L50, L95, SigMax
  Lm50 <- choice$LD50 # estimated size at 50% maturity
  LML <- choice$LML
  #eLML from L50
  midpts <- seq(2,210,2)
  G <- STM(param,midpts)
  Nt <- numeric(105)
  Nt[trunc(Lm50/2)] <- 1000
  Nt1 <- G %*% Nt  #1 year growth post LM50
  Nt2 <-G %*% (G %*% Nt) #2 year Grwoth post LM50
  Nt3 <-G %*% (G %*% (G %*% Nt))  #3 year Grwoth post LM50
  choice$eLML50.1y<-(findmedL(Nt1))
  choice$eLML50.2y<-(findmedL(Nt2))
  choice$eLML50.3y<-(findmedL(Nt3))
  #%pp 1yr
  Nt1df<-as.data.frame(Nt1)
  Nt1df$Length<-midpts
  pick<-which(Nt1df$V1 > 0)
  Nt1df <- Nt1df[pick,]
  pick<-which(Nt1df$Length >= LML)
  L.LML1 <- Nt1df[pick,]
  choice$PPLM50.1yr<-sum(L.LML1$V1/10)/2
  #% pp 2yr
  Nt2df<-as.data.frame(Nt2)
  Nt2df$Length<-midpts
  pick<-which(Nt2df$V1 > 0)
  Nt2df <- Nt2df[pick,]
  pick<-which(Nt2df$Length >= LML)
  L.LML2 <- Nt2df[pick,]
  choice$PPLM50.2yr<-sum(L.LML2$V1/10)/2
  #% pp 3yr
  Nt3df<-as.data.frame(Nt3)
  Nt3df$Length<-midpts
  pick<-which(Nt3df$V1 > 0)
  Nt3df <- Nt3df[pick,]
  pick<-which(Nt3df$Length >= LML)
  L.LML3 <- Nt3df[pick,]
  choice$PPLM50.3yr<-sum(L.LML3$V1/10)/2
  
  pick<-choice[,c(1,27:32)]  
  if (exists("eLMLResults"))
    eLMLResults <- rbind(eLMLResults, pick)
  else
    eLMLResults <- pick
}
#pass back to GwthResults
GwthResults<-join(SAMILResults, eLMLResults, by='SiteCode')


#####
#     L90% multiyear
#####
if (exists("eLMLResults")) 
  rm(eLMLResults)

for(i in Sites){
  choice<-subset(SAMILResults, SiteCode == i)
  param <- c(choice$MaxDL,choice$L50,choice$L95,choice$SigMaxLD50) # MaxDL, L50, L95, SigMax
  Lm50 <- choice$LD90 # estimated size at 50% maturity
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
  pick<-which(Nt1df$Length >= LML)
  L.LML1 <- Nt1df[pick,]
  choice$PPLM90.1yr<-sum(L.LML1$V1/10)/2
  #% pp 2yr
  Nt2df<-as.data.frame(Nt2)
  Nt2df$Length<-midpts
  pick<-which(Nt2df$V1 > 0)
  Nt2df <- Nt2df[pick,]
  pick<-which(Nt2df$Length >= LML)
  L.LML2 <- Nt2df[pick,]
  choice$PPLM90.2yr<-sum(L.LML2$V1/10)/2
  #% pp 3yr
  Nt3df<-as.data.frame(Nt3)
  Nt3df$Length<-midpts
  pick<-which(Nt3df$V1 > 0)
  Nt3df <- Nt3df[pick,]
  pick<-which(Nt3df$Length >= LML)
  L.LML3 <- Nt3df[pick,]
  choice$PPLM90.3yr<-sum(L.LML3$V1/10)/2
  
  pick<-choice[,c(1,27:32)]  
  if (exists("eLMLResults"))
    eLMLResults <- rbind(eLMLResults, pick)
  else
    eLMLResults <- pick
}
#pass back to GwthResults
GwthResults<-join(GwthResults, eLMLResults, by='SiteCode')
