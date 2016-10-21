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

#Calculate SigMax for each site
SAMILResults$SigMaxLD50<-SAMILResults$MaxDL/(1+exp((log(19)*(SAMILResults$LD50-SAMILResults$L50)/(SAMILResults$L95-SAMILResults$L50))))

Sites<-unique(SAMILResults$SiteCode)

#####
#     L50%
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
  Nt1 <- G %*% (G %*% Nt)
  choice$eLML50<-(findmedL(Nt1))
  Nt1df<-as.data.frame(Nt1)
  Nt1df$Length<-midpts
  pick<-which(Nt1df$V1 > 0)
  Nt1df <- Nt1df[pick,]
  pick<-which(Nt1df$Length <= LML)
  L.LML <- Nt1df[pick,]
  choice$PPLM50<-sum(L.LML$V1/10)/2
  
  pick<-choice[,c(1,56:57)]  
  if (exists("eLMLResults"))
    eLMLResults <- rbind(eLMLResults, pick)
  else
    eLMLResults <- pick
}
#pass back to GwthResults
GwthResults<-join(SAMILResults, eLMLResults, by='SiteCode')



# #Calculate SigMax for each site
# SAMILResults$SigMaxLD75<-SAMILResults$MaxDL/(1+exp((log(19)*(SAMILResults$LD75-SAMILResults$L50)/(SAMILResults$L95-SAMILResults$L50))))
# 
# #####
# #     L75%
# #####
# if (exists("eLMLResults")) 
#  rm(eLMLResults75)
# 
# for(i in Sites){
#  choice<-subset(SAMILResults, SiteCode == i)
#  param <- c(choice$MaxDL,choice$L50,choice$L95,choice$SigMaxLD75) # MaxDL, L50, L95, SigMax
#  Lm50 <- choice$LD75 # estimated size at 75% maturity
#  LML <- choice$LML
#  #eLML from L50
#  midpts <- seq(2,210,2)
#  G <- STM(param,midpts)
#  Nt <- numeric(105)
#  Nt[trunc(Lm50/2)] <- 1000
#  Nt1 <- G %*% (G %*% Nt)
#  choice$eLML75<-(findmedL(Nt1))
#  Nt1df<-as.data.frame(Nt1)
#  Nt1df$Length<-midpts
#  pick<-which(Nt1df$V1 > 0)
#  Nt1df <- Nt1df[pick,]
#  pick<-which(Nt1df$Length <= LML)
#  L.LML <- Nt1df[pick,]
#  choice$PPLM75<-sum(L.LML$V1/10)/2
#  
#  pick<-choice[,c(2,49:50)]  
#  if (exists("eLMLResults75"))
#   eLMLResults75 <- rbind(eLMLResults75, pick)
#  else
#   eLMLResults75 <- pick
# }
# 
# GwthResults<-join(GwthResults, eLMLResults75, by='SiteCode')
# 
# 
# #Calculate SigMax for each site
# SAMILResults$SigMaxLD85<-SAMILResults$MaxDL/(1+exp((log(19)*(SAMILResults$LD85-SAMILResults$L50)/(SAMILResults$L95-SAMILResults$L50))))
# 
# #####
# #     L85%
# #####
# if (exists("eLMLResults")) 
#  rm(eLMLResults85)
# 
# for(i in Sites){
#  choice<-subset(SAMILResults, SiteCode == i)
#  param <- c(choice$MaxDL,choice$L50,choice$L95,choice$SigMaxLD85) # MaxDL, L50, L95, SigMax
#  Lm50 <- choice$LD85 # estimated size at 75% maturity
#  LML <- choice$LML
#  #eLML from L50
#  midpts <- seq(2,210,2)
#  G <- STM(param,midpts)
#  Nt <- numeric(105)
#  Nt[trunc(Lm50/2)] <- 1000
#  Nt1 <- G %*% (G %*% Nt)
#  choice$eLML85<-(findmedL(Nt1))
#  Nt1df<-as.data.frame(Nt1)
#  Nt1df$Length<-midpts
#  pick<-which(Nt1df$V1 > 0)
#  Nt1df <- Nt1df[pick,]
#  pick<-which(Nt1df$Length <= LML)
#  L.LML <- Nt1df[pick,]
#  choice$PPLM85<-sum(L.LML$V1/10)/2
#  
#  pick<-choice[,c(2,50:51)]  
#  if (exists("eLMLResults85"))
#   eLMLResults85 <- rbind(eLMLResults85, pick)
#  else
#   eLMLResults85 <- pick
# }
# 
# GwthResults<-join(GwthResults, eLMLResults85, by='SiteCode')

#Calculate SigMax for each site
SAMILResults$SigMaxLD90<-SAMILResults$MaxDL/(1+exp((log(19)*(SAMILResults$LD90-SAMILResults$L50)/(SAMILResults$L95-SAMILResults$L50))))

#####
#     L90%
#####
if (exists("eLMLResults")) 
 rm(eLMLResults90)

for(i in Sites){
 choice<-subset(SAMILResults, SiteCode == i)
 param <- c(choice$MaxDL,choice$L50,choice$L95,choice$SigMaxLD90) # MaxDL, L50, L95, SigMax
 Lm50 <- choice$LD90 # estimated size at 75% maturity
 LML <- choice$LML
 #eLML from L50
 midpts <- seq(2,210,2)
 G <- STM(param,midpts)
 Nt <- numeric(105)
 Nt[trunc(Lm50/2)] <- 1000
 Nt1 <- G %*% (G %*% Nt)
 choice$eLML90<-(findmedL(Nt1))
 Nt1df<-as.data.frame(Nt1)
 Nt1df$Length<-midpts
 pick<-which(Nt1df$V1 > 0)
 Nt1df <- Nt1df[pick,]
 pick<-which(Nt1df$Length <= LML)
 L.LML <- Nt1df[pick,]
 choice$PPLM90<-sum(L.LML$V1/10)/2
 
 pick<-choice[,c(1,55:56)]  
 if (exists("eLMLResults90"))
  eLMLResults90 <- rbind(eLMLResults90, pick)
 else
  eLMLResults90 <- pick
}

GwthResults<-join(GwthResults, eLMLResults90, by='SiteCode')
