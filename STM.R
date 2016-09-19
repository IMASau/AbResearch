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
SAMILResults$SigMax<-SAMILResults$MaxDL/(1+exp((log(19)*(SAMILResults$LD50-SAMILResults$L50)/(SAMILResults$L95-SAMILResults$L50))))

Sites<-unique(SAMILResults$SiteCode)

#####
#     L50%
#####
if (exists("eLMLResults")) 
  rm(eLMLResults)

for(i in Sites){
  choice<-subset(SAMILResults, SiteCode == i)
  param <- c(choice$MaxDL,choice$L50,choice$L95,choice$SigMax) # MaxDL, L50, L95, SigMax
  Lm50 <- choice$LD50 # estimated size at 50% maturity
  LML <- choice$LML
  #eLML from L50
  midpts <- seq(2,210,2)
  G <- STM(param,midpts)
  Nt <- numeric(105)
  Nt[trunc(Lm50/2)] <- 1000
  Nt1 <- G %*% (G %*% Nt)
  choice$eLML<-(findmedL(Nt1))
  Nt1df<-as.data.frame(Nt1)
  Nt1df<-add_rownames(Nt1df, "Length")
  pick<-which(Nt1df$V1 > 0)
  Nt1df <- Nt1df[pick,]
  pick<-which(Nt1df$Length >= LML)
  U.LML <- Nt1df[pick,]
  choice$PctU.LML<-sum(U.LML$V1/10)/2
  
  pick<-choice[,c(1,41:42)]  
  if (exists("eLMLResults"))
    eLMLResults <- rbind(eLMLResults, pick)
  else
    eLMLResults <- pick
}
#pass back to GwthResults
GwthResults<-join(SAMILResults, eLMLResults, by='SiteCode')

