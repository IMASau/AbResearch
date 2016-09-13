


####################     Theoretical LML  #######################
#L50T
SamResults$L50T<-1.1539*SamResults$LM50-15.335
#L95T
SamResults$L95T<-1.0862*SamResults$LM50+32.461
#MaxDeltaLt
SamResults$MaxDLt<-0.46095*SamResults$L95T-0.46856*SamResults$L50T+5.58943
#DL
SamResults$DL<-SamResults$MaxDLt/1+exp((log(19))*((SamResults$LM50-SamResults$L50T)/(SamResults$L95T-SamResults$L50T)))
#LM50 plus 1 year
SamResults$Yr1<-SamResults$LM50+SamResults$DL
#DL at initial length of LM50 + 1 year
SamResults$DL2<-SamResults$MaxDLt/1+exp((log(19))*((SamResults$Yr1-SamResults$L50T)/(SamResults$L95T-SamResults$L50T)))
#Theoretical LML
SamResults$MLSt<-SamResults$Yr1+SamResults$DL2
#Corrected Theo LML
SamResults$MLStc<-(SamResults$MLSt-16.8868)/0.8946