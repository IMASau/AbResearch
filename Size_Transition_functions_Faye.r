# Depends on Growth Functions.R in D:\GitCode\AbResearch
STM_IL<-function(p,cw,n,midpts) {
G <- matrix(0,nrow=n,ncol=n,dimnames=list(midpts,midpts))
deltaL <- IL(p,midpts)
SigL <- p[4]
MeanL <- midpts + deltaL

for (j in 1:n) {
  for (i in 1:n) {
     Prob <- (1-pnorm(midpts[i]+cw,MeanL[j],SigL,FALSE))
     if (i < j)  { G[i,j] <- 0.0 }
     if (i == j) { G[i,j] <- Prob }
     if (i > j)  { G[i,j] <- Prob - (1-pnorm(midpts[i-1]+cw,MeanL[j],SigL,FALSE)) }
    # if (i == 31) { G[i,j] <- (1-pnorm(250,MeanL[j],SigL[j],FALSE))-(1-pnorm(midpts[i-1]+5,MeanL[j],SigL[j],FALSE)) }
  }
}
G[n,] <- G[n,]+ (1-colSums(G))
return(G)
}

STM_Fb<-function(p,cw,n,midpts) {
G <- matrix(0,nrow=n,ncol=n,dimnames=list(midpts,midpts))
deltaL <- Fb(p,midpts,Dt)
SigL <- p[3]
MeanL <- midpts + deltaL

for (j in 1:n) {
  for (i in 1:n) {
     Prob <- (1-pnorm(midpts[i]+cw,MeanL[j],SigL,FALSE))
     if (i < j)  { G[i,j] <- 0.0 }
     if (i == j) { G[i,j] <- Prob }
     if (i > j)  { G[i,j] <- Prob - (1-pnorm(midpts[i-1]+cw,MeanL[j],SigL,FALSE)) }
    # if (i == 31) { G[i,j] <- (1-pnorm(250,MeanL[j],SigL[j],FALSE))-(1-pnorm(midpts[i-1]+5,MeanL[j],SigL[j],FALSE)) }
  }
}
G[n,] <- G[n,]+ (1-colSums(G))
return(G)
}

STM_Gz<-function(p,cw,n,midpts) {
G <- matrix(0,nrow=n,ncol=n,dimnames=list(midpts,midpts))
deltaL <- Gz(p,midpts,Dt)
SigL <- p[3]
MeanL <- midpts + deltaL
for (j in 1:n) {
  for (i in 1:n) {
     Prob <- (1-pnorm(midpts[i]+cw,MeanL[j],SigL,FALSE))
     if (i < j)  { G[i,j] <- 0.0 }
     if (i == j) { G[i,j] <- Prob }
     if (i > j)  { G[i,j] <- Prob - (1-pnorm(midpts[i-1]+cw,MeanL[j],SigL,FALSE)) }
    # if (i == 31) { G[i,j] <- (1-pnorm(250,MeanL[j],SigL[j],FALSE))-(1-pnorm(midpts[i-1]+5,MeanL[j],SigL[j],FALSE)) }
  }
}
G[n,] <- G[n,]+ (1-colSums(G))
return(G)
}
