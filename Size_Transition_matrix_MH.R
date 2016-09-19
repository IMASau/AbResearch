#MH r functions for calculating SIZE TRANSITION MATRIX from p67 FRDC draft testing empirical HS


## R code to conduct eLML calculation using a size transition matrix
STM <- function(p,mids) {
  n <- length(mids)
  G <- matrix(0,nrow=n, ncol=n, dimnames=list(mids, mids))
  cw <- mids[2] - mids[1]
  SigL <- p[4]/((1+exp(log(19.0) * (mids - p[3])/(mids[n] - p[3]))))
  MeanL <- mids + (p[1]/((1 + exp(log(19.0)*(mids - p[2])/(p[3] - p[2])))))
  for (j in 1:n) {
    for (i in 1:n) {
      Prob <- (1-pnorm(mids[i]+cw/2.0,MeanL[j],SigL[j],FALSE))
      if (i < j) { G[i,j] <- 0.0 }
      if (i == j) { G[i,j] <- Prob }
      if (i > j) { G[i,j] <- Prob - (1-pnorm(mids[i-1]+cw/2.0,MeanL[j],SigL[j],FALSE)) }
    }
  }
  G[n,] <- G[n,]+ (1-colSums(G)) # plus group rather than distributing the excess across all
  return(G)
}
findmedL <- function(x) { # A function to find the median length from a vector of frequencies
  pick <- which(x > 0)
  Len <- midpts[pick] # midpts is a global variable and hence available in the function
  x <- x[pick]
  n <- length(x)
  y <- x
  for (i in 2:n) y[i] <- y[(i-1)] + x[i]
  midx <- max(y)/2
  upper <- which(y > midx)[1]
  propdiff <- (midx - y[(upper-1)])/(y[upper] - y[(upper-1)])
  eLML <- Len[(upper-1)] + propdiff * (Len[upper] - Len[(upper-1)])
  return(eLML)
}





# # empirical LML
# # Example R code to conduct such a calculation using inverse logistic growth
# invlog <- function(x,y,z,L) { # calculates the expected growth increment for length L
#   ans <- x/(1+exp(log(19)*(L-y)/(z-y)))
#   return(ans)
# }
# param <- c(MaxDL,L50,L95,SigMax) # MaxDL, L50, L95, SigMax
# Lm50 <- median(results[,"SaM"]) # estimated size at 50% maturity
# oneyear <- Lm50 + invlog(param[1], param[2], param[3], Lm50)
# eLML <- oneyear + invlog(param[1], param[2], param[3], oneyear)
# print(eLML) # deterministic empirical LML
