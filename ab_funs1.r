

  # Model definition needing 5 parameters MaxDL, p50, p95, MaxS, s95
#  neglog5 <- function(param)  {
#        expDL <- invlog(param[1],param[2],param[3],absite$Lt)
#        expSD <- invlog(param[4],param[3],param[5],absite$Lt)
#        neglogl <- -sum(dnorm(absite$DL1,expDL,expSD,log=T))
#  }

  # Model definition needing 5 parameters MaxDL, p50, p95, MaxS, s95=210
  neglog4 <- function(param)  {
        expDL <- invlog(param[1],param[2],param[3],absite$Lt)
        expSD <- invlog(param[4],param[3],210,absite$Lt)
       # DL1 <- absite$DL/absite$Dt
        neglogl <- -sum(dnorm(absite$DL1,expDL,expSD,log=T))
        return(neglogl)
  }
             

getsitedata <- function(siten, indat) {
 # Subset the data to select only those data records relating to the SiteNo
  abpick <- subset(indat,indat$SiteId %in% siten)
  abpick$DL1 <- abpick$DL/abpick$Dt
  pick <- which((is.na(abpick$Lt)) | (abpick$Lt <= 0))
  if (length(pick) > 0) abpick <- abpick[-pick,]
  pick <- which((is.na(abpick$DL1)) | (abpick$DL1 <= -3) | (abpick$DL1 > 100))
  if (length(pick) > 0) abpick <- abpick[-pick,]  
  return(abpick)
}


#initpars<-as.vector(c(17.33576,93.89494,126.2470,4.794411))
initpars <- function(abdata) {
   pars <- numeric(4)  
   minL <- min(abdata$Lt,na.rm=T)
   maxL <- max(abdata$Lt,na.rm=T)
   extent <- maxL - minL
   lim <- minL+0.2*extent
   pick <- which(abdata$Lt < lim)
   if (length(pick) > 1) {
     pars[1] <- mean(abdata$DL1[pick])
     pars[4] <- sd(abdata$DL1[pick])
     } else { 
     pars[1] <- 22.0
     pars[4] <- 4.5
   }
   pars[2] <- minL + 0.5*extent
   pars[3] <- minL + 0.9*extent
   return(pars)
}


compare <- function(modela, modelb, modelc) {
  comparison <- paste(modela[[2]]," vs ",modelb[[2]],sep="") 
  minsep <- modela[[1]]$minimum+modelb[[1]]$minimum
  mincom <- modelc[[1]]$minimum  
  # Likelihood ratio
  diff2 <- 2.0*abs(mincom-minsep)   
#  print(comparison)
#  print(diff2) 
  # Is this significant - chisquared probability 
  probab <- pchisq(diff2,4,lower.tail=F)
#  print(probab)
  return(probab)
}


plotfirst <- function(model1) {
  MaxDL <- model1$estimate[1]
  p50 <- model1$estimate[2]
  p95 <- model1$estimate[3]
  MaxSig <- model1$estimate[4]
  label <- paste(absite$SIT_Id[1],absite$SIT_StatBlock[1],absite$SIT_Name[1],sep=" ")
  label2 <- paste(trunc(MaxDL*10000)/10000,trunc(p50*10000)/10000,
                  trunc(p95*10000)/10000,trunc(MaxSig*10000)/10000,sep="  ")  
  predDL <- invlog(MaxDL,p50,p95,predx)
  expDL <-  invlog(MaxDL,p50,p95,absite$Lt)
  resids <- abs(absite$DL1 - expDL)
  expSD <- invlog(MaxSig,p95,210,absite$Lt)
  outliers <- resids - 3.0*expSD
  pick <- which(outliers > 0)
  fsize <- 1.25
# Now plot the firs data set and the fitted curve
 # par(windows(width=10.0,height=6.0))
  par(mfrow = c(1,1))
  par(mai=c(0.5,0.5,0.3,0.1))
  par(cex=0.8, mgp=c(1.35,0.35,0), font.axis=7)
  plot(absite$Lt, absite$DL1, type="p", xlim=c(minx,maxx),xaxs="r",yaxs="r",
      ylim=c(miny,maxy), xlab="", ylab="",
      pch=1, cex=1.2, col=1)
      title(ylab=list("Annual Growth Increment", cex=fsize, col=1, font=7),
            xlab=list("Initial Length mm", cex=fsize, col=1, font=7),
            main=list(label, cex=fsize, col=1, font=7))     
      abline(0,0,col="grey", lwd=1)
  points(predx, predDL, type="l", col=2, lwd=2)
  if (length(pick)>0) {  #absite <- absite[-pick,]
      points(absite$Lt[pick],absite$DL1[pick],type="p",pch=16,col=4)
  }
  x <- minx + (maxx-minx)/2
  text(x,0.95*maxy,label2,cex=1.25,font=7)
}

plotsecond <- function(model1) {
  points(absite$Lt, absite$DL1, type="p", pch=3, col=4)
  MaxDL <- model1$estimate[1]
  p50 <- model1$estimate[2]
  p95 <- model1$estimate[3]
  predDL <- invlog(MaxDL,p50,p95,predx)
  points(predx, predDL, type="l", col=4, lwd=2)
}

plotthird <- function(model3) {
  MaxDL <- model3$estimate[1]
  p50 <- model3$estimate[2]
  p95 <- model3$estimate[3]
  predDL <- invlog(MaxDL,p50,p95,predx)
 # points(predx, predDL, type="l", col=6, lwd=2)
   points(predx, predDL, type="l", col=6, lwd=2)
}


getdyn <- function(DMax,p50,p95) { 
  LaA <- matrix(0,nrow=24,ncol=2,dimnames=list(seq(1,24,1)-1,c("DL","LatAge")))
  LaA[1,2] <- 2
  for (count in 1:23) {
    Lt <- LaA[count,2]
    DL <- invlog(DMax,p50,p95,Lt)
    LaA[count,1] <- DL
    Lt1 <- Lt + DL
    if (Lt1 <= p50) {
       SaM <- count + 1 # to account for age 0
    }
    LaA[count+1,2] <- LaA[count,2] + LaA[count,1]
  }
  down <- LaA[SaM,2]
  up <- LaA[SaM+1,2]
  dif <- up - down
  prop <- p50 - down
  AgeM <- SaM + prop/dif
  Len23 <- LaA[24,2] 
  ans <- list(Len23,AgeM,LaA)
  names(ans) <- c("Len23","AgeM","LaA")
  return(ans)
}
 
fitdata <- function(insite) {
  logneg4 <- function(param) {
        expDL <- invlog(param[1],param[2],param[3],insite$Lt)
        expSD <- invlog(param[4],param[3],210,insite$Lt)
        neglogl <- -sum(dnorm(insite$DL1,expDL,expSD,log=T))
        return(neglogl)
  }

  pars1 <- initpars(insite)
  len1 <- length(insite$DL1)
  MaxL <- max(insite$Lt+insite$DL1)
  best <- optim(pars1,logneg4,method="Nelder-Mead",
               hessian=FALSE,
               control=list(trace=0, maxit=2000))
  pars1 <- best$par
  # fit the model using the 4 parameter model
  model1 <- nlm(logneg4,pars1,hessian=T, gradtol = 1e-7) 
  # Remove the outliers and repeat the fitting  
  MaxDL <- model1$estimate[1]
  L50 <- model1$estimate[2]
  L95 <- model1$estimate[3]
  MaxSig <- model1$estimate[4]
  expDL <-  invlog(MaxDL,L50,L95,insite$Lt)
  resids <- abs(insite$DL1 - expDL)
  expSD <- invlog(MaxSig,L95,210,insite$Lt)
  outliers <- resids - 3.0*expSD
  pick <- which(outliers > 0)
  if ((length(pick) >0) ==TRUE) {  
      insite <- insite[-pick,]
  }
  best <- optim(pars1,logneg4,method="Nelder-Mead",
               hessian=FALSE,
               control=list(trace=0, maxit=2000))
  pars1 <- best$par
  # refit the model (minus outliers) using the 4 parameter model
  model2 <- nlm(logneg4,pars1,hessian=T, gradtol = 1e-7) 
  MaxDL <- model2$estimate[1]
  L50 <- model2$estimate[2]
  L95 <- model2$estimate[3]
  out <- getdyn(MaxDL,L50,L95)  
  grow <- array(0,dim=12,dimnames=c("MaxDL","L50","L95","MaxSig","Nobs","MaxObsL",
                                    "SiteId","Lat","Long","Block","Len23","AgeM"))

  grow[1] <- model2$estimate[1]
  grow[2] <- model2$estimate[2]
  grow[3] <- model2$estimate[3] 
  grow[4] <- model2$estimate[4]
  grow[5] <- len1
  grow[6] <- MaxL
  grow[7] <- insite$SIT_Id[1]
  grow[8] <- insite$SIT_Latitude[1]
  grow[9] <- insite$SIT_Longitude[1]
  grow[10] <- insite$SIT_StatBlock[1] 
  grow[11] <- out$Len23
  grow[12] <- out$AgeM
  ans <- list(grow,model2)
  names(ans) <- c("grow","model")
#  plotfirst(model2)
  return(ans)
}   
 
 
 fitaltIL <- function(x, y, SiteId=0,outliers=F,full=T) {
  negLIL <- function(parsin) {
                     expDL <- invlog(parsin[1],parsin[2],parsin[3],x)
                     expSD <- parsin[4]*expDL^parsin[5]
                     neglogl <- -sum(dnorm(y,expDL,expSD,log=T))
        return(neglogl)
  }  
  parsin <- initpar(x,y)  
  best <- optim(parsin,negLIL,method="Nelder-Mead",
               hessian=FALSE,
               control=list(trace=0, maxit=1000))
  parsin <- best$par
  mod <- nlm(negLIL,parsin,hessian=T, gradtol = 1e-7)
  parsin <- mod$estimate
  MaxDL <- mod$estimate[1]
  L50 <- mod$estimate[2]
  L95 <- mod$estimate[3]
  MaxSig <- mod$estimate[4]
  xout <- NULL  # will contain the list of outliers if one exists
  yout <- NULL
  if (outliers) {
     expDL <-  invlog(MaxDL,L50,L95,x)
     resids <- abs(y - expDL)
     expSD <- invlog(MaxSig,L95,210,x)
     outers <- resids - 2.576*expSD
     pick <- which(outers > 0)
     if ((length(pick) >0)==TRUE) {  
         xout <- x[pick]
         yout <- y[pick]
         x <- x[-pick]
         y <- y[-pick]
     }
     best <- optim(parsin,negLIL,method="Nelder-Mead",
               hessian=FALSE,
               control=list(trace=0, maxit=2000))
     parsin <- best$par
     mod <- nlm(negLIL,parsin,hessian=T, gradtol = 1e-7)
     MaxDL <- mod$estimate[1]
     L50 <- mod$estimate[2]
     L95 <- mod$estimate[3]
     MaxSig <- mod$estimate[4]
  }   
  Ltrg <- range(x,na.rm=T)
  xmin <- min(Ltrg[1],50)
  xmax <- max(Ltrg[2],180)
  predLt <- seq(xmin,xmax,1)
  predDL <- invlog(MaxDL,L50,L95,predLt)
  Nobs <- length(x)
  ans <- list(mod,MaxDL,L50,L95,MaxSig,predLt,predDL,Nobs,x,y,xout,yout,SiteId)
  names(ans) <- c("model","MaxDL","L50","L95","MaxSig","PredLt","PredDL","Nobs",
                  "Lt","DL","OutLt","OutDL","SiteId")
  class(ans) <- "IL"
  return(ans)       
}
