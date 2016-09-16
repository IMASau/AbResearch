ILFuns <- function() {
  print("invlog(MaxDL,L50,L95,Lt)",quote=F)
  print("fitIL(Lt, DL, SiteId=0,outliers=F)",quote=F) 
  print("dobootIL(output from fitIL)",quote=F)
  print("summary(output from fitIL)",quote=F)
  print("plot(output from fitIL)",quote=F)
  print("plot(output from dobootIL)",quote=F)  
  print("print - or just type the name of the output from fitIL",quote=F)  
  print("initpar(Lt,DL)",quote=F)    
}


 ## The inverse logistic functions
#invlog <- function(x, ...) {
#  if(is.null(class(x))) class(x) <- data.class(x)
#  UseMethod("invlog",x)
#}

#3 Estimate starting parameters for fitting an IL curve
initpar <- function(Ltin,DLin) {
   pars <- numeric(4)
   minL <- min(Ltin,na.rm=T)
   maxL <- max(Ltin,na.rm=T)
   extent <- maxL - minL
   lim <- minL+0.2*extent
   pick <- which(Ltin < lim)
   if (length(pick) > 1) {
     pars[1] <- mean(DLin[pick])
     pars[4] <- sd(DLin[pick])
     } else {
     pars[1] <- 22.0
     pars[4] <- 4.5
   }
   pars[2] <- minL + 0.5*extent
   pars[3] <- minL + 0.9*extent
   return(pars)
}

## x = MaxDl, y = L50,  z= L95, and L = lengths
## Calculates the mean growht increment from the Inverse Logistic
invlog <- function(x,y,z,L) {
  ans <- x/(1+exp(log(19)*(L-y)/(z-y)))     
  return(ans)
}

#invlog.vector <- function(x, ...) invlog(x[1],x[2],x[3],...)
#invlog.matrix <- function(x, ...) invlog(x[1,1],x[1,2],x[1,3],...)

## Overloaded function used to fit an IL curve to x=Lt and y=DL data
fitIL <- function(x, ...) {
   if(is.null(class(x))) class(x) <- data.class(x)
   UseMethod("fitIL", x)
}

## Alternative fitIL Methods
fitIL.matrix <- function(x, ...) fitIL(x[,1],x[,2], ...)
fitIL.list <- function(x, ...) fitIL(x[[1]], x[[2]], ...)

## Fit the Inverse Logistic to x=Lt and y=DL data, generate an object of class IL
fitIL.default <- function(x, y, SiteId=0,outliers=F,sitename="") {
  negLIL <- function(parsin) {
        expDL <- invlog(parsin[1],parsin[2],parsin[3],x)
        expSD <- invlog(parsin[4],parsin[3],210,x)
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
  L50out <- NULL
  L95out <- NULL
  MaxDLout <- NULL
  MaxSigout <- NULL  
  if (outliers) {
     L50out <- L50
     L95out <- L95
     MaxDLout <- MaxDL
     MaxSigout <- MaxSig
     expDL <-  invlog(MaxDL,L50,L95,x)
     resids <- abs(y - expDL)
     expSD <- invlog(MaxSig,L95,210,x)
     outers <- resids - 2.576*expSD   #99% confidence limits  
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
  ans <- list(mod,MaxDL,L50,L95,MaxSig,predLt,predDL,Nobs,x,y,xout,yout,L50out,L95out,
              MaxDLout,MaxSigout,SiteId,sitename)
  names(ans) <- c("model","MaxDL","L50","L95","MaxSig","PredLt","PredDL","Nobs",
                  "Lt","DL","OutLt","OutDL","L50out","L95out","MaxDLout","MaxSigout",
                  "SiteId","SiteName")
  class(ans) <- "IL"
  return(ans)       
}

summary.IL <- function(x) {
    Ltrge <- range(x$Lt,na.rm=T)
    DLrge <- range(x$DL,na.rm=T)
    outs <- F
    if (length(x$MaxDLout) > 0) { outs <- T }
    cat("\n Site Id : ",x$SiteId) 
    cat("\n SiteName: ",x$SiteN)
    if (outs){ cat("\n MaxDL   : ",round(x$MaxDL,digits=4),"   ",round(x$MaxDLout,digits=4)) }
       else  { cat("\n MaxDL   : ",round(x$MaxDL,digits=4)) }
    if (outs){ cat("\n L50     : ",round(x$L50,digits=4),"  ",round(x$L50out,digits=4)) }
       else  { cat("\n L50     : ",round(x$L50,digits=4)) }
    if (outs){ cat("\n L95     : ",round(x$L95,digits=4)," ",round(x$L95out,digits=4)) }
       else  { cat("\n L95     : ",round(x$L95,digits=4)) }
    if (outs){ cat("\n MaxSig  : ",round(x$MaxSig,digits=4),"   ",round(x$MaxSigout,digits=4)) }
       else  { cat("\n MaxSig  : ",round(x$MaxSig,digits=4)) }
    cat("\n N       : ",x$Nobs)
    cat("\n Outliers: ",length(x$OutLt))
    cat("\n Range Lt: ",Ltrge) 
    cat("\n Range DL: ",DLrge)    
    cat("\n -ve LL  : ",x$model$minimum)  
    cat("\n Other Components")
    cat("\n $Lt and $DL are the input data minus any outliers")
    cat("\n $model  contains the nlm fit")
    cat("\n $PredLt and PredDL = fitted line")
    cat("\n $OutLt and $OutDL = outlier values")    
    cat("\n ")
   # return(
}

print.IL <- function(x) {
    cat("\n Site Id : ",x$SiteId)    
    cat("\n MaxDL   : ",x$MaxDL)
    cat("\n L50     : ",x$L50)
    cat("\n L95     : ",x$L95)
    cat("\n MaxSig  : ",x$MaxSig)
    cat("\n ")       
}

plot.IL <- function(x) {
   opar <- par(no.readonly=TRUE)
   expDL <-  invlog(x$MaxDL,x$L50,x$L95,x$Lt)
   resids <- x$DL - expDL
   expSD <- invlog(x$MaxSig,x$L95,210,x$PredLt)
   outer99 <- 2.5760 * expSD
   outer90 <- 1.965 * expSD
 par(mfrow = c(2,2))
 par(mai=c(0.2,0.4,0.1,0.1), oma=c(2,0,2,0))
 par(cex=0.8, mgp=c(1.35,0.35,0), font.axis=7)
 # Plot the basic fit with outliers if any
   ymax <- max(x$DL,x$OutDL)*1.025
   xmax <- max(max(x$Lt,x$OutLt)*1.025,180)
   xmin <- min(min(x$Lt,x$OutLt) - 1,50)
   plot(x$Lt,x$DL,type="p",pch=20,xlab="",ylab="",xaxs="r",yaxs="r",
        xlim<- c(xmin,xmax),ylim=c(-3,ymax))
   lines(x$PredLt,x$PredDL,col=2,lwd=2)
   lines(x$PredLt,x$PredDL+outer99,col=2,lty=2)
   lines(x$PredLt,x$PredDL-outer99,col=2,lty=2)
   lines(x$PredLt,x$PredDL+outer90,col=4,lty=2)
   lines(x$PredLt,x$PredDL-outer90,col=4,lty=2)
   
   abline(h=0,col="grey")
   abline(h=-3,col="grey")
   if (length(x$OutLt)>0) {
       points(x$OutLt,x$OutDL,col=2,pch=20)
   }   
   title(ylab=list("Growth Increment DL", cex=1.1, col=1, font=7))  
   text(170,0.95*ymax,round(x$MaxDL,3),cex=0.8,font=7)
   text(170,0.9*ymax,round(x$L50,3),cex=0.8,font=7)
   text(170,0.85*ymax,round(x$L95,3),cex=0.8,font=7)
   text(170,0.8*ymax,round(x$MaxSig,3),cex=0.8,font=7)   
 # Plot the residuals    
   plot(x$Lt,resids,type="p",pch=20,xlab="",ylab="",xaxs="r",yaxs="r",
        xlim=c(xmin,xmax)) 
   lines(x$PredLt,outer99,col=2,lty=2)
   lines(x$PredLt,-outer99,col=2,lty=2)
   lines(x$PredLt,outer90,col=4,lty=2)
   lines(x$PredLt,-outer90,col=4,lty=2)                   
   abline(h=0,col="grey")  
   title(ylab=list("Residuals mm", cex=1.1, col=1, font=7))
# Plot the rate of change in DL          
   N <- length(x$PredDL)   
   diffDL <- numeric(N-1)
   for (index in 2:N) {
     diffDL[index-1] <- x$PredDL[index] - x$PredDL[index-1]
   }
   plot(x$PredLt[1:(N-1)],diffDL,type="l",xlim=c(xmin,xmax),xlab="",ylab="")
   abline(v=x$L50,col=2)
   title(ylab=list("Rate of Change of DL", cex=1.1, col=1, font=7))

   bins <- seq(xmin,xmax,5)
   hist(x$Lt,breaks=bins,xlab="",ylab="",main="")
   abline(v=x$L50,col="grey")
   title(ylab=list("Density of Data Points", cex=1.1, col=1, font=7))   
      
   mtext("Initial Length Lt",side=1,line=0.5,outer=T,font=7,cex=1.25)         
   mtext(paste("",x$SiteN , sep=""),side=3,line=0.5,outer=T,font=7,cex=1.25)    
   par(opar)    
}

dobootIL <- function(x,reps=100) {
   columns <- c("MaxDL","MDLL95%","MDL50%","MDLU95%","L50","L50L95%","L5050%","L50U95%",
                "L95","L95L95%","L9550%","L95U95%","MaxSig","MSL95%","MS50%","MSU95%","Obs","Errors")
   bootans <- matrix(0,nrow=1,ncol=18,dimnames=list(x$SiteId,columns))
   mainpar <- c(1,5,9,13)
   outboot <- matrix(0,nrow=reps,ncol=4,dimnames=list(seq(1,reps,1),c("MaxDL","L50","L95","MaxSig")))
   bootans[mainpar] <- x$model$estimate
   xLt <- x$Lt 
   yDL <- x$DL
   nb <- x$Nobs
   bootans[17] <- nb  
   pick <- seq(1,nb,1)
   bootans[18] <- 0
   doboot <- function(inpick,inLt,inDL) {
      boots <- sample(pick,replace=T)
      boots <- boots[order(boots)]
      model <- fitIL(xLt[boots],yDL[boots])
      return(model$model$estimate)
   }  
   res <- lapply(1:reps, function(i) try(doboot(pick,xLt,yDL), TRUE))
   for (bootnum in 1:reps) {
     if (is.numeric(res[[bootnum]])) {
         outboot[bootnum,] <- res[[bootnum]]
      } else {
         bootans[18] <- bootans[18] + 1
         outboot[bootnum,] <- c(NA,NA,NA,NA)
     }
   }                       
   initcol <- -2
   for (index in 1:4) {
      CIs <- quantile(outboot[,index],probs=c(0.025,0.5,0.975),na.rm=T)
      initcol <- initcol + 4
      bootans[initcol:(initcol+2)] <- CIs
   }
   ans <- list(bootans,outboot)
   names(ans) <- c("Percentiles","Replicates")
   class(ans) <- "bootIL"   
   return(ans)
}
   
plot.bootIL <- function(x) {
   opar <- par(no.readonly=TRUE)
   collab = c("MaxDL","L50","L95","MaxSig")
   initcol <- -2
   sitenum <- rownames(x$Percentiles)
   par(mfrow = c(2,2))
   par(mai=c(0.5,0.5,0.1,0.1), oma=c(0,0,2,0))
   par(cex=0.8, mgp=c(1.35,0.35,0), font.axis=7)
   for (index in 1:4) {
      initcol <- initcol + 4
      CIs <- x$Percentiles[initcol:(initcol+2)]
      hist(x$Replicates[,index],xlab="",main="")
      title(xlab=list(collab[index], cex=1.0, col=1, font=7))   
      abline(v=CIs[1],col=2,lty=2)
      abline(v=CIs[2],col=4,lty=2)
      abline(v=CIs[3],col=2,lty=2)     
   }
   mtext(paste("Site ",sitenum,sep=""),side=3,line=0.5,outer=T,font=7,cex=1.25)
   par(opar) 
}

getdyn <- function(x) { 
  LaA <- matrix(0,nrow=24,ncol=2,dimnames=list(seq(1,24,1)-1,
                c("DLarAge","LatAge")))
  LaA[1,2] <- 2
  for (count in 1:23) {
    Lt <- LaA[count,2]
    DL <- invlog(x$MaxDL,x$L50,x$L95,Lt)
    LaA[count,1] <- DL
    Lt1 <- Lt + DL
    if (Lt1 <= x$L50) {
       SaM <- count + 1 # to account for age 0
    }
    LaA[count+1,2] <- LaA[count,2] + LaA[count,1]
  }
  down <- LaA[SaM,2]
  up <- LaA[SaM+1,2]
  dif <- up - down
  prop <- x$L50 - down
  AgeM <- SaM + prop/dif
  Len23 <- LaA[24,2] 
# InterQuartile Difference
   LIQ <- 0.75*x$MaxDL
   UIQ <- 0.25*x$MaxDL
   pick <- which(x$PredDL < LIQ)
   low <- x$PredLt[pick[1]-1]
   pick <- which(x$PredDL < UIQ)
   high <- x$PredLt[pick[1]-1]
   iqg <- (LIQ-UIQ)/(high - low)  
  ans <- list(Len23,AgeM,LaA,iqg)
  names(ans) <- c("Len23","AgeM","LaA","InterQG")
  return(ans)
}

## return the IL coefficients from an IL object
coef.IL <- function(x) {
   ans <- x$model$estimate
   return(ans)
}      

## calculate the mean
estGR <- function(x,tMaxDL,tL50,tL95) {
   pick <- which(x$Lt < x$L50)
   xLt <- x$Lt[pick]
   yDL <- x$DL[pick]
   PredDL <- invlog(tMaxDL,tL50,tL95,xLt)
   GR <- mean(yDL-PredDL)
   return(GR)
}


## x <- model
plotsingleIL <- function(x,removeoutlier=T) {
   expDL <-  invlog(x$MaxDL,x$L50,x$L95,x$Lt)
   expSD <- invlog(x$MaxSig,x$L95,210,x$PredLt)
   outer99 <- 2.5760 * expSD
   outer90 <- 1.965 * expSD
   par(mfrow = c(1,1))
   par(mai=c(0.4,0.4,0.2,0.1), oma=c(0,0,0,0))
   par(cex=0.8, mgp=c(1.35,0.35,0), font.axis=7)
 # Plot the basic fit with outliers if any
   ymax <- max(x$DL,x$OutDL)*1.025
   xmax <- max(max(x$Lt,x$OutLt)*1.025,180)
   xmin <- min(min(x$Lt,x$OutLt) - 1,50)
   plot(x$Lt,x$DL,type="p",pch=20,xlab="",ylab="",xaxs="r",yaxs="r",
        xlim<- c(xmin,xmax),ylim=c(-3,ymax))
   lines(x$PredLt,x$PredDL,col=2,lwd=2)
 #  lines(x$PredLt,x$PredDL+outer99,col=2,lty=2)
 #  lines(x$PredLt,x$PredDL-outer99,col=2,lty=2)
 #  lines(x$PredLt,x$PredDL+outer90,col=4,lty=2)
 #  lines(x$PredLt,x$PredDL-outer90,col=4,lty=2)
   abline(h=0,col="grey")
   abline(h=-3,col="grey")
   if ((length(x$OutLt)>0) & (removeoutlier == F)) {
       points(x$OutLt,x$OutDL,col=2,pch=20)
   }
   title(main= x$SiteN,
         ylab=list("Growth Increment DL", cex=1.0, col=1, font=7),
         xlab=list("Shell Length (mm)", cex=1.0, col=1, font=7))
}

 