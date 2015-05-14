## Version 31/05/2104
## Fit logistic regression
doLogistic <- function(indat) {
    model <- glm(MatRatio ~ Length, family=binomial(link = "logit"), data = SizeMat, weights = Total)
    LM50 <-  dose.p(model, p = 0.5)[[1]]
    IQ <-    dose.p(model, p = 0.75)[[1]] -  dose.p(model, p = 0.25)[[1]]
    ans <- list(LM50,IQ,model)
    names(ans) <- c("LM50","IQ","Model")
    return(ans)
}


##http://stackoverflow.com/questions/28053542/confidence-intervals-for-lethal-dose-ld-for-logistic-regression-in-r
# LM50 <- dose.p(model, p=c(0.50, 0.90, 0.95))  # from MASS
# LM50.ci <- LM50 + attr(LM50, "SE") %*% matrix(qnorm(1 - 0.05/2)*c(-1,1), nrow=1)
# zp.est <- cbind(LM50, attr(LM50, "SE"), LM50.ci[,1], LM50.ci[,2])
# dimnames(zp.est)[[2]] <- c("LD", "SE", "LCL","UCL")
# zp.est 



## Version 31/05/2104
## Fit logistic regression with additional factors (Site & Season)
doLogisticOrth <- function(indat) {
 model <- glm(MatRatio ~ Length + Season * Region, family=binomial(link = "logit"), data = indat, weights = Total)
 glm.summary <-  summary(model)
 SigSeason <- glm.summary$coefficients[3,4]
 SigRegion <- glm.summary$coefficients[4,4]
 SigInteract <- glm.summary$coefficients[5,4]
 TotalN <- nrow(model$data)
   ans <- list(SigSeason,SigRegion,SigInteract,TotalN,glm.summary,model)
 names(ans) <- c("SigSeason","SigRegion","SigInteract","TotalN","ModelSummary","Model")
 return(ans)
}


## Plot the logistic curve
plotgraph <- function(indat,LM50,Site,scN,savefile=F) {
    if (savefile) {
        graphfile <- paste(resdir,Site,"_Maturity.tiff",sep="")
        if (file.exists(graphfile)) file.remove(graphfile)
        tiff(file=graphfile,width=150,height=110,units="mm",res=300,compression=c("lzw"))
    }
    par(mfrow = c(1,1))
    par(mai=c(0.4,0.4,0.1,0.05),oma=c(0,0,0.0,0.0))
    par(cex=0.75, mgp=c(1.35,0.35,0), font.axis=2,font=2)
    npts <- length(indat$Length)
    pick <- which(indat$Total > 1)
    plot(indat$Length[pick],indat$MatRatio[pick],type="p",pch=16,cex=1.0,xlim=c(50,160),xaxs="r",
         xlab="",ylab="")
    if (length(pick) < npts) points(indat$Length[-pick],indat$MatRatio[-pick],pch=16,cex=1.0,col=2)
    #abline(lm(SizeMat$MatRatio~SizeMat$Length),col='gray', lwd=2,lty=3)
    abline(h=0.5,v=LM50,col='blue',lty=2)
    xx <- seq(min(indat$Length), max(indat$Length), length=100)
    yy <- predict(model, data.frame(Length=xx), type='response', se=T)

    mydata <- cbind(xx,yy$fit)
    mydata <- as.data.frame(mydata)
    colnames(mydata) <- c("X", "YPredict")

    lines(xx,yy$fit, col='blue', lwd=4, lty=2)
    lines(xx, plogis(model$coef[1]+xx*model$coef[2]))
    lines(yy$fit+yy$se.fit ~ xx, col="red", type="l", lwd=2, lty=3)
    lines(yy$fit-yy$se.fit ~ xx, col="red", type="l", lwd=2, lty=3)
    text(60,1.0,paste("Small  ",scN[1],sep=""),cex=1.0,font=2)
    text(60,0.9,paste("Medium ",scN[2],sep=""),cex=1.0,font=2)
    text(60,0.8,paste("Large  ",scN[3],sep=""),cex=1.0,font=2)
    legend(145,0.1,c("N > 1","N = 1"),pch=c(16,16),col=c(1,2),cex=1.25,bty="n")
    title(ylab=list("Proportion Mature",cex=1.0,font=2),
          xlab=list(paste(Site,"   Length mm",sep=""),cex=1.0,font=2))

    if (savefile) {
        dev.off()
        graphics.off()
    }
} # end of plotgraph



## Merge the individual sites & seasons
allSites <- function(indat,savefile=F) {
#check for prexisting copy of SizeMatCompare & remove if it does
if (exists("SizeMatCompare")) 
 rm(SizeMatCompare)

Sites <- unique(indat$Site_Name); Sites
NS <- length(Sites)

RegionSeasonVec <- unique(samdata$RegionSeason); RegionSeasonVec
NSR <-  length(RegionSeasonVec)

for (pSite in 1:NSR) {
 Site <- RegionSeasonVec[pSite]
 pick <- which(indat$RegionSeason == Site)
 if (length(pick) > 0) {
  subdata <- indat[pick,]
 } else { "Oops something has gone wrong"
 }
 
  
 pRegion <- unique(subdata$Region)
 pSeason <- unique(as.character(subdata$Season))
 SizeMat <- table(subdata$Length, subdata$Mat)
 SizeMat <- as.data.frame(rbind(SizeMat))
 SizeMat$Length <- as.numeric(rownames(SizeMat))
 head(SizeMat)
 SizeMat$Total <- NA
 SizeMat$Total <- SizeMat$I + SizeMat$M
 SizeMat$MatRatio <- NA
 SizeMat$MatRatio <- SizeMat$M/SizeMat$Total
 SizeMat$Region <- as.factor(pRegion)
 SizeMat$Season <- as.factor(pSeason)
 SizeMat$Site_Name <- unique(subdata$Site_Name)
 SizeMat$RegionSeason <- unique(subdata$RegionSeason)
 
 if (exists("SizeMatCompare")) 
  SizeMatCompare <- rbind(SizeMatCompare, SizeMat)
 else
  SizeMatCompare <- SizeMat
 }
return(SizeMatCompare)
}


## Merge the individual sites & seasons
dataThinning <- function(indat2,SizeClass,ThinFactor, savefile=F) {

 #check for prexisting copy of SizeMatCompare & remove if it does
  if (exists("samThin")) 
  rm(samThin)

#   indat2 <- processed2
#   SizeClass <- "Medium"
#   ThinFactor <- 2
#   
  # Characterize Sites
  Sites <- unique(indat2$Site_Name); Sites
  NS <- length(Sites)
  
  pSite <- 2
 for (pSite in 1:NS) {
  Site <- Sites[pSite]
  pick <- which(indat2$Site_Name == Site)
  if (length(pick) > 0) {
   subdata <- indat2[pick,]
  } else { "Oops something has gone wrong"
  }
  
  pick <- which(subdata$SizeC == SizeClass)
  tmp <- subdata[-pick,]            #  data from the other two size classes
  classdat <- subdata[pick,]        # data from the selected size class
  categorysize <- length(pick)
  pickfrom <- seq(1,categorysize,1)   # this will be sampled to decide with rows to use
  samplesize <- trunc(categorysize/ThinFactor)  # This takes 50% of the original sample size
  adddata <- sort(sample(pickfrom,samplesize,replace=FALSE))
  subdata2 <- rbind(tmp,classdat[adddata,])    # combine the bits
  ## Create required fields for logistic regression
    
  if (exists("samThin")) 
   samThin <- rbind(samThin, subdata2)
  else
   samThin <- subdata2
 }
 return(samThin)
}


###---------------------------------------------------------------------------------##
## Thin an individual site for bootstrap calculations
dataThinningOneSite <- function(indat3,SizeClass,ThinFactor, savefile=F) {
 
 #check for prexisting copy of SizeMatCompare & remove if it does
 if (exists("samThinOneSite")) 
  rm(samThinOneSite)

  pick <- which(indat3$SizeC == SizeClass)
  tmp <- indat3[-pick,]            #  data from the other two size classes
  classdat <- indat3[pick,]        # data from the selected size class
  categorysize <- length(pick)
  pickfrom <- seq(1,categorysize,1)   # this will be sampled to decide with rows to use
  samplesize <- trunc(categorysize/ThinFactor)  # This takes 50% of the original sample size
  adddata <- sort(sample(pickfrom,samplesize,replace=FALSE))
  subdata2 <- rbind(tmp,classdat[adddata,])    # combine the bits
  ## Create required fields for logistic regression
 
    samThinOneSite <- subdata2
 
 return(samThinOneSite)
}


###---------------------------------------------------------------------------------##




