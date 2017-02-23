## Plot the logistic curve
plotgraph <- function(indat,LM50,Site,scN){ #},savefile=F) {
 # if (savefile) {
 #  graphfile <- paste(resdir,Site,"_Maturity.tiff",sep="")
 #  if (file.exists(graphfile)) file.remove(graphfile)
 #  tiff(file=graphfile,width=150,height=110,units="mm",res=300,compression=c("lzw"))
 # }
 par(mfrow = c(1,1))
 par(mai=c(0.4,0.4,0.1,0.05),oma=c(0,0,0.0,0.0))
 par(cex=0.75, mgp=c(1.35,0.35,0), font.axis=2,font=2)
 npts <- length(indat$ShellLength)
 pick <- which(indat$Total > 1)
 plot(indat$ShellLength[pick],indat$MatRatio[pick],type="p",pch=16,cex=1.0,xlim=c(50,160),xaxs="r",
      xlab="",ylab="")
 if (length(pick) < npts) points(indat$ShellLength[-pick],indat$MatRatio[-pick],pch=16,cex=1.0,col=2)
 #abline(lm(SizeMat$MatRatio~SizeMat$Length),col='gray', lwd=2,lty=3)
 abline(h=0.5,v=LM50,col='blue',lty=2)
 xx <- seq(min(indat$ShellLength), max(indat$ShellLength), length=100)
 yy <- predict(r, data.frame(ShellLength=xx), type='response', se=T)
 
 mydata <- cbind(xx,yy$fit)
 mydata <- as.data.frame(mydata)
 colnames(mydata) <- c("X", "YPredict")
 
 lines(xx,yy$fit, col='blue', lwd=4, lty=2)
 lines(xx, plogis(r$coef[1]+xx*r$coef[2]))
 lines(yy$fit+yy$se.fit ~ xx, col="red", type="l", lwd=2, lty=3)
 lines(yy$fit-yy$se.fit ~ xx, col="red", type="l", lwd=2, lty=3)
 text(60,1.0,paste("n < LM05 =",SamList$N.underLD05[i],sep=" "),cex=1.0,font=2)
 text(60,0.9,paste("n with IQR =",SamList$N.IQR[i],sep=" "),cex=1.0,font=2)
 text(60,0.8,paste("n > LM95  =",SamList$N.overLD95[i],sep=" "),cex=1.0,font=2)
 legend(145,0.1,c("N > 1","N = 1"),pch=c(16,16),col=c(1,2),cex=1.25,bty="n")
 title(ylab=list("Proportion Mature",cex=1.0,font=2),
       xlab=list(paste(Site,"Length mm",sep=" "),cex=1.0,font=2))
 
 # if (savefile) {
 #  dev.off()
 #  graphics.off()
 # }
} # end of plotgraph

