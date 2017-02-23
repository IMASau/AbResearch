Fb<-function(p,Length,Dt){
  DL_pred<-(p[1]-Length)*(1-exp(-p[2]*Dt))
  return(DL_pred)
}

Gz<-function(p,Length,Dt){
  DL_pred<-p[1]*(Length/p[1])^(exp(-p[2]*Dt))-Length
  return(DL_pred)
}

IL<-function(p,Length){
  DL_pred<-p[1]/(1+exp(log(19)*((Length-p[2])/(p[3]-p[2]))))
  return(DL_pred)
}

S<-function(p,Length,Dt){
  DL_pred<- -Length+((Length^p[1])*exp(-p[2]*Dt)+p[3]*(1-exp(-p[2]*Dt)))^(1/p[1])
  return(DL_pred)
}

negLL <- function(p1,p2,p3,log=T){
  negLL<- -sum(dnorm(p1,p2,p3,log=T))
  return(negLL)
}

negLL1<-function(p){
  Fb_pred<-(p[1]-Lt)*(1-exp(-p[2]*Dt))
  negLL<-(-sum(dnorm(DL1,Fb_pred,p[3],log=T)))
  return(negLL)
}

negLL2<-function(p){
  Gz_pred<-Gz<-p[1]*(Lt/p[1])^(exp(-p[2]*1))-Lt
  negLL<-(-sum(dnorm(DL1,Gz_pred,p[3],log=T)))
  return(negLL)
}

negLL3<-function(p){
  IL_pred<-p[1]/(1+exp(log(19)*((Lt-p[2])/(p[3]-p[2]))))
  negLL<-(-sum(dnorm(DL1,IL_pred,p[4],log=T)))
  return(negLL)
}

negLL4<-function(p){
  S_pred<- -Lt+((Lt^p[1])*exp(-p[2]*Dt)+p[3]*(1-exp(-p[2]*Dt)))^(1/p[1])
  negLL<-(-sum(dnorm(DL1,S_pred,p[4],log=T)))
  return(negLL)
}

# Fabens version of VB (Linf,k,s)
Fb_param<-c(171.7560278,0.260818809,3.691558335)   #std dev is the same for all models
pscaleFb<-c(100,0.01,1)
Fb_ass <- Fb_param

#Gompertz #(Linf,g,s)
Gz_param<-c(164.53959,0.387031315,3.691558335)  #std dev is the same for all models
pscaleGz<-c(100,0.01,1)
Gz_ass <- Gz_param

#Inv logistic(MaxDL,L50,L95,s)
#IL_param<-c(19.88090044,131.5299934,166.4191692,3.691558335) #std dev is the same for all models
IL_param<-c(10.880900,80.529993,95,3.691558)  #use this one for chapter 4 
#IL_param<-c(10.880900,80.529993,110.419169,3.691558)
pscaleIL<-c(10,100,100,1)
IL_ass<-IL_param

#Schnute(b,a,c,stddev)
S_param<-c(0.002256778,0.386739224,1.011583414,3.691558335) #std dev is the same for all models
pscaleS<-c(0.0001,0.001,0.0001,1)
S_ass<-S_param

