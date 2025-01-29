



library(biology)
library(hplot)
library(codeutils)


data(tasab)
table(tasab$site,tasab$sex)
sites <- sort(unique(tasab$site))
nsite <- length(sites)
scenes <- c("Male1","Female1","Male2","Female2")
models <- makelist(scenes)
sexes <- c("M","F")
count <- 0
for (i in 1:nsite) { #  i = 1
  for (s in 1:2) {
    count <- count + 1
    picksex <- which((tasab$sex %in% c(sexes[s], "I")) & (tasab$site == sites[i]))
    models[[count]] <- fitmaturity(tasab[picksex,],
                                   length="length",mature="mature",lower=50,upper=160)
  }
}
str1(models)
str1(models[["Male1"]])


plotprep(width=10, height=8)
parset(plots=c(2,2))
for (i in 1:length(models)) {
  plotmaturity(models[[i]],label=scenes[i],col=2,xmin=0,xmax=0,CI=FALSE,
                           setpar=FALSE,lwd=2)  
}




