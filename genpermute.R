library(dplyr)
library(tidyr)
library(openxlsx)
library(ggplot2)
library(hierfstat)
library(poppr)

library(doSNOW) 
library(snowfall)
#library(foreach)
library(doParallel)
library(R.utils)
library(gdata)

setwd("c:/CloudStor/Fisheries Research/Abalone/GenPermute")

## Get data in ####
# greens <- read.xlsx(
#  "c:/CloudStor/Fisheries Research/Abalone/GenPermute/greenlip hierfst/3levels.xlsx",
#  sheet = "Tip & HS as one location +vic"
# )
# ## Convert all loci from chr to num
# data <-
#  greens %>% mutate_all(funs(type.convert(as.character(.)))) %>% as.data.frame()

# # read greenlip data file into R for use in heirfstat
 data <- read.table("c:/CloudStor/Fisheries Research/Abalone/GenPermute/greenlip hierfst/3levelfinal.txt",header=TRUE)
## convert all NAs to 0
data <- NAToUnknown(x = data, unknown = 0)


# read blacklip data file into R for use in heirfstat
data.bl <- read.table("c:/CloudStor/Fisheries Research/Abalone/GenPermute/blacklip for hierfst.txt",header=TRUE)

data <- data.bl
names(data)

group_by(data, Lev1, Lev2, Lev3) %>%
 summarise(n = n()) %>%  
 as.data.frame()

## -----------------------------------------------------##
## -----------------------------------------------------##
## Permute reduced sample size by population ####

## start time
ptm <- proc.time()
strt<-Sys.time()
Sys.time()

if (exists("permuted")) 
rm(permuted)

sfInit(parallel=TRUE, cpus=3)

#permutes <- 999
resampN <- 15

sfExport("data","resampN", local=FALSE)
sfLibrary(hierfstat)
sfLibrary(tidyr)
sfLibrary(dplyr)
pb <- txtProgressBar(max=permutes, style=3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress=progress)

## Start parallel processing loop
#permuted <- foreach(i = 1:permutes, .packages=c("hierfstat","tidyr","dplyr"), .combine=rbind, .options.snow=opts) %dopar% {
 
# if (exists("permute.dat")) 
#  rm(permute.dat)

#for (i in 1:permutes) {

wrapper <- function(a) { # reduced N wrapper function
# print(i)
reducedN <- data %>%
 group_by(Lev1, Lev2, Lev3) %>%
 sample_n(size = resampN, replace = FALSE) %>%
 as.data.frame()

#reducedN <- data

#define and name data frame that contains only loci information
## blacklip
loci<- select(reducedN, X6.CO4, X2.14, X9.H11, X1.24, X1.14, X2.B01)

## greenlip
#loci<- select(reducedN, Loc214,Loc13C12,Loc13F06,Loc16G08,Loc17D11,Loc2B01,Loc2G01,Loc7B11,Loc8D02,Loc9G01,Loc9H03,cmrHr1.14,cmrHr2.30,Hrub1.H08,Hrub3.F03)

#create data frame with heirarchical levels
levels <- select(reducedN, Lev1, Lev2, Lev3)

#estimate heirarchical f-statistics
hfst <- varcomp.glob(levels,loci)
#estimate global Fst
hfst2 <- varcomp.glob(levels$Lev3,loci)


# test for statistical significance at highest level in heirarchy (between regions)
p.regions <- test.between(loci,rand.unit=levels$Lev2,test=levels$Lev1,nperm=1000)

#test for statistical significance at second level of heirarchy (among locations)
p.locInreg <- test.between.within(loci,within=levels$Lev1,rand.unit=levels$Lev3,test=levels$Lev2,nperm=1000)

#test for statistical significance at lowest level in heirarchy (among replicate sites)
p.sitInloc <- test.within(loci,test=levels$Lev3,within=levels$Lev2,nperm=1000)

#test for statistical significance at at pairwise across populations
p.pairpop <- test.g(data = loci, levels$Lev3, nperm = 1000)

# out1 <- data.frame(hfst$F[1,1],hfst$F[2,2],hfst$F[3,3])
# colnames(out1) <- c("Frt","Flr","Fsl")
# out2 <- data.frame(as.numeric(p.regions[2]), as.numeric(p.locInreg[2]), as.numeric(p.sitInloc[2]))
# colnames(out2) <- c("PFrt","PFlr","PFsl")

out1 <-
 data.frame(
  hfst$F[1, 1],
  as.numeric(p.regions[2]),
  hfst$F[2, 2],
  as.numeric(p.locInreg[2]),
  hfst$F[3, 3],
  as.numeric(p.sitInloc[2]),
  hfst2$F[1,1],
  as.numeric(p.pairpop[2])
 )
colnames(out1) <- c("Frt", "PFrt", "Flr", "PFlr", "Fsl", "PFsl","PopFst","PPopFst")

#Basic stats
### blacklip
dat <- select(reducedN, Lev3,X6.CO4,X2.14,X9.H11, X1.24, X1.14, X2.B01)
## greenlip
#dat <- select(reducedN, Loc214,Loc13C12,Loc13F06,Loc16G08,Loc17D11,Loc2B01,Loc2G01,Loc7B11,Loc8D02,Loc9G01,Loc9H03,cmrHr1.14,cmrHr2.30,Hrub1.H08,Hrub3.F03)

out2 <- as.data.frame(basic.stats(dat, diploid=TRUE)$overall)

bind.dat <- cbind(out1, t(out2))

bind.dat$run <- a
bind.dat$n <- resampN

# if (exists("permute.dat")) 
#  permute.dat <- rbind(permute.dat, bind.dat)
# else
#  permute.dat <- bind.dat

return(bind.dat)
}

result <- sfLapply(1:999, wrapper)
permuteBlN15 <- bind_rows(result )
sfStop()


cputime <-proc.time() - ptm
cputime
print(Sys.time()-strt)
Sys.time()

save.image(paste0("c:/CloudStor/R_Stuff/genetics/BlpermuteN",resampN))

## boot.ppfst(dat=dat,nboot=100,quant=c(0.025,0.975),diploid=TRUE,dig=4,...)


## End reduced N permutation
## -----------------------------------------------------##
## -----------------------------------------------------##
## Permute by region ####

## start time
ptm <- proc.time()
strt<-Sys.time()
Sys.time()

if (exists("permuted")) 
 rm(permuted)

sfInit(parallel=TRUE, cpus=3)

data <- data.bl

#permutes <- 999
resampN <- 10

sfExport("data", "resampN", local=FALSE)
sfLibrary(hierfstat)
sfLibrary(tidyr)
sfLibrary(dplyr)

## Progress bar
# pb <- txtProgressBar(max=999, style=3)
# progress <- function(n) setTxtProgressBar(pb, n)
# opts <- list(progress=progress)

## Start parallel processing loop
#permuted <- foreach(i = 1:permutes, .packages=c("hierfstat","tidyr","dplyr"), .combine=rbind, .options.snow=opts) %dopar% {


# if (exists("permute.dat")) 
#  rm(permute.dat)

#for (i in 1:permutes) {

wrapper.site <- function(a) {
 # print(i)
 #paste("Processing Component:", a) 
 
 byLoc <- data %>% group_by(Lev1,Lev2) %>%
  filter(Lev3 %in% sample(unique(Lev3), 1)) %>%
  as.data.frame()


## data check
#  group_by(byLoc, Lev3) %>%
#   summarise(n = n()) %>%  
#   as.data.frame()
 
 
## Uncomment and change value of resampN if permuting sample size as well. 
 reducedN <- data %>%
  group_by(Lev1, Lev2, Lev3) %>%
  sample_n(size = resampN, replace = FALSE) %>%
  as.data.frame()
 
 #define and name data frame that contains only loci information
 ## blacklip
loci <- select(byLoc, Lev3, X6.CO4, X2.14, X9.H11, X1.24, X1.14, X2.B01)
 ## greenlip
 #loci<- select(byLoc, Loc214,Loc13C12,Loc13F06,Loc16G08,Loc17D11,Loc2B01,Loc2G01,Loc7B11,Loc8D02,Loc9G01,Loc9H03,cmrHr1.14,cmrHr2.30,Hrub1.H08,Hrub3.F03)
 
 #create data frame with heirarchical levels
 levels <- select(byLoc, Lev1, Lev2)
 
 #estimate heirarchical f-statistics
 hfst <- varcomp.glob(levels,loci)
 
 #estimate global Fst
 hfst2 <- varcomp.glob(levels$Lev2,loci)
 
 
 # test for statistical significance at highest level in heirarchy (between regions)
 p.regions <- test.between(loci,rand.unit=levels$Lev2,test=levels$Lev1,nperm=1000)
 
 #test for statistical significance at second level of heirarchy (among locations)
 p.locInreg <- test.between.within(loci,within=levels$Lev1,rand.unit=levels$Lev3,test=levels$Lev2,nperm=1000)
 
 # #test for statistical significance at lowest level in heirarchy (among replicate sites)
 # p.sitInloc <- test.within(loci,test=levels$Lev3,within=levels$Lev2,nperm=1000)
 # 
 #test for statistical significance at at pairwise across populations
 p.pairpop <- test.g(data = loci, levels$Lev2, nperm = 1000)
 
 # out1 <- data.frame(hfst$F[1,1],hfst$F[2,2],hfst$F[3,3])
 # colnames(out1) <- c("Frt","Flr","Fsl")
 # out2 <- data.frame(as.numeric(p.regions[2]), as.numeric(p.locInreg[2]), as.numeric(p.sitInloc[2]))
 # colnames(out2) <- c("PFrt","PFlr","PFsl")
 
 out1 <-
  data.frame(
   hfst$F[1, 1],
   as.numeric(p.regions[2]),
   hfst$F[2, 2],
   as.numeric(p.locInreg[2]),
   # hfst$F[3, 3],
   # as.numeric(p.sitInloc[2]),
   hfst2$F[1,1],
   as.numeric(p.pairpop[2])
  )
 colnames(out1) <- c("Frt", "PFrt", "Flr", "PFlr","PopFst","PPopFst")
 
 #Basic stats
## blacklip
dat <- select(byLoc, Lev3,X6.CO4,X2.14,X9.H11, X1.24, X1.14, X2.B01)
## greenlip
#dat <- select(byLoc, Lev3,Loc214,Loc13C12,Loc13F06,Loc16G08,Loc17D11,Loc2B01,Loc2G01,Loc7B11,Loc8D02,Loc9G01,Loc9H03,cmrHr1.14,cmrHr2.30,Hrub1.H08,Hrub3.F03)
 
 out2 <- as.data.frame(basic.stats(dat, diploid=TRUE)$overall)
 
 bind.dat <- cbind(out1, t(out2))
 
 bind.dat$run <- a
 bind.dat$n <- resampN
 
 # if (exists("permute.dat")) 
 #  permute.dat <- rbind(permute.dat, bind.dat)
 # else
 #  permute.dat <- bind.dat
 
 return(bind.dat)
}


result.site <- sfLapply(1:999, wrapper.site)
permuteSiteBl10 <- bind_rows(result.site )
sfStop()


cputime <-proc.time() - ptm
cputime
print(Sys.time()-strt)
Sys.time()

#save.image(paste0("c:/CloudStor/R_Stuff/genetics/permuteSite",resampN))
save.image("c:/CloudStor/R_Stuff/genetics/permuteSiteBl10")


## End permute by region
## -----------------------------------------------------##
# require(dplyr)
# dat1 %>%
#  group_by(ID) %>%
#  do(sample_n(., 5, replace = TRUE))
#  
#  df %>% group_by(color) %>% sample_n(size = 3)
#  


# mtcars %>% group_by(am) %>% 
#  do(rs = modelr::bootstrap(., 10)) %>% 
#  group_by(am) %>% 
#  unnest %>% 
#  group_by(am, .id) %>% 
#  do(model = lm(mpg~wt, data = as.data.frame(.$strap))) %>% 
#  tidy(model)
#  http://danlec.com/st4k#questions/44177275
#  
#  
## Metrics
## pairwisefst
## - no. alleles, allelic richness, heterozygosity, HWE Among populations – Fst (hierarchical or otherwise) and possibly equivalent methods such as Gst and/or Jost’s D


hist(permuteN10$PFrt, breaks=20)
hist(permuteN10$PFlr, breaks=20)
hist(permuteN10$PFsl, breaks=20)
hist(permuteN10$PPopFst, breaks=20)
hist(permuteN10$Fst, breaks=20)

hist(permuteN20$PFrt, breaks=20)
hist(permuteN20$PFlr, breaks=20)
hist(permuteN20$PFsl, breaks=20)
hist(permuteN20$PPopFst, breaks=20)
hist(permuteN20$Fst, breaks=20)


hist(permuteSite$PFrt, breaks=20)
hist(permuteSite$PFlr, breaks=20)
hist(permuteSite$PPopFst, breaks=20)
hist(permuteSite$Fst, breaks=20)

hist(permuteSiteBl$PFrt, breaks=20)
hist(permuteSiteBl$PFlr, breaks=20)
hist(permuteSiteBl$PPopFst, breaks=20)
hist(permuteSiteBl$Fst, breaks=20)

## Check for duplicates
pick <- which(duplicated(permuteN20[,1:16]) == TRUE)
permuteN20[pick,]



permuteSiteGl10 %>% filter(PFrt < 0.05) %>%
 summarize(n = n()/999 * 100)
permuteSiteGl10 %>% filter(PFlr < 0.05) %>%
 summarize(n = n()/999 * 100)
permuteSiteGl10 %>% filter(PFsl < 0.05) %>%
 summarize(n = n()/999 * 100)
permuteSiteGl10 %>% filter(PPopFst < 0.05) %>%
 summarize(n = n()/999 * 100)

permuteBlN15 %>% filter(PFrt < 0.05) %>%
 summarize(n = n()/999 * 100)
permuteBlN15 %>% filter(PFlr < 0.05) %>%
 summarize(n = n()/999 * 100)
permuteBlN15 %>% filter(PFsl < 0.05) %>%
 summarize(n = n()/999 * 100)
permuteBlN15 %>% filter(PPopFst < 0.05) %>%
 summarize(n = n()/999 * 100)


           