#=============================================================================================================#
# Script created by Mark Christie, contact at Redpath.Christie@gmail.com
# Script created in version R 3.0.1 
# This script: calculates global unbiased (with respect to population size) Fst in R
# Usage notes: run line by line
#=============================================================================================================#
# Set working directory, load packages, import data

library("hierfstat")

setwd("D:/OwnCloud/Fisheries Research/Abalone/GenPermute")  # Set the working directory
dat <- read.table("Microsatellite_data.txt", header=TRUE,  na.strings="?", dec=".", check.names=FALSE) # read in data
head(dat)

#=============================================================================================================#

n.loci    <- (ncol(dat)/2)-1    #how many loci are there?
n.loci
n.per.pop <- table(dat[, 2])    #how many individuals per population
n.per.pop


dat       <- dat[, -1]          #neccessary formatting
col.names <- colnames(dat)      #neccessary formatting
temp      <- dat[, -1]          #neccessary formatting
OUT = NULL                      #pasting diploid columns together to make a single column
for(i in seq(1, ncol(temp), 2)) {
 loc  <- paste(temp[, i], temp[, i+1], sep="")
 loc2 <- as.numeric(as.character(loc))
 OUT  <- cbind(OUT, loc2)
}

dat           <- cbind(dat[, 1], OUT)
rownames(dat) <- make.names(1:length(dat[, 1]), unique = TRUE)   # neccessary formatting
colnames(dat) <- col.names[c(1, seq(2, length(col.names), 2))]   # neccessary formatting

stats         <- basic.stats(as.data.frame(dat), diploid=TRUE)  # calculate basic statistics
stats                                            # view statistics (including Fst)
