library(hierfstat)
library(poppr)
library(reshape)
library(xlsx)

# Read data in from csv file, in original AGRF format
# Note: replace "Failed" in the csv files first
AGRF <- read.csv("D:\\Abalone\\Greenlip project\\Ranalysis\\Peaks.csv",  sep=",",quote="\"", dec=".")

# Melt, then cast data into required matrix forma useing functions in package reshape
melted <- melt(AGRF, id=c("Sample.Name", "Marker"), measure.vars=c("Allele.1", "Allele.2"))
casted <- cast(melted, Sample.Name ~ Marker + variable)

#Write  data back to csv file, after data are converted to matrix format
write.csv(casted, file = "D:\\Abalone\\Greenlip project\\Ranalysis\\PeaksMat.csv", quote=FALSE, row.names=FALSE, na="")


#Read data in from csv file, where data are already converted to matrix format
#Note: make sure there is a column name for each column
Mat <- read.csv("D:\\Abalone\\Greenlip project\\Ranalysis\\Mat.csv",  sep=",",quote="\"", dec=".")

#Combine from multipe dataframes which have identical variables (columns) - i.e append dataframes
FullSet <- rbind(Mat, casted)

#Write full matrix to an xlsx file
write.xlsx(FullSet, "\\Abalone\\Greenlip project\\Ranalysis\\FullSet.xlsx", sheetName="Sheet1",  col.names=TRUE, row.names=TRUE, append=FALSE)

#read full matrix from an xlsx file
FullSettest <- read.xlsx("D:\\Abalone\\Greenlip project\\Ranalysis\\FullSet.xlsx", sheetName="Sheet1",  col.names=TRUE, row.names=TRUE, append=FALSE)


#Heirfstat
allele.count(FullSet[,2:31])


# read data file into R for use in heirfstat
data <- read.table("D:/OwnCloud/Fisheries Research/Abalone/GenPermute/greenlip hierfst/3levelfinal.txt",header=TRUE)


#define and name data frame that contains only loci information
loci<- select(data, Loc214,Loc13C12,Loc13F06,Loc16G08,Loc17D11,Loc2B01,Loc2G01,Loc7B11,Loc8D02,Loc9G01,Loc9H03,cmrHr1.14,cmrHr2.30,Hrub1.H08,Hrub3.F03)

temp <- select(data, Lev1,Loc214,Loc13C12,Loc13F06,Loc16G08,Loc17D11,Loc2B01,Loc2G01,Loc7B11,Loc8D02,Loc9G01,Loc9H03,cmrHr1.14,cmrHr2.30,Hrub1.H08,Hrub3.F03)

basic.stats(temp, diploid=TRUE)
poppr(temp)

allele.count(temp,diploid=TRUE)

#create data frame with heirarchical levels
levels <- select(data, Lev1, Lev2, Lev3)

#estimate heirarchical f-statistics
varcomp.glob(levels,loci)

#estimate heirarchical f-statistics for a single locus
varcomp(data.frame(levels,Hrub3.F03))

# test for statistical significance at highest level in heirarchy (between regions)
test.between(loci,rand.unit=Lev2,test=Lev1,nperm=1000)

#test for statistical significance at lowest level in heirarchy (among replicate sites)
test.within(loci,test=Lev3,within=Lev2,nperm=1000)

#test for statistical significance at second level of heirarchy (among locations)
test.between.within(loci,within=Lev1,rand.unit=Lev3,test=Lev2,nperm=1000)

#test for statistical significance at highest level for single locus
test.between(data.frame(Loc9G01),rand.unit=Lev2,test=Lev1,nperm=10000)

#test for statistical significance at lowest level for single locus
test.within(data.frame(Hrub3.F03),test=Lev3,within=Lev2,nperm=1000)

#test for statistical significance at second level of heirarchy for single locus
test.between.within(data.frame(Hrub3.F03),within=Lev1,rand.unit=Lev3,test=Lev2,nperm=1000)


basic.stats(dat, diploid=TRUE) 
