
setwd('D:/R_Stuff/AutoAssess')
## Manually choose .rdata file	
myFile <- file.choose()
load(myFile)
getwd()

library(plyr)
library(xlsx)

blackaBdat <- subset(newBlackCE, fishdate >= '2014-11-01' & fishdate <= '2015-10-31')
blackcatch.ply <- ddply(blackaBdat, .(as.factor(new_zone), as.factor(blockno)), summarize,  catch = sum(blips))
colnames(blackcatch.ply) <- c("zone","blockno","catch")
blackcatch.ply$Species <- "Blacklip"
write.xlsx(blackcatch.ply, "D:\\R_Stuff\\AutoAssess\\AbCatchdataForRecs.xlsx", sheetName="Blacklip",  col.names=TRUE, row.names=TRUE, append=FALSE)


greenaBdat <- subset(newGreenCE, fishdate >= '2014-11-01' & fishdate <= '2015-10-31')
greencatch.ply <- ddply(greenaBdat, .(as.factor(zone), as.factor(blockno)), summarize,  catch = sum(glips))
colnames(greencatch.ply) <- c("zone","blockno","catch")
greencatch.ply$Species <- "Greenlip"
write.xlsx(greencatch.ply, "D:\\R_Stuff\\AutoAssess\\AbCatchdataForRecs.xlsx", sheetName="Greenlip",  col.names=TRUE, row.names=TRUE, append=TRUE)


