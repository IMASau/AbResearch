library(plyr)
library(data.table)
library(dplyr)
load("D:/R_Stuff/AutoAssess/MM_AnalysisOutput060716.RData")
unique(compiled.df$zone_fishery_code)
#Drop Greenlip records
blk_cd<-subset(compiled.df, zone_fishery_code != "AQG")
blk_cd<-subset(compiled.df, zone_fishery_code != "AQG")
blk_cd<-blk_cd[complete.cases(blk_cd[,12]),]

unique(blk_cd$zone_fishery_code)

blk_cd<-blk_cd[complete.cases(blk_cd[,15]),]


SLMax<-ddply(blk_cd,.(blocklist), summarize,  MaxSL = max(shell.length, na.rm=T), 
                    SLq95 = quantile(shell.length, 0.95, na.rm=T))

SLMax$Block2<-lapply(strsplit(as.character(SLMax$blocklist), "\\,"),"[", 2)
SLMax$Block1<-lapply(strsplit(as.character(SLMax$blocklist), "\\,"),"[", 1)                    
SLMax$Block3<-lapply(strsplit(as.character(SLMax$blocklist), "\\,"), tail, 1)

ind <- data.frame(as.numeric(duplicated(SLMax[,c(4,6)])))
colnames(ind) <- c("dup") #renames column for simplicity
SLMax2 <- cbind(SLMax, ind) #bind to original df
df3 <- subset(df2, dup == 1) #subsets df using binary var for duplicated`

Blk1<-SLMax2[,c(2:3,5)]
names(Blk1)[3]<-'BlockNo'
Blk2<-SLMax2[,c(2:4)]
Blk2<-subset(Blk2, Block2 != "NA")
names(Blk2)[3]<-'BlockNo'
Blk3<-SLMax2[,c(2:3,6:7)]
Blk3[Blk3 ==0]<-NA
Blk3<-subset(Blk3, dup != "NA")
names(Blk3)[3]<-'BlockNo'

Blks<-rbind(Blk1, Blk2, Blk3[,c(1:3)])

Blocks<-unique(as.numeric(Blks$BlockNo))
if (exists("MaxSLResults")) 
 rm(MaxSLResults)

for(b in Blocks){
 Bn<-subset(Blks, BlockNo ==b)
MaxSL<-max(Bn$MaxSL)
SLq95<-max(Bn$SLq95)
pick<-data.frame(b, MaxSL, SLq95)
if (exists("MaxSLResults"))
 MaxSLResults <- rbind(MaxSLResults, pick)
else
 MaxSLResults <- pick
}
names(MaxSLResults)[1]<-'BlockNo'
write.csv(MaxSLResults, file= 'D:/Fisheries Research/Abalone/SAM/MMBlockMaxSL.csv')
