#use SamFilter as precusror to this file 
# use SamFilter030816.RData 
library(car)
library(MASS)
library(boot)
library(dplyr)
library(plyr)
library(gdata)
library(ggplot2)
library(multcompView)
library(devtools)
keep(SamFilter, SamResults, sure=T)

## The follwoing csv file was created from the Growth parameters tab in sex maturity data.xlsx held in Abalone\Section Shared\Size_limits SAMS\s+2 review
# lat and longs were assigned from the research database except for sites blackjack, Deal island and Hope early which were given lat longs based on closest recorded work.
# lat long for long island (Hogan) was not available in database and was estimated from manifold. IL calculations (MaxDL, L50 , L95) were drawn straight from the excel file and were not recalculated
setwd("D:/Fisheries Research/Abalone/SAM")
IL.info<-read.csv("Inv.Log.Data.csv", header = T)
summary(IL.info)
# names(IL.info)[names(IL.info)=='Latitude']<-"Latitude.IL"
# names(IL.info)[names(IL.info)=='Longitude']<-"Longitude.IL"
# names(IL.info)[names(IL.info)=='Site']<-"Site.IL"
# names(IL.info)[names(IL.info)=='SubBlockNo']<-"SubBlockNo.IL"
# names(IL.info)[names(IL.info)=='Site_code']<-"Growth.site.IL"


IL.Allo<-read.csv("DT_gwth_allocation.csv", header = T)
library(lubridate) # year and month functions
IL.Allo<-IL.Allo[,1:15]

# create new field SiteCode same as in SamResults
IL.Allo$SAM_Date <- as.Date(strptime(as.character(IL.Allo$Date2.IL), "%d/%m/%Y", tz='AUSTRALIA/HOBART'))
IL.Allo$FishYear<-format(IL.Allo$SAM_Date,'%Y')
IL.Allo$SiteCode <- paste(IL.Allo$Site.ID.IL,'_',year(IL.Allo$SAM_Date),'_',month(IL.Allo$SAM_Date), sep="")


#find unique sitecodes in SamResults and extract those records from DT IL.Allo
SiteCodes<-unique(SamFilter$SiteCode)
IL.Allo.extract<-IL.Allo[IL.Allo$SiteCode %in% SiteCodes,]

#remove duplicate records
IL.Allo.extract<-IL.Allo.extract[!duplicated(IL.Allo.extract[,18]),]

IL.cbind<-IL.Allo.extract[,c(4,6:7,12:15,17,18)]

IL.cbind$maxDL<-as.character(IL.cbind$maxDL)
IL.cbind$maxDL<-as.numeric(IL.cbind$maxDL)
IL.cbind$L50<-as.character(IL.cbind$L50)
IL.cbind$L50<-as.numeric(IL.cbind$L50)
IL.cbind$L95<-as.character(IL.cbind$L95)
IL.cbind$L95<-as.numeric(IL.cbind$L95)

#join IL.info and IL.cbind

IL.cbind<-join(IL.cbind, IL.info[,c(3:5)], by = "Growth.site.IL", type= "left")

#join the IL.cbind to SamFilter
SamFilter$Site.ID.IL<-sapply(strsplit(SamFilter$SiteCode, "\\_"), `[[`, 1)
SamFilter$Site.ID.IL<-as.integer(SamFilter$Site.ID.IL)

SamFilterIL<-left_join(SamFilter,IL.cbind, by = 'Site.ID.IL')

# look at unallocated SiteCodes
Non.IL<-subset(SamFilterIL, is.na(maxDL))

Non.IL<-Non.IL[,1:33]

subblocks<-unique(Non.IL$SubBlockNo)

filter.IL<-IL.cbind[IL.cbind$SubBlockNo.IL %in% subblocks,]
#compact SamFilter

#Sam.IL<-SamFilter[,c(1,6,18:20,22,33:41)]
