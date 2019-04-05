#### MARKET MEASURE ANALYSIS ####

# for windows 8 database connections use this in windows explorer
# C:\Windows\SysWOW64\odbcad32.exe

## load required libaries for analysis
library(RODBC)
library(R.utils)
library(lubridate)
#library(plyr)
library(rgdal)
library(sp)
library(maptools)
library(tools)
library(openxlsx)
library(lubridate)
library(gdata)
library(dplyr)
library(doBy)
library(tidyr)
library(tidyverse)
library(splitstackshape)
library(readxl)
library(data.table)

###############################################################################################################

#
 #
  #
   #Catch-effort and processor data ####
  #  
 #
#
# Import the most recent catch-effort and processor meta-data to link with market measure data to identify
# capture locations, dates, etc.

## load most recent newBlackCE dataframe from the catcheffort data output
setwd('c:/CloudStor/R_Stuff/AutoAssess')

## manually select the most recent .rdata file (or file that covers the market measure timeframe)
myFile <- file.choose()
load(myFile)
getwd()

## load most recent abalone processor details
#load("Ab_processor220616.RData")
load('AbProcessors_2018_10_09.RData')
#keep(processorlist, docketinfo, sure=T)
rm(abprocs, docketblocks, docketblocks.bl, docketblocks.gl, docketlink, GaryMM)

## Change 'docket_number' column name out of SQL format
#summary(docketinfo)
#names(docketinfo)
names(docketinfo)[names(docketinfo)=="docket_number"] <- "docket.number"

###############################################################################################################

#   
 #  
  #                                         
   #Post 2010 MM data held in Gary Carlos db ####
  #
 # 
#  
# Extract data from Access database establisehed by Gary Carlos holding all electronic measuring board
# data from 2010 (part) to 2015. 

## establish connection with Access database and create dataframe 'FeMM'
channel <- odbcConnect('Gary_abMM')
sql <- "SELECT 
DataLoggerImport.DownlaodEventID, 
DataLoggerImport.measureID, 
DataLoggerImport.DownloadFile, 
DataLoggerImport.Order, DataLoggerImport.Zone, 
DataLoggerImport.Entitlement, 
DataLoggerImport.Docket, 
DataLoggerImport.Size, 
DataLoggerImport.msrDate, 
DataLoggerImport.msrTime
FROM DataLoggerImport
ORDER BY DataLoggerImport.DownloadFile, 
DataLoggerImport.msrDate, 
DataLoggerImport.msrTime;
"
FeMM <- sqlQuery(channel, sql)
close(channel)

## get mesuring board download date(s) from database
channel <- odbcConnect('Gary_abMM')
sql <- "SELECT 
DownlaodEvent.DownlaodEventID, 
DownlaodEvent.DownloadDate,
DownlaodEvent.DataSourceID
FROM DownlaodEvent
ORDER BY DownlaodEvent.DownlaodEventID;
"
Download.date <- sqlQuery(channel, sql)
close(channel)

## create dataframe of processors and identify download date of their electronic measuring boards
e.processors <- c("RALPH'S TASMANIAN SEAFOOD PTY LTD","ABALONE TASMANIA PTY LTD", "TASMANIAN SEAFOODS PTY LTD", "TASMANIAN SEAFOODS PTY LTD", "ADELAIDE BAY SEAFOODS PTY LTD")
e.pro <- as.data.frame(e.processors)
e.pro$DataSourceID <- c(1,2,3,4,5)
Download.date <- left_join(Download.date, e.pro, by = "DataSourceID")

## join FeMM and Download.date
FeMM <- left_join(FeMM, Download.date, by = "DownlaodEventID")

## rename columns out of SQL format
colnames(FeMM) <- c("download.event", "measure.id", "Download.file", "record.number", "Zone", "entitlement.number", "docket.number","shell.length",
                    "msr.date", "msr.time", "dload.date", "datasource", "eprocessors") #, "msr.time","fishdate", "year")
#
 # STEP 1 - remove records without docket.number and missing shell.length. ####
#

FeMM <- FeMM[!is.na(FeMM$docket.number),]
FeMM <- FeMM[!is.na(FeMM$shell.length),]
FeMM <- droplevels(subset(FeMM, docket.number !=0))

# PROBLEM = multiple downloads of same data into gary database gives duplicate data

#
 # STEP 2 -  remove duplicate records by selecting for matchings docket.numbers and first selecting docket with ####
#            max number of animals and then if still multiple by first download date.

#n.per.docket<-ddply(FeMM,.(docket.number, dload.date, eprocessors), summarize,  n = length(shell.length), 
                                                                                #meanSL = round(mean(shell.length), 1),
                                                                               # minSL = round(min(shell.length), 1))
n.per.docket <- FeMM %>%
 group_by(docket.number, dload.date, eprocessors) %>%
 summarise(n = length(shell.length), 
           meanSL = round(mean(shell.length), 1),
           minSL = round(min(shell.length), 1))

## loop to keep the duplicate record with the higher n of animals and then initial download date only
db.dup.dockets <- unique(n.per.docket$docket.number)

if (exists("pick_db.docket")) 
 rm(pick_db.docket)

for(b in db.dup.dockets){
 choice<-subset(n.per.docket, docket.number ==b)
 maxim<-max(choice$n)  
 pick<-subset(choice, n == maxim)
 minin<-min(pick$dload.date)
 pick<-subset(pick, dload.date == minin)
  if (exists("pick_db.docket"))
  pick_db.docket <- rbind(pick_db.docket, pick)
 else
  pick_db.docket <- pick
}

## check to ensure each docket.number is now unique 
n_occur <- data.frame(table(pick_db.docket$docket.number))
range(n_occur$Freq)

# subset FeMM database based on loop above
#FeMM.sub <- inner_join(FeMM, pick_db.docket, by = c("docket.number", "dload.date"))
FeMM.sub <- inner_join(FeMM, pick_db.docket)

#
 # STEP 3 - subset docketinfo to >2010, and by processors that have e-boards ####
#

## FeMM database only has data from 2010 onwards so to reduce chances of duplicate records subset docketinfo to post 2009
docketinfo.2010 <- droplevels(subset(docketinfo,unloading_date >= as.POSIXct('2010-01-01 00:00')))

e.processors <- c("RALPH'S TASMANIAN SEAFOOD PTY LTD","ABALONE TASMANIA PTY LTD", "TASMANIAN SEAFOODS PTY LTD", "ADELAIDE BAY SEAFOODS PTY LTD", "M & K HAULAGE (TAS) PTY LTD")
docketinfo.2010 <- droplevels(subset(docketinfo.2010, processorname %in% e.processors))

#
 # STEP 4 - join unique database docket to docketinfo.2010 ####
#

## join unique FeMM dockets to the identifying information in docket.info from FILMS database
FeMM.docket.info <- left_join(pick_db.docket, docketinfo.2010, by = "docket.number")

## separate dupilicated dockets (how many times each docket.number occur?)
n_occur <- data.frame(table(FeMM.docket.info$docket.number))
range(n_occur$Freq)

#
 # STEP 5 - extract unique dockets and join to FeMMsub ####
#

## docket.uniq is all dockets without duplicates and can then be joined back to the original FeMMsub data
docket.uniq <- as.data.frame(FeMM.docket.info[FeMM.docket.info$docket.number %in% n_occur$Var1[n_occur$Freq == 1],])

## OUTPUT 1 = create new dataframe joining the FeMMsub to docket.uniq only keeping common docket data to both

#Output1.unique<-inner_join(FeMM.sub, docket.uniq, by = "docket.number")
Output1.unique <- inner_join(FeMM.sub, docket.uniq)

## add columns to show date differences between landing date and msr.Date and dload.Date
Output1.unique$dload.date <- as.Date(Output1.unique$dload.date, format="yyyy-%mm-%dd")
Output1.unique$unloading_date <- as.Date(Output1.unique$unloading_date, format="yyyy-%mm-%dd")
Output1.unique$msr.date.diff <- as.Date(Output1.unique$msr.date, format="yyyy-%mm-%dd")-as.Date(Output1.unique$unloading_date, format="yyyy-%mm-%dd")
Output1.unique$dload.date.diff <- as.Date(Output1.unique$dload.date, format="yyyy-%mm-%dd")-as.Date(Output1.unique$unloading_date, format="yyyy-%mm-%dd")

## remove dockets with errors in dates and or processor

# dockets with issues in that download.date is prior to landing date but there are no duplicated dockets suggestion is incorrect docket number in eboard.
dload.date.false <- subset(Output1.unique, dload.date.diff <= -1)
dload.date.f.unique <- unique(dload.date.false[c("docket.number", "eprocessors", "dload.date", "unloading_date",
                                               "dload.date.diff", "msr.date.diff")])
#Output.error.date<-join(dload.date.f.unique, docketinfo.2010,  by = "docket.number", type ="left")
Output.error.date <- left_join(dload.date.f.unique, docketinfo.2010,  by = "docket.number")

# dockets with issues in that eprocessor is not processorID
epro.proID.false <- as.data.frame(Output1.unique[Output1.unique$eprocessors != Output1.unique$processorname,])
epro.proID.false <- epro.proID.false[!is.na(epro.proID.false$docket.number),]
epro.proID.f.unique <- unique(epro.proID.false[c("docket.number", "eprocessors", "dload.date", "unloading_date","msr.date",
                                               "dload.date.diff", "msr.date.diff")])
#Output.error.epro<-join(epro.proID.f.unique, docketinfo.2010,  by = "docket.number", type ="left")
Output.error.epro <- left_join(epro.proID.f.unique, docketinfo.2010,  by = "docket.number")


# remove the dockets with issues
Output1.unique <- Output1.unique[!(Output1.unique$docket.number %in% dload.date.f.unique$docket.number),]
Output1.unique <- Output1.unique[!(Output1.unique$docket.number %in% epro.proID.false$docket.number),]

#
 # STEP 6 - remove duplicated dockets remove split catches by matching eprocessor (garydb) to processorID (films) in dupliacted catches. ####
#

## select only dupicated dockets from the FeMM.docket.info list
docket.dupes <- as.data.frame(FeMM.docket.info[FeMM.docket.info$docket.number %in% n_occur$Var1[n_occur$Freq > 1],])
docket.epro <- subset(docket.dupes, (docket.dupes$eprocessors == docket.dupes$processorname))

n_occur <- data.frame(table(docket.epro$docket.number))
range(n_occur$Freq)

#
 # STEP 7 - extract unique dockets and join to FeMMsub. ####
#

## docket.uniq is all dockets without duplicates and can then be joined back to the original FeMMsub data
docket.epro.uniq <- as.data.frame(docket.epro[docket.epro$docket.number %in% n_occur$Var1[n_occur$Freq == 1],])
 
## OUTPUT 2 = create new dataframe joining the FeMMsub to docket.epro.uniq only keeping common docket data to both
#Output2.epro<-join(FeMM.sub, docket.epro.uniq, by = "docket.number", type = "inner")
Output2.epro <- inner_join(FeMM.sub, docket.epro.uniq)

## add columns to show date differences between landing date, msr.Date and dload.Date
Output2.epro$dload.date <- as.Date(Output2.epro$dload.date, format="yyyy-%mm-%dd")
Output2.epro$unloading_date <- as.Date(Output2.epro$unloading_date, format="yyyy-%mm-%dd")
Output2.epro$msr.date.diff <- as.Date(Output2.epro$msr.date, format="yyyy-%mm-%dd")-as.Date(Output2.epro$unloading_date, format="yyyy-%mm-%dd")
Output2.epro$dload.date.diff <- as.Date(Output2.epro$dload.date, format="yyyy-%mm-%dd")-as.Date(Output2.epro$unloading_date, format="yyyy-%mm-%dd")

#
 # STEP 8 extract unique dockets and join to FeMMsub ####
#

docket.dble <- as.data.frame(docket.epro[docket.epro$docket.number %in% n_occur$Var1[n_occur$Freq > 1],])

## look at unique dates in dupes
docket.dble$dload.date.diff <- as.Date(docket.dble$dload.date, format="yyyy-%mm-%dd")-as.Date(docket.dble$unloading_date, format="yyyy-%mm-%dd")
docket.dble <- subset(docket.dble, dload.date.diff >= -1)
d.docket <- unique(docket.dble$docket.number)

## loop to keep the duplicate record with the smallest date.diff between landing date and dload.date
if (exists("pick.dble.docket")) 
 rm(pick.dble.docket)

for(d in d.docket){
 choice<-subset(docket.dble, docket.number ==d)
 minim<-min(choice$dload.date.diff)  
 pick<-subset(choice, dload.date.diff == minim)
 
 if (exists("pick.dble.docket"))
  pick.dble.docket <- rbind(pick.dble.docket, pick)
 else
  pick.dble.docket <- pick
}

## check on how many times does each docket.number occur
n_occur <- data.frame(table(pick.dble.docket$docket.number))
range(n_occur$Freq)

## OUTPUT 3 = create new dataframe joining the FeMM to docket.dupes.uniq only keeping common docket data to both
#Output3.dble <- join(FeMM.sub, pick.dble.docket, by = "docket.number", type = "inner")
Output3.dble <- inner_join(FeMM.sub, pick.dble.docket)

## add columns to show date differences between ladning date and msr.Date and dload.Date
Output3.dble$dload.date <- as.Date(Output3.dble$dload.date, format="yyyy-%mm-%dd")
Output3.dble$unloading_date <- as.Date(Output3.dble$unloading_date, format="yyyy-%mm-%dd")
Output3.dble$msr.date.diff <- as.Date(Output3.dble$msr.date, format="yyyy-%mm-%dd")-as.Date(Output3.dble$unloading_date, format="yyyy-%mm-%dd")
Output3.dble$dload.date.diff <- as.Date(Output3.dble$dload.date, format="yyyy-%mm-%dd")-as.Date(Output3.dble$unloading_date, format="yyyy-%mm-%dd")

#
 # STEP 9 - Compile the final dataframe ####
#

## add column to identify which output each docket is derived from
Output1.unique$output <- 1
Output2.epro$output <- 2
Output3.dble$output <- 3

compiled.docket.FeMM <- rbind(Output1.unique, Output3.dble, Output2.epro)

## reduce column number
compiled.docket.FeMM <- droplevels(subset(compiled.docket.FeMM, select = c("output", "joincode", "docket.number", "shell.length","meanSL", "minSL", "n","total_landed_weight", "catch", "msr.date", "dload.date", "unloading_date",
                                                               "dload.date.diff", "msr.date.diff", "eprocessors", "processorname", "processor_licence_id", "zone_fishery_code", "Zone", "received_location", 
                                                              "blocklist", "numblocks", "subblocklist", "numsubblocks", "Download.file")))

## subset records with bad docket.numbers e.g. missing all matching info from FILMS
Output.error.docket <- compiled.docket.FeMM[is.na(compiled.docket.FeMM$processorname),]
compiled.docket.FeMM <- compiled.docket.FeMM[!(compiled.docket.FeMM$docket.number %in% Output.error.docket$docket.number),]

## there are now four dataframes from this work
summary(compiled.docket.FeMM) # is the complete MM data with attached docket info
summary(Output.error.date) # is records with errors with dates dload.date preceeds landing date but there is no duplicate records which match
summary(Output.error.epro) # is records with non-matching processors from FILMS database and the eboard from which data was collected.
summary(Output.error.docket) # is records with non-matching docket.numbers from FILMS database.

## in all cases of the error files i suspect there has been a mistake in the factory of docket.number entered.
###############################################################################################################

#keep(compiled.docket.FeMM, Output.error.docket, Output.error.epro, Output.error.date, docketinfo, sure=T)
rm(list=ls()[! ls() %in% c('compiled.docket.FeMM', 'Output.error.docket', 'Output.error.epro', 'Output.error.date', 'docketinfo')])
#saveRDS(compiled.docket.FeMM, 'C:/CloudStor/R_Stuff/MMLF/compiled.docket.FeMM.RDS')

###############################################################################################################

#
 #
  #
   # 2007-2011 abalone e-measure MM raw data ####
  #
 #
#      
# allocation of docket information to raw data held in market measuring folder for dates 2007-2011

setwd("R:/TAFI/TAFI_MRL_Sections/Wild_Fisheries_Program/Shared/13. Market measuring/MM_2007_2010_by_Processor")

## loop to search for summary files
MM_files <- c()
#change the grep to search based on folder names
for (dir_finals in grep('a',list.files(path='.',all.files=FALSE,full.names=TRUE),value=TRUE))
{
 MM_files <- c(MM_files,grep('MM_RawData_',list.files(path = dir_finals, all.files = FALSE, full.names = TRUE, recursive = TRUE),value=TRUE) )
}
all_data<-NULL
#combine files
Raw_MM <- do.call(rbind,lapply(MM_files,read.csv))
summary(Raw_MM)

## tidy up some of the column names and data in Raw_MM dataframe
colnames(Raw_MM) <- c("x","sample.number", "docket.number","shell.length","time","msr.date", "filesource","e.processor")
Raw_MM$msr.date <- strptime(as.character(Raw_MM$msr.date), "%d/%m/%Y")
Raw_MM$msr.date<-as.POSIXct(Raw_MM$msr.date)
Raw_MM$docket.number <- as.integer(Raw_MM$docket.number)
Raw_MM$shell.length <- as.integer(Raw_MM$shell.length)

#
 # STEP 1 - remove records without docket number or docket = 0 or shell length = NA ####
#

Raw.MM.Output.error.docket <- Raw_MM[is.na(Raw_MM$docket.number),]
Raw_MM <- Raw_MM[!is.na(Raw_MM$docket.number),]
Raw_MM <- Raw_MM[!is.na(Raw_MM$shell.length),]
Raw_MM <- droplevels(subset(Raw_MM, docket.number !=0))

## rename processors
Raw_MM$e.processor <- as.character(Raw_MM$e.processor)
Raw_MM$e.processor[Raw_MM$e.processor == "AbaloneTasmania"] <- "ABALONE TASMANIA PTY LTD"
Raw_MM$e.processor[Raw_MM$e.processor == "Tas Seafoods Margate"] <- "TASMANIAN SEAFOODS PTY LTD"
Raw_MM$e.processor[Raw_MM$e.processor == "Tas Seafoods Smithton"] <- "TASMANIAN SEAFOODS PTY LTD"
Raw_MM$e.processor[Raw_MM$e.processor == "Ralphs"] <- "RALPH'S TASMANIAN SEAFOOD PTY LTD"
Raw_MM$e.processor[Raw_MM$e.processor == "Coastal Waters"] <- "COASTAL WATERS SEAFOODS PTY LTD"
Raw_MM$e.processor[Raw_MM$e.processor == "Tas Live Abalone"] <- "TAS LIVE ABALONE PTY LTD"
Raw_MM$e.processor[Raw_MM$e.processor == "MK Haulage"] <- "M & K HAULAGE (TAS) PTY LTD"
Raw_MM$e.processor <- as.factor(Raw_MM$e.processor)
eproname <- as.data.frame(unique(Raw_MM$e.processor))
colnames(eproname) <- c("e.processor")

## additional data cleaning as some msr.date years appear to be errors (i.e. 0020 and 2088)
Raw_MM$msr.date[which.minn(Raw_MM$msr.date, 20)]
Raw_MM$msr.date[which.maxn(Raw_MM$msr.date, 20)]
Raw_MM$msr.date[Raw_MM$msr.date < as.Date('2000-01-01')] <- NA
Raw_MM$msr.date[Raw_MM$msr.date > as.Date('2011-02-11')] <- NA

## convert incorrect times to NA's
time.dates <- grep('/', Raw_MM$time)
Raw_MM$time[time.dates] <- NA

## summarize the Raw_MM dataset
n.per.docket <- Raw_MM %>%
 group_by(docket.number, msr.date, e.processor) %>%
 summarise(n = length(shell.length), 
                    meanSL = round(mean(shell.length), 1),
                    minSL = round(min(shell.length), 1))

## remove non-e.processors form the docketinfo dataframe and rename column.
e.processors <- eproname$e.processor
docketinfo.epro <- droplevels(subset(docketinfo, processorname %in% e.processors))
colnames(docketinfo.epro)[colnames(docketinfo.epro)=="processorname"] <- "e.processor"

## match docketinfo.epro to the n.per.docket dataframe
#docket.join<-join(n.per.docket, docketinfo.epro, by = c("docket.number", "e.processor"), type = "inner")
docket.join <- inner_join(n.per.docket, docketinfo.epro)

## add date difference column
docket.join$msr.date.diff <- as.Date(docket.join$msr.date, format="yyyy-%mm-%dd")-as.Date(docket.join$unloading_date, format="yyyy-%mm-%dd")

#
 # STEP 2 - extract unique dockets and join to RawMM ####
#

## separate dupilicated dockets
# how many times does each docket.number occur
n_occur <- data.frame(table(docket.join$docket.number))
range(n_occur$Freq)
docket.uniq <- as.data.frame(docket.join[docket.join$docket.number %in% n_occur$Var1[n_occur$Freq == 1],])

# check
n_occur <- data.frame(table(docket.uniq$docket.number))
range(n_occur$Freq)

## OUTPUT 1 = create new dataframe joining the FeMMsub to docket.uniq only keeping common docket data to both
#Output1.unique <- join(Raw_MM, docket.uniq, by = c("docket.number", "e.processor"), type = "inner")
Output1.unique <- inner_join(Raw_MM, docket.uniq)

#
 #STEP 3 - Duplicated dockets, remove duplicated catches which have multiple downloads in RawMM. ####
#

## reset n-occur to docket.join
n_occur <- data.frame(table(docket.join$docket.number))
range(n_occur$Freq)

## select only dupicated dockets from the docket.join df
docket.dupes <- as.data.frame(docket.join[docket.join$docket.number %in% n_occur$Var1[n_occur$Freq > 1],])

## extract all dockets with duplicated results in docket.number and meanSL 
dupes.sub <- docket.dupes[ cbind( which(duplicated(docket.dupes[c(1,5)])), which(duplicated(docket.dupes[c(1,5)], fromLast=F))),]
dupes.sub <- dupes.sub[order(dupes.sub$msr.date),]
dupes.sub <- dupes.sub %>% distinct(docket.number, .keep_all = T) #was missing .keep_all = T and droppping variables (JM)

## n-occur to docket.join
n_occur <- data.frame(table(dupes.sub$docket.number))
range(n_occur$Freq)

## OUTPUT 2 = create new dataframe joining the FeMMsub to docket.uniq only keeping common docket data to both
#Output2.dupes <- join(Raw_MM, dupes.sub, by = c("docket.number", "e.processor"), type = "inner")
Output2.dupes<-inner_join(Raw_MM, dupes.sub)

#
 #STEP 4 - Duplicated dockets, remove duplicated records which have multiple docket entries. ####
#

## cut download duplicates from dupes.sub.dnumber that are assigned in Output2.dupes
dupes.sub.dnumber <- unique(Output2.dupes$docket.number)
dupes.multi <- docket.dupes[!docket.dupes$docket.number %in% dupes.sub.dnumber,]

# #dupes.multi<-droplevels(subset(dupes.multi, select = c("docket.number",  "msr.date", "unloading_date", "msr.date.diff", "meanSL", "minSL", "n", 
#                                                        "total_landed_weight", "catch",
#                                                       "received_location", "e.processor", "joincode", "processor_licence_id", 
#                                                        "blocklist", "numblocks", "subblocklist", "numsubblocks")))

## change records with false msr.date to NA
dupes.multi$msr.date.diff[dupes.multi$msr.date.diff<=-100] <- NA

## remove duplicates with NA results, negative date diff and false large date diff.
dupes.multi <- droplevels(subset(dupes.multi, msr.date.diff >=0 & msr.date.diff<=100))

## remove duplicates with same docket number and unloading date
dupes.multi.sub <- docket.dupes[ cbind( which(duplicated(dupes.multi[c(1,3)])), which(duplicated(dupes.multi[c(1,3)], fromLast=F))),]

## select distinct eg unqiue dockets (dplyr) function
dupes.multi.sub <- dupes.multi.sub %>% distinct(docket.number, .keep_all = T)

## identify unique dockets and remove from dupes.multi
dupes.ms.d <- unique(dupes.multi.sub$docket.number)
dupes.multi <- dupes.multi[!dupes.multi$docket.number %in% dupes.ms.d,]

## OUTPUT 3 = create new dataframe joining the FeMMsub to docket.uniq only keeping common docket data to both
#Output3.multi <- join(Raw_MM, dupes.multi, by = c("docket.number", "msr.date"), type = "inner")
Output3.multi <- inner_join(Raw_MM, dupes.multi)

## OUTPUT 4 = create new dataframe joining the FeMMsub to docket.uniq only keeping common docket data to both
#Output4.extra <- join(Raw_MM, dupes.multi.sub, by = c("docket.number", "msr.date"), type = "inner")
Output4.extra <- inner_join(Raw_MM, dupes.multi.sub)

## format each output data.frame to be identical columns:

Output1.unique <- subset(Output1.unique, select=c("docket.number",  "filesource", "msr.date", "unloading_date", "msr.date.diff", "shell.length", 
                                                "total_landed_weight", "catch", "received_location", "e.processor", "joincode", 
                                                "processor_licence_id", "zone_fishery_code", "blocklist", "numblocks", "subblocklist", "numsubblocks"))
Output2.dupes <- subset(Output2.dupes, select=c("docket.number",  "filesource", "msr.date", "unloading_date", "msr.date.diff", "shell.length", 
                                                "total_landed_weight", "catch", "received_location", "e.processor", "joincode", 
                                                "processor_licence_id", "zone_fishery_code", "blocklist", "numblocks", "subblocklist", "numsubblocks"))
Output3.multi <- subset(Output3.multi, select=c("docket.number",  "filesource", "msr.date", "unloading_date", "msr.date.diff", "shell.length", 
                                              "total_landed_weight", "catch", "received_location", "e.processor", "joincode", 
                                              "processor_licence_id", "zone_fishery_code", "blocklist", "numblocks", "subblocklist", "numsubblocks"))
Output4.extra <- subset(Output4.extra, select=c("docket.number",  "filesource", "msr.date", "unloading_date", "msr.date.diff", "shell.length", 
                                              "total_landed_weight", "catch", "received_location", "e.processor", "joincode", 
                                              "processor_licence_id", "zone_fishery_code", "blocklist", "numblocks", "subblocklist", "numsubblocks"))
#
 # STEP 5 - compile Outputs ####
#
compiled.docket.07.11 <- rbind(Output4.extra, Output3.multi, Output1.unique, Output2.dupes)


###############################################################################################################

#keep(compiled.docket.FeMM, compiled.docket.07.11, Output.error.docket, Output.error.epro, Output.error.date, Raw.MM.Output.error.docket, docketinfo, sure=T)
rm(list=ls()[! ls() %in% c('compiled.docket.FeMM', 'compiled.docket.07.11', 'Output.error.docket', 'Output.error.epro', 'Output.error.date', 'Raw.MM.Output.error.docket', 'docketinfo')])

###############################################################################################################

#
 #
  #
   # Abalone e-measure MM data held in abalone database ####
  #
 #
#
# docket information held in Abalone research database FactoryEmeasure Query 

# extract FactoryEmeasure data from research Database table FactoryEmeasure
channel <- odbcConnect('Asd_data')
sql <- "SELECT FactoryEmeasure.[Sequence number], 
FactoryEmeasure.[Docket number], 
FactoryEmeasure.Length, 
FactoryEmeasure.Time, 
FactoryEmeasure.Date, 
FactoryEmeasure.block, 
FactoryEmeasure.zone, 
FactoryEmeasure.diver, 
FactoryEmeasure.Factory, 
FactoryEmeasure.[est blips], 
FactoryEmeasure.[est glips], 
FactoryEmeasure.[landed weight], 
FactoryEmeasure.[sample quality]
FROM FactoryEmeasure;"
abdbMM <- sqlQuery(channel, sql)
close(channel)

## rename col.names from sql format and reformat date column
colnames(abdbMM) <- c("sample.number","docket.number", "shell.length","time","msr.date", "blocklist", "zone", "diver", 
                      "e.processor", "est.Blip", "est.Glip", "total_landed_weight", "sample.quality")
abdbMM$msr.date <- as.Date(abdbMM$msr.date, format="%d/%m/%Y" )


## remove records without docket number or docket = 0 or shell length = NA
Output.error.docket.abdbMM <- abdbMM[is.na(abdbMM$docket.number),]
abdbMM<-abdbMM[!is.na(abdbMM$docket.number),]
abdbMM<-abdbMM[!is.na(abdbMM$shell.length),]

## rename processors
abdbMM$e.processor <- as.character(abdbMM$e.processor)
abdbMM$e.processor[abdbMM$e.processor == "Adelaide Bay Seafoods Pty Ltd"] <- "ADELAIDE BAY SEAFOODS PTY LTD"
abdbMM$e.processor[abdbMM$e.processor == "Tasmanian Seafoods at Margate"] <- "TASMANIAN SEAFOODS PTY LTD"
abdbMM$e.processor[abdbMM$e.processor == "Tasmanian Seafoods at Smithton"] <- "TASMANIAN SEAFOODS PTY LTD"
abdbMM$e.processor[abdbMM$e.processor == "Ralphs Tasmanian Seafood Pty Ltd"] <- "RALPH'S TASMANIAN SEAFOOD PTY LTD"
abdbMM$e.processor <- as.factor(abdbMM$e.processor)

## summarize the dataset
n.per.docket <- abdbMM %>%
 group_by(docket.number, msr.date, e.processor) %>%
 summarise(n = length(shell.length), 
                    meanSL = round(mean(shell.length), 1),
                    minSL = round(min(shell.length), 1))

## create dataframe of all e-processors
eproname <- as.data.frame(unique(abdbMM$e.processor))
colnames(eproname) <- c("e.processor")

#
 # STEP 1 - remove non-e.processors form the docketinfo dataframe and rename column. ####
#

e.processors <- eproname$e.processor
docketinfo.epro <- droplevels(subset(docketinfo, processorname %in% e.processors))
colnames(docketinfo.epro)[colnames(docketinfo.epro)=="processorname"] <- "e.processor"

## match docketinfo.epro to the n.per.docket dataframe
#docket.join<-join(n.per.docket, docketinfo.epro, by = c("docket.number", "e.processor"), type = "inner")
docket.join <- inner_join(n.per.docket, docketinfo.epro, by = c("docket.number", "e.processor"))

## add date difference column
docket.join$msr.date.diff <- as.Date(docket.join$msr.date, format="yyyy-%mm-%dd")-as.Date(docket.join$unloading_date, format="yyyy-%mm-%dd")

#
 # STEP 2 - extract unique dockets and join to abdbMM ####
#

## separate dupilicated dockets
# how many times does each docket.number occur
n_occur <- data.frame(table(docket.join$docket.number))
range(n_occur$Freq)
docket.uniq <- as.data.frame(docket.join[docket.join$docket.number %in% n_occur$Var1[n_occur$Freq == 1],])

## check
n_occur <- data.frame(table(docket.uniq$docket.number))
range(n_occur$Freq)

## OUTPUT 1 = create new dataframe joining the FeMMsub to docket.uniq only keeping common docket data to both
# dplyr inner_join adds .x and .y suffixes to some variables common between dataframes when joining (e.g. blocklist). 
# It appears that the values in these columns are presented slightly different between the dataframes and
# may be causing the suffix to be added. This creates issues later in the coding when calling for these variables.
# Using the plyr join function seems to resolve this.
library(plyr)
Output1.unique <- join(abdbMM, docket.uniq, by = c("docket.number", "e.processor"), type = "inner")
#Output1.unique.1 <- inner_join(abdbMM, docket.uniq, by = c("docket.number", "e.processor")) 

#
 #STEP 3 - duplicated dockets, remove duplicated catches which have multiple downloads in RawMM. ####
#

## reset n-occur to docket.join
n_occur <- data.frame(table(docket.join$docket.number))
range(n_occur$Freq)

## select only dupicated dockets from the FeMM.docket.info list
docket.dupes <- as.data.frame(docket.join[docket.join$docket.number %in% n_occur$Var1[n_occur$Freq > 1],])

## extract all dockets with duplicated results in docket.number and meanSL 
dupes.sub <- docket.dupes[ cbind( which(duplicated(docket.dupes[c(1,5)])), which(duplicated(docket.dupes[c(1,5)], fromLast=F))),]
dupes.sub <- dupes.sub[order(dupes.sub$msr.date),]
dupes.sub <- dupes.sub %>% distinct(docket.number, .keep_all = T)

## n-occur to docket.join
n_occur <- data.frame(table(dupes.sub$docket.number))
range(n_occur$Freq)

## OUTPUT 2 = create new dataframe joining the FeMMsub to docket.uniq only keeping common docket data to both
Output2.dupes <- join(abdbMM, dupes.sub, by = c("docket.number", "e.processor"), type = "inner")
#Output2.dupes <- inner_join(abdbMM, dupes.sub, by = c("docket.number", "e.processor"))

#
 #STEP 4 DUPLICATED DOCKETS remove duplicated records which have multiple docket entries.
#

## cut download duplicates from dupes.sub.dnumber that are assigned in Output2.dupes
dupes.sub.dnumber <- unique(Output2.dupes$docket.number)
dupes.multi <- docket.dupes[!docket.dupes$docket.number %in% dupes.sub.dnumber,]

# #dupes.multi<-droplevels(subset(dupes.multi, select = c("docket.number",  "msr.date", "unloading_date", "msr.date.diff", "meanSL", "minSL", "n", 
#                                                        "total_landed_weight", "catch",
#                                                       "received_location", "e.processor", "joincode", "processor_licence_id", 
#                                                        "blocklist", "numblocks", "subblocklist", "numsubblocks")))
# change records with false msr.date to NA
dupes.multi$msr.date.diff[dupes.multi$msr.date.diff <= -100] <- NA


## remove duplicates with same docket number and unloading date
dupes.multi.sub <- dupes.multi[ cbind( which(duplicated(dupes.multi[c(1,3,13)])), which(duplicated(dupes.multi[c(1,3,13)], fromLast=F))),]

## select distinct eg unqiue dockets (dplyr) function
dupes.multi.sub <- dupes.multi.sub %>% distinct(docket.number, .keep_all = T)

## OUTPUT 4 = create new dataframe joining the FeMMsub to docket.uniq only keeping common docket data to both
Output4.extra <- join(abdbMM, dupes.multi.sub, by = c("docket.number", "msr.date"), type = "inner")
#Output4.extra <- inner_join(abdbMM, dupes.multi.sub, by = c("docket.number", "msr.date"))

## identify unique dockets and remove from dupes.multi
dupes.ms.d <- unique(dupes.multi.sub$docket.number)
dupes.multi <- dupes.multi[!dupes.multi$docket.number %in% dupes.ms.d,]

## remove duplicates with NA results, negative date diff and false large date diff.
dupes.multi.sub <- droplevels(subset(dupes.multi, msr.date.diff >= 0 & msr.date.diff <= 100))

## OUTPUT 5 = create new dataframe joining the abdbMM to dupes.multi.sub only keeping common docket data to both
Output5.date <- join(abdbMM, dupes.multi.sub, by = c("docket.number"), type = "inner")
#Output5.date <- inner_join(abdbMM, dupes.multi.sub, by = c("docket.number"))

## identify unique dockets and remove from dupes.multi
dupes.ms.d <- unique(dupes.multi.sub$docket.number)
dupes.multi <- dupes.multi[!dupes.multi$docket.number %in% dupes.ms.d,]

## OUTPUT 3 = create new dataframe joining the FeMMsub to docket.uniq only keeping common docket data to both
Output3.multi <- join(abdbMM, dupes.multi, by = c("docket.number"), type = "inner")
#Output3.multi <- inner_join(abdbMM, dupes.multi, by = c("docket.number"))

## format each output data.frame to be identical columns:
names(Output1.unique)
Output1.unique <- subset(Output1.unique, select=c("docket.number",  "msr.date", "unloading_date", "msr.date.diff", "shell.length", 
                                                "total_landed_weight", "catch", "received_location", "e.processor", "joincode", 
                                                "processor_licence_id", "zone_fishery_code", "blocklist", "numblocks", "subblocklist", "numsubblocks"))
Output2.dupes<-subset(Output2.dupes, select=c("docket.number",  "msr.date", "unloading_date", "msr.date.diff", "shell.length", 
                                              "total_landed_weight", "catch", "received_location", "e.processor", "joincode", 
                                              "processor_licence_id", "zone_fishery_code", "blocklist", "numblocks", "subblocklist", "numsubblocks"))
Output3.multi<-subset(Output3.multi, select=c("docket.number",  "msr.date", "unloading_date", "msr.date.diff", "shell.length", 
                                              "total_landed_weight", "catch", "received_location", "e.processor", "joincode", 
                                              "processor_licence_id", "zone_fishery_code", "blocklist", "numblocks", "subblocklist", "numsubblocks"))
Output4.extra<-subset(Output4.extra, select=c("docket.number",  "msr.date", "unloading_date", "msr.date.diff", "shell.length", 
                                              "total_landed_weight", "catch", "received_location", "e.processor", "joincode", 
                                              "processor_licence_id", "zone_fishery_code", "blocklist", "numblocks", "subblocklist", "numsubblocks"))
Output5.date<-subset(Output5.date, select=c("docket.number",  "msr.date", "unloading_date", "msr.date.diff", "shell.length", 
                                              "total_landed_weight", "catch", "received_location", "e.processor", "joincode", 
                                              "processor_licence_id", "zone_fishery_code", "blocklist", "numblocks", "subblocklist", "numsubblocks"))
#compile Outputs
compiled.docket.abdb <- rbind(Output4.extra, Output3.multi, Output1.unique, Output2.dupes, Output5.date)

#n-occur to docket.join
test <- unique(compiled.docket.abdb[c("docket.number","e.processor","unloading_date", "zone_fishery_code")])
n_occur <- data.frame(table(test$docket.number))
range(n_occur$Freq)

#select only dupicated dockets from the FeMM.docket.info list
docket.test <- as.data.frame(test[test$docket.number %in% n_occur$Var1[n_occur$Freq > 1],])

###############################################################################################################
#keep(compiled.docket.FeMM, compiled.docket.07.11, compiled.docket.abdb, Output.error.docket, Output.error.epro, Output.error.date, Raw.MM.Output.error.docket, docketinfo, compiled.docket.abdb, sure=T)
rm(list=ls()[! ls() %in% c('compiled.docket.FeMM', 'compiled.docket.07.11', 'compiled.docket.abdb', 'Output.error.docket', 'Output.error.epro', 'Output.error.date', 'Raw.MM.Output.error.docket', 'docketinfo', 'compiled.docket.abdb')])
###############################################################################################################
#
 #
  #
   # 2000-2007 abalone research database ####
  #
 #
#
# docket information held in Abalone research database FactoryEmeasure Query 

## extract FactoryEmeasure data from research Database table FactoryEmeasure
channel <- odbcConnect('App2k')
sql <- "SELECT mm_data_00_07.[CSA_Docket], 
mm_data_00_07.[CSP_Length], 
mm_data_00_07.[CSA_CatchDate], 
mm_data_00_07.[CSA_Block], 
mm_data_00_07.[CSA_SubBlock], 
mm_data_00_07.[COD_Name], 
mm_data_00_07.[CON_Surname], 
mm_data_00_07.[CON_ORGName]
FROM mm_data_00_07;"
facMM <- sqlQuery(channel, sql)
close(channel)

## tidy up some of the column names and variable formats from sql query
colnames(facMM) <- c("docket.number", "shell.length","msr.date", "block.fac", "subblock.fac", "Species", "diver", 
                      "processorname")
facMM$msr.date <- as.Date(facMM$msr.date, format="%d/%m/%Y" )
#facMM$docket.number<-as.numeric(facMM$docket.number)
facMM$subblock.fac <- paste(facMM$block.fac,facMM$subblock.fac, sep="")

## rename processors
facMM$processorname <- as.character(facMM$processorname)
facMM$processorname[facMM$processorname == "AbaloneTasmania"] <- "ABALONE TASMANIA PTY LTD"
facMM$processorname[facMM$processorname == "Tasmanian Seafoods Pty Ltd (Margate)"] <- "TASMANIAN SEAFOODS PTY LTD"
facMM$processorname[facMM$processorname == "Tasmanian Seafoods Pty Ltd (Smithton)"] <- "TASMANIAN SEAFOODS PTY LTD"
facMM$processorname[facMM$processorname == "Ralphs (Electrona)"] <- "RALPH'S TASMANIAN SEAFOOD PTY LTD"
facMM$processorname[facMM$processorname == "Coastal Waters Seafoods"] <- "COASTAL WATERS SEAFOODS PTY LTD"
facMM$processorname[facMM$processorname == "Taslive Abalone"] <- "TAS LIVE ABALONE PTY LTD"
facMM$processorname[facMM$processorname == "Australian Wholesale Seafoods"] <- "AUSTRALIAN WHOLESALE SEAFOODS PTY LTD"
facMM$processorname[facMM$processorname == "Southern Unite Seafoods"] <- "SOUTHERN UNITED SEAFOOD AUSTRALIA PTY LTD"
facMM$processorname[facMM$processorname == "Watken Quality Tasmanian Abalone"] <- "WATKEN QUALITY TASMANIAN ABALONE PTY LTD"
facMM$processorname[facMM$processorname == "Osprey Seafoods Pty Ltd"] <- "OSPREY SEAFOODS PTY LTD"
facMM$processorname[facMM$processorname == "Hai Loong Seafood Export Pty Ltd"] <- "HAI LOONG SEAFOOD EXPORT PTY LTD"
facMM$processorname[facMM$processorname == "Hai Loong Lobster export"] <- "HAI LOONG SEAFOOD EXPORT PTY LTD"
facMM$processorname[facMM$processorname == "Southern Ocean Seafoods"] <- "SOUTHERN OCEAN SEAFOODS (NSW) PTY LTD"
facMM$processorname[facMM$processorname == "Krystal Harbour"] <- "KRYSTAL HARBOUR PTY LTD"
facMM$processorname[facMM$processorname == "A.R.(Tony) Garth Fish Processor Pty Ltd"] <- "A R GARTH FISH PROCESSOR PTY LTD"
facMM$processorname[facMM$processorname == "Pacific Shogi Pty Ltd"] <- "PACIFIC SHOJI PTY LTD"
facMM$processorname[facMM$processorname == "UNKWOWN"] <- NA
facMM$processorname[facMM$processorname == "Seafood Traders Pty Ltd"] <- "SEAFOOD TRADERS PTY LTD"
facMM$processorname<-as.factor(facMM$processorname)

## manual edit two records missing processorname
facMM$processorname[facMM$docket.number %in% c("32740", "32743")] <- "TASMANIAN SEAFOODS PTY LTD"

## summarise the dataframe
facMM.uniq <- ddply(facMM,.(docket.number, processorname), summarize,  n = length(shell.length), 
      meanSL = round(mean(shell.length), 1),
      minSL = round(min(shell.length), 1))

# #test on duplicates
# #n-occur to docket.join
# test<-unique(facMM.uniq[c("docket.number","processorname")])
# n_occur <- data.frame(table(facMM.uniq$docket.number))
# range(n_occur$Freq)

## match docket info to facMM.uniq datframe to test for duplicates
facMM.uniq.di <- join(facMM.uniq, docketinfo, by = c("docket.number", "processorname"), type = "left")

## test on duplicates
#n-occur to docket.join
test <- unique(facMM.uniq.di[c("docket.number","processorname")])
n_occur <- data.frame(table(test$docket.number))
range(n_occur$Freq)

## compile the final dataframe
compiled.docket.00.07 <- join(facMM, facMM.uniq.di, by = c("docket.number","processorname" ), type = "inner")
compiled.docket.00.07$blocklist[is.na(compiled.docket.00.07$blocklist)] <- compiled.docket.00.07$block.fac[is.na(compiled.docket.00.07$blocklist)]
compiled.docket.00.07$subblocklist[is.na(compiled.docket.00.07$subblocklist)] <- compiled.docket.00.07$subblock.fac[is.na(compiled.docket.00.07$subblocklist)]

#added<-anti_join(facMM, compiled.docket.00.07, by=c("docket.number","processorname"))

## add date difference column
compiled.docket.00.07$msr.date.diff<-as.Date(compiled.docket.00.07$msr.date, format="yyyy-%mm-%dd")-as.Date(compiled.docket.00.07$unloading_date, format="yyyy-%mm-%dd")


###################################################################################################################

#keep(compiled.docket.00.07, compiled.docket.FeMM, compiled.docket.07.11, compiled.docket.abdb, Output.error.docket, Output.error.epro, Output.error.date, Raw.MM.Output.error.docket, docketinfo, compiled.docket.abdb, sure=T)
rm(list=ls()[! ls() %in% c('compiled.docket.00.07', 'compiled.docket.FeMM', 'compiled.docket.07.11', 'compiled.docket.abdb', 'Output.error.docket', 'Output.error.epro', 'Output.error.date', 'Raw.MM.Output.error.docket', 'docketinfo', 'compiled.docket.abdb')])

###############################################################################################################
#
 #
  #
   # Compile dataframes ####
  #
 #
#

# names(compiled.docket.FeMM)
# names(compiled.docket.07.11)
# names(compiled.docket.00.07)
# names(compiled.docket.abdb)

## add variable to each dataframe to identify data source
compiled.docket.FeMM$datasource <- "FeMM"
compiled.docket.07.11$datasource <- "RawMM"
compiled.docket.00.07$datasource <- "MM.00.07"
compiled.docket.abdb$datasource <- "abdb"
#
 # STEP 1 - format dataframes to match by column ####
#

## rename some of the coloumn names so dataframes match
names(compiled.docket.07.11)[names(compiled.docket.07.11)=="e.processor"] <- "processorname"
names(compiled.docket.abdb)[names(compiled.docket.abdb)=="e.processor"] <- "processorname"

## subset each dataframe to contain the same variables
cd.FEM <- subset(compiled.docket.FeMM, select=c("docket.number",  "msr.date", "unloading_date", "msr.date.diff", "shell.length", 
                                                           "total_landed_weight", "catch", "received_location", "processorname", "joincode", 
                                                           "processor_licence_id", "zone_fishery_code", "blocklist", "numblocks", "subblocklist", 
                                                           "numsubblocks", "datasource"))
cd.07.11 <- subset(compiled.docket.07.11, select=c("docket.number",  "msr.date", "unloading_date", "msr.date.diff", "shell.length", 
                                                            "total_landed_weight", "catch", "received_location", "processorname", "joincode", 
                                                            "processor_licence_id", "zone_fishery_code", "blocklist", "numblocks", "subblocklist", 
                                                            "numsubblocks", "datasource"))
cd.abdb <- subset(compiled.docket.abdb, select=c("docket.number",  "msr.date", "unloading_date", "msr.date.diff", "shell.length", 
                                                            "total_landed_weight", "catch", "received_location", "processorname", "joincode", 
                                                            "processor_licence_id", "zone_fishery_code", "blocklist", "numblocks", "subblocklist", 
                                                            "numsubblocks", "datasource"))
cd.00.07 <- subset(compiled.docket.00.07, select=c("docket.number",  "msr.date", "unloading_date", "msr.date.diff", "shell.length", 
                                                            "total_landed_weight", "catch", "received_location", "processorname", "joincode", 
                                                            "processor_licence_id", "zone_fishery_code", "blocklist", "numblocks", "subblocklist", 
                                                            "numsubblocks", "datasource"))
#
 # STEP 2 - compile dataframes to produce single dataframe ####
#

## Note: the abdb data has been excluded due to uncertainty in the accuracy of the data)
compiled.df <- rbind( cd.07.11, cd.FEM, cd.00.07)

## re-format the unloading date in compiled.df
compiled.df$unloading_date <- as.Date(compiled.df$unloading_date)

#
 # STEP 3 - remove duplicates of docket numbers
#

## summarize the compiled.df dataset
n.per.docket<-ddply(compiled.df,.(docket.number, unloading_date,  msr.date.diff), summarize,  n = length(shell.length),
                    meanSL = round(mean(shell.length), 1),
                    minSL = round(min(shell.length), 1))

#n.per.docket<-unique(compiled.df[c("docket.number", "datasource")])
#n-occur to docket.join
n_occur <- data.frame(table(n.per.docket$docket.number))
range(n_occur$Freq)

## select unique dockets from the n.per.docket list
docket.uniq <- as.data.frame(n.per.docket[n.per.docket$docket.number %in% n_occur$Var1[n_occur$Freq ==1],])

## join unique FeMM dockets to the identifying information in docket.info from FILMS database
compiled.uniq <- join(compiled.df, docket.uniq,  by = c("docket.number","msr.date.diff", "unloading_date"), type ="inner")

## select unique dockets from the n.per.docket list
docket.dupes <- as.data.frame(n.per.docket[n.per.docket$docket.number %in% n_occur$Var1[n_occur$Freq ==2],])

## loop to keep the duplicate record with the higher n of animals and then initial download date only
db.dup.dockets <- unique(docket.dupes$docket.number)

# b<-174610
# d<-"2000-03-02"

if (exists("pick_db.docket")) 
 rm(pick_db.docket)

for(b in db.dup.dockets){
 choice<-subset(n.per.docket, docket.number == b)
 uniq.date<-unique(choice$unloading_date)
 for(d in uniq.date){
  choice.date<-subset(choice, unloading_date == d)
  maxim<-max(choice.date$n)  
  pick<-subset(choice.date, n == maxim)
  pick.uniq<- pick %>% distinct(docket.number, .keep_all = T)
  if (exists("pick_db.docket"))
  pick_db.docket <- rbind(pick_db.docket, pick.uniq)
 else
  pick_db.docket <- pick.uniq
}
}

## join unique FeMM dockets to the identifying information in docket.info from FILMS database
compiled.dupes <- join(compiled.df, pick_db.docket,  by = c("docket.number","msr.date.diff", "unloading_date"), type ="inner")

## select unique dockets from the n.per.docket list
docket.triples <- as.data.frame(n.per.docket[n.per.docket$docket.number %in% n_occur$Var1[n_occur$Freq >2],])
docket.triples <- droplevels(subset(docket.triples, n >50))
docket.triples <- droplevels(subset(docket.triples, docket.triples$msr.date.diff <50 & docket.triples$msr.date.diff >-20))
db.dup.dockets <- unique(docket.triples$docket.number)

# b<-174610
# d<-"2000-03-02"

if (exists("pick_db.docket")) 
 rm(pick_db.docket)

for(b in db.dup.dockets){
 choice<-subset(docket.triples, docket.number == b)
 uniq.date<-unique(choice$unloading_date)
 for(d in uniq.date){
  choice.date<-subset(choice, unloading_date == d)
  maxim<-max(choice.date$n)  
  pick<-subset(choice.date, n == maxim)
  pick.uniq<- pick %>% distinct(docket.number, .keep_all = T)
  if (exists("pick_db.docket"))
   pick_db.docket <- rbind(pick_db.docket, pick.uniq)
  else
   pick_db.docket <- pick.uniq
 }
}

## join unique FeMM dockets to the identifying information in docket.info from FILMS database####
compiled.triples <- join(compiled.df, pick_db.docket,  by = c("docket.number","msr.date.diff", "unloading_date"), type ="inner")
compiled.df <- rbind(compiled.uniq,compiled.dupes, compiled.triples)


#############################################################################################################

#keep(compiled.df, compiled.docket.00.07, compiled.docket.FeMM, compiled.docket.07.11, compiled.docket.abdb, Output.error.docket, Output.error.epro, Output.error.date, Raw.MM.Output.error.docket, docketinfo, compiled.docket.abdb, sure=T)
rm(list=ls()[! ls() %in% c('compiled.df', 'cd.abdb', 'compiled.docket.00.07', 'compiled.docket.FeMM', 'compiled.docket.07.11', 'compiled.docket.abdb', 'Output.error.docket', 'Output.error.epro', 'Output.error.date', 'Raw.MM.Output.error.docket', 'docketinfo', 'compiled.docket.abdb')])

#############################################################################################################
#
 #
  #
   # Identify dockets from abalone database not in compiled.df ####
  #
 #
#
## a look at the docket composition of the ab-reserach database and the compiled.df from this r script identification of dockets not present in compiled.df

abdb.docket.uniq <- as.data.frame(unique(cd.abdb$docket.number))
colnames(abdb.docket.uniq)[colnames(abdb.docket.uniq)=="unique(cd.abdb$docket.number)"] <- "docket.number"
comp.docket.uniq <- as.data.frame(unique(as.integer(compiled.df$docket.number)))
colnames(comp.docket.uniq)[colnames(comp.docket.uniq)=="unique(as.integer(compiled.df$docket.number))"] <- "docket.number"
doc.distinct <- anti_join(abdb.docket.uniq, comp.docket.uniq) #just keeps x which have no duplicate in y

## subset cd.abdb by docket.numbers already in compiled.df
abdb.distinct <- cd.abdb[cd.abdb$docket.number %in% doc.distinct$docket.number,]

#############################################################################################################

#keep(compiled.df, abdb.distinct, Output.error.docket, Output.error.epro, Output.error.date, Raw.MM.Output.error.docket, docketinfo,  sure=T)
rm(list=ls()[! ls() %in% c('abdb.distinct','abdb.docket.uniq', 'compiled.df', 'cd.abdb', 'compiled.docket.00.07', 'compiled.docket.FeMM', 'compiled.docket.07.11', 'compiled.docket.abdb', 'Output.error.docket', 'Output.error.epro', 'Output.error.date', 'Raw.MM.Output.error.docket', 'docketinfo', 'compiled.docket.abdb')])

#############################################################################################################
#
 #
  #
   # Identify if any error dockects match those from abalone database ####
  #
 #
#
# a look at the error dockets and if any of the dockets unique to abdb.distinct match
colnames(Raw.MM.Output.error.docket)[colnames(Raw.MM.Output.error.docket)=="e.processor"] <- "processorname"
Error.docket.uniq <- as.data.frame(unique(as.integer(Raw.MM.Output.error.docket$docket.number)))
colnames(Error.docket.uniq)[colnames(Error.docket.uniq)=="unique(as.integer(Raw.MM.Output.error.docket$docket.number))"] <- "docket.number"

err.dkt <- ddply(Output.error.docket,.(docket.number, msr.date, processorname), summarize,  n = length(shell.length), 
               meanSL = round(mean(shell.length), 1),
               minSL = round(min(shell.length), 1))

## comparing the two df
err.docket.match <- err.dkt[err.dkt$docket.number %in% abdb.docket.uniq$docket.number,]

##############################################################################################################
#
 #
  # Add zone to compiled.df ####
 #
#

## Make a copy of the compiled.df
ab.newMM <- compiled.df

# The compiled.df contains some records with no docket.numbers but do have measure date, processorname and 
# sub-block information.  These arise from MM.00.07 data and are represented by 'below' and '27/03/' as the
# docket number.  The shell length data can still be used, therefore it was decided to convert the docket
# information to NAs.  Alternatively these can be removed (955 records).

# ab.newMM$docket.number[ab.newMM$docket.number %in% c('below', '27/03/')] <- NA
# remove records if needed
ab.newMM <- subset(ab.newMM, !(docket.number %in% c('below', '27/03/')))
ab.newMM$docket.number <- as.numeric(ab.newMM$docket.number)

## some sub-blocks have an 'N' making it read 'NA', remove N and replace with ''
ab.newMM$subblocklist <- gsub('N', '', ab.newMM$subblocklist)

## remove records with no shell length
ab.newMM<-ab.newMM[!is.na(ab.newMM$shell.length),]

## add column for fishyear
ab.newMM$fishyear <- year(ab.newMM$msr.date)

# there are 79124 records missing zone data to determine species, however
# block information is provided.  Most blocks are southern areas and can be assigned to blacklip
# however there are about 20 dockets from greenlip blocks which need to
# be assigned a species for plotting, etc.  Assuming only legal sized animals have been measured, 
# animals have been assigned a species based on the legal minimum length for species in that block.
# The greenlip blocks are 1-4, 31-49.  An initial examination of summary data
# indicates most of the records in question come from blocks 48 and 49 in the years 2000-2001
# when GL size limit was 140 mm. 

## subset data for animals where zone is missing
ab.newMM.nz <- subset(ab.newMM, is.na(zone_fishery_code))
ab.newMM.z <- subset(ab.newMM, !is.na(zone_fishery_code))

## create summary of min shell length for each docket id
ab.newMM.nz.sum <- ab.newMM.nz %>%
 group_by(docket.number, processorname, blocklist, fishyear) %>%
 summarise(n = length(shell.length), 
           meanSL = round(mean(shell.length), 1),
           minSL = round(min(shell.length), 1)) %>%
 as.data.frame()

## create additional column in summary data to identify species
ab.newMM.nz.sum.sp <- ab.newMM.nz.sum %>%
 mutate(species = ifelse(minSL < 132, 1,
                         ifelse(blocklist %in% c(seq(5, 40, 1)), 1,
                                ifelse(blocklist %in% c(seq(1, 4, 1)) & minSL >= 150, 2, 
                                       ifelse(blocklist %in% c(seq(41, 49, 1)) & fishyear < 2002 & minSL >= 140, 2, 1)))))


codeZoneHistoric <- function(Bl) {
 {
  Bl$newzone[Bl$blocklist %in% seq(13, 31, 1)] <- "E"
  Bl$newzone[Bl$blocklist %in% c(seq(7, 12, 1))] <- "W"
  Bl$newzone[Bl$blocklist %in% c(1, 2, 3, 4, 5, 6, 39, 40)] <- "N"
  Bl$newzone[Bl$blocklist %in% c(seq(32, 38, 1), seq(41, 49, 1), seq(50, 57, 1))] <- "BS"
  Bl$newzone[Bl$species == 2] <- 'G'
  
 }
 return(Bl)
}

ab.newMM.nz.sum.sp.z <- codeZoneHistoric(ab.newMM.nz.sum.sp)

## join the summary data to the original data to identify species for records without zone 
ab.newMM.join <- left_join(ab.newMM.nz, ab.newMM.nz.sum.sp.z, 
                           by = c('docket.number', 'processorname', 'blocklist', 'fishyear'))

## remove previous summary data (.x) as filtering since HJ has removed some additional records since then
ab.newMM.join <- ab.newMM.join %>% select(-c(n.x, meanSL.x, minSL.x))

## keep new summary data (.y) and rename columns to match original data frame
ab.newMM.join <- ab.newMM.join %>% rename(n = n.y, meanSL = meanSL.y, minSL = minSL.y)

## re-join dataframes
ab.newMM.df <- bind_rows(ab.newMM.z, ab.newMM.join)


## identify species for records with 'zone_fishery_code'
ab.newMM.df$species <- ifelse(is.na(ab.newMM.df$zone_fishery_code), ab.newMM.df$species,
                              ifelse(as.character(ab.newMM.df$zone_fishery_code) == 'AQG', 2, 1))

## extract zone for records with 'zone_fishery_code'
ab.newMM.df$newzone <- ifelse(!is.na(ab.newMM.df$zone_fishery_code), substr(ab.newMM.df$zone_fishery_code, 3,
                                                                            nchar(as.character(ab.newMM.df$zone_fishery_code))),
                              ifelse(!is.na(ab.newMM.df$zone_fishery_code) & ab.newMM.df$species == 2, 'G',
                                     ab.newMM.df$newzone))

## split block list into seperate blocks to define regions
ab.newMM.df <- cSplit(ab.newMM.df, 'blocklist', ',', drop = F)

## create column for 'blockno' identfying the first block from the 'blocklist' to enable the Region_Recode 
## function to run
ab.newMM.df$blockno <- ab.newMM.df$blocklist_1

## subset into blacklip and greenlip to define regions
ab.newMM.sub.bl <- subset(ab.newMM.df, species == 1)
ab.newMM.sub.gl <- subset(ab.newMM.df, species == 2)
#ab.newMM.sub.na <- subset(ab.newMM.df, is.na(species))

## code historical data zones and regions
source("C:/GitCode/AbHarvestStrategy/Region_Recode2018.r")

## code regions for blacklip and greenlip
ab.newMM.bl <- codeBlregion(ab.newMM.sub.bl)
ab.newMM.gl <- codeGlregion(ab.newMM.sub.gl)

## re-join blacklip and greenlip df
ab.newMM.df <- bind_rows(ab.newMM.bl, ab.newMM.gl)

##############################################################################################################
#
 #
  # 1960-2000 abalone data from CSIRO ####
 #
#
# These data contain records collected by CSIRO between 1967 and 2000. No original raw data files can
# be found; the only data record being an Excel file created by DT containing what appears to be a 
# compilation of those records which have been transferred to the abalone database. These data are
# added to the compiled.df

channel <- odbcConnect('Asd_data')
sql <- "SELECT * 
FROM MMCommercialSamplesQuery;"
ab.oldMM <- sqlQuery(channel, sql)
close(channel)

## save a copy of raw ab.oldMM.R data as a backup prior to cleaning
saveRDS(ab.oldMM, 'C:/CloudStor/R_Stuff/MMLF/ab.oldMM_raw.RDS')
#write.csv(ab.oldMM, 'ab.oldMM.csv')

## subset historic data to include only the required variables 
ab.oldMM.sub <- subset(ab.oldMM, select = c("CSA_Docket", "CSA_Processor", "CSA_SampleDate", "CSA_CatchDate",
                                            "CSA_Species", "CSA_Block", "CSA_SubBlock", "CSA_CatchLocation", "CSA_CatchWeight",
                                            "CSP_Length", "CommercialSamples_CSA_ID"))

## rename most variables to match more recent data
colnames(ab.oldMM.sub) <- c("docket.number", "processor_licence_id", "msr.date", "unloading_date", 
                            'species', "blocklist", "subblocklist", 'catchlocation', "total_landed_weight",
                            "shell.length", 'sampleid')

## remove records without shell length
ab.oldMM.sub <- ab.oldMM.sub[!is.na(ab.oldMM.sub$shell.length),]

## search for duplicate records
n.per.sample.id <- ab.oldMM.sub %>%
 group_by(sampleid, msr.date) %>%
 summarise(n = length(shell.length), 
           meanSL = round(mean(shell.length), 1),
           minSL = round(min(shell.length), 1))

db.dup.sample.ids <- unique(n.per.sample.id$sampleid)

if (exists("pick_db.sample.id")) 
 rm(pick_db.sample.id)

for(b in db.dup.sample.ids){
 choice<-subset(n.per.sample.id, sampleid == b)
 maxim<-max(choice$n)  
 pick<-subset(choice, n == maxim)
 minin<-min(pick$msr.date)
 pick<-subset(pick, msr.date == minin)
 if (exists("pick_db.sample.id"))
  pick_db.sample.id <- rbind(pick_db.sample.id, pick)
 else
  pick_db.sample.id <- pick
}

n_occur <- data.frame(table(pick_db.sample.id$sampleid))
range(n_occur$Freq)

# NO duplicates were found using above code

## add a column 'datasource' to distinguish historic from more recent data
ab.oldMM.sub$datasource <- 'MM.pre00'

## add columns to match 'compiled.df' and populate with NA's
ab.oldMM.sub.var <- c('catch', 'received_location', 'processorname', 'joincode', 'zone_fishery_code',
                      'numblocks', 'numsubblocks', 'msr.date.diff')
ab.oldMM.sub[ab.oldMM.sub.var] <- NA

## populate additional columns where possible
ab.oldMM.sub$msr.date.diff <- ymd(ab.oldMM.sub$msr.date) - ymd(ab.oldMM.sub$unloading_date)
ab.oldMM.sub$numblocks <- count.fields(textConnection(as.character(ab.oldMM.sub$blocklist)), sep = ',')
ab.oldMM.sub$numsubblocks <- nchar(as.character(ab.oldMM.sub$subblocklist))

## summarise data for number of samples, mean and min shell length and add to dataframe
ab.oldMM.sub <- ab.oldMM.sub %>% 
 group_by(sampleid) %>%
 summarise(n = length(shell.length),
           meanSL = round(mean(shell.length, na.rm = T), 1),
           minSL = round(min(shell.length), 1)) %>%
 inner_join(ab.oldMM.sub, 'sampleid')

## re-order columns
ab.oldMM.sub <- ab.oldMM.sub[,c("sampleid", "species", "catchlocation", "docket.number", "msr.date", "unloading_date", "msr.date.diff", "shell.length",
                                "total_landed_weight", "catch", "received_location", "processorname", "joincode",
                                "processor_licence_id", "zone_fishery_code", "blocklist", "numblocks", "subblocklist",
                                "numsubblocks", "datasource", "n", "meanSL", "minSL")]

## code historical data zones and regions
source("C:/GitCode/AbHarvestStrategy/Region_Recode2018.r")

## rename 'blocklist' to 'blockno' to enable region recode function to run
ab.oldMM.sub <- rename(ab.oldMM.sub, blockno = blocklist)

## code zones for black and greenlip
ab.oldMM.sub <- codeBlZoneHistoric(ab.oldMM.sub) 
ab.oldMM.sub <- ab.oldMM.sub %>%
 mutate(newzone = replace(newzone, species == 2, 'G'))

## subset ab.oldMM.sub into blacklip and greenlip to define regions
ab.oldMM.sub.bl <- subset(ab.oldMM.sub, species == 1)
ab.oldMM.sub.gl <- subset(ab.oldMM.sub, species == 2)

## Code regions for blacklip and greenlip
ab.oldMM.sub.bl <- codeBlRegionHistoric(ab.oldMM.sub.bl)
ab.oldMM.sub.gl <- codeGlRegionHistoric(ab.oldMM.sub.gl)

## re-join blacklip and greenlip df
ab.oldMM.df <- bind_rows(ab.oldMM.sub.bl, ab.oldMM.sub.gl)
#write.csv(ab.oldMM.df, 'ab.oldMM.df.csv')

##############################################################################################################
#
 #
  # Post 2016 MM abalone data from Gary Carlos ####
 #
#
## Since to 2010 the market measuring program for abalone has been sporadic and there are currently
# only samples available from a single processor in 2016 held in a csv file.

## import data from .csv file
ab.2016MM.raw <- read.csv('R:/TAFI/TAFI_MRL_Sections/Wild_Fisheries_Program/Shared/13. Market measuring/DataSummary_from_1Jan2016_to_31Dec2016.csv')

## create a backup
ab.2016MM.backup <- ab.2016MM.raw
#ab.2016MM <- ab.2016MM.backup

## remove unesseary varibales (zone, entitlement, order)
#ab.2016MM <- ab.2016MM[,-c(3, 4, 6)]
ab.2016MM <- subset(ab.2016MM.raw, select = c('DataSource', 'DownloadDate', 'Docket', 'Size', 'MesrTime'))

## re-format some of the data and variable names
colnames(ab.2016MM) <- tolower(colnames(ab.2016MM))
ab.2016MM$mesrtime <- dmy_hms(ab.2016MM$mesrtime)
ab.2016MM$msr.date <- date(ab.2016MM$mesrtime)
ab.2016MM$downloaddate <- dmy_hms(ab.2016MM$downloaddate)
ab.2016MM$downloaddate <- date(ab.2016MM$downloaddate)

## remove measure time
ab.2016MM <- ab.2016MM[,-c(5)]

## re-label column names to match other datframes
colnames(ab.2016MM) <- c('e.processor','download.date', 'docket.number', 'shell.length', 'msr.date') 

## add variable to distinguish datasource
ab.2016MM$datasource <- '2016MM.csv'

## remove records without docket number or docket = 0 or shell length = NA
ab.2016MM.Output.error.docket <- ab.2016MM[is.na(ab.2016MM$docket.number),]
ab.2016MM <- ab.2016MM[!is.na(ab.2016MM$docket.number),]
ab.2016MM <- ab.2016MM[!is.na(ab.2016MM$shell.length),]
ab.2016MM <- droplevels(subset(ab.2016MM, docket.number !=0))

## rename processors
ab.2016MM$e.processor <- as.character(ab.2016MM$e.processor)
ab.2016MM$e.processor[ab.2016MM$e.processor == "AbaloneTasmania"] <- "ABALONE TASMANIA PTY LTD"
ab.2016MM$e.processor[ab.2016MM$e.processor == "Tas Seafoods Margate"] <- "TASMANIAN SEAFOODS PTY LTD"
ab.2016MM$e.processor[ab.2016MM$e.processor == "Tasmanian Seafoods at Margate"] <- "TASMANIAN SEAFOODS PTY LTD"
ab.2016MM$e.processor[ab.2016MM$e.processor == "Tas Seafoods Smithton"] <- "TASMANIAN SEAFOODS PTY LTD"
ab.2016MM$e.processor[ab.2016MM$e.processor == "Ralphs"] <- "RALPH'S TASMANIAN SEAFOOD PTY LTD"
ab.2016MM$e.processor[ab.2016MM$e.processor == "Coastal Waters"] <- "COASTAL WATERS SEAFOODS PTY LTD"
ab.2016MM$e.processor[ab.2016MM$e.processor == "Tas Live Abalone"] <- "TAS LIVE ABALONE PTY LTD"
ab.2016MM$e.processor[ab.2016MM$e.processor == "MK Haulage"] <- "M & K HAULAGE (TAS) PTY LTD"
ab.2016MM$e.processor<-as.factor(ab.2016MM$e.processor)

## create dataframe of e.processors from data
eproname <- as.data.frame(unique(ab.2016MM$e.processor))
colnames(eproname) <- c("e.processor")
e.processors <- eproname$e.processor

docketinfo.epro <- droplevels(subset(docketinfo, processorname %in% e.processors))
docketinfo.epro$e.processor <- docketinfo.epro$processorname
#colnames(docketinfo.epro)[colnames(docketinfo.epro)=="processorname"] <- "e.processor"

## summarise data for number of samples, mean and min shell length and add to dataframe 
#  to check for duplicates
n.per.docket <- ab.2016MM %>% 
 group_by(docket.number, msr.date, e.processor) %>%
 summarise(n = length(shell.length),
           meanSL = round(mean(shell.length, na.rm = T), 1),
           minSL = round(min(shell.length), 1))

## match docketinfo.epro to the n.per.docket dataframe
docket.join <- inner_join(n.per.docket, docketinfo.epro, by = c("docket.number", "e.processor"))

## add date difference column
docket.join$msr.date.diff <- as.Date(docket.join$msr.date, format="yyyy-%mm-%dd") - as.Date(docket.join$unloading_date, format="yyyy-%mm-%dd")

## check for and seperate out dupilicated dockets
#  how many times each docket.number occur
n_occur <- data.frame(table(docket.join$docket.number))
range(n_occur$Freq)

docket.uniq <- as.data.frame(docket.join[docket.join$docket.number %in% n_occur$Var1[n_occur$Freq == 1],])

## check
n_occur <- data.frame(table(docket.uniq$docket.number))
range(n_occur$Freq)

## join unique dockets to ab.2016MM
ab.2016MM.unique <- inner_join(ab.2016MM, docket.uniq)


## subset data and filter out uneeded or duplicated variables
ab.2016MM.df <- subset(ab.2016MM.unique, select=c("docket.number",  "datasource", "msr.date", "unloading_date", "msr.date.diff", "shell.length", 
                                           "total_landed_weight", "catch", "received_location", "processorname", "joincode", 
                                           "processor_licence_id", "zone_fishery_code", "blocklist", "numblocks", "subblocklist", "numsubblocks"))

## identify abalone species based on zone variable
ab.2016MM.df$species <- ifelse(is.na(ab.2016MM.df$zone_fishery_code == T), 1,
                               ifelse(ab.2016MM.df$zone_fishery_code == 'AQG', 2, 1))

## create 'newzone' variable to match historical data and extract zone from 'zone_fishery_code'
ab.2016MM.df$newzone <- substr(ab.2016MM.df$zone_fishery_code, 3, nchar(as.character(ab.2016MM.df$zone_fishery_code)))

## split block list into seperate blocks to define regions
ab.2016MM.df <- cSplit(ab.2016MM.df, 'blocklist', ',', drop = F)

## create column for 'blockno' identfying the first block from the 'blocklist' to enable the Region_Recode 
#  function to run
ab.2016MM.df$blockno <- ab.2016MM.df$blocklist_1

## subset into blacklip and greenlip to define regions
ab.2016MM.df.sub.bl <- subset(ab.2016MM.df, species == 1)
ab.2016MM.df.sub.gl <- subset(ab.2016MM.df, species == 2)

## code regions for blacklip and greenlip
ab.2016MM.df.sub.bl <- codeBlregion(ab.2016MM.df.sub.bl)

## re-join blacklip and greenlip data to create final dataframe
ab.2016MM.df <- bind_rows(ab.2016MM.df.sub.bl, ab.2016MM.df.sub.gl)

##############################################################################################################
#
 #
  # Join recent and historical data together to produce final data frame ####
 #
#
# convert 'unloading_date' to POSIXct to allow bind_rows function to work
ab.newMM.df$unloading_date <- as.POSIXct(ab.newMM.df$unloading_date, format = '%Y-%m-%d')
ab.2016MM.df$msr.date <- as.POSIXct(ab.2016MM.df$msr.date, format = '%Y-%m-%d')

# join old and new market measure data frames together
compiledMM.df <- bind_rows(ab.oldMM.df, ab.newMM.df)
compiledMM.df$docket.number <- as.integer(compiledMM.df$docket.number)

# join compiledMM.df to most recent data
ab.2016MM.df$msr.date <- as.POSIXct(ab.2016MM.df$msr.date, format = '%Y-%m-%d')
compiledMM.df <- bind_rows(compiledMM.df, ab.2016MM.df)

## save a copy of the R file
saveRDS(compiledMM.df, 'C:/CloudStor/R_Stuff/MMLF/compiledMM.df.RDS')

##############################################################################################################
#
 #
  # Data analysis and plots of final data frame ####
 #
#
#compiledMM.df <- readRDS('C:/CloudStor/R_Stuff/MMLF/compiledMM.df.RDS')

## add column for fishing year, month and quarter
compiledMM.df$fishyear <- ifelse(is.na(compiledMM.df$msr.date), year(compiledMM.df$unloading_date),
                                 year(compiledMM.df$msr.date))
compiledMM.df$fishmonth <- month(compiledMM.df$msr.date)
compiledMM.df$fishquarter <- quarter(compiledMM.df$msr.date)

## extract plot data for boxplots
plotdat <-
 compiledMM.df %>% filter(newzone == 'E' & (between(shell.length, 132, 200))
                          & (between(fishyear, 2000, 2016)))
plotdat <-
 compiledMM.df %>% filter(newzone == 'E')

## convert required grouping variable to factor for boxplot
plotdat$fishyear <- as.factor(plotdat$fishyear)
#plotdat$fishquarter <- as.factor(plotdat$fishquarter)

## generate a count of records for each year to add to boxplot
df <- plotdat %>% group_by(fishyear) %>% summarize(n = n())

## generate boxplot of shell lengths for chosen grouping variable above.
ggplot(plotdat, aes(x = fishyear, y = shell.length)) + 
 geom_boxplot(outlier.colour = "orange", outlier.size = 1.5) +
 geom_text(data = df, aes(y = 260, label = n,), size = 3, angle = 90) +
 geom_hline(aes(yintercept = 138), colour = 'red', linetype = 'dotted') +
 ggtitle('Western Zone 1967-2016') +
 xlab('Year') +
 ylab('Shell length (mm)')+ 
 theme_bw() + 
 theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(), panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
       axis.text.x = element_text(angle = 45, vjust = 0.5))

## loop to generate boxplot of shell lengths for each zone and export as .pdf
zones <- unique(compiledMM.df$newzone)
compiledMM.df$fishyear <- as.factor(compiledMM.df$fishyear)

for (i in zones){
 zone.plot <- ggplot(subset(compiledMM.df, newzone == i & (between(shell.length, 100, 250))),
                     aes(x = fishyear, y = shell.length, group = fishyear)) +
  geom_boxplot(outlier.colour = "orange", outlier.size = 1.5) +
  #geom_hline(aes(yintercept = 140), colour = 'red', linetype = 'dotted') +
  #ggtitle(ifelse(as.character(i) == 'W', 'Western Zone', i)) +
  geom_text(aes(label = ..count..), y = 240, stat = 'count',  size = 3, angle = 90) +
  # ggtitle(i) +
  xlab('Year') +
  ylab('Shell length (mm)') + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, vjust = 0.5))
 ggsave(filename = paste('MM_1967-2016_boxplot_', i, '.pdf', sep = ''), plot = zone.plot)
 dev.off()
}

## loop to generate boxplot of black lip shell lengths for each block and export as .pdf
blocks <- unique(compiledMM.df$blockno)
compiledMM.df$fishyear <- as.factor(compiledMM.df$fishyear)

for (i in blocks){
 block.plot <- ggplot(subset(compiledMM.df, blockno == i & (between(shell.length, 100, 250)) & species == 1),
                     aes(x = fishyear, y = shell.length, group = fishyear)) +
  geom_boxplot(outlier.colour = "orange", outlier.size = 1.5) +
  geom_text(aes(label = ..count..), y = 240, stat = 'count',  size = 3, angle = 90) +
  ggtitle(paste('Block ', i)) +
  xlab('Year') +
  ylab('Shell length (mm)') + 
  ylim(100, 250) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, vjust = 0.5))
 ggsave(filename = paste('MM_1967-2016_boxplot_BL_Block ', i, '.pdf', sep = ''), plot = block.plot)
 dev.off()
}

## loop to generate boxplot of green lip shell lengths for each block and export as .pdf
blocks <- unique(compiledMM.df$blockno)
compiledMM.df$fishyear <- as.factor(compiledMM.df$fishyear)

for (i in blocks){
 block.plot <- ggplot(subset(compiledMM.df, blockno == i & (between(shell.length, 100, 250)) & species == 2),
                      aes(x = fishyear, y = shell.length, group = fishyear)) +
  geom_boxplot(outlier.colour = "orange", outlier.size = 1.5) +
  geom_text(aes(label = ..count..), y = 240, stat = 'count',  size = 3, angle = 90) +
  ggtitle(paste('Block ', i)) +
  xlab('Year') +
  ylab('Shell length (mm)') + 
  ylim(100, 250) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, vjust = 0.5))
 ggsave(filename = paste('MM_1967-2016_boxplot_GL_Block ', i, '.pdf', sep = ''), plot = block.plot)
 dev.off()
}

## size frequency plots for zone and block every 5 years
plotdat.2 <- compiledMM.df %>% filter(newzone == 'E' & fishyear == c(2000, 2005, 2010, 2015))

ggplot(plotdat.2, aes(shell.length))+
 geom_histogram(data = subset(plotdat.2, newzone == 'W'), fill = 'white', colour = 'black',  binwidth = 5)+
 geom_histogram(data = subset(plotdat.2, blockno == 11), fill = 'black', binwidth = 5)+
 theme_bw()+
 ylab("Frequency") +
 xlab("Shell Length (mm)")+
 xlim(100, 220)+
 facet_grid(fishyear ~ ., scales = "free_y")+
 #add size limits for each time period
 #geom_vline(data = filter(plotdat.2, fishyear == 2000), aes(xintercept = 132),colour = 'red', linetype = 'dashed', size = 1)+
 #geom_vline(data = filter(plotdat.2, fishyear == 2005), aes(xintercept = 136),colour = 'red', linetype = 'dashed', size = 1)+
 #geom_vline(data = filter(plotdat.2, fishyear == 2010), aes(xintercept = 138),colour = 'red', linetype = 'dashed', size = 1)+
 #geom_vline(data = filter(plotdat.2, fishyear == 2015), aes(xintercept = 138),colour = 'red', linetype = 'dashed', size = 1)
 geom_vline(aes(xintercept = 140),colour = 'red', linetype = 'dashed', size = 1)


## summary to determine % of catches measured
landings <- tasCE %>% 
 filter(bl.zone == 'W' & fishyear >= 2000) %>%
 group_by(fishyear, blockno) %>%
 summarise(landings = n())

measured <- compiledMM.df %>%
 mutate(fishyear = as.numeric(levels(fishyear)[fishyear])) %>%
 filter(newzone == 'W') %>%
 group_by(fishyear, blockno) %>%
 summarise(measured = length(unique(docket.number)))

measured_summary <- left_join(landings, measured, c("fishyear", "blockno")) %>%
 mutate(meas.propland = round((measured/landings)*100, 0)) %>%
 filter(blockno == 13) %>%
 filter(fishyear %in% c(2000, 2005, 2010, 2015))

## boxplot of size structure for block vs overall for year(need to overlay bar plot of %landings measured)
plotdat.3a <- compiledMM.df %>% 
 filter(blockno == 13 & fishyear == c(2000, 2005, 2010, 2015)) %>%
 mutate(join.code = 'block.13') %>%
 select(fishyear, join.code, shell.length)

plotdat.3b <- compiledMM.df %>% 
 filter(newzone == 'E' & fishyear == c(2000, 2005, 2010, 2015)) %>%
 mutate(join.code = 'eastern.zone') %>%
 select(fishyear, join.code, shell.length)

plotdat.4 <- bind_rows(plotdat.3a, plotdat.3b)

ggplot(plotdat.4, aes(x = fishyear, y = shell.length)) + 
 geom_boxplot(aes(fill = join.code), outlier.colour = "orange", outlier.size = 1.5, 
              width = .6)+
 theme_bw()+
 ylim(125, 220)+
 ylab("Shell length (mm)") +
 xlab("Year")
 geom_bar(data = measured_summary, aes(x = fishyear, y = meas.propland), stat = 'identity')+
 scale_y_continuous(sec.axis = sec_axis(~./50))
 
 
ggplot(plotdat.4, aes(x = fishyear, y = shell.length)) +
  + geom_point()+
  + stat_summary(fun.data = my.stderr, geom = 'point', colour = 'red')

       
 


