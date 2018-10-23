####MARKET MEASURE ANALYSIS######################

# for windows 8 database connections use this in windows explorer
#C:\Windows\SysWOW64\odbcad32.exe


library(RODBC)
library(R.utils)
library(lubridate)
library(plyr)
library(rgdal)
library(sp)
library(maptools)
library(tools)
library(openxlsx)
library(lubridate)
library(gdata)
library(dplyr)

# Pull in newBlackCE dataframe from the catcheffort data output
setwd('c:/CloudStor/R_Stuff/AutoAssess')
## Manually choose .rdata file
myFile <- file.choose()
load(myFile)
getwd()

#load("Ab_processor220616.RData")
load('AbProcessors_2018_10_09.RData')

keep(processorlist, docketinfo, sure=T)

#AbMM data loaded from AbProcessorDetails.R, rename df and change column names out of SQL format ###
summary(docketinfo)
names(docketinfo)
names(docketinfo)[names(docketinfo)=="docket_number"] <- "docket.number"
#   
 #  
  #                                         
   ####POST 2010 MM data held in Gary Carlos db.
  #
 # 
#  
#extract data from gary Database table data from 2010 (part) to 2015 

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

#get download date from database
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

e.processors<-c("RALPH'S TASMANIAN SEAFOOD PTY LTD","ABALONE TASMANIA PTY LTD", "TASMANIAN SEAFOODS PTY LTD", "TASMANIAN SEAFOODS PTY LTD", "ADELAIDE BAY SEAFOODS PTY LTD")
e.pro<-as.data.frame(e.processors)
e.pro$DataSourceID<-c(1,2,3,4,5)
Download.date<-join(Download.date, e.pro, by = "DataSourceID", type ="left")

#join FeMM and Download.date
FeMM<-join(FeMM, Download.date, by = "DownlaodEventID", type ="left")
#rename columns out of SQL format
colnames(FeMM) <- c("download.event", "measure.id", "Download.file", "record.number", "Zone", "entitlement.number", "docket.number","shell.length",
                    "msr.date", "msr.time", "dload.date", "datasource", "eprocessors") #, "msr.time","fishdate", "year")
#
 #
  #STEP 1 remove records without docket.number and missing shell.length.
 #
#
#remove records without docket number or shell length
FeMM<-FeMM[!is.na(FeMM$docket.number),]
FeMM<-FeMM[!is.na(FeMM$shell.length),]
FeMM<-droplevels(subset(FeMM, docket.number !=0))

#  PROBLEM = multiple downloads of same data into gary database gives duplicate data
 #
  #STEP 2 -  remove duplicate records by selecting for matchings docket.numbers and first selecting docket with
  #          max number of animals and then if still multiple by first download date.
 #
#
n.per.docket<-ddply(FeMM,.(docket.number, dload.date, eprocessors), summarize,  n = length(shell.length), 
                                                                                meanSL = round(mean(shell.length), 1),
                                                                                minSL = round(min(shell.length), 1))

#loop to keep the duplicate record with the higher n of animals and then initial download date only
db.dup.dockets<-unique(n.per.docket$docket.number)

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
#CHECK to ensure each docket.number is now unique 
n_occur <- data.frame(table(pick_db.docket$docket.number))
range(n_occur$Freq)

#subset db based on loop above
FeMM.sub<-join(FeMM, pick_db.docket, by = c("docket.number", "dload.date"), type = "inner")

#
 #
  #STEP 3 subset docketinfo to >2010, and by processors that have e-boards
 #
#

#FeMM database only has data from 2010 onwards so to reduce chances of duplicate records subset docketinfo to post 2009
docketinfo.2010<-droplevels(subset(docketinfo,unloading_date >= as.POSIXct('2010-01-01 00:00')))

e.processors<-c("RALPH'S TASMANIAN SEAFOOD PTY LTD","ABALONE TASMANIA PTY LTD", "TASMANIAN SEAFOODS PTY LTD", "ADELAIDE BAY SEAFOODS PTY LTD", "M & K HAULAGE (TAS) PTY LTD")
docketinfo.2010<-droplevels(subset(docketinfo.2010, processorname %in% e.processors))
#
 #
  #STEP 4 join unique database docket to docketinfo.2010
 #
#

#join unique FeMM dockets to the identifying information in docket.info from FILMS database
FeMM.docket.info<-join(pick_db.docket, docketinfo.2010, by = "docket.number", type ="left")

#separate dupilicated dockets
#how many times does each docket.number occur
n_occur <- data.frame(table(FeMM.docket.info$docket.number))
range(n_occur$Freq)

#
 #
  #STEP 5 extract unique dockets and join to FeMMsub
 #
#
#docket.uniq is all dockets without duplicates and can then be joined back to the original FeMMsub data
docket.uniq<-as.data.frame(FeMM.docket.info[FeMM.docket.info$docket.number %in% n_occur$Var1[n_occur$Freq == 1],])

  #
#OUTPUT 1 = create new dataframe joining the FeMMsub to docket.uniq only keeping common docket data to both
  #
Output1.unique<-join(FeMM.sub, docket.uniq, by = "docket.number", type = "inner")

#add columns to show date differences between ladning date and msr.Date and dload.Date
Output1.unique$dload.date<-as.Date(Output1.unique$dload.date, format="yyyy-%mm-%dd")
Output1.unique$unloading_date<-as.Date(Output1.unique$unloading_date, format="yyyy-%mm-%dd")
Output1.unique$msr.date.diff<-as.Date(Output1.unique$msr.date, format="yyyy-%mm-%dd")-as.Date(Output1.unique$unloading_date, format="yyyy-%mm-%dd")
Output1.unique$dload.date.diff<-as.Date(Output1.unique$dload.date, format="yyyy-%mm-%dd")-as.Date(Output1.unique$unloading_date, format="yyyy-%mm-%dd")

#
 #  REMOVE dockets with errors in dates and or processor
#
#Dockets with issues in that download.date is prior to landing date but there are no duplicated dockets suggestion is incorrect docket number in eboard.
dload.date.false<-subset(Output1.unique, dload.date.diff <=-1)
dload.date.f.unique<-unique(dload.date.false[c("docket.number", "eprocessors", "dload.date", "unloading_date",
                                               "dload.date.diff", "msr.date.diff")])
Output.error.date<-join(dload.date.f.unique, docketinfo.2010,  by = "docket.number", type ="left")

#Dockets with issues in that eprocessor is not processorID
epro.proID.false<-as.data.frame(Output1.unique[Output1.unique$eprocessors != Output1.unique$processorname,])
epro.proID.false<-epro.proID.false[!is.na(epro.proID.false$docket.number),]
epro.proID.f.unique<-unique(epro.proID.false[c("docket.number", "eprocessors", "dload.date", "unloading_date","msr.date",
                                               "dload.date.diff", "msr.date.diff")])
Output.error.epro<-join(epro.proID.f.unique, docketinfo.2010,  by = "docket.number", type ="left")

#remove the dockets with issues
Output1.unique<-Output1.unique[!(Output1.unique$docket.number %in% dload.date.f.unique$docket.number),]
Output1.unique<-Output1.unique[!(Output1.unique$docket.number %in% epro.proID.false$docket.number),]

#
 #
  #STEP 6 DUPLICATED DOCKETS remove split catches by matching eprocessor (garydb) to processorID (films) in dupliacted catches.
 #
#
#select only dupicated dockets from the FeMM.docket.info list
docket.dupes<-as.data.frame(FeMM.docket.info[FeMM.docket.info$docket.number %in% n_occur$Var1[n_occur$Freq > 1],])

docket.epro<-subset(docket.dupes, (docket.dupes$eprocessors == docket.dupes$processorname))

n_occur <- data.frame(table(docket.epro$docket.number))
range(n_occur$Freq)

#
 #
  #STEP 7 extract unique dockets and join to FeMMsub
 #
#
#docket.uniq is all dockets without duplicates and can then be joined back to the original FeMMsub data
docket.epro.uniq<-as.data.frame(docket.epro[docket.epro$docket.number %in% n_occur$Var1[n_occur$Freq == 1],])
 #
#OUTPUT 2 = create new dataframe joining the FeMMsub to docket.epro.uniq only keeping common docket data to both
 #
Output2.epro<-join(FeMM.sub, docket.epro.uniq, by = "docket.number", type = "inner")

#add columns to show date differences between ladning date and msr.Date and dload.Date
Output2.epro$dload.date<-as.Date(Output2.epro$dload.date, format="yyyy-%mm-%dd")
Output2.epro$unloading_date<-as.Date(Output2.epro$unloading_date, format="yyyy-%mm-%dd")
Output2.epro$msr.date.diff<-as.Date(Output2.epro$msr.date, format="yyyy-%mm-%dd")-as.Date(Output2.epro$unloading_date, format="yyyy-%mm-%dd")
Output2.epro$dload.date.diff<-as.Date(Output2.epro$dload.date, format="yyyy-%mm-%dd")-as.Date(Output2.epro$unloading_date, format="yyyy-%mm-%dd")
#
 #
  #STEP 8 extract unique dockets and join to FeMMsub
 #
#
docket.dble<-as.data.frame(docket.epro[docket.epro$docket.number %in% n_occur$Var1[n_occur$Freq > 1],])

#look at unique dates in dupes
docket.dble$dload.date.diff<-as.Date(docket.dble$dload.date, format="yyyy-%mm-%dd")-as.Date(docket.dble$unloading_date, format="yyyy-%mm-%dd")
docket.dble<-subset(docket.dble, dload.date.diff >=-1)

d.docket<-unique(docket.dble$docket.number)

#loop to keep the duplicate record with the smallest date.diff between landing date and dload.date
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

#A check on how many times does each docket.number occur
n_occur <- data.frame(table(pick.dble.docket$docket.number))
range(n_occur$Freq)

#
 #OUTPUT 3 = create new dataframe joining the FeMM to docket.dupes.uniq only keeping common docket data to both
#
Output3.dble<-join(FeMM.sub, pick.dble.docket, by = "docket.number", type = "inner")

#add columns to show date differences between ladning date and msr.Date and dload.Date
Output3.dble$dload.date<-as.Date(Output3.dble$dload.date, format="yyyy-%mm-%dd")
Output3.dble$unloading_date<-as.Date(Output3.dble$unloading_date, format="yyyy-%mm-%dd")
Output3.dble$msr.date.diff<-as.Date(Output3.dble$msr.date, format="yyyy-%mm-%dd")-as.Date(Output3.dble$unloading_date, format="yyyy-%mm-%dd")
Output3.dble$dload.date.diff<-as.Date(Output3.dble$dload.date, format="yyyy-%mm-%dd")-as.Date(Output3.dble$unloading_date, format="yyyy-%mm-%dd")

#
 #
  #STEP 9 - Compile the final dataframe
 #
#

#add column to identify which output each docket is derived from
Output1.unique$output<-1
Output2.epro$output<-2
Output3.dble$output<-3

compiled.docket.FeMM<-rbind(Output1.unique, Output3.dble, Output2.epro)



#reduce column number
compiled.docket.FeMM<-droplevels(subset(compiled.docket.FeMM, select = c("output", "joincode", "docket.number", "shell.length","meanSL", "minSL", "n","total_landed_weight", "catch", "msr.date", "dload.date", "unloading_date",
                                                               "dload.date.diff", "msr.date.diff", "eprocessors", "processorname", "processor_licence_id", "zone_fishery_code", "Zone", "received_location", 
                                                              "blocklist", "numblocks", "subblocklist", "numsubblocks", "Download.file")))

#subset records with bad docket.numbers e.g. missing all matching info from FILMS
Output.error.docket<-compiled.docket.FeMM[is.na(compiled.docket.FeMM$processorname),]

compiled.docket.FeMM<-compiled.docket.FeMM[!(compiled.docket.FeMM$docket.number %in% Output.error.docket$docket.number),]



#THERE ARE 4 OUT DATAFRAMES FROM THIS WORK
summary(compiled.docket.FeMM) # is the complete MM data with attached docket info
summary(Output.error.date) # is records with errors with dates dload.date preceeds landing date but there is no duplicate records which match
summary(Output.error.epro) # is records with non-matching processors from FILMS database and the eboard from which data was collected.
summary(Output.error.docket) # is records with non-matching docket.numbers from FILMS database.

#in all cases of the error files i suspect there has been a mistake in the factory of docket.number entered.
###############################################################################################################

keep(compiled.docket.FeMM, Output.error.docket, Output.error.epro, Output.error.date, docketinfo, sure=T)


###############################################################################################################
#
#
#

#      allocation of docket information to raw data held in market measuring folder for dates 2007-2011

#
#
#

setwd("R:/TAFI/TAFI_MRL_Sections/Wild_Fisheries_Program/Shared/13. Market measuring/MM_2007_2010_by_Processor")

#loop to search for summary files
MM_files<-c()
#change the grep to search based on folder names
for (dir_finals in grep('a',list.files(path='.',all.files=FALSE,full.names=TRUE),value=TRUE))
{
 MM_files<-c(MM_files,grep('MM_RawData_',list.files(path = dir_finals, all.files = FALSE, full.names = TRUE, recursive = TRUE),value=TRUE) )
}
all_data<-NULL
#combine files
Raw_MM<-do.call(rbind,lapply(MM_files,read.csv))
summary(Raw_MM)

colnames(Raw_MM) <- c("x","sample.number", "docket.number","shell.length","time","msr.date", "filesource","e.processor")
Raw_MM$msr.date <- strptime(as.character(Raw_MM$msr.date), "%d/%m/%Y")
Raw_MM$msr.date<-as.POSIXct(Raw_MM$msr.date)
Raw_MM$docket.number<-as.integer(Raw_MM$docket.number)
Raw_MM$shell.length<-as.integer(Raw_MM$shell.length)

#remove records without docket number or docket = 0 or shell length = NA
Raw.MM.Output.error.docket<-Raw_MM[is.na(Raw_MM$docket.number),]

Raw_MM<-Raw_MM[!is.na(Raw_MM$docket.number),]
Raw_MM<-Raw_MM[!is.na(Raw_MM$shell.length),]
Raw_MM<-droplevels(subset(Raw_MM, docket.number !=0))

#rename processors
Raw_MM$e.processor <- as.character(Raw_MM$e.processor)
Raw_MM$e.processor[Raw_MM$e.processor == "AbaloneTasmania"] <- "ABALONE TASMANIA PTY LTD"
Raw_MM$e.processor[Raw_MM$e.processor == "Tas Seafoods Margate"] <- "TASMANIAN SEAFOODS PTY LTD"
Raw_MM$e.processor[Raw_MM$e.processor == "Tas Seafoods Smithton"] <- "TASMANIAN SEAFOODS PTY LTD"
Raw_MM$e.processor[Raw_MM$e.processor == "Ralphs"] <- "RALPH'S TASMANIAN SEAFOOD PTY LTD"
Raw_MM$e.processor[Raw_MM$e.processor == "Coastal Waters"] <- "COASTAL WATERS SEAFOODS PTY LTD"
Raw_MM$e.processor[Raw_MM$e.processor == "Tas Live Abalone"] <- "TAS LIVE ABALONE PTY LTD"
Raw_MM$e.processor[Raw_MM$e.processor == "MK Haulage"] <- "M & K HAULAGE (TAS) PTY LTD"
Raw_MM$e.processor<-as.factor(Raw_MM$e.processor)

eproname<-as.data.frame(unique(Raw_MM$e.processor))
colnames(eproname)<-c("e.processor")

## clean up Raw_MM dataset - Jaime McAllister ##
summary(Raw_MM)
# msr. date remove records with no date and where date is unrealistic (e.g. years 0020 and 2088), add these to 
# Raw.MM.Output.error.docket dataframe
Raw.MM.Output.error.docket <- Raw_MM[is.na(Raw_MM$msr.date), ]
Raw.MM.Output.error.docket <- Raw_MM[as.Date(Raw_MM$msr.date) < '1999-12-31', ]
Raw.MM.Output.error.docket <- Raw_MM[as.Date(Raw_MM$msr.date) > '2011-02-11', ]
Raw_MM <- Raw_MM[!is.na(Raw_MM$msr.date), ]
Raw_MM <- Raw_MM[!as.Date(Raw_MM$msr.date) < '1999-12-31', ]
Raw_MM <- Raw_MM[!as.Date(Raw_MM$msr.date) > '2011-02-11', ]
# convert incorrect times to NA's
time.dates <- grep('/', Raw_MM$time)
Raw_MM$time[time.dates] <- NA

#summarize the Raw_MM dataset
n.per.docket<-ddply(Raw_MM,.(docket.number, msr.date, e.processor), summarize,  n = length(shell.length), 
                    meanSL = round(mean(shell.length), 1),
                    minSL = round(min(shell.length), 1))

#Remove non-e.processors form the docketinfo dataframe and rename column.
e.processors<-eproname$e.processor
docketinfo.epro<-droplevels(subset(docketinfo, processorname %in% e.processors))

colnames(docketinfo.epro)[colnames(docketinfo.epro)=="processorname"] <- "e.processor"

#match docketinfo.epro to the n.per.docket dataframe
docket.join<-join(n.per.docket, docketinfo.epro, by = c("docket.number", "e.processor"), type = "inner")

#add date difference column
docket.join$msr.date.diff<-as.Date(docket.join$msr.date, format="yyyy-%mm-%dd")-as.Date(docket.join$unloading_date, format="yyyy-%mm-%dd")

#
 #
  #STEP 2 extract unique dockets and join to RawMM
 #
#
#separate dupilicated dockets
#how many times does each docket.number occur
n_occur <- data.frame(table(docket.join$docket.number))
range(n_occur$Freq)

docket.uniq<-as.data.frame(docket.join[docket.join$docket.number %in% n_occur$Var1[n_occur$Freq == 1],])

#check
n_occur <- data.frame(table(docket.uniq$docket.number))
range(n_occur$Freq)

#OUTPUT 1 = create new dataframe joining the FeMMsub to docket.uniq only keeping common docket data to both
#
Output1.unique<-join(Raw_MM, docket.uniq, by = c("docket.number", "e.processor"), type = "inner")

#
 #
  #STEP 3 DUPLICATED DOCKETS remove duplicated catches which have multiple downloads in RawMM.
 #
#

#resetn-occur to docket.join
n_occur <- data.frame(table(docket.join$docket.number))
range(n_occur$Freq)

#select only dupicated dockets from the docket.join df
docket.dupes<-as.data.frame(docket.join[docket.join$docket.number %in% n_occur$Var1[n_occur$Freq > 1],])

#extract all dockets with duplicated results in docket.number and meanSL 
dupes.sub<-docket.dupes[ cbind( which(duplicated(docket.dupes[c(1,5)])), which(duplicated(docket.dupes[c(1,5)], fromLast=F))),]

dupes.sub<-dupes.sub[order(dupes.sub$msr.date),]
dupes.sub<-dupes.sub %>% distinct(docket.number, .keep_all = T) #was missing .keep_all = T and droppping variables (JM)

#n-occur to docket.join
n_occur <- data.frame(table(dupes.sub$docket.number))
range(n_occur$Freq)

#OUTPUT 2 = create new dataframe joining the FeMMsub to docket.uniq only keeping common docket data to both
#
Output2.dupes<-join(Raw_MM, dupes.sub, by = c("docket.number", "e.processor"), type = "inner")


#
 #
  #STEP 4 DUPLICATED DOCKETS remove duplicated records which have multiple docket entries.
 #
#

#Cut download duplicates from dupes.sub.dnumber that are assigned in Output2.dupes
dupes.sub.dnumber<-unique(Output2.dupes$docket.number)

dupes.multi<-docket.dupes[!docket.dupes$docket.number %in% dupes.sub.dnumber,]


# #dupes.multi<-droplevels(subset(dupes.multi, select = c("docket.number",  "msr.date", "unloading_date", "msr.date.diff", "meanSL", "minSL", "n", 
#                                                        "total_landed_weight", "catch",
#                                                       "received_location", "e.processor", "joincode", "processor_licence_id", 
#                                                        "blocklist", "numblocks", "subblocklist", "numsubblocks")))
# change records with false msr.date to NA
dupes.multi$msr.date.diff[dupes.multi$msr.date.diff<=-100] <-NA

#remove duplicates with NA results, negative date diff and false large date diff.
dupes.multi<-droplevels(subset(dupes.multi, msr.date.diff >=0 & msr.date.diff<=100))

#remove duplicates with same docket number and unloading date
dupes.multi.sub<-docket.dupes[ cbind( which(duplicated(dupes.multi[c(1,3)])), which(duplicated(dupes.multi[c(1,3)], fromLast=F))),]
#select distinct eg unqiue dockets (dplyr) function
dupes.multi.sub<-dupes.multi.sub %>% distinct(docket.number, .keep_all = T)
#identify unique dockets and remove from dupes.multi
dupes.ms.d<-unique(dupes.multi.sub$docket.number)
dupes.multi<-dupes.multi[!dupes.multi$docket.number %in% dupes.ms.d,]

#OUTPUT 3 = create new dataframe joining the FeMMsub to docket.uniq only keeping common docket data to both
#
Output3.multi<-join(Raw_MM, dupes.multi, by = c("docket.number", "msr.date"), type = "inner")

#OUTPUT 4 = create new dataframe joining the FeMMsub to docket.uniq only keeping common docket data to both
#
Output4.extra<-join(Raw_MM, dupes.multi.sub, by = c("docket.number", "msr.date"), type = "inner")

# format each output data.frame to be identical columns:

Output1.unique<-subset(Output1.unique, select=c("docket.number",  "filesource", "msr.date", "unloading_date", "msr.date.diff", "shell.length", 
                                                "total_landed_weight", "catch", "received_location", "e.processor", "joincode", 
                                                "processor_licence_id", "zone_fishery_code", "blocklist", "numblocks", "subblocklist", "numsubblocks"))
Output2.dupes<-subset(Output2.dupes, select=c("docket.number",  "filesource", "msr.date", "unloading_date", "msr.date.diff", "shell.length", 
                                                "total_landed_weight", "catch", "received_location", "e.processor", "joincode", 
                                                "processor_licence_id", "zone_fishery_code", "blocklist", "numblocks", "subblocklist", "numsubblocks"))
Output3.multi<-subset(Output3.multi, select=c("docket.number",  "filesource", "msr.date", "unloading_date", "msr.date.diff", "shell.length", 
                                              "total_landed_weight", "catch", "received_location", "e.processor", "joincode", 
                                              "processor_licence_id", "zone_fishery_code", "blocklist", "numblocks", "subblocklist", "numsubblocks"))
Output4.extra<-subset(Output4.extra, select=c("docket.number",  "filesource", "msr.date", "unloading_date", "msr.date.diff", "shell.length", 
                                              "total_landed_weight", "catch", "received_location", "e.processor", "joincode", 
                                              "processor_licence_id", "zone_fishery_code", "blocklist", "numblocks", "subblocklist", "numsubblocks"))
#compile Outputs
compiled.docket.07.11<-rbind(Output4.extra, Output3.multi, Output1.unique, Output2.dupes)


###############################################################################################################

keep(compiled.docket.FeMM, compiled.docket.07.11, Output.error.docket, Output.error.epro, Output.error.date, Raw.MM.Output.error.docket, docketinfo, sure=T)


###############################################################################################################
#
#
#

#      docket information held in Abalone research database FactoryEmeasure Query 

#
#
#
#extract FactoryEmeasure data from research Database table FactoryEmeasure
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

colnames(abdbMM) <- c("sample.number","docket.number", "shell.length","time","msr.date", "blocklist", "zone", "diver", 
                      "e.processor", "est.Blip", "est.Glip", "total_landed_weight", "sample.quality")
abdbMM$msr.date<-as.Date(abdbMM$msr.date, format="%d/%m/%Y" )


#remove records without docket number or docket = 0 or shell length = NA
Output.error.docket.abdbMM<-abdbMM[is.na(abdbMM$docket.number),]

abdbMM<-abdbMM[!is.na(abdbMM$docket.number),]
abdbMM<-abdbMM[!is.na(abdbMM$shell.length),]

#rename processors
abdbMM$e.processor <- as.character(abdbMM$e.processor)
abdbMM$e.processor[abdbMM$e.processor == "Adelaide Bay Seafoods Pty Ltd"] <- "ADELAIDE BAY SEAFOODS PTY LTD"
abdbMM$e.processor[abdbMM$e.processor == "Tasmanian Seafoods at Margate"] <- "TASMANIAN SEAFOODS PTY LTD"
abdbMM$e.processor[abdbMM$e.processor == "Tasmanian Seafoods at Smithton"] <- "TASMANIAN SEAFOODS PTY LTD"
abdbMM$e.processor[abdbMM$e.processor == "Ralphs Tasmanian Seafood Pty Ltd"] <- "RALPH'S TASMANIAN SEAFOOD PTY LTD"
abdbMM$e.processor<-as.factor(abdbMM$e.processor)


#summarize the dataset
n.per.docket<-ddply(abdbMM,.(docket.number, msr.date, e.processor), summarize,  n = length(shell.length), 
                    meanSL = round(mean(shell.length), 1),
                    minSL = round(min(shell.length), 1))


eproname<-as.data.frame(unique(abdbMM$e.processor))
colnames(eproname)<-c("e.processor")


#Remove non-e.processors form the docketinfo dataframe and rename column.
e.processors<-eproname$e.processor
docketinfo.epro<-droplevels(subset(docketinfo, processorname %in% e.processors))
colnames(docketinfo.epro)[colnames(docketinfo.epro)=="processorname"] <- "e.processor"
#match docketinfo.epro to the n.per.docket dataframe
docket.join<-join(n.per.docket, docketinfo.epro, by = c("docket.number", "e.processor"), type = "inner")

#add date difference column
docket.join$msr.date.diff<-as.Date(docket.join$msr.date, format="yyyy-%mm-%dd")-as.Date(docket.join$unloading_date, format="yyyy-%mm-%dd")

#
 #
  #STEP 2 extract unique dockets and join to abdbMM
 #
#
#separate dupilicated dockets
#how many times does each docket.number occur
n_occur <- data.frame(table(docket.join$docket.number))
range(n_occur$Freq)

docket.uniq<-as.data.frame(docket.join[docket.join$docket.number %in% n_occur$Var1[n_occur$Freq == 1],])

#check
n_occur <- data.frame(table(docket.uniq$docket.number))
range(n_occur$Freq)

#OUTPUT 1 = create new dataframe joining the FeMMsub to docket.uniq only keeping common docket data to both
#
Output1.unique<-join(abdbMM, docket.uniq, by = c("docket.number", "e.processor"), type = "inner")

#
 #
  #STEP 3 DUPLICATED DOCKETS remove duplicated catches which have multiple downloads in RawMM.
 #
#

#resetn-occur to docket.join
n_occur <- data.frame(table(docket.join$docket.number))
range(n_occur$Freq)

#select only dupicated dockets from the FeMM.docket.info list
docket.dupes<-as.data.frame(docket.join[docket.join$docket.number %in% n_occur$Var1[n_occur$Freq > 1],])

#extract all dockets with duplicated results in docket.number and meanSL 
dupes.sub<-docket.dupes[ cbind( which(duplicated(docket.dupes[c(1,5)])), which(duplicated(docket.dupes[c(1,5)], fromLast=F))),]

dupes.sub<-dupes.sub[order(dupes.sub$msr.date),]
dupes.sub<-dupes.sub %>% distinct(docket.number, .keep_all = T)

#n-occur to docket.join
n_occur <- data.frame(table(dupes.sub$docket.number))
range(n_occur$Freq)

#OUTPUT 2 = create new dataframe joining the FeMMsub to docket.uniq only keeping common docket data to both
#
Output2.dupes<-join(abdbMM, dupes.sub, by = c("docket.number", "e.processor"), type = "inner")

#
 #
  #STEP 4 DUPLICATED DOCKETS remove duplicated records which have multiple docket entries.
 #
#

#Cut download duplicates from dupes.sub.dnumber that are assigned in Output2.dupes
dupes.sub.dnumber<-unique(Output2.dupes$docket.number)

dupes.multi<-docket.dupes[!docket.dupes$docket.number %in% dupes.sub.dnumber,]


# #dupes.multi<-droplevels(subset(dupes.multi, select = c("docket.number",  "msr.date", "unloading_date", "msr.date.diff", "meanSL", "minSL", "n", 
#                                                        "total_landed_weight", "catch",
#                                                       "received_location", "e.processor", "joincode", "processor_licence_id", 
#                                                        "blocklist", "numblocks", "subblocklist", "numsubblocks")))
# change records with false msr.date to NA
dupes.multi$msr.date.diff[dupes.multi$msr.date.diff<=-100] <-NA


#remove duplicates with same docket number and unloading date
dupes.multi.sub<-dupes.multi[ cbind( which(duplicated(dupes.multi[c(1,3,13)])), which(duplicated(dupes.multi[c(1,3,13)], fromLast=F))),]
#select distinct eg unqiue dockets (dplyr) function
dupes.multi.sub<-dupes.multi.sub %>% distinct(docket.number, .keep_all = T)

#OUTPUT 4 = create new dataframe joining the FeMMsub to docket.uniq only keeping common docket data to both
#
Output4.extra<-join(abdbMM, dupes.multi.sub, by = c("docket.number", "msr.date"), type = "inner")

#identify unique dockets and remove from dupes.multi
dupes.ms.d<-unique(dupes.multi.sub$docket.number)
dupes.multi<-dupes.multi[!dupes.multi$docket.number %in% dupes.ms.d,]

# #remove duplicates with NA results, negative date diff and false large date diff.
dupes.multi.sub<-droplevels(subset(dupes.multi, msr.date.diff >=0 & msr.date.diff<=100))

#OUTPUT 5 = create new dataframe joining the abdbMM to dupes.multi.sub only keeping common docket data to both
#
Output5.date<-join(abdbMM, dupes.multi.sub, by = c("docket.number"), type = "inner")

#identify unique dockets and remove from dupes.multi
dupes.ms.d<-unique(dupes.multi.sub$docket.number)
dupes.multi<-dupes.multi[!dupes.multi$docket.number %in% dupes.ms.d,]


#OUTPUT 3 = create new dataframe joining the FeMMsub to docket.uniq only keeping common docket data to both
#
Output3.multi<-join(abdbMM, dupes.multi, by = c("docket.number"), type = "inner")


# format each output data.frame to be identical columns:

Output1.unique<-subset(Output1.unique, select=c("docket.number",  "msr.date", "unloading_date", "msr.date.diff", "shell.length", 
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
compiled.docket.abdb<-rbind(Output4.extra, Output3.multi, Output1.unique, Output2.dupes, Output5.date)

#n-occur to docket.join
test<-unique(compiled.docket.abdb[c("docket.number","e.processor","unloading_date", "zone_fishery_code")])
n_occur <- data.frame(table(test$docket.number))
range(n_occur$Freq)

#select only dupicated dockets from the FeMM.docket.info list
docket.test<-as.data.frame(test[test$docket.number %in% n_occur$Var1[n_occur$Freq > 1],])





###############################################################################################################

keep(compiled.docket.FeMM, compiled.docket.07.11, compiled.docket.abdb, Output.error.docket, Output.error.epro, Output.error.date, Raw.MM.Output.error.docket, docketinfo, compiled.docket.abdb, sure=T)

###############################################################################################################

###############################################################################################################

#      docket information held in Abalone research database FactoryEmeasure Query 

#
#
#
#extract FactoryEmeasure data from research Database table FactoryEmeasure
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

colnames(facMM) <- c("docket.number", "shell.length","msr.date", "block.fac", "subblock.fac", "Species", "diver", 
                      "processorname")
facMM$msr.date<-as.Date(facMM$msr.date, format="%d/%m/%Y" )
#facMM$docket.number<-as.numeric(facMM$docket.number)
facMM$subblock.fac <- paste(facMM$block.fac,facMM$subblock.fac, sep="")


#rename processors
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

#manual edit to two records missing processorname
facMM$processorname[facMM$docket.number %in% c("32740", "32743")] <- "TASMANIAN SEAFOODS PTY LTD"


facMM.uniq<-ddply(facMM,.(docket.number, processorname), summarize,  n = length(shell.length), 
      meanSL = round(mean(shell.length), 1),
      minSL = round(min(shell.length), 1))

# #test on duplicates
# #n-occur to docket.join
# test<-unique(facMM.uniq[c("docket.number","processorname")])
# n_occur <- data.frame(table(facMM.uniq$docket.number))
# range(n_occur$Freq)
# 
facMM.uniq.di<-join(facMM.uniq, docketinfo, by = c("docket.number", "processorname"), type = "left")

#test on duplicates
#n-occur to docket.join
test<-unique(facMM.uniq.di[c("docket.number","processorname")])
n_occur <- data.frame(table(test$docket.number))
range(n_occur$Freq)
#
compiled.docket.00.07<-join(facMM, facMM.uniq.di, by = c("docket.number","processorname" ), type = "inner")

compiled.docket.00.07$blocklist[is.na(compiled.docket.00.07$blocklist)] <- compiled.docket.00.07$block.fac[is.na(compiled.docket.00.07$blocklist)]
compiled.docket.00.07$subblocklist[is.na(compiled.docket.00.07$subblocklist)] <- compiled.docket.00.07$subblock.fac[is.na(compiled.docket.00.07$subblocklist)]

#added<-anti_join(facMM, compiled.docket.00.07, by=c("docket.number","processorname"))


#add date difference column
compiled.docket.00.07$msr.date.diff<-as.Date(compiled.docket.00.07$msr.date, format="yyyy-%mm-%dd")-as.Date(compiled.docket.00.07$unloading_date, format="yyyy-%mm-%dd")


###################################################################################################################

keep(compiled.docket.00.07, compiled.docket.FeMM, compiled.docket.07.11, compiled.docket.abdb, Output.error.docket, Output.error.epro, Output.error.date, Raw.MM.Output.error.docket, docketinfo, compiled.docket.abdb, sure=T)

###############################################################################################################





names(compiled.docket.FeMM)
names(compiled.docket.07.11)
names(compiled.docket.00.07)
names(compiled.docket.abdb)

compiled.docket.FeMM$datasource<-"FeMM"
compiled.docket.07.11$datasource<-"RawMM"
compiled.docket.00.07$datasource<-"MM.00.07"
compiled.docket.abdb$datasource<-"abdb"



names(compiled.docket.07.11)[names(compiled.docket.07.11)=="e.processor"] <- "processorname"
names(compiled.docket.abdb)[names(compiled.docket.abdb)=="e.processor"] <- "processorname"

cd.FEM<-subset(compiled.docket.FeMM, select=c("docket.number",  "msr.date", "unloading_date", "msr.date.diff", "shell.length", 
                                                           "total_landed_weight", "catch", "received_location", "processorname", "joincode", 
                                                           "processor_licence_id", "zone_fishery_code", "blocklist", "numblocks", "subblocklist", 
                                                           "numsubblocks", "datasource"))
cd.07.11<-subset(compiled.docket.07.11, select=c("docket.number",  "msr.date", "unloading_date", "msr.date.diff", "shell.length", 
                                                            "total_landed_weight", "catch", "received_location", "processorname", "joincode", 
                                                            "processor_licence_id", "zone_fishery_code", "blocklist", "numblocks", "subblocklist", 
                                                            "numsubblocks", "datasource"))
cd.abdb<-subset(compiled.docket.abdb, select=c("docket.number",  "msr.date", "unloading_date", "msr.date.diff", "shell.length", 
                                                            "total_landed_weight", "catch", "received_location", "processorname", "joincode", 
                                                            "processor_licence_id", "zone_fishery_code", "blocklist", "numblocks", "subblocklist", 
                                                            "numsubblocks", "datasource"))
cd.00.07<-subset(compiled.docket.00.07, select=c("docket.number",  "msr.date", "unloading_date", "msr.date.diff", "shell.length", 
                                                            "total_landed_weight", "catch", "received_location", "processorname", "joincode", 
                                                            "processor_licence_id", "zone_fishery_code", "blocklist", "numblocks", "subblocklist", 
                                                            "numsubblocks", "datasource"))
compiled.df<-rbind( cd.07.11, cd.FEM, cd.00.07)




#
compiled.df$unloading_date<-as.Date(compiled.df$unloading_date)

#summarize the Raw_MM dataset
n.per.docket<-ddply(compiled.df,.(docket.number, unloading_date,  msr.date.diff), summarize,  n = length(shell.length),
                    meanSL = round(mean(shell.length), 1),
                    minSL = round(min(shell.length), 1))

#n.per.docket<-unique(compiled.df[c("docket.number", "datasource")])
#n-occur to docket.join
n_occur <- data.frame(table(n.per.docket$docket.number))
range(n_occur$Freq)

#select unique dockets from the n.per.docket list
docket.uniq<-as.data.frame(n.per.docket[n.per.docket$docket.number %in% n_occur$Var1[n_occur$Freq ==1],])

#join unique FeMM dockets to the identifying information in docket.info from FILMS database
compiled.uniq<-join(compiled.df, docket.uniq,  by = c("docket.number","msr.date.diff", "unloading_date"), type ="inner")


#select unique dockets from the n.per.docket list
docket.dupes<-as.data.frame(n.per.docket[n.per.docket$docket.number %in% n_occur$Var1[n_occur$Freq ==2],])

#loop to keep the duplicate record with the higher n of animals and then initial download date only
db.dup.dockets<-unique(docket.dupes$docket.number)

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

#join unique FeMM dockets to the identifying information in docket.info from FILMS database
compiled.dupes<-join(compiled.df, pick_db.docket,  by = c("docket.number","msr.date.diff", "unloading_date"), type ="inner")

#select unique dockets from the n.per.docket list
docket.triples<-as.data.frame(n.per.docket[n.per.docket$docket.number %in% n_occur$Var1[n_occur$Freq >2],])

docket.triples<-droplevels(subset(docket.triples, n >50))
docket.triples<-droplevels(subset(docket.triples, docket.triples$msr.date.diff <50 & docket.triples$msr.date.diff >-20))

db.dup.dockets<-unique(docket.triples$docket.number)

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

#join unique FeMM dockets to the identifying information in docket.info from FILMS database####
compiled.triples<-join(compiled.df, pick_db.docket,  by = c("docket.number","msr.date.diff", "unloading_date"), type ="inner")

compiled.df<-rbind(compiled.uniq,compiled.dupes, compiled.triples)


#############################################################################################################
#
keep(compiled.df, compiled.docket.00.07, compiled.docket.FeMM, compiled.docket.07.11, compiled.docket.abdb, Output.error.docket, Output.error.epro, Output.error.date, Raw.MM.Output.error.docket, docketinfo, compiled.docket.abdb, sure=T)
#
#
#############################################################################################################
# a look at the docket composition of the ab-reserach database and the compiled.df from this r script identification of dockets not present in compiled.df


abdb.docket.uniq<-as.data.frame(unique(cd.abdb$docket.number))
colnames(abdb.docket.uniq)[colnames(abdb.docket.uniq)=="unique(cd.abdb$docket.number)"] <- "docket.number"
comp.docket.uniq<-as.data.frame(unique(as.integer(compiled.df$docket.number)))
colnames(comp.docket.uniq)[colnames(comp.docket.uniq)=="unique(as.integer(compiled.df$docket.number))"] <- "docket.number"

doc.distinct<-anti_join(abdb.docket.uniq, comp.docket.uniq) #just keeps x which have no duplicate in y

#subset cd.abdb by docket.numbers already in compiled.df

abdb.distinct<-cd.abdb[cd.abdb$docket.number %in% doc.distinct$docket.number,]

#############################################################################################################
#
keep(compiled.df, abdb.distinct, Output.error.docket, Output.error.epro, Output.error.date, Raw.MM.Output.error.docket, docketinfo,  sure=T)
#
#
#############################################################################################################




# a look at the error dockets and if any of the dockets unique to abdb.distinct match
colnames(Raw.MM.Output.error.docket)[colnames(Raw.MM.Output.error.docket)=="e.processor"] <- "processorname"

Error.docket.uniq<-as.data.frame(unique(as.integer(Raw.MM.Output.error.docket$docket.number)))
colnames(Error.docket.uniq)[colnames(Error.docket.uniq)=="unique(as.integer(Raw.MM.Output.error.docket$docket.number))"] <- "docket.number"


err.dkt<-ddply(Output.error.docket,.(docket.number, msr.date, e.processor), summarize,  n = length(shell.length), 
               meanSL = round(mean(shell.length), 1),
               minSL = round(min(shell.length), 1))

#comparing the two df
err.docket.match<-err.dkt[err.dkt$docket.number %in% abdb.uniq.docket$docket.number,]

##############################################################################################################

##############################################################################################################


compiled.df$fishyear<-as.numeric(format(as.Date(compiled.df$unloading_date,format="%Y-%m-%d"),"%Y"))

compiled.df$new_zone[compiled.df$blocklist %in% seq(13, 31, 1)] <- "E"
compiled.df$new_zone[compiled.df$blocklist %in% c(seq(7, 12, 1))] <- "W"
compiled.df$new_zone[compiled.df$blocklist %in% c(6)] <- "CW"
compiled.df$new_zone[compiled.df$blocklist %in% c(1, 2, 3, 4, 5, 47, 48, 49, 39, 40)] <- "N"
compiled.df$new_zone[compiled.df$blocklist %in% c(seq(32, 38, 1), seq(41, 46, 1), seq(50, 
                                                                                      57, 1))] <- "BS"

Year.summary<-ddply(compiled.df,.(fishyear, new_zone), summarize,  n = length(unique(docket.number)))

