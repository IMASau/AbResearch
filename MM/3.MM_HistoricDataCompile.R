# The following script collates all available historical abalone market measuring data into a single dataframe.
# The first four compilations are data extracted from various TAFI/IMAS databases and cross-checked for duplication.
# Additional data collected by CSIRO and others and held in various formats are also included. 
# More recent data are compiled from electronic measuring boards. 
# On-going market measuring data is added from the MM_NextGen4GCompile.R script.

# Created by: Hugh Jones (2015) and Jaime McAllister

##-------------------------------------------------------------------------------------------------------##
## Libaries and Functions ####

# load required libaries and functions for analysis

library(RODBC)
library(R.utils)
library(lubridate)
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
library(scales)
library(gridExtra)
#library(plyr)

source("C:/GitCode/AbResearch/codeBLnewzone.r")

##-------------------------------------------------------------------------------------------------------##
## Catch-effort and processor data ####

# import the most recent catch-effort and processor meta-data to link with market measure data to identify
# capture locations, dates, etc.

# load most recent newBlackCE dataframe from the catcheffort data output

setwd('c:/CloudStor/R_Stuff/AutoAssess')

# manually select the most recent .rdata file (or file that covers the market measure timeframe)

myFile <- file.choose()
load(myFile)

##-------------------------------------------------------------------------------------------------------##

# NOTE: latest docketinfo query below is missing data from year 2000, therefore temporarily use the
# older docket information to extract this data for analysis of compiled.docket.00.07 data

# load('AbProcessors_2018_10_09.RData')

# rename dataframe

# docketinfo.pre2001 <- docketinfo

# rename columns from SQL format

# names(docketinfo.pre2001)[names(docketinfo.pre2001) == 'docket_number'] <- 'docket.number'

# remove unessary dataframes

# rm(list=ls()[! ls() %in% c('docketinfo.pre2001')])


##-------------------------------------------------------------------------------------------------------##

# load most recent abalone processor details

# docketinfo <- readRDS('docketinfo2.RDS')
# docketinfo <- readRDS('C:/CloudStor/R_Stuff/AutoAssess/docketinfo3.rds')
docketinfo <- readRDS('C:/CloudStor/R_Stuff/MMLF/docketinfo3.rds')

# load('Dev_MM3.Rdata')

##-------------------------------------------------------------------------------------------------------##

## Processor data clean ####

# remove alpha characters from docket_number in processor data

docketinfo <- docketinfo %>%
 mutate(docket_number = as.numeric((gsub('[^0-9]', '', docket_number)))) %>%
 mutate(masterdocket = as.numeric((gsub('[^0-9]', '', masterdocket))))

# change 'docket_number' and 'certifcate_holder' column name out of SQL format in docketinfo

# names(docketinfo)[names(docketinfo)=="docket_number"] <- "docket.number"
docketinfo <- docketinfo %>% 
   rename(docket.number = docket_number,
          processorname = certificate_holder)

# # NOTE: certifcate_holder removed from latest docketinfo so added temporarily
# docketinfo$certificate_holder <- NA
# colnames(docketinfo)[colnames(docketinfo) == "certificate_holder"] <- "processorname"

# change processorname toupper

docketinfo <- docketinfo %>%
 dplyr::mutate_at('processorname', funs(toupper))

# add newzone using function (NOTE: G zone needs to be manually re-named)

docketinfo <- docketinfo %>%
 mutate(subblockno = subblocklist) %>%
 codeBlnewzone() %>%
 mutate(newzone = ifelse(zone == 'G', 'G', newzone)) %>%
 select(-c(subblockno))

# rename some of the processors who have changed buisness names over the years

docketinfo$processorname[docketinfo$processorname %in%
                          c(
                           "R & R HOBART INVESTMENTS PTY LTD",
                           "RTS PAUACO PTY LTD AS BARE TRUSTEE FOR THE RTS PAUACO LIMITED PARTNERSHIP"
                          )] <- "RALPH'S TASMANIAN SEAFOOD PTY LTD"

# the processor data extract from 2019 no longer contains unloading date or location, instead contains a list of fishing dates which
# can be used to generate the last fishing day in the case of a multi-day trip (i.e. daylist_max). In most cases there are no more
# than 6 fishing dates per record.

docketinfo <- docketinfo %>%
 separate(daylist, c('daylist_1', 'daylist_2', 'daylist_3', 'daylist_4', 'daylist_5', 'daylist_6', 'daylist_7', 'daylist_8'), sep = ',', remove = F) %>%
 mutate_at(vars(starts_with('daylist_')), funs(as.Date)) %>%
 ungroup() %>%
 mutate(daylist_max = pmax(daylist_1, daylist_2, daylist_3, daylist_4, daylist_5, daylist_6, daylist_7, daylist_8, na.rm = T)) %>%
 select(-c(daylist_1, daylist_2, daylist_3, daylist_4, daylist_5, daylist_6, daylist_7, daylist_8))

##-------------------------------------------------------------------------------------------------------##
## compiled.docket.FeMM ####
## Post 2010 MM data held in Gary Carlos db
 
# extract data from Access database establisehed by Gary Carlos holding all electronic measuring board
# data from 2010 (part) to 2015. 

# establish connection with Access database and create dataframe 'FeMM'

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

# get mesuring board download date(s) from database

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

# create dataframe of processors and identify download date of their electronic measuring boards

e.processors <- c("RALPH'S TASMANIAN SEAFOOD PTY LTD",
                  "ABALONE TASMANIA PTY LTD", 
                  "TASMANIAN SEAFOODS PTY LTD", 
                  "TASMANIAN SEAFOODS PTY LTD", 
                  "ADELAIDE BAY SEAFOODS PTY LTD")
e.pro <- as.data.frame(e.processors)
e.pro$DataSourceID <- c(1,2,3,4,5)
Download.date <- left_join(Download.date, e.pro, by = "DataSourceID")

# join FeMM and Download.date

FeMM <- left_join(FeMM, Download.date, by = "DownlaodEventID")

# rename columns out of SQL format

colnames(FeMM) <-
 c(
  "download.event",
  "measure.id",
  "Download.file",
  "record.number",
  "Zone",
  "entitlement.number",
  "docket.number",
  "shell.length",
  "msr.date",
  "msr.time",
  "dload.date",
  "datasource",
  "eprocessors"
 )

# STEP 1 - remove records without docket.number and missing shell.length. ####

FeMM <- FeMM[!is.na(FeMM$docket.number),]
FeMM <- FeMM[!is.na(FeMM$shell.length),]
FeMM <- droplevels(subset(FeMM, docket.number !=0))

# PROBLEM = multiple downloads of same data into gary database gives duplicate data

# STEP 2 -  remove duplicate records by selecting for matchings docket.numbers and first selecting docket with ####
#           max number of animals and then if still multiple by first download date.

n.per.docket <- FeMM %>%
 group_by(docket.number, dload.date, eprocessors) %>%
 summarise(n = length(shell.length), 
           meanSL = round(mean(shell.length), 1),
           minSL = round(min(shell.length), 1))

# loop to keep the duplicate record with the higher n of animals and then initial download date only

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

# check to ensure each docket.number is now unique 

n_occur <- data.frame(table(pick_db.docket$docket.number))
range(n_occur$Freq)

# subset FeMM database based on loop above

FeMM.sub <- inner_join(FeMM, pick_db.docket)

# format msr.date

FeMM.sub$msr.date <- as.Date(FeMM.sub$msr.date, format="yyyy-%mm-%dd")

# STEP 3 - subset docketinfo to >2010, and by processors that have e-boards ####

# FeMM database only has data from 2010 onwards so to reduce chances of duplicate records subset docketinfo to post 2009 and for 
# e.processors only

docketinfo.2010 <- droplevels(subset(docketinfo, fishyear >= 2010))
e.processors <- c("RALPH'S TASMANIAN SEAFOOD PTY LTD",
                  "ABALONE TASMANIA PTY LTD", 
                  "TASMANIAN SEAFOODS PTY LTD", 
                  "ADELAIDE BAY SEAFOODS PTY LTD", 
                  "M & K HAULAGE (TAS) PTY LTD")
docketinfo.2010 <- droplevels(subset(docketinfo.2010, processorname %in% e.processors))

# STEP 4 - join unique database docket to docketinfo.2010 ####

# join unique FeMM dockets to the identifying information in docket.info from FILMS database

FeMM.docket.info <- left_join(pick_db.docket, docketinfo.2010, by = "docket.number")

# separate dupilicated dockets (how many times each docket.number occur?)

n_occur <- data.frame(table(FeMM.docket.info$docket.number))
range(n_occur$Freq)

# STEP 5 - extract unique dockets and join to FeMMsub ####

# docket.uniq is all dockets without duplicates and can then be joined back to the original FeMMsub data

docket.uniq <- as.data.frame(FeMM.docket.info[FeMM.docket.info$docket.number %in% n_occur$Var1[n_occur$Freq == 1],])

# OUTPUT 1 = create new dataframe joining the FeMMsub to docket.uniq only keeping common docket data to both

Output1.unique <- inner_join(FeMM.sub, docket.uniq)

## add columns to show date differences between last fishing day (daylist_max), and msr.date and dload.date

Output1.unique <- Output1.unique %>%
 ungroup() %>%
 mutate(msr.date = as.Date(msr.date)) %>%
 mutate(dload.date = as.Date(dload.date)) %>%
 mutate(msr.date.diff = as.numeric(msr.date - daylist_max)) %>%
 mutate(dload.date.diff = as.numeric(dload.date - daylist_max))

# remove dockets with errors in dates and or processor
# dockets with issues in that download.date is prior to landing date but there are no duplicated dockets suggestion is incorrect docket number in eboard.

dload.date.false <- subset(Output1.unique, dload.date.diff <= -1)
dload.date.f.unique <- unique(dload.date.false, by = c("docket.number", "eprocessors", "dload.date", "daylist_max", "dload.date.diff", "msr.date.diff"))
Output.error.date <- left_join(dload.date.f.unique, docketinfo.2010,  by = "docket.number")

# dockets with issues in that eprocessor is not processorID

epro.proID.false <- as.data.frame(Output1.unique[Output1.unique$eprocessors != Output1.unique$processorname,])
epro.proID.false <- epro.proID.false[!is.na(epro.proID.false$docket.number),]
epro.proID.f.unique <- unique(epro.proID.false[c("docket.number", "eprocessors", "dload.date", "daylist_max","msr.date",
                                                 "dload.date.diff", "msr.date.diff")])
Output.error.epro <- left_join(epro.proID.f.unique, docketinfo.2010,  by = "docket.number")

# remove the dockets with issues

Output1.unique <- Output1.unique[!(Output1.unique$docket.number %in% dload.date.f.unique$docket.number),]
Output1.unique <- Output1.unique[!(Output1.unique$docket.number %in% epro.proID.false$docket.number),]

# STEP 6 - remove duplicated dockets remove split catches by matching eprocessor (garydb) to processorID (films) in dupliacted catches. ####

# select only dupicated dockets from the FeMM.docket.info list

docket.dupes <- as.data.frame(FeMM.docket.info[FeMM.docket.info$docket.number %in% n_occur$Var1[n_occur$Freq > 1],])
docket.epro <- subset(docket.dupes, (docket.dupes$eprocessors == docket.dupes$processorname))
n_occur <- data.frame(table(docket.epro$docket.number))
range(n_occur$Freq)

# STEP 7 - extract unique dockets and join to FeMMsub. ####

# docket.uniq is all dockets without duplicates and can then be joined back to the original FeMMsub data

docket.epro.uniq <- as.data.frame(docket.epro[docket.epro$docket.number %in% n_occur$Var1[n_occur$Freq == 1],])
 
# OUTPUT 2 = create new dataframe joining the FeMMsub to docket.epro.uniq only keeping common docket data to both

Output2.epro <- inner_join(FeMM.sub, docket.epro.uniq)

# add columns to show date differences between landing date, msr.Date and dload.Date

Output2.epro <- Output2.epro %>%
 ungroup() %>%
 mutate(msr.date = as.Date(msr.date)) %>%
 mutate(dload.date = as.Date(dload.date)) %>%
 mutate(msr.date.diff = as.numeric(msr.date - daylist_max)) %>%
 mutate(dload.date.diff = as.numeric(dload.date - daylist_max))

# STEP 8 - extract unique dockets and join to FeMMsub ####

docket.dble <- as.data.frame(docket.epro[docket.epro$docket.number %in% n_occur$Var1[n_occur$Freq > 1],])

# look at unique dates in dupes

docket.dble$dload.date.diff <- as.Date(docket.dble$dload.date, format="yyyy-%mm-%dd")-as.Date(docket.dble$daylist_max, format="yyyy-%mm-%dd")
docket.dble <- subset(docket.dble, dload.date.diff >= -1)
d.docket <- unique(docket.dble$docket.number)

# loop to keep the duplicate record with the smallest date.diff between landing date and dload.date
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

# check on how many times does each docket.number occur

n_occur <- data.frame(table(pick.dble.docket$docket.number))
range(n_occur$Freq)

# OUTPUT 3 = create new dataframe joining the FeMM to docket.dupes.uniq only keeping common docket data to both

Output3.dble <- inner_join(FeMM.sub, pick.dble.docket)

# add columns to show date differences between landing date, msr.Date and dload.Date

Output3.dble <- Output3.dble %>%
 ungroup() %>%
 mutate(msr.date = as.Date(msr.date)) %>%
 mutate(dload.date = as.Date(dload.date)) %>%
 mutate(msr.date.diff = as.numeric(msr.date - daylist_max)) %>%
 mutate(dload.date.diff = as.numeric(dload.date - daylist_max))

# STEP 9 - compile the final dataframe ####

# add column to identify which output each docket is derived from

Output1.unique$output <- 1
Output2.epro$output <- 2
Output3.dble$output <- 3

compiled.docket.FeMM <- bind_rows(Output1.unique, Output3.dble, Output2.epro)

# reduce column number

compiled.docket.FeMM <-
 droplevels(subset(
  compiled.docket.FeMM,
  select = c(
   "docket.number",
   "msr.date",
   "processorname",
   "eprocessors",
   "proc",
   "numprocs",
   "proclist",
   "newzone",
   "numdays",
   "daylist",
   "daylist_max",
   "dload.date",
   "dload.date.diff",
   "msr.date.diff",
   "numblocks",
   "blocklist",
   "numsubblocks",
   "subblocklist",
   "catch",
   "n",
   "meanSL",
   "minSL",
   "shell.length"
  )
 ))


# subset records with bad docket.numbers e.g. missing all matching info from FILMS

Output.error.docket <- compiled.docket.FeMM[is.na(compiled.docket.FeMM$processorname),]
compiled.docket.FeMM <- compiled.docket.FeMM[!(compiled.docket.FeMM$docket.number %in% Output.error.docket$docket.number),]

# there are now four dataframes from this work

# summary(compiled.docket.FeMM) # is the complete MM data with attached docket info
# summary(Output.error.date) # is records with errors with dates dload.date preceeds landing date but there is no duplicate records which match
# summary(Output.error.epro) # is records with non-matching processors from FILMS database and the eboard from which data was collected.
# summary(Output.error.docket) # is records with non-matching docket.numbers from FILMS database.

# in all cases of the error files i suspect there has been a mistake in the factory of docket.number entered.

##-------------------------------------------------------------------------------------------------------##

#keep(compiled.docket.FeMM, Output.error.docket, Output.error.epro, Output.error.date, docketinfo, sure=T)
rm(list=ls()[! ls() %in% c('compiled.docket.FeMM', 'Output.error.docket', 'Output.error.epro', 'Output.error.date', 'docketinfo')])
saveRDS(compiled.docket.FeMM, 'C:/CloudStor/R_Stuff/MMLF/compiled.docket.FeMM.RDS')

##-------------------------------------------------------------------------------------------------------##
## compiled.docket.07.11 ####
# 2007-2011 abalone e-measure MM raw data
     
# allocation of docket information to raw data held in market measuring folder for dates 2007-2011

setwd("R:/TAFI/TAFI_MRL_Sections/Wild_Fisheries_Program/Shared/13. Market measuring/MM_2007_2010_by_Processor")

# loop to search for summary files (created with MM_CSV_Compile.R)

MM_files <- c()

#change the grep to search based on folder names

for (dir_finals in grep('a',list.files(path='.',all.files=FALSE,full.names=TRUE),value=TRUE))
{
 MM_files <- c(MM_files,grep('MM_RawData_',list.files(path = dir_finals, all.files = FALSE, full.names = TRUE, recursive = TRUE),value=TRUE) )
}
all_data<-NULL

# combine files

Raw_MM <- do.call(rbind,lapply(MM_files,read.csv))

# tidy up some of the column names and data in Raw_MM dataframe

colnames(Raw_MM) <- c("x","sample.number", "docket.number","shell.length","time","msr.date", "filesource","e.processor")
Raw_MM$msr.date <- strptime(as.character(Raw_MM$msr.date), "%d/%m/%Y")
Raw_MM$msr.date <- as.Date(Raw_MM$msr.date)
Raw_MM$docket.number <- as.integer(Raw_MM$docket.number)
Raw_MM$shell.length <- as.integer(Raw_MM$shell.length)

# STEP 1 - remove records without docket number or docket = 0 or shell length = NA ####

Raw.MM.Output.error.docket <- Raw_MM[is.na(Raw_MM$docket.number),]
Raw_MM <- Raw_MM[!is.na(Raw_MM$docket.number),]
Raw_MM <- Raw_MM[!is.na(Raw_MM$shell.length),]
Raw_MM <- droplevels(subset(Raw_MM, docket.number !=0))

# rename processors and create dataframe of eprocessors

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

# additional data cleaning as some msr.date years appear to be errors (i.e. 0020 and 2088)
Raw_MM$msr.date[Raw_MM$msr.date < as.Date('2000-01-01')] <- NA
Raw_MM$msr.date[Raw_MM$msr.date > as.Date('2011-02-11')] <- NA

# convert incorrect times to NA's

time.dates <- grep('/', Raw_MM$time)
Raw_MM$time[time.dates] <- NA

# summarize the Raw_MM dataset

n.per.docket <- Raw_MM %>%
 group_by(docket.number, msr.date, e.processor) %>% 
 summarise(n = length(shell.length), 
                    meanSL = round(mean(shell.length), 1),
                    minSL = round(min(shell.length), 1))

# remove non-e.processors form the docketinfo dataframe and rename column.

e.processors <- eproname$e.processor
docketinfo.epro <- droplevels(subset(docketinfo, processorname %in% e.processors))
colnames(docketinfo.epro)[colnames(docketinfo.epro)=="processorname"] <- "e.processor"

# match docketinfo.epro to the n.per.docket dataframe

docket.join <- inner_join(n.per.docket, docketinfo.epro)

# add date difference column

docket.join <- docket.join %>%
 ungroup() %>%
 mutate(msr.date = as.Date(msr.date)) %>%
 mutate(msr.date.diff = as.numeric(msr.date - daylist_max))

# STEP 2 - extract unique dockets and join to RawMM ####

# separate dupilicated dockets 

n_occur <- data.frame(table(docket.join$docket.number))
range(n_occur$Freq)
docket.uniq <- as.data.frame(docket.join[docket.join$docket.number %in% n_occur$Var1[n_occur$Freq == 1],])

# check

n_occur <- data.frame(table(docket.uniq$docket.number))
range(n_occur$Freq)

# OUTPUT 1 = create new dataframe joining the FeMMsub to docket.uniq only keeping common docket data to both

Output1.unique <- inner_join(Raw_MM, docket.uniq, by = c("docket.number", 'msr.date', "e.processor"))

# STEP 3 - duplicated dockets, remove duplicated catches which have multiple downloads in RawMM. ####

# reset n-occur to docket.join

n_occur <- data.frame(table(docket.join$docket.number))
range(n_occur$Freq)

# select only dupicated dockets from the docket.join df

docket.dupes <- as.data.frame(docket.join[docket.join$docket.number %in% n_occur$Var1[n_occur$Freq > 1],])

# extract all dockets with duplicated results in docket.number and meanSL 

dupes.sub <- docket.dupes %>%
 group_by(docket.number, meanSL) %>%
 filter(n() > 1) %>%
 distinct(docket.number, .keep_all = T) %>%
 group_by(docket.number) %>%
 filter(docket.number != 183335) #this docket seems to have two different meanSL

# n-occur to docket.join

n_occur <- data.frame(table(dupes.sub$docket.number))
range(n_occur$Freq)

# OUTPUT 2 = create new dataframe joining the FeMMsub to docket.uniq only keeping common docket data to both

Output2.dupes <- inner_join(Raw_MM, dupes.sub, by = c("docket.number", "e.processor")) %>%
 mutate(msr.date = msr.date.x) %>%
 select(-c(msr.date.x, msr.date.y))

# STEP 4 - duplicated dockets, remove duplicated records which have multiple docket entries. ####

# cut download duplicates from dupes.sub.dnumber that are assigned in Output2.dupes

dupes.sub.dnumber <- unique(Output2.dupes$docket.number)
dupes.multi <- docket.dupes[!docket.dupes$docket.number %in% dupes.sub.dnumber,]

# change records with false msr.date to NA

dupes.multi$msr.date.diff[dupes.multi$msr.date.diff <= -100] <- NA

# remove duplicates with NA results, negative date diff and false large date diff.

dupes.multi <- droplevels(subset(dupes.multi, msr.date.diff >= 0 & msr.date.diff <= 100))

# remove duplicates with same docket number and daylist_max

dupes.multi.sub <- dupes.multi %>%
 group_by(docket.number, daylist_max) %>%
 filter(n() > 1)

# select distinct dockets 

dupes.multi.sub <- dupes.multi.sub %>% 
 distinct(docket.number, .keep_all = T)

# identify unique dockets and remove from dupes.multi

dupes.ms.d <- unique(dupes.multi.sub$docket.number)
dupes.multi <- dupes.multi[!dupes.multi$docket.number %in% dupes.ms.d,]

# OUTPUT 3 = create new dataframe joining the dupes.multi to raw_MM only keeping common docket data to both

Raw_MM$e.processor <- as.character(Raw_MM$e.processor)
Output3.multi <- inner_join(Raw_MM, dupes.multi, by = c("docket.number", "msr.date", 'e.processor'))

# OUTPUT 4 = create new dataframe joining the dupes.multi.sub to raw_MM only keeping common docket data to both

Output4.extra <- inner_join(Raw_MM, dupes.multi.sub, by = c("docket.number", "msr.date", 'e.processor'))


## format each output data.frame to be identical columns:

Output1.unique <- subset(Output1.unique, select = c("docket.number", "msr.date", "e.processor", "proc", "numprocs", "proclist", 
                                                    "newzone", "numdays", "daylist", "daylist_max", "msr.date.diff", "numblocks",
                                                    "blocklist", "numsubblocks", "subblocklist", "catch", "n", "meanSL", "minSL", 
                                                    "shell.length"))

Output2.dupes <- subset(Output2.dupes, select = c("docket.number", "msr.date", "e.processor", "proc", "numprocs", "proclist", 
                                                  "newzone", "numdays", "daylist", "daylist_max", "msr.date.diff", "numblocks",
                                                  "blocklist", "numsubblocks", "subblocklist", "catch", "n", "meanSL", "minSL", 
                                                  "shell.length"))

Output3.multi <- subset(Output3.multi, select = c("docket.number", "msr.date", "e.processor", "proc", "numprocs", "proclist", 
                                                           "newzone", "numdays", "daylist", "daylist_max", "msr.date.diff", "numblocks",
                                                           "blocklist", "numsubblocks", "subblocklist", "catch", "n", "meanSL", "minSL", 
                                                           "shell.length"))

Output4.extra <- subset(Output4.extra, select = c("docket.number", "msr.date", "e.processor", "proc", "numprocs", "proclist", 
                                                  "newzone", "numdays", "daylist", "daylist_max", "msr.date.diff", "numblocks",
                                                  "blocklist", "numsubblocks", "subblocklist", "catch", "n", "meanSL", "minSL", 
                                                  "shell.length"))

# STEP 5 - compile outputs ####

compiled.docket.07.11 <- bind_rows(Output4.extra, Output3.multi, Output1.unique, Output2.dupes)


##-------------------------------------------------------------------------------------------------------##

#keep(compiled.docket.FeMM, compiled.docket.07.11, Output.error.docket, Output.error.epro, Output.error.date, Raw.MM.Output.error.docket, docketinfo, sure=T)
rm(list=ls()[! ls() %in% c('compiled.docket.FeMM', 'compiled.docket.07.11', 'Output.error.docket', 'Output.error.epro', 'Output.error.date', 'Raw.MM.Output.error.docket', 'docketinfo')])
saveRDS(compiled.docket.07.11, 'C:/CloudStor/R_Stuff/MMLF/compiled.docket.07.11.RDS')

##-------------------------------------------------------------------------------------------------------##
## compiled.docket.abdb ####
## abalone e-measure MM data held in abalone database
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

# rename col.names from sql format and reformat date column

colnames(abdbMM) <- c("sample.number","docket.number", "shell.length","time","msr.date", "blocklist", "zone", "diver", 
                      "e.processor", "est.Blip", "est.Glip", "total_landed_weight", "sample.quality")
abdbMM$msr.date <- as.Date(abdbMM$msr.date, format="%d/%m/%Y" )


# remove records without docket number or docket = 0 or shell length = NA

Output.error.docket.abdbMM <- abdbMM[is.na(abdbMM$docket.number),]
abdbMM <- abdbMM[!is.na(abdbMM$docket.number),]
abdbMM <- abdbMM[!is.na(abdbMM$shell.length),]

# rename processors

abdbMM$e.processor <- as.character(abdbMM$e.processor)
abdbMM$e.processor[abdbMM$e.processor == "Adelaide Bay Seafoods Pty Ltd"] <- "ADELAIDE BAY SEAFOODS PTY LTD"
abdbMM$e.processor[abdbMM$e.processor == "Tasmanian Seafoods at Margate"] <- "TASMANIAN SEAFOODS PTY LTD"
abdbMM$e.processor[abdbMM$e.processor == "Tasmanian Seafoods at Smithton"] <- "TASMANIAN SEAFOODS PTY LTD"
abdbMM$e.processor[abdbMM$e.processor == "Ralphs Tasmanian Seafood Pty Ltd"] <- "RALPH'S TASMANIAN SEAFOOD PTY LTD"
abdbMM$e.processor <- as.factor(abdbMM$e.processor)

# summarize the dataset

n.per.docket <- abdbMM %>%
 group_by(docket.number, msr.date, e.processor) %>%
 summarise(n = length(shell.length), 
                    meanSL = round(mean(shell.length), 1),
                    minSL = round(min(shell.length), 1))

# create dataframe of all e-processors

eproname <- as.data.frame(unique(abdbMM$e.processor))
colnames(eproname) <- c("e.processor")

# STEP 1 - remove non-e.processors from the docketinfo dataframe and rename column. ####

e.processors <- eproname$e.processor
docketinfo.epro <- droplevels(subset(docketinfo, processorname %in% e.processors))
colnames(docketinfo.epro)[colnames(docketinfo.epro)=="processorname"] <- "e.processor"

# match docketinfo.epro to the n.per.docket dataframe

docket.join <- inner_join(n.per.docket, docketinfo.epro, by = c("docket.number", "e.processor"))

# add date difference column

docket.join <- docket.join %>%
 ungroup() %>%
 mutate(msr.date = as.Date(msr.date)) %>%
 mutate(msr.date.diff = as.numeric(msr.date - daylist_max)) 

# STEP 2 - extract unique dockets and join to abdbMM ####

# separate dupilicated dockets
n_occur <- data.frame(table(docket.join$docket.number))
range(n_occur$Freq)

docket.uniq <- as.data.frame(docket.join[docket.join$docket.number %in% n_occur$Var1[n_occur$Freq == 1],])

# check

n_occur <- data.frame(table(docket.uniq$docket.number))
range(n_occur$Freq)

## OUTPUT 1 = create new dataframe joining the abdbMM to docket.uniq only keeping common docket data to both

Output1.unique <- inner_join(abdbMM, docket.uniq, by = c("docket.number", "e.processor")) %>%
 mutate(msr.date = msr.date.x, blocklist = blocklist.y, zone = zone.y) %>%
 select(-c(msr.date.x, msr.date.y, blocklist.x, blocklist.y, zone.x, zone.y))

# STEP 3 - duplicated dockets, remove duplicated catches which have multiple downloads in RawMM. ####

# reset n-occur to docket.join

n_occur <- data.frame(table(docket.join$docket.number))
range(n_occur$Freq)

# select only dupicated dockets from the FeMM.docket.info list

docket.dupes <- as.data.frame(docket.join[docket.join$docket.number %in% n_occur$Var1[n_occur$Freq > 1],])

# extract all dockets with duplicated results in docket.number and meanSL 

dupes.sub <- docket.dupes %>%
 group_by(docket.number, meanSL) %>%
 filter(n() > 1) %>%
 distinct(docket.number, .keep_all = T) %>%
 filter(!docket.number %in% c(178459, 183335, 181190, 181189, 176983, 178455, 180633, 180630))

# check

n_occur <- data.frame(table(dupes.sub$docket.number))
range(n_occur$Freq)

# OUTPUT 2 = create new dataframe joining the abdbMM to dupes.sub only keeping common docket data to both

Output2.dupes <- inner_join(abdbMM, dupes.sub, by = c("docket.number", "msr.date", 'e.processor')) %>%
 mutate(blocklist = blocklist.y, zone = zone.y) %>%
 select(-c(blocklist.x, blocklist.y, zone.x, zone.y))

# STEP 4 - DUPLICATED DOCKETS remove duplicated records which have multiple docket entries

# cut download duplicates from dupes.sub.dnumber that are assigned in Output2.dupes

dupes.sub.dnumber <- unique(Output2.dupes$docket.number)
dupes.multi <- docket.dupes[!docket.dupes$docket.number %in% dupes.sub.dnumber,]

# change records with false msr.date to NA

dupes.multi$msr.date.diff[dupes.multi$msr.date.diff <= -100] <- NA

# remove duplicates with NA results, negative date diff and false large date diff.

dupes.multi <- droplevels(subset(dupes.multi, msr.date.diff >= 0 & msr.date.diff <= 100))

# remove duplicates with same docket number and daylist_max

dupes.multi.sub <- dupes.multi %>%
 group_by(docket.number, e.processor, daylist_max) %>%
 filter(n() > 1)

# select distinct dockets

dupes.multi.sub <- dupes.multi.sub %>% 
 distinct(docket.number, .keep_all = T)

# identify unique dockets and remove from dupes.multi

dupes.ms.d <- unique(dupes.multi.sub$docket.number)
dupes.multi <- dupes.multi[!dupes.multi$docket.number %in% dupes.ms.d,]

# OUTPUT 3 = create new dataframe joining the abdbMM to dupes.multi only keeping common docket data to both

Output3.multi <- inner_join(abdbMM, dupes.multi, by = c("docket.number", "msr.date", 'e.processor')) %>%
 mutate(blocklist = blocklist.y, zone = zone.y) %>%
 select(-c(blocklist.x, blocklist.y, zone.x, zone.y))

# OUTPUT 4 = create new dataframe joining the abdbMM to dupes.multi.sub only keeping common docket data to both

Output4.extra <- inner_join(abdbMM, dupes.multi.sub, by = c("docket.number", "msr.date", 'e.processor')) %>%
 mutate(blocklist = blocklist.y, zone = zone.y) %>%
 select(-c(blocklist.x, blocklist.y, zone.x, zone.y))

# STEP 4 - compile outputs ####

# format each output data.frame to be identical columns:

Output1.unique <- subset(Output1.unique, select = c("docket.number", "msr.date", "e.processor", "proc", "numprocs", "proclist", 
                                                    "newzone", "numdays", "daylist", "daylist_max", "msr.date.diff", "numblocks",
                                                    "blocklist", "numsubblocks", "subblocklist", "catch", "n", "meanSL", "minSL", 
                                                    "shell.length")) 
Output2.dupes<-subset(Output2.dupes, select = c("docket.number", "msr.date", "e.processor", "proc", "numprocs", "proclist", 
                                                "newzone", "numdays", "daylist", "daylist_max", "msr.date.diff", "numblocks",
                                                "blocklist", "numsubblocks", "subblocklist", "catch", "n", "meanSL", "minSL", 
                                                "shell.length"))
Output3.multi<-subset(Output3.multi, select = c("docket.number", "msr.date", "e.processor", "proc", "numprocs", "proclist", 
                                                "newzone", "numdays", "daylist", "daylist_max", "msr.date.diff", "numblocks",
                                                "blocklist", "numsubblocks", "subblocklist", "catch", "n", "meanSL", "minSL", 
                                                "shell.length"))
Output4.extra<-subset(Output4.extra, select = c("docket.number", "msr.date", "e.processor", "proc", "numprocs", "proclist", 
                                                "newzone", "numdays", "daylist", "daylist_max", "msr.date.diff", "numblocks",
                                                "blocklist", "numsubblocks", "subblocklist", "catch", "n", "meanSL", "minSL", 
                                                "shell.length"))

# compile Outputs

compiled.docket.abdb <- rbind(Output4.extra, Output3.multi, Output1.unique, Output2.dupes)

# n-occur to docket.join

test <- unique(compiled.docket.abdb[c("docket.number","e.processor","daylist_max")])
n_occur <- data.frame(table(test$docket.number))
range(n_occur$Freq)

# select only dupicated dockets from the abdbMM.docket.info list

docket.test <- as.data.frame(test[test$docket.number %in% n_occur$Var1[n_occur$Freq > 1],])

##-------------------------------------------------------------------------------------------------------##

#keep(compiled.docket.FeMM, compiled.docket.07.11, compiled.docket.abdb, Output.error.docket, Output.error.epro, Output.error.date, Raw.MM.Output.error.docket, docketinfo, compiled.docket.abdb, sure=T)
rm(list=ls()[! ls() %in% c('compiled.docket.FeMM', 'compiled.docket.07.11', 'compiled.docket.abdb', 'Output.error.docket', 'Output.error.epro', 'Output.error.date', 'Raw.MM.Output.error.docket', 'docketinfo', 'compiled.docket.abdb')])
saveRDS(compiled.docket.abdb, 'C:/CloudStor/R_Stuff/MMLF/compiled.docket.abdb.RDS')

##-------------------------------------------------------------------------------------------------------##
## compiled.docket.00.07 ####
## 2000-2007 abalone research database
# docket information held in Abalone research database FactoryEmeasure Query 

# extract FactoryEmeasure data from research Database table FactoryEmeasure

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

# tidy up some of the column names and variable formats from sql query

colnames(facMM) <- c("docket.number", "shell.length","msr.date", "block.fac", "subblock.fac", "Species", "diver", 
                      "processorname")
facMM$msr.date <- as.Date(facMM$msr.date, format="%d/%m/%Y" )
facMM$subblock.fac <- paste(facMM$block.fac,facMM$subblock.fac, sep="")
facMM$fishyear <- year(facMM$msr.date)

# rename processors

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

# manual edit two records missing processorname

facMM$processorname[facMM$docket.number %in% c("32740", "32743")] <- "TASMANIAN SEAFOODS PTY LTD"

# remove records where the docket.number is incorrect or missing

facMM$docket.number <- as.numeric(as.character(facMM$docket.number))
facMM <- subset(facMM, !(docket.number %in% c('below', '27/03/')))
facMM <- subset(facMM, !is.na(docket.number))

# some docket numbers are duplicated across different years and zones, therefore rename block and subblock columns, and add zone
source("C:/GitCode/AbResearch/codeBLnewzone.r")
facMM <- facMM %>%
 mutate(blocklist = block.fac, subblockno = subblock.fac) %>%
 codeBlnewzone() %>%
 mutate(zone = newzone) %>%
 select(-c(block.fac, subblock.fac))

# summarise the dataframe

facMM.uniq <- facMM %>%
 group_by(docket.number, processorname) %>%
 summarise(n = length(shell.length),
           meanSL = round(mean(shell.length), 1),
           minSL = round(min(shell.length), 1))

# match docket info to facMM.uniq datframe to test for duplicates

facMM.uniq.di <- left_join(facMM.uniq, docketinfo, by = c("docket.number", "processorname"))

##-------------------------------------------------------------------------------------------------------##

# test on duplicates

test <- unique(facMM.uniq.di[c("docket.number","processorname")])
n_occur <- data.frame(table(test$docket.number))
range(n_occur$Freq)

# compile the final dataframe

compiled.docket.00.07 <- inner_join(facMM, facMM.uniq.di, by = c("docket.number","processorname", 'fishyear', 'newzone')) %>% 
 mutate(blocklist = blocklist.y) %>%
 select(-c(blocklist.x, blocklist.y, zone.x, zone.y))

## add date difference column
compiled.docket.00.07 <- compiled.docket.00.07 %>%
 ungroup() %>%
 mutate(msr.date = as.Date(msr.date)) %>%
 mutate(msr.date.diff = as.numeric(msr.date - daylist_max))

##-------------------------------------------------------------------------------------------------------##

#keep(compiled.docket.00.07, compiled.docket.FeMM, compiled.docket.07.11, compiled.docket.abdb, Output.error.docket, Output.error.epro, Output.error.date, Raw.MM.Output.error.docket, docketinfo, compiled.docket.abdb, sure=T)
rm(list=ls()[! ls() %in% c('compiled.docket.00.07', 'compiled.docket.FeMM', 'compiled.docket.07.11', 'compiled.docket.abdb', 'Output.error.docket', 'Output.error.epro', 'Output.error.date', 'Raw.MM.Output.error.docket', 'docketinfo', 'compiled.docket.abdb')])
saveRDS(compiled.docket.00.07, 'C:/CloudStor/R_Stuff/MMLF/compiled.docket.00.07.RDS')

##-------------------------------------------------------------------------------------------------------##
## compile dataframes ####

# add variable to each dataframe to identify data source

compiled.docket.FeMM$datasource <- "FeMM"
compiled.docket.07.11$datasource <- "RawMM"
compiled.docket.00.07$datasource <- "MM.00.07"
compiled.docket.abdb$datasource <- "abdb"

# STEP 1 - format dataframes to match by column ####

# rename some of the coloumn names so dataframes match

names(compiled.docket.07.11)[names(compiled.docket.07.11)=="e.processor"] <- "processorname"
names(compiled.docket.abdb)[names(compiled.docket.abdb)=="e.processor"] <- "processorname"
names(compiled.docket.FeMM)[names(compiled.docket.FeMM)=="eprocessors"] <- "e.processor"

# subset each dataframe to contain the same variables

cd.FEM <- subset(compiled.docket.FeMM, select = c("docket.number", "msr.date", "proc", "processorname", "numprocs", "proclist", 
                                                  "newzone", "numdays", "daylist", "daylist_max", "msr.date.diff", "numblocks",
                                                  "blocklist", "numsubblocks", "subblocklist", "catch", "n", "meanSL", "minSL", 
                                                  "shell.length"))
cd.07.11 <- subset(compiled.docket.07.11, select = c("docket.number", "msr.date", "proc", "processorname", "numprocs", "proclist", 
                                                     "newzone", "numdays", "daylist", "daylist_max", "msr.date.diff", "numblocks",
                                                     "blocklist", "numsubblocks", "subblocklist", "catch", "n", "meanSL", "minSL", 
                                                     "shell.length"))
cd.abdb <- subset(compiled.docket.abdb, select = c("docket.number", "msr.date", "proc", "processorname", "numprocs", "proclist", 
                                                            "newzone", "numdays", "daylist", "daylist_max", "msr.date.diff", "numblocks",
                                                            "blocklist", "numsubblocks", "subblocklist", "catch", "n", "meanSL", "minSL", 
                                                            "shell.length"))
cd.00.07 <- subset(compiled.docket.00.07, select = c("docket.number", "msr.date", "proc", "processorname", "numprocs", "proclist", 
                                                     "newzone", "numdays", "daylist", "daylist_max", "msr.date.diff", "numblocks",
                                                     "blocklist", "numsubblocks", "subblocklist", "catch", "n", "meanSL", "minSL", 
                                                     "shell.length"))

# STEP 2 - compile dataframes to produce single dataframe ####

# NOTE: the abdb data has been excluded due to uncertainty in the accuracy of the data)

compiled.df <- bind_rows( cd.07.11, cd.FEM, cd.00.07)


# STEP 3 - remove duplicates of docket numbers

# summarise the compiled.df dataset

n.per.docket <- compiled.df %>%
 group_by(docket.number, daylist_max,  msr.date.diff) %>%
 summarise(n = length(shell.length),
                    meanSL = round(mean(shell.length), 1),
                    minSL = round(min(shell.length), 1))

n_occur <- data.frame(table(n.per.docket$docket.number))
range(n_occur$Freq)

# select unique dockets from the n.per.docket list

docket.uniq <- as.data.frame(n.per.docket[n.per.docket$docket.number %in% n_occur$Var1[n_occur$Freq ==1],])

# join unique FeMM dockets to the identifying information in docket.info from FILMS database

compiled.uniq <- inner_join(compiled.df, docket.uniq,  by = c("docket.number","msr.date.diff", "daylist_max")) %>%
 mutate(n = n.x, meanSL = meanSL.x, minSL = minSL.x) %>%
 select(-c(n.x, n.y, meanSL.x, meanSL.y, minSL.x, minSL.y))

# select duplicate dockets from the n.per.docket list

docket.dupes <- as.data.frame(n.per.docket[n.per.docket$docket.number %in% n_occur$Var1[n_occur$Freq == 2],])

# loop to keep the duplicate record with the higher n of animals and then initial download date only

db.dup.dockets <- unique(docket.dupes$docket.number)

if (exists("pick_db.docket")) 
 rm(pick_db.docket)

for(b in db.dup.dockets){
 choice<-subset(n.per.docket, docket.number == b)
 uniq.date<-unique(choice$daylist_max)
 for(d in uniq.date){
  choice.date<-subset(choice, daylist_max == d)
  maxim<-max(choice.date$n)  
  pick<-subset(choice.date, n == maxim)
  pick.uniq<- pick %>% distinct(docket.number, .keep_all = T)
  if (exists("pick_db.docket"))
  pick_db.docket <- rbind(pick_db.docket, pick.uniq)
 else
  pick_db.docket <- pick.uniq
}
}

# join unique FeMM dockets to the identifying information in docket.info from FILMS database

compiled.dupes <- inner_join(compiled.df, pick_db.docket,  by = c("docket.number","msr.date.diff", "daylist_max")) %>%
 mutate(n = n.x, meanSL = meanSL.x, minSL = minSL.x) %>%
 select(-c(n.x, n.y, meanSL.x, meanSL.y, minSL.x, minSL.y))

# select triplicate dockets from the n.per.docket list

docket.triples <- as.data.frame(n.per.docket[n.per.docket$docket.number %in% n_occur$Var1[n_occur$Freq > 2],])
docket.triples <- droplevels(subset(docket.triples, n > 50))
docket.triples <- droplevels(subset(docket.triples, docket.triples$msr.date.diff < 50 & docket.triples$msr.date.diff > -20))
db.dup.dockets <- unique(docket.triples$docket.number)

if (exists("pick_db.docket")) 
 rm(pick_db.docket)

for(b in db.dup.dockets){
 choice<-subset(docket.triples, docket.number == b)
 uniq.date<-unique(choice$daylist_max)
 for(d in uniq.date){
  choice.date<-subset(choice, daylist_max == d)
  maxim<-max(choice.date$n)  
  pick<-subset(choice.date, n == maxim)
  pick.uniq<- pick %>% distinct(docket.number, .keep_all = T)
  if (exists("pick_db.docket"))
   pick_db.docket <- rbind(pick_db.docket, pick.uniq)
  else
   pick_db.docket <- pick.uniq
 }
}

# join triplicate dockets to the identifying information in docket.info from FILMS database

compiled.triples <- inner_join(compiled.df, pick_db.docket,  by = c("docket.number","msr.date.diff", "daylist_max")) %>%
 mutate(n = n.x, meanSL = meanSL.x, minSL = minSL.x) %>%
 select(-c(n.x, n.y, meanSL.x, meanSL.y, minSL.x, minSL.y))

compiled.df <- bind_rows(compiled.uniq, compiled.dupes, compiled.triples)

##-------------------------------------------------------------------------------------------------------##

#keep(compiled.df, compiled.docket.00.07, compiled.docket.FeMM, compiled.docket.07.11, compiled.docket.abdb, Output.error.docket, Output.error.epro, Output.error.date, Raw.MM.Output.error.docket, docketinfo, compiled.docket.abdb, sure=T)
rm(list=ls()[! ls() %in% c('compiled.df', 'cd.abdb', 'compiled.docket.00.07', 'compiled.docket.FeMM', 'compiled.docket.07.11', 'compiled.docket.abdb', 'Output.error.docket', 'Output.error.epro', 'Output.error.date', 'Raw.MM.Output.error.docket', 'docketinfo', 'compiled.docket.abdb')])
saveRDS(compiled.df, 'C:/CloudStor/R_Stuff/MMLF/compiled.df.RDS')

##-------------------------------------------------------------------------------------------------------##
## Identify dockets from abalone database not in compiled.df ####
## a look at the docket composition of the ab-reserach database and the compiled.df from this r script 
## identification of dockets not present in compiled.df

abdb.docket.uniq <- as.data.frame(unique(cd.abdb$docket.number))
colnames(abdb.docket.uniq)[colnames(abdb.docket.uniq)=="unique(cd.abdb$docket.number)"] <- "docket.number"
comp.docket.uniq <- as.data.frame(unique(as.integer(compiled.df$docket.number)))
colnames(comp.docket.uniq)[colnames(comp.docket.uniq)=="unique(as.integer(compiled.df$docket.number))"] <- "docket.number"
doc.distinct <- anti_join(abdb.docket.uniq, comp.docket.uniq) #just keeps x which have no duplicate in y

## subset cd.abdb by docket.numbers already in compiled.df
abdb.distinct <- cd.abdb[cd.abdb$docket.number %in% doc.distinct$docket.number,]

##-------------------------------------------------------------------------------------------------------##

#keep(compiled.df, abdb.distinct, Output.error.docket, Output.error.epro, Output.error.date, Raw.MM.Output.error.docket, docketinfo,  sure=T)
rm(list=ls()[! ls() %in% c('abdb.distinct','abdb.docket.uniq', 'compiled.df', 'cd.abdb', 'compiled.docket.00.07', 'compiled.docket.FeMM', 'compiled.docket.07.11', 'compiled.docket.abdb', 'Output.error.docket', 'Output.error.epro', 'Output.error.date', 'Raw.MM.Output.error.docket', 'docketinfo', 'compiled.docket.abdb')])
saveRDS(abdb.distinct, 'C:/CloudStor/R_Stuff/MMLF/abdb.distinct.RDS')

##-------------------------------------------------------------------------------------------------------##
## Identify error dockets that match those from abalone database ####
# a look at the error dockets and if any of the dockets unique to abdb.distinct match

colnames(Raw.MM.Output.error.docket)[colnames(Raw.MM.Output.error.docket)=="e.processor"] <- "processorname"
Error.docket.uniq <- as.data.frame(unique(as.integer(Raw.MM.Output.error.docket$docket.number)))
colnames(Error.docket.uniq)[colnames(Error.docket.uniq)=="unique(as.integer(Raw.MM.Output.error.docket$docket.number))"] <- "docket.number"


err.dkt <- Output.error.docket %>%
 group_by(docket.number, msr.date, processorname) %>%
 summarise(n = length(shell.length), 
               meanSL = round(mean(shell.length), 1),
               minSL = round(min(shell.length), 1))

## comparing the two df
err.docket.match <- err.dkt[err.dkt$docket.number %in% abdb.docket.uniq$docket.number,]

##-------------------------------------------------------------------------------------------------------##

rm(list=ls()[! ls() %in% c('err.docket.match', 'abdb.distinct','abdb.docket.uniq', 'compiled.df', 'cd.abdb', 'compiled.docket.00.07', 'compiled.docket.FeMM', 'compiled.docket.07.11', 'compiled.docket.abdb', 'Output.error.docket', 'Output.error.epro', 'Output.error.date', 'Raw.MM.Output.error.docket', 'docketinfo', 'compiled.docket.abdb')])

##-------------------------------------------------------------------------------------------------------##
## compiled.docket.CSIRO ####
# these data contain records collected by CSIRO between 1967 and 2000. No original raw data files can
# be found; the only data record being an Excel file created by Dave Tarbath containing what appears to be a 
# compilation of those records which have been transferred to the abalone database. These data are
# added to the compiled.df

channel <- odbcConnect('Asd_data')
sql <- "SELECT * 
FROM MMCommercialSamplesQuery;"
ab.oldMM <- sqlQuery(channel, sql)
close(channel)

# rename dataframe and save a copy of raw ab.oldMM.R data as a backup prior to cleaning

csiroMM <- ab.oldMM
saveRDS(csiroMM, 'C:/CloudStor/R_Stuff/MMLF/csiroMM.RDS')

# subset historic data to include only the required variables 

csiroMM.sub <- subset(csiroMM, select = c("CSA_Docket", "CSA_Processor", "CSA_SampleDate", "CSA_CatchDate",
                                            "CSA_Species", "CSA_Block", "CSA_SubBlock", "CSA_CatchLocation", "CSA_CatchWeight",
                                            "CSP_Length", "CommercialSamples_CSA_ID"))

# rename most variables to match more recent data

colnames(csiroMM.sub) <- c("docket.number", "proc", "msr.date", "daylist_max", 
                            'species', "blocklist", "subblocklist", 'catchlocation', "catch",
                            "shell.length", 'sampleid')

# remove records without shell length

csiroMM.sub <- csiroMM.sub[!is.na(csiroMM.sub$shell.length),]

# search for duplicate records

n.per.sample.id <- csiroMM.sub %>%
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

# add a column 'datasource' to distinguish historic from more recent data

csiroMM.sub$datasource <- 'MM.CSIRO'

# add columns to match 'compiled.df' and populate with NA's

csiroMM.sub.var <- c('processorname', 'newzone', 'numblocks', 'numsubblocks', 'msr.date.diff')
csiroMM.sub[csiroMM.sub.var] <- NA

# populate additional columns where possible

csiroMM.sub$msr.date.diff <- ymd(csiroMM.sub$msr.date) - ymd(csiroMM.sub$daylist_max)
csiroMM.sub$numblocks <- count.fields(textConnection(as.character(csiroMM.sub$blocklist)), sep = ',')
csiroMM.sub$numsubblocks <- nchar(as.character(csiroMM.sub$subblocklist))

# summarise data for number of samples, mean and min shell length and join to original dataframe

csiroMM.sub <- csiroMM.sub %>% 
 group_by(sampleid) %>%
 summarise(n = length(shell.length),
           meanSL = round(mean(shell.length, na.rm = T), 1),
           minSL = round(min(shell.length), 1)) %>%
 inner_join(csiroMM.sub, 'sampleid')

# re-order columns

csiroMM.sub <- csiroMM.sub[,c("docket.number", "msr.date", "processorname", "proc", 
                              "newzone", "daylist_max", "msr.date.diff", "numblocks",
                              "blocklist", "numsubblocks", "subblocklist", "catch", "n", "meanSL", "minSL", 
                              "shell.length", "species")]

# add columns to 'compiled.df' to match and populate with NA's

# compiled.df.var <- c('catchlocation', 'sampleid', 'species')
# compiled.df[compiled.df.var] <- NA

## convert variables to allow join with compiled.df
csiroMM.sub$msr.date <- as.Date(csiroMM.sub$msr.date)
csiroMM.sub$proc <- as.character(csiroMM.sub$proc)
csiroMM.sub$blocklist <- as.character(csiroMM.sub$blocklist)
csiroMM.sub$subblocklist <- as.character(csiroMM.sub$subblocklist)
csiroMM.sub$daylist_max <- as.Date(csiroMM.sub$daylist_max)
csiroMM.sub$newzone <- as.character(csiroMM.sub$newzone)
csiroMM.sub$processorname <- as.character(csiroMM.sub$processorname)
csiroMM.sub$docket.number <- as.numeric(csiroMM.sub$docket.number)
csiroMM.sub$numblocks <- as.numeric(csiroMM.sub$numsubblocks)

# # join to compiled.df

compiled.df.csiro <- bind_rows(compiled.df, csiroMM.sub)
compiled.docket.csiro <- compiled.df.csiro
# saveRDS(compiled.df.csiro, 'C:/CloudStor/R_Stuff/MMLF/compiled.df.csiro.RDS')
saveRDS(compiled.docket.csiro, 'C:/CloudStor/R_Stuff/MMLF/compiled.docket.csiro.RDS')
compiled.docket.csiro <- readRDS('C:/CloudStor/R_Stuff/MMLF/compiled.docket.csiro.RDS')
##-------------------------------------------------------------------------------------------------------##
## compiled.docket.16.19 ####
## post 2016 MM abalone data from measuring boards provided by Gary Carlos in summary .csv files

# import data from .csv file

ab.2016MM.raw <- read.csv('R:/TAFI/TAFI_MRL_Sections/Wild_Fisheries_Program/Shared/13. Market measuring/DataSummary_from_1Jan2016_to_31Dec2016.csv')
ab.2019MM.raw.1 <- read.csv("R:/TAFI/TAFI_MRL_Sections/Wild_Fisheries_Program/Shared/13. Market measuring/Dave's recent stuff/DataSummary_from_1Mar2017_to_15May2019.csv")
ab.2019MM.raw.2 <- read.csv("R:/TAFI/TAFI_MRL_Sections/Wild_Fisheries_Program/Shared/13. Market measuring/TassieLobster_8April2019_to_15April2019.csv")

# import data for Tasmanian Seafoods compiled in 'TasmanianSeafoods2019_MM_Compile.R' script.

ab.2019MM.raw.3 <- readRDS('R:/TAFI/TAFI_MRL_Sections/Wild_Fisheries_Program/Shared/13. Market measuring/TasmanianSeafoods2019/TasmanianSeafoodsOct2019.RDS')

# fix incorrect MesrTime from raw data input

ab.2019MM.raw.2$MesrTime <- sub('04/08/2019', '08/04/2019', ab.2019MM.raw.2$MesrTime)

# row.bind data together

ab.2019MM.raw <- bind_rows(ab.2016MM.raw, ab.2019MM.raw.1, ab.2019MM.raw.2) %>%
   rename_all(tolower) %>% 
   mutate(downloaddate = dmy_hms(downloaddate),
          mesrtime = dmy_hms(mesrtime)) %>% 
   bind_rows(ab.2019MM.raw.3) %>% 
   select(datasource, entitlement, zone, docket, order, mesrtime, size, weight, downloaddate)


# remove unesseary varibales (zone, entitlement, order)

ab.2019MM <- subset(ab.2019MM.raw, select = c('datasource', 'downloaddate', 'docket', 'size', 'weight', 'mesrtime'))


# # re-format some of the data and variable names
# 
# colnames(ab.2019MM) <- tolower(colnames(ab.2019MM))
ab.2019MM$msr.date <- as.Date(ab.2019MM$mesrtime)
# ab.2019MM$msr.date <- date(ab.2019MM$msr.date)
# ab.2019MM$downloaddate <- dmy_hms(ab.2019MM$downloaddate)
# ab.2019MM$downloaddate <- date(ab.2019MM$downloaddate)

# remove measure time

ab.2019MM <- ab.2019MM %>%
 select(-c(mesrtime))

# re-label column names to match other datframes

colnames(ab.2019MM) <- c('e.processor','download.date', 'docket.number', 'shell.length', 'whole.weight', 'msr.date') 

# add variable to distinguish datasource

ab.2019MM$datasource <- '2019MM.csv'

# remove records without docket number or docket = 0 or shell length = NA

ab.2019MM.Output.error.docket <- ab.2019MM[is.na(ab.2019MM$docket.number),]
ab.2019MM <- ab.2019MM[!is.na(ab.2019MM$docket.number),]
ab.2019MM <- ab.2019MM[!is.na(ab.2019MM$shell.length),]
ab.2019MM <- droplevels(subset(ab.2019MM, docket.number !=0))

# rename processors (first convert to character and then factor)

ab.2019MM$e.processor <- as.character(ab.2019MM$e.processor)
ab.2019MM$e.processor[ab.2019MM$e.processor == "AbaloneTasmania"] <- "ABALONE TASMANIA PTY LTD"
ab.2019MM$e.processor[ab.2019MM$e.processor == "Tas Seafoods Margate"] <- "TASMANIAN SEAFOODS PTY LTD"
ab.2019MM$e.processor[ab.2019MM$e.processor == "Tasmanian Seafoods at Margate"] <- "TASMANIAN SEAFOODS PTY LTD"
ab.2019MM$e.processor[ab.2019MM$e.processor == "Tas Seafoods Smithton"] <- "TASMANIAN SEAFOODS PTY LTD"
ab.2019MM$e.processor[ab.2019MM$e.processor == "Ralphs"] <- "RALPH'S TASMANIAN SEAFOOD PTY LTD"
ab.2019MM$e.processor[ab.2019MM$e.processor == "Ralphs Tasmanian Seafood Pty Ltd"] <- "RALPH'S TASMANIAN SEAFOOD PTY LTD"
ab.2019MM$e.processor[ab.2019MM$e.processor == "Coastal Waters"] <- "COASTAL WATERS SEAFOODS PTY LTD"
ab.2019MM$e.processor[ab.2019MM$e.processor == "Tas Live Abalone"] <- "TAS LIVE ABALONE PTY LTD"
ab.2019MM$e.processor[ab.2019MM$e.processor == "MK Haulage"] <- "M & K HAULAGE (TAS) PTY LTD"
ab.2019MM$e.processor[ab.2019MM$e.processor == "Tassie Live Lobster"] <- 'TASSIE LOBSTER PTY LTD'
ab.2019MM$e.processor<-as.factor(ab.2019MM$e.processor)

# create dataframe of e.processors from data

eproname <- as.data.frame(unique(ab.2019MM$e.processor))
colnames(eproname) <- c("e.processor")
e.processors <- eproname$e.processor
docketinfo.epro <- droplevels(subset(docketinfo, processorname %in% e.processors))
docketinfo.epro$e.processor <- docketinfo.epro$processorname

# summarise data for number of samples, mean and min shell length and add to dataframe to check for duplicates

n.per.docket <- ab.2019MM %>% 
 group_by(docket.number, msr.date, e.processor) %>%
 summarise(n = length(shell.length),
           meanSL = round(mean(shell.length, na.rm = T), 1),
           minSL = round(min(shell.length), 1))

# match docketinfo.epro to the n.per.docket dataframe

docket.join <- inner_join(n.per.docket, docketinfo.epro, by = c("docket.number", "e.processor"))

# add date difference column

docket.join <- docket.join %>%
 ungroup() %>%
 mutate(msr.date = as.Date(msr.date)) %>%
 mutate(msr.date.diff = as.numeric(msr.date - daylist_max))

# check for and seperate out dupilicated dockets

n_occur <- data.frame(table(docket.join$docket.number))
range(n_occur$Freq)

docket.uniq <- as.data.frame(docket.join[docket.join$docket.number %in% n_occur$Var1[n_occur$Freq == 1],])

# check

n_occur <- data.frame(table(docket.uniq$docket.number))
range(n_occur$Freq)

# join unique dockets to ab.2019MM

ab.2019MM.unique <- inner_join(ab.2019MM, docket.uniq)


# subset data and filter out uneeded or duplicated variables

compiled.docket.16.19 <- subset(ab.2019MM.unique, select = c("docket.number", "msr.date", "proc", "processorname", "numprocs", "proclist", 
                                                             "newzone", "numdays", "daylist", "daylist_max", "msr.date.diff", "numblocks",
                                                             "blocklist", "numsubblocks", "subblocklist", "catch", "n", "meanSL", "minSL",
                                                             "shell.length", "whole.weight"))

saveRDS(compiled.docket.16.19, 'C:/CloudStor/R_Stuff/MMLF/compiled.docket.16.19.RDS')
compiled.docket.16.19 <- readRDS('C:/CloudStor/R_Stuff/MMLF/compiled.docket.16.19.RDS')

# join to compiled.df.csiro
# compiledMM.df <- bind_rows(compiled.df.csiro, compiled.docket.16.19)
compiled.docket <- bind_rows(compiled.docket.csiro, compiled.docket.16.19)

##-------------------------------------------------------------------------------------------------------##
## compiled.docket.81.96 ####
# this data comprises length frequency records collected by staff at fish processing
# factories between 1981-1996. Data are entered on a database from which the following
# .csv extract was produced.

# import raw data

ab.8196MM.raw <- read.csv("c:/CloudStor/R_Stuff/MMLF/mm_81_95.csv", header = TRUE, sep = ',', dec = '.', as.is = TRUE)

# rename variables to match other datasources and remove unwanted variables

ab.8196MM.df <- ab.8196MM.raw %>% 
   rename_all(tolower) %>% 
   mutate(docket.number = msd_sample_code,
          msr.date = as.Date(as.character(msd_capture_date), "%d/%m/%Y"),
          proc = msd_factory_code,
          daylist_max = msr.date,
          species = msd_species_code,
          blocklist = msd_block,
          shell.length = mm_size) %>% 
   select(msr.date, docket.number, species, blocklist, proc, shell.length, daylist_max)

# add variable to distinguish datasource

ab.8196MM.df$datasource <- '8196MM.csv'

# recode species which are listed as '3'
ab.8196MM.df$species <- as.integer(gsub(3, 1, ab.8196MM.df$species))

# summarise data for number of samples, mean and min shell length and add to dataframe to check for duplicates

n.per.docket <- ab.8196MM.df %>% 
   group_by(docket.number, msr.date, proc) %>%
   summarise(n = length(shell.length),
             meanSL = round(mean(shell.length, na.rm = T), 1),
             minSL = round(min(shell.length), 1))


# check for and seperate out dupilicated dockets

n_occur <- data.frame(table(n.per.docket$docket.number))
range(n_occur$Freq)

# join summary data to original data
compiled.docket.81.96 <- left_join(ab.8196MM.df, n.per.docket, by = c('docket.number', 'proc', 'msr.date'))

# include and populate known additional variables
compiled.docket.81.96 <- compiled.docket.81.96 %>% 
   mutate(numprocs = 1,
          proclist = as.character(proc),
          numdays = 1,
          daylist = as.character(daylist_max),
          msr.date.diff = 0,
          numblocks = 1,
          numsubblocks = 1,
          subblocklist = as.character(blocklist),
          blockno = as.character(blocklist),
          subblockno = as.character(blocklist))

compiled.docket.81.96$blocklist <- as.character(compiled.docket.81.96$blocklist)
compiled.docket.81.96$proc <- as.character(compiled.docket.81.96$proc)

# save dataframe
saveRDS(compiled.docket.81.96, 'C:/CloudStor/R_Stuff/MMLF/compiled.docket.81.96.RDS')
compiled.docket.81.96 <- readRDS('C:/CloudStor/R_Stuff/MMLF/compiled.docket.81.96.RDS')

# join dataframe to historical length frequency compiled data frame
compiled.docket.pre.2020 <- bind_rows(compiled.docket, compiled.docket.81.96)
saveRDS(compiled.docket.pre.2020, 'C:/CloudStor/R_Stuff/MMLF/compiled.docket.pre.2020.RDS')
##-------------------------------------------------------------------------------------------------------##
## compiled.docket.next.gen ####
# this data collected since 2020 using the Next Generation 4G measuring boards which
# upload data to the stfp server. Data are compiled in the seperate script 'MM_NextGen_4G.R'.

# import latest version of compiled next generation measuring board data

measure.board.next.gen.df <- readRDS('C:/CloudStor/R_Stuff/MMLF/measure.board.next.gen.df.RDS')

# match data to existing compiled dataframes
# remove unecessary variables and rename variables to match compiledMM.df

measure.board.next.gen.df.match <- measure.board.next.gen.df %>% 
   select(-c(rawutc, logger_date, local_date, latitude, longitude, abalonenum, zone, logname)) %>% 
   rename(msr.date = plaindate,
          docket.number = docketnum,
          shell.length = shelllength,
          whole.weight = wholeweight,
          e.processor = processor) %>% 
   mutate(e.processor = replace(e.processor, e.processor == 'RALPHS TASMANIAN SEAFOODS PTY LTD', "RALPH'S TASMANIAN SEAFOOD PTY LTD"))

# add variable to distinguish datasource
measure.board.next.gen.df.match$datasource <- '2020NextGen4G'                  

# extract docketinfo for e.processors listed in new data
eproname <- as.data.frame(unique(measure.board.next.gen.df.match$e.processor))
colnames(eproname) <- c("e.processor")
e.processors <- eproname$e.processor

docketinfo.epro <- droplevels(subset(docketinfo, processorname %in% e.processors)) %>% 
   rename(e.processor = processorname)

# summarise data for number of samples, mean and min shell length and add to dataframe to check for duplicates
n.per.docket <- measure.board.next.gen.df.match %>% 
   group_by(docket.number, msr.date, e.processor) %>%
   summarise(n = length(shell.length),
             meanSL = round(mean(shell.length, na.rm = T), 1),
             minSL = round(min(shell.length), 1))

# match docketinfo.epro to the n.per.docket dataframe
docket.join <- inner_join(n.per.docket, docketinfo.epro, by = c("docket.number", "e.processor"))

# add date difference column
docket.join <- docket.join %>%
   ungroup() %>%
   mutate(msr.date = as.Date(msr.date)) %>%
   mutate(msr.date.diff = as.numeric(msr.date - daylist_max))

# check for and seperate out dupilicated dockets
n_occur <- data.frame(table(docket.join$docket.number))
range(n_occur$Freq)
docket.uniq <- as.data.frame(docket.join[docket.join$docket.number %in% n_occur$Var1[n_occur$Freq == 1],])

# check
n_occur <- data.frame(table(docket.uniq$docket.number))
range(n_occur$Freq)

# join unique dockets to df.1
measure.board.next.gen.df.unique <- inner_join(measure.board.next.gen.df.match, docket.uniq)

# subset data and filter out uneeded or duplicated variables
compiled.docket.next.gen <- measure.board.next.gen.df.unique %>%
   select(
      docket.number,
      msr.date,
      proc,
      e.processor,
      numprocs,
      proclist,
      newzone,
      numdays,
      daylist,
      daylist_max,
      msr.date.diff,
      numblocks,
      blocklist,
      numsubblocks,
      subblocklist,
      catch,
      n,
      meanSL,
      minSL,
      shell.length,
      whole.weight,
      datasource
   ) %>% 
   rename('processorname' = e.processor)

# save RDS file

saveRDS(compiled.docket.next.gen, 'C:/CloudStor/R_Stuff/MMLF/compiled.docket.next.gen.RDS')
# compiled.docket.next.gen <- readRDS('C:/CloudStor/R_Stuff/MMLF/compiled.docket.next.gen.RDS')

# join dataframe to historical length frequency compiled data frame

# load most recent compile of historic data
compiled.docket.pre.2020 <- readRDS('C:/CloudStor/R_Stuff/MMLF/compiled.docket.pre.2020.RDS')

# merge most recent compile of historic data with next gen measureboard data
compiledMM.df <- bind_rows(compiled.docket.pre.2020, compiled.docket.next.gen)

# save RDS file
saveRDS(compiledMM.df, 'C:/CloudStor/R_Stuff/MMLF/compiledMM.df.RDS')

##-------------------------------------------------------------------------------------------------------##
## compiled.docket.non.modem ####

# this data has been collected since 2020 using the Next Generation non-modem measuring boards which
# are paired to 4G GPS logger issued to divers which then upload data to the stfp server. 
# data are compiled in the seperate script 'MM_NextGenMeasureBoard_Woodham.R'.
# as of August 2020 there are three active boards with divers: Greg Woodham, Ben Allen, and Sean Larby

# import latest version of compiled next generation woodham measuring board data

measure.board.df.non.modem <- readRDS('C:/CloudStor/R_Stuff/MMLF/MM_Plots/measure.board.df.non.modem.RDS')

# match data to existing compiled dataframes
measure.board.df.non.modem.match <- measure.board.df.non.modem %>% 
 select(-c(rawutc, logger_date, local_date, abalonenum, logname, geometry, memorymodule, catches.measured)) %>% 
 dplyr::rename(msr.date = plaindate,
        shell.length = shelllength,
        whole.weight = wholeweight,
        processorname = processor) %>% 
   mutate(proc = NA,
       numprocs = NA,
       proclist = NA,
       blocklist = blockno,
       subblocklist = subblockno,
       datasource = '2020NONMODEM',
       msr.date = as.Date(msr.date),
       daylist = as.character(msr.date),
       daylist_max = msr.date,
       msr.date.diff = as.numeric(msr.date - daylist_max),
       newzone = zone,
       numdays = 1,
       numblocks = 1,
       numsubblocks = 1,
       catch = NA) %>% 
   select(-blockno)

# summarise data for number of samples, mean and min shell length and add to dataframe to check for duplicates
n.per.docket <- measure.board.df.non.modem.match %>% 
   group_by(sample.id, species, msr.date) %>%
   summarise(n = length(shell.length),
             meanSL = round(mean(shell.length, na.rm = T), 1),
             minSL = round(min(shell.length), 1))

# match docketinfo.epro to the n.per.docket dataframe
docket.join <- inner_join(n.per.docket, measure.board.df.non.modem.match, 
                          by = c("sample.id", 'species', 'msr.date')) %>% 
   ungroup()
   # select(-(sample.id))

# subset data and filter out uneeded or duplicated variables
compiled.docket.non.modem <- docket.join %>%
   select(
      msr.date,
      proc,
      processorname,
      numprocs,
      proclist,
      newzone,
      numdays,
      daylist,
      daylist_max,
      msr.date.diff,
      numblocks,
      blocklist,
      numsubblocks,
      subblocklist,
      catch,
      meanSL,
      minSL,
      species,
      shell.length,
      whole.weight,
      datasource,
      sample.id
   )

# save RDS file

saveRDS(compiled.docket.non.modem, 'C:/CloudStor/R_Stuff/MMLF/compiled.docket.non.modem.RDS')

# join dataframe to historical length frequency compiled data frame

# load most recent compile of historic data
compiledMM.df <- readRDS('C:/CloudStor/R_Stuff/MMLF/compiledMM.df.RDS')

# merge most recent compile of historic data and next gen measureboard data with woodham data
compiledMM.df <- bind_rows(compiledMM.df, compiled.docket.non.modem)

## save compiledMM.df ####

saveRDS(compiledMM.df, 'C:/CloudStor/R_Stuff/MMLF/compiledMM.df.RDS')

##-------------------------------------------------------------------------------------------------------##
## save compiledMM.df ####

saveRDS(compiledMM.df, 'C:/CloudStor/R_Stuff/MMLF/compiledMM.df.RDS')
# compiledMM.df <- readRDS('C:/CloudStor/R_Stuff/MMLF/compiledMM.df.RDS')
rm(list=ls()[! ls() %in% c('err.docket.match', 'abdb.distinct','abdb.docket.uniq', 
                           'compiled.df', 'cd.abdb', 'compiled.docket.00.07', 
                           'compiled.docket.FeMM', 'compiled.docket.07.11', 
                           'compiled.docket.abdb', 'Output.error.docket', 
                           'Output.error.epro', 'Output.error.date', 
                           'Raw.MM.Output.error.docket', 'docketinfo', 
                           'compiled.docket.abdb', 'csiroMM', 'compiled.df.csiro',
                           'compiledMM.df')])


##-------------------------------------------------------------------------------------------------------##
## final compiledMM.df ####

# final data cleaning of compiledMM.df

# Make a copy of the compiledMM.df if needed

compiledMM.df.copy <- compiledMM.df
# compiledMM.df <- compiledMM.df.copy
# compiledMM.df <- compiledMM.df.2

# some sub-blocks have an 'N' making it read 'NA', remove N and replace with ''

compiledMM.df$subblocklist <- gsub('N', '', compiledMM.df$subblocklist)

# remove records with no shell length

compiledMM.df <- compiledMM.df[!is.na(compiledMM.df$shell.length),]

# add column for fishyear

compiledMM.df$fishyear <- year(compiledMM.df$daylist_max)

# there are records missing zone data to determine species, however
# block information is provided.  Most blocks are southern areas and can be assigned to blacklip
# however there are about 20 dockets from greenlip blocks which need to
# be assigned a species for plotting, etc.  Assuming only legal sized animals have been measured, 
# animals have been assigned a species based on the legal minimum length for species in that block.
# The greenlip blocks are 1-4, 31-49.  An initial examination of summary data
# indicates most of the records in question come from blocks 48 and 49 in the years 2000-2001
# when GL size limit was 140 mm. 

# subset compiledMM.df into seperate dataframes to identify species

compiledMM.df.spp <- subset(compiledMM.df, !is.na(newzone) & !is.na(species))
compiledMM.df.zone <- subset(compiledMM.df, !is.na(newzone) & is.na(species))
compiledMM.df.spp.nozone <- subset(compiledMM.df, is.na(newzone) & !is.na(species))

# identify species for records missing species but contain zone (i.e 'newzone')

compiledMM.df.zone.spp <- compiledMM.df.zone %>%
 mutate(species = ifelse(!is.na(species), species, 
                         ifelse(newzone == 'G', 2, 1)))

# rejoin dataframes 

compiledMM.df.2 <- bind_rows(compiledMM.df.zone.spp, compiledMM.df.spp.nozone, compiledMM.df.spp)

# split blocklist into seperate blocks to define region or regions where multiple blocks were fished

compiledMM.df.3 <- cSplit(compiledMM.df.2, 'blocklist', ',', drop = F)

# add unique ID column for each record to enable filtering operations

compiledMM.df.3 <- compiledMM.df.3 %>%
 mutate(id = row_number())

# create variables blockno and subblockno to run zone orignal region recode functions (these could be re-written at some stage)

compiledMM.df.3 <- compiledMM.df.3 %>%
 mutate(blockno = as.character(blocklist)) %>%
 mutate(subblockno = as.character(subblocklist))

# recode regions and zones 

# NOTE: for reporting at regional or block scale, include all data where there are multiple blocks
# fished. For reporting at a block scale remove all records where there are multiple blocks as we can not asign catch to 
# a single block. For reporting at a regional scale, remove records where the region recode
# does not match when calculated for each seperate block listed. This only applies to the
# recent data (i.e. >2000) for black and green lip.

# re-load region and zone recode function

source("C:/GitCode/AbResearch/codeBLnewzone.r")

# recode zone for data post 2000

compiledMM.df.recent.zone <- compiledMM.df.3 %>%
 filter(fishyear >= 2000) %>%
 codeBlnewzone() %>%
 mutate(newzone = ifelse(species == 2, 'G', newzone))

# recode zone for data pre 2000

compiledMM.df.historic.zone <- compiledMM.df.3 %>%
 filter(fishyear <= 1999) %>%
 codeBlnewZoneHistoric() %>%
 mutate(newzone = ifelse(species == 2,'G', newzone))

# recode region for data pre 2000

gl.historic <- codeGlRegionHistoric(subset(compiledMM.df.historic.zone, newzone == 'G')) %>%
 mutate(region.1 = gl.region) %>%
 mutate(same.region = 1) %>%
 select(-c(gl.region))

bl.historic <- codeBlRegionHistoric(subset(compiledMM.df.historic.zone, !newzone == 'G')) %>%
 mutate(region.1 = bl.region) %>%
 mutate(same.region = 1) %>%
 select(-c(bl.region))

# filter for blacklip and determine how many seperate data frames need to be created

bl.recent.multi.block <- subset(compiledMM.df.recent.zone, numblocks > 1 & species == 1)
max(bl.recent.multi.block$numblocks)

# for BLACKLIP create seperate data frames for each block within blocklist and determine their region

bl.recent.1 <- subset(compiledMM.df.recent.zone, !is.na(blocklist_1) & species == 1) %>%
 mutate(blockno.temp = blockno) %>%
 mutate(blockno = blocklist_1) %>%
 codeBlregion() %>%
 mutate(region.1 = bl.region) %>%
 mutate(blockno = blockno.temp) %>%
 select(-c(bl.region, blockno.temp))

bl.recent.2 <- subset(bl.recent.1, !is.na(blocklist_2)) %>%
 mutate(blockno.temp = blockno) %>%
 mutate(blockno = blocklist_2) %>%
 codeBlregion() %>%
 mutate(region.2 = bl.region) %>%
 mutate(blockno = blockno.temp) %>%
 select(-c(bl.region, region.1, blockno.temp))

bl.recent.3 <- subset(bl.recent.2, !is.na(blocklist_3)) %>%
 mutate(blockno.temp = blockno) %>%
 mutate(blockno = blocklist_3) %>%
 codeBlregion() %>%
 mutate(region.3 = bl.region) %>%
 mutate(blockno = blockno.temp) %>%
 select(-c(bl.region, region.2, blockno.temp))

# join seperate blacklip dataframes together to compare if the regions match

bl.recent.join <- left_join(bl.recent.1, bl.recent.2) %>%
 left_join(bl.recent.3)

# filter dataframe to exclude blacklip records where the regions DO match

bl.recent.same.region <- bl.recent.join %>% 
 mutate(same.region = if_else(!is.na(region.1) & is.na(region.2), 1,
                              if_else(is.na(region.3) & region.1 == region.2, 1, 
                                      if_else(region.1 == region.2 & region.2 == region.3, 1, 0)))) %>%
 filter(!same.region == 0)

# filter dataframe to exclude blacklip records where the regions DO NOT match

bl.recent.diff.region <- bl.recent.join %>% 
 mutate(same.region = if_else(!is.na(region.1) & is.na(region.2), 1,
                              if_else(is.na(region.3) & region.1 == region.2, 1, 
                                      if_else(region.1 == region.2 & region.2 == region.3, 1, 0)))) %>%
 filter(same.region == 0)

# filter for greenlip and determine how many seperate data frames need to be created
# repeating the steps above

gl.recent.multi.block <- subset(compiledMM.df.recent.zone, numblocks > 1 & species == 2)
max(gl.recent.multi.block$numblocks)

# for GREENLIP create seperate data frames for each block within blocklist and determine their region

gl.recent.1 <- subset(compiledMM.df.recent.zone, !is.na(blocklist_1) & species == 2 & fishyear >= 2006) %>%
 mutate(blockno.temp = blockno) %>%
 mutate(blockno = blocklist_1) %>%
 codeGlregion.post2006() %>%
 mutate(region.1 = gl.region) %>%
 mutate(blockno = blockno.temp) %>%
 select(-c(gl.region, blockno.temp))

gl.recent.2 <- subset(gl.recent.1, !is.na(blocklist_2)) %>%
 mutate(blockno.temp = blockno) %>%
 mutate(blockno = blocklist_2) %>%
 codeGlregion.post2006() %>%
 mutate(region.2 = gl.region) %>%
 mutate(blockno = blockno.temp) %>%
 select(-c(gl.region, region.1, blockno.temp))

gl.recent.3 <- subset(gl.recent.2, !is.na(blocklist_3)) %>%
 mutate(blockno.temp = blockno) %>%
 mutate(blockno = blocklist_3) %>%
 codeGlregion.post2006() %>%
 mutate(region.3 = gl.region) %>%
 mutate(blockno = blockno.temp) %>%
 select(-c(gl.region, region.2, blockno.temp))

gl.recent.4 <- subset(gl.recent.3, !is.na(blocklist_4)) %>%
 mutate(blockno.temp = blockno) %>%
 mutate(blockno = blocklist_4) %>%
 codeGlregion.post2006() %>%
 mutate(region.4 = gl.region) %>%
 mutate(blockno = blockno.temp) %>%
 select(-c(gl.region, region.3, blockno.temp))

gl.recent.5 <- subset(gl.recent.4, !is.na(blocklist_5)) %>%
 mutate(blockno.temp = blockno) %>%
 mutate(blockno = blocklist_5) %>%
 codeGlregion.post2006() %>%
 mutate(region.5 = gl.region) %>%
 mutate(blockno = blockno.temp) %>%
 select(-c(gl.region, region.4, blockno.temp))

# gl.historic.1 <- subset(compiledMM.df.historic.zone, !is.na(blocklist_1) & species == 2 & fishyear < 2006) %>%
#  mutate(blockno.temp = blockno) %>%
#  mutate(blockno = blocklist_1) %>%
#  codeGlregion.pre2006() %>%
#  mutate(region.1 = gl.region) %>%
#  mutate(blockno = blockno.temp) %>%
#  select(-c(gl.region, blockno.temp))
# 
# gl.historic.2 <- subset(gl.historic.1, !is.na(blocklist_2)) %>%
#  mutate(blockno.temp = blockno) %>%
#  mutate(blockno = blocklist_2) %>%
#  codeGlregion.pre2006() %>%
#  mutate(region.2 = gl.region) %>%
#  mutate(blockno = blockno.temp) %>%
#  select(-c(gl.region, region.1, blockno.temp))
# 
# gl.historic.3 <- subset(gl.historic.2, !is.na(blocklist_3)) %>%
#  mutate(blockno.temp = blockno) %>%
#  mutate(blockno = blocklist_3) %>%
#  codeGlregion.pre2006() %>%
#  mutate(region.3 = gl.region) %>%
#  mutate(blockno = blockno.temp) %>%
#  select(-c(gl.region, region.2, blockno.temp))
# 
# gl.historic.4 <- subset(gl.historic.3, !is.na(blocklist_4)) %>%
#  mutate(blockno.temp = blockno) %>%
#  mutate(blockno = blocklist_4) %>%
#  codeGlregion.pre2006() %>%
#  mutate(region.4 = gl.region) %>%
#  mutate(blockno = blockno.temp) %>%
#  select(-c(gl.region, region.3, blockno.temp))
# 
# gl.historic.5 <- subset(gl.historic.4, !is.na(blocklist_5)) %>%
#  mutate(blockno.temp = blockno) %>%
#  mutate(blockno = blocklist_5) %>%
#  codeGlregion.pre2006() %>%
#  mutate(region.5 = gl.region) %>%
#  mutate(blockno = blockno.temp) %>%
#  select(-c(gl.region, region.4, blockno.temp))

# join seperate greenlip dataframes together to compare if the regions match

gl.recent.join <- left_join(gl.recent.1, gl.recent.2) %>%
 left_join(gl.recent.3) %>%
 left_join(gl.recent.4) %>%
 left_join(gl.recent.5)

# gl.historic.join <- left_join(gl.historic.1, gl.historic.2) %>%
#  left_join(gl.historic.3) %>%
#  left_join(gl.historic.4) %>%
#  left_join(gl.historic.5)

# gl.recent.join <- bind_rows(gl.recent.join.a, gl.historic.join)

# filter dataframe to exclude greenlip records where the regions DO match

gl.recent.same.region <- gl.recent.join %>%
 mutate(same.region = if_else(!is.na(region.1) & is.na(region.2), 1,
         if_else(is.na(region.5) & is.na(region.4) & is.na(region.3) & region.1 == region.2, 1,
                       if_else(is.na(region.5) & is.na(region.4) & region.1 == region.2 & region.2 == region.3, 1,
                               if_else(is.na(region.5) & region.1 == region.2 & region.2 == region.3 & region.3 == region.4, 1,
                                       if_else(!is.na(region.5) & region.5 == region.1, 1, 0)))))) %>%
 filter(!same.region == 0)


# filter dataframe to exclude greenlip records where the regions DO NOT match

gl.recent.diff.region <- gl.recent.join %>%
 mutate(same.region = if_else(!is.na(region.1) & is.na(region.2), 1,
                              if_else(is.na(region.5) & is.na(region.4) & is.na(region.3) & region.1 == region.2, 1,
                                      if_else(is.na(region.5) & is.na(region.4) & region.1 == region.2 & region.2 == region.3, 1,
                                              if_else(is.na(region.5) & region.1 == region.2 & region.2 == region.3 & region.3 == region.4, 1,
                                                      if_else(!is.na(region.5) & region.5 == region.1, 1, 0)))))) %>%
 filter(same.region == 0)

# rejoin all the seperate black and greenlip dataframes together with a region recode for each block in blocklist
# NOTE: there are 2802 records without a blocklist and these have been excluded

compiledMM.df.final <- bind_rows(gl.recent.diff.region, gl.recent.same.region, 
                             bl.recent.diff.region, bl.recent.same.region,
                             gl.historic, bl.historic)

# check if there are any missing regions following region recode

missing.region <- subset(compiledMM.df.final, is.na(region.1))
missing.zone <- subset(compiledMM.df.final, is.na(newzone))


# populate blank sample.id column with docket number
compiledMM.df.final <- compiledMM.df.final %>% 
   mutate(sample.id = ifelse(is.na(sample.id), docket.number, sample.id))

# save a copy of r.file

saveRDS(compiledMM.df.final, 'C:/CloudStor/R_Stuff/MMLF/compiledMM.df.final.RDS')
# compiledMM.df.final <- readRDS('C:/CloudStor/R_Stuff/MMLF/compiledMM.df.final.RDS')

##-------------------------------------------------------------------------------------------------------##

rm(list=ls()[! ls() %in% c('err.docket.match', 'abdb.distinct','abdb.docket.uniq', 
                           'compiled.df', 'cd.abdb', 'compiled.docket.00.07', 
                           'compiled.docket.FeMM', 'compiled.docket.07.11', 
                           'compiled.docket.abdb', 'Output.error.docket', 
                           'Output.error.epro', 'Output.error.date', 
                           'Raw.MM.Output.error.docket', 'docketinfo', 
                           'compiled.docket.abdb', 'csiroMM', 'compiled.df.csiro',
                           'compiledMM.df', 'compiledMM.df.final')])

##-------------------------------------------------------------------------------------------------------##
# ## ****OLD PLOT STUFF CAN BE DELETED - PLOTS MOVED TO SEPERATE SCRIPT****
# 
# ## Plots of final data frame
# 
# # load most recent MM data compelation for 1974-2015
# # (NOTE: data for 2000 is currently not included in this df as the latest docketinfo query does not extract these records
# #  and therfore can not be matched to more measuring board information)
# 
# # compiledMM.df.final <- readRDS('C:/CloudStor/R_Stuff/MMLF/compiledMM.df.final.RDS')
# 
# # set working directory to export plots
# 
# setwd("C:/CloudStor/R_Stuff/MMLF/MM_Plots")
# 
# ##-------------------------------------------------------------------------------------------------------##
# # LF boxplot
# # Length frequency boxplot for zone and/or region for chosen time interval
# 
# # 1. select zone and region
# 
# # E = Actaeons < BichenoFreycinet < BrunyIsland < Channel < Fortescue < StHelens < StormBay
# # W = Actaeons < Granville < NorthWest < SouthCoast < SouthWest < Strahan
# # N = HunterIsland < KingIsland < NorthEast < NorthWest < StHelens
# # BS = BassStraitIslands < CentralNorth < FurneauxGroup < HunterIsland
# # G = CentralNorth < FurneauxGroup < KingIsland < NorthEast < NorthWest < PerkinsBay
# 
# zone <- 'E'
# region <- 'Actaeons'
# block <- 13
# 
# # 2. extract plot data
# 
# plotdat <-
#  compiledMM.df.final %>% 
#  filter(region.1 %in% region & 
#          newzone %in% zone &
#          same.region == 1 &
#          (between(shell.length, 100, 220)) & 
#          (between(fishyear, 2000, 2019)))
# 
# # plotdat <-
# #  compiledMM.df.final %>%
# #  filter(newzone %in% zone &
# #          same.region == 1 &
# #          (between(shell.length, 100, 220)) &
# #          (between(fishyear, 2016, 2016)))
# 
# # 3. convert required grouping variable to factor for boxplot
# 
# plotdat$fishyear <- as.factor(plotdat$fishyear)
# #plotdat$fishquarter <- as.factor(plotdat$fishquarter)
# 
# # 4. generate a count of records for each year to add to boxplot
# 
# plotdat.n <- plotdat %>% 
#  group_by(fishyear, region.1) %>% 
#  summarize(n = n())
# 
# # 5. generate boxplot of shell lengths for chosen grouping variable
# 
# mm.zone.boxplot <- ggplot(plotdat, aes(x = fishyear, y = shell.length)) + 
#  geom_boxplot(outlier.colour = "orange", outlier.size = 1.5) +
#  geom_text(data = plotdat.n, aes(y = 220, label = n,), size = 3, angle = 90) +
#  # geom_hline(data = filter(plotdat, region.1 == 'Actaeons'), aes(yintercept = 138),colour = 'red', linetype = 'dashed', size = 0.5)+
#  # geom_hline(data = filter(plotdat, region.1 == 'Channel'), aes(yintercept = 138),colour = 'red', linetype = 'dashed', size = 0.5)+
#  # geom_hline(data = filter(plotdat, region.1 == 'Fortescue'), aes(yintercept = 138),colour = 'red', linetype = 'dashed', size = 0.5)+
#  # geom_hline(data = filter(plotdat, region.1 == 'StHelens'), aes(yintercept = 138),colour = 'red', linetype = 'dashed', size = 0.5)+
#  # geom_hline(data = filter(plotdat, region.1 == 'BichenoFreycinet'), aes(yintercept = 145),colour = 'red', linetype = 'dashed', size = 0.5)+
#  # geom_hline(data = filter(plotdat, region.1 == 'StormBay'), aes(yintercept = 138),colour = 'red', linetype = 'dashed', size = 0.5)+
#  #geom_hline(aes(yintercept = 145), colour = 'red', linetype = 'dotted')+
#  geom_hline(aes(yintercept = 132), colour = 'red', linetype = 'dotted')+
#  #facet_grid(. ~ region.1)+
#  #ggtitle('Western Zone 1967-2016') +
#  xlab('Year') +
#  ylab('Shell Length (mm)')+ 
#  coord_cartesian(ylim = c(100, 225))+
#  theme_bw() + 
#  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
#        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#        axis.text.x = element_text(angle = 0, vjust = 0.5))
# 
# print(mm.zone.boxplot)
# 
# # 6. save plots to working directory
# 
# ggsave(filename = paste(zone, 'Z', '_', region, '_LF_boxplot', '.pdf', sep = ''), plot = mm.zone.boxplot)
# ggsave(filename = paste(zone, 'Z', '_', region, '_LF_boxplot', '.wmf', sep = ''), plot = mm.zone.boxplot)
# 
# ##-------------------------------------------------------------------------------------------------------##
# ## LF histogram
# ## Length frequency plot for zone, with block or region vertically adjacent every 5 years
# 
# 
# 
# # 1. select zone
# 
# zone <- 'E'
# 
# # 2. select block from zone if using block as the overlay
# # unique((compiledMM.df %>% filter(newzone %in% zone))$blockno)
# # block.no <- c(13, 16, 22, 23, 29)
# 
# # 3. select region from zone if using zone as the overlay
# 
# ordered(unique((compiledMM.df.final %>% 
#                  filter(newzone %in% zone & 
#                          species == 1 & 
#                          same.region == 1))$region.1))
# 
# # E = Actaeons < BichenoFreycinet < BrunyIsland < Channel < Fortescue < StHelens < StormBay
# # W = Actaeons < Granville < NorthWest < SouthCoast < SouthWest < Strahan
# # N = HunterIsland < KingIsland < NorthEast < NorthWest < StHelens
# # BS = BassStraitIslands < CentralNorth < FurneauxGroup < HunterIsland
# # G = CentralNorth < FurneauxGroup < KingIsland < NorthEast < NorthWest < PerkinsBay
# 
# region.plot<- 'Actaeons'
# 
# # 4. create list of regions for chosen zone
# 
# region.2019 <- unique((compiledMM.df.final %>% 
#                         filter(newzone %in% zone))$region.1)
# 
# # 5. select data for zone
# 
# plotdat.2.zone <- compiledMM.df.final %>% 
#  filter(newzone %in% zone 
#         & fishyear %in% c(2000, 2005, 2010, 2015, 2019) 
#         & between(shell.length, 100, 220)
#         & same.region == 1) %>% 
#  group_by(fishyear) %>%
#  mutate(n = n())
# 
# # 6. select data for zone and count the number of measurments
# 
# plotdat.2.n.zone <- compiledMM.df.final %>% 
#  filter(newzone %in% zone 
#         & fishyear %in% c(2000, 2005, 2010, 2015, 2019) 
#         & between(shell.length, 100, 220)
#         & same.region == 1) %>%
#  group_by(fishyear) %>%
#  summarise(zone.n = n(),
#            n = paste('n =', n()))
# 
# # 7. select data for zone, and for chosen region or block  
# 
# plotdat.2 <- compiledMM.df.final %>% 
#  filter(newzone %in% zone 
#         & region.1 %in% region.plot 
#         & fishyear %in% c(2000, 2005, 2010, 2015, 2019) 
#         & between(shell.length, 100, 220)
#         & same.region == 1) %>%
#  group_by(fishyear) %>%
#  mutate(n = n())
# 
# # 8. select data for zone, and for chosen region or block and count the number of measurments
# 
# plotdat.2.n <- compiledMM.df.final %>% 
#  filter(newzone %in% zone 
#         & region.1 %in% region.plot 
#         & fishyear %in% c(2000, 2005, 2010, 2015, 2019) 
#         & between(shell.length, 100, 220)
#         & same.region == 1) %>%
#  group_by(fishyear) %>%
#  summarise(region.n = n(),
#            n = paste('n =', n())) 
# 
# # 9. join count of measurments for zone, and for chosen region or block, and create plot label
# 
# plotdat.2.n.2.join <- 
#  left_join(plotdat.2.n.zone, plotdat.2.n, by = "fishyear") %>%
#  mutate(percent.region = round((region.n/zone.n)*100), 0) %>%
#  mutate(n = ifelse(is.na(region.n), 'NO DATA',
#                    paste0('n = ', region.n, '\n', '(', percent.region, '%', ')')))
# 
# # 10. generate plot (add or remove size limits where data is absent)
# 
# mm.zone.plot.2 <- ggplot(plotdat.2, aes(shell.length))+
#  geom_histogram(data = transform(subset(plotdat.2.zone, newzone %in% zone), region.1 = NULL), 
#                 aes(y = -..density.. * 5), fill = 'white', col = 'black',  binwidth = 5)+
#  geom_histogram(data = subset(plotdat.2, region.1 %in% region.2019), 
#                 aes(y = ..density.. *5), fill = 'black', colour = 'black', binwidth = 5)+
#  #overlay selected block or region
#  #geom_histogram(data = subset(plotdat.2, blockno %in% block.no), fill = 'black', binwidth = 5)+
#  theme_bw()+
#  ylab(paste(region.plot, "region", " Percentage (%)")) +
#  xlab("Shell Length (mm)")+
#  #coord_cartesian(xlim = c(100, 220), ylim = c(-0.4, 0.4))+
#  coord_flip(xlim = c(100, 220), ylim = c(-0.4, 0.4))+
#  facet_grid(. ~ fishyear)+
#  scale_y_continuous(labels = percent_format(accuracy = 1, suffix = ''))+
#  geom_text(data = plotdat.2.n.2.join, aes(x = 200, y = 0.2, label = n), colour = 'black', inherit.aes = F, parse = F, size = 3.5)+
#  #add size limits for each time period
#  geom_vline(data = filter(plotdat.2, fishyear == 2000), aes(xintercept = 132),colour = 'red', linetype = 'dashed', size = 0.5)+
#  geom_vline(data = filter(plotdat.2, fishyear == 2005), aes(xintercept = 136),colour = 'red', linetype = 'dashed', size = 0.5)+
#  geom_vline(data = filter(plotdat.2, fishyear == 2010), aes(xintercept = 138),colour = 'red', linetype = 'dashed', size = 0.5)+
#  geom_vline(data = filter(plotdat.2, fishyear == 2015), aes(xintercept = 138),colour = 'red', linetype = 'dashed', size = 0.5)+
#  geom_vline(data = filter(plotdat.2, fishyear == 2019), aes(xintercept = 138),colour = 'red', linetype = 'dashed', size = 0.5)
#  #geom_vline(data = filter(plotdat.2, fishyear == 2010), aes(xintercept = 120),colour = 'darkgreen', linetype = 'dashed', size = 0.5)
#  #geom_vline(data = filter(plotdat.2, fishyear == 2015), aes(xintercept = 132),colour = 'red', linetype = 'dashed', size = 0.5)
#  # geom_vline(aes(xintercept = 127),colour = 'red', linetype = 'dashed', size = 0.5)+
#  # geom_text(data = plotdat.2.n, aes(x = 200, y = 100, label = n), colour = 'black', inherit.aes = F, parse = F, size = 3.5)
# 
# print(mm.zone.plot.2)
# 
# # 11. save plots to file
# 
# # setwd('C:/CloudStor/R_Stuff/MMLF/MM_Plots')
# ggsave(filename = paste(region.plot, zone, '_LF_FRAG2_2019', '.pdf', sep = ''), plot = mm.zone.plot.2, width = 7.4, height = 5.57, units = 'in')
# ggsave(filename = paste(region.plot, zone, '_LF_FRAG2_2019', '.wmf', sep = ''), plot = mm.zone.plot.2, width = 7.4, height = 5.57, units = 'in')
# 
# ## LF histrogram/boxplot
# 
# ## Overlay 5-year histrogram with longterm boxplot
# mm.zone.plot.3 <- grid.arrange(
#  arrangeGrob(cowplot::plot_grid(mm.zone.plot.2, mm.zone.boxplot, align = 'v', ncol = 1),
#              ncol = 1))
# 
# ggsave(filename = paste(zone, '_', region.plot, '_LF&boxplot', '.pdf', sep = ''), plot = mm.zone.plot.3)
# ggsave(filename = paste(zone, '_', region.plot, '_LF&boxplot', '.wmf', sep = ''), plot = mm.zone.plot.3)
#  
# #
# # 2019 MM abalone data - quick analysis of Tas Live Lobster catch April 2019
# #
# #
# ab.2019MM.raw.a <- read.csv('R:/TAFI/TAFI_MRL_Sections/Wild_Fisheries_Program/Shared/13. Market measuring/TassieLobster_08042019.csv')
# ab.2019MM.raw.b <- read.csv('R:/TAFI/TAFI_MRL_Sections/Wild_Fisheries_Program/Shared/13. Market measuring/TassieLobster_15042019.csv')
# 
# 
# ## create a backup
# ab.2019MM <- bind_rows(ab.2019MM.raw.a, ab.2019MM.raw.b)
# #ab.2019MM <- ab.2019MM.raw
# #ab.2019MM <- ab.2019MM.backup
# 
# # convert dates
# ab.2019MM$unloading_date <- as.POSIXct(ab.2019MM$unloading_date, format = '%d/%m/%Y')
# ab.2019MM$msr.date <- as.POSIXct(ab.2019MM$msr.date, format = '%m/%d/%Y')
# 
# ## add column for fishing year, month and quarter
# ab.2019MM$fishyear <- ifelse(is.na(ab.2019MM$msr.date), year(ab.2019MM$unloading_date),
#                              year(ab.2019MM$msr.date))
# ab.2019MM$fishmonth <- month(ab.2019MM$msr.date)
# ab.2019MM$fishquarter <- quarter(ab.2019MM$msr.date)
# 
# ## add 1 mm to shell.length for April samples - measuring board was 1 mm out and could not be calibrated withou passcode
# ab.2019MM$shell.length <- ab.2019MM$shell.length + 1
# 
# ## size frequency histogram
# plotdat.3 <- ab.2019MM %>% 
#  filter(zone_fishery_code == 'E') %>%
#  group_by(blocklist) %>%
#  mutate(n = n())
# 
# plotdat.3.n <- ab.2019MM %>% 
#  filter(zone_fishery_code == 'E') %>%
#  group_by(blocklist) %>%
#  summarise(n = paste('n =', n()))
# 
# plotdat.3$blocklist <- as.factor(plotdat.3$blocklist)
# 
# ggplot(plotdat.3, aes(shell.length, fill = blocklist))+
#  geom_histogram(data = subset(plotdat.3, zone_fishery_code == 'E'), colour = 'black',  binwidth = 2)+
#  #geom_histogram(data = subset(plotdat.3, blocklist == 29), fill = 'black', binwidth = 5)+
#  theme_bw()+
#  ylab("Frequency") +
#  xlab("Shell Length (mm)")+
#  xlim(100, 220)+
#  ylim(0, 30)+
#  facet_grid(blocklist ~., scales = "free_y")+
#  ggtitle('Tassie Live Lobster - April 2019')+
#  #add size limits for each time period
#  geom_vline(data = filter(plotdat.3, blocklist == 13), aes(xintercept = 138),colour = 'red', linetype = 'dashed', size = 1)+
#  geom_vline(data = filter(plotdat.3, blocklist == 22), aes(xintercept = 138),colour = 'red', linetype = 'dashed', size = 1)+
#  geom_vline(data = filter(plotdat.3, blocklist == 24), aes(xintercept = 138),colour = 'red', linetype = 'dashed', size = 1)+
#  geom_vline(data = filter(plotdat.3, blocklist == 27), aes(xintercept = 145),colour = 'red', linetype = 'dashed', size = 1)+
#  geom_vline(data = filter(plotdat.3, blocklist == 29), aes(xintercept = 138),colour = 'red', linetype = 'dashed', size = 1)+
#  geom_text(data = plotdat.3.n, aes(x = 200, y = 10, label = n), colour = 'black', inherit.aes = F, parse = F)+
#  theme(legend.position = 'none')
# #geom_vline(aes(xintercept = 140),colour = 'red', linetype = 'dashed', size = 1)
# 
# ## size frequency histogram proportion
# plotdat.3$bins <- cut(plotdat.3$shell.length, breaks = seq(138,195, 2), right = FALSE)
# 
# plotdat.4 <- plotdat.3 %>%
#  dplyr:::group_by(blocklist, bins) %>%
#  dplyr:::summarise(n = length(shell.length)) %>%
#  dplyr:::mutate(ngr = sum(n)) %>% 
#  dplyr:::mutate(prop = n/ngr*100)
# 
# 
# ggplot(plotdat.4, aes(x = bins, y = prop)) +
#  geom_bar(aes(fill = factor(blocklist)), position = 'dodge', stat = "identity", col = 'black') +
#  scale_y_continuous(
#   breaks = seq(0, 30, 5),
#   minor_breaks = seq(0 , 30, 1),
#   limits = c(0, 30),
#   expand = c(0, 0)
#  )  +
#  labs(x = "Shell length class",
#       y = "Proportion %",
#       title = "Tassie Live Lobster - April 2019",
#       fill = 'Block')+
#  theme_bw() +
#  theme(
#   axis.text.x = element_text(
#    size = 12,
#    color = "Black",
#    angle = 90,
#    vjust = 0.5
#   ),
#   axis.text.y = element_text(size = 14, color = "Black")
#  )
# 
# ggplot(plotdat.3, aes(x=shell.length, fill=as.factor(blocklist)))+ 
#  geom_density( kernel="gaussian", alpha=.5)+
#  labs(x = "Shell length class",
#       y = "Density",
#       title = "Tassie Live Lobster - April 2019",
#       fill = 'Block')
# 
# ## Processor milestone query
# 
# df.1 <- compiledMM.df.final %>%
#    filter(fishyear >= 2000) %>%
#    group_by(processorname, fishyear) %>%
#    summarise(n = n()) %>%
#    spread(key = fishyear, value = n)
#    
# write.csv(df.1, file = 'C:/CloudStor/R_Stuff/MMLF/FisheryDataProcessorSampling_MilestoneReport13.1.csv',
#           na = '') 
# 
# 
# ## Older plot stuff which can be deleted
# 
# # ## loop to generate boxplot of shell lengths for each zone and export as .pdf
# # zones <- unique(compiledMM.df$newzone)
# # compiledMM.df$fishyear <- as.factor(compiledMM.df$fishyear)
# # 
# # for (i in zones){
# #  zone.plot <- ggplot(subset(compiledMM.df, newzone == i & (between(shell.length, 132, 200))),
# #                      aes(x = fishyear, y = shell.length, group = fishyear)) +
# #   geom_boxplot(outlier.colour = "orange", outlier.size = 1.5) +
# #   #geom_hline(aes(yintercept = 140), colour = 'red', linetype = 'dotted') +
# #   #ggtitle(ifelse(as.character(i) == 'W', 'Western Zone', i)) +
# #   geom_text(aes(label = ..count..), y = 240, stat = 'count',  size = 3, angle = 90) +
# #   # ggtitle(i) +
# #   xlab('Year') +
# #   ylab('Shell length (mm)') + 
# #   theme_bw() + 
# #   theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
# #         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
# #         axis.text.x = element_text(angle = 45, vjust = 0.5))
# #  ggsave(filename = paste('MM_1967-2016_boxplot_', i, '.pdf', sep = ''), plot = zone.plot)
# #  dev.off()
# # }
# # 
# # ## loop to generate boxplot of black lip shell lengths for each block and export as .pdf
# # blocks <- unique(compiledMM.df$blockno)
# # compiledMM.df$fishyear <- as.factor(compiledMM.df$fishyear)
# # 
# # for (i in blocks){
# #  block.plot <- ggplot(subset(compiledMM.df, blockno == i & (between(shell.length, 100, 250)) & species == 1),
# #                      aes(x = fishyear, y = shell.length, group = fishyear)) +
# #   geom_boxplot(outlier.colour = "orange", outlier.size = 1.5) +
# #   geom_text(aes(label = ..count..), y = 240, stat = 'count',  size = 3, angle = 90) +
# #   ggtitle(paste('Block ', i)) +
# #   xlab('Year') +
# #   ylab('Shell length (mm)') + 
# #   ylim(100, 250) +
# #   theme_bw() + 
# #   theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
# #         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
# #         axis.text.x = element_text(angle = 45, vjust = 0.5))
# #  ggsave(filename = paste('MM_1967-2016_boxplot_BL_Block ', i, '.pdf', sep = ''), plot = block.plot)
# #  dev.off()
# # }
# # 
# # ## loop to generate boxplot of green lip shell lengths for each block and export as .pdf
# # blocks <- unique(compiledMM.df$blockno)
# # compiledMM.df$fishyear <- as.factor(compiledMM.df$fishyear)
# # 
# # for (i in blocks){
# #  block.plot <- ggplot(subset(compiledMM.df, blockno == i & (between(shell.length, 100, 250)) & species == 2),
# #                       aes(x = fishyear, y = shell.length, group = fishyear)) +
# #   geom_boxplot(outlier.colour = "orange", outlier.size = 1.5) +
# #   geom_text(aes(label = ..count..), y = 240, stat = 'count',  size = 3, angle = 90) +
# #   ggtitle(paste('Block ', i)) +
# #   xlab('Year') +
# #   ylab('Shell length (mm)') + 
# #   ylim(100, 250) +
# #   theme_bw() + 
# #   theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
# #         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
# #         axis.text.x = element_text(angle = 45, vjust = 0.5))
# #  ggsave(filename = paste('MM_1967-2016_boxplot_GL_Block ', i, '.pdf', sep = ''), plot = block.plot)
# #  dev.off()
# # }
# 
# # ## summary of median shell length for zone
# # plotdat.2.summary <- compiledMM.df %>% 
# #  #filter(fishyear %in% c(2000, 2005, 2010, 2013, 2015) & between(shell.length, 100, 220)) %>%
# #  filter(between(fishyear, 2000, 2015) & between(shell.length, 100, 220)) %>%
# #  group_by(newzone, fishyear) %>%
# #  summarise(median(shell.length))
# # 
# # ## plot proportion of catch by bin classes
# # plotdat.2$bins <- cut(plotdat.2$shell.length, breaks = seq(100,220, 5), right = FALSE)
# # 
# # test <- plotdat.2 %>%
# #  dplyr:::group_by(fishyear, bins) %>%
# #  dplyr:::summarise(n = length(shell.length)) %>%
# #  dplyr:::mutate(ngr = sum(n)) %>% 
# #  dplyr:::mutate(prop = n/ngr*100) %>%
# #  dplyr:::mutate(cum.prop = 100 - cumsum(prop))
# # 
# # 
# # ggplot(test, aes(x = bins, y = prop)) +
# #  geom_bar(stat = "identity") +
# #  labs(x = "Shell length (mm)", y = "Percentage") +
# #  theme_bw() +
# #  facet_grid(fishyear~.)
# # 
# # 
# # 
# 
# ## quick plot to compare with data from 2019
#  plotdat.3 <- compiledMM.df %>% filter(newzone == 'E' & fishyear == c(2015) & blockno == c(22, 24, 27, 29))
#  ggplot(plotdat.3, aes(shell.length))+
#   geom_histogram()
#   #geom_histogram(data = subset(plotdat.2, fishyear == '2015'), fill = 'white', colour = 'black',  binwidth = 5)+
#   #geom_histogram(data = subset(plotdat.3, blockno == c(22, 24, 27, 29), fill = 'black', binwidth = 5))+
#   theme_bw()+
#   ylab("Frequency") +
#   xlab("Shell Length (mm)")+
#   xlim(100, 220)+
#   facet_grid(blockno ~ ., scales = "free_y")+
#   ggtitle('Eastern Zone 2000-2015 vs Block 29') +
#   #add size limits for each time period
#   # geom_vline(data = filter(plotdat.2, fishyear == 2000), aes(xintercept = 132),colour = 'red', linetype = 'dashed', size = 1)+
#   # geom_vline(data = filter(plotdat.2, fishyear == 2005), aes(xintercept = 136),colour = 'red', linetype = 'dashed', size = 1)+
#   # geom_vline(data = filter(plotdat.2, fishyear == 2010), aes(xintercept = 138),colour = 'red', linetype = 'dashed', size = 1)+
#   # geom_vline(data = filter(plotdat.2, fishyear == 2015), aes(xintercept = 138),colour = 'red', linetype = 'dashed', size = 1)
#  geom_vline(aes(xintercept = 138),colour = 'red', linetype = 'dashed', size = 1)
#  
# ## summary to determine % of catches measured
# landings <- tasCE %>% 
#  filter(gl.zone == 'G' & fishyear >= 2000) %>%
#  group_by(fishyear, blockno) %>%
#  summarise(landings = n())
# 
# measured <- compiledMM.df %>%
#  #mutate(fishyear = as.numeric(levels(fishyear)[fishyear])) %>%
#  filter(newzone == 'G' & fishyear >= 2000) %>%
#  group_by(fishyear, blockno) %>%
#  summarise(measured = length(unique(docket.number)))
# 
# measured_summary <- left_join(landings, measured, c("fishyear", "blockno")) %>%
#  mutate(meas.propland = round((measured/landings)*100, 0))
#  #filter(blockno == 13) %>%
#  #filter(fishyear %in% c(2000, 2005, 2010, 2015))
# 
# # ## proportion of catches measured summary
# # abcebl.docks <- abCEbl %>%
# #  group_by(newzone, fishyear) %>%
# #  summarise(bl.n = n_distinct(docket_number))
# # 
# # abcegl.docks <- abCEgl %>%
# #  group_by(newzone, fishyear) %>%
# #  summarise(gl.n = n_distinct(docket_number))
# # 
# # mm.docks <- compiledMM.df %>%
# #  group_by(newzone, fishyear) %>%
# #  summarise(mm.n = n_distinct(docket.number))
# # 
# # n.docks <- full_join(abcebl.docks, mm.docks) %>%
# #  left_join(abcegl.docks) %>%
# #  mutate(prop.measured = ifelse(newzone == 'G', round((mm.n/gl.n)*100, 0), round((mm.n/bl.n)*100, 0))) %>%
# #  filter(between(fishyear, 2000, 2015) & newzone == 'N')
# # #filter(fishyear %in% c(2000, 2005, 2010, 2015))
# 
# ## boxplot of size structure for block vs overall for year(need to overlay bar plot of %landings measured)
# plotdat.3a <- compiledMM.df %>% 
#  filter(blockno == 13 & fishyear == c(2000, 2005, 2010, 2015)) %>%
#  mutate(join.code = 'block.13') %>%
#  select(fishyear, join.code, shell.length)
# 
# plotdat.3b <- compiledMM.df %>% 
#  filter(newzone == 'E' & fishyear == c(2000, 2005, 2010, 2015)) %>%
#  mutate(join.code = 'eastern.zone') %>%
#  select(fishyear, join.code, shell.length)
# 
# plotdat.4 <- bind_rows(plotdat.3a, plotdat.3b)
# 
# ggplot(plotdat.4, aes(x = fishyear, y = shell.length)) + 
#  geom_boxplot(aes(fill = join.code), outlier.colour = "orange", outlier.size = 1.5, 
#               width = .6)+
#  theme_bw()+
#  ylim(125, 220)+
#  ylab("Shell length (mm)") +
#  xlab("Year")
#  geom_bar(data = measured_summary, aes(x = fishyear, y = meas.propland), stat = 'identity')+
#  scale_y_continuous(sec.axis = sec_axis(~./50))
#  
#  
# ggplot(plotdat.4, aes(x = fishyear, y = shell.length)) +
#   + geom_point()+
#   + stat_summary(fun.data = my.stderr, geom = 'point', colour = 'red')
# 
#        
#  
# 
# 
