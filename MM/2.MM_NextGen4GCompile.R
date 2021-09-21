# The following script collates all available .txt files uploaded to the sftp server from the Next Gen 4G measuring
# boards. These data can then be joined to historical length frequency data and associated docket information
# in MM_HistoricalDataCompile.R.

# Created by: Craig Mundy and Jaime McAllister

##---------------------------------------------------------------------------##
# clear console ####
rm(list = ls())

# load libaries ####
suppressPackageStartupMessages({
# library(sftp)
library(RCurl)
library(tidyverse)
library(fs)
library(keyring)
library(tools)
library(R.utils)
# library(RDCOMClient)
library(openxlsx)
library(fuzzyjoin)
library(lubridate)
library(vroom)
library(readtext)
library(quanteda)
library(splitstackshape)
})
##---------------------------------------------------------------------------##
## Local working folder ####

sftp.local <- "R:/TAFI/TAFI_MRL_Sections/Abalone/AbTrack/RawData/NextGen/Data/sftpServerCopy"

##---------------------------------------------------------------------------##
## Extract .txt files ####

## Extract data from sftp download folder and compile into dataframe
## Note: measuring board .txt files are denoted with '07' prefix

localfiles <- list.files(sftp.local,  pattern = "^07.*txt", full.names = T) 

localfiles.dat <- lapply (localfiles, read.table, sep = ",", header = F, row.names = NULL, as.is = T,
                  colClasses = c("character", "numeric", "numeric", "numeric", "character", "character"))
localfiles.df <- do.call(rbind, localfiles.dat)

logged.data <- localfiles.df

##---------------------------------------------------------------------------##
## Extract info from data packet ####

# rename variable names of dataframe

colnames(logged.data) <- c("logname", "seqindex","identifier","rawutc","datapack","crc_status")

# # identify measuring board logger names
# 
# unique(logged.data$logname)
# 
# # identify number of records for each identifier
# table(logged.data$identifier)

logged.data$logger_date <- as.POSIXct(logged.data$rawutc, origin = "1970-01-01", tz = "GMT")
logged.data$local_date <- as.POSIXct(logged.data$rawutc, origin = "1970-01-01", tz = "Australia/BRISBANE")
logged.data <- logged.data %>%  
        mutate(plaindate = as.Date(local_date, tz="Australia/BRISBANE"))

# tail(logged.data)

##---------------------------------------------------------------------------##
## Check LoggerName data ####

logname <- filter(logged.data, identifier == 32962) 
logname <-  separate(logname, datapack, c("abalonenum","devicename"), sep = ",", remove = FALSE,
                     convert = TRUE) %>%
 as.data.frame()

# tail(logname)

##---------------------------------------------------------------------------##
## Step 2: Extract Battery voltage  ####

loggerbattery <- filter(logged.data, identifier %in% c(32833) ) %>%
 separate(datapack, c("volts"), sep = ",", remove = FALSE,
          convert = FALSE) %>%
 arrange(logname,local_date) %>% 
 as.data.frame()

# tail(loggerbattery)

##---------------------------------------------------------------------------##
## Step 3A: Extract GPS RMC Part A  ####
gps.RMC_A <- filter(logged.data, identifier == 220) %>%
 separate(datapack, c("longitude","latitude"), sep = ",", remove = FALSE,
          convert = FALSE) %>%
 as.data.frame()

# tail(gps.RMC_A)

##---------------------------------------------------------------------------##
## Step 3B: Extract GPS RMC Part B ####
gps.RMC_B <- filter(logged.data, identifier == 221) %>%
 separate(datapack, c("valid", "speed", "course", "variation"), sep = ",", remove = FALSE,
          convert = FALSE) %>%
 as.data.frame()

# tail(gps.RMC_B)

##---------------------------------------------------------------------------##
## Step 3C: Join RMC Part A & B   ####

gps.RMC <- left_join(gps.RMC_B, select(gps.RMC_A, logname, local_date, longitude, latitude), 
                     by=c("local_date", 'logname')) 

# remove duplicate records resulting from upload failures or loggers going out of range 
# and filter out measuring board records
gps.RMC <- gps.RMC %>%
        distinct(logname, seqindex, identifier, rawutc, .keep_all = T) %>% 
        filter(grepl('^07', logname))

# tail(gps.RMC)
# 
# table(gps.RMC$plaindate)
# 
# pick <- which(duplicated(gps.RMC$local_date) == TRUE)
# gps.RMC[pick,]

##---------------------------------------------------------------------------##
## Step 4: Extract Docket details ####
docket <- filter(logged.data, identifier == 32963) 
docket <-  separate(docket, datapack, c("abalonenum", "zone", "docketnum"), sep = ",", remove = FALSE,
                     convert = TRUE) %>%
 as.data.frame()

# remove duplicate records resulting from upload failures or loggers going out of range
docket <- docket %>% 
        distinct(logname, seqindex, identifier, rawutc, .keep_all = T)

# tail(docket)

##---------------------------------------------------------------------------##
## Step 5: Extract Weight length ####
ablength <- filter(logged.data, identifier == 32964) 
ablength <-  separate(ablength, datapack, c("abalonenum","shelllength"), sep = ",", remove = FALSE,
                     convert = TRUE) %>%
 as.data.frame()

# remove duplicate records resulting from upload failures or loggers going out of range
ablength <- ablength %>% 
        distinct(logname, seqindex, identifier, abalonenum, .keep_all = T)

# tail(ablength)

##---------------------------------------------------------------------------##
## Step 6: Extract Weight  ####
abweight <- filter(logged.data, identifier == 32965) 
abweight <-  separate(abweight, datapack, c("abalonenum","wholeweight"), sep = ",", remove = FALSE,
                      convert = TRUE) %>%
 as.data.frame()

# remove duplicate records resulting from upload failures or loggers going out of range
abweight <- abweight %>% 
        distinct(logname, seqindex, identifier, abalonenum, .keep_all = T)

# tail(abweight)

##---------------------------------------------------------------------------##
## Step 7: Join components into a flat form ####

lengthweight <- left_join(select(gps.RMC, logname, rawutc, logger_date, local_date, plaindate, latitude, longitude),
                          select(logname, logname, rawutc, abalonenum), by = c('logname', "rawutc")) %>%      
 left_join(select(docket, logname, rawutc, zone, docketnum), by = c('logname', "rawutc")) %>%   
 left_join(select(ablength, logname, rawutc,shelllength), by = c('logname', "rawutc")) %>%  
 left_join(select(abweight, logname, rawutc, wholeweight), by = c('logname', "rawutc"))

# tail(lengthweight)

measure.board.df <- lengthweight

##---------------------------------------------------------------------------##
## Step 8: clean and save dataframe ####

# remove records with missing docketnum
measure.board.df <- measure.board.df %>% 
        filter(!is.na(docketnum))

# remove measuring board testing data (pre-deployment logger names and random test docket numbers) 
measure.board.df <- measure.board.df %>% 
        filter(logname %in% c(7020055, 7020056, 7020057, 7020058) | 
                       !docketnum %in% c(45575, 111111, 123456, 222222, 323232, 
                                         333333, 454545, 474747, 555555, 565656,
                                         616161, 654321, 666666, 811881, 369852, 0))

# fix any known errors with measureboard data
# Steve Crocker (Tassie Live Lobster) notified me that he had entered an incorrect docket number
# Simon Leonard (RTS) informed me that zone for docket number 812576 should be AW not AN
# Mick (Tas Seafoods) informed me that zone for docket number 525708 should be AE not AW
# Mick (Tas Seafoods) informed me that zone for docket number 812883 should be AW not AE
# Mark Fleming (Tas Seafoods) notified me that he had entered an incorrect docket number (811698 to 811691)
# Mark Fleming (Tas Seafoods) notified me that he had entered an incorrect docket number (801875 to 810875)
# Mark Fleming (Tas Seafoods) notified me that he had entered an incorrect docket number (524522 to 524552)
measure.board.df <- measure.board.df %>% 
        mutate(docketnum = replace(docketnum, docketnum == 812222, 812227),
               docketnum = replace(docketnum, docketnum == 811698, 811691),
               docketnum = replace(docketnum, docketnum == 801875, 810875),
               docketnum = replace(docketnum, docketnum == 524522, 524552),
               zone = if_else(docketnum == 812576, 'AW', 
                              if_else(docketnum == 525708, 'AE', 
                                      if_else(docketnum == 812883, 'AW', 
                                              if_else(docketnum == 524401, 'AE', zone)))))

# Meahgan Dodd (RTS) informed me of an error with several lengths for docket number 809708
measure.board.df <- measure.board.df %>% 
        mutate(remove.record = if_else(docketnum == 809078 & 
                                               abalonenum %in% c(64:67, 95), 1, 0)) %>% 
        filter(remove.record == 0) %>% 
        select(-remove.record)

# Incorrect sample for docket number 812803 at Seafood Traders 2021-03-15
measure.board.df <- measure.board.df %>% 
        mutate(remove.record = if_else(docketnum == 812803 &
                       plaindate == as.Date('2021-03-15'), 1, 0)) %>%   
        filter(remove.record == 0) %>% 
        select(-remove.record)

# combine data from multiple boards measuring the same docketnum at Steve Crocker (Tassie Live Lobster) on 2020-01-22
# also combine multiple samples from Seafood Traders on 2021-02-22 where the measuring board may have restarted for
# docket AW812802
# adjust abalone number where not in sequence from 0:n()
multi.board.abnum <- measure.board.df %>% 
        filter(docketnum %in% c('812108', '812302', '523229', '812204', 
                                '523630', '519218', '812628', '809078',
                                '811918', '522956', '812456', '523108',
                                '512188', '523185', '812802')) %>% 
        arrange(local_date, docketnum) %>% 
        dplyr::group_by(docketnum) %>% 
        mutate(abalonenum = row_number() - 1)

measure.board.df.single.board <- measure.board.df %>% 
        filter(!docketnum %in% c('812108', '812302', '523229', '812204', 
                                 '523630', '519218', '812628', '809078',
                                 '811918', '522956', '812456', '523108',
                                 '512188', '523185', '812802'))

measure.board.df <- bind_rows(multi.board.abnum, measure.board.df.single.board)

## remove unknown but likely factory test samples
measure.board.df <- measure.board.df %>% 
        filter(logname != 07020057 &
                       !rawutc %in% c(1589771875, 1589771877))

## remove duplicate samples (i.e. AW809078)
measure.board.df <- measure.board.df %>% 
        distinct()

# adjust GPS dropout for 07-02-0056 on 2020-10-26 (Scielex suspect 4G antenna proximity to GPS) 
measure.board.df <- measure.board.df %>% 
        mutate(logger_date = if_else(docketnum == 521218 &
                                           zone == 'AE' &
                                           plaindate == as.Date('2027-07-05') &
                                           logname == '07020056',
                                   ymd_hms(gsub('2027-07-05', '2020-10-26', logger_date)), ymd_hms(logger_date)),
               local_date = if_else(docketnum == 521218 &
                                             zone == 'AE' &
                                             plaindate == as.Date('2027-07-05') &
                                             logname == '07020056',
                                     ymd_hms(gsub('2027-07-05', '2020-10-26', local_date)), ymd_hms(local_date)),
               plaindate = if_else(docketnum == 521218 &
                                             zone == 'AE' &
                                             plaindate == as.Date('2027-07-05') &
                                             logname == '07020056',
                                     ymd('2020-10-26'), plaindate))

# remove weights from Tas Seafoods measurements between 2020-07-03 and 2020-10-19 where the inferior scales were
# used resulting in unreliable and erroneous weights being recorded.

measure.board.df <- measure.board.df %>% 
        mutate(wholeweight = if_else(between(plaindate, as.Date('2020-07-03'), as.Date('2020-10-19')) &
                                             logname == '07020058', 0, wholeweight))

##---------------------------------------------------------------------------##
# identify dockets sampled > 1 per day (e.g. catch measured from separate holding tanks)
docketnum.samp.day <- measure.board.df %>%
        filter(!is.na(docketnum)) %>%
        group_by(docketnum, abalonenum, plaindate) %>% 
        summarise(n.day = n()) %>% 
        filter(abalonenum == 0) %>% 
        ungroup() %>% 
        select(docketnum, n.day) 

multi.samp.day <- left_join(measure.board.df, docketnum.samp.day) %>% 
        filter(!is.na(docketnum),
               n.day > 1,
               abalonenum == 0) %>% 
        group_by(docketnum) %>% 
        arrange(local_date) %>% 
        mutate(samp.no.day = row_number())

multi.samp.day.df <- left_join(measure.board.df, docketnum.samp.day) %>% 
        filter(!is.na(docketnum),
               n.day > 1) %>% 
        left_join(., multi.samp.day) %>% 
        fill(samp.no.day)

measure.board.df <- left_join(measure.board.df, multi.samp.day.df) %>% 
        mutate(docketnum.day = if_else(!is.na(samp.no.day), 
                                       paste(docketnum, samp.no.day, sep = '_'), 
                                       as.character(docketnum))) %>% 
        select(-c(n.day, samp.no.day))
##---------------------------------------------------------------------------##
## Step 9: assign measuring board to processor ####

# load inventory of processors which held measuring boards 

mb.invent <- read.xlsx("C:/CloudStor/R_Stuff/MMLF/IMAS_measuringboard_log_inventory.xlsx",
                       detectDates = T)

# where measuring board is still with processor and the enddate is missing replace with todays date

mb.invent <- mb.invent %>% 
        mutate(startdate = as.POSIXct(startdate),
               enddate = as.POSIXct(enddate),
               enddate = if_else(is.na(enddate), Sys.time(), enddate))

# join measuring board raw data with inventory data 

measure.board.next.gen.df <- fuzzy_left_join(
        measure.board.df,
        mb.invent,
        by = c(
                "logname" = "logname",
                "logger_date" = "startdate",
                "logger_date" = "enddate"
        ),
        match_fun = list(`==`, `>=`, `<=`)
) %>% select(-c(logname.y, startdate, enddate, platformscales, comments)) %>%
        mutate(logname = logname.x) %>%
        select(-(logname.x)) %>%
        filter(!is.na(docketnum))

# measure.board.next.gen.df <- measure.board.next.gen.df %>% 
#         arrange(logname, local_date, processor) %>% 
#         mutate(sample.id = cumsum(c(-1, diff(abalonenum)) < 0)) 


# ## determine number of abalone in each sample and join
# samp.id.no <- measure.board.next.gen.df %>% 
#         group_by(docketnum.day, processor, plaindate) %>% 
#         summarise(sample.n = n())
# 
# measure.board.next.gen.df <- left_join(measure.board.next.gen.df, samp.id.no)

# add docket.index
measure.board.next.gen.df <- measure.board.next.gen.df %>% 
        mutate(docket.index = paste(zone, docketnum.day, plaindate, processor, sep = '-'))

##---------------------------------------------------------------------------##
## Step 10: incomplete uploads ####
## search previous compiled dataframe for incomplete uploads for a docket number to pass onto
## processor summary script

# load previous measuring board data that comes from MM_NextGen_4G.R script
measure.board.next.gen.df.old <- readRDS('C:/CloudStor/R_Stuff/MMLF/measure.board.next.gen.df.RDS')

docket.incomplete <- measure.board.next.gen.df.old %>% 
        group_by(zone, docketnum, plaindate, processor) %>%  
        summarise(abalonenum.start = min(abalonenum),
                  abalonenum.end = max(abalonenum),
                  n = n()) %>%   
        filter(n < 100 & #search for samples with <100 abalone
                       abalonenum.start == 0 , #include samples where start number is zero
                       !docketnum %in% c(523229, 523632, 812108, 813512, 524261, 812204, 812716,
                                         814356, 812647, 812456, 811918, 809078, 525739, 523951, 
                                         523274, 523185, 523108, 522956, 519218, 515571, 512188,
                                         523630, 519891, 520017, 523338, 523645, 523857, 523860,
                                         524555, 524903, 607952, 812334, 812647, 813158, 519891,
                                         520017, 522969, 523605, 523646, 523858, 523954, 524679, 
                                         524904, 809423, 812647, 812932, 524621))  #remove samples where manual check of raw data found no refresh/or additional data
        # pull(docketnum)

saveRDS(docket.incomplete, 'C:/CloudStor/R_Stuff/MMLF/docket.incomplete.RDS')

##---------------------------------------------------------------------------##
## Step 11: save RDS of dataframe ####

saveRDS(measure.board.next.gen.df, 'C:/CloudStor/R_Stuff/MMLF/measure.board.next.gen.df.RDS')

##---------------------------------------------------------------------------##
measure.board.next.gen.df %>% 
        arrange(local_date) %>% 
        tail()

