# The following script collates all available .txt files uploaded to the sftp server from the Next Gen 4G measuring
# boards. These data can then be joined to historical length frequency data and associated docket information
# in MM_HistoricalDataCompile.R.

# Created by: Craig Mundy and Jaime McAllister

##---------------------------------------------------------------------------##
# load libaries ####

library(sftp)
library(RCurl)
library(tidyverse)
library(fs)
library(keyring)
library(tools)
library(R.utils)
library(RDCOMClient)
library(openxlsx)
library(fuzzyjoin)
library(lubridate)
library(vroom)
library(readtext)
library(quanteda)

##---------------------------------------------------------------------------##
## Local working folder ####

sftp.local <- "R:/TAFI/TAFI_MRL_Sections/Abalone/AbTrack/RawData/sftpServer/FilesNew"

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

tail(logname)

##---------------------------------------------------------------------------##
## Step 2: Extract Battery voltage  ####

loggerbattery <- filter(logged.data, identifier %in% c(32833) ) %>%
 separate(datapack, c("volts"), sep = ",", remove = FALSE,
          convert = FALSE) %>%
 arrange(logname,local_date) %>% 
 as.data.frame()

tail(loggerbattery)

##---------------------------------------------------------------------------##
## Step 3A: Extract GPS RMC Part A  ####
gps.RMC_A <- filter(logged.data, identifier == 220) %>%
 separate(datapack, c("longitude","latitude"), sep = ",", remove = FALSE,
          convert = FALSE) %>%
 as.data.frame()

tail(gps.RMC_A)

##---------------------------------------------------------------------------##
## Step 3B: Extract GPS RMC Part B ####
gps.RMC_B <- filter(logged.data, identifier == 221) %>%
 separate(datapack, c("valid","speed", "course","variation"), sep = ",", remove = FALSE,
          convert = FALSE) %>%
 as.data.frame()

tail(gps.RMC_B)

##---------------------------------------------------------------------------##
## Step 3C: Join RMC Part A & B   ####

gps.RMC <- left_join(gps.RMC_B, select(gps.RMC_A, logname, local_date, longitude, latitude), 
                     by=c("local_date", 'logname')) 

# remove duplicate records resulting from upload failures or loggers going out of range 
# and filter out measuring board records
gps.RMC <- gps.RMC %>%
        distinct(logname, seqindex, identifier, .keep_all = T) %>%  
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
        distinct(logname, seqindex, identifier, .keep_all = T)

tail(docket)

##---------------------------------------------------------------------------##
## Step 5: Extract Weight length ####
ablength <- filter(logged.data, identifier == 32964) 
ablength <-  separate(ablength, datapack, c("abalonenum","shelllength"), sep = ",", remove = FALSE,
                     convert = TRUE) %>%
 as.data.frame()

# remove duplicate records resulting from upload failures or loggers going out of range
ablength <- ablength %>% 
        distinct(logname, seqindex, identifier, abalonenum, .keep_all = T)

tail(ablength)

##---------------------------------------------------------------------------##
## Step 6: Extract Weight  ####
abweight <- filter(logged.data, identifier == 32965) 
abweight <-  separate(abweight, datapack, c("abalonenum","wholeweight"), sep = ",", remove = FALSE,
                      convert = TRUE) %>%
 as.data.frame()

# remove duplicate records resulting from upload failures or loggers going out of range
abweight <- abweight %>% 
        distinct(logname, seqindex, identifier, abalonenum, .keep_all = T)

tail(abweight)

##---------------------------------------------------------------------------##
## Step 7: Join components into a flat form ####

lengthweight <- left_join(select(gps.RMC, logname, rawutc, logger_date, local_date, plaindate, latitude, longitude),
                          select(logname, logname, rawutc, abalonenum), by = c('logname', "rawutc")) %>%    
 left_join(select(docket, logname, rawutc, zone, docketnum), by = c('logname', "rawutc")) %>%  
 left_join(select(ablength, logname, rawutc,shelllength), by = c('logname', "rawutc")) %>% 
 left_join(select(abweight, logname, rawutc, wholeweight), by = c('logname', "rawutc"))

tail(lengthweight)

measure.board.df <- lengthweight

##---------------------------------------------------------------------------##
## Step 8: clean and save dataframe ####

# remove measuring board testing data (pre-deployment logger names and random test docket numbers) 
measure.board.df <- measure.board.df %>% 
        filter(logname %in% c(7020055, 7020056, 7020057, 7020058) | 
                       !docketnum %in% c(45575, 111111, 123456, 222222, 323232, 
                                         333333, 454545, 474747, 555555, 565656,
                                         616161, 654321, 666666, 811881))

# fix any known errors with measureboard data
# Steve Crocker (Tassie Live Lobster) notified me that he had entered an incorrect docket number
measure.board.df <- measure.board.df %>% 
        mutate(docketnum = replace(docketnum, docketnum == 812222, 812227))



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

##---------------------------------------------------------------------------##
## Step 10: save RDS of dataframe ####

saveRDS(measure.board.next.gen.df, 'C:/CloudStor/R_Stuff/MMLF/measure.board.next.gen.df.RDS')

