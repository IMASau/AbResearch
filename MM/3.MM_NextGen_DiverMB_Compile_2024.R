##---------------------------------------------------------------------------##
# load libaries ####

suppressPackageStartupMessages({
library(RCurl)
library(fs)
library(keyring)
library(tools)
library(R.utils)
library(openxlsx)
library(fuzzyjoin)
library(lubridate)
library(vroom)
library(readtext)
library(quanteda)
library(openxlsx)
library(fuzzyjoin)
library(lubridate)
library(tidyr)
library(dplyr)
library(ggsci)
library(ggpubr)
library(scales)
library(RODBC)
library(tictoc)
 library(ggspatial)
 library(tmap)
 library(sf)
 library(sp)
 # library(rgdal)
  library(gridExtra)
 library(tidyverse)
 library(progress)
 library(furrr)
 library(purrr)
 library(keyring)
 library(odbc)
})

## Set source location of functions to be used in script
source("c:/GitCode/AbSpatial/AbTrack/MapNextgen_Queries_v1.R")

##---------------------------------------------------------------------------##
## Local working folder ####

mm_data_folder <- paste(sprintf('C:/Users/%s/Dropbox (UTAS Research)/DiveFisheries/Abalone/MMdata/',
                                Sys.info()[["user"]]))

##---------------------------------------------------------------------------##
## Use the NG_DB_RawData_extract.R code to compile data from the AbTrack ##
##---------------------------------------------------------------------------##

## Establish connection to database
conn <- DBI::dbConnect(odbc(), "AbTrack")

# Set year
year = 2025

# Import data
dstream <-
 DBI::dbGetQuery(conn,
                 "SELECT * FROM NG_DATA_STREAM_VIEW\nwhere NG_DS_Year = ?",
                 params = year)
toc()
dbDisconnect(conn)

# Force time zone changes
dstream$Logger_Date <- force_tz(dstream$Logger_Date, tzone="GMT")
dstream$Local_Date <- force_tz(dstream$Local_Date, tzone="Australia/BRISBANE")

# Filter for columns and rename to match data_packet
data_packet <- dstream %>% select(10:17) %>% 
 rename(logname = Logname, seqindex = SeqIndex, identifier = Identifier, 
        logger_date = Logger_Date, local_date = Local_Date)

# Remove dstream
# rm(dstream)

## Remove duplicate records 
# NB. local_date needed as seq index cyclical
data_packet <- rowid_to_column(data_packet, "RowNum")
dupes <- data_packet[duplicated(data_packet[,c("logname", "seqindex", "identifier", "local_date")]),]
data_packet <- filter(data_packet, !RowNum %in% c(dupes$RowNum))
unique(dupes$logname)

## Filter for measuring board data - list of NextGen measuring boards post 2024
logged_data <- data_packet %>% 
 filter(logname %in% c('10010008',
                       '10010012',
                       '10010017',
                       '10010018',
                       '10010019',
                       '10010020',
                       '10010021'))

## Save logged_data
save(logged_data, file = paste(mm_data_folder, 'measuringboard_logged_data_', Sys.Date(), '.RData', sep = ''))

## Load logged_data
myFile <- dir_info(mm_data_folder, recurse = FALSE, glob = "*.RData") %>%
 filter(type == "file",  size > "10KB") %>%
 arrange(desc(modification_time)) %>%
 filter(str_detect(path, "measuringboard_logged_data_202")) %>% 
 filter(modification_time == max(modification_time))

# print(myFile$path)
load(myFile$path)

# Set time zones
logged_data$logger_date <- as.POSIXct(logged_data$rawutc, origin = "1970-01-01", tz = "GMT")
logged_data$local_date <- as.POSIXct(logged_data$rawutc, origin = "1970-01-01", tz = "Australia/BRISBANE")
logged_data <- logged_data %>%  
 mutate(plaindate = as.Date(local_date, tz="Australia/BRISBANE"))

##---------------------------------------------------------------------------##
## Extract info from data packets ####
##---------------------------------------------------------------------------##
## Step 1: Check LoggerName data ####

log_name <- logged_data %>% 
 filter(plaindate > as.Date('2024-01-01') & 
         identifier == 33217)

log_name <-  separate(log_name, datapack, c("abalonenum", "devicename"), sep = ",", remove = FALSE,
                     convert = TRUE) %>%
 as.data.frame()

# remove duplicate records resulting from upload failures or loggers going out of range
log_name <- log_name %>% 
  distinct(logname, seqindex, identifier, .keep_all = T)

# tail(log_name)

##---------------------------------------------------------------------------##
## Step 2: Extract Battery voltage  ####

loggerbattery <- logged_data %>% 
 filter(plaindate > as.Date('2024-01-01') &
         identifier == 33235) %>%  
 separate(datapack, c("volts"), sep = ",", remove = FALSE,
          convert = FALSE) %>%
 arrange(logname,local_date) %>% 
 as.data.frame()

# tail(loggerbattery)

##---------------------------------------------------------------------------##
## Step 3A: Extract GPS RMC Part A  ####
gps_RMC_A <- filter(logged_data, identifier == 220) %>%
 separate(datapack, c("longitude", "latitude"), sep = ",", remove = FALSE,
          convert = FALSE) %>%
 as.data.frame()

gps_RMC_A <- gps_RMC_A %>% 
  distinct(logname, rawutc, longitude, latitude, .keep_all = T)

# tail(gps_RMC_A)

##---------------------------------------------------------------------------##
## Step 3B: Extract GPS RMC Part B ####
gps_RMC_B <- filter(logged_data, identifier == 221) %>%
 separate(datapack, c("valid","speed", "course", "variation"), sep = ",", remove = FALSE,
          convert = FALSE) %>%
 as.data.frame()

gps_RMC_B <- gps_RMC_B %>% 
  distinct(logname, rawutc, .keep_all = T)

# tail(gps_RMC_B)

##---------------------------------------------------------------------------##
## Step 3C: Join RMC Part A & B   ####

gps_RMC <- left_join(gps_RMC_B, select(gps_RMC_A, logname, rawutc, longitude, latitude), 
                     by = c('logname', 'rawutc')) 


# remove duplicate records resulting from upload failures or loggers going out of range 
# and filter out measuring board records
mb_lognames <- c('^07', '^10')

gps_RMC <- gps_RMC %>%
  distinct(logname, seqindex, identifier, .keep_all = T) %>% 
  filter(grepl(paste(mb_lognames, collapse = '|'), logname))

# tail(gps.RMC)

##---------------------------------------------------------------------------##
## Step 4: Extract Docket details ####
 
docket <- logged_data %>% 
 filter(plaindate > as.Date('2024-01-01') &
         identifier == 33219)

docket <-  docket %>% 
 group_split(newdock = plaindate > as.Date('2024-01-01')) %>% 
 map(function(d) {
  into = if (d$newdock[1]) {
   c("abalonenum", "docketnum")
  } else {
   c("abalonenum", "zone", "docketnum")
  }
  separate(d, datapack, into = into, sep = ",", remove = FALSE, convert = TRUE)
 }) %>% 
   bind_rows() %>% 
   select(-newdock)

##---------------------------------------------------------------------------##
## Step 5: Extract Zone ####

zone <- logged_data %>% 
 filter(plaindate > as.Date('2024-01-01') &
         identifier == 33218)

zone <-  zone %>% 
 group_split(newzone = plaindate > as.Date('2024-01-01')) %>% 
 map(function(d) {
  into = if (d$newzone[1]) {
   c("abalonenum", "zone")
  } else {
   c("abalonenum", "zone", "docketnum")
  }
  separate(d, datapack, into = into, sep = ",", remove = FALSE, convert = TRUE)
 }) %>% 
 bind_rows() %>% 
 select(c(abalonenum, logname, rawutc, seqindex, zone))

# Re-join docket and zone details
docket <- left_join(docket, zone, by = c('abalonenum', "rawutc", 'logname'))

# Remove duplicate records resulting from upload failures or loggers going out of range
docket <- docket %>% 
 distinct(abalonenum, logname, rawutc, identifier, .keep_all = T)

# tail(docket)


##---------------------------------------------------------------------------##
## Step 6: Extract length ####

ablength <- logged_data %>% 
 filter(plaindate > as.Date('2024-01-01') &
         identifier == 33220)

ablength <-  separate(ablength, datapack, c("abalonenum", "shelllength"), sep = ",", remove = FALSE,
                      convert = TRUE) %>%
 as.data.frame()

# remove duplicate records resulting from upload failures or loggers going out of range
ablength <- ablength %>% 
  distinct(logname, seqindex, identifier, abalonenum, .keep_all = T)

# tail(ablength)

##---------------------------------------------------------------------------##
## Step 7: Extract Weight  ####

abweight <- logged_data %>% 
 filter(plaindate > as.Date('2024-01-01') &
         identifier == 33221)

abweight <-  separate(abweight, datapack, c("abalonenum", "est.weight"), sep = ",", remove = FALSE,
                      convert = TRUE) %>%
 as.data.frame()

# remove duplicate records resulting from upload failures or loggers going out of range
abweight <- abweight %>% 
  distinct(logname, seqindex, identifier, abalonenum, .keep_all = T)

# tail(abweight)

##---------------------------------------------------------------------------##
## Step 8: Join components into a flat form ####

lengthweight <- left_join(select(gps_RMC, logname, rawutc, logger_date, local_date, plaindate, latitude, longitude),
                          select(log_name, logname, rawutc, abalonenum), by = c("rawutc", 'logname')) %>%    
 left_join(select(docket, logname, rawutc, zone, docketnum, abalonenum), by = c("rawutc", 'logname', 'abalonenum')) %>%  
 left_join(select(ablength, logname, rawutc, shelllength, abalonenum), by = c("rawutc", 'logname', 'abalonenum')) %>%  
 left_join(select(abweight, logname, rawutc, est.weight, abalonenum), by = c("rawutc", 'logname', 'abalonenum'))

# Remove measuring board test data and records with no length or weight data

test_dockets <- c(111111, 123123)

lengthweight <- lengthweight %>% 
 filter(!docketnum %in% test_dockets & 
           !is.na(abalonenum))

mb_df <- lengthweight

##---------------------------------------------------------------------------##
## Step 9: assign measuring board to processor ####

# Load inventory of processors with measuring boards 

mb_invent <- read.xlsx(paste0(mm_data_folder, 'IMAS_measuringboard_log_inventory.xlsx'), detectDates = T)

# Where measuring board is still with processor and the enddate is missing replace with todays date

mb_invent <- mb_invent %>% 
 mutate(startdate = as.POSIXct(startdate),
        enddate = as.POSIXct(enddate),
        enddate = if_else(is.na(enddate), Sys.time(), enddate))

# Join measuring board raw data with inventory data 

mb_next_gen_df <- fuzzy_left_join(
 mb_df,
 mb_invent,
 by = c(
  "logname" = "logname",
  "logger_date" = "startdate",
  "logger_date" = "enddate"
 ),
 match_fun = list(`==`, `>=`, `<=`)
) %>% select(-c(logname.y, startdate, enddate, platformscales, memorymodule, comments)) %>%
 mutate(logname = logname.x) %>%
 select(-(logname.x)) %>%
 filter(!is.na(docketnum))


# # add docket.index
# mb_next_gen_df <- mb_next_gen_df %>% 
#  mutate(docket.index = paste(zone, docketnum.day, plaindate, processor, sep = '-'))

##---------------------------------------------------------------------------##
 
## Step 12: match docketinfo ####

# The following script allocates NextGen measuring board data from processing 
# facilities to docket information extracted from FILMS using '1.MM_Processor_Docketinfo.R'.
# Measuring board data are then matched to historical data structure.

# NOTE: Recent measuring board data may not be allocated to docketinfo depending
# on the latest input of paper docket data by NRE into FILMs.
##----------------------------------------------------------------------------##
# Load latest docketinfo data

docketinfo <- readRDS(paste0(mm_data_folder, 'docketinfo3.rds'))

##----------------------------------------------------------------------------##
# Match processor measuring board data to historical, allocate to docket data and
# join to historical data. Data are compiled in the separate script 
# '2.MM_NextGen_ProcessorMB_Compile.R'.

# Import latest version of compiled next generation measuring board data

# measure.board.next.gen.df <- readRDS(paste0(mm_data_folder, 'measure.board.next.gen.df.RDS'))

# Match data to historical compiled dataframes, remove unecessary variables and 
# rename variables to match compiledMM.df

measure.board.next.gen.df.match <- measure.board.next.gen.df %>% 
 select(-c(rawutc, logger_date, local_date, latitude, longitude, abalonenum, zone, logname)) %>% 
 dplyr::rename(msr.date = plaindate,
               docket.number = docketnum,
               shell.length = shelllength,
               whole.weight = wholeweight,
               e.processor = processor) %>% 
 mutate(e.processor = replace(e.processor, e.processor == 'RALPHS TASMANIAN SEAFOODS PTY LTD', "RALPH'S TASMANIAN SEAFOOD PTY LTD"))

# Add variable to distinguish datasource
measure.board.next.gen.df.match$datasource <- '2020NextGen4G'                  

# Extract docketinfo for e.processors listed in new data
eproname <- as.data.frame(unique(measure.board.next.gen.df.match$e.processor))
colnames(eproname) <- c("e.processor")
e.processors <- eproname$e.processor

docketinfo.epro <- droplevels(subset(docketinfo, processorname %in% e.processors)) %>% 
 dplyr::rename(e.processor = processorname)

# Summarise data for number of samples, mean and min shell length and add to 
# dataframe to check for duplicates
n.per.docket <- measure.board.next.gen.df.match %>% 
 group_by(docket.number, msr.date, e.processor) %>%
 summarise(n = length(shell.length),
           meanSL = round(mean(shell.length, na.rm = T), 1),
           minSL = round(min(shell.length), 1))

# Match docketinfo.epro to the n.per.docket dataframe
docket.join <- inner_join(n.per.docket, docketinfo.epro, by = c("docket.number", "e.processor"))

# Add date difference column
docket.join <- docket.join %>%
 ungroup() %>%
 mutate(msr.date = as.Date(msr.date)) %>%
 mutate(msr.date.diff = as.numeric(msr.date - daylist_max))

# Check for and seperate out dupilicated dockets
n_occur <- data.frame(table(docket.join$docket.number))
range(n_occur$Freq)
docket.uniq <- as.data.frame(docket.join[docket.join$docket.number %in% n_occur$Var1[n_occur$Freq == 1],])

# Check
n_occur <- data.frame(table(docket.uniq$docket.number))
range(n_occur$Freq)

# Join unique dockets to df.1
measure.board.next.gen.df.unique <- inner_join(measure.board.next.gen.df.match, docket.uniq)

# Subset data and filter out uneeded or duplicated variables
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
 dplyr::rename('processorname' = e.processor)

# Save RDS file

saveRDS(compiled.docket.next.gen, paste0(mm_data_folder, 'compiled.docket.next.gen.RDS'))

# saveRDS(compiled.docket.next.gen, 'C:/CloudStor/R_Stuff/MMLF/compiled.docket.next.gen.RDS')
# compiled.docket.next.gen <- readRDS('C:/CloudStor/R_Stuff/MMLF/compiled.docket.next.gen.RDS')
##----------------------------------------------------------------------------##

