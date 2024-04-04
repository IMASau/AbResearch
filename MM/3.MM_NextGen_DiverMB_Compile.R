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
 library(rgdal)
  library(gridExtra)
 library(tidyverse)
 library(progress)
 library(furrr)
 library(purrr)
 library(keyring)
 library(odbc)
})
# ##---------------------------------------------------------------------------##
# ## Local working folder ####
# 
# # sftp.local <- "R:/TAFI/TAFI_MRL_Sections/Abalone/AbTrack/RawData/sftpServer/FilesNew"
# # measureboard.non.modem <- "C:/CloudStor/R_Stuff/MMLF/MM_Plots/MM_Woodham"
# 
# # measureboard.non.modem <- "R:/TAFI/TAFI_MRL_Sections/Abalone/AbTrack/RawData/NextGen/Data/RawTextFiles"
# 
# ##---------------------------------------------------------------------------##
# 
# 
# ## load latest file directory list (see NewGenBatteryCheck.R script) 
# load(paste('R:/TAFI/TAFI_MRL_Sections/Abalone/AbTrack/RawData/NextGen/Data/BV_Rev/dirlistdf_', Sys.Date(), '.RData', sep = ''))
# 
# # load(paste('R:/TAFI/TAFI_MRL_Sections/Abalone/AbTrack/RawData/NextGen/Data/BV_Rev/dirlistdf_', '2023-04-24', '.RData', sep = ''))
# 
# 
# ## filter directory list for loggers paired with measuring boards
# dirlistdf <- dirlistdf %>% 
#  filter(gps_SID %in% c('05010107', '05010036', '05010045', '05010040', '05010037', '05010112',
#                        '05010156', '05010086', '05010134', '05010016', '05010044', '05010070',
#                        '05010083','05010050','05010168'))
# 
# ## create backup file
# dirlistdf_bk <- dirlistdf
# 
# ## Import files
# setwd("R:/TAFI/TAFI_MRL_Sections/Abalone/AbTrack/RawData/NextGen/Data/RawTextFiles")
# 
# if (exists("data_packet"))
#  rm(data_packet)
# 
# numfiles <- nrow(dirlistdf)
# 
# ## Setup progress bar
# pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)", total = numfiles)
# pb$tick(0)
# 
# ## Establish function for retrieving  files
# loop_Batt_Check <- function(dirlistdf) {
#  infile <- as.character(dirlistdf$dirlist)
#  data.p <- read.csv(infile,
#                     header=FALSE,
#                     sep=',',
#                     as.is=TRUE,
#                     colClasses=c("character", "numeric", "numeric", "numeric", "character", "character"),
#                     skipNul = TRUE)
#  
#  colnames(data.p) <- c("logname", "seqindex","identifier","rawutc","datapack","crc_status")
#  data.p$logger_date <- as.POSIXct(data.p$rawutc, origin = "1970-01-01", tz = "GMT")
#  data.p$local_date <- as.POSIXct(data.p$rawutc, origin = "1970-01-01", tz = "Australia/BRISBANE")
#  data.p$file_name <- infile # Add file name to row
#  return(data.p)
# } # End Function loop_Batt_Check
# 
# ## Multi-core apply file retrieve
# options(future.rng.onMisuse="ignore")
# furrr_options(globals = TRUE, seed = TRUE)
# 
# ## set number of workers ## nb. don't overload cores
# future::plan(multisession(workers = 10)) # Jaime = 14 cores max.
# 
# tic("multi core battery check")
# 
# data_packet <- dirlistdf %>%
#  split(.$dirlist) %>%
#  future_map_dfr( ~ loop_Batt_Check(.), .progress = TRUE)
# 
# toc()
# 
# future:::ClusterRegistry("stop")
# 
# 
# #### ------------------------------------------------------------------------##
# ## NewGEN QA/QC processing ####
# 
# # # Backup
# # data_packet_bk <- data_packet
# # data_packet <- data_packet_bk
# 
# ## Data Duplication check 
# # Remove duplicate recs
# # NB local_date needed as seq index cyclical
# data_packet <- rowid_to_column(data_packet, "RowNum")
# dupes <- data_packet[duplicated(data_packet[,c("logname","seqindex","identifier","local_date")]),]
# data_packet <- filter(data_packet, !RowNum %in% c(dupes$RowNum))
# 
# unique(dupes$logname)
# 
# ## Measuring Board
# save(data_packet, file = paste('R:/TAFI/TAFI_MRL_Sections/Abalone/AbTrack/RawData/NextGen/Data/WorkingData/MeasuringBoard/data_packet_full_', Sys.Date(), '.RData', sep = ''))
# 
# 
# ##---------------------------------------------------------------------------##
# # load('R:/TAFI/TAFI_MRL_Sections/Abalone/AbTrack/RawData/NextGen/Data/WorkingData/MeasuringBoard/data_packet_full_2022-10-20.RData')
# 
# # logged.data <- data_packet %>% 
# #  select("logname", "seqindex","identifier","rawutc","datapack","crc_status")
# 
# ##---------------------------------------------------------------------------##
# # # Extract logger data files for GPS loggers allocated to divers 
# # # 05010107 - Greg Woodham
# # # 05010036 - Ben Allen
# # # 05010040 or 05010037 or 05010112 - Sean Larby
# # # 05010156 - Bryan Denny
# # 
# # 
# # localfiles <- list.files(measureboard.non.modem,  
# #                          pattern = "^05010107|05010036|05010045|05010040|05010037|05010112|
# #                          05010156|05010086|05010134|05010016|05100044|05010070.*txt", full.names = T)
# # 
# # ##---------------------------------------------------------------------------##
# # ## Extract .txt files ####
# # 
# # ## Extract data from sftp download folder and compile into dataframe
# # ## Note: measuring board .txt files are normally denoted with '07' prefix however these are field-based
# # ## measuring boards and are linked to a diver GPS unit (i.e. prefix '05'). They only record length data as there is currently
# # ## no weight integration.
# # 
# # # localfiles <- list.files(measureboard.non.modem,  pattern = "^05.*txt", full.names = T) 
# # 
# # # localfiles.dat <- lapply(localfiles, read.table, sep = ",", header = F, row.names = NULL, as.is = T,
# # #                           colClasses = c("character", "numeric", "numeric", "numeric", "character", "character"))
# # 
# # localfiles.dat <- lapply(localfiles, function(x) {
# #   tryCatch(read.table(x, header = FALSE, sep = ',', as.is = T,
# #                       colClasses = c("character", "numeric", "numeric", "numeric", "character", "character")), 
# #            error=function(e) NULL)
# # })
# # 
# # 
# # localfiles.df <- do.call(rbind, localfiles.dat)
# # 
# # logged.data <- localfiles.df
##---------------------------------------------------------------------------##
## Run the above code to extract rawdata files only for data pre-2023 ##

## Use the NG_DB_RawData_extract.R code to compile data from the AbTrack database ##

## Set source location of functions to be used in script
source("c:/GitCode/AbSpatial/AbTrack/MapNextgen_Queries_v1.R")

## Establish connection to database
conn <- DBI::dbConnect(odbc(), "AbTrack")

# Set year
year = 2023

# Import data
tic("sql retrieve")
dstream <-
 DBI::dbGetQuery(conn,
                 NG_DATA_STREAM_VIEW_select,
                 params = year)
toc()
dbDisconnect(conn)

## Force time zone changes
dstream$Logger_Date <- force_tz(dstream$Logger_Date, tzone="GMT")
dstream$Local_Date <- force_tz(dstream$Local_Date, tzone="Australia/BRISBANE")

## Filter for columns and rename to match data_packet
data_packet <- dstream %>% select(10:17) %>% 
 rename(logname = Logname, seqindex = SeqIndex, identifier = Identifier, logger_date = Logger_Date, local_date = Local_Date)

# Remove dstream
# rm(dstream)

## Remove duplicate recs 
# NB. local_date needed as seq index cyclical
data_packet <- rowid_to_column(data_packet, "RowNum")
dupes <- data_packet[duplicated(data_packet[,c("logname","seqindex","identifier","local_date")]),]
data_packet <- filter(data_packet, !RowNum %in% c(dupes$RowNum))
unique(dupes$logname)

## Filter for measuring board data
logged_data <- data_packet %>% 
 filter(logname %in% c('07010050', 
                       '07010051', 
                       '07010052', 
                       '07010053'))

## Save logged_data
save(logged_data, file = paste('C:/cloudstor/R_Stuff/MMLF/measuringboard_logged_data_', Sys.Date(), '.RData', sep = ''))

## Load logged_data
load('C:/cloudstor/R_Stuff/MMLF/measuringboard_logged_data_2023-08-31.RData')

logged.data <- logged_data

##---------------------------------------------------------------------------##
## Extract info from data packet ####

# rename variable names of dataframe

# colnames(logged.data) <- c("logname", "seqindex","identifier","rawutc","datapack","crc_status")

# identify measuring board logger names

# unique(logged.data$logname)

# identify number of records for each identifier
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

# remove duplicate records resulting from upload failures or loggers going out of range
logname <- logname %>% 
  distinct(logname, seqindex, identifier, .keep_all = T)

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

gps.RMC_A <- gps.RMC_A %>% 
  distinct(logname, rawutc, longitude, latitude, .keep_all = T)

# tail(gps.RMC_A)

##---------------------------------------------------------------------------##
## Step 3B: Extract GPS RMC Part B ####
gps.RMC_B <- filter(logged.data, identifier == 221) %>%
 separate(datapack, c("valid","speed", "course","variation"), sep = ",", remove = FALSE,
          convert = FALSE) %>%
 as.data.frame()

gps.RMC_B <- gps.RMC_B %>% 
  distinct(logname, rawutc, .keep_all = T)

# tail(gps.RMC_B)

##---------------------------------------------------------------------------##
## Step 3C: Join RMC Part A & B   ####

gps.RMC <- left_join(gps.RMC_B, select(gps.RMC_A, logname, local_date, longitude, latitude), by = c('logname', "local_date")) 


# remove duplicate records resulting from upload failures or loggers going out of range 
# and filter out measuring board records
gps.RMC <- gps.RMC %>%
  distinct(logname, seqindex, identifier, .keep_all = T) %>% 
  filter(grepl('^07', logname))

# tail(gps.RMC)

# table(gps.RMC$plaindate)

##---------------------------------------------------------------------------##
## Step 4: Extract Docket details ####
docket <- filter(logged.data, identifier == 32963) 
docket <-  separate(docket, datapack, c("abalonenum","zone", "docketnum"), sep = ",", remove = FALSE,
                    convert = TRUE) %>%
 as.data.frame()

# remove duplicate records resulting from upload failures or loggers going out of range
docket <- docket %>% 
  distinct(logname, seqindex, identifier, .keep_all = T)

# tail(docket)

##---------------------------------------------------------------------------##
## Step 5: Extract length ####
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
abweight <-  separate(abweight, datapack, c("abalonenum","est.weight"), sep = ",", remove = FALSE,
                      convert = TRUE) %>%
 as.data.frame()

# remove duplicate records resulting from upload failures or loggers going out of range
abweight <- abweight %>% 
  distinct(logname, seqindex, identifier, abalonenum, .keep_all = T)

# tail(abweight)

##---------------------------------------------------------------------------##
## Step 7: Join components into a flat form ####

lengthweight <- left_join(select(gps.RMC, logname, rawutc, logger_date, local_date, plaindate, latitude, longitude),
                          select(logname, logname, rawutc, abalonenum), by = c("rawutc", 'logname')) %>%  
 left_join(select(docket, logname, rawutc, zone, docketnum), by = c("rawutc", 'logname')) %>% 
 left_join(select(ablength, logname, rawutc,shelllength), by = c("rawutc", 'logname')) %>% 
 left_join(select(abweight, rawutc, est.weight), by = "rawutc")

# tail(lengthweight)

measure.board.df <- lengthweight

measure.board.df.non.modem <- measure.board.df

#remove duplicate data where duplicate rawutc times have occured 
measure.board.df.non.modem <- measure.board.df.non.modem %>% 
  distinct(logname, rawutc, .keep_all = T) 

##---------------------------------------------------------------------------##
# manually add species and zone for known greenlip catches

# Greg Woodham mixed species fishing on 2020-07-07 in Block 39A - species measurements mixed
# and have been separated by examining where clusters of abalone have been measured that are 
# below the greenlip LML of 145 mm and labeling these blacklip

woodham.gl.bl <- measure.board.df.non.modem %>%
  filter(logname == '07010050' &
           plaindate %in% as.Date(c("2020-07-07", "2020-07-08")))

woodham.bl <- measure.board.df.non.modem %>%
  filter(logname == '07010050' &
           plaindate == as.Date("2020-07-07")) %>%
  filter(
    between(abalonenum, 59, 69) |
      between(abalonenum, 114, 117) |
      between(abalonenum, 139, 148) |
      between(abalonenum, 169, 179) |
      between(abalonenum, 197, 202) |
      between(abalonenum, 217, 219) |
      between(abalonenum, 239, 245) |
      between(abalonenum, 269, 272) |
      between(abalonenum, 301, 306) |
      between(abalonenum, 322, 332) |
      between(abalonenum, 345, 349) |
      between(abalonenum, 366, 370) |
      between(abalonenum, 460, 462) |
      between(abalonenum, 468, 477) |
      between(abalonenum, 486, 493) |
      between(abalonenum, 508, 512) |
      between(abalonenum, 518, 524)
  ) %>% 
  mutate(species = 1)

woodham.gl.a <- left_join(woodham.gl.bl, woodham.bl) %>% 
  mutate(species = ifelse(is.na(species), 2, species))

# Woodham greenlip measurements from NE fishery in 2022
woodham.gl.b <- measure.board.df.non.modem %>%
 filter(logname == '07010050' &
         plaindate %in% as.Date(c("2022-07-04", 
                                  "2022-07-05",
                                  "2022-07-07"))) %>% 
 mutate(species = 2)

woodham.gl <- bind_rows(woodham.gl.a, woodham.gl.b)

# Ben Allen greenlip data mostly from block 02C (King Island) and 48C

allen.gl <- measure.board.df.non.modem %>% 
  filter(logname == '07010051' &
           plaindate %in% as.Date(c("2020-07-28",
                                  "2020-07-29",
                                  "2020-08-16",
                                  "2020-08-17",
                                  "2020-08-18",
                                  "2020-08-24",
                                  "2020-08-26",
                                  "2020-10-13"))) %>% 
  mutate(species = 2)

# Sean Larby greenlip data from 2020-09-09 and 2021-07-05

larby.gl <- measure.board.df.non.modem %>% 
  filter(logname == '07010053' &
           plaindate %in% as.Date(c("2020-09-09", "2020-09-29", "2020-10-19", "2020-11-17"))|
           logname == '07010053' & 
           plaindate == as.Date("2021-07-05") & 
           between(rawutc, 1625439114, 1625448758)) %>% 
           mutate(species = 2)

# combine Woodham, Allen and Larby greenlip data
woodham.allen.larby.gl <- bind_rows(woodham.gl, allen.gl, larby.gl)

# re-join Woodham and Allen data to main dataframe

measure.board.df.non.modem <- left_join(measure.board.df.non.modem, woodham.allen.larby.gl) %>% 
  mutate(species = ifelse(is.na(species), 1, species),
         zone = ifelse(species == 2, 'G', zone))

##---------------------------------------------------------------------------##
## Step 8: Identify multiple samples in a day and remove any trial/practice data ####

## identify different samples based on breaks in abalonenum sequence
mb.df.non.modem.seq <- measure.board.df.non.modem %>%
 arrange(local_date) %>% 
 mutate(sample.id = cumsum(c(-1, diff(abalonenum)) < 0)) %>% 
 group_by(sample.id, species)

## determine number of abalone in each sample
mb.df.non.modem.seq.no <- mb.df.non.modem.seq %>% 
 summarise(sample.n = n())

mb.df.non.modem.seq.no.join <- left_join(mb.df.non.modem.seq, mb.df.non.modem.seq.no)

## remove trial/practice samples
## Ben Allen indicated he made some trial measurements on 2020-07-14
measure.board.df.non.modem <- mb.df.non.modem.seq.no.join %>% 
 filter(sample.n >= 5) %>% 
 select(-c(sample.id, sample.n))

measure.board.df.non.modem <- measure.board.df.non.modem %>% 
  filter(logname %in% c('07010050', '07010053') &
           plaindate %in% as.Date(c('2020-06-30',
                                    '2020-08-11', 
                                    '2020-08-12',
                                    '2020-08-13',
                                    '2020-08-22'))|
           logname %in% c('07010053') &
           plaindate %in% as.Date(c('2020-09-29')) &
           abalonenum %in% c(165)|
           logname %in% c('07010053') &
           plaindate %in% as.Date(c('2020-10-28')) &
           abalonenum %in% c(116, 117, 118, 119, 120, 121, 122, 123, 124)) %>%
  mutate(mb.test.data = 1) %>% 
  left_join(measure.board.df.non.modem, .) %>%   
  filter(is.na(mb.test.data)) %>% 
  ungroup() %>% 
  select(-c(mb.test.data, sample.id))

## Remove practice samples for Ben Allens board before sending to him on 2020-07-02
measure.board.df.non.modem <- measure.board.df.non.modem %>% 
  filter(!(logname == '07010051' & 
             plaindate == as.Date('2020-07-02')))

## Sean Larby indicated that his deckhand incorrectly measured fish on 2020-12-19 by sliding gate open
## and closing back onto shell

measure.board.df.non.modem <- measure.board.df.non.modem %>% 
  filter(!(logname == '07010053' &
           plaindate == as.Date('2020-12-19')))

## Remove practice samples for Bryan Denny
measure.board.df.non.modem <- measure.board.df.non.modem %>% 
  filter(!(logname == '07010052' &
           plaindate %in% as.Date(c('2021-07-29', 
                                    '2021-07-11'))))


##---------------------------------------------------------------------------##
## Step 9: Determine sample location ####

# determine likely latitude and longitude where GPS has dropped out

measure.board.df.non.modem <- measure.board.df.non.modem %>% 
  mutate(latitude = na_if(as.numeric(latitude), 0),
         longitude = na_if(as.numeric(longitude), 0)) %>%  
  fill(latitude, .direction = 'downup') %>% 
  fill(longitude, .direction = 'downup')

# fix GPS erroneous GPS coordinates

measure.board.df.non.modem <- measure.board.df.non.modem %>%
  mutate(
    long.adj = if_else(
      abalonenum == 0 &
        logname == lead(logname) &
        plaindate == lead(plaindate) &
        between(longitude - lead(longitude), -2, 2),
      longitude,
      if_else(
        abalonenum == 0 &
          logname == lead(logname) &
          plaindate == lead(plaindate) &
          (longitude - lead(longitude) < -2),
        lead(longitude),
        if_else(
          abalonenum == 0 &
            logname == lead(logname) &
            plaindate == lead(plaindate) &
            (longitude - lead(longitude) > 2),
          lead(longitude),
          if_else(
            logname == lag(logname) &
              logname == lead(logname) &
              plaindate == lag(plaindate) &
              abalonenum == (lag(abalonenum) + 1) &
              (longitude - ((lag(
                longitude
              )) + (lead(
                longitude
              ))) / 2 < -2) |
              (longitude - ((lag(
                longitude
              )) + (lead(
                longitude
              ))) / 2 > 2),
            ((lag(longitude)) + (lead(longitude))) / 2,
            longitude
          )
        )
      )
    ),
    lat.adj = if_else(
      abalonenum == 0 &
        logname == lead(logname) &
        plaindate == lead(plaindate) &
        between(latitude - lead(latitude), -2, 2),
      latitude,
      if_else(
        abalonenum == 0 &
          logname == lead(logname) &
          plaindate == lead(plaindate) &
          (latitude - lead(latitude) < -2),
        lead(latitude),
        if_else(
          abalonenum == 0 &
            logname == lead(logname) &
            plaindate == lead(plaindate) &
            (latitude - lead(latitude) > 2),
          lead(latitude),
          if_else(
            logname == lag(logname) &
              logname == lead(logname) &
              plaindate == lag(plaindate) &
              abalonenum == (lag(abalonenum) + 1) &
              (latitude - ((lag(
                latitude
              )) + (lead(
                latitude
              ))) / 2 < -2) |
              (latitude - ((lag(
                latitude
              )) + (lead(
                latitude
              ))) / 2 > 2),
            ((lag(latitude)) + (lead(latitude))) / 2,
            latitude
          )
        )
      )
    )
  )



measure.board.df.non.modem <- measure.board.df.non.modem %>% 
  mutate(long.adj = if_else(is.na(lead(longitude)), lag(long.adj), long.adj),
         lat.adj = if_else(is.na(lead(latitude)), lag(lat.adj), lat.adj))

measure.board.df.non.modem <- measure.board.df.non.modem %>% 
  mutate(longitude = long.adj,
         latitude = lat.adj) %>% 
  select(-c(long.adj, lat.adj))

# read in Subblock map as an sf::sfc polygon object
# sf.subblock.map <- st_read("C:/Users/jaimem/Dropbox/AbaloneData/SpatialLayers/SubBlockMaps.gpkg")
sf.subblock.map <- st_read(paste(sprintf("C:/Users/%s/University of Tasmania/IMAS-DiveFisheries - Assessments - Documents/Assessments/GIS/SpatialLayers/IMAS_Layers/IMAS_subblock_rev2022.gpkg", Sys.info()[["user"]])))

# set CRS
GDA2020 <- st_crs(7855)
GDA94 <- st_crs(28355)
WGS84 <- st_crs(4326)

# transform map to GDA2020
sf.subblock.map <- st_transform(sf.subblock.map, GDA2020)

# convert neasuring board data to sf object 
mb.samp.loc <- measure.board.df.non.modem %>% 
  # filter(latitude < 0) %>% 
  as.data.frame() %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = WGS84)

# transform to GDA2020
mb.samp.loc <- st_transform(mb.samp.loc, GDA2020)

# create a join based on the nearest SAU polygon to each measuring board measurement
mb.samp.loc.geo <- st_join(mb.samp.loc, sf.subblock.map, join = st_nearest_feature) %>%  
  # st_set_geometry(NULL) %>%
  # select(-c(version, area, zone.x)) %>% 
  # dplyr::rename(zone = zone.y) %>% 
 select(-c(zone)) %>% 
  rename_all(tolower)

# identify greenlip zones
mb.samp.loc.geo <- mb.samp.loc.geo %>% 
  mutate(zone = ifelse(species == 2, 'G', zone))

##---------------------------------------------------------------------------##
## Step 10: Add diver details ####

## load inventory of divers which hold measuring boards
mb.invent <- read.xlsx("C:/CloudStor/R_Stuff/MMLF/IMAS_measuringboard_log_inventory.xlsx",
                       detectDates = T)

## where measuring board is still with diver and the endate is missing replace with todays date
mb.invent <- mb.invent %>% 
 mutate(startdate = as.POSIXct(startdate),
        enddate = as.POSIXct(enddate),
        enddate = if_else(is.na(enddate), Sys.time(), enddate))

## join diver details to dataframe
mb.df.non.modem.diver <- fuzzy_left_join(
  mb.samp.loc.geo,
 mb.invent,
 by = c(
  "logname" = "logname",
  "plaindate" = "startdate",
  "plaindate" = "enddate"
 ),
 match_fun = list(`==`, `>=`, `<=`)
) %>% 
 select(-c(logname.y, startdate, enddate, platformscales, comments)) %>% 
 dplyr::rename('logname' = logname.x)

measure.board.df.non.modem <- mb.df.non.modem.diver %>% 
 mutate(blockno = as.character(blockno))


##---------------------------------------------------------------------------##
## Step 10: Subblockno and zone adjustments ####

# manually adjust block and subblockno where boat has drifted during measuring 
# or where additional abalone have been measured at the end of the day. 


measure.board.df.non.modem <- measure.board.df.non.modem %>%
  mutate(
    subblockno = if_else(
      processor == 'Greg Woodham' &
        plaindate == as.Date('2020-05-19') &
        subblockno %in% c('14A', '13C'),
      '13E',
      if_else(
        processor == 'Ben Allen' &
          plaindate == as.Date('2020-07-29') &
          subblockno %in% c('02B'),
        '02C',
        if_else(
          processor == 'Greg Woodham' &
            plaindate == as.Date('2020-08-26') &
            subblockno %in% c('39B'),
          '31B',
          if_else(
            processor == 'Sean Larby' &
              plaindate == as.Date('2020-09-16') &
              subblockno %in% c('30A'),
            '29D',
            if_else(
              processor == 'Sean Larby' &
                plaindate == as.Date('2020-10-19') &
                subblockno %in% c('40B'),
              '40A',
              if_else(
                processor == 'Sean Larby' &
                  plaindate == as.Date('2020-09-29') &
                  subblockno %in% c('31B'),
                '32B',
                if_else(
                 processor == 'Bryan Denny' &
                  plaindate == as.Date('2022-09-14') &
                  subblockno %in% c('30A'),
                 '31B',
                subblockno
          )
        )
      )
    )
    )
    )),
    blockno = if_else(
      processor == 'Greg Woodham' &
        plaindate == as.Date('2020-05-19') &
        subblockno %in% c('13E'),
      '13',
      if_else(
        processor == 'Greg Woodham' &
          plaindate == as.Date('2020-08-26') &
          subblockno %in% c('31B'),
        '31',
        if_else(
          processor == 'Sean Larby' &
            plaindate == as.Date('2020-09-16') &
            subblockno %in% c('29D'),
          '29',
          if_else(
            processor == 'Sean Larby' &
              plaindate == as.Date('2020-09-29') &
              subblockno %in% c('32B'),
            '32',
            if_else(
             processor == 'Bryan Denny' &
              plaindate == as.Date('2022-09-14') &
              subblockno %in% c('31B'),
             '31',
          blockno
        )
      )
    )
  )
  ),
  zone = if_else(
   processor == 'Bryan Denny' &
    plaindate == as.Date('2022-09-14') &
    subblockno %in% c('31B'), 'N',
   zone)
    )

# add sample ID
mb.df.non.modem.sample.id <- measure.board.df.non.modem %>% 
  group_by(processor, plaindate, subblockno, species) %>% 
  summarise(catches.measured = n_distinct(plaindate),
            n = n_distinct(abalonenum)) %>% 
  as.data.frame %>% 
  arrange(plaindate) %>% 
  mutate(sample.id = row_number())

measure.board.df.non.modem <- left_join(measure.board.df.non.modem, mb.df.non.modem.sample.id)

##---------------------------------------------------------------------------##

# remove known practice samples on days where actual sampling occured but could
# not be identified earlier without location data

measure.board.df.non.modem <- measure.board.df.non.modem %>% 
 filter(!(logname == '07010050' &
           plaindate == as.Date('2022-08-08') &
           subblockno %in% c('12D', '13A'))) %>% 
 filter(!(logname == '07010050' &
            plaindate == as.Date('2022-01-15') &
            subblockno %in% c('14A', '13C')))

##---------------------------------------------------------------------------##
## Step 11: Combine data from SQL database with pre-2023 rawdata files ####

# load most last compilation of pre-2023 rawdata
measure.board.df.non.modem_old <- readRDS('C:/CloudStor/R_Stuff/MMLF/MM_Plots/measure.board.df.non.modem.RDS')

# remove sample.id from dataframes
measure.board.df.non.modem_old <- measure.board.df.non.modem_old %>% 
 select(-sample.id)

measure.board.df.non.modem <- measure.board.df.non.modem %>% 
 select(-sample.id)

# combine dataframes
mb_df_nm <- bind_rows(measure.board.df.non.modem_old, measure.board.df.non.modem)

# re-add sample_id
mb_df_nm_id <- mb_df_nm %>% 
 group_by(processor, plaindate, subblockno, species) %>% 
 summarise(catches.measured = n_distinct(plaindate),
           n = n_distinct(abalonenum)) %>% 
 as.data.frame %>% 
 arrange(plaindate) %>% 
 mutate(sample.id = row_number())

mb_df_nm_final <- left_join(mb_df_nm, mb_df_nm_id)


##---------------------------------------------------------------------------##
## Step 12: Save RDS of dataframe ####

# saveRDS(measure.board.df.non.modem, 'C:/CloudStor/R_Stuff/MMLF/MM_Plots/measure.board.df.non.modem.RDS')
saveRDS(mb_df_nm_final, 'C:/cloudstor/DiveFisheries/Abalone/MMdata/mb_df_nm_final.RDS')

# measure.board.df.non.modem <- readRDS('C:/CloudStor/R_Stuff/MMLF/MM_Plots/measure.board.df.non.modem.RDS')

##---------------------------------------------------------------------------##
## Step 13: match data structure ####

# Match diver measuring board data to historical data structure. 
# Data are compiled in the separate script '3.MM_NextGen_DiverMB_Compile.R'.
# Measuring board data are then matched to historical data structure.

# Import latest version of compiled next generation woodham measuring board data

# measure.board.df.non.modem <- readRDS('C:/CloudStor/R_Stuff/MMLF/MM_Plots/measure.board.df.non.modem.RDS')

mb_df_nm_final <- readRDS('C:/cloudstor/DiveFisheries/Abalone/MMdata/mb_df_nm_final.RDS')

measure.board.df.non.modem <- mb_df_nm_final

# Match data to existing compiled dataframes
measure.board.df.non.modem.match <- measure.board.df.non.modem %>% 
 select(-c(rawutc, logger_date, local_date, abalonenum, logname, geometry, memorymodule, catches.measured)) %>% 
 dplyr::rename(msr.date = plaindate,
               shell.length = shelllength,
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
 select(-c(blockno, est.weight))

# Summarise data for number of samples, mean and min shell length and add to dataframe to check for duplicates
n.per.docket <- measure.board.df.non.modem.match %>% 
 group_by(sample.id, species, msr.date) %>%
 summarise(n = length(shell.length),
           meanSL = round(mean(shell.length, na.rm = T), 1),
           minSL = round(min(shell.length), 1))

# Match docketinfo.epro to the n.per.docket dataframe
docket.join <- inner_join(n.per.docket, measure.board.df.non.modem.match, 
                          by = c("sample.id", 'species', 'msr.date')) %>% 
 ungroup()
# select(-(sample.id))

# Subset data and filter out uneeded or duplicated variables
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
  datasource,
  sample.id
 )

# Save RDS file

saveRDS(compiled.docket.non.modem, 'C:/CloudStor/R_Stuff/MMLF/compiled.docket.non.modem.RDS')

# Re-write this line to save data to Dropbox

# mm_data_folder <- paste(sprintf('C:/Users/%s/Dropbox (UTAS Research)/DiveFisheries/Abalone/MMdata/',
#                                 Sys.info()[["user"]]))

##----------------------------------------------------------------------------##
## Step 12: Plot data ####

## load most recent RDS data frame of non modem measuring board data
# measure.board.df.non.modem <- readRDS('C:/CloudStor/R_Stuff/MMLF/MM_Plots/measure.board.df.non.modem.RDS')
measure.board.df.non.modem <- readRDS('C:/cloudstor/DiveFisheries/Abalone/MMdata/mb_df_nm_final.RDS')

measure.board.df.non.modem %>% 
  group_by(processor, plaindate) %>% 
  filter(plaindate >= as.Date('2021-12-31')) %>% 
  summarise(catches.measured = n_distinct(sample.id),
            n = n()) %>% 
  janitor::adorn_totals() %>% 
  as.data.frame()

##---------------------------------------------------------------------------##
## Step 13: Add size-limit data ####

# load legal minimum length data
size.limits <- read.csv("C:/CloudStor/R_Stuff/MMLF/AbaloneSizeLimits2.csv", fileEncoding="UTF-8-BOM")

# clean lml data
colnames(size.limits) <- tolower(colnames(size.limits))
names(size.limits) <- gsub('.', '-', names(size.limits), fixed = T)

# convert lml data to long format and create lml index variable
size.limits.tab <- size.limits %>%
  gather(monthyear, sizelimit, `jan-1962`:`dec-2023`) %>% 
  mutate(monthyear = gsub('jan', 1, monthyear)) %>% 
  mutate(monthyear = gsub('feb', 2, monthyear)) %>% 
  mutate(monthyear = gsub('mar', 3, monthyear)) %>% 
  mutate(monthyear = gsub('apr', 4, monthyear)) %>% 
  mutate(monthyear = gsub('may', 5, monthyear)) %>% 
  mutate(monthyear = gsub('jun', 6, monthyear)) %>% 
  mutate(monthyear = gsub('jul', 7, monthyear)) %>% 
  mutate(monthyear = gsub('aug', 8, monthyear)) %>% 
  mutate(monthyear = gsub('sep', 9, monthyear)) %>% 
  mutate(monthyear = gsub('oct', 10, monthyear)) %>% 
  mutate(monthyear = gsub('nov', 11, monthyear)) %>% 
  mutate(monthyear = gsub('dec', 12, monthyear)) %>% 
  mutate(sizelimit.index = paste(abzone, subblockno, monthyear, sep = '-')) %>% 
  select(sizelimit.index, sizelimit)

# create size limit index
measure.board.df.non.modem <- measure.board.df.non.modem %>%
  mutate(sizelimit.index = paste(
    zone,
    subblockno,
    lubridate::month(plaindate),
    lubridate::year(plaindate),
    sep = '-'
  ))

# join size limit data and compiledMM.df to include size limit for each observation
measure.board.df.non.modem <- left_join(measure.board.df.non.modem, size.limits.tab, "sizelimit.index")

##---------------------------------------------------------------------------##
## Step 14: Summary table 1 ####
## create summary table for each sample date and diver

# list of unique measureboards for summary and plot loops
lognames <- unique(measure.board.df.non.modem$logname)
processors <- unique(measure.board.df.non.modem$processor)

for (i in processors){

mb.df.non.modem.summary <- measure.board.df.non.modem %>%
  filter(!is.na(shelllength) &
           grepl('^07', logname) &
           between(shelllength, 100, 220) &
          processor == i) %>%  
  # distinct(plaindate, abalonenum, .keep_all = T) %>%  
  group_by(plaindate, logname, zone, subblockno, processor, species) %>% 
  summarise('Number\nmeasured' = n(),
            'Mean\nsize\n(mm)' = round(mean(shelllength), 0),
            'Min\nsize\n(mm)' = round(min(shelllength), 0),
            'Max\nsize\n(mm)' = round(max(shelllength), 0)) %>% 
  arrange(desc(plaindate)) %>%
  ungroup() %>% 
  mutate_if(is.factor,
            fct_explicit_na,
            na_level = '') %>% 
 dplyr::rename('Dive date' = plaindate,
        'SubBlock' = subblockno) %>% 
 as.data.frame() %>%
 select(-c(zone)) %>% 
  mutate(species = ifelse(species == 1, 'Blacklip', 'Greenlip')) %>% 
 dplyr::rename('Diver' = processor,
        'Species' = species) %>% 
 select(c("Diver", "Dive date",  "SubBlock", "Species", "Number\nmeasured", "Mean\nsize\n(mm)", "Min\nsize\n(mm)", "Max\nsize\n(mm)"))

diver <- unique(mb.df.non.modem.summary$Diver)

## create formatted summary table
mb.df.non.modem.summary.formated <- mb.df.non.modem.summary %>% 
  ggpubr::ggtexttable(rows = NULL, theme = ggpubr::ttheme('mOrange'))

## save table
setwd("C:/CloudStor/R_Stuff/MMLF/MM_Plots/MM_Woodham")

ggsave(
  filename = paste(diver, '_Measureboard_NonModem_SizeSummary_Formatted_', Sys.Date(), '.pdf', sep = ''),
  plot = mb.df.non.modem.summary.formated,
  width = 11.69,
  height = 10,
  units = 'in'
)

}

##---------------------------------------------------------------------------##
## Step 15: Summary table 2 ####
## summary table with % above reference points

# list of unique measureboards for summary and plot loops
lognames <- unique(measure.board.df.non.modem$logname)
processors <- unique(measure.board.df.non.modem$processor)


for (i in processors){

mb.df.non.modem.summary.ref <- measure.board.df.non.modem %>%
  filter(!is.na(shelllength) &
           grepl('^07', logname) &
           between(shelllength, 100, 220) &
          processor == i) %>%   
 distinct(abalonenum, rawutc, .keep_all = T) %>%
 group_by(plaindate, logname, zone, subblockno, processor, species) %>%  
  summarise('Number\nmeasured' = n(),
            'Mean\nsize\n(mm)' = round(mean(shelllength), 0),
            'Min\nsize\n(mm)' = round(min(shelllength), 0),
            'Max\nsize\n(mm)' = round(max(shelllength), 0),
            n = n(),
            ref.a = sum(shelllength >= 150),
            ref.b = sum(shelllength >= 155),
            ref.c = sum(shelllength >= 160),
            '>150mm\n(%)' = round((ref.a/n)*100),
            '>155mm\n(%)' = round((ref.b/n)*100),
            '>160mm\n(%)' = round((ref.c/n)*100)) %>% 
  arrange(desc(plaindate)) %>%
  ungroup() %>% 
  mutate_if(is.factor,
            fct_explicit_na,
            na_level = '') %>%
  mutate(species = ifelse(species == 1, 'Blacklip', 'Greenlip')) %>%
 dplyr::rename('Dive date' = plaindate,
        'SubBlock' = subblockno,
        'Species' = species) %>% 
 as.data.frame() %>%
 select(-c(n, ref.a, ref.b, ref.c, zone)) %>% 
 dplyr::rename('Diver' = processor) %>% 
 select(c("Dive date", Diver, SubBlock, Species, 'Number\nmeasured', "Mean\nsize\n(mm)", "Min\nsize\n(mm)", "Max\nsize\n(mm)",
          ">150mm\n(%)", ">155mm\n(%)", ">160mm\n(%)"))

diver <- unique(mb.df.non.modem.summary.ref$Diver)

mb.df.non.modem.summary.reference.limits <- mb.df.non.modem.summary.ref %>% 
  ggpubr::ggtexttable(rows = NULL, theme = ggpubr::ttheme('mOrange'))

setwd("C:/CloudStor/R_Stuff/MMLF/MM_Plots/MM_Woodham")

write.xlsx(mb.df.non.modem.summary.ref,
           file = paste(diver, '_Measureboard_NonModem_SizeSummaryReferenceLimits_', Sys.Date(), '.xlsx'),
           sheetName = "Sheet1",
           col.names = TRUE,
           row.names = TRUE,
           append = FALSE)

ggsave(
  filename = paste(diver, '_Measureboard_NonModem_SizeSummaryReferenceLimits_Formatted_', Sys.Date(), '.pdf', sep = ''),
  plot = mb.df.non.modem.summary.reference.limits,
  width = 11.69,
  height = 16,
  units = 'in'
)
}

##---------------------------------------------------------------------------##
# summary for greg woodham
woodie.west <- measure.board.df.non.modem %>%
 filter(!is.na(shelllength) &
         grepl('^07', logname) &
         between(shelllength, 100, 220) &
         logname == '07010050',
        zone == 'W') %>%   
 distinct(abalonenum, rawutc, .keep_all = T) %>%
 group_by(logname, zone, subblockno, processor, species) %>%  
 summarise('Number\nmeasured' = n(),
           'Mean\nsize\n(mm)' = round(mean(shelllength), 0),
           'Min\nsize\n(mm)' = round(min(shelllength), 0),
           'Max\nsize\n(mm)' = round(max(shelllength), 0),
           n = n(),
           ref.a = sum(shelllength >= 150),
           ref.b = sum(shelllength >= 155),
           ref.c = sum(shelllength >= 160),
           '>150mm\n(%)' = round((ref.a/n)*100),
           '>155mm\n(%)' = round((ref.b/n)*100),
           '>160mm\n(%)' = round((ref.c/n)*100)) %>% 
 ungroup() %>% 
 mutate_if(is.factor,
           fct_explicit_na,
           na_level = '') %>%
 mutate(species = ifelse(species == 1, 'Blacklip', 'Greenlip')) %>% 
 dplyr::rename('SubBlock' = subblockno,
               'Species' = species) %>% 
 as.data.frame() %>%
 select(-c(n, ref.a, ref.b, ref.c, zone)) %>% 
 dplyr::rename('Diver' = processor) %>% 
 select(c(Diver, SubBlock, Species, 'Number\nmeasured', "Mean\nsize\n(mm)", "Min\nsize\n(mm)", "Max\nsize\n(mm)",
          ">150mm\n(%)", ">155mm\n(%)", ">160mm\n(%)"))

woodie.east <- measure.board.df.non.modem %>%
 filter(!is.na(shelllength) &
         grepl('^07', logname) &
         between(shelllength, 100, 220) &
         logname == '07010050',
        zone == 'E') %>%   
 distinct(abalonenum, rawutc, .keep_all = T) %>%
 group_by(logname, zone, subblockno, processor, species) %>%  
 summarise('Number\nmeasured' = n(),
           'Mean\nsize\n(mm)' = round(mean(shelllength), 0),
           'Min\nsize\n(mm)' = round(min(shelllength), 0),
           'Max\nsize\n(mm)' = round(max(shelllength), 0),
           n = n(),
           ref.a = sum(shelllength >= 140),
           ref.b = sum(shelllength >= 142),
           ref.c = sum(shelllength >= 145),
           '>140mm\n(%)' = round((ref.a/n)*100),
           '>142mm\n(%)' = round((ref.b/n)*100),
           '>145mm\n(%)' = round((ref.c/n)*100)) %>% 
 ungroup() %>% 
 mutate_if(is.factor,
           fct_explicit_na,
           na_level = '') %>%
 mutate(species = ifelse(species == 1, 'Blacklip', 'Greenlip')) %>% 
 dplyr::rename('SubBlock' = subblockno,
               'Species' = species) %>% 
 as.data.frame() %>%
 select(-c(n, ref.a, ref.b, ref.c, zone)) %>% 
 dplyr::rename('Diver' = processor) %>% 
 select(c(Diver, SubBlock, Species, 'Number\nmeasured', "Mean\nsize\n(mm)", "Min\nsize\n(mm)", "Max\nsize\n(mm)",
          ">140mm\n(%)", ">142mm\n(%)", ">145mm\n(%)"))


setwd("C:/CloudStor/R_Stuff/MMLF/MM_Plots/MM_Woodham")

write.xlsx(woodie.west,
           file = paste('Woodham_WEST__Measureboard_NonModem_SizeSummaryReferenceLimits_', Sys.Date(), '.xlsx'),
           sheetName = "Sheet1",
           col.names = TRUE,
           row.names = TRUE,
           append = FALSE)

write.xlsx(woodie.east,
           file = paste('Woodham_EAST__Measureboard_NonModem_SizeSummaryReferenceLimits_', Sys.Date(), '.xlsx'),
           sheetName = "Sheet1",
           col.names = TRUE,
           row.names = TRUE,
           append = FALSE)

##---------------------------------------------------------------------------##
## Step 16: Plot Blacklip LF  ####
## size frequency plot blacklip

measure.board.df.non.modem.bl <- measure.board.df.non.modem %>% 
  filter(species == 1)

mb.df.non.modem.bl.samples <- unique(measure.board.df.non.modem.bl$sample.id)

for (i in mb.df.non.modem.bl.samples) {
  plot.n.measured <- measure.board.df.non.modem.bl %>% 
    filter(!is.na(shelllength) &
             grepl('^07', logname) &
             sample.id == i &
             species == 1 &
             between(shelllength, 100, 220)) %>%
    distinct(abalonenum, rawutc, .keep_all = T) %>%  
    summarise(n = paste('n =', n()))
  
 plot.length.freq.dat <- measure.board.df.non.modem.bl %>%
  filter(grepl('^07', logname) &
          sample.id == i &
           species == 1 &
          between(shelllength, 100, 220)) %>% 
   distinct(abalonenum, rawutc, .keep_all = T)
 
 length.freq.plot <- ggplot(plot.length.freq.dat, aes(shelllength)) +
  geom_histogram(
   aes(y = ..density.. * 5),
   fill = '#EFC000FF',
   col = 'black',
   binwidth = 5,
   alpha = 0.6
  ) +
  coord_cartesian(xlim = c(100, 215), ylim = c(0, 0.5)) +
  theme_bw() +
  xlab("Shell Length (mm)")+
  ylab(paste('SubBlockNo', plot.length.freq.dat$subblockno, " Percentage (%)"))+
  scale_y_continuous(labels = percent_format(accuracy = 1, suffix = ''))+ 
   geom_vline(aes(xintercept = as.numeric(sizelimit)), linetype = 'dashed', colour = 'red', size = 0.5)+
   geom_text(data = plot.n.measured, aes(x = 200, y = 0.4, label = n), color = 'black', size = 3)
  
 # print(length.freq.plot)
 
 xbp.len <- ggplot(plot.length.freq.dat,
                   aes(
                    x = factor(logname),
                    y = shelllength,
                    group = logname
                   )) +
  geom_boxplot(
   fill = 'lightgray',
   outlier.colour = "black",
   outlier.size = 1.5,
   position = position_dodge(0.85),
   width = 0.25
  ) +
   stat_summary(fun.y = mean, geom = 'point', shape = 20, size = 3, colour = 'red', fill = 'red')+
  rotate() +
  theme_transparent()
 
 # print(xbp.len)
 
 xbp_grob <- ggplotGrob(xbp.len)
 xmin.len <- min(plot.length.freq.dat$shelllength)
 xmax.len <- max(plot.length.freq.dat$shelllength)
 
 length.plot <- length.freq.plot +
  annotation_custom(
   grob = xbp_grob,
   xmin = xmin.len,
   xmax = xmax.len,
   ymin = 0.3
  )
 
 # print(length.plot)
 
 plaindate <- unique(plot.length.freq.dat$plaindate)
 diver <- unique(plot.length.freq.dat$processor)
 zone <- unique(plot.length.freq.dat$zone)
 subblockno <- unique(plot.length.freq.dat$subblockno)
 
 setwd("C:/CloudStor/R_Stuff/MMLF/MM_Plots/MM_Woodham")
 ggsave(
  filename = paste(i, '_', diver, '_', as_date(plaindate),'_', zone, '_', subblockno, '_length.summary.plot', '.pdf', sep = ''),
  plot = length.plot,
  width = 7.4,
  height = 5.57,
  units = 'in'
 )
}

##---------------------------------------------------------------------------##
## Step 17: Plot Greenlip LF  ####
## size frequency plot greenlip

measure.board.df.non.modem.gl <- measure.board.df.non.modem %>% 
  filter(species == 2)

mb.df.non.modem.gl.samples <- unique(measure.board.df.non.modem.gl$sample.id)

for (i in mb.df.non.modem.gl.samples) {
  plot.n.measured <- measure.board.df.non.modem.gl %>% 
    filter(!is.na(shelllength) &
             grepl('^07', logname) &
             sample.id == i &
             species == 2 &
             between(shelllength, 100, 220)) %>%
    distinct(abalonenum, rawutc, .keep_all = T) %>%  
    summarise(n = paste('n =', n()))
  
  plot.length.freq.dat <- measure.board.df.non.modem.gl %>%
    filter(grepl('^07', logname) &
             sample.id == i &
             species == 2 &
             between(shelllength, 100, 220)) %>% 
    distinct(abalonenum, rawutc, .keep_all = T)
  
  length.freq.plot <- ggplot(plot.length.freq.dat, aes(shelllength)) +
    geom_histogram(
      aes(y = ..density.. * 5),
      fill = '#EFC000FF',
      col = 'black',
      binwidth = 5,
      alpha = 0.6
    ) +
    coord_cartesian(xlim = c(120, 215), ylim = c(0, 0.5)) +
    theme_bw() +
    xlab("Shell Length (mm)")+
    ylab(paste('SubBlockNo', plot.length.freq.dat$subblockno, " Percentage (%)"))+
    scale_y_continuous(labels = percent_format(accuracy = 1, suffix = ''))+ 
    geom_vline(aes(xintercept = as.numeric(sizelimit)), linetype = 'dashed', colour = 'red', size = 0.5)+
    geom_text(data = plot.n.measured, aes(x = 200, y = 0.4, label = n), color = 'black', size = 3)
  
  # print(length.freq.plot)
  
  xbp.len <- ggplot(plot.length.freq.dat,
                    aes(
                      x = factor(logname),
                      y = shelllength,
                      group = logname
                    )) +
    geom_boxplot(
      fill = 'lightgray',
      outlier.colour = "black",
      outlier.size = 1.5,
      position = position_dodge(0.85),
      width = 0.25
    ) +
    stat_summary(fun.y = mean, geom = 'point', shape = 20, size = 3, colour = 'red', fill = 'red')+
    rotate() +
    theme_transparent()
  
  # print(xbp.len)
  
  xbp_grob <- ggplotGrob(xbp.len)
  xmin.len <- min(plot.length.freq.dat$shelllength)
  xmax.len <- max(plot.length.freq.dat$shelllength)
  
  length.plot <- length.freq.plot +
    annotation_custom(
      grob = xbp_grob,
      xmin = xmin.len,
      xmax = xmax.len,
      ymin = 0.3
    )
  
  # print(length.plot)
  
  plaindate <- unique(plot.length.freq.dat$plaindate)
  diver <- unique(plot.length.freq.dat$processor)
  zone <- unique(plot.length.freq.dat$zone)
  subblockno <- unique(plot.length.freq.dat$subblockno)
  
  setwd("C:/CloudStor/R_Stuff/MMLF/MM_Plots/MM_Woodham")
  ggsave(
    filename = paste(i, '_', diver, '_', as_date(plaindate),'_', zone, '_', subblockno, '_length.summary.plot', '.pdf', sep = ''),
    plot = length.plot,
    width = 7.4,
    height = 5.57,
    units = 'in'
  )
}

##---------------------------------------------------------------------------##
## Step 18: Plot measure location ####

# read in Subblock map as an sf::sfc polygon object
sf.tas.map <- st_read("C:/Users/jaimem/Dropbox/AbaloneData/SpatialLayers/TasLand.gpkg")
sf.tas.coast.map <- st_read("C:/Users/jaimem/Dropbox/AbaloneData/SpatialLayers/TasCoastLine.gpkg")

# set CRS
GDA2020 <- st_crs(7855)
GDA94 <- st_crs(28355)
WGS84 <- st_crs(4326)

sf.tas.coast.map <-sf.tas.coast.map %>% 
  st_set_crs(GDA2020)

# transform sf.tas.map to GDA2020
sf.tas.map <- st_transform(sf.tas.map, GDA2020)
sf.tas.coast.map <- st_transform(sf.tas.coast.map, GDA2020)

# create bbox for area(s) of interest (Note: possibly look to add buffer)
st.helens.island <- st_bbox(
  c(
    xmin = 611443.2131532715,
    ymin = 5421139.764842981,
    xmax = 613192.9442468286,
    ymax = 5422172.58085798
  ),
  crs = GDA2020
)


# filter/crop measureboard data to area of interest
df.1 <- measure.board.df.non.modem %>% 
  # filter(between(shelllength, 138, 200)) %>% 
  st_as_sf() %>% 
  st_crop(., st.helens.island)

# size.classes <- c(0, 138, 140, 150, 160, 170, 180, 200)
# 
# df.1 <- df.1 %>% 
#   mutate(size.class = cut(shelllength, size.classes, include.lowest = T, right = F))

# convert to sp
df.2 <- as_Spatial(df.1)
# str(df.2)

# crop coastal map to area of interest
sf.tas.coast.map.crop <- st_crop(sf.tas.map, st.helens.island)
# class(sf.tas.coast.map.crop)

sp.tas.coast.map.crop <- as_Spatial(sf.tas.coast.map.crop)
# class(sf.tas.coast.map.crop.sp)

# grid (1 ha side = 402.0673 m ?)
idw_grid <- st_make_grid(df.1, cellsize = 402.0673, square = FALSE)
# st_crs(idw_grid)

# idw
P_idw_hex <- gstat::idw(shelllength ~ 1, df.2, newdata = idw_grid, idp = 2)

rslt_hex <- st_as_sf(P_idw_hex)
  

ggplot(data = st_geometry(sf.tas.coast.map.crop)) +
  
  geom_sf(data = rslt_hex, aes(fill = var1.pred), col = "grey60", size = 0.1)+
  scale_fill_viridis_c()+
  geom_sf(data = df.1) +
  geom_sf() +
    # geom_sf(data = df.1) +
  # scale_colour_viridis_c(option = "viridis") +
  theme_bw() +
  theme(legend.key.height=unit(1,"cm")) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.05, "cm"), pad_y = unit(0.1, "cm"),
                         style = north_arrow_fancy_orienteering) +
  labs(fill = 'Shell length\n(mm)')

##---------------------------------------------------------------------------##
## Step 19: Weight Grade Estimate ####
## Estimate weight and grading of individual abalone from catch sample
## Create combined length and weight grading plot

# Load most recent compilation of Commercial Abalone Catch Sampling data.

## Read in most recent commercial catch sampling compiled MM dataframe
compiledMM.df.final <- readRDS('C:/CloudStor/R_Stuff/MMLF/compiledMM.df.final.RDS')

## remove erroneous data
lw.dat <- compiledMM.df.final %>%
 mutate(sizelimit = as.numeric(sizelimit)) %>% 
  filter(!is.na(shell.length) &
          !is.na(whole.weight) &
          !is.na(sizelimit) &
   between(whole.weight, 200, 1500) & # abalone above or below these weights unlikely
    (shell.length >= sizelimit - 5 & shell.length <= 220) &  
    # between(shell.length, sizelimit - 5, 220))  # removes calibration measures around 100 mm and accounts for minor measuring error for abalone near the LML
      !(shell.length > 175 & whole.weight < 600) & # these appear to be erroneous weights
    !(shell.length > 180 & whole.weight < 1000))# these appear to be erroneous weights

## Calculate log of length and weight
lw.dat.log <- lw.dat %>% 
  mutate(log.sl = log(shell.length),
         log.wt = log(whole.weight))

## Calculate length-weight regression coefficients for each zone
lw.dat.coeff.zone <- lw.dat.log %>%
  nest(data = -newzone) %>% 
  mutate(fit = map(data, ~ lm(log.wt ~ log.sl, data = .x)),
         tidied = map(fit, broom::tidy)) %>% 
  unnest(tidied) %>%   
  filter(term %in% c('(Intercept)', 'log.sl')) %>% 
  select(c(newzone, estimate, term)) %>% 
  as.data.frame() %>% 
  spread(., term, estimate) %>%  
  dplyr::rename(b = 'log.sl',
                intercept = "(Intercept)") %>%  
  mutate(a = exp(intercept)) %>% 
  select(c(newzone, a, b))

## Join length-weight regression parameters by zone to diver measuring board lengths
## and estimate weight

measure.board.df.non.modem.est.wt <- measure.board.df.non.modem %>% 
  left_join(., lw.dat.coeff.zone, by = c('zone' = 'newzone')) %>% 
  mutate(est.weight = ((a * (shelllength ^ b))))

## Add weight grading to estimated weights
mb.df.non.modem.graded <- measure.board.df.non.modem.est.wt %>% 
  mutate(grade = dplyr::if_else(est.weight == 0, NA_character_,
                                dplyr::if_else(between(est.weight, 1, 400), 'xsmall',
                                               dplyr::if_else(between(est.weight, 1, 600), 'small', 
                                                              dplyr::if_else(between(est.weight, 601, 800), 'medium', 'large')))),
         est.weight = replace(est.weight, est.weight == 0, NA))

## Determine number of abalone measured per sample
sample.id.n.meas <- mb.df.non.modem.graded %>% 
  group_by(zone, sample.id, processor, plaindate) %>% 
  # group_by(zone, sample.id, processor, plaindate, docket.index) %>% 
  summarise(ab.meas = n()) 

## Determine number of estimated weights per sample
sample.id.n.weighed <- mb.df.non.modem.graded %>% 
  filter(!is.na(est.weight)) %>%
  group_by(zone, sample.id, processor, plaindate) %>% 
  summarise(ab.weighed = n())

## Create summary table of abalone measured and estimated weights
sample.id.n <- left_join(sample.id.n.meas, sample.id.n.weighed)

## Determine number of abalone measured by grade per sample
sample.id.grade.meas <- mb.df.non.modem.graded %>% 
  filter(!is.na(est.weight)) %>% 
  group_by(sample.id, grade, processor, plaindate) %>% 
  summarise(grade.meas = n())

## Select blacklip abalone (#NOTE: minimal data to generate accurate greenlip length-weight relationship)
measure.board.df.non.modem.bl <- measure.board.df.non.modem %>% 
  filter(species == 1)

## Identify unique sample ID numbers for generating plot in loop
mb.df.non.modem.bl.samples <- unique(measure.board.df.non.modem.bl$sample.id)


## Create a dataframe of weight grading threshold for plotting reference lines
grades <- data.frame(y = 0.35, x = c(300, 500, 700, 900), 
                     lab = c('XSmall', 'Small', 'Medium', 'Large'))

## Create plot
for (i in mb.df.non.modem.bl.samples) {
  # create length plot
  plot.length.freq.dat <- mb.df.non.modem.graded %>%
    filter(sample.id == i &
             between(shelllength, 100, 200))
  
  plot.n.measured <- mb.df.non.modem.graded %>% 
    filter(!is.na(shelllength) &
             sample.id == i
           & between(shelllength, 100, 200)) %>%  
    summarise(n = paste('n =', n()))
  
  plot.zone <- unique(plot.length.freq.dat$zone)
  sample.id.day <- unique(plot.length.freq.dat$sample.id)
  
  length.freq.plot <- ggplot(plot.length.freq.dat, aes(shelllength)) +
    geom_histogram(
      aes(y = ..density.. * 5),
      fill = '#EFC000FF',
      col = 'black',
      binwidth = 5,
      alpha = 0.6
    ) +
    coord_cartesian(xlim = c(100, 200), ylim = c(0, 0.45)) +
    theme_bw() +
    xlab("Shell Length (mm)") +
    ylab(paste('SubBlockNo', plot.length.freq.dat$subblockno, " Percentage (%)"))+
    geom_vline(aes(xintercept = as.numeric(sizelimit)), linetype = 'dashed', colour = 'red', size = 0.5)+
    # geom_vline(
    #   aes(xintercept = ifelse(zone == 'AW', 145, 
    #                           ifelse(zone == 'AB', 114, 
    #                                  ifelse(zone == 'AN', 127, 
    #                                         ifelse(zone == 'AG', 145, 138))))),
    #   linetype = 'dashed',
    #   colour = 'red',
    #   size = 0.5
    # ) +
    scale_y_continuous(labels = percent_format(accuracy = 1, suffix = ''))+
    geom_text(data = plot.n.measured, aes(x = 180, y = 0.3, label = n), color = 'black', size = 3)
  
  xbp.len <- ggplot(plot.length.freq.dat,
                    aes(
                      x = factor(sample.id.day),
                      y = shelllength,
                      group = sample.id.day
                    )) +
    geom_boxplot(
      fill = 'lightgray',
      outlier.colour = "black",
      outlier.size = 1.5,
      position = position_dodge(0.85),
      width = 0.5
    ) +
    rotate() +
    theme_transparent()
  
  xbp_grob <- ggplotGrob(xbp.len)
  xmin.len <- min(plot.length.freq.dat$shelllength)
  xmax.len <- max(plot.length.freq.dat$shelllength)
  
  length.plot <- length.freq.plot +
    annotation_custom(
      grob = xbp_grob,
      xmin = xmin.len,
      xmax = xmax.len,
      ymin = 0.3
    )
  
  # create weight plot
  
  plot.weight.freq.dat <- mb.df.non.modem.graded %>%
    filter(sample.id == i &
             est.weight != 0)
  
  plot.n.weighed <- mb.df.non.modem.graded %>% 
    filter(sample.id == i &
             est.weight != 0) %>% 
    summarise(n = paste('n =', n()))
  
  if (nrow(plot.weight.freq.dat) != 0) {
    
    weight.freq.plot <- ggplot(plot.weight.freq.dat, aes(est.weight)) +
      geom_histogram(
        aes(y = ..density.. * 50),
        fill = '#0073C2FF',
        col = 'black',
        binwidth = 50,
        alpha = 0.6
      ) +
      coord_cartesian(xlim = c(100, 1300),
                      ylim = c(0, 0.35)) +
      theme_bw() +
      theme(panel.grid = element_blank()) +
      xlab("Estimated Whole weight (g)") +
      ylab(paste('SubBlockNo', plot.length.freq.dat$subblockno, " Percentage (%)"))+
      geom_vline(aes(xintercept = 400),
                 colour = 'red',
                 linetype = 'dotted') +
      geom_vline(aes(xintercept = 600),
                 colour = 'red',
                 linetype = 'dotted') +
      geom_vline(aes(xintercept = 800),
                 colour = 'red',
                 linetype = 'dotted') +
      geom_text(
        data = grades,
        aes(
          x = x,
          y = y,
          label = lab
        ),
        inherit.aes = FALSE,
        vjust = 0.5,
        hjust = 0.5,
        size = 4,
        angle = 0,
        colour = c(
          'xsmall' = '#868686FF',
          'small' = '#EFC000FF',
          'medium' = '#0073C2FF',
          'large' = '#CD534CFF'
        )
      ) +
      scale_y_continuous(labels = percent_format(accuracy = 1, suffix = ''))+
      geom_text(data = plot.n.weighed, aes(x = 850, y = 0.25, label = n), color = 'black', size = 3)
    
    xbp.wt <- ggplot(
      plot.weight.freq.dat,
      aes(
        x = factor(sample.id.day),
        y = est.weight,
        group = sample.id.day
      )
    ) +
      geom_boxplot(
        fill = 'lightgray',
        outlier.colour = "black",
        outlier.size = 1.5,
        position = position_dodge(0.85),
        width = 0.3
      ) +
      stat_summary(fun.y = mean, geom = 'point', shape = 20, size = 3, colour = 'red', fill = 'red')+
      rotate() +
      theme_transparent()
    
    xbp_grob <- ggplotGrob(xbp.wt)
    xmin.wt <- min(plot.weight.freq.dat$est.weight)
    xmax.wt <- max(plot.weight.freq.dat$est.weight)
    
    # add pie chart to weight frequency plot
    
    sample.id.day.grade.summary <-
      left_join(sample.id.n, sample.id.grade.meas, by = c('processor', 'plaindate', 'sample.id')) %>%
      mutate(grade.perc = round((grade.meas / ab.weighed) * 100))
    
    pie.plot.dat <- sample.id.day.grade.summary %>%
      filter(sample.id == i) %>%
      mutate(
        lab.position = cumsum(grade.perc),
        lab.y.position = lab.position - grade.perc / 2,
        lab = paste0(grade.perc, "%")
      )
    
    docket.pie.plot <-
      ggplot(data = pie.plot.dat, aes(
        x = "",
        y = grade.perc,
        fill = grade
      )) +
      geom_bar(stat = "identity", colour = 'white') +
      coord_polar(theta = "y") +
      geom_text(
        aes(label = lab),
        position = position_stack(vjust = 0.5),
        colour = 'white',
        size = 5
      ) +
      theme_void() +
      theme(legend.position = 'none') +
      scale_fill_manual(
        values = c(
          'xsmall' = '#868686FF',
          'small' = '#EFC000FF',
          'medium' = '#0073C2FF',
          'large' = '#CD534CFF'
        )
      )
    
    pp_grob <- ggplotGrob(docket.pie.plot)
    
    wt.plot <- weight.freq.plot +
      annotation_custom(
        grob = xbp_grob,
        xmin = xmin.wt,
        xmax = xmax.wt,
        ymin = 0.25
      ) +
      annotation_custom(
        grob = pp_grob,
        xmin = 900,
        xmax = 1200,
        ymax = 0.4
      )
    
    #combine length and weight plots
    
    plot.a <- grid.arrange(
      arrangeGrob(cowplot::plot_grid(wt.plot, length.plot, align = 'v', 
                                     ncol = 1), ncol = 1))
    
    #save plots
    setwd("C:/CloudStor/R_Stuff/MMLF/MM_Plots/MM_Woodham")
    plaindate <- unique(plot.length.freq.dat$plaindate)
    diver <- unique(plot.length.freq.dat$processor)
    zone <- unique(plot.length.freq.dat$zone)
    subblockno <- unique(plot.length.freq.dat$subblockno)
    
    ggsave(
      filename = paste(i, '_', diver, '_', as_date(plaindate),'_', zone, '_', subblockno, '_SUMMARYPLOT', '.pdf', sep = ''),
   plot = plot.a,
      width = 200,
      height = 297,
      units = 'mm')
  }
  else{
    plot.a <- length.plot
    
    #save plots
    setwd("C:/CloudStor/R_Stuff/MMLF/MM_Plots/MM_Woodham")
    plaindate <- unique(plot.length.freq.dat$plaindate)
    diver <- unique(plot.length.freq.dat$processor)
    zone <- unique(plot.length.freq.dat$zone)
    subblockno <- unique(plot.length.freq.dat$subblockno)
    
    ggsave(
      filename = paste(i, '_', diver, '_', as_date(plaindate),'_', zone, '_', subblockno, '_SUMMARYPLOT', '.pdf', sep = ''),
      plot = plot.a,
      width = 7.4,
      height = 5.57,
      units = 'in')
  }
}

##---------------------------------------------------------------------------##
## Step 20: Plot measure location ####

#NOTE: Version2 - work in progress

# Filter for data of interest
df.1 <- measure.board.df.non.modem %>% 
 filter(sample.id == 108) %>% 
 st_as_sf()

idw_grid <- st_make_grid(df.1, cellsize = 402.0673, square = FALSE)
idw_grid <- st_make_grid(df.1, cellsize = 200, square = FALSE)
df.2 <- as_Spatial(df.1)
P_idw_hex <- gstat::idw(shelllength ~ 1, df.2, newdata = idw_grid, idp = 2)
rslt_hex <- st_as_sf(P_idw_hex)

df.2 <- df.1 %>%
 mutate(longitude = unlist(map(df.1$geometry,1)),
        latitude = unlist(map(df.1$geometry,2)))

map.buffer <- 100

ggplot() + 
 geom_sf(data = sf.tas.coast.map) +
 # geom_sf(data = df.2 %>% filter(shelllength < 200), aes(colour = shelllength)) +
 # scale_colour_viridis_c()+
 geom_sf(data = rslt_hex, aes(fill = var1.pred), col = "grey60", size = 0.1)+
 scale_fill_viridis_c()+
 # geom_point(data = Points_properties, 
 #            aes(x = Longitude, y = Latitude))+
 coord_sf(xlim = c(min(df.2$longitude) - map.buffer,
                   max(df.2$longitude) + map.buffer),
          ylim = c(min(df.2$latitude) - map.buffer,
                   max(df.2$latitude) + map.buffer),
          expand = FALSE) +
 theme_bw()

ggplot(rslt_hex, aes(fill = var1.pred))
         