# The following scipt is used to extract docket details from processors held in FILMS database 
# which are then matched to length frequency/measuring board data compiled in MM_Analysis.R. 

# Run the script periodically to refresh with the latest processor data in order to match with new measuring board data.
# NOTE: there can be a delay between processors submitting catch dockets and DPIPWE entering data on FILMS
# and it may not be possible to match with more recent length frequency data unitl these are processed.

# Created by: Craig Mundy (edited by: Jaime McAllister) 

##--------------------------------------------------------------------------##
# load libaries ####

library(RODBC)
library(R.utils)
library(lubridate)
library(tidyverse)
library(gdata)
library(tictoc)
source("C:/GitCode/AbResearch/codeBLnewzone.r")

# setwd("c:/CloudStor/R_Stuff/AutoAssess")

##--------------------------------------------------------------------------##
# identify data folder
mm_data_folder <- paste(sprintf('C:/Users/%s/Dropbox (UTAS Research)/DiveFisheries/Abalone/MMdata/',
                                Sys.info()[["user"]]))
##--------------------------------------------------------------------------##

## QMS/LMM Extract data ####
## QMS/LMM Extract data from the TAFIPROD processor and docketlink views
## setup connection to cray_res

chFilms.QMS <- odbcConnect("FILMS", uid = "cray_res", pwd = "cray_res0cray_resSNI", 
                       believeNRows = FALSE)

## QMS/LMM Extract list of processor license ids 
sql.proc <- "SELECT *
FROM ABPROCESSORS p
ORDER BY p.QUDO_ID"
abprocs.qms <- sqlQuery(chFilms.QMS, sql.proc)

## QMS/LMM versionExtract dockets with join code based on the qudo_id/related_qudo_id
sql.dl <- "Select *
From abdocketlink dl
Order BY dl.QUDO_ID,dl.RELATED_QUDO_ID"
docketlink.qms <- sqlQuery(chFilms.QMS, sql.dl)
close(chFilms.QMS)

##--------------------------------------------------------------------------##
## FILMS Extract data ####
## FILMS Extract list of processor license ids and price
## NOTE: processor returns are submitted monthly, so some dockets and associated
## price info won't be available
## setup connection to ab_res
chFilms <-  odbcConnect("FILMS",
                        uid = "ab_res",
                        pwd = "dp1pW#",
                        believeNRows = FALSE)

sql.proc <- "select * from films_abprocessor2 p
 ORDER BY p.docket_id"
abprocs2 <- sqlQuery(chFilms, sql.proc)

sql.proc <- "select * from films_abprocessor3"
abprocs3 <- sqlQuery(chFilms, sql.proc)

sql.proc <- "select * from films_abprocessors4 p
 ORDER BY p.docket_id"
abprocs4 <- sqlQuery(chFilms, sql.proc)

sql.proc <- "select * from FILMS_ABPROCESSSOR5 p
 ORDER BY p.docket_id"
abprocs5 <- sqlQuery(chFilms, sql.proc)

sql.proc <- "select * from films_abdocketlink p
 where extract(year from p.received_date_time) > 1999
 ORDER BY p.docket_id"
docklink <- sqlQuery(chFilms, sql.proc)

sql.proc <- "select * from films_postz p
 ORDER BY p.docket_id"
postz <- sqlQuery(chFilms, sql.proc)

## FILMS Extract landed price info per docket 
sql.lp <- "select * from LANDED_PRICE_VW"
landedprice <- sqlQuery(chFilms, sql.lp)

## Note the landed price View contains qudo_ids that are NA

sql.pcl <- "select * from processor_certificate_lic_vw"
processorlicense <- sqlQuery(chFilms, sql.pcl)

close(chFilms)
##--------------------------------------------------------------------------##
## Join docket data ####

# change field names to lower case

var.names <- tolower(colnames(abprocs2))
colnames(abprocs2) <- var.names

var.names <- tolower(colnames(abprocs3))
colnames(abprocs3) <- var.names

var.names <- tolower(colnames(abprocs4))
colnames(abprocs4) <- var.names

var.names <- tolower(colnames(abprocs5))
colnames(abprocs5) <- var.names

var.names <- tolower(colnames(docklink))
colnames(docklink) <- var.names

var.names <- tolower(colnames(landedprice))
colnames(landedprice) <- var.names

var.names <- tolower(colnames(processorlicense))
colnames(processorlicense) <- var.names

# join docket data

abprocs.data <- abprocs5

abprocs.data <- abprocs.data %>%
  left_join(select(docklink, joincode, docket_id), by = 'docket_id')

##--------------------------------------------------------------------------##
## Re-format dataframe variables ####

# extract zone from string 

abprocs.data$zone <- substring(abprocs.data$docket_fishery_part, 1, 1)

# create subblockno field from blockno and convert values to uppercase

abprocs.data$subblockno <- gsub("ab", "", abprocs.data$abalone_block_code)
abprocs.data$subblockno <- toupper(abprocs.data$subblockno)

# create blockno from subblockno

abprocs.data$blockno <- as.integer(substring(abprocs.data$subblockno, 1, 2))
abprocs.data$docket_number <- as.character(abprocs.data$docket_number)
abprocs.data$master_docket_number <- as.character(abprocs.data$master_docket_number)

# create year column

abprocs.data$fishyear <- year(abprocs.data$fishing_date)

# add processor to docket details

abprocs.data <- left_join(abprocs.data, processorlicense, by = c("processor_licence_id" = "licence_id") )

##--------------------------------------------------------------------------##
## Master docket summaries ####
## Creates a summary table for each master docket based on the FILMS processor
## query version II

tic("start group by - short form")
  docketprocs.films <- abprocs.data %>% 
  group_by(fishyear, zone, joincode) %>% #group_by(fishyear, zone, joincode) 
  summarise(blocklist = paste(unique(blockno), collapse = ", "),
            numblocks = length(unique(blockno)),
            subblocklist = paste(unique(subblockno), collapse = ", "),
            numsubblocks = length(unique(subblockno)),
            numdocks = length(unique(docket_id)),
            docketidlist = paste(unique(docket_id), collapse = ", "),
            docketnumlist = paste(unique(docket_number), collapse = ", "),
            numprocs = length(unique(processor_entitlement)),
            proclist = paste(unique(processor_entitlement), collapse = ", "))  %>%
    as.data.frame()
  
  docketprocs.films <- docketprocs.films %>%
    separate(docketnumlist, c("dock1","dock2", "dock3","dock4"), sep = ",", remove = FALSE,
             convert = FALSE) 
  toc()

tic("start group by - long form")
docketprocs.films.mdn <- abprocs.data %>% 
  group_by(fishyear, zone, docket_id) %>%
  summarise(docket_number = unique(docket_number),
            masterdocket = unique(master_docket_number),
            masterdocketid = unique(joincode),
            #processor = unique(certificate_holder),
            blocklist = paste(unique(blockno), collapse = ", "),
            numblocks = length(unique(blockno)),
            subblocklist = paste(unique(subblockno), collapse = ", "),
            numsubblocks = length(unique(subblockno)),
            proc = paste(unique(processor_entitlement), collapse = ", "),
            numdays = length(unique(fishing_date)),
            daylist =  paste(unique(fishing_date), collapse = ", "),
            catch = sum(catch_weight)) %>%
  as.data.frame()
toc()

##--------------------------------------------------------------------------##
## Join master and child dockets ####

docketinfo.transfer <- docketprocs.films %>%
  select(joincode, numprocs, proclist) 

docketinfo <- left_join(docketprocs.films.mdn, docketinfo.transfer, by = c("masterdocketid" = "joincode"))

docketinfo <- left_join(docketinfo, select(processorlicense,processor_entitlement, expiry_year, certificate_holder) , by = c("proc" = "processor_entitlement", "fishyear" = "expiry_year"))

docketinfo_backup <- docketinfo
##--------------------------------------------------------------------------##
## Data clean ####

# remove alpha characters from docket_number in processor data

docketinfo <- docketinfo %>%
 mutate(docket_number = as.numeric((gsub('[^0-9]', '', docket_number)))) %>%
 mutate(masterdocket = as.numeric((gsub('[^0-9]', '', masterdocket))))

# change 'docket_number' and 'certifcate_holder' column name out of SQL format in docketinfo

# names(docketinfo)[names(docketinfo)=="docket_number"] <- "docket.number"
docketinfo <- docketinfo %>% 
 dplyr::rename(docket.number = docket_number,
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

##--------------------------------------------------------------------------##
## Save docketinfo ####

saveRDS(docketinfo, paste0(mm_data_folder, 'docketinfo3.rds'))
# saveRDS(docketinfo, "c:/CloudStor/R_Stuff/MMLF/docketinfo3.rds")
# saveRDS(docketinfo, "c:/CloudStor/R_Stuff/AutoAssess/docketinfo3.rds")
# docketinfo <- readRDS("c:/CloudStor/R_Stuff/AutoAssess/docketinfo3.rds")

##--------------------------------------------------------------------------##
 