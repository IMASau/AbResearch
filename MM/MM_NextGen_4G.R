
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

## Local working folder

#sftp.local <- "R:/TAFI_MRL_Sections/Abalone/AbTrack/RawData/sftpServer/FilesNew"

sftp.local <- "c:/CloudStor/Shared/AbTrack/NextGen/Data/"


##---------------------------------------------------------------------------##
## Using base functions
localfiles <- list.files(sftp.local,  pattern = "*.txt") %>%
 as.data.frame()
colnames(localfiles) <- c("InFileName")

tail(localfiles)

#localfiles[58,]

filesize <- file.size(paste0(sftp.local,"/", localfiles$InFileName)) %>%
 as.numeric()

tail(filesize)

fileinf <- file.info(paste0(sftp.local,"/", localfiles$InFileName)) 

tail(fileinf)


#file_info(paste0(sftp.local,"/",localfiles)) %>%
#  as.data.frame()


##Get info about files in local sftp folder (uses library(fs))
## Need to loop through this, or work out how to use apply
locals <- file_info(paste0(sftp.local,"/",localfiles$InFileName)) %>%
 as.data.frame()

tail(locals)

## Note: modification_time from file_info appears to = creation date on the ftp server

## Need to loop through this, or work out how to use apply
fs.pre <- file_size(paste0(sftp.local,"/", localfiles$InFileName)) %>%
 as.numeric()

tail(fs.pre)




#sftp.files$OutFileName <- paste0("archive/",sftp.files$InFileName)

#sftp_rename(sftp.files$InFileName[5], paste0("../archive/",sftp.files$InFileName[5]), sftp_connection = sftp_con, verbose = TRUE, curlPerformVerbose = FALSE)

##---------------------------------------------------------------------------##
## Extract info from data packet ####


## manual choice of input file
infile <- choose.files(paste0(sftp.local,"07020055_20200122.txt"))

## Specific file
# infile <- paste0(sftp.local,"/", sftp.files$InFileName[5])

logged.data <- read.csv(infile,
                            skip = 6,
                            header=FALSE, 
                            sep=',', 
                            as.is=TRUE,
                            colClasses=c("character", "numeric", "numeric", "numeric", "character", "character"))

colnames(logged.data) <- c("logname", "seqindex","identifier","rawutc","datapack","crc_status")


infile
unique(logged.data$logname)
## Number of records for each identifier
table(logged.data$identifier)

logged.data$logger_date <- as.POSIXct(logged.data$rawutc, origin = "1970-01-01", tz = "GMT")
logged.data$local_date <- as.POSIXct(logged.data$rawutc, origin = "1970-01-01", tz = "Australia/BRISBANE")
logged.data <- logged.data %>%  mutate(plaindate = as.Date(local_date, tz="Australia/BRISBANE"))

tail(logged.data)

##---------------------------------------------------------------------------##
## Extract Battery voltage  ####

loggerbattery <- filter(logged.data, identifier %in% c(32833) ) %>%
 separate(datapack, c("volts"), sep = ",", remove = FALSE,
          convert = FALSE) %>%
 arrange(logname,local_date) %>% 
 as.data.frame()

tail(loggerbattery)


##---------------------------------------------------------------------------##
## Extract GPS RMC Part A  ####
gps.RMC_A <- filter(logged.data, identifier == 220) %>%
 separate(datapack, c("longitude","latitude"), sep = ",", remove = FALSE,
          convert = FALSE) %>%
 as.data.frame()

#head(gps.RMC_A)
tail(gps.RMC_A)



##---------------------------------------------------------------------------##
## Extract GPS RMC Part B ####
gps.RMC_B <- filter(logged.data, identifier == 221) %>%
 separate(datapack, c("valid","speed", "course","variation"), sep = ",", remove = FALSE,
          convert = FALSE) %>%
 as.data.frame()

#head(gps.RMC_B)
tail(gps.RMC_B)


##---------------------------------------------------------------------------##
## Join RMC Part A & B   ####

gps.RMC <- left_join(gps.RMC_B,select(gps.RMC_A, local_date, longitude, latitude), by=c("local_date")) 

tail(gps.RMC)

table(gps.RMC$plaindate)

pick <- which(duplicated(gps.RMC$local_date) == TRUE)
gps.RMC[pick,]


##---------------------------------------------------------------------------##
## Extract LoggerName data ####
logname <- filter(logged.data, identifier == 32962) 
logname <-  separate(logname, datapack, c("abalonenum","devicename"), sep = ",", remove = FALSE,
                     convert = TRUE) %>%
 as.data.frame()

#head(logname)
tail(logname)

##---------------------------------------------------------------------------##
## Extract Docket details ####
docket <- filter(logged.data, identifier == 32963) 
docket <-  separate(docket, datapack, c("abalonenum","zone", "docketnum"), sep = ",", remove = FALSE,
                     convert = TRUE) %>%
 as.data.frame()

#head(docket)
tail(docket)


##---------------------------------------------------------------------------##
## Extract Weight length ####
ablength <- filter(logged.data, identifier == 32964) 
ablength <-  separate(ablength, datapack, c("abalonenum","shelllength"), sep = ",", remove = FALSE,
                     convert = TRUE) %>%
 as.data.frame()

#head(ablength)
tail(ablength)


##---------------------------------------------------------------------------##
## Extract Weight  ####
abweight <- filter(logged.data, identifier == 32965) 
abweight <-  separate(abweight, datapack, c("abalonenum","wholeweight"), sep = ",", remove = FALSE,
                      convert = TRUE) %>%
 as.data.frame()

#head(abweight)
tail(abweight)



##---------------------------------------------------------------------------##
## join components into a flat form ####

lengthweight <- left_join(select(gps.RMC, logname, rawutc, logger_date, local_date, plaindate, latitude, longitude),
                          select(logname, rawutc, abalonenum), by = "rawutc") %>% 
 left_join(select(docket, rawutc, zone, docketnum), by = "rawutc") %>% 
 left_join(select(ablength, rawutc,shelllength), by = "rawutc") %>% 
 left_join(select(abweight, rawutc, wholeweight), by = "rawutc")

IMAS056 <- lengthweight

