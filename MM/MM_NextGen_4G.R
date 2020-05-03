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

## Local working folder

sftp.local <- "R:/TAFI/TAFI_MRL_Sections/Abalone/AbTrack/RawData/sftpServer/FilesNew"

#sftp.local <- "c:/CloudStor/Shared/AbTrack/NextGen/Data/"


##---------------------------------------------------------------------------##
## Using base functions
# localfiles <- list.files(sftp.local,  pattern = "*.txt") %>%
#  as.data.frame()
# colnames(localfiles) <- c("InFileName")

# tail(localfiles)

##---------------------------------------------------------------------------##
## Extract .txt files from sftp download folder and compile into dataframe
## Measuring board .txt files are denoted with '07' prefix

localfiles <- list.files(sftp.local,  pattern = "^07.*txt", full.names = T) 

localfiles.dat <- lapply (localfiles, read.table, sep = ",", header = F, row.names = NULL, as.is = T,
                  colClasses = c("character", "numeric", "numeric", "numeric", "character", "character"))
localfiles.df <- do.call(rbind, localfiles.dat)

logged.data <- localfiles.df

#localfiles[58,]

filesize <- file.size(paste0(sftp.local,"/", localfiles.df$InFileName)) %>%
 as.numeric()

tail(filesize)

fileinf <- file.info(paste0(sftp.local,"/", localfiles.df$InFileName)) 

tail(fileinf)


#file_info(paste0(sftp.local,"/",localfiles)) %>%
#  as.data.frame()


##Get info about files in local sftp folder (uses library(fs))
## Need to loop through this, or work out how to use apply
# locals <- file_info(paste0(sftp.local,"/",localfiles.df$InFileName)) %>%
#  as.data.frame()
# 
# tail(locals)
# 
# ## Note: modification_time from file_info appears to = creation date on the ftp server
# 
# ## Need to loop through this, or work out how to use apply
# fs.pre <- file_size(paste0(sftp.local,"/", localfiles.df$InFileName)) %>%
#  as.numeric()
# 
# tail(fs.pre)

#sftp.files$OutFileName <- paste0("archive/",sftp.files$InFileName)

#sftp_rename(sftp.files$InFileName[5], paste0("../archive/",sftp.files$InFileName[5]), sftp_connection = sftp_con, verbose = TRUE, curlPerformVerbose = FALSE)

##---------------------------------------------------------------------------##
## Extract info from data packet ####


# <<<<<<< HEAD
## manual choice of input file
# infile <- choose.files(paste0(sftp.local,"07020055_20200122.txt"))
# =======
## Step 1: Manual choice of input file ####
# infile <- choose.files(paste0(sftp.local,"/07*.txt"))
# >>>>>>> 8207f5d73c94c99dec08f59769fe392c758f236f

## Specific file
# infile <- paste0(sftp.local,"/", sftp.files$InFileName[5])

# logged.data <- read.csv(infile,
#                             skip = 6,
#                             header=FALSE, 
#                             sep=',', 
#                             as.is=TRUE,
#                             colClasses=c("character", "numeric", "numeric", "numeric", "character", "character"))

colnames(logged.data) <- c("logname", "seqindex","identifier","rawutc","datapack","crc_status")


# infile
unique(logged.data$logname)
## Number of records for each identifier
table(logged.data$identifier)

logged.data$logger_date <- as.POSIXct(logged.data$rawutc, origin = "1970-01-01", tz = "GMT")
logged.data$local_date <- as.POSIXct(logged.data$rawutc, origin = "1970-01-01", tz = "Australia/BRISBANE")
logged.data <- logged.data %>%  mutate(plaindate = as.Date(local_date, tz="Australia/BRISBANE"))

tail(logged.data)


##---------------------------------------------------------------------------##
## Check LoggerName data ####
logname <- filter(logged.data, identifier == 32962) 
logname <-  separate(logname, datapack, c("abalonenum","devicename"), sep = ",", remove = FALSE,
                     convert = TRUE) %>%
 as.data.frame()

#head(logname)
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

#head(gps.RMC_A)
tail(gps.RMC_A)



##---------------------------------------------------------------------------##
## Step 3B: Extract GPS RMC Part B ####
gps.RMC_B <- filter(logged.data, identifier == 221) %>%
 separate(datapack, c("valid","speed", "course","variation"), sep = ",", remove = FALSE,
          convert = FALSE) %>%
 as.data.frame()

#head(gps.RMC_B)
tail(gps.RMC_B)


##---------------------------------------------------------------------------##
## Step 3C: Join RMC Part A & B   ####

gps.RMC <- left_join(gps.RMC_B,select(gps.RMC_A, local_date, longitude, latitude), by=c("local_date")) 

tail(gps.RMC)

table(gps.RMC$plaindate)

pick <- which(duplicated(gps.RMC$local_date) == TRUE)
gps.RMC[pick,]



##---------------------------------------------------------------------------##
## Step 4: Extract Docket details ####
docket <- filter(logged.data, identifier == 32963) 
docket <-  separate(docket, datapack, c("abalonenum","zone", "docketnum"), sep = ",", remove = FALSE,
                     convert = TRUE) %>%
 as.data.frame()

#head(docket)
tail(docket)


##---------------------------------------------------------------------------##
## Step 5: Extract Weight length ####
ablength <- filter(logged.data, identifier == 32964) 
ablength <-  separate(ablength, datapack, c("abalonenum","shelllength"), sep = ",", remove = FALSE,
                     convert = TRUE) %>%
 as.data.frame()

#head(ablength)
tail(ablength)


##---------------------------------------------------------------------------##
## Step 6: Extract Weight  ####
abweight <- filter(logged.data, identifier == 32965) 
abweight <-  separate(abweight, datapack, c("abalonenum","wholeweight"), sep = ",", remove = FALSE,
                      convert = TRUE) %>%
 as.data.frame()

#head(abweight)
tail(abweight)



##---------------------------------------------------------------------------##
## Step 7: Join components into a flat form ####

lengthweight <- left_join(select(gps.RMC, logname, rawutc, logger_date, local_date, plaindate, latitude, longitude),
                          select(logname, rawutc, abalonenum), by = "rawutc") %>% 
 left_join(select(docket, rawutc, zone, docketnum), by = "rawutc") %>% 
 left_join(select(ablength, rawutc,shelllength), by = "rawutc") %>% 
 left_join(select(abweight, rawutc, wholeweight), by = "rawutc")

tail(lengthweight)
# <<<<<<< HEAD

measure.board.df <- lengthweight

##---------------------------------------------------------------------------##
## Step 8: clean and save dataframe ####

# remove measuring board testing data (pre-deployment lognames and random test docket numbers) 
measure.board.df <- measure.board.df %>% 
        filter(logname %in% c(7020055, 7020056, 7020057, 7020058) | 
                       !docketnum %in% c(45575, 111111, 123456, 222222, 323232, 
                                         333333, 454545, 474747, 555555, 565656,
                                         616161, 654321, 666666, 811881))
# save RDS of dataframe
saveRDS(measure.board.df, 'C:/CloudStor/R_Stuff/MMLF/measure.board.df.RDS')

##---------------------------------------------------------------------------##
## Quick summaries ####

# quick summary of measureboard data
measure.board.df %>% filter(!is.na(docketnum)) %>%  
        group_by(docketnum) %>% 
        summarise(sampdate = max(plaindate),
                  n = n(),
                  minsl = min(shelllength),
                  medsl = median(shelllength),
                  maxweight = max(wholeweight)) %>% 
        arrange(sampdate)


##----------------------------------------------## 
## Extras: Manual date check ####

checktime <- 1579611010

pointintime <- as.POSIXct(checktime, origin = "1970-01-01", tz = "GMT")

localpointintime <- as.POSIXct(checktime, origin = "1970-01-01", tz = "Australia/BRISBANE")
localpointintime
>>>>>>> 8207f5d73c94c99dec08f59769fe392c758f236f

