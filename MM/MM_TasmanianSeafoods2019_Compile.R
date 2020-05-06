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
library(chron)

setwd('R:/TAFI/TAFI_MRL_Sections/Wild_Fisheries_Program/Shared/13. Market measuring/TasmanianSeafoods2019')

## create list of csv files in working directory
mm.files <- list.files(pattern = '*.csv')

## import all csv files from work directory to R
for (i in 1:length(mm.files)) assign(mm.files[i], read.csv(mm.files[i]))

## identify max column numbers from files in working directory
no.col <- ncol(read.csv(mm.files[8], header = FALSE, nrows = 1))

## create vector to select the desired columns and populate with NA's and identify additional columns
## to exclude from the import (i.e. blank cells or cells containing pivot table summaries)
colClasses <- replace(rep("NULL", no.col), c(1:8), NA)

## read in all csv files and required columns and compile as list
mm.list <- lapply(mm.files, read.csv, header = T, colClasses = colClasses)

## add source file to each csv in list to check for duplication
names(mm.list) <- mm.files
for(i in mm.files) mm.list[[i]]$filesource = i

## combine list into single dataframe
mm.data <- do.call("rbind", mm.list)

## convert column names to lowercase and rename to more appropriate variables
colnames(mm.data) <- tolower(colnames(mm.data))

mm.data <- mm.data %>% 
 rename(board.sn = sn,
        cap.date = docket_date,
        docket.number = docket_no,
        size = length,
        weight = weight,
        msr.date = date,
        msr.time = time,
        order = index)

## convert dates and times
mm.data$msr.time <- chron::chron(times = mm.data$msr.time)
mm.data$msr.date <- lubridate::dmy(mm.data$msr.date)
mm.data$cap.date <- lubridate::dmy(mm.data$cap.date)
mm.data$mesrtime <- lubridate::ymd_hms(paste(mm.data$msr.date, mm.data$msr.time))


## remove records with no docket number, blank docket number or no shell length and remove duplicate records
mm.data <- mm.data[!is.na(mm.data$docket.number),]
mm.df <- mm.data %>% 
 filter(docket.number != '' |!is.na(size)) %>% 
        select(-c(filesource, msr.date, msr.time, board.sn, cap.date)) %>% 
        distinct()

tas.seafoods.oct2019.df <- mm.df %>% 
        mutate(datasource = 'TASMANIAN SEAFOODS PTY LTD',
               downloaddate = as.Date(ymd('2019-10-16'))) %>% 
        extract(docket.number, into = c("zone", "docket"), "^([A-Z]+)(\\d+)$") %>% 
        mutate(zone = gsub('A', '', zone),
               docket = as.integer(docket),
               downloaddate = as.POSIXct(downloaddate))

str(tas.seafoods.oct2019.df)

saveRDS(tas.seafoods.oct2019.df, 'R:/TAFI/TAFI_MRL_Sections/Wild_Fisheries_Program/Shared/13. Market measuring/TasmanianSeafoods2019/TasmanianSeafoodsOct2019.RDS')

# ## create summary table for each docket number
# mm.df.sum <- mm.df %>% 
#  group_by(docket.number) %>% 
#  summarise(meanSL = mean(shell.length),
#            minSL = min(shell.length),
#            maxSL = max(shell.length),
#            n = length(shell.length))

