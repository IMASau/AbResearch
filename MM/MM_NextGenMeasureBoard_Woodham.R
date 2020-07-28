##---------------------------------------------------------------------------##
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
})
##---------------------------------------------------------------------------##
## Local working folder ####

# sftp.local <- "R:/TAFI/TAFI_MRL_Sections/Abalone/AbTrack/RawData/sftpServer/FilesNew"
measureboard.non.modem <- "C:/CloudStor/R_Stuff/MMLF/MM_Plots/MM_Woodham"

##---------------------------------------------------------------------------##
## Uncompress NextGen files from sfptServer ####

## Choose directory where compressed files for import are located
imp_dir <- measureboard.non.modem

## Choose directory where unpacked .txt files are to be saved
out_dir <- measureboard.non.modem

## UnPack compressed files (tar.gz) ####
# Get a full list of all compressed files in the import directory
dirlist_cmp <- list_files_with_exts(imp_dir,c("tar.gz"), full.names=FALSE)
# Get a full list of all unpacked .txt files in the export directory
dirlist_txt <- list_files_with_exts(out_dir,c("txt"), full.names=FALSE)
## Convert both lists to data frames and combine
dirlistdf_cmp <- as.data.frame(dirlist_cmp)
dirlistdf_cmp$dirlist_cmp <- as.character(dirlistdf_cmp$dirlist_cmp)
dirlistdf_cmp <- dirlistdf_cmp %>% rename(FileName = dirlist_cmp)
dirlistdf_txt <- as.data.frame(dirlist_txt)
dirlistdf_txt$dirlist_txt <- as.character(dirlistdf_txt$dirlist_txt)
dirlistdf_txt <- dirlistdf_txt %>% rename(FileName = dirlist_txt)

dirlistdf_unp <- rbind(dirlistdf_cmp,dirlistdf_txt)

# Find file names and extensions
dirlistdf_unp <- dirlistdf_unp %>% separate(FileName,c("F_name","F_ext"),sep="[.]",remove=F)

#Remove unpacked files from list (ie. find duplicates)
unpacked_files <- dirlistdf_unp[duplicated(dirlistdf_unp[,c("F_name")]),]
files_to_unpack <- dirlistdf_unp %>% 
 filter(!F_name %in% c(unpacked_files$F_name)) %>% 
 filter(F_ext == c("tar"))

# Unpack files
tic()
numfiles <- nrow(files_to_unpack)
if (numfiles > 0){
 for (f in 1:numfiles){
  untar(paste(imp_dir,"\\",files_to_unpack$FileName[f],sep=""),exdir = out_dir)
 }}
toc()

##---------------------------------------------------------------------------##
## Extract .txt files ####

## Extract .txt files ####

## Extract data from sftp download folder and compile into dataframe
## Note: measuring board .txt files are normally denoted with '07' prefix however these are field-based
## measuring boards and are linked to a diver GPS unit (i.e. prefix '05'). They only record length data as there is currently
## no weight integration.

localfiles <- list.files(measureboard.non.modem,  pattern = "^05.*txt", full.names = T) 

localfiles.dat <- lapply (localfiles, read.table, sep = ",", header = F, row.names = NULL, as.is = T,
                          colClasses = c("character", "numeric", "numeric", "numeric", "character", "character"))
localfiles.df <- do.call(rbind, localfiles.dat)

logged.data <- localfiles.df

##---------------------------------------------------------------------------##
## Extract info from data packet ####

# rename variable names of dataframe

colnames(logged.data) <- c("logname", "seqindex","identifier","rawutc","datapack","crc_status")

# identify measuring board logger names

unique(logged.data$logname)

# identify number of records for each identifier
table(logged.data$identifier)

logged.data$logger_date <- as.POSIXct(logged.data$rawutc, origin = "1970-01-01", tz = "GMT")
logged.data$local_date <- as.POSIXct(logged.data$rawutc, origin = "1970-01-01", tz = "Australia/BRISBANE")
logged.data <- logged.data %>%  
 mutate(plaindate = as.Date(local_date, tz="Australia/BRISBANE"))

tail(logged.data)


##---------------------------------------------------------------------------##
## Check LoggerName data ####

logname <- filter(logged.data, identifier == 32962) 
logname <-  separate(logname, datapack, c("abalonenum","devicename"), sep = ",", remove = FALSE,
                     convert = TRUE) %>%
 as.data.frame()

# remove duplicate records resulting from upload failures or loggers going out of range
logname <- logname %>% 
  distinct(logname, seqindex, identifier, .keep_all = T)

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

gps.RMC <- left_join(gps.RMC_B,select(gps.RMC_A, local_date, longitude, latitude), by=c("local_date")) 

# remove duplicate records resulting from upload failures or loggers going out of range 
# and filter out measuring board records
gps.RMC <- gps.RMC %>%
  distinct(logname, seqindex, identifier, .keep_all = T) %>% 
  filter(grepl('^07', logname))
 
tail(gps.RMC)

table(gps.RMC$plaindate)

##---------------------------------------------------------------------------##
## Step 4: Extract Docket details ####
docket <- filter(logged.data, identifier == 32963) 
docket <-  separate(docket, datapack, c("abalonenum","zone", "docketnum"), sep = ",", remove = FALSE,
                    convert = TRUE) %>%
 as.data.frame()

# remove duplicate records resulting from upload failures or loggers going out of range
docket <- docket %>% 
  distinct(logname, seqindex, identifier, .keep_all = T)

tail(docket)

##---------------------------------------------------------------------------##
## Step 5: Extract length ####
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
                          select(logname, rawutc, abalonenum), by = "rawutc") %>% 
 left_join(select(docket, rawutc, zone, docketnum), by = "rawutc") %>% 
 left_join(select(ablength, rawutc,shelllength), by = "rawutc") %>% 
 left_join(select(abweight, rawutc, wholeweight), by = "rawutc")

tail(lengthweight)

measure.board.df <- lengthweight

measure.board.df.non.modem <- measure.board.df

##---------------------------------------------------------------------------##
## Step 8: Identify multiple samples in a day and remove any trial/practice data ####

## identify different samples based on breaks in abalonenum sequence
mb.df.non.modem.seq <- measure.board.df.non.modem %>%
 arrange(local_date) %>% 
 mutate(sample.id = cumsum(c(-1, diff(abalonenum)) < 0)) %>% 
 group_by(sample.id)

## determine number of abalone in each sample
mb.df.non.modem.seq.no <- mb.df.non.modem.seq %>% 
 summarise(sample.n = n())

mb.df.non.modem.seq.no.join <- left_join(mb.df.non.modem.seq, mb.df.non.modem.seq.no)

## remove trial/practice samples
## Ben Allen indicated he made some trial measurements on 2020-07-14
measure.board.df.non.modem <- mb.df.non.modem.seq.no.join %>% 
 filter(sample.n >= 5) %>% 
 select(-c(sample.id, sample.n))

## remove any other known testing data
measure.board.df.non.modem <- measure.board.df.non.modem %>% 
 filter(plaindate != '2020-06-30')

##---------------------------------------------------------------------------##
# # manually add zone and docket number to measurboard dataframe where missing
# woodham.docketnums <- data.frame(plaindate = as.Date(c("2020-05-05", "2020-05-06", "2020-05-18", "2020-05-19", 
#                                                        "2020-05-25", "2020-05-26", "2020-05-27", "2020-06-22", 
#                                                        "2020-06-24", "2020-06-25", "2020-07-07", "2020-07-08")), 
#                                  zone = c('AE', 'AE', 'AE', 'AE', 'AW', 'AW', 'AW', 'AW', 'AW', 'AW', 'G', 'G'), 
#                                  docketnum = c(525976, 525977, 525979, 525980, 813251, 813251, 813251, NA, NA, NA, NA, NA),
#                                  blockno = c(NA, NA, NA, NA, '11B', '11B', '12B', '11C', '10B', '12A', '39A', '31B'))
# 
# measure.board.df.woodham <- left_join(select(measure.board.df.woodham, -c(zone, docketnum)), 
#                                       woodham.docketnums, by = 'plaindate')
##---------------------------------------------------------------------------##
## Step 9: Determine sample location ####

## calculate mean position for each sample
mb.samp.loc <- measure.board.df.non.modem %>% 
 filter(latitude < 0) %>% 
 group_by(logname, plaindate) %>% 
 summarise(mean.long = mean(as.numeric(longitude)),
           mean.lat = mean(as.numeric(latitude)))  

## convert to spatial points data frame and set CRS to WGS84
coordinates(mb.samp.loc) <- ~ mean.long + mean.lat  
proj4string(mb.samp.loc) <- CRS("+proj=longlat")

## set EPSG CRS
GDA2020 <- CRS(SRS_string='EPSG:7855')

## convert lat/long to UTM and set CRS to GDA2020
mb.samp.loc.sp <- spTransform(mb.samp.loc, GDA2020)

## load abalone spatial block layer
new.ab.blocks.sp <- readOGR(dsn = 'C:/GitCode/r-AbSpatialAnalyses/Tas_Ab_Polyg_SubBlocks.shp'
                            , layer = 'Tas_Ab_Polyg_SubBlocks', verbose = F)

# convert measuring board sample locations and spatial blocks to sf data frame
mb.samp.loc.sf <- st_as_sf(mb.samp.loc.sp)
new.ab.blocks.sf <- st_as_sf(new.ab.blocks.sp)

## transform spatial block data from GDA94 to GDA2020
new.ab.blocks.sf <- st_set_crs(new.ab.blocks.sf, GDA2020)

## join data frames to identify sampling block and zone
mb.samp.loc.df <- st_join(mb.samp.loc.sf, new.ab.blocks.sf, join = st_within) %>% 
 st_set_geometry(NULL) %>% 
 select(c(logname, plaindate, BlockNo, Zone, SubBlockNo)) %>% 
 rename_all(tolower)

measure.board.df.non.modem <- left_join(select(measure.board.df.non.modem, -c(zone, docketnum)),
                  mb.samp.loc.df, by = c('plaindate', 'logname'))

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
 measure.board.df.non.modem,
 mb.invent,
 by = c(
  "logname" = "logname",
  "plaindate" = "startdate",
  "plaindate" = "enddate"
 ),
 match_fun = list(`==`, `>=`, `<=`)
) %>% 
 select(-c(logname.y, startdate, enddate, platformscales, comments)) %>% 
 rename('logname' = logname.x)

measure.board.df.non.modem <- mb.df.non.modem.diver

##---------------------------------------------------------------------------##

## Step 11: save RDS of dataframe ####

saveRDS(measure.board.df.non.modem, 'C:/CloudStor/R_Stuff/MMLF/MM_Plots/measure.board.df.non.modem.RDS')

# measure.board.df.non.modem <- readRDS('C:/CloudStor/R_Stuff/MMLF/MM_Plots/measure.board.df.non.modem.RDS')
##---------------------------------------------------------------------------##
## Step 12: load data ####

## load most recent RDS data frame of non modem measuring board data
measure.board.df.non.modem <- readRDS('C:/CloudStor/R_Stuff/MMLF/MM_Plots/measure.board.df.non.modem.RDS')

## create summary table for each sample date and diver

# list of unique measureboards for summary and plot loops
lognames <- unique(measure.board.df.non.modem$logname)

for (i in lognames){

mb.df.non.modem.summary <- measure.board.df.non.modem %>%
  filter(!is.na(shelllength) &
           grepl('^07', logname) &
           between(shelllength, 120, 220) &
          logname == i) %>%  
  # distinct(plaindate, abalonenum, .keep_all = T) %>%  
  group_by(plaindate, logname, zone, subblockno, processor) %>% 
  summarise('Number\nmeasured' = n(),
            'Mean\nsize\n(mm)' = round(mean(shelllength), 0),
            'Min\nsize\n(mm)' = round(min(shelllength), 0),
            'Max\nsize\n(mm)' = round(max(shelllength), 0)) %>% 
  arrange(desc(plaindate)) %>%
  ungroup() %>% 
  mutate_if(is.factor,
            fct_explicit_na,
            na_level = '') %>% 
 rename('Dive date' = plaindate,
        'Block' = subblockno) %>% 
 as.data.frame() %>%
 select(-c(zone)) %>% 

# ## add diver detail to summary table
# mb.df.non.modem.summary.diver <- fuzzy_left_join(
#  mb.df.non.modem.summary,
#  mb.invent,
#  by = c(
#   "logname" = "logname",
#   "Dive date" = "startdate",
#   "Dive date" = "enddate"
#  ),
#  match_fun = list(`==`, `>=`, `<=`)
# ) %>% 
 rename('Diver' = processor) %>% 
 select(c("Diver", "Dive date",  "Block", "Number\nmeasured", "Mean\nsize\n(mm)", "Min\nsize\n(mm)", "Max\nsize\n(mm)"))

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
  height = 5.57,
  units = 'in'
)

}

##---------------------------------------------------------------------------##
## summary table with % above reference points

# list of unique measureboards for summary and plot loops
lognames <- unique(measure.board.df.non.modem$logname)

for (i in lognames){

mb.df.non.modem.summary.ref <- measure.board.df.non.modem %>%
  filter(!is.na(shelllength) &
           grepl('^07', logname) &
           between(shelllength, 120, 220) &
          logname == i) %>%   
 distinct(abalonenum, rawutc, .keep_all = T) %>%
 group_by(plaindate, logname, zone, subblockno, processor) %>%  
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
 rename('Dive date' = plaindate,
        'Block' = subblockno) %>% 
 as.data.frame() %>%
 select(-c(n, ref.a, ref.b, ref.c, zone)) %>% 
 rename('Diver' = processor) %>% 
 select(c("Dive date", Diver, Block, 'Number\nmeasured', "Mean\nsize\n(mm)", "Min\nsize\n(mm)", "Max\nsize\n(mm)",
          ">150mm\n(%)", ">155mm\n(%)", ">160mm\n(%)"))

diver <- unique(mb.df.non.modem.summary.ref$Diver)

mb.df.non.modem.summary.reference.limits <- mb.df.non.modem.summary.ref %>% 
  ggpubr::ggtexttable(rows = NULL, theme = ggpubr::ttheme('mOrange'))

setwd("C:/CloudStor/R_Stuff/MMLF/MM_Plots/MM_Woodham")

ggsave(
  filename = paste(diver, '_Measureboard_NonModem_SizeSummaryReferenceLimits_Formatted_', Sys.Date(), '.pdf', sep = ''),
  plot = mb.df.non.modem.summary.reference.limits,
  width = 11.69,
  height = 5.57,
  units = 'in'
)
}

##---------------------------------------------------------------------------##
## size frequency plot

mb.df.non.modem.samples <- unique(measure.board.df.non.modem$sample.id)

for (i in mb.df.non.modem.samples) {
  plot.n.measured <- measure.board.df.non.modem %>% 
    filter(!is.na(shelllength) &
             grepl('^07', logname) &
             sample.id == i &
             between(shelllength, 120, 220)) %>%
    distinct(abalonenum, rawutc, .keep_all = T) %>%  
    summarise(n = paste('n =', n()))
  
 plot.length.freq.dat <- measure.board.df.non.modem %>%
  filter(grepl('^07', logname) & #!is.na(docketnum) &
          sample.id == i &
          between(shelllength, 120, 220)) %>% 
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
  # geom_vline(aes(xintercept = 138), colour = 'red',
  #            linetype = 'dashed', size = 0.5)+
  scale_y_continuous(labels = percent_format(accuracy = 1, suffix = ''))+ 
   geom_vline(aes(xintercept = ifelse(zone == 'W', 145, 
                                      ifelse(zone == 'E', 138,
                                             ifelse(zone == 'G', 145, 
                                                    ifelse(subblockno == '31B', 127, 138))))),
              linetype = 'dashed', colour = 'red', size = 0.5)+
 # geom_vline(
 #   aes(xintercept = 145),
 #   linetype = 'dashed',
 #   colour = 'red',
 #   size = 0.5
 #  )+
   geom_text(data = plot.n.measured, aes(x = 180, y = 0.4, label = n), color = 'black', size = 3)
  
 
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
 
 plaindate <- unique(plot.length.freq.dat$plaindate)
 
 # ## add diver detail
 # plot.length.freq.dat.diver <- fuzzy_left_join(
 #  plot.length.freq.dat,
 #  mb.invent,
 #  by = c(
 #   "logname" = "logname",
 #   "plaindate" = "startdate",
 #   "plaindate" = "enddate"
 #  ),
 #  match_fun = list(`==`, `>=`, `<=`))
 
 diver <- unique(plot.length.freq.dat$processor)
 
 setwd("C:/CloudStor/R_Stuff/MMLF/MM_Plots/MM_Woodham")
 ggsave(
  filename = paste(i,'_', diver,'_', as_date(plaindate), '_length.summary.plot', '.pdf', sep = ''),
  plot = length.plot,
  width = 7.4,
  height = 5.57,
  units = 'in'
 )
 
}

##---------------------------------------------------------------------------##
