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
library(tidyverse)
library(dplyr)
library(ggsci)
library(ggpubr)
library(scales)
})
##---------------------------------------------------------------------------##
## Local working folder ####

# sftp.local <- "R:/TAFI/TAFI_MRL_Sections/Abalone/AbTrack/RawData/sftpServer/FilesNew"
woodham <- "C:/CloudStor/R_Stuff/MMLF/MM_Plots/MM_Woodham"

##---------------------------------------------------------------------------##
## Extract .txt files ####

## Extract .txt files ####

## Extract data from sftp download folder and compile into dataframe
## Note: measuring board .txt files are normally denoted with '07' prefix however these are field-based
## measuring boards and are linked to a diver GPS unit (i.e. prefix '05'). They only record length data as there is currently
## no weight integration.

localfiles <- list.files(woodham,  pattern = "^05.*txt", full.names = T) 

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

measure.board.df.woodham <- measure.board.df

##---------------------------------------------------------------------------##
# remove testing data
measure.board.df.woodham <- measure.board.df.woodham %>% 
  filter(plaindate != '2020-06-30')

##---------------------------------------------------------------------------##
# manually add zone and docket number to measurboard dataframe where missing
woodham.docketnums <- data.frame(plaindate = as.Date(c("2020-05-05", "2020-05-06", "2020-05-18", "2020-05-19", 
                                                       "2020-05-25", "2020-05-26", "2020-05-27", "2020-06-22", 
                                                       "2020-06-24", "2020-06-25", "2020-07-07", "2020-07-08")), 
                                 zone = c('AE', 'AE', 'AE', 'AE', 'AW', 'AW', 'AW', 'AW', 'AW', 'AW', 'G', 'G'), 
                                 docketnum = c(525976, 525977, 525979, 525980, 813251, 813251, 813251, NA, NA, NA, NA, NA),
                                 blockno = c(NA, NA, NA, NA, '11B', '11B', '12B', '11C', '10B', '12A', '39A', '31B'))

measure.board.df.woodham <- left_join(select(measure.board.df.woodham, -c(zone, docketnum)), 
                                      woodham.docketnums, by = 'plaindate')

##---------------------------------------------------------------------------##
## Step 10: save RDS of dataframe ####

saveRDS(measure.board.df.woodham, 'C:/CloudStor/R_Stuff/MMLF/MM_Plots/MM_Woodham/measure.board.df.woodham.RDS')

# measure.board.df.woodham <- readRDS('C:/CloudStor/R_Stuff/MMLF/MM_Plots/MM_Woodham/measure.board.df.woodham.RDS')
##---------------------------------------------------------------------------##
## summary table

woodham.summary <- measure.board.df.woodham %>%
  filter(!is.na(shelllength) &
           grepl('^07', logname) &
           between(shelllength, 120, 200)) %>%  
  # distinct(plaindate, abalonenum, .keep_all = T) %>%  
  group_by(plaindate, docketnum, zone) %>% 
  summarise('Number\nmeasured' = n(),
            'Mean\nsize\n(mm)' = round(mean(shelllength), 0),
            'Min\nsize\n(mm)' = round(min(shelllength), 0),
            'Max\nsize\n(mm)' = round(max(shelllength), 0)) %>% 
  arrange(desc(plaindate)) %>%
  ungroup() %>% 
  mutate_if(is.factor,
            fct_explicit_na,
            na_level = '') %>%
  mutate(docketnum = paste(zone, docketnum, sep = '')) %>% 
  rename('Dive date' = plaindate,
         'Docket\nnumber' = docketnum) %>%
  as.data.frame() %>% 
  select(-zone)

woodham.summary.formated <- woodham.summary %>% 
  ggpubr::ggtexttable(rows = NULL, theme = ggpubr::ttheme('mOrange'))

setwd("C:/CloudStor/R_Stuff/MMLF/MM_Plots/MM_Woodham")

ggsave(
  filename = paste('Woodham_SizeSummary_Formatted_', Sys.Date(), '.pdf', sep = ''),
  plot = woodham.summary.formated,
  width = 11.69,
  height = 5.57,
  units = 'in'
)

##---------------------------------------------------------------------------##
## summary table with % above reference points

woodham.summary.ref <- measure.board.df.woodham %>%
  filter(!is.na(shelllength) &
           grepl('^07', logname) &
           between(shelllength, 120, 200)) %>%  
  # distinct(plaindate, abalonenum, .keep_all = T) %>%  
  group_by(plaindate, docketnum, zone) %>%  
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
  mutate(docketnum = paste(zone, docketnum, sep = '')) %>% 
  rename('Dive date' = plaindate,
         'Docket\nnumber' = docketnum) %>%
  select(-c(n, ref.a, ref.b, ref.c, zone)) %>% 
  as.data.frame()

woodham.summary.reference.limits <- woodham.summary.ref %>% 
  ggpubr::ggtexttable(rows = NULL, theme = ggpubr::ttheme('mOrange'))

setwd("C:/CloudStor/R_Stuff/MMLF/MM_Plots/MM_Woodham")

ggsave(
  filename = paste('Woodham_SizeSummaryReferenceLimits_Formatted_', Sys.Date(), '.pdf', sep = ''),
  plot = woodham.summary.reference.limits,
  width = 11.69,
  height = 5.57,
  units = 'in'
)

##---------------------------------------------------------------------------##
## size frequency plot

woodham.dates <- unique(measure.board.df.woodham$plaindate)

for (i in woodham.dates) {
  plot.n.measured <- measure.board.df.woodham %>% 
    filter(!is.na(shelllength) &
             grepl('^07', logname) &
             plaindate == i &
             between(shelllength, 120, 200)) %>%
    distinct(abalonenum, .keep_all = T) %>%  
    summarise(n = paste('n =', n()))
  
 plot.length.freq.dat <- measure.board.df.woodham %>%
  filter(grepl('^07', logname) & #!is.na(docketnum) &
          plaindate == i &
          between(shelllength, 120, 200)) %>% 
   distinct(abalonenum, .keep_all = T)
 
 length.freq.plot <- ggplot(plot.length.freq.dat, aes(shelllength)) +
  geom_histogram(
   aes(y = ..density.. * 5),
   fill = '#EFC000FF',
   col = 'black',
   binwidth = 5,
   alpha = 0.6
  ) +
  coord_cartesian(xlim = c(120, 200), ylim = c(0, 0.5)) +
  theme_bw() +
  xlab("Shell Length (mm)")+
  ylab(paste(" Percentage (%)"))+
  # geom_vline(aes(xintercept = 138), colour = 'red',
  #            linetype = 'dashed', size = 0.5)+
  scale_y_continuous(labels = percent_format(accuracy = 1, suffix = ''))+ 
   geom_vline(aes(xintercept = ifelse(zone == 'AW', 145, 
                                      ifelse(zone == 'AE', 138,
                                             ifelse(zone == 'G', 145, 138)))),
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
                    x = factor(docketnum),
                    y = shelllength,
                    group = docketnum
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
 
 setwd("C:/CloudStor/R_Stuff/MMLF/MM_Plots/MM_Woodham")
 ggsave(
  filename = paste(as_date(i), '_length.summary.plot', '.pdf', sep = ''),
  plot = length.plot,
  width = 7.4,
  height = 5.57,
  units = 'in'
 )
 
}

##---------------------------------------------------------------------------##
df.1 <- measure.board.df.woodham %>% 
  sf::st_as_sf(coords = c('longitude', 'latitude'))
# GDA2020 <- CRS(SRS_string='EPSG:7855')
df.1 <- df.1 %>% 
  # st_set_crs(GDA2020) %>% 
  filter(plaindate == '2020-07-08' &
           abalonenum != 449)

ggplot() + 
  geom_sf(fill = 'salmon', color = 'white') +
  geom_sf(data = df.1) +
  theme_minimal() +
  ylab("Latitude") +
  xlab("Longitude")

outname.point <- "C:/CloudStor/R_Stuff/MMLF/MM_Plots/MM_Woodham/measureboardGPSdata2020-07-08.gpkg"
st_write(df.1, dsn = outname.point, layer = "point", driver = "GPKG")


df.2 <- measure.board.df.woodham %>% 
  filter(plaindate == '2020-07-07')

setwd('C:/CloudStor/R_Stuff/MMLF/MM_Plots/MM_Woodham')
write.xlsx(df.2,
           file = 'measureboardGPSdata2020-07-07.xlsx',
           sheetName = "Sheet1",
           col.names = TRUE,
           row.names = TRUE,
           append = FALSE)
