##---------------------------------------------------------------------------##
# clear console
rm(list = ls())

## 1. Load libraries ####
suppressPackageStartupMessages({
        library(dplyr)
        library(ggplot2)
        library(scales)
        library(tidyr)
        library(gdata)
        library(openxlsx)
        library(lubridate)
        library(reshape)
        library(gridExtra)
        library(ggpubr)
        library(readxl)
        library(tibble)
        library(data.table)
        library(janitor)
        library(anytime)
        library(stringr)
        library(broom)
        library(purrr)
        library(sf)
        library(ggspatial)
        library(tmap)
        library(sf)
        library(sp)
        library(rgdal)
        library(RColorBrewer)
        library(viridis)
        library(ggpmisc)
        library(arsenal)
  library(fuzzyjoin)
 library(tidytext)
})

source("C:/GitCode/AbResearch/getLegend.r")
source("C:/GitCode/AbResearch/StandardError_Functions.r")

##---------------------------------------------------------------------------##
## 2. Set sample year and file paths ####

# identify sampling year of interest
samp.year <- 2023

# identify associated sampling year folder path to save dataframes
samp.year.folder <- file.path('C:', 'CloudStor', 'DiveFisheries', 
                              'Abalone', 'FISdata',
                              paste('FIS_TimedSwimSurveys', samp.year, sep = ''))

# identify associated sampling year folder path to save plots
ts.plots.folder <- file.path('C:', 'CloudStor', 'DiveFisheries', 
                             'Abalone', 'Assessment', 'Figures', 'FIS',
                             paste('FIS_TimedSwimSurvey', samp.year, '_Plots', sep = ''))
##---------------------------------------------------------------------------##
## 3. Load raw data ####

# load timed swim length frequency raw data
time.swim.dat <- read.xlsx("R:/TAFI/TAFI_MRL_Sections/Abalone/Section Shared/Abalone_databases/Data/Data for Transfer/2020/FIS_TimedSwim_RawData_2020.xlsx",
                       detectDates = T)

# load timed swim meta data
time.swim.meta.dat <- read.xlsx("R:/TAFI/TAFI_MRL_Sections/Abalone/Section Shared/Abalone_databases/Data/Data for Transfer/2020/FIS_TimedSwim_MetaData_2020.xlsx",
                           detectDates = T)
##---------------------------------------------------------------------------##
# remove blank or additional rows from raw data where cells have been pre-filled
# for data entry.This step can be deleted once data entry is complete.
time.swim.dat <- time.swim.dat %>% 
        filter(!is.na(starttime) &
                       !is.na(finishtime) &
                       !is.na(sizeclass_freq))
##---------------------------------------------------------------------------##
# clean site data by removing white space from Excel data entry
time.swim.dat <- time.swim.dat %>% 
        mutate(site = str_trim(site))

time.swim.meta.dat <- time.swim.meta.dat %>% 
        mutate(site = str_trim(site))

##---------------------------------------------------------------------------##
## 4. Raw data compile ####

# re-format Excel times for time.swim.dat
time.swim.dat$starttime <- convertToDateTime(time.swim.dat$starttime, origin = "1970-01-01", tz = "Australia/HOBART")
time.swim.dat$firstabtime <- convertToDateTime(time.swim.dat$firstabtime, origin = "1970-01-01", tz = "Australia/HOBART")
time.swim.dat$finishtime <- convertToDateTime(time.swim.dat$finishtime, origin = "1970-01-01", tz = "Australia/HOBART")

time.swim.dat <- time.swim.dat %>% 
        mutate(starttime = strftime(starttime, format="%H:%M:%S"),
               firstabtime = strftime(firstabtime, format="%H:%M:%S"),
               finishtime = strftime(finishtime, format="%H:%M:%S"))

time.swim.dat$starttime <- as.POSIXct(paste(time.swim.dat$sampdate, time.swim.dat$starttime), format = "%Y-%m-%d %H:%M:%S")
time.swim.dat$firstabtime <- as.POSIXct(paste(time.swim.dat$sampdate, time.swim.dat$firstabtime), format = "%Y-%m-%d %H:%M:%S")
time.swim.dat$finishtime <- as.POSIXct(paste(time.swim.dat$sampdate, time.swim.dat$finishtime), format = "%Y-%m-%d %H:%M:%S")

# re-format Excel times for time.swim.meta.dat
time.swim.meta.dat$time.in <- convertToDateTime(time.swim.meta.dat$time.in, origin = "1970-01-01", tz = "Australia/HOBART")
time.swim.meta.dat$time.out <- convertToDateTime(time.swim.meta.dat$time.out, origin = "1970-01-01", tz = "Australia/HOBART")

time.swim.meta.dat <- time.swim.meta.dat %>% 
        mutate(time.in = strftime(time.in, format="%H:%M:%S"),
               time.out = strftime(time.out, format="%H:%M:%S"))

time.swim.meta.dat$time.in <- as.POSIXct(paste(time.swim.meta.dat$date, time.swim.meta.dat$time.in), format = "%Y-%m-%d %H:%M:%S")
time.swim.meta.dat$time.out <- as.POSIXct(paste(time.swim.meta.dat$date, time.swim.meta.dat$time.out), format = "%Y-%m-%d %H:%M:%S")

time.swim.meta.dat <- time.swim.meta.dat %>% 
        dplyr::rename('sampdate' = date,
                      'starttime' = time.in,
                      'finishtime' = time.out) %>% 
        mutate(site = gsub('AB-22-Random1', 'WP-72-73', site),
               site = gsub('AB-22-Random2', 'WP-74-75', site))

# calculate elapsed dive time (seconds)
time.swim.dat <- time.swim.dat %>% 
        mutate(time.elapsed = ifelse(is.na(firstabtime), finishtime - starttime, finishtime - firstabtime))

# add sample year
time.swim.dat <- time.swim.dat %>% 
        mutate(sampyear = year(sampdate))

time.swim.dat <- time.swim.dat %>% 
        mutate(nums = str_count(site, '\\d+')) %>% 
        # mutate(site2 = site) %>% 
        mutate(site2 = ifelse(nums == 2, site, '')) %>% 
        separate(col = site2, into = c('ab', 'blockno'), sep = '-') %>% 
        mutate(site3 = ifelse(nums == 3, site, '')) %>% 
        separate(col = site3, into = c('ab', 'sampyear2', 'blockno2'), sep = '-') %>%
        mutate(blockno = ifelse(blockno %in% c(72, 74, 'LEG', 'THU'), 22, 
                                ifelse(blockno %in% c('BRS'), 13, blockno))) %>% 
        mutate(blockno = ifelse(is.na(blockno), blockno2, blockno)) %>% 
        dplyr::select(-c(ab, sampyear2, blockno2, nums))

# determine legal and sub-legal abalone
time.swim.dat <- time.swim.dat %>% 
        mutate(legal.size = ifelse(sizeclass %in% c("0-20", "20-40", "40-60", "60-80", "80-100", "100-120", "120-140"), '<140 mm', '>140 mm'))

# add mid-point to 2020 size classes; add 2021 size classes and add midpoint
time.swim.dat <- time.swim.dat %>% 
        separate(sizeclass, into = c('minsize', 'maxsize'), sep = '-', convert = TRUE, remove = F) %>% 
        mutate(midsize = (minsize + maxsize)/2,
               sizeclass.2021 = ifelse(maxsize <= 100, '0-100', sizeclass),
               midsize.2021 = ifelse(maxsize <= 100, 50, midsize))

# create data frame of individual abalone lengths
time.swim.dat.df <- time.swim.dat %>% 
        uncount(sizeclass_freq, .remove = F) %>% 
        dplyr::rename('shelllength' = midsize,
                      'shelllength.2021' = midsize.2021)

# manually add vessel name to metadata
time.swim.meta.dat <- time.swim.meta.dat %>% 
  mutate(vesselname = ifelse(sampdate == as.Date('2021-10-08'), 'Taroona', 'MoranaII'))

# save dataframes
saveRDS(time.swim.dat, paste(samp.year.folder, '/time.swim.dat.RDS', sep = ''))
saveRDS(time.swim.dat.df, paste(samp.year.folder, '/time.swim.dat.df.RDS', sep = ''))
saveRDS(time.swim.meta.dat, paste(samp.year.folder, '/time.swim.meta.dat.RDS', sep = ''))

##---------------------------------------------------------------------------##
## 5. Raw data check #### 
#quick check that sites match between raw and meta data

# create summary data frames of sites sampled by date
ts.dat.sites <- time.swim.dat %>% 
        dplyr::select(sampdate, site) %>% 
        distinct_all()

ts.meta.dat.sites <- time.swim.meta.dat %>% 
        dplyr::select(sampdate, site) %>% 
        distinct_all()

# compare data frames to see if the site-date combinations match
summary(arsenal::comparedf(ts.dat.sites, ts.meta.dat.sites, by = c('sampdate', 'site')))

##---------------------------------------------------------------------------##
## 6. Vessel GPS data compile ####
## load data from vessel GPS downloads and match to raw data

# define CRS
GDA2020 <- st_crs(7855)

# read GPX files from vessel plotter

gps_downloads_folder <- paste(sprintf("C:/Users/%s/OneDrive - University of Tasmania/IMAS-DiveFisheries-FIS-Data/FIS_VesselGPS_Downloads", Sys.info()[["user"]]))

# vessel data for 2020 surveys
# morana.gps.2020 <- st_read('C:/CloudStor/DiveFisheries/Abalone/FISdata/FIS_TimedSwimSurveys2021/MORANAII-2020-10-09_download.gpx', layer = 'waypoints')
morana.gps.2020 <- st_read(file.path(gps_downloads_folder, 'MORANAII-2020-10-09_download.gpx'), layer = 'waypoints')

# vessel data for 2021 additional surveys (i.e. block 13-14)
# morana.gps.ref <- st_read('C:/CloudStor/DiveFisheries/Abalone/FISdata/FIS_TimedSwimSurveys2021/MORANAII-2021-04-06_download.gpx', layer = 'waypoints')
morana.gps.ref <- st_read(file.path(gps_downloads_folder, 'MORANAII-2021-04-06_download.gpx'), layer = 'waypoints')

# vessel data for 2021 surveys
# morana.gps.2021.a <- st_read('C:/CloudStor/DiveFisheries/Abalone/FISdata/FIS_TimedSwimSurveys2021/MORANAII-2021-07-15_download.gpx', layer = 'waypoints')
morana.gps.2021.a <- st_read(file.path(gps_downloads_folder, 'MORANAII-2021-07-15_download.gpx'), layer = 'waypoints')
# morana.gps.2021.b <- st_read('C:/CloudStor/DiveFisheries/Abalone/FISdata/FIS_TimedSwimSurveys2021/MORANAII-2021-08-12_download.gpx', layer = 'waypoints')
morana.gps.2021.b <- st_read(file.path(gps_downloads_folder, 'MORANAII-2021-08-12_download.gpx'), layer = 'waypoints')
# morana.gps.2021.c <- st_read('C:/CloudStor/DiveFisheries/Abalone/FISdata/FIS_TimedSwimSurveys2021/MORANAII-2021-09-08_download.gpx', layer = 'waypoints')
morana.gps.2021.c <- st_read(file.path(gps_downloads_folder, 'MORANAII-2021-09-08_download.gpx'), layer = 'waypoints')
# morana.gps.2021.d <- st_read('C:/CloudStor/DiveFisheries/Abalone/FISdata/FIS_TimedSwimSurveys2021/MORANAII-2021-10-07_download.gpx', layer = 'waypoints')
morana.gps.2021.d <- st_read(file.path(gps_downloads_folder, 'MORANAII-2021-10-07_download.gpx'), layer = 'waypoints')
# taroona.gps.2021.a <- st_read('C:/CloudStor/DiveFisheries/Abalone/FISdata/FIS_TimedSwimSurveys2021/TAROONA-2021-10-15_download.gpx', layer = 'waypoints')
taroona.gps.2021.a <- st_read(file.path(gps_downloads_folder, 'TAROONA-2021-10-15_download.gpx'), layer = 'waypoints')

morana.gps.2021 <- bind_rows(morana.gps.2021.a, morana.gps.2021.b, morana.gps.2021.c, 
                             morana.gps.2021.d) %>% 
 distinct(., name, .keep_all = T)

# vessel data for 2022 surveys
# morana.gps.2022.a <- st_read('C:/CloudStor/DiveFisheries/Abalone/FISdata/FIS_TimedSwimSurveys2022/MORANAII-2022-08-10_download.gpx', layer = 'waypoints')
morana.gps.2022.a <- st_read(file.path(gps_downloads_folder, 'MORANAII-2022-08-10_download.gpx'), layer = 'waypoints')
# morana.gps.2022.b <- st_read('C:/CloudStor/DiveFisheries/Abalone/FISdata/FIS_TimedSwimSurveys2022/MORANAII-2022-09-06_download.gpx', layer = 'waypoints')
morana.gps.2022.b <- st_read(file.path(gps_downloads_folder, 'MORANAII-2022-09-06_download.gpx'), layer = 'waypoints')
# morana.gps.2022.c <- st_read('C:/CloudStor/DiveFisheries/Abalone/FISdata/FIS_TimedSwimSurveys2022/MORANAII-2022-09-27_download.gpx', layer = 'waypoints')
morana.gps.2022.c <- st_read(file.path(gps_downloads_folder, 'MORANAII-2022-09-27_download.gpx'), layer = 'waypoints')

morana.gps.2022 <- bind_rows(morana.gps.2022.a, morana.gps.2022.b, morana.gps.2022.c) %>% 
 distinct(., name, .keep_all = T)

# vessel data for 2023 surveys
# morana_gps_2023 <- st_read('C:/CloudStor/DiveFisheries/Abalone/FISdata/FIS_TimedSwimSurveys2023/TimedSwim_Actaeons/MORANAII-2023-03-27_download.gpx', layer = 'waypoints')
morana_gps_2023_a <- st_read(file.path(gps_downloads_folder, 'MORANAII-2023-03-27_download.gpx'), layer = 'waypoints')
# morana_gps_2023.b <- st_read('C:/CloudStor/DiveFisheries/Abalone/FISdata/FIS_TimedSwimSurveys2023/TimedSwim_Actaeons/MORANAII-2023-03-27_download.gpx', layer = 'waypoints')
morana_gps_2023_b <- st_read(file.path(gps_downloads_folder, 'MORANAII-2023-06-24_download.gpx'), layer = 'waypoints')
morana_gps_2023_c <- st_read(file.path(gps_downloads_folder, 'MORANAII-2023-07-17_download.gpx'), layer = 'waypoints')
morana_gps_2023_d <- st_read(file.path(gps_downloads_folder, 'MORANAII-2023-07-19_download.gpx'), layer = 'waypoints')
morana_gps_2023_e <- st_read(file.path(gps_downloads_folder, 'MORANAII-2023-08-03_download.gpx'), layer = 'waypoints')

morana_gps_2023 <- bind_rows(morana_gps_2023_a, 
                             morana_gps_2023_b, 
                             morana_gps_2023_c, 
                             morana_gps_2023_d,
                             morana_gps_2023_e) %>% 
 distinct(., name, .keep_all = T)

# add sample year (note: gps time refers to time waypoint was uploaded or taken,
# therefore DO NOT use this column to determine year. Waypoint numbers should 
# always correspond to the actual sample time whereas names will generally 
# correspond to an upload time unless they have been manually entered)

morana.gps.2020 <- morana.gps.2020 %>%
        mutate(sampyear = 2020,
               vesselname = 'MoranaII')

morana.gps.ref <- morana.gps.ref %>%
        mutate(sampyear = 2021,
               vesselname = 'MoranaII')

morana.gps.2021 <- morana.gps.2021 %>%
        mutate(sampyear = 2021,
               vesselname = 'MoranaII')

taroona.gps.2021 <- taroona.gps.2021.a %>%
  distinct(., name, .keep_all = T) %>% 
  mutate(sampyear = 2021,
         vesselname = 'Taroona',
         name = as.numeric(name),
         name = if_else(name < 10, as.character(paste('00', name, sep = '')),
                                   if_else(between(name, 10, 99), as.character(paste(0, name, sep = '')),
                                           as.character(name))))
morana.gps.2022 <- morana.gps.2022 %>%
 mutate(sampyear = 2022,
        vesselname = 'MoranaII')

morana_gps_2023 <- morana_gps_2023 %>%
 mutate(sampyear = 2023,
        vesselname = 'MoranaII')

# combine dataframes
vessel.gps <- bind_rows(morana.gps.2020, morana.gps.ref, morana.gps.2021, taroona.gps.2021,
                        morana.gps.2022, morana_gps_2023) %>%  
        mutate(gpsdate = as.Date(time),
               gpstime = time) %>% 
        select(c(name, sampyear, gpstime, gpsdate, geometry, vesselname))

# load latest compiled timed swim meta dataframe
time.swim.meta.dat <- readRDS(paste(samp.year.folder, '/time.swim.meta.dat.RDS', sep = ''))

# separate start positions
ts.site.start <- time.swim.meta.dat %>%
        select(-c(waypoint.finish, finishtime)) %>% 
        dplyr::rename('waypoint' = waypoint.start,
                      'samptime' = starttime) %>% 
        mutate(sampperiod = 'start',
               sampyear = year(sampdate))

# separate finish positions
ts.site.finish <- time.swim.meta.dat %>%
        select(-c(waypoint.start, starttime)) %>% 
        dplyr::rename('waypoint' = waypoint.finish,
                      'samptime' = finishtime) %>% 
        mutate(sampperiod = 'finish',
               sampyear = year(sampdate))

# re-join start and finish positions and remove non-sampled sites (e.g. Betsey)        
ts.site.start.finish <- bind_rows(ts.site.start, ts.site.finish) %>%
        mutate(waypoint = if_else(waypoint < 10, as.character(paste('00', waypoint, sep = '')),
                                  if_else(between(waypoint, 10, 99), as.character(paste(0, waypoint, sep = '')),
                                          as.character(waypoint)))) %>%  
        select(c(sampdate, samptime, sampperiod, site, waypoint, sampyear, vesselname)) %>%
        filter(site != 'Betsey')

# join geometry where start waypoint recorded as being on the original GPS position/waypoint mark
ts.site.start.finish.mark <- ts.site.start.finish %>% 
        filter(is.na(waypoint) & sampperiod == 'start') %>%  
        left_join(., vessel.gps, by = c('site' = 'name', 'sampyear', 'vesselname')) %>% 
        select(c(sampyear, samptime, gpstime, sampperiod, site, waypoint, geometry, vesselname))

# join geometry where start and finish waypoints were recorded 
ts.site.start.finish.wp <- ts.site.start.finish %>% 
        filter(!is.na(waypoint)) %>% 
        left_join(., vessel.gps, by = c('waypoint' = 'name', 'sampdate' = 'gpsdate', 'vesselname')) %>% 
        dplyr::rename('sampyear' = sampyear.x) %>%
        select(c(sampyear, samptime, gpstime, sampperiod, site, waypoint, geometry, vesselname))

# re-join all waypoint data and transform to GDA2020

ts.site.start.finish.loc <- bind_rows(ts.site.start.finish.mark, ts.site.start.finish.wp) %>% 
        st_as_sf() %>% 
        st_transform(GDA2020)

vessel.gps.dat <- ts.site.start.finish.loc

# re-combine metadata frame
ts_meta_dat <- bind_rows(ts.site.start, ts.site.finish) %>%
 mutate(waypoint = if_else(waypoint < 10, as.character(paste('00', waypoint, sep = '')),
                           if_else(between(waypoint, 10, 99), as.character(paste(0, waypoint, sep = '')),
                                   as.character(waypoint)))) %>%  
 select(c(sampdate, samptime, sampperiod, site, waypoint, sampyear, vesselname, max.depth,
          habitat.type, percent.algae.cover, percent.urchins, urchin.deep, comments)) %>%
 filter(site != 'Betsey')

# separate metadata where start waypoint not recorded and join vessel gps data
meta_dat_no_wp <- ts_meta_dat %>% 
 filter(is.na(waypoint) & sampperiod == 'start') %>%  
 left_join(., vessel.gps, by = c('site' = 'name', 'sampyear', 'vesselname')) %>% 
 select(c(sampyear, samptime, gpstime, sampperiod, site, waypoint, geometry, vesselname, max.depth,
          habitat.type, percent.algae.cover, percent.urchins, urchin.deep, comments))

# separate metadata where start waypoint recorded and join vessel data
meta_dat_wp <- ts_meta_dat %>% 
 filter(!is.na(waypoint)) %>% 
 left_join(., vessel.gps, by = c('waypoint' = 'name', 'sampdate' = 'gpsdate', 'vesselname')) %>% 
 dplyr::rename('sampyear' = sampyear.x) %>%
 select(c(sampyear, samptime, gpstime, sampperiod, site, waypoint, geometry, vesselname, max.depth,
          habitat.type, percent.algae.cover, percent.urchins, urchin.deep, comments))

# recombine metadata and transform geometry
meta_dat_gps <- bind_rows(meta_dat_no_wp, meta_dat_wp) %>% 
 st_as_sf() %>% 
 st_transform(GDA2020)

# extract blockno from site name
meta_dat_vessel <- meta_dat_gps %>% 
 mutate(nums = str_count(site, '\\d+')) %>% 
 # mutate(site2 = site) %>% 
 mutate(site2 = ifelse(nums == 2, site, '')) %>% 
 separate(col = site2, into = c('ab', 'blockno'), sep = '-') %>% 
 mutate(site3 = ifelse(nums == 3, site, '')) %>% 
 separate(col = site3, into = c('ab', 'sampyear2', 'blockno2'), sep = '-') %>%
 mutate(blockno = ifelse(blockno %in% c(72, 74, 'LEG', 'THU'), 22, 
                         ifelse(blockno %in% c('BRS'), 13, blockno))) %>% 
 mutate(blockno = ifelse(is.na(blockno), blockno2, blockno)) %>% 
 select(-c(ab, sampyear2, blockno2, nums))

# save files
saveRDS(vessel.gps.dat, paste(samp.year.folder, '/vessel.gps.dat.RDS', sep = ''))

saveRDS(meta_dat_vessel, paste(samp.year.folder, '/meta_dat_vessel.RDS', sep = ''))

# save spatial layer for QGIS
st_write(vessel.gps.dat,
         dsn = paste(samp.year.folder, '/vessel.gps.dat_', Sys.Date(), '.gpkg', sep = ''),
         layer = "vessel.gps.dat", driver = "GPKG", overwrite = T, delete_dsn = T)

##---------------------------------------------------------------------------##
## 7. Proposed site data ####
## identify sites sampled based on proposed GPS site data and
## create map of sites sampled within each Block adding subblock to site name 
## based on proposed position
## use these data for 'proposed geometry' when joining to raw data 

# Combine proposed timed swim sites from all years into one data frame 

# load final proposed sites for 2020 and 2021 surveys (i.e. adjusted site names)
ts.sites.final.pre2022 <- readRDS('C:/CloudStor/DiveFisheries/Abalone/FISdata/FIS_TimedSwimSurveys2021/ts.sites.final.sf.RDS')

# load final proposed sites for 2022
ts.sites.final.2022 <- read.xlsx('C:/CloudStor/DiveFisheries/Abalone/FISdata/FIS_TimedSwimSurveys2022/TimedSwimSites_Final2022.xlsx', 
                                      detectDates = T)

# load final proposed sites for 2023
ts_sites_east_final_2023 <- read.xlsx('C:/CloudStor/DiveFisheries/Abalone/FISdata/FIS_TimedSwimSurveys2023/TimedSwim_EastCoast/TimedSwimSites_EastCoast_Final2023.xlsx', 
                                          detectDates = T) %>% .[,-1]

ts_sites_actaeons_final_2023 <- read.xlsx('C:/CloudStor/DiveFisheries/Abalone/FISdata/FIS_TimedSwimSurveys2023/TimedSwim_Actaeons/TimedSwimSites_SubBlock13DE_Final2023.xlsx', 
                                 detectDates = T) %>% .[,-1] 

ts_sites_greens_final_2023 <- read.xlsx('C:/CloudStor/DiveFisheries/Abalone/FISdata/FIS_TimedSwimSurveys2023/TimedSwim_NEGreens/TimedSwim_NE_GL_2023_50m.xlsx', 
                                          detectDates = T) %>% .[,-1] %>% mutate(oid = as.character(oid))

ts.sites.final.2023 <- bind_rows(ts_sites_east_final_2023, 
                                 ts_sites_actaeons_final_2023, 
                                 ts_sites_greens_final_2023)

# set CRS
GDA2020 <- st_crs(7855)
GDA94 <- st_crs(28355)
WGS84 <- st_crs(4326)

# convert 2022 site data to sf
ts.site.2022.sf <- ts.sites.final.2022 %>% 
 st_as_sf(coords = c("longitude", "latitude"), crs = WGS84) %>%
 select(c(site, geometry)) %>% 
 mutate(sampyear = 2022)

# convert 2023 site data to sf
ts.site.2023.sf <- ts.sites.final.2023 %>% 
 st_as_sf(coords = c("longitude", "latitude"), crs = WGS84) %>%
 select(c(site, geometry)) %>% 
 mutate(sampyear = 2023)

ts.sites.final.pre2022 <- st_transform(ts.sites.final.pre2022, crs = WGS84)

# combine all years
ts.sites.final.sf <- bind_rows(ts.sites.final.pre2022, ts.site.2022.sf, ts.site.2023.sf)

# save final proposed sites up to sample year
saveRDS(ts.sites.final.sf, paste(samp.year.folder, '/ts.sites.final.sf.RDS', sep = ''))

# load timed swim raw dataframe
time.swim.dat <- readRDS(paste(samp.year.folder, '/time.swim.dat.RDS', sep = ''))

# identify unique sites sampled from timed swim dataframe
ts.dat.unique <- time.swim.dat %>%
 distinct(site, sampyear, .keep_all = T) %>% 
 select(c(site, sampyear, sampdate))

# join sites sampled to renamed site file for 2020
ts.sites.join.2020 <- ts.sites.final.sf %>% 
 filter(sampyear == 2020) %>% 
 left_join(., ts.dat.unique, by = c('site.old' = 'site',
                                    'sampyear' = 'sampyear'))

# join sites sampled to renamed site file for 2021
ts.sites.join.2021 <- ts.sites.final.sf %>% 
 filter(sampyear == 2021) %>% 
 left_join(., ts.dat.unique, by = c('site' = 'site',
                                    'sampyear' = 'sampyear'))

# join sites sampled to renamed site file for 2022
ts.sites.join.2022 <- ts.sites.final.sf %>% 
 filter(sampyear == 2022) %>% 
 left_join(., ts.dat.unique, by = c('site' = 'site',
                                    'sampyear' = 'sampyear'))

# join sites sampled to renamed site file for 2023
ts.sites.join.2023 <- ts.sites.final.sf %>% 
 filter(sampyear == 2023) %>% 
 left_join(., ts.dat.unique, by = c('site' = 'site',
                                    'sampyear' = 'sampyear'))

# compile joint site files
ts.dat.site.join <- bind_rows(ts.sites.join.2020, ts.sites.join.2021,
                              ts.sites.join.2022, ts.sites.join.2023)

# identity sites sampled 
ts.dat.site.sampled <- ts.dat.site.join %>% 
 mutate(sampled = ifelse(is.na(sampdate), 0, 1)) %>%  
 select(c(site, site.old, sampled, sampyear, geometry))

# transform to GDA2020
GDA2020 <- st_crs(7855)
ts.dat.site.sampled <- st_transform(ts.dat.site.sampled, GDA2020)

# read in Subblock map as an sf::sfc polygon object
# sf.subblock.map <- st_read("C:/Users/jaimem/Dropbox/AbaloneData/SpatialLayers/SubBlockMaps.gpkg")
sf.subblock.map <- st_read("C:/CloudStor/DiveFisheries/GIS/SpatialLayers/SubBlockMaps.gpkg")

# transform map to GDA2020
sf.subblock.map <- st_transform(sf.subblock.map, GDA2020)

# extract block number from site names
site.samp.loc <- ts.dat.site.sampled %>% 
 mutate(nums = str_count(site, '\\d+')) %>% 
 # mutate(site2 = site) %>% 
 mutate(site2 = ifelse(nums == 2, site, '')) %>% 
 separate(col = site2, into = c('ab', 'blockno'), sep = '-') %>% 
 mutate(site3 = ifelse(nums == 3, site, '')) %>% 
 separate(col = site3, into = c('ab', 'sampyear2', 'blockno2'), sep = '-') %>%
 mutate(blockno = ifelse(blockno %in% c(72, 74, 'LEG', 'THU'), 22, 
                         ifelse(blockno %in% c('BRS'), 13, blockno))) %>% 
 mutate(blockno = ifelse(is.na(blockno), blockno2, blockno)) %>% 
 select(-c(ab, sampyear2, blockno2, nums))

# join data to subblock map to identify subblockno and remove reference sites 
# (i.e. these subblocks were open to fishing)
site.samp.subblock.loc <- st_join(site.samp.loc, sf.subblock.map, join = st_nearest_feature) %>% 
 select(-c(version, area, blockno.y)) %>% 
 dplyr::rename(blockno = blockno.x) %>% 
 rename_all(tolower) %>% 
 mutate(subblockno = ifelse(subblockno == '29A' &
                             sampyear == 2020, '28C', 
                            ifelse(site == 'AB-23-50-A', '23A', subblockno))) %>%
 filter(
  (!subblockno %in% c('28C', '28B'))&
   (!blockno %in% c('13', '14', '29', '30')))

# join data to subblock map to identify subblockno (i.e. inc. all blocks)
site.samp.subblock.loc.ref <- st_join(site.samp.loc, sf.subblock.map, join = st_nearest_feature) %>% 
 select(-c(version, area, blockno.y)) %>% 
 dplyr::rename(blockno = blockno.x) %>% 
 rename_all(tolower) %>% 
 mutate(subblockno = ifelse(subblockno == '29A' &
                             sampyear == 2020, '28C', 
                            ifelse(site == 'AB-23-50-A', '23A', subblockno)))

# save file of actual geometry for raw data join
ts.proposed.geom <- site.samp.subblock.loc.ref

saveRDS(ts.proposed.geom, paste(samp.year.folder, '/ts.proposed.geom.RDS', sep = ''))

# create maps for each block (#note: sample year and/or reference sites)

# select sampling year
samp.year <- 2023

# identify blocks sampled
blocks.sampled <- site.samp.loc %>% 
 filter(sampyear == samp.year) %>% 
 distinct(blockno) %>% 
 pull()

# create maps
for (i in blocks.sampled){
 
 site.samp.loc.blockno <- site.samp.subblock.loc.ref %>% 
  filter(blockno == i &
          sampyear == samp.year) %>%  
  mutate(sampled = factor(sampled, levels = c('0', '1')))
 
 sub_blocks <- site.samp.loc.blockno %>% distinct(subblockno) %>% pull()
  
 sf.subblock.map.crop.1 <- sf.subblock.map %>%
  filter(blockno == i &
          !subblockno %in% c('28B', '28C'))
 
 sf.subblock.map.crop.2 <- sf.subblock.map %>%
  {if(i == '27') filter(., blockno == '26') else filter(., blockno == i)}
 
 sf.subblock.map.crop.3 <- sf.subblock.map %>%
  {if(i == '16') filter(., blockno %in% c('14', '15')) else filter(., blockno == i)}
 
 sf.subblock.map.crop.4 <- sf.subblock.map %>%
  {if(i == '13') filter(., subblockno %in% c('13D', '13E')) else filter(., blockno == i)}
 
 
 sf.subblock.map.crop <- bind_rows(sf.subblock.map.crop.1, sf.subblock.map.crop.2, 
                                   sf.subblock.map.crop.3, sf.subblock.map.crop.4) %>% 
  filter(subblockno %in% sub_blocks)
 
 site.samp.map.blockno <- ggplot(data = st_geometry(sf.subblock.map.crop)) +
  geom_sf(fill = NA) +
  geom_sf_text(data = sf.subblock.map.crop, aes(label = subblockno))+
  geom_sf(data = site.samp.loc.blockno, aes(colour = sampled)) +
  scale_colour_manual(values = c('red', 'blue'))+
  theme_bw() +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.05, "cm"), pad_y = unit(0.1, "cm"),
                         style = north_arrow_fancy_orienteering)+
  theme(legend.position="none")+
  xlab('Longitude')+
  ylab('Latitude')

 # save plots
 setwd(ts.plots.folder)
 ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_SitesSampled_Proposed_BlockNo_', i, '.pdf', sep = ''),
        plot = site.samp.map.blockno, units = 'mm', width = 190, height = 250)
 ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_SitesSampled_Proposed_BlockNo_', i, '.png', sep = ''),
        plot = site.samp.map.blockno, units = 'mm', width = 190, height = 150)
 
}

# save spatial layer for QGIS
st_write(ts.proposed.geom, 
         dsn = paste(samp.year.folder, '/ts.proposed.geom_', Sys.Date(), '.gpkg', sep = ''),
         layer = "ts.proposed.geom", driver = "GPKG", overwrite = T, delete_dsn = T)

##---------------------------------------------------------------------------##
## 8. Actual site data ####
## Create dataframe of actual sites sampled and generate map of sites sampled within 
## each Block with recorded GPS positions and 
## add subblock to site name based on vessel GPS data and start position
## use these data for 'actual geometry' when joining to raw data 

# load compiled vessel GPS data
vessel.gps.dat <- readRDS(paste(samp.year.folder, '/vessel.gps.dat.RDS', sep = ''))

# read in Subblock map as an sf::sfc polygon object
sf.subblock.map <- st_read("C:/CloudStor/DiveFisheries/GIS/SpatialLayers/SubBlockMaps.gpkg")

# transform map to GDA2020
sf.subblock.map <- st_transform(sf.subblock.map, GDA2020)

# extract block number from site names
site.samp.start.loc <- vessel.gps.dat %>% 
        mutate(nums = str_count(site, '\\d+')) %>%
        mutate(site2 = ifelse(nums == 2, site, '')) %>%
        separate(col = site2, into = c('ab', 'blockno'), sep = '-') %>%
        mutate(site3 = ifelse(nums == 3, site, '')) %>%
        separate(col = site3, into = c('ab', 'sampyear2', 'blockno2'), sep = '-', remove = F) %>%
        mutate(blockno = ifelse(blockno %in% c(72, 74, 'LEG', 'THU'), 22, 
                                ifelse(blockno %in% c('BRS'), 13, blockno))) %>% 
        mutate(blockno = ifelse(is.na(blockno), blockno2, blockno)) %>%
        select(-c(ab, sampyear2, blockno2, nums))

# join data to subblock map to identify subblockno and remove any blocks/sites 
# (i.e. these subblocks were open to fishing) and select start position
site.samp.start.subblock.loc <- st_join(site.samp.start.loc, sf.subblock.map, join = st_nearest_feature) %>% 
        select(-c(version, area, blockno.y, site3)) %>% 
        dplyr::rename(blockno = blockno.x) %>% 
        rename_all(tolower) %>% 
        filter(
                (!subblockno %in% c('28C', '28B') & sampperiod == 'start'))
                        # (!blockno %in% c('13', '14', '29', '30') & sampperiod == 'start'))

# join data to subblock map to identify subblockno and remove any blocks/sites 
# (i.e. these subblocks were open to fishing) and select finish position
site.samp.finish.subblock.loc <- st_join(site.samp.start.loc, sf.subblock.map, join = st_nearest_feature) %>% 
 select(-c(version, area, blockno.y, site3)) %>% 
 dplyr::rename(blockno = blockno.x,
               finish.geom = geometry) %>% 
 rename_all(tolower) %>% 
 filter(
  (!subblockno %in% c('28C', '28B') & sampperiod == 'finish'))
   # (!blockno %in% c('13', '14', '29', '30') & sampperiod == 'finish'))

# re-combine start and finish data
site.samp.start.finish <- left_join(as.data.frame(site.samp.start.subblock.loc), 
                as.data.frame(site.samp.finish.subblock.loc),
                by = c("sampyear", "site", 
                       "vesselname", "blockno", "zone",
                       "subblockno"))

# calculate straighline distance between start and finish position 
site.samp.distance <- site.samp.start.finish %>%
 mutate(distance = as.numeric(units::drop_units(st_distance(geometry, finish.geom, by_element = T)))) %>% 
 dplyr::rename(samptime = samptime.x,
               gpstime = gpstime.x,
               sampperiod = sampperiod.x,
               waypoint = waypoint.x) %>% 
 select(-c(samptime.y, gpstime.y, sampperiod.y, waypoint.y, finish.geom)) %>% 
 st_as_sf()

# join data to subblock map to identify subblockno (i.e. inc. all blocks)
site.samp.start.subblock.loc.ref <- st_join(site.samp.distance, sf.subblock.map, join = st_nearest_feature) %>% 
 select(-c(version, area, blockno.y, subblockno.y, zone.y)) %>% 
 dplyr::rename(blockno = blockno.x,
               subblockno = subblockno.x,
               zone = zone.x) %>% 
 rename_all(tolower) %>%  
 filter(sampperiod == 'start')

# load proposed site file
ts.sites.final.sf <- readRDS(paste(samp.year.folder, '/ts.sites.final.sf.RDS', sep = ''))

# join sites sampled to renamed site file for 2020
ts.actual.geom <- ts.sites.final.sf %>%
 st_drop_geometry() %>% 
 filter(sampyear == 2020) %>% 
 left_join(site.samp.start.subblock.loc.ref, ., by = c('site' = 'site.old',
                           'sampyear' = 'sampyear')) %>% 
 mutate(site.old = ifelse(sampyear == 2020, site, ''),
        site = ifelse(sampyear == 2020, site.y, site),
        site = ifelse(sampyear == 2020 & is.na(site), site.old, site)) %>%  
 select(-site.y)

# save file of actual geometry for raw data join
saveRDS(ts.actual.geom, paste(samp.year.folder, '/ts.actual.geom.RDS', sep = ''))

# create maps for each block (#note: sample year and/or reference sites)

# identify blocks sampled
blocks.sampled <- site.samp.start.subblock.loc.ref %>% 
 filter(sampyear == samp.year) %>% 
 distinct(blockno) %>% 
 pull()

# create maps
for (i in blocks.sampled){
        
        site.samp.loc.blockno <- site.samp.start.subblock.loc.ref %>% 
                filter(blockno == i &
                               sampyear == samp.year)
        
        sub_blocks <- site.samp.loc.blockno %>% distinct(subblockno) %>% pull()
        
        sf.subblock.map.crop.1 <- sf.subblock.map %>%
                       filter(blockno == i &
                               !subblockno %in% c('28B', '28C'))

        sf.subblock.map.crop.2 <- sf.subblock.map %>%
                {if(i == '27') filter(., blockno == '26') else filter(., blockno == i)}
        
        sf.subblock.map.crop.3 <- sf.subblock.map %>%
                {if(i == '16') filter(., blockno %in% c('14', '15')) else filter(., blockno == i)}
        
        sf.subblock.map.crop.4 <- sf.subblock.map %>%
         {if(i == '13') filter(., subblockno %in% c('13D', '13E')) else filter(., blockno == i)}
        
        
        sf.subblock.map.crop <- bind_rows(sf.subblock.map.crop.1, sf.subblock.map.crop.2, 
                                          sf.subblock.map.crop.3, sf.subblock.map.crop.4) %>% 
         filter(subblockno %in% sub_blocks)
        
        site.samp.map.blockno <- ggplot(data = st_geometry(sf.subblock.map.crop)) +
                geom_sf(fill = NA) +
                geom_sf_text(data = sf.subblock.map.crop, aes(label = subblockno))+
                geom_sf(data = site.samp.loc.blockno, colour = 'blue') +
                theme_bw() +
                annotation_scale(location = "bl", width_hint = 0.5) +
                annotation_north_arrow(location = "br", which_north = "true",
                                       pad_x = unit(0.05, "cm"), pad_y = unit(0.1, "cm"),
                                       style = north_arrow_fancy_orienteering)+
                theme(legend.position="none")+
                xlab('Longitude')+
                ylab('Latitude')
   
        
        setwd(ts.plots.folder)
        ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_SitesSampled_GPS_BlockNo_', i, '.pdf', sep = ''),
               plot = site.samp.map.blockno, units = 'mm', width = 190, height = 250)
        ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_SitesSampled_GPS_BlockNo_', i, '.png', sep = ''),
               plot = site.samp.map.blockno, units = 'mm', width = 190, height = 150)
        
}

# save spatial layer for QGIS
st_write(ts.actual.geom, 
         dsn = paste(samp.year.folder, '/ts.actual.geom_', Sys.Date(), '.gpkg', sep = ''),
         layer = "ts.actual.geom", driver = "GPKG", overwrite = T, delete_dsn = T)

##---------------------------------------------------------------------------##
# # Extract block 13 and 14 reference site coordinates
# df.1 <- site.samp.start.loc %>% 
#   filter(blockno %in% c(13, 14) &
#            sampperiod == 'start')
# 
# WGS84 <- st_crs(4326)
# 
# df.2 <- st_transform(df.1, WGS84) %>% 
#   st_coordinates()
# 
# st_write(df.1,
#          dsn = "C:/CloudStor/Shared/DiveFisheries/Abalone/FISdata/FIS_TimedSwimSurveys2021/block13-14_refsites.gpkg",
#          layer = "df.1", driver = "GPKG", overwrite = T, delete_dsn = T)

##---------------------------------------------------------------------------##
## 9. Final data frames ####
## Join spatial site data (proposed and actual GPS sites) to raw count data and save final dataframe

# Load raw count, proposed and actual site data frames
time.swim.dat <- readRDS(paste(samp.year.folder, '/time.swim.dat.RDS', sep = ''))
time.swim.dat.df <- readRDS(paste(samp.year.folder, '/time.swim.dat.df.RDS', sep = ''))
ts.actual.geom <- readRDS(paste(samp.year.folder, '/ts.actual.geom.RDS', sep = ''))
ts.proposed.geom <- readRDS(paste(samp.year.folder, '/ts.proposed.geom.RDS', sep = ''))

# add unique join ID
time.swim.dat <- time.swim.dat %>% 
        mutate(join_id = row_number())

# add sampdate to actual.geom file
ts.actual.geom <- ts.actual.geom %>% 
  mutate(sampdate = date(samptime))

# join proposed site data to size frequency data
        ts.dat.prop.2020 <- time.swim.dat %>% 
                filter(sampyear == 2020) %>% 
                left_join(., ts.proposed.geom, by = c('site' = 'site.old',
                                                   'sampyear' = 'sampyear')) %>%    
                mutate(site = ifelse(is.na(site.y), site, site.y)) 
        
        ts.dat.prop.2021 <- time.swim.dat %>% 
                filter(sampyear == 2021) %>% 
                left_join(., ts.proposed.geom, by = c('site' = 'site',
                                                      'sampyear' = 'sampyear'))
        
        ts.dat.prop.2022 <- time.swim.dat %>% 
         filter(sampyear == 2022) %>% 
         left_join(., ts.proposed.geom, by = c('site' = 'site',
                                               'sampyear' = 'sampyear'))
        
        ts.dat.prop.2023 <- time.swim.dat %>% 
         filter(sampyear == 2023) %>% 
         left_join(., ts.proposed.geom, by = c('site' = 'site',
                                               'sampyear' = 'sampyear'))
        
        ts.dat.prop <- bind_rows(ts.dat.prop.2020, ts.dat.prop.2021, ts.dat.prop.2022, ts.dat.prop.2023) %>%   
                select(-c(blockno.y, site.y, site.old)) %>% 
                dplyr::rename(proposed.geom = 'geometry',
                              blockno = 'blockno.x') 
        
# join actual site data to size frequency data
        ts.dat.act.2020 <- time.swim.dat %>% 
                filter(sampyear == 2020) %>%  
                left_join(., ts.actual.geom, by = c('site' = 'site.old',
                                                      'sampyear' = 'sampyear',
                                                    'sampdate')) %>%    
                mutate(site = ifelse(is.na(site.y), site, site.y)) %>% 
                select(-site.y)
        
        ts.dat.act.2021 <- time.swim.dat %>% 
                filter(sampyear == 2021) %>% 
                left_join(., ts.actual.geom, by = c('site' = 'site',
                                                      'sampyear' = 'sampyear',
                                                    'sampdate'))
        
        ts.dat.act.2022 <- time.swim.dat %>% 
         filter(sampyear == 2022) %>% 
         left_join(., ts.actual.geom, by = c('site' = 'site',
                                             'sampyear' = 'sampyear',
                                             'sampdate'))
        
        ts.dat.act.2023 <- time.swim.dat %>% 
         filter(sampyear == 2023) %>% 
         left_join(., ts.actual.geom, by = c('site' = 'site',
                                             'sampyear' = 'sampyear',
                                             'sampdate'))
        
        ts.dat.act <- bind_rows(ts.dat.act.2020, ts.dat.act.2021, ts.dat.act.2022, ts.dat.act.2023) %>%    
                select(-c(blockno.y, site.old)) %>% 
                dplyr::rename(actual.geom = 'geometry',
                              blockno = 'blockno.x')         
        
time.swim.dat.act.prop <- left_join(ts.dat.prop %>% dplyr::select(-c(subblockno, zone)), ts.dat.act) %>%  
        select(c(site, sampdate, sampyear, blockno, subblockno, diver, starttime, 
                 firstabtime, finishtime, time.elapsed, sizeclass, sizeclass.2021, sizeclass_freq, 
                 minsize, midsize, midsize.2021, maxsize, legal.size, actual.geom, proposed.geom, distance))

# join proposed site data to individual length data

# add unique join ID
time.swim.dat.df <- time.swim.dat.df %>%
        mutate(join_id = row_number())

ts.dat.prop.df.2020 <- time.swim.dat.df %>%
        filter(sampyear == 2020) %>%
        left_join(., ts.proposed.geom, by = c('site' = 'site.old',
                                              'sampyear' = 'sampyear')) %>%
        mutate(site = ifelse(is.na(site.y), site, site.y))

ts.dat.prop.df.2021 <- time.swim.dat.df %>%
        filter(sampyear == 2021) %>%
        left_join(., ts.proposed.geom, by = c('site' = 'site',
                                              'sampyear' = 'sampyear'))

ts.dat.prop.df.2022 <- time.swim.dat.df %>%
 filter(sampyear == 2022) %>%
 left_join(., ts.proposed.geom, by = c('site' = 'site',
                                       'sampyear' = 'sampyear'))

ts.dat.prop.df.2023 <- time.swim.dat.df %>%
 filter(sampyear == 2023) %>%
 left_join(., ts.proposed.geom, by = c('site' = 'site',
                                       'sampyear' = 'sampyear'))

ts.dat.prop.df <- bind_rows(ts.dat.prop.df.2020, ts.dat.prop.df.2021, ts.dat.prop.df.2022, ts.dat.prop.df.2023) %>%
        select(-c(blockno.y, site.y, site.old)) %>%
        dplyr::rename(proposed.geom = 'geometry',
                      blockno = 'blockno.x') %>%
        select(c(-sampled))

# join actual site data to individual length data
ts.dat.act.df.2020 <- time.swim.dat.df %>%
        filter(sampyear == 2020) %>%
        left_join(., ts.actual.geom, by = c('site' = 'site.old',
                                            'sampyear' = 'sampyear',
                                            'sampdate')) %>%
        mutate(site = ifelse(is.na(site.y), site, site.y)) %>%
        select(-site.y)

ts.dat.act.df.2021 <- time.swim.dat.df %>%
        filter(sampyear == 2021) %>%
        left_join(., ts.actual.geom, by = c('site' = 'site',
                                            'sampyear' = 'sampyear',
                                            'sampdate'))

ts.dat.act.df.2022 <- time.swim.dat.df %>%
 filter(sampyear == 2022) %>%
 left_join(., ts.actual.geom, by = c('site' = 'site',
                                     'sampyear' = 'sampyear',
                                     'sampdate'))

ts.dat.act.df.2023 <- time.swim.dat.df %>%
 filter(sampyear == 2023) %>%
 left_join(., ts.actual.geom, by = c('site' = 'site',
                                     'sampyear' = 'sampyear',
                                     'sampdate'))

ts.dat.act.df <- bind_rows(ts.dat.act.df.2020, ts.dat.act.df.2021, ts.dat.act.df.2022, ts.dat.act.df.2023) %>%
        select(-c(blockno.y, site.old)) %>%
        dplyr::rename(actual.geom = 'geometry',
                      blockno = 'blockno.x') %>%
        select(-c(gpstime, sampperiod, samptime, waypoint))

time.swim.dat.df.act.prop <- left_join(ts.dat.prop.df %>% dplyr::select(-c(subblockno, zone)), ts.dat.act.df) %>%  
        select(c(site, sampdate, sampyear, blockno, subblockno, diver, starttime,
                 firstabtime, finishtime, time.elapsed, shelllength, shelllength.2021, sizeclass, 
                 sizeclass.2021, sizeclass_freq, minsize, maxsize, legal.size, 
                 actual.geom, proposed.geom, distance))

# join time swim size frequency and individual length data to original site data to 
# include 'oid', 'cell.ntile' and 'sam.count'

# load cpue oid data and combine
ts.site.cpue.2020.join <- readRDS('C:/CloudStor/DiveFisheries/Abalone/FISdata/FIS_TimedSwimSurveys2021/ts.site.cpue.2020.join.RDS')
ts.site.cpue.2021.join <- readRDS('C:/CloudStor/DiveFisheries/Abalone/FISdata/FIS_TimedSwimSurveys2021/ts.site.cpue.2021.join.RDS')
ts.site.cpue.join <- bind_rows(ts.site.cpue.2020.join, ts.site.cpue.2021.join)

# load sam data and remove duplicate sites with same site name
ts.site.sam.dat <- readRDS('C:/CloudStor/DiveFisheries/Abalone/FISdata/FIS_TimedSwimSurveys2021/ts.site.sam.2020.join.RDS')
ts.site.sam.join <- ts.site.sam.dat %>% 
        distinct(site, .keep_all = T)

# join oid and sam data to final size frequency data
time.swim.dat.final <- left_join(time.swim.dat.act.prop, select(ts.site.cpue.join, site, site.old, sampyear, oid, cell.ntile, cpue.kg.hr), by = c('site', 'sampyear')) %>%
        left_join(., select(ts.site.sam.join, site, sampyear, site.old, sam.count), by = c('site' = 'site')) %>%
    dplyr::rename(sampyear = 'sampyear.x') %>% 
        select(-c(site.old.x, geometry.x, sampyear.y, site.old.y, geometry.y))

# join oid and sam data to final individual size data
time.swim.dat.df.final <- left_join(time.swim.dat.df.act.prop, select(ts.site.cpue.join, site, site.old, sampyear, oid, cell.ntile), by = c('site', 'sampyear')) %>%
    left_join(., select(ts.site.sam.join, site, sampyear, site.old, sam.count), by = c('site' = 'site')) %>% 
    dplyr::rename(sampyear = 'sampyear.x') %>% 
    select(-c(site.old.x, geometry.x, sampyear.y, site.old.y, geometry.y))

# join vessel GPS data to meta-data frame

# Import metadata frame
time.swim.meta.dat <- readRDS(paste(samp.year.folder, '/time.swim.meta.dat.RDS', sep = ''))

# Import vessel data frame
vessel.gps.dat <- readRDS(paste(samp.year.folder, '/vessel.gps.dat.RDS', sep = ''))

# remove leading zeros from waypoint numbers
vessel_gps_dat_zero <- vessel.gps.dat %>% 
 mutate(waypoint = as.numeric(gsub('^0+', '', waypoint)))

# rename variables in metadata 
ts_meta_dat_rename <- time.swim.meta.dat %>% 
 gather(., sampperiod, waypoint, waypoint.start:waypoint.finish) %>% 
 mutate(sampperiod = gsub('waypoint.', '', sampperiod),
        sampyear = year(sampdate)) %>% 
 select(-c(sampdate))

# join meta and vessel data
ts_meta_dat_join <- ts_meta_dat_rename %>% 
 left_join(., vessel_gps_dat_zero)

# separate meta data start position and label
df_start <- ts_meta_dat_join %>% 
 filter(sampperiod == 'start') %>% 
 dplyr::rename(start_geom = 'geometry',
               start_gps_time = 'gpstime',
               start_waypoint = 'waypoint') %>% 
 select(-c(sampperiod, finishtime))

# separate meta data finish position and label
df_finish <- ts_meta_dat_join %>% 
 filter(sampperiod == 'finish') %>% 
 dplyr::rename(finish_geom = 'geometry',
               finish_gps_time = 'gpstime',
               finish_waypoint = 'waypoint') %>% 
 select(-c(sampperiod, starttime, samptime))

# rejoin start and finish data with labels
df_start_finish <- left_join(df_start, df_finish) %>% 
 select(c(site, divers, starttime, finishtime, max.depth, habitat.type, percent.algae.cover,
          percent.urchins, urchin.deep, comments, vesselname, start_waypoint, finish_waypoint,
          start_gps_time, finish_gps_time, start_geom, finish_geom))

# extract block number from site names
time.swim.meta.dat.final <- df_start_finish %>% 
 mutate(nums = str_count(site, '\\d+')) %>% 
 # mutate(site2 = site) %>% 
 mutate(site2 = ifelse(nums == 2, site, '')) %>% 
 separate(col = site2, into = c('ab', 'blockno'), sep = '-') %>% 
 mutate(site3 = ifelse(nums == 3, site, '')) %>% 
 separate(col = site3, into = c('ab', 'sampyear2', 'blockno2'), sep = '-') %>%
 mutate(blockno = ifelse(blockno %in% c(72, 74, 'LEG', 'THU'), 22, 
                         ifelse(blockno %in% c('BRS'), 13, blockno))) %>% 
 mutate(blockno = ifelse(is.na(blockno), blockno2, blockno)) %>% 
 select(-c(ab, sampyear2, blockno2, nums)) %>% 
 select(c(site, blockno, divers, starttime, finishtime, max.depth, habitat.type, percent.algae.cover,
          percent.urchins, urchin.deep, comments, vesselname, start_waypoint, finish_waypoint,
          start_gps_time, finish_gps_time, start_geom, finish_geom))

# save dataframes
saveRDS(time.swim.dat.final, paste(samp.year.folder, '/time.swim.dat.final.RDS', sep = ''))
saveRDS(time.swim.dat.df.final, paste(samp.year.folder, '/time.swim.dat.df.final.RDS', sep = ''))
saveRDS(time.swim.meta.dat.final, paste(samp.year.folder, '/time.swim.meta.dat.final.RDS', sep = ''))

##---------------------------------------------------------------------------##
rm(list = setdiff(ls(), c('time.swim.dat.final',
                          'time.swim.dat.df.final',
                          'time.swim.meta.dat.final',
                          'samp.year', 
                          'samp.year.folder', 
                          'ts.plots.folder')))

