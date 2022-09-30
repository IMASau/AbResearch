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

##---------------------------------------------------------------------------##
## 1. Set sample year and file paths ####

# identify sampling year of interest
samp.year <- 2022

# identify associated sampling year folder path to save dataframes
samp.year.folder <- file.path('C:', 'CloudStor', 'Shared', 'DiveFisheries', 
                              'Abalone', 'FISdata',
                              paste('FIS_TimedSwimSurveys', samp.year, sep = ''))

# identify associated sampling year folder path to save plots
ts.plots.folder <- file.path('C:', 'CloudStor', 'Shared', 'DiveFisheries', 
                             'Abalone', 'Assessment', 'Figures', 'FIS',
                             paste('FIS_TimedSwimSurvey', samp.year, '_Plots', sep = ''))
##---------------------------------------------------------------------------##
## 2. Load raw data ####

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
## 3. Raw data conversions ####

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
        select(-c(ab, sampyear2, blockno2, nums))

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
## 4. Data check #### 
#quick check that sites match between raw and meta data

# create summary data frames of sites sampled by date
ts.dat.sites <- time.swim.dat %>% 
        select(sampdate, site) %>% 
        distinct_all()

ts.meta.dat.sites <- time.swim.meta.dat %>% 
        select(sampdate, site) %>% 
        distinct_all()

# compare data frames to see if the site-date combinations match
summary(arsenal::comparedf(ts.dat.sites, ts.meta.dat.sites, by = c('sampdate', 'site')))

##---------------------------------------------------------------------------##
## 5. Vessel GPS data ####
## load data from vessel GPS downloads and match to raw data

# define CRS
GDA2020 <- st_crs(7855)

# read GPX files from vessel plotter

# vessel data for 2020 surveys
morana.gps.2020 <- st_read('C:/CloudStor/Shared/DiveFisheries/Abalone/FISdata/FIS_TimedSwimSurveys2021/MORANAII-2020-10-09_download.gpx', layer = 'waypoints')

# vessel data for 2021 reference site surveys (i.e. block 13-14)
morana.gps.ref <- st_read('C:/CloudStor/Shared/DiveFisheries/Abalone/FISdata/FIS_TimedSwimSurveys2021/MORANAII-2021-04-06_download.gpx', layer = 'waypoints')

# vessel data for 2021 surveys
morana.gps.2021.a <- st_read('C:/CloudStor/Shared/DiveFisheries/Abalone/FISdata/FIS_TimedSwimSurveys2021/MORANAII-2021-07-15_download.gpx', layer = 'waypoints')

morana.gps.2021.b <- st_read('C:/CloudStor/Shared/DiveFisheries/Abalone/FISdata/FIS_TimedSwimSurveys2021/MORANAII-2021-08-12_download.gpx', layer = 'waypoints')

morana.gps.2021.c <- st_read('C:/CloudStor/Shared/DiveFisheries/Abalone/FISdata/FIS_TimedSwimSurveys2021/MORANAII-2021-09-08_download.gpx', layer = 'waypoints')

morana.gps.2021.d <- st_read('C:/CloudStor/Shared/DiveFisheries/Abalone/FISdata/FIS_TimedSwimSurveys2021/MORANAII-2021-10-07_download.gpx', layer = 'waypoints')

taroona.gps.2021.a <- st_read('C:/CloudStor/Shared/DiveFisheries/Abalone/FISdata/FIS_TimedSwimSurveys2021/TAROONA-2021-10-15_download.gpx', layer = 'waypoints')

morana.gps.2021 <- bind_rows(morana.gps.2021.a, morana.gps.2021.b, morana.gps.2021.c, 
                             morana.gps.2021.d) %>% 
 distinct(., name, .keep_all = T)

# vessel data for 2022 surveys
morana.gps.2022.a <- st_read('C:/CloudStor/Shared/DiveFisheries/Abalone/FISdata/FIS_TimedSwimSurveys2022/MORANAII-2022-08-10_download.gpx', layer = 'waypoints')

morana.gps.2022.b <- st_read('C:/CloudStor/Shared/DiveFisheries/Abalone/FISdata/FIS_TimedSwimSurveys2022/MORANAII-2022-09-06_download.gpx', layer = 'waypoints')

morana.gps.2022.c <- st_read('C:/CloudStor/Shared/DiveFisheries/Abalone/FISdata/FIS_TimedSwimSurveys2022/MORANAII-2022-09-06_download.gpx', layer = 'waypoints')

morana.gps.2022 <- bind_rows(morana.gps.2022.a, morana.gps.2022.b, morana.gps.2022.c) %>% 
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

# join GPX files
vessel.gps <- bind_rows(morana.gps.2020, morana.gps.ref, morana.gps.2021, taroona.gps.2021,
                        morana.gps.2022) %>%  
        mutate(gpsdate = as.Date(time),
               gpstime = time) %>% 
        select(c(name, sampyear, gpstime, gpsdate, geometry, vesselname))

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
        # dplyr::rename('gpstime' = time,
        #               'site' = site) %>% 
        select(c(sampyear, samptime, gpstime, sampperiod, site, waypoint, geometry, vesselname))

# re-join all waypoint data and transform to GDA2020

ts.site.start.finish.loc <- bind_rows(ts.site.start.finish.mark, ts.site.start.finish.wp) %>% 
        st_as_sf() %>% 
        st_transform(GDA2020)

vessel.gps.dat <- ts.site.start.finish.loc

# save files
saveRDS(vessel.gps.dat, paste(samp.year.folder, '/vessel.gps.dat.RDS', sep = ''))

# save spatial layer for QGIS
st_write(vessel.gps.dat,
         dsn = paste(samp.year.folder, '/vessel.gps.dat_', Sys.Date(), '.gpkg', sep = ''),
         layer = "vessel.gps.dat", driver = "GPKG", overwrite = T, delete_dsn = T)

##---------------------------------------------------------------------------##
## 6. Sites sampled - proposed ####
## identify sites sampled based on proposed GPS site data and
## create map of sites sampled within each Block adding subblock to site name 
## based on proposed position
## use these data for 'proposed geometry' when joining to raw data 

# Combine proposed timed swim sites from all years into one data frame 

# load final proposed sites for 2020 and 2021 surveys (i.e. adjusted site names)
ts.sites.final.pre2022 <- readRDS('C:/CloudStor/Shared/DiveFisheries/Abalone/FISdata/FIS_TimedSwimSurveys2021/ts.sites.final.sf.RDS')

# load final proposed sites for 2022
ts.sites.final.2022 <- read.xlsx('C:/CloudStor/Shared/DiveFisheries/Abalone/FISdata/FIS_TimedSwimSurveys2022/TimedSwimSites_Final2022.xlsx', 
                                      detectDates = T)
# set CRS
GDA2020 <- st_crs(7855)

# convert 2022 site data to sf
ts.site.2022.sf <- ts.sites.final.2022 %>% 
 st_as_sf(coords = c("longitude", "latitude"), crs = GDA2020) %>%
 select(c(site, geometry)) %>% 
 mutate(sampyear = 2022)

# combine 2022 sites with 2020/21 sites
ts.sites.final.sf <- bind_rows(ts.sites.final.pre2022, ts.site.2022.sf)

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

# compile joint site files
ts.dat.site.join <- bind_rows(ts.sites.join.2020, ts.sites.join.2021,
                              ts.sites.join.2022)

# identity sites sampled 
ts.dat.site.sampled <- ts.dat.site.join %>% 
 mutate(sampled = ifelse(is.na(sampdate), 0, 1)) %>%  
 select(c(site, site.old, sampled, sampyear, geometry))

# transform to GDA2020
GDA2020 <- st_crs(7855)
ts.dat.site.sampled <- st_transform(ts.dat.site.sampled, GDA2020)

# read in Subblock map as an sf::sfc polygon object
# sf.subblock.map <- st_read("C:/Users/jaimem/Dropbox/AbaloneData/SpatialLayers/SubBlockMaps.gpkg")
sf.subblock.map <- st_read("C:/CloudStor/Shared/DiveFisheries/GIS/SpatialLayers/SubBlockMaps.gpkg")

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

# # remove no proposed sites (i.e. additional sites surveyed based on diver recommendations or 
# # were FIS sites)
# site.samp.subblock.loc.ref <- site.samp.subblock.loc.ref %>% 
#     filter(!site %in% c('AB-THU-LEG-1-0',
#                         'AB-THU-LEG-2-0',
#                         'AB-BRS-LEG-1-0',
#                         'AB-BRS-LEG-2-0',
#                         'WP-74-75',
#                         'WP-72-73',
#                         'AB-27-SL-1',
#                         'AB-27-NW-1'))

# save file of actual geometry for raw data join
ts.proposed.geom <- site.samp.subblock.loc.ref

saveRDS(ts.proposed.geom, paste(samp.year.folder, '/ts.proposed.geom.RDS', sep = ''))

# create maps for each block (#note: sample year and/or reference sites)

# identify blocks sampled
blocks.sampled <- unique(site.samp.loc$blockno)

# select sampling year
samp.year <- 2022

# create maps
for (i in blocks.sampled){
 
 site.samp.loc.blockno <- site.samp.subblock.loc.ref %>% 
  filter(blockno == i &
          sampyear == samp.year) %>%  
  mutate(sampled = factor(sampled, levels = c('0', '1')))
 
 sf.subblock.map.crop.1 <- sf.subblock.map %>%
  filter(blockno == i &
          !subblockno %in% c('28B', '28C'))
 
 sf.subblock.map.crop.2 <- sf.subblock.map %>%
  {if(i == '27') filter(., blockno == '26') else filter(., blockno == i)}
 
 sf.subblock.map.crop.3 <- sf.subblock.map %>%
  {if(i == '16') filter(., blockno %in% c('14', '15')) else filter(., blockno == i)}
 
 
 sf.subblock.map.crop <- bind_rows(sf.subblock.map.crop.1, sf.subblock.map.crop.2, sf.subblock.map.crop.3)
 
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
## 7. Sites sampled - actual ####
## create map of sites sampled within each Block with recorded GPS positions and 
## add subblock to site name based on vessel GPS data and start position
## use these data for 'actual geometry' when joining to raw data 

vessel.gps.dat <- readRDS('C:/CloudStor/Shared/DiveFisheries/Abalone/FISdata/FIS_TimedSwimSurveys2022/vessel.gps.dat.RDS')

# read in Subblock map as an sf::sfc polygon object
sf.subblock.map <- st_read("C:/CloudStor/Shared/DiveFisheries/GIS/SpatialLayers/SubBlockMaps.gpkg")

# transform map to GDA2020
sf.subblock.map <- st_transform(sf.subblock.map, GDA2020)

# extract block number from site names
site.samp.start.loc <- vessel.gps.dat %>% 
        mutate(nums = str_count(site, '\\d+')) %>%
        # mutate(site2 = site) %>%
        mutate(site2 = ifelse(nums == 2, site, '')) %>%
        separate(col = site2, into = c('ab', 'blockno'), sep = '-') %>%
        mutate(site3 = ifelse(nums == 3, site, '')) %>%
        separate(col = site3, into = c('ab', 'sampyear2', 'blockno2'), sep = '-', remove = F) %>%
        mutate(blockno = ifelse(blockno %in% c(72, 74, 'LEG', 'THU'), 22, 
                                ifelse(blockno %in% c('BRS'), 13, blockno))) %>% 
        mutate(blockno = ifelse(is.na(blockno), blockno2, blockno)) %>%
        select(-c(ab, sampyear2, blockno2, nums))

# join data to subblock map to identify subblockno and remove reference sites 
# (i.e. these subblocks were open to fishing)
site.samp.start.subblock.loc <- st_join(site.samp.start.loc, sf.subblock.map, join = st_nearest_feature) %>% 
        select(-c(version, area, blockno.y, site3)) %>% 
        dplyr::rename(blockno = blockno.x) %>% 
        rename_all(tolower) %>% 
        filter(
                (!subblockno %in% c('28C', '28B') & sampperiod == 'start') &
                        (!blockno %in% c('13', '14', '29', '30') & sampperiod == 'start'))

# join data to subblock map to identify subblockno (i.e. inc. all blocks)
site.samp.start.subblock.loc.ref <- st_join(site.samp.start.loc, sf.subblock.map, join = st_nearest_feature) %>% 
        select(-c(version, area, blockno.y, site3)) %>% 
        dplyr::rename(blockno = blockno.x) %>% 
        rename_all(tolower) %>% 
        filter(sampperiod == 'start')

# load proposed site file
ts.sites.final.sf <- readRDS('C:/CloudStor/Shared/DiveFisheries/Abalone/FISdata/FIS_TimedSwimSurveys2022/ts.sites.final.sf.RDS')

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
blocks.sampled <- unique(site.samp.start.subblock.loc.ref$blockno)

# # select sampling year
# samp.year <- 2022
# 
# # identify folder path to save plots for sampling year
# ts.plots.folder <- file.path('C:', 'CloudStor', 'R_Stuff', 'FIS', 
#                              paste('FIS_', samp.year, sep = ''),
#                              paste('FIS_TimedSwimSurveys', samp.year, sep = ''),
#                              paste('FIS_TimedSwimSurvey', samp.year, '_Plots', sep = ''))

# create maps
for (i in blocks.sampled){
        
        site.samp.loc.blockno <- site.samp.start.subblock.loc.ref %>% 
                filter(blockno == i &
                               sampyear == samp.year)
        
        sf.subblock.map.crop.1 <- sf.subblock.map %>%
                       filter(blockno == i &
                               !subblockno %in% c('28B', '28C'))

        sf.subblock.map.crop.2 <- sf.subblock.map %>%
                {if(i == '27') filter(., blockno == '26') else filter(., blockno == i)}
        
        sf.subblock.map.crop.3 <- sf.subblock.map %>%
                {if(i == '16') filter(., blockno %in% c('14', '15')) else filter(., blockno == i)}
        
        
        sf.subblock.map.crop <- bind_rows(sf.subblock.map.crop.1, sf.subblock.map.crop.2, sf.subblock.map.crop.3)
        
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
## 8. Final data frame ####
## Join spatial site data (proposed and actual GPS sites) to count data and save final dataframe

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
        
        ts.dat.prop <- bind_rows(ts.dat.prop.2020, ts.dat.prop.2021, ts.dat.prop.2022) %>%   
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
        
        ts.dat.act <- bind_rows(ts.dat.act.2020, ts.dat.act.2021, ts.dat.act.2022) %>%    
                select(-c(blockno.y, site.old)) %>% 
                dplyr::rename(actual.geom = 'geometry',
                              blockno = 'blockno.x')         
        
time.swim.dat.act.prop <- left_join(ts.dat.prop %>% dplyr::select(-c(subblockno, zone)), ts.dat.act) %>%  
        select(c(site, sampdate, sampyear, blockno, subblockno, diver, starttime, 
                 firstabtime, finishtime, time.elapsed, sizeclass, sizeclass.2021, sizeclass_freq, 
                 minsize, midsize, midsize.2021, maxsize, legal.size, actual.geom, proposed.geom))

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

ts.dat.prop.df <- bind_rows(ts.dat.prop.df.2020, ts.dat.prop.df.2021, ts.dat.prop.df.2022) %>%
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

ts.dat.act.df <- bind_rows(ts.dat.act.df.2020, ts.dat.act.df.2021, ts.dat.act.df.2022) %>%
        select(-c(blockno.y, site.old)) %>%
        dplyr::rename(actual.geom = 'geometry',
                      blockno = 'blockno.x') %>%
        select(-c(gpstime, sampperiod, samptime, waypoint))

time.swim.dat.df.act.prop <- left_join(ts.dat.prop.df %>% dplyr::select(-c(subblockno, zone)), ts.dat.act.df) %>%  
        select(c(site, sampdate, sampyear, blockno, subblockno, diver, starttime,
                 firstabtime, finishtime, time.elapsed, shelllength, shelllength.2021, sizeclass, 
                 sizeclass.2021, sizeclass_freq, minsize, maxsize, legal.size, 
                 actual.geom, proposed.geom))

# join time swim size fequency and individual length data to original site data to 
# include 'oid', 'cell.ntile' and 'sam.count'

# load cpue oid data and combine
ts.site.cpue.2020.join <- readRDS('C:/CloudStor/Shared/DiveFisheries/Abalone/FISdata/FIS_TimedSwimSurveys2021/ts.site.cpue.2020.join.RDS')
ts.site.cpue.2021.join <- readRDS('C:/CloudStor/Shared/DiveFisheries/Abalone/FISdata/FIS_TimedSwimSurveys2021/ts.site.cpue.2021.join.RDS')
ts.site.cpue.join <- bind_rows(ts.site.cpue.2020.join, ts.site.cpue.2021.join)

# load sam data and remove duplicate sites with same site name
ts.site.sam.dat <- readRDS('C:/CloudStor/Shared/DiveFisheries/Abalone/FISdata/FIS_TimedSwimSurveys2021/ts.site.sam.2020.join.RDS')
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


saveRDS(time.swim.dat.final, paste(samp.year.folder, '/time.swim.dat.final.RDS', sep = ''))
saveRDS(time.swim.dat.df.final, paste(samp.year.folder, '/time.swim.dat.df.final.RDS', sep = ''))

##---------------------------------------------------------------------------##
## 9. Data frame description ####
# sampdate = date of sample collection
# site = sample location name (Note: 2020 sites renamed for south-north order)
# diver = diver name initials
# starttime = start time of timed swim
# firstabtime = time first abalone found on timed swim
# finishtime = finish time of timed swim
# sizeclass = size class bins used for 2020 survey (i.e. 20 mm size bins)
# minsize = minimum size of 2020 size class
# maxsize = maximum size of 2020 size class
# sizeclass_freq = number of abalone counted in 2020 size class
# time.elapsed = time elapsed between firstabtime and finish time in seconds
# sampyear = year of sample
# blockno = block number of sample
# legal.size = arbitrary reference size of legal minimum length for Eastern Zone
# midsize = mid-point of 2020 size class        
# sizeclass.2021 = size class bins used for 2021 survey and applied to 2020 data
# (i.e. 0-100 mm then 20 mm size bins)
# midsize.2021 = mid-point of 2021 size class 
##---------------------------------------------------------------------------##
# 10. Data Analysis ####

# clear list
# rm(list = ls())

# Import final dataframes 
time.swim.dat.final <-
        readRDS(paste(samp.year.folder, '/time.swim.dat.final.RDS', sep = ''))

time.swim.dat.df.final <-
        readRDS(paste(samp.year.folder, '/time.swim.dat.df.final.RDS', sep = ''))

##---------------------------------------------------------------------------##
## 11. Standardise data #### 

# Standardise counts for 10 minute swim (i.e. some swims marginally shorter or longer duration)
std.ts.dat <- time.swim.dat.final %>% 
    mutate(sizeclass_freq_10 = round((sizeclass_freq / time.elapsed) * 10))

# Summarise total count for blockno x site x sampyear x legal.size
ts.count.sum <- std.ts.dat %>% 
    filter(!subblockno %in% c('28B', '28C')) %>%
    group_by(blockno, site, sampyear, legal.size) %>% 
    summarise(ab.n = sum(sizeclass_freq_10)) %>%  
    group_by(blockno, site, sampyear, legal.size) %>% 
    group_by(site)

ts.av.count <- std.ts.dat %>% 
  filter(!subblockno %in% c('28B', '28C')) %>%
  group_by(blockno, site, diver, sampyear, legal.size) %>% 
  summarise(ab.n = sum(sizeclass_freq_10)) %>%  
  group_by(blockno, sampyear, legal.size) %>%
  summarise(av.count = mean(ab.n),
            sites = n_distinct(site)) %>% 
  pivot_wider(id_cols = c(blockno),
              names_from = c(legal.size, sampyear),
              values_from = c('av.count', 'sites'))
##---------------------------------------------------------------------------##
std.ts.dat %>% 
 filter(!subblockno %in% c('28B', '28C')) %>%
 group_by(blockno, sampyear) %>% 
 summarise(n = n_distinct(sampdate),
           sites = n_distinct(site)) %>% 
 pivot_wider(id_cols = c(blockno),
             names_from = c(sampyear),
             values_from = c('n', 'sites')) %>% 
 adorn_totals()
##---------------------------------------------------------------------------##
## PLOT 1: COUNT PER TEN MIN YEARS ####
## average count of all legal and sub-legal abalone per 10 min by year for each site within each block 
## (i.e. the average count between paired divers for each site)

# determine mean abalone abundance for block x sampyear x size class
ten.min.mean.year <- std.ts.dat %>% 
 filter(!subblockno %in% c('28B', '28C') &
         !blockno %in% c('13', '14', '29', '30')) %>% 
 group_by(blockno, site, diver, sampyear, time.elapsed, legal.size) %>% 
 summarise(ab.n = sum(sizeclass_freq_10)) %>% 
 group_by(blockno, sampyear, legal.size) %>% 
 summarise(mean.ab.n = mean(ab.n)) %>% 
 mutate(sampyear = factor(sampyear))

time.swim.dat.n <- std.ts.dat %>% 
 filter(!subblockno %in% c('28B', '28C')&
         !blockno %in% c('13', '14', '29', '30')) %>% 
 group_by(sampyear, blockno, legal.size) %>% 
 summarise(n = n_distinct(site))

sub.legal.plot <- std.ts.dat %>% 
 filter(!subblockno %in% c('28B', '28C'),
        legal.size == '<140 mm' &
         !blockno %in% c('13', '14', '29', '30')) %>%
 # filter(midsize < 150) %>% 
 group_by(blockno, site, diver, sampyear) %>% 
 summarise(ab.n = sum(sizeclass_freq_10)) %>% 
 group_by(blockno, site, sampyear) %>% 
 summarise(mean.ab.n = mean(ab.n)) %>% 
 mutate(sampyear = factor(sampyear, levels = c('2020', '2021', '2022'))) %>%  
 ggplot(aes(x = blockno, y = mean.ab.n))+
 geom_boxplot(aes(fill = sampyear), position = position_dodge2(1, preserve = 'single'),
              outlier.colour = '#EE8866') +
 scale_fill_manual(values = c("#77AADD", "#BBCC33", "#DDDDDD"))+
 geom_point(data = ten.min.mean.year %>% filter(legal.size == '<140 mm'), aes(group = factor(sampyear, levels = c('2020', '2021', '2022'))), shape = 19,
            size = 2, colour = 'red', fill = 'red', position = position_dodge2(0.8))+
 theme_bw()+
 ylab(bquote('Average count (abalone.10'*~min^-1*')'))+
 xlab('Blockno')+
 ylim(0, 150)+
 geom_text(data = time.swim.dat.n %>% filter(legal.size == '<140 mm'), aes(y = 150, label = n, colour = factor(sampyear, levels = c('2020', '2021', '2022'))), size = 3, 
           position = position_dodge2(0.8))+
 scale_colour_manual(values = c("#77AADD", "#BBCC33", "#DDDDDD"))+
 guides(size = 'legend', colour = 'none',
        fill = guide_legend(title = 'Year'))+
 ggtitle('Sub-legal <140 mm')+
 theme(plot.title = element_text(vjust = 0, hjust = 0))+
 theme(legend.position = 'none')

legal.plot <- std.ts.dat %>% 
 filter(!subblockno %in% c('28B', '28C'),
        legal.size == '>140 mm' &
         !blockno %in% c('13', '14', '29', '30')) %>%
 # filter(midsize < 150) %>% 
 group_by(blockno, site, diver, sampyear) %>% 
 summarise(ab.n = sum(sizeclass_freq_10)) %>% 
 group_by(blockno, site, sampyear) %>% 
 summarise(mean.ab.n = mean(ab.n)) %>% 
 mutate(sampyear = factor(sampyear, levels = c('2020', '2021', '2022'))) %>%  
 ggplot(aes(x = blockno, y = mean.ab.n))+
 geom_boxplot(aes(fill = sampyear), position = position_dodge2(1, preserve = 'single'),
              outlier.colour = '#EE8866') +
 scale_fill_manual(values = c("#77AADD", "#BBCC33", "#DDDDDD"))+
 geom_point(data = ten.min.mean.year %>% filter(legal.size == '>140 mm'), aes(group = factor(sampyear, levels = c('2020', '2021', '2022'))), shape = 19,
            size = 2, colour = 'red', fill = 'red', position = position_dodge2(0.8))+
 theme_bw()+
 ylab(bquote('Average count (abalone.10'*~min^-1*')'))+
 xlab('Blockno')+
 ylim(0, 150)+
 guides(size = 'legend', colour = 'none',
        fill = guide_legend(title = 'Year'))+
 ggtitle('Legal >140 mm')+
 theme(plot.title = element_text(vjust = 0, hjust = 0))+
 theme(legend.title = element_blank(),
       legend.position = c(0.9, 0.7))

count.plot.sizeclass <- grid.arrange(sub.legal.plot, legal.plot, nrow = 2)

setwd(ts.plots.folder)
ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_TenMinuteCount_LegalSubLegal', '.pdf', sep = ''), 
       plot = count.plot.sizeclass, units = 'mm', width = 190, height = 200)
ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_TenMinuteCount_LegalSubLegal', '.png', sep = ''), 
       plot = count.plot.sizeclass, units = 'mm', width = 190, height = 200)

##---------------------------------------------------------------------------##
## PLOT 2: REPEAT COUNT PER TEN MIN YEARS ####
# compare counts at repeat sites between 2020 and 2021

count.plot.rep.dat <- std.ts.dat %>% 
 filter(!subblockno %in% c('28B', '28C') &
         !blockno %in% c('13', '14', '29', '30')) %>% 
 group_by(blockno, site, diver, sampyear, legal.size) %>% 
 summarise(ab.n = sum(sizeclass_freq)) %>% 
 group_by(blockno, site, sampyear, legal.size) %>% 
 summarise(mean.ab.n = mean(ab.n)) %>% 
 group_by(site) %>% 
 filter(n() > 2)

ten.min.mean.rep.year.sites <- count.plot.rep.dat %>% 
 group_by(blockno, sampyear, legal.size) %>% 
 summarise(n = n_distinct(site))

count.plot.rep.mean <- std.ts.dat %>% 
 filter(!subblockno %in% c('28B', '28C'),
        !blockno %in% c(13, 14, 29, 30)) %>%
 group_by(blockno, site, diver, sampyear, legal.size) %>% 
 summarise(ab.n = sum(sizeclass_freq)) %>% 
 group_by(blockno, sampyear, legal.size) %>%
 filter(n() > 1) %>% 
 summarise(mean.ab.n = mean(ab.n))

sub.legal.plot.rep <- count.plot.rep.dat %>% 
 filter(legal.size == '<140 mm') %>%
 mutate(sampyear = factor(sampyear, levels = c('2020', '2021', '2022'))) %>%  
 ggplot(aes(x = blockno, y = mean.ab.n))+
 geom_boxplot(aes(fill = sampyear), position = position_dodge2(1, preserve = 'single'),
              outlier.colour = '#EE8866') +
 scale_fill_manual(values = c("#77AADD", "#BBCC33", "#DDDDDD"))+
 geom_point(data = count.plot.rep.mean %>% filter(legal.size == '<140 mm'), aes(group = factor(sampyear, levels = c('2020', '2021', '2022'))), shape = 19,
            size = 2, colour = 'red', fill = 'red', position = position_dodge2(0.8))+
 theme_bw()+
 ylab(bquote('Average count (abalone.10'*~min^-1*')'))+
 xlab('Blockno')+
 ylim(0, 100)+
 geom_text(data = ten.min.mean.rep.year.sites %>% filter(legal.size == '<140 mm'), aes(y = 100, label = n, colour = factor(sampyear, levels = c('2020', '2021', '2022'))), size = 3, 
           position = position_dodge2(0.8))+
 scale_colour_manual(values = c("#77AADD", "#BBCC33", "#DDDDDD"))+
 guides(size = 'legend', colour = 'none',
        fill = guide_legend(title = 'Year'))+
 ggtitle('Sub-legal <140 mm Repeat Sites')+
 theme(plot.title = element_text(vjust = 0, hjust = 0))+
 theme(legend.position = 'none')

legal.plot.rep <- count.plot.rep.dat %>% 
 filter(legal.size == '>140 mm') %>%
 mutate(sampyear = factor(sampyear, levels = c('2020', '2021', '2022'))) %>%  
 ggplot(aes(x = blockno, y = mean.ab.n))+
 geom_boxplot(aes(fill = sampyear), position = position_dodge2(1, preserve = 'single'),
              outlier.colour = '#EE8866') +
 scale_fill_manual(values = c("#77AADD", "#BBCC33", "#DDDDDD"))+
 geom_point(data = count.plot.rep.mean %>% filter(legal.size == '>140 mm'), 
            aes(group = factor(sampyear, levels = c('2020', '2021', '2022'))), shape = 19,
            size = 2, colour = 'red', fill = 'red', position = position_dodge2(0.8))+
 theme_bw()+
 ylab(bquote('Average count (abalone.10'*~min^-1*')'))+
 xlab('Blockno')+
 ylim(0, 100)+
 guides(size = 'legend', colour = 'none',
        fill = guide_legend(title = 'Year'))+
 ggtitle('Legal >140 mm Repeat Sites')+
 theme(plot.title = element_text(vjust = 0, hjust = 0))+
 theme(legend.title = element_blank(),
       legend.position = c(0.9, 0.8))

count.plot.rep.sizeclass <- grid.arrange(sub.legal.plot.rep, legal.plot.rep, nrow = 2)

setwd(ts.plots.folder)
ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_TenMinuteCount_RepeatSites', '.pdf', sep = ''), 
       plot = count.plot.rep.sizeclass, units = 'mm', width = 190, height = 200)
ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_TenMinuteCount_RepeatSites', '.png', sep = ''), 
       plot = count.plot.rep.sizeclass, units = 'mm', width = 190, height = 200)
##---------------------------------------------------------------------------##
## PLOT 3: COUNT PER TEN MIN ####
## plot showing average count of all legal and sub-legal abalone per 10 min for
## each site within each blockno (i.e. the average between paired divers for each site)

# determine number of sites sampled for each blockno
time.swim.dat.n <- time.swim.dat.final %>%
 filter(!subblockno %in% c('28B', '28C'),
        sampdate > as.Date('2022-01-01')) %>%
 group_by(blockno) %>%
 summarise(n = n_distinct(site))

## average count per 10 min for each blockno
count.plot <- std.ts.dat %>%
 filter(!subblockno %in% c('28B', '28C'),
        sampdate > as.Date('2022-01-01')) %>%
 # filter(midsize >= 150) %>%
 group_by(blockno, site, diver) %>%
 summarise(ab.n = sum(sizeclass_freq_10)) %>%
 group_by(blockno, site, diver) %>%
 summarise(mean.ab.n = mean(ab.n)) %>%
 ggplot(aes(x = blockno, y = mean.ab.n, colour = diver)) +
 geom_point(size = 3) +
 scale_colour_grey() +
 stat_summary(
  fun.y = mean,
  geom = 'point',
  shape = 19,
  size = 4,
  colour = 'red',
  fill = 'red'
 ) +
 theme_bw() +
 ylab(bquote('Average count (abalone.10' *  ~ min ^ -1 * ')')) +
 xlab('Blockno') +
 ylim(0, 250) +
 geom_text(data = time.swim.dat.n,
           aes(y = 250, label = n),
           color = 'black',
           size = 3) +
 theme(legend.position = 'none')

setwd(ts.plots.folder)

ggsave(
 filename = paste('TimedSwimSurvey_', samp.year, '_TenMinuteCountPlot', '.pdf', sep = ''),
 plot = count.plot,
 units = 'mm',
 width = 190,
 height = 120
)
ggsave(
 filename = paste('TimedSwimSurvey_', samp.year, '_TenMinuteCountPlot', '.png', sep = ''),
 plot = count.plot,
 units = 'mm',
 width = 190,
 height = 120
)

##---------------------------------------------------------------------------##
## PLOT 4: COUNT PER TEN MIN SIZECLASS ####
## average count of all legal and sub-legal abalone per 10 min by sizeclass for each site within each blockno 
## (i.e. the average between paired divers for each site)

# determine mean count per 10 min by size class for each blockno
ten.min.mean <- std.ts.dat %>% 
 filter(!subblockno %in% c('28B', '28C'),
        sampdate > as.Date('2022-01-01')) %>% 
 # filter(midsize < 150) %>% 
 group_by(blockno, site, diver, legal.size) %>% 
 summarise(ab.n = sum(sizeclass_freq)) %>% 
 group_by(blockno, legal.size) %>% 
 summarise(mean.ab.n = mean(ab.n)) %>% 
 mutate(legal.size = factor(legal.size))

ten.min.mean.site <- std.ts.dat %>% 
 filter(!subblockno %in% c('28B', '28C'),
        sampdate > as.Date('2022-01-01')) %>% 
 # filter(midsize < 150) %>% 
 group_by(site, blockno, subblockno, diver, legal.size) %>% 
 summarise(ab.n = sum(sizeclass_freq)) %>% 
 group_by(site, blockno, subblockno, legal.size) %>% 
 summarise(mean.ab.n = mean(ab.n)) %>% 
 mutate(legal.size = factor(legal.size))%>% 
 as.data.frame()

count.plot.sizeclass <- std.ts.dat %>% 
 filter(!subblockno %in% c('28B', '28C'),
        sampdate > as.Date('2022-01-01')) %>% 
 # filter(midsize < 150) %>% 
 group_by(blockno, site, diver, legal.size) %>% 
 summarise(ab.n = sum(sizeclass_freq)) %>% 
 group_by(blockno, site, legal.size) %>% 
 summarise(mean.ab.n = mean(ab.n)) %>% 
 mutate(legal.size = factor(legal.size)) %>% 
 ggplot(aes(x = blockno, y = mean.ab.n))+
 # geom_boxplot()+
 geom_boxplot(aes(fill = legal.size), position = position_dodge(0.9), outlier.colour = '#EE8866')+
 scale_fill_manual(values = c("#999999", "#56B4E9"))+
 # stat_summary(fun.y = mean, geom = 'point', shape = 19, size = 4, colour = 'red', fill = 'red')+
 geom_point(data = ten.min.mean, aes(group = legal.size), shape = 19, size = 2, colour = 'red', fill = 'red', position = position_dodge(0.9))+
 theme_bw()+
 ylab(bquote('Average count (abalone.10'*~min^-1*')'))+
 xlab('Blockno')+
 ylim(0, 125)+
 geom_text(data = time.swim.dat.n, aes(y = 125, label = n), color = 'black', size = 3)+
 theme(legend.position="none")

setwd(ts.plots.folder)
ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_TenMinuteCountSizeClassPlot', '.pdf', sep = ''), 
       plot = count.plot.sizeclass, units = 'mm', width = 190, height = 120)
ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_TenMinuteCountSizeClassPlot', '.png', sep = ''), 
       plot = count.plot.sizeclass, units = 'mm', width = 190, height = 120)
##---------------------------------------------------------------------------##
# PLOT 5: Counts x year x repeat sites ####
# Compare total counts of size class between years for repeat site

# Select repeat sites between years
ts.rep.dat <- ts.count.sum %>% 
    filter(n() > 2)
    
# Identify repeat blocks
ts.rep.blocks <- ts.rep.dat %>% 
  group_by(blockno) %>% 
  distinct(blockno) %>% 
  pull()

# Generate plot
for (i in ts.rep.blocks){

 rep.plot <- ts.rep.dat %>% 
  filter(blockno == i) %>%
  group_by(sampyear, site) %>%
  ggplot(aes(reorder_within(site, -ab.n, sampyear, fun = 'sum'), 
             y = ab.n, fill = legal.size))+
  geom_bar(stat = 'identity', position = 'stack')+
  ylim(0, 400)+
  xlab('Site')+
  ylab(bquote('Total count (abalone.10'*~min^-1*')'))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  scale_fill_manual(values = c("#999999", "#56B4E9"))+
  scale_x_reordered()+
  facet_wrap(~sampyear, scales = 'free')+
  theme(legend.position = 'none')

# save plot
setwd(ts.plots.folder)
ggsave(filename = paste('TimedSwimSurvey_RepeatSites_', 'BlockNo', i, 
                        '_TotalCount_', min(ts.rep.dat$sampyear), '-',
                        max(ts.rep.dat$sampyear), '.pdf', sep = ''),
       plot = rep.plot, units = 'mm', width = 190, height = 120)

ggsave(filename = paste('TimedSwimSurvey_RepeatSites_', 'BlockNo', i, 
                        '_TotalCount_', min(ts.rep.dat$sampyear), '-',
                        max(ts.rep.dat$sampyear), '.png', sep = ''),
       plot = rep.plot, units = 'mm', width = 190, height = 120)

}

##---------------------------------------------------------------------------##
## PLOT 6: Counts x year x top ten sites ####
# Determine and compare top ten sites in each block and year based on total 
# counts of size class

# Identify closed blocks sampled in 2020 only
ts.blocks <- c(16, 22, 23, 24, 27, 28)

# Generate plot
for (i in ts.blocks){
 
 top.ten.plot <- ts.count.sum %>% 
  filter(blockno == i) %>%
  group_by(sampyear, site) %>%
  summarise(ab.n.sum = sum(ab.n)) %>%
  slice_max(order_by = c(ab.n.sum), n = 10, with_ties = F) %>% 
  mutate(top.ten = 1) %>% 
  left_join(., ts.count.sum) %>% 
  ggplot(aes(reorder_within(site, -ab.n, sampyear, fun = 'sum'), 
             y = ab.n, fill = legal.size))+
  geom_bar(stat = 'identity', position = 'stack')+
  ylim(0, 400)+
  xlab('Site')+
  ylab(bquote('Total count (abalone.10'*~min^-1*')'))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  scale_fill_manual(values = c("#999999", "#56B4E9"))+
  scale_x_reordered()+
  facet_wrap(~sampyear, scales = 'free')+
  theme(legend.position = 'none')

# save plot
 setwd(ts.plots.folder)
ggsave(filename = paste('TimedSwimSurvey_TopTenSites_', 'BlockNo', i, 
                        '_TotalCount_', min(ts.count.sum$sampyear), '-',
                        max(ts.count.sum$sampyear), '.pdf', sep = ''),
       plot = top.ten.plot, units = 'mm', width = 190, height = 120)

ggsave(filename = paste('TimedSwimSurvey_TopTenSites_', 'BlockNo', i, 
                        '_TotalCount_', min(ts.count.sum$sampyear), '-',
                        max(ts.count.sum$sampyear), '.png', sep = ''),
       plot = top.ten.plot, units = 'mm', width = 190, height = 120)
}

##---------------------------------------------------------------------------##
## PLOT 7: LF x block ####
## Length frequency plot by block (mid-points) for sampling year

# determine number of abalone recorded and number of sites sampled per block
block.ab.n <- time.swim.dat.df.final %>% 
        filter(!subblockno %in% c('28B', '28C') &
               sampyear == samp.year) %>% 
               # sampdate > as.Date('2021-01-01')) %>% 
        group_by(blockno) %>% 
        summarise(ab.n = paste('n = ', n()))

block.site.n <- time.swim.dat.final %>% 
        filter(!subblockno %in% c('28B', '28C') &
                sampyear == samp.year) %>% 
 # sampdate > as.Date('2021-01-01')) %>% 
        group_by(blockno) %>% 
        summarise(site.n = paste('(', n_distinct(site), ')', sep = ''))

block.ab.site.n <- left_join(block.ab.n, block.site.n) %>% 
        mutate(n = paste(ab.n, site.n, sep = ' '))

# create length frequency plot
lf.plot <- time.swim.dat.df.final %>% 
        filter(!subblockno %in% c('28B', '28C')&
                sampyear == samp.year) %>% 
 # sampdate > as.Date('2021-01-01')) %>% 
        ggplot(aes(shelllength.2021, group = blockno)) +
        geom_bar(aes(y = ..prop.., stat = 'count'), width = 20, col = 'black', fill = '#EFC000FF')+
        # scale_x_binned()+
        geom_vline(aes(xintercept = ifelse(blockno %in% c('27', '28'), 145, 138)), linetype = 'dashed', colour = 'red', size = 0.5) +
        theme_bw() +
        facet_grid(blockno ~ .) +
        scale_y_continuous(breaks = seq(0, 0.4, 0.1), labels = seq(0, 40, 10))+
        xlab("Shell Length (mm)")+
        ylab(paste("Percentage (%)"))+
        geom_text(data = block.ab.site.n, aes(x = 180, y = 0.2, label = n), color = 'black', size = 3)


setwd(ts.plots.folder)
ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_SizeFrequencyPlot', '.pdf', sep = ''),
       plot = lf.plot, units = 'mm', width = 190, height = 250)
ggsave(filename = paste('TimedSwimSurvey_', samp.year, 'SizeFrequencyPlot', '.png', sep = ''),
       plot = lf.plot, units = 'mm', width = 190, height = 250)

##---------------------------------------------------------------------------##
## PLOT 8: LF x year ####
## Overlaid length frequency plot by block and year (size-classes)

# Determine number of abalone recorded and number of sites sampled per block
# and create plot labels
block.ab.n <- time.swim.dat.df.final %>% 
    filter(!subblockno %in% c('28B', '28C'),
           # sampdate > as.Date('2021-01-01'),
           !blockno %in% c(13, 14, 29, 30)) %>% 
    group_by(sampyear, blockno) %>% 
    summarise(ab.n = paste('n = ', n()))

block.site.n <- time.swim.dat.final %>% 
    filter(!subblockno %in% c('28B', '28C'),
           # sampdate > as.Date('2021-01-01'),
           !blockno %in% c(13, 14, 29, 30)) %>%
    group_by(sampyear, blockno) %>% 
    summarise(site.n = paste('(', n_distinct(site), ')', sep = ''))

block.ab.site.n <- left_join(block.ab.n, block.site.n) %>% 
    mutate(n = paste(ab.n, site.n, sep = ' '))

# Determine length frequency proportions for sampling years and combine
lf.df <- time.swim.dat.final %>%
 filter(!subblockno %in% c('28B', '28C'),
        # sampdate > as.Date('2021-01-01'),
        !blockno %in% c(13, 14, 29, 30)) %>%
 group_by(sampyear, blockno, sizeclass.2021) %>% 
 summarise(n = sum(sizeclass_freq)) %>% 
 mutate(freq = n / sum(n))

lf.plot <- ggplot()+
 geom_bar(data = lf.df, aes(x = sizeclass.2021, y = freq*100, fill = factor(sampyear)),
          stat = 'identity', position = 'dodge')+
 geom_vline(data = lf.df, aes(xintercept = ifelse(blockno %in% c('27', '28'), 3.8, 3.5)), linetype = 'dashed', colour = 'red', size = 0.5) +
 theme_bw() +
 scale_fill_manual(values = c("#77AADD", "#BBCC33", "#DDDDDD"))+
 # scale_fill_manual(values = c("#7AA6DC99", "#8F770099"))+
 guides(fill = guide_legend(title = 'Year'))+
 facet_grid(blockno ~ .) +
 scale_y_continuous(breaks = seq(0, 40, 10), labels = seq(0, 40, 10))+
 xlab("Shell Length (mm)")+
 ylab(paste("Percentage (%)"))+
 geom_text(data = block.ab.site.n, aes(x = 7, y = 10, label = n, colour = factor(sampyear, levels = c('2020', '2021', '2022'))), size = 3, 
           position = position_stack(vjust = 0.8))+
 scale_colour_manual(values = c("#77AADD", "#BBCC33", "#DDDDDD"))+
 guides(size = 'legend', colour = 'none',
        fill = guide_legend(title = 'Year'))

setwd(ts.plots.folder)
ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_SizeFrequencyPlot_BLOCKS16-28', '.pdf', sep = ''),
       plot = lf.plot, units = 'mm', width = 190, height = 250)
ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_SizeFrequencyPlot_BLOCKS16-28', '.png', sep = ''),
       plot = lf.plot, units = 'mm', width = 190, height = 250)

##---------------------------------------------------------------------------##
## TAB 1: SUMMARY ####
## summary table of counts and CPUE by size class and block 
## (NOTE: run script for CPUE above)

# Arrange size classes in order
sizeclasses <- c("0-20", "20-40", "40-60", "60-80", "80-100", "100-120", "120-140", "140-160", "160-180", "180-200", "200-220")

# Create summary table
time.swim.count.blockno <- std.ts.dat %>% 
 filter(!subblockno %in% c('28B', '28C'),
        sampdate > as.Date('2022-01-01')) %>% 
 group_by(blockno, site, diver, sampyear, legal.size, time.elapsed) %>% 
 summarise(ab.n = sum(sizeclass_freq)) %>% 
 group_by(blockno, legal.size) %>%
 summarise(sites = n_distinct(site),
           ab.min = round(mean(ab.n), digits = 2)) %>% 
 spread(legal.size, ab.min) %>%
 as.data.frame() %>% 
 dplyr::rename('Blockno' = blockno,
               'Sites' = sites,
               'Average\ncount\n<140mm' = '<140 mm',
               'Average\ncount\n>140mm' = '>140 mm') %>% 
 adorn_totals(fill = '',,,, contains('Sites'))

# Create formatted summary table
time.swim.summary.tab <- time.swim.count.blockno %>% 
 ggpubr::ggtexttable(rows = NULL, theme = ggpubr::ttheme('mOrange'))

# Save summary tables
setwd(ts.plots.folder)
write.xlsx(time.swim.count.blockno, paste('TimedSwimSurvey_', samp.year, '_SummaryTable.xlsx'), sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)
ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_SummaryTable', '.pdf', sep = ''), 
       plot = time.swim.summary.tab, units = 'mm', width = 190, height = 120)
ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_SummaryTable', '.png', sep = ''), 
       plot = time.swim.summary.tab, units = 'mm', width = 190, height = 120)
##---------------------------------------------------------------------------##
# Sites completed summary table
ts.tab <- std.ts.dat %>% 
 filter(!subblockno %in% c('28B', '28C'),
        sampdate > as.Date('2022-01-01')) %>% 
 group_by(blockno) %>%
 summarise(sites = n_distinct(site),
           field.days = n_distinct(sampdate),
           site.day = round(sites / field.days, digits = 1)) %>% 
 as.data.frame() %>% 
 dplyr::rename('Blockno' = blockno,
               'Sites' = sites,
               'Days' = field.days,
               'Sites.Day' = site.day) %>%
 add_row(Blockno = 'Total', Sites = sum(.$Sites), Days = sum(.$Days), 
         Sites.Day = round(mean(.$Sites.Day), digits = 1))

# Create formatted summary table
ts.tab.format <- ts.tab %>% 
 ggpubr::ggtexttable(rows = NULL, theme = ggpubr::ttheme('mOrange'))

# Save summary tables
setwd(ts.plots.folder)
write.xlsx(ts.tab, paste('TimedSwimSurvey_', samp.year, '_SiteSummaryTable.xlsx'), sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)
ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_SiteSummaryTable', '.pdf', sep = ''), 
       plot = ts.tab.format, units = 'mm', width = 190, height = 120)
ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_SiteSummaryTable', '.png', sep = ''), 
       plot = ts.tab.format, units = 'mm', width = 190, height = 120)
##---------------------------------------------------------------------------##
## MAP 1: Site Count x size ####
## Average Count by site - possibly explore KUDs

sf.tas.map <- st_read("C:/CloudStor/Shared/DiveFisheries/GIS/SpatialLayers/TasLand.gpkg")
sf.subblock.map <- st_read("C:/CloudStor/Shared/DiveFisheries/GIS/SpatialLayers/SubBlockMaps.gpkg")

# transform maps to GDA2020

# set CRS
GDA2020 <- st_crs(7855)
GDA94 <- st_crs(28355)
WGS84 <- st_crs(4326)

sf.tas.map <- sf.tas.map %>% 
        st_set_crs(GDA2020)

sf.subblock.map <- st_transform(sf.subblock.map, GDA2020)

# join CPUE data to site location data

# identify unique sites sampled from timed swim dataframe and join to renamed site file
time.swim.sites <- std.ts.dat %>%
    filter(sampyear == samp.year) %>% 
           # sampdate > as.Date('2021-01-01')) %>%
        distinct(site, .keep_all = T) %>% 
        select(c(site, blockno, subblockno, sampdate, actual.geom)) %>% 
        st_as_sf() %>% 
        st_set_crs(GDA2020)

ten.min.mean.site <- std.ts.dat %>% 
    filter(!subblockno %in% c('28B', '28C'),
           sampyear == samp.year) %>% 
           # sampdate > as.Date('2021-01-01')) %>%
    group_by(site, blockno, subblockno, diver, legal.size) %>% 
    summarise(ab.n = sum(sizeclass_freq)) %>% 
    group_by(site, blockno, subblockno, legal.size) %>% 
    summarise(mean.ab.n = mean(ab.n)) %>% 
    mutate(legal.size = factor(legal.size))%>% 
    as.data.frame()

# # transform to GDA2020
# time.swim.sites <- st_transform(time.swim.sites, GDA2020)

time.swim.count.site.loc <- left_join(ten.min.mean.site, time.swim.sites) %>% 
        st_as_sf() 

# identify blocks sampled
blocks.sampled <- unique(time.swim.count.site.loc$blockno)

# identify sites not surveyed but proposed
ts.proposed.geom <- readRDS(paste(samp.year.folder, '/ts.proposed.geom.RDS', sep = ''))

# identify proposed sites not surveyed
not.surveyed.df <- ts.proposed.geom %>% 
  filter(sampyear == samp.year,
         sampled == '0')

# create plot

for (i in blocks.sampled){
        
        # filter for blockno and sub-legal counts
        count.site.dat.non.legal <- time.swim.count.site.loc %>% 
                filter(blockno == i &
                               legal.size == '<140 mm',
                       !subblockno %in% c('28B', '28C')) %>% 
                st_as_sf
        
        count.site.dat.legal <- time.swim.count.site.loc %>% 
                filter(blockno == i &
                               legal.size == '>140 mm',
                       !subblockno %in% c('28B', '28C')) %>% 
                st_as_sf
        
        # filter subblock map for blockno
        sf.subblock.map.crop <- sf.subblock.map %>%
                filter(blockno == i &
                               !subblockno %in% c('28B', '28C'))  %>% 
                st_as_sf() 
        
        # filter proposed sites not surveyed
        sites.not.surveyed <- not.surveyed.df %>% 
                filter(blockno == i,
                       !subblockno %in% c('28B', '28C'))
        
        # crop tas map to blockno
        sf.tas.map.crop <- st_crop(sf.tas.map, sf.subblock.map.crop)
        
        count.site.map.non.legal <- ggplot(data = st_geometry(sf.tas.map.crop)) +
                geom_sf(data = sf.subblock.map.crop, aes(label = subblockno), fill = NA)+
                geom_sf_text(data = sf.subblock.map.crop, aes(label = subblockno))+
                geom_sf(fill = 'grey') +
                geom_sf(data = sites.not.surveyed, shape = 5, size = 0.8, colour = 'red')+
                geom_sf(data = count.site.dat.non.legal, aes(fill = mean.ab.n), shape = 21, size = 2)+
                scale_fill_gradientn(colours = c("navyblue", "blue", "cyan", "green", "yellow", "orange", "red"),
                                     limits = c(0, 125),
                                     breaks = c(0, 50, 100),
                                     labels = c(0, 50, 100))+
                theme_bw() +
                annotation_scale(location = "bl", width_hint = 0.5) +
                annotation_north_arrow(location = "br", which_north = "true", 
                                       pad_x = unit(0.05, "cm"), pad_y = unit(0.1, "cm"),
                                       style = north_arrow_fancy_orienteering)+
                labs(title = '<140 mm', fill = (bquote('Average\ncount')))+
                xlab('Longitude')+
                scale_x_continuous(breaks = seq(140, 149, by = 0.1))+
                ylab('Latitude')
        
        count.site.map.legal <- ggplot(data = st_geometry(sf.tas.map.crop)) +
                geom_sf(data = sf.subblock.map.crop, aes(label = subblockno), fill = NA)+
                geom_sf_text(data = sf.subblock.map.crop, aes(label = subblockno))+
                geom_sf(fill = 'grey') +
                geom_sf(data = sites.not.surveyed, shape = 5, size = 0.8, colour = 'red')+
                geom_sf(data = count.site.dat.legal, aes(fill = mean.ab.n), shape = 21, size = 2)+
                scale_fill_gradientn(colours = c("navyblue", "blue", "cyan", "green", "yellow", "orange", "red"),
                                     limits = c(0, 125),
                                     breaks = c(0, 50, 100),
                                     labels = c(0, 50, 100))+
                theme_bw() +
                annotation_scale(location = "bl", width_hint = 0.5) +
                annotation_north_arrow(location = "br", which_north = "true", 
                                       pad_x = unit(0.05, "cm"), pad_y = unit(0.1, "cm"),
                                       style = north_arrow_fancy_orienteering)+
                labs(title = '>140 mm', fill = (bquote('Average\ncount')))+
                xlab('Longitude')+
                scale_x_continuous(breaks = seq(140, 149, by = 0.1))+
                ylab('Latitude')
        
        # extract common legend from maps
        plot.legend <- get_legend(count.site.map.non.legal)  
        
        # remove legend from non legal map
        no.legend.non.legal <- count.site.map.non.legal +
          theme(legend.position = "none")
        
        # remove legend from legal map
        no.legend.legal <- count.site.map.legal +
          theme(legend.position = "none")
        
        # combine plots with common legend (adjusting layout for blocks)
        if(!i %in% c('16', '27')){ 
          count.site.map <- grid.arrange(arrangeGrob(no.legend.non.legal, no.legend.legal), plot.legend, 
                                         ncol = 2, widths = c(2.5, 0.8))
        } else{
          count.site.map <- grid.arrange(no.legend.non.legal, no.legend.legal, plot.legend, 
                                         ncol = 3, widths = c(2.5, 2.5, 0.8))
        }

        setwd(ts.plots.folder)
        ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_SiteCountMap_BlockNo_', i, '.pdf', sep = ''),
               plot = count.site.map, units = 'mm', width = 250, height = 250)
        ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_SiteCountMap_BlockNo_', i, '.png', sep = ''),
               plot = count.site.map, units = 'mm', width = 250, height = 250)
        
}

##---------------------------------------------------------------------------##
## MAP 2: Site Count x year ####
## Average Count by site between sample and previous year

sf.tas.map <- st_read("C:/CloudStor/Shared/DiveFisheries/GIS/SpatialLayers/TasLand.gpkg")
sf.subblock.map <- st_read("C:/CloudStor/Shared/DiveFisheries/GIS/SpatialLayers/SubBlockMaps.gpkg")

# transform maps to GDA2020

# set CRS
GDA2020 <- st_crs(7855)
GDA94 <- st_crs(28355)
WGS84 <- st_crs(4326)

sf.tas.map <- sf.tas.map %>% 
  st_set_crs(GDA2020)

sf.subblock.map <- st_transform(sf.subblock.map, GDA2020)

# join CPUE data to site location data

# identify unique sites sampled from timed swim dataframe and join to renamed site file
time.swim.sites <- std.ts.dat %>%
  # filter(sampdate > as.Date('2021-01-01')) %>%
  distinct(site, sampyear, .keep_all = T) %>% 
  select(c(site, blockno, subblockno, sampdate, actual.geom, sampyear)) %>% 
  st_as_sf() %>% 
  st_set_crs(GDA2020)

ten.min.mean.site <- std.ts.dat %>% 
  filter(!subblockno %in% c('28B', '28C')) %>% 
  group_by(site, blockno, subblockno, diver, sampyear) %>% 
  summarise(ab.n = sum(sizeclass_freq)) %>% 
  group_by(site, blockno, subblockno, sampyear) %>% 
  summarise(mean.ab.n = mean(ab.n)) %>% 
  # mutate(sampyear = factor(sampyear))%>% 
  as.data.frame()

time.swim.count.site.loc <- left_join(ten.min.mean.site, time.swim.sites) %>% 
  st_as_sf()

# identify blocks sampled
blocks.sampled <- time.swim.count.site.loc %>% 
  as.data.frame() %>% 
  filter(sampyear == samp.year) %>% 
  distinct(blockno) %>% 
  pull()

# identify sites not surveyed but proposed
ts.proposed.geom <- readRDS(paste(samp.year.folder, '/ts.proposed.geom.RDS', sep = ''))

# identify proposed sites not surveyed
not.surveyed.df <- ts.proposed.geom %>% 
  filter(sampled == '0')

# create plot

for (i in blocks.sampled){
  
  # filter for blockno and sub-legal counts
  count.site.dat.2020 <- time.swim.count.site.loc %>% 
    filter(blockno == i &
             sampyear == samp.year - 1) %>% 
    st_as_sf
  
  count.site.dat.2021 <- time.swim.count.site.loc %>% 
    filter(blockno == i &
             sampyear == samp.year) %>% 
    st_as_sf
  
  # filter subblock map for blockno
  sf.subblock.map.crop <- sf.subblock.map %>%
    filter(blockno == i &
             !subblockno %in% c('28B', '28C'))  %>% 
    st_as_sf() 
  
  # filter proposed sites not surveyed
  sites.not.surveyed.2020 <- not.surveyed.df %>% 
    filter(blockno == i &
             sampyear == samp.year - 1 &
             !subblockno %in% c('28B', '28C'))
  
  sites.not.surveyed.2021 <- not.surveyed.df %>% 
    filter(blockno == i &
             sampyear == samp.year &
             !subblockno %in% c('28B', '28C'))
  
  # crop tas map to blockno
  sf.tas.map.crop <- st_crop(sf.tas.map, sf.subblock.map.crop)
  
  # # crop tas and subblock map to spatial extent of all sites with a 1 km buffer
  # df.1 <- bind_rows(count.site.dat.2020, count.site.dat.2021) %>% 
  #   st_buffer(dist = 1000)
  # sf.tas.map.crop <- st_crop(sf.tas.map, df.1)
  # sf.subblock.map.crop <- st_crop(st_make_valid(sf.subblock.map), df.1)
  
  # ggplot(data = st_geometry(df.2))+
  #   geom_sf(data = df.3, aes(label = subblockno), fill = NA)+
  #   geom_sf_text(data = df.3, aes(label = subblockno))
  
  # create map for 2020
  count.site.map.2020 <- ggplot(data = st_geometry(sf.tas.map.crop)) +
    geom_sf(data = sf.subblock.map.crop, aes(label = subblockno), fill = NA)+
    geom_sf_text(data = sf.subblock.map.crop, aes(label = subblockno))+
    geom_sf(fill = 'grey') +
    geom_sf(data = sites.not.surveyed.2020, shape = 5, size = 0.8, colour = 'red')+
    geom_sf(data = count.site.dat.2020, aes(fill = mean.ab.n), shape = 21, size = 2)+
    scale_fill_gradientn(colours = c("navyblue", "blue", "cyan", "green", "yellow", "orange", "red"),
                         limits = c(0, 200),
                         breaks = c(0, 50, 100, 150),
                         labels = c(0, 50, 100, 150))+
    theme_bw() +
    annotation_scale(location = "bl", width_hint = 0.5) +
    annotation_north_arrow(location = "br", which_north = "true", 
                           pad_x = unit(0.05, "cm"), pad_y = unit(0.1, "cm"),
                           style = north_arrow_fancy_orienteering)+
    labs(title = samp.year - 1, fill = (bquote('Average\ncount')))+
    xlab('Longitude')+
    scale_x_continuous(breaks = seq(140, 149, by = 0.1))+
    ylab('Latitude')
  
  # create map for 2021
  count.site.map.2021 <- ggplot(data = st_geometry(sf.tas.map.crop)) +
    geom_sf(data = sf.subblock.map.crop, aes(label = subblockno), fill = NA)+
    geom_sf_text(data = sf.subblock.map.crop, aes(label = subblockno))+
    geom_sf(fill = 'grey') +
    geom_sf(data = sites.not.surveyed.2021, shape = 5, size = 0.8, colour = 'red')+
    geom_sf(data = count.site.dat.2021, aes(fill = mean.ab.n), shape = 21, size = 2)+
    scale_fill_gradientn(colours = c("navyblue", "blue", "cyan", "green", "yellow", "orange", "red"),
                         limits = c(0, 200),
                         breaks = c(0, 50, 100, 150),
                         labels = c(0, 50, 100, 150))+
    theme_bw() +
    annotation_scale(location = "bl", width_hint = 0.5) +
    annotation_north_arrow(location = "br", which_north = "true", 
                           pad_x = unit(0.05, "cm"), pad_y = unit(0.1, "cm"),
                           style = north_arrow_fancy_orienteering)+
    labs(title = samp.year, fill = (bquote('Average\ncount')))+
    xlab('Longitude')+
    scale_x_continuous(breaks = seq(140, 149, by = 0.1))+
    ylab('Latitude')

  # extract common legend from maps
plot.legend <- get_legend(count.site.map.2020)  

# remove legend from 2020 map
no.legend.2020 <- count.site.map.2020 +
  theme(legend.position = "none")

# remove legend from 2021 map
no.legend.2021 <- count.site.map.2021 +
  theme(legend.position = "none")

# combine plots with common legend (adjusting layout for blocks)
if(!i %in% c('16', '27')){ 
 count.site.map <- grid.arrange(arrangeGrob(no.legend.2020, no.legend.2021), plot.legend, 
                                ncol = 2, widths = c(2.5, 0.8))
} else{
 count.site.map <- grid.arrange(no.legend.2020, no.legend.2021, plot.legend, 
                                ncol = 3, widths = c(2.5, 2.5, 0.8))
}

# # combine plots with common legend (adjusting layout for blocks)
# if(i != '16'){ 
#   count.site.map <- grid.arrange(no.legend.2020, no.legend.2021, plot.legend, 
#                                  ncol = 3, widths = c(2.5, 2.5, 0.8))
#   # count.site.map <- grid.arrange(arrangeGrob(no.legend.2020, no.legend.2021), plot.legend, 
#   #                              ncol = 2, widths = c(2.5, 0.8))
# } else{
#   count.site.map <- grid.arrange(no.legend.2020, no.legend.2021, plot.legend, 
#                                ncol = 3, widths = c(2.5, 2.5, 0.8))
# }
  # save plot
setwd(ts.plots.folder)
ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_SiteCountMap-', samp.year - 1,
'vs', samp.year, '_BlockNo_', i, '.pdf', sep = ''),
         plot = count.site.map, units = 'mm', width = 250, height = 250)
  ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_SiteCountMap-', samp.year - 1,
                          'vs', samp.year, '_BlockNo_', i, '.png', sep = ''),
         plot = count.site.map, units = 'mm', width = 250, height = 250)
  
}

##---------------------------------------------------------------------------##
## MAP 3: Site Count x year x size class ####
## Average Count by site between years

sf.tas.map <- st_read("C:/CloudStor/Shared/DiveFisheries/GIS/SpatialLayers/TasLand.gpkg")
sf.subblock.map <- st_read("C:/CloudStor/Shared/DiveFisheries/GIS/SpatialLayers/SubBlockMaps.gpkg")

# transform maps to GDA2020

# set CRS
GDA2020 <- st_crs(7855)
GDA94 <- st_crs(28355)
WGS84 <- st_crs(4326)

sf.tas.map <- sf.tas.map %>% 
  st_set_crs(GDA2020)

sf.subblock.map <- st_transform(sf.subblock.map, GDA2020)

# join CPUE data to site location data

# identify unique sites sampled from timed swim dataframe and join to renamed site file
time.swim.sites <- std.ts.dat %>%
  # filter(sampdate > as.Date('2021-01-01')) %>%
  distinct(site, sampyear, .keep_all = T) %>% 
  select(c(site, blockno, subblockno, sampdate, actual.geom, sampyear)) %>% 
  st_as_sf() %>% 
  st_set_crs(GDA2020)

ten.min.mean.site <- std.ts.dat %>% 
  filter(!subblockno %in% c('28B', '28C')) %>% 
  group_by(site, blockno, subblockno, diver, sampyear, legal.size) %>% 
  summarise(ab.n = sum(sizeclass_freq)) %>% 
  group_by(site, blockno, subblockno, sampyear, legal.size) %>% 
  summarise(mean.ab.n = mean(ab.n)) %>% 
  # mutate(sampyear = factor(sampyear))%>% 
  as.data.frame()


time.swim.count.site.loc <- left_join(ten.min.mean.site, time.swim.sites) %>% 
  st_as_sf()

# identify blocks sampled
blocks.sampled <- time.swim.count.site.loc %>% 
  as.data.frame() %>% 
  filter(sampyear == samp.year) %>% 
  distinct(blockno) %>% 
  pull()

# identify sites not surveyed but proposed
ts.proposed.geom <- readRDS(paste(samp.year.folder, '/ts.proposed.geom.RDS', sep = ''))

# identify proposed sites not surveyed
not.surveyed.df <- ts.proposed.geom %>% 
  filter(sampled == '0')


# create plot

for (i in blocks.sampled){
  
  # filter for blockno and sub-legal counts
  count.site.dat.2020 <- time.swim.count.site.loc %>% 
    filter(blockno == i &
             sampyear == samp.year - 1) %>% 
    st_as_sf
  
  count.site.dat.2021 <- time.swim.count.site.loc %>% 
    filter(blockno == i &
             sampyear == samp.year) %>% 
    st_as_sf
  
  # filter subblock map for blockno
  sf.subblock.map.crop <- sf.subblock.map %>%
    filter(blockno == i &
             !subblockno %in% c('28B', '28C'))  %>% 
    st_as_sf() 
  
  # filter proposed sites not surveyed
  sites.not.surveyed.2020 <- not.surveyed.df %>% 
    filter(blockno == i &
             sampyear == samp.year - 1 &
             !subblockno %in% c('28B', '28C'))
  
  sites.not.surveyed.2021 <- not.surveyed.df %>% 
    filter(blockno == i &
             sampyear == samp.year &
             !subblockno %in% c('28B', '28C'))
  
  # crop tas map to blockno
  sf.tas.map.crop <- st_crop(sf.tas.map, sf.subblock.map.crop)
  
  # # crop tas and subblock map to spatial extent of all sites with a 1 km buffer
  # df.1 <- bind_rows(count.site.dat.2020, count.site.dat.2021) %>% 
  #   st_buffer(dist = 1000)
  # sf.tas.map.crop <- st_crop(sf.tas.map, df.1)
  # sf.subblock.map.crop <- st_crop(st_make_valid(sf.subblock.map), df.1)
  
  # ggplot(data = st_geometry(df.2))+
  #   geom_sf(data = df.3, aes(label = subblockno), fill = NA)+
  #   geom_sf_text(data = df.3, aes(label = subblockno))
  
  # create map for 2020
  count.site.map.2020.sublegal <- ggplot(data = st_geometry(sf.tas.map.crop)) +
    geom_sf(data = sf.subblock.map.crop, aes(label = subblockno), fill = NA)+
    geom_sf_text(data = sf.subblock.map.crop, aes(label = subblockno))+
    geom_sf(fill = 'grey') +
    geom_sf(data = sites.not.surveyed.2020, shape = 5, size = 0.8, colour = 'red')+
    geom_sf(data = count.site.dat.2020 %>% filter(legal.size == '<140 mm'), aes(fill = mean.ab.n), shape = 21, size = 2)+
    scale_fill_gradientn(colours = c("navyblue", "blue", "cyan", "green", "yellow", "orange", "red"),
                         limits = c(0, 150),
                         breaks = c(0, 50, 100, 150),
                         labels = c(0, 50, 100, 150))+
    theme_bw() +
    annotation_scale(location = "bl", width_hint = 0.5) +
    annotation_north_arrow(location = "br", which_north = "true", 
                           pad_x = unit(0.05, "cm"), pad_y = unit(0.1, "cm"),
                           style = north_arrow_fancy_orienteering)+
    labs(title = paste(samp.year - 1, ' Sub-legal <140 mm'), fill = (bquote('Average\ncount')))+
    xlab('Longitude')+
    scale_x_continuous(breaks = seq(140, 149, by = 0.1))+
    ylab('Latitude')+
   theme(plot.margin=unit(c(0.1, 0.1, -1, 0.1), "cm"))
  
  count.site.map.2020.legal <- ggplot(data = st_geometry(sf.tas.map.crop)) +
    geom_sf(data = sf.subblock.map.crop, aes(label = subblockno), fill = NA)+
    geom_sf_text(data = sf.subblock.map.crop, aes(label = subblockno))+
    geom_sf(fill = 'grey') +
    geom_sf(data = sites.not.surveyed.2020, shape = 5, size = 0.8, colour = 'red')+
    geom_sf(data = count.site.dat.2020 %>% filter(legal.size == '>140 mm'), aes(fill = mean.ab.n), shape = 21, size = 2)+
    scale_fill_gradientn(colours = c("navyblue", "blue", "cyan", "green", "yellow", "orange", "red"),
                         limits = c(0, 150),
                         breaks = c(0, 50, 100, 150),
                         labels = c(0, 50, 100, 150))+
    theme_bw() +
    annotation_scale(location = "bl", width_hint = 0.5) +
    annotation_north_arrow(location = "br", which_north = "true", 
                           pad_x = unit(0.05, "cm"), pad_y = unit(0.1, "cm"),
                           style = north_arrow_fancy_orienteering)+
    labs(title = paste(samp.year - 1, ' Legal >140 mm'), fill = (bquote('Average\ncount')))+
    xlab('Longitude')+
    scale_x_continuous(breaks = seq(140, 149, by = 0.1))+
    ylab('Latitude')+
   theme(plot.margin=unit(c(-1, 0.1, 0.1, 0.1), "cm"))
  
  # create map for 2021
  count.site.map.2021.sublegal <- ggplot(data = st_geometry(sf.tas.map.crop)) +
    geom_sf(data = sf.subblock.map.crop, aes(label = subblockno), fill = NA)+
    geom_sf_text(data = sf.subblock.map.crop, aes(label = subblockno))+
    geom_sf(fill = 'grey') +
    geom_sf(data = sites.not.surveyed.2021, shape = 5, size = 0.8, colour = 'red')+
    geom_sf(data = count.site.dat.2021 %>% filter(legal.size == '<140 mm'), aes(fill = mean.ab.n), shape = 21, size = 2)+
    scale_fill_gradientn(colours = c("navyblue", "blue", "cyan", "green", "yellow", "orange", "red"),
                         limits = c(0, 150),
                         breaks = c(0, 50, 100, 150),
                         labels = c(0, 50, 100, 150))+
    theme_bw() +
    annotation_scale(location = "bl", width_hint = 0.5) +
    annotation_north_arrow(location = "br", which_north = "true", 
                           pad_x = unit(0.05, "cm"), pad_y = unit(0.1, "cm"),
                           style = north_arrow_fancy_orienteering)+
    labs(title = paste(samp.year, ' Sub-legal <140 mm'), fill = (bquote('Average\ncount')))+
    xlab('Longitude')+
    scale_x_continuous(breaks = seq(140, 149, by = 0.1))+
    ylab('Latitude')+
   theme(plot.margin=unit(c(0.1, 0.1, -1, 0.1), "cm"))
  
  count.site.map.2021.legal <- ggplot(data = st_geometry(sf.tas.map.crop)) +
    geom_sf(data = sf.subblock.map.crop, aes(label = subblockno), fill = NA)+
    geom_sf_text(data = sf.subblock.map.crop, aes(label = subblockno))+
    geom_sf(fill = 'grey') +
    geom_sf(data = sites.not.surveyed.2021, shape = 5, size = 0.8, colour = 'red')+
    geom_sf(data = count.site.dat.2021 %>% filter(legal.size == '>140 mm'), aes(fill = mean.ab.n), shape = 21, size = 2)+
    scale_fill_gradientn(colours = c("navyblue", "blue", "cyan", "green", "yellow", "orange", "red"),
                         limits = c(0, 150),
                         breaks = c(0, 50, 100, 150),
                         labels = c(0, 50, 100, 150))+
    theme_bw() +
    annotation_scale(location = "bl", width_hint = 0.5) +
    annotation_north_arrow(location = "br", which_north = "true", 
                           pad_x = unit(0.05, "cm"), pad_y = unit(0.1, "cm"),
                           style = north_arrow_fancy_orienteering)+
    labs(title = paste(samp.year, ' Legal >140 mm'), fill = (bquote('Average\ncount')))+
    xlab('Longitude')+
    scale_x_continuous(breaks = seq(140, 149, by = 0.1))+
    ylab('Latitude')+
   theme(plot.margin=unit(c(-1, 0.1, 0.1, 0.1), "cm"))
  
  # extract common legend from maps
  plot.legend <- get_legend(count.site.map.2020.sublegal)  
  
  # remove legend from 2020 map
  no.legend.2020 <- count.site.map.2020.sublegal +
    theme(legend.position = "none")
  
  no.legend.2020.legal <- count.site.map.2020.legal +
    theme(legend.position = "none")
  
  # remove legend from 2021 map
  no.legend.2021 <- count.site.map.2021.sublegal +
    theme(legend.position = "none")
  
  no.legend.2021.legal <- count.site.map.2021.legal +
    theme(legend.position = "none")
  
  # combine plots with common legend (adjusting layout for blocks)
  count.site.map <- grid.arrange(no.legend.2020, no.legend.2021, plot.legend,
                                 no.legend.2020.legal, no.legend.2021.legal, 
                                   ncol = 3, nrow = 2, widths = c(2.5, 2.5, 0.8))

  # save plot
  setwd(ts.plots.folder)
  ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_SiteCountMap-',
  samp.year - 1, 'vs', samp.year, '_BlockNo_', i, '.pdf', sep = ''),
         plot = count.site.map, units = 'mm', width = 250, height = 300)
  ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_SiteCountMap-',
                          samp.year - 1, 'vs', samp.year, '_BlockNo_', i, '.png', sep = ''),
         plot = count.site.map, units = 'mm', width = 250, height = 300)
  
}

##---------------------------------------------------------------------------##
# above plot as facet wrap

sf.tas.map <- st_read("C:/CloudStor/Shared/DiveFisheries/GIS/SpatialLayers/TasLand.gpkg")
sf.subblock.map <- st_read("C:/CloudStor/Shared/DiveFisheries/GIS/SpatialLayers/SubBlockMaps.gpkg")

# transform maps to GDA2020

# set CRS
GDA2020 <- st_crs(7855)
GDA94 <- st_crs(28355)
WGS84 <- st_crs(4326)

sf.tas.map <- sf.tas.map %>% 
 st_set_crs(GDA2020)

sf.subblock.map <- st_transform(sf.subblock.map, GDA2020)

# join CPUE data to site location data

# identify unique sites sampled from timed swim dataframe and join to renamed site file
time.swim.sites <- std.ts.dat %>%
 # filter(sampdate > as.Date('2021-01-01')) %>%
 distinct(site, sampyear, .keep_all = T) %>% 
 select(c(site, blockno, subblockno, sampdate, actual.geom, sampyear)) %>% 
 st_as_sf() %>% 
 st_set_crs(GDA2020)

ten.min.mean.site <- std.ts.dat %>% 
 filter(!subblockno %in% c('28B', '28C')) %>% 
 group_by(site, blockno, subblockno, diver, sampyear, legal.size) %>% 
 summarise(ab.n = sum(sizeclass_freq)) %>% 
 group_by(site, blockno, subblockno, sampyear, legal.size) %>% 
 summarise(mean.ab.n = mean(ab.n)) %>% 
 # mutate(sampyear = factor(sampyear))%>% 
 as.data.frame()


time.swim.count.site.loc <- left_join(ten.min.mean.site, time.swim.sites) %>% 
 st_as_sf()

#create index of legal/sub-legal x sampyear

df.1 <- time.swim.count.site.loc %>% 
 unite(legal.size.year, sampyear, legal.size,  sep = '', remove = F)

# identify blocks sampled
blocks.sampled <- df.1 %>% 
 as.data.frame() %>% 
 filter(sampyear == samp.year) %>% 
 distinct(blockno) %>% 
 pull()

# identify sites not surveyed but proposed
ts.proposed.geom <- readRDS(paste(samp.year.folder, '/ts.proposed.geom.RDS', sep = ''))

# identify proposed sites not surveyed
not.surveyed.df <- ts.proposed.geom %>% 
 filter(sampled == '0')

i <- 16

# filter for blockno and sub-legal counts
count.site.dat.2020 <- df.1 %>% 
 filter(blockno == i &
         between(sampyear, samp.year - 1, samp.year)) %>% 
 st_as_sf

count.site.dat.2020$legal.size.year = factor(count.site.dat.2020$legal.size.year,
                                             levels = c(paste(samp.year - 1, '<140 mm', sep = ''),
                                                        paste(samp.year, '<140 mm', sep = ''),
                                                        paste(samp.year - 1, '>140 mm', sep = ''),
                                                        paste(samp.year, '>140 mm', sep = '')))

# count.site.dat.2021 <- df.1 %>% 
#  filter(blockno == i &
#          sampyear == samp.year) %>% 
#  st_as_sf

# filter subblock map for blockno
sf.subblock.map.crop <- sf.subblock.map %>%
 filter(blockno == i &
         !subblockno %in% c('28B', '28C'))  %>% 
 st_as_sf() 

# filter proposed sites not surveyed
sites.not.surveyed.2020 <- not.surveyed.df %>% 
 filter(blockno == i &
         between(sampyear, samp.year - 1, samp.year) &
         !subblockno %in% c('28B', '28C'))

# sites.not.surveyed.2021 <- not.surveyed.df %>% 
#  filter(blockno == i &
#          sampyear == samp.year &
#          !subblockno %in% c('28B', '28C'))

# crop tas map to blockno
sf.tas.map.crop <- st_crop(sf.tas.map, sf.subblock.map.crop)

# # crop tas and subblock map to spatial extent of all sites with a 1 km buffer
# df.1 <- bind_rows(count.site.dat.2020, count.site.dat.2021) %>% 
#   st_buffer(dist = 1000)
# sf.tas.map.crop <- st_crop(sf.tas.map, df.1)
# sf.subblock.map.crop <- st_crop(st_make_valid(sf.subblock.map), df.1)

# ggplot(data = st_geometry(df.2))+
#   geom_sf(data = df.3, aes(label = subblockno), fill = NA)+
#   geom_sf_text(data = df.3, aes(label = subblockno))

# create map for 2020
count.site.map.2020.sublegal <- ggplot(data = st_geometry(sf.tas.map.crop)) +
 geom_sf(data = sf.subblock.map.crop, aes(label = subblockno), fill = NA)+
 geom_sf_text(data = sf.subblock.map.crop, aes(label = subblockno))+
 geom_sf(fill = 'grey') +
 geom_sf(data = sites.not.surveyed.2020, shape = 5, size = 0.8, colour = 'red')+
 geom_sf(data = count.site.dat.2020, aes(fill = mean.ab.n), shape = 21, size = 2)+
 scale_fill_gradientn(colours = c("navyblue", "blue", "cyan", "green", "yellow", "orange", "red"),
                      limits = c(0, 150),
                      breaks = c(0, 50, 100, 150),
                      labels = c(0, 50, 100, 150))+
 theme_bw() +
 annotation_scale(location = "bl", width_hint = 0.5) +
 annotation_north_arrow(location = "br", which_north = "true", 
                        pad_x = unit(0.05, "cm"), pad_y = unit(0.1, "cm"),
                        style = north_arrow_fancy_orienteering)+
 xlab('Longitude')+
 scale_x_continuous(breaks = seq(140, 149, by = 0.1))+
 ylab('Latitude')+
 labs(fill = 'Average\ncount')+
 facet_wrap(~ legal.size.year, ncol = if_else(i == 16, 4, 2))

##---------------------------------------------------------------------------##

# PLOT 1: Diver deviation ####

# Load timed swim diver pair data
time.swim.divers <- read.xlsx("C:/CloudStor/Shared/DiveFisheries/Abalone/FISdata/FIS_TimedSwimSurveys2022/timed_swim_diver_details_2022.xlsx",
                              detectDates = T)

# Add end date for divers still participating
time.swim.divers <- time.swim.divers %>% 
 mutate(end.date = if_else(is.na(end.date), Sys.Date(), end.date))

# Summarise total count for site x blockno x sampyear x sampdate x diver x legal.size
ts.count.sum.2 <- std.ts.dat %>% 
 filter(!subblockno %in% c('28B', '28C')) %>%
 group_by(site, blockno, sampyear, sampdate, diver, legal.size) %>% 
 summarise(ab.n = sum(sizeclass_freq_10)) %>%
 as.data.frame()

# Add diver pairing details to summary data
ts.count.divers <-  fuzzy_left_join(ts.count.sum.2, time.swim.divers, 
                                    by = c("diver" = "diver", 
                                           "sampdate" = "start.date",
                                           "sampdate" = "end.date"),
                                    match_fun = list(`==`, `>=`, `<=`))

# Identify diver pairs x year
ts.count.divers %>% 
 group_by(sampyear, diver.x, dive.pair.id) %>%
 distinct(dive.pair.id) 

# Select sample year
samp.year <- c(2022)

# Identify diver pair IDs x chosen year (remove JM)
diver.ids <- ts.count.divers %>% 
 filter(sampyear == samp.year,
        dive.pair.id != 4) %>% 
 distinct(dive.pair.id) %>% 
 pull()

q <- list()

for (i in diver.ids){
 
 
 # Select data for dive pair and sample year
 ts.diver.dev.dat <- ts.count.divers %>% 
  filter(dive.pair.id == i, sampyear == samp.year) %>% 
  select(c(site, blockno, legal.size, sampyear, sampdate, diver.id, ab.n)) %>%  
  spread(key = diver.id, value = ab.n) %>%  
  mutate(dive.diff = abs(.[[6]] - .[[7]]))
 
 # Identify diver names for plot title
 dive.dev.divers <- ts.diver.dev.dat %>% 
  select(c(6,7))
 
 # determine number of sites surveyed by diver pair for plot
 divers.site.n <- ts.diver.dev.dat %>%
  filter(legal.size == '<140 mm') %>% 
  group_by(blockno) %>% 
  summarise(site.n = paste(n_distinct(site), sep = ''))
 
 q[[i]] <-
  # create plot
  dive.dev.plot <- ggplot(data = ts.diver.dev.dat, aes(x = blockno, y = dive.diff))+
  geom_boxplot(aes(fill = factor(legal.size)))+
  xlab('BlockNo')+
  ylab('Diver Total Count Deviation')+
  ylim(0, 90)+
  theme_bw()+
  ggtitle(paste(samp.year, 'Diver', names(dive.dev.divers[1]), 'vs',
                'Diver', names(dive.dev.divers[2])))+
  geom_text(data = divers.site.n, aes(y = 90, label = site.n), size = 3)+
  scale_fill_manual(values = c("#999999", "#56B4E9"))+
  theme(legend.title = element_blank(),
        legend.position = c(0.15, 0.8))
  # theme(legend.title = element_blank(),
  #       legend.position = ifelse(names(dive.dev.divers) == '1' &
  #                                 names(dive.dev.divers) == '5' &
  #                                 samp.year == 2020, c(0.1, 0.8), "none"))
 
}

dive.dev.plot <- q %>% 
 discard(is.null) %>% 
 cowplot::plot_grid(plotlist = .)



# save plot
setwd(ts.plots.folder)
ggsave(filename = paste('TimedSwimSurvey_DiverDeviation_', samp.year, '.pdf', sep = ''),
       plot = dive.dev.plot, units = 'mm', width = 210, height = 200)

ggsave(filename = paste('TimedSwimSurvey_DiverDeviation_', samp.year, '.png', sep = ''),
       plot = dive.dev.plot, units = 'mm', width = 210, height =200)


##---------------------------------------------------------------------------##

## NOTE FOR LACHIE - STOP HERE

## These are additional summary plots for final report examining present day with
# available historical data.


##---------------------------------------------------------------------------##
## 2021 site selection ####

# Randomly select 15 sites from each block to keep as reference sites for 2021 survey and
# beyond. Determine hexcell IDs to filter from random generation of new sites from
# GPS logger data in 2021.

set.seed(123) 
sites.2020.keep <- time.swim.dat.final %>%
        filter(sampyear == 2020,
               !subblockno %in% c('28B', '28C')) %>% 
        group_by(blockno) %>% 
        distinct(site, .keep_all = T) %>% 
        sample_n(15) %>% 
        select(-proposed.geom) %>% 
        st_as_sf()

# save GIS spatial layer
# st_write(sites.2020.keep,
#          dsn = "C:/Users/jaimem/Documents/Abalone_research/AB_TimedSwimFIS/FIS_TIMEDSWIMSITES_2020REFERENCE_2021-05-17.gpkg",
#          layer = "sites.2020.keep", driver = "GPKG", overwrite = T, delete_dsn = T)

# NOTE: set.seed wasn't used when generating the selection of reference sites from 2020 to use in 2021.
# Therefore the resulting output gpkg file above has been re-imported to identify those reference sites
# and stored as an RDS for future reference.

sites.2020.kept.sf <- st_read("C:/Users/jaimem/OneDrive - University of Tasmania/Documents/AB_TimedSwimFIS/FIS_TIMEDSWIMSITES_2020REFERENCE_2021-05-17.gpkg")

# convert to data frame
sites.2020.kept.df <- sites.2020.kept.sf %>%
  sfheaders::sf_to_df(fill = T)

# save file
saveRDS(sites.2020.kept.df, 'C:/CloudStor/Shared/DiveFisheries/Abalone/FISdata/FIS_TimedSwimSurveys2021/time.swim.2020.repeat.sites.RDS')
sites.2020.kept.df <- readRDS('C:/CloudStor/Shared/DiveFisheries/Abalone/FISdata/FIS_TimedSwimSurveys2021/time.swim.2020.repeat.sites.RDS')

# create vector of oid to be sampled in 2021 from 2020 sites sampled
# Note: 12 of the reference sites are SAM historical research sites (i.e. n = 78) 
sites.2020.keep <- sites.2020.kept.df
sites.2020.keep.oid <- sites.2020.keep %>% 
        filter(!is.na(oid)) %>% 
        ungroup() %>% 
        pull(oid)

# create vector of all oid sampled in 2020 to filter from 2021 random site selection
sites.2020.oid <- time.swim.dat.final %>%
        filter(!subblockno %in% c('28B', '28C'),
               sampdate < as_date('2021-01-01')) %>% 
        group_by(blockno) %>% 
        distinct(site, .keep_all = T) %>% 
        filter(!is.na(oid)) %>% 
        ungroup() %>% 
        pull(oid)


##---------------------------------------------------------------------------##
#Excel file of raw site data with gps positions

# read GPX file from plotter
morana.gps <- st_read('C:/Users/jaimem/Documents/Abalone_research/AB_TimedSwimFIS/MORANAII-2020-10-09_download.gpx', layer = 'waypoints')

# separate start positions
df.start <- time.swim.meta.dat %>%
  select(-c(waypoint.finish, finishtime)) %>% 
  dplyr::rename('waypoint' = waypoint.start,
                'samptime' = starttime) %>% 
  mutate(sampperiod = 'start')

# separate finish positions
df.finish <- time.swim.meta.dat %>%
  select(-c(waypoint.start, starttime)) %>% 
  dplyr::rename('waypoint' = waypoint.finish,
                'samptime' = finishtime) %>% 
  mutate(sampperiod = 'finish')

# re-join start and finish positions  and remove non-sampled sites        
df.start.finish <- bind_rows(time.swim.site.start, time.swim.site.finish) %>%
  mutate(waypoint = if_else(waypoint < 100, as.character(paste(0, waypoint, sep = '')), as.character(waypoint))) %>%  
  # select(c(sampdate, samptime, sampperiod, site, waypoint)) %>% 
  filter(site != 'Betsey')

# join geometry where start waypoint recorded as being on the original GPS position/waypoint mark
df.site.start.finish.mark <- df.start.finish %>% 
  filter(is.na(waypoint) & sampperiod == 'start') %>% 
  left_join(., morana.gps, by = c('site' = 'name')) %>% 
  dplyr::rename('gpstime' = time,
                'site' = site)
# select(c(sampdate, samptime, gpstime, sampperiod, site, waypoint, geometry))

# join geometry where waypoints were recorded 
df.site.start.finish.wp <- df.start.finish %>% 
  filter(!is.na(waypoint)) %>% 
  left_join(., morana.gps, by = c('waypoint' = 'name')) %>% 
  dplyr::rename('gpstime' = time,
                'site' = site) 
# select(c(sampdate, samptime, gpstime, sampperiod, site, waypoint, geometry))

# re-join all waypoint data
df.site.start.finish.loc <- bind_rows(df.site.start.finish.mark, df.site.start.finish.wp) %>% 
  st_as_sf() %>%
  mutate(lat = unlist(map(geometry,1)),
         long = unlist(map(geometry,2))) %>% 
  select(c("sampdate", "site", "divers", "samptime", "waypoint",
           "habitat.type", "percent.algae.cover", "percent.urchins",
           "comments", "sampperiod", "sym", 'lat', 'long')) %>% 
  as.data.frame() %>% 
  select(-c('geometry'))

# transform to GDA2020
# df.site.start.finish.loc <- st_transform(df.site.start.finish.loc, GDA2020) %>% 
#         as.data.frame()

setwd('C:/Users/jaimem/Documents/Abalone_research/AB_TimedSwimFIS')
write.xlsx(df.site.start.finish.loc,
           file = paste('TimedSwimSurveyMetaData_2020_', Sys.Date(), '.xlsx'),
           sheetName = "Sheet1",
           col.names = TRUE,
           row.names = TRUE,
           append = FALSE)
##---------------------------------------------------------------------------##
## PLOT 13: CPUE KG.HR ####
## estimate CPUE in kg/hr for each block using length-weight relationships from commercial catch sampling 
## to convert timed swim lengths to weight

# load most recent commercial catch sampling compiled MM dataframe
compiledMM.df.final <- readRDS('C:/CloudStor/R_Stuff/MMLF/compiledMM.df.final.RDS')

# select length-weight data, removing obvious erroneous data or data collected from multiple blocks in a trip
lw.dat <- compiledMM.df.final %>% 
 filter(between(whole.weight, 200, 1500) & 
         between(shell.length, sizelimit - 5, 220) & 
         !(shell.length > 175 & whole.weight < 600), 
        !(shell.length > 180 & whole.weight < 1000) &
         numblocks == 1,
        species == 1) 

# calculate log of length and weight
lw.dat.log <- lw.dat %>% 
 mutate(log.sl = log(shell.length),
        log.wt = log(whole.weight))

# calculate regression coefficient summary table for each blockno
lw.dat.coeff <- lw.dat.log %>%
 nest(data = -blockno) %>% 
 mutate(fit = map(data, ~ lm(log.wt ~ log.sl, data = .x)),
        tidied = map(fit, broom::tidy)) %>% 
 unnest(tidied) %>%   
 filter(term %in% c('(Intercept)', 'log.sl')) %>% 
 select(c(blockno, estimate, term)) %>% 
 as.data.frame() %>% 
 spread(., term, estimate) %>%  
 dplyr::rename(b = 'log.sl',
               intercept = "(Intercept)") %>%  
 mutate(a = exp(intercept)) %>% 
 select(-intercept)

# determine sample size for each blockno and join to regression summaries
lw.dat.n <- lw.dat.log %>% 
 group_by(blockno) %>% 
 summarise(n = n())

lw.dat.coeff.blockno <- left_join(lw.dat.coeff, lw.dat.n)

# select regression parameters to use for estimating weight
# block 13 used across all blocks given no length-weight data has been collected from
# the closed blocks since the re-commencement of commercial catch sampling in 2018. 
lw.coeff <- lw.dat.coeff.blockno %>% 
 filter(blockno == 13) %>% 
 select(a, b) %>% 
 mutate(join.id = 1)

# join chosen regression parameters to timed swim data
time.swim.dat.df.lw <- std.ts.dat %>% 
 mutate(join.id = 1) %>% 
 left_join(., lw.coeff)

# saveRDS(time.swim.dat.df.lw, 'C:/Users/jaimem/Documents/Abalone_research/AB_TimedSwimFIS/TimedSwimData_LW_2020-09-16.RDS')

# estimate total weight of each sizeclass above 140 mm
time.swim.dat.df.wt <- time.swim.dat.df.lw %>% 
 filter(midsize >= 150) %>%
 mutate(est.weight = ((a * (midsize ^ b)) * sizeclass_freq) / 1000)

# quick summary of total kg estimate by block
time.swim.dat.df.wt %>%
 filter(!subblockno %in% c('28B', '28C')) %>%
 dplyr::group_by(sampyear, blockno) %>% 
 dplyr::summarise(total.kg = round(sum(est.weight), digits = 2)) %>% 
 as.data.frame()

# estimate CPUE for each site
time.swim.cpue.site <- time.swim.dat.df.wt %>%
 filter(!subblockno %in% c('28B', '28C')) %>%
 dplyr::group_by(blockno, site, diver, sampyear) %>% 
 dplyr::summarise(total.kg = round(sum(est.weight), digits = 2),
                  est.kg.hr = round((total.kg / max(time.elapsed)) * 60, digits = 2)) %>%   
 group_by(blockno, site, sampyear) %>% 
 summarise(est.kg.hr = round(mean(est.kg.hr), digits = 2)) %>% 
 as.data.frame()

# estimate CPUE for each blockno
time.swim.cpue.blockno <- time.swim.cpue.site %>%
 group_by(blockno, sampyear) %>% 
 summarise(est.kg.hr = round(mean(est.kg.hr), digits = 2)) %>% 
 as.data.frame()

# determine number of sites sampled for each blockno
time.swim.cpue.n <- time.swim.cpue.site %>% 
 group_by(blockno, sampyear) %>% 
 summarise(n = n_distinct(site))

# plot CPUE estimate for each blocnkno between years
cpue.plot <- time.swim.cpue.site %>% 
 ggplot(aes(x = blockno, y = est.kg.hr)) +
 geom_boxplot(aes(fill = as.factor(sampyear)), position = position_dodge2(1, preserve = 'single'),
              outlier.colour = '#EE8866') +
 scale_fill_manual(values = c("#77AADD", "#BBCC33"))+
 stat_summary(aes(group = as.factor(sampyear)), fun.y = mean, 
              geom = 'point', shape = 19, size = 2, colour = 'red', fill = 'red',
              position = position_dodge2(0.8))+
 theme_bw() +
 ylim(0, 200)+
 ylab(bquote('Estimated CPUE ('*~kg.hr^-1*')'))+
 xlab('Blockno')+
 geom_text(data = time.swim.cpue.n, aes(y = 200, label = n, 
                                        colour = factor(sampyear, levels = c('2020', '2021'))), size = 3, position = position_dodge2(0.8))+
 scale_colour_manual(values = c("#77AADD", "#BBCC33"))+
 guides(size = 'legend', colour = 'none',
        fill = guide_legend(title = 'Year'))

setwd('C:/CloudStor/Shared/DiveFisheries/Abalone/FISdata/FIS_TimedSwimSurveys2021/FIS_TimedSwimSurvey2021_Plots')
ggsave(filename = paste('TimedSwimSurvey_2021_CPUEPlot', '.pdf', sep = ''), 
       plot = cpue.plot, units = 'mm', width = 190, height = 120)
ggsave(filename = paste('TimedSwimSurvey_2021_CPUEPlot', '.png', sep = ''), 
       plot = cpue.plot, units = 'mm', width = 190, height = 120)

##---------------------------------------------------------------------------##
## PLOT 14: CPUE vs TS ####
## plot timed swim average counts for legal and sub-leagl
## against historical fishery CPUE for each site

ts.vs.cpue.plot <- std.ts.dat %>% 
 filter(!subblockno %in% c('28B', '28C'),
        sampdate > as.Date('2021-01-01')) %>% 
 group_by(site, diver, legal.size, cell.ntile, sam.count, cpue.kg.hr) %>% 
 summarise(ab.n = sum(sizeclass_freq_10)) %>% 
 group_by(site, legal.size, cell.ntile, sam.count, cpue.kg.hr) %>% 
 summarise(mean.ab.n = mean(ab.n))%>%  
 filter(!is.na(cell.ntile)) %>%
 ggplot(aes(x = cpue.kg.hr, y = mean.ab.n))+
 geom_point(aes(colour = cell.ntile), size = 3)+
 geom_smooth(method = 'lm', formula = y~x, se = T)+
 stat_poly_eq(formula = y~x, aes(label = paste(..rr.label.., p.value.label, sep = "~~~")), 
              parse = TRUE, label.y = 0.95) +
 theme_bw()+
 xlab(bquote('CPUE ('*~kg.hr^-1*')'))+
 ylab(bquote('Timed Swim average count (abalone.10'*~min^-1*')'))+
 labs(colour = 'Rank')+
 ylim(0, 160)+
 theme(legend.position = c(0.95, 0.85))+
 facet_wrap(~legal.size, ncol = 2)+
 theme(strip.background = element_blank(),
       strip.text.x = element_text(size = 12, face = 'bold'))

setwd('C:/CloudStor/Shared/DiveFisheries/Abalone/FISdata/FIS_TimedSwimSurveys2021/FIS_TimedSwimSurvey2021_Plots')
ggsave(filename = paste('TimedSwimSurvey_2021_TenMinuteCountvsCPUE.kg.hr', '.pdf', sep = ''), 
       plot = ts.vs.cpue.plot, units = 'mm', width = 190, height = 120)
ggsave(filename = paste('TimedSwimSurvey_2021_TenMinuteCountvsCPUE.kg.hr', '.png', sep = ''), 
       plot = ts.vs.cpue.plot, units = 'mm', width = 190, height = 120)

##---------------------------------------------------------------------------##
##PLOT 10: SAM vs Timed count ####
## Boxplot comparing average counts from historical SAM data and recent
## time swim survey data as block x year

# Load original SAM count data and adjusted/renamed site details 
fis.sam.data <- read.xlsx("C:/Users/jaimem/OneDrive - University of Tasmania/Documents/AB_TimedSwimFIS/SampleDetailsforTimedSwimV2_JM.xlsx",
                          detectDates = T)

ts.site.sam.dat <- readRDS('C:/CloudStor/Shared/DiveFisheries/Abalone/FISdata/FIS_TimedSwimSurveys2021/ts.site.sam.2020.join.RDS')

colnames(fis.sam.data) <- tolower(colnames(fis.sam.data))

# Join original SAM count and site data to get final time swim survey site names 
sam.dat <- fis.sam.data %>% 
 filter(stat.block %in% c(16, 22, 23, 24, 27, 28)) %>% 
 left_join(., ts.site.sam.dat %>% select(c(site.id, site)), by = c('site.id' = 'site.id')) %>% 
 dplyr::rename(site.sam = site.y) %>%   
 mutate(sampyear = year(date)) %>% 
 filter(sampyear >= 2001) %>% 
 dplyr::rename(sampdate = date,
               blockno = stat.block,
               ab.n = avg_count_per.10min,
               diver = collector) %>% 
 select(c(blockno, site.sam, diver, ab.n, sampyear)) %>% 
 filter(!is.na(ab.n))

# Quick plot examining counts by site in each block by year
sam.dat %>% 
 ggplot(aes(x = as.factor(blockno), y = ab.n))+
 geom_jitter(aes(colour = sampyear), width = 0.2, size = 3)+
 scale_color_gradientn(colours = rainbow(11))

# Select latest time swim survey data from standardised data
ts.dat <- std.ts.dat %>% 
 filter(!subblockno %in% c('28B', '28C')) %>%
 group_by(sampyear, blockno, site, diver) %>% 
 summarise(ab.n = sum(sizeclass_freq)) %>% 
 mutate(blockno = as.numeric(blockno))

# Combine SAM and latest time swim survey data
ts.sam.dat <- bind_rows(sam.dat, ts.dat) %>% 
 mutate(site = if_else(!is.na(site), site, site.sam))

# Create plot 
ts.sam.count.plot <- ts.sam.dat %>%
 filter(!blockno %in% c(13, 14, 29, 30)) %>% 
 group_by(blockno, sampyear, site) %>% 
 ggplot(aes(x = as.factor(blockno), y = ab.n, fill = as.factor(sampyear)))+
 geom_boxplot(position = position_dodge2(preserve = "single"))+
 theme_bw()+
 ylab(bquote('Average count (abalone.10'*~min^-1*')'))+
 xlab('Blockno')+
 geom_text(data = ts.sam.dat %>%
            filter(!blockno %in% c(13, 14, 29, 30)) %>%
            group_by(blockno, sampyear) %>% 
            summarise(n = n_distinct(site), lab.pos = max(ab.n) + 1),
           aes(y = lab.pos, label = paste0(n, '\n')),
           position = position_dodge2(width = 0.75, preserve = 'single'),
           size = 2)+
 labs(fill = 'Year')+
 scale_fill_viridis(discrete = TRUE)

# Save plot
setwd('C:/CloudStor/Shared/DiveFisheries/Abalone/FISdata/FIS_TimedSwimSurveys2021/FIS_TimedSwimSurvey2021_Plots')
ggsave(filename = paste('TimedSwimSurvey_2020_TenMinuteCountHistoricPlot', '.pdf', sep = ''), 
       plot = ts.sam.count.plot, units = 'mm', width = 190, height = 120)
ggsave(filename = paste('TimedSwimSurvey_2020_TenMinuteCountHistoricPlot', '.png', sep = ''), 
       plot = ts.sam.count.plot, units = 'mm', width = 190, height = 120)

##---------------------------------------------------------------------------##
## PLOT 11: TIMED VS HISTORIC ####
## compare average timed swim counts with available historical count data at SAM 
## research sites and the rating score used to select timed swim sites from GPS logger data

## average count vs cpue rating score
time.swim.dat.vs.cpue.plot <- std.ts.dat %>% 
 filter(!subblockno %in% c('28B', '28C'),
        sampdate < as.Date('2022-01-01')) %>% 
 group_by(site, diver, legal.size, cell.ntile, sam.count) %>% 
 summarise(ab.n = sum(sizeclass_freq)) %>% 
 group_by(site, legal.size, cell.ntile, sam.count) %>% 
 summarise(mean.ab.n = mean(ab.n))%>% 
 filter(!is.na(cell.ntile)) %>%  
 ggplot(aes(x = cell.ntile, y = mean.ab.n, colour = legal.size))+
 geom_point(size = 3)+
 scale_colour_manual(values = c("#999999", "#56B4E9"))+
 theme_bw()+
 ylab(bquote('Timed Swim average count (abalone.10'*~min^-1*')'))+
 xlab('HexCell Rank')+
 ylim(0, 150)+
 # geom_text(data = time.swim.dat.n, aes(y = 200, label = n), color = 'black', size = 3)+
 theme(legend.position = c(0.9, 0.9),
       legend.title = element_blank())



## average count vs historical SAM counts
time.swim.dat.vs.sam.plot <- std.ts.dat %>%
 filter(!subblockno %in% c('28B', '28C'),
        sampdate < as.Date('2022-01-01')) %>% 
 group_by(site, diver, legal.size, cell.ntile, sam.count) %>% 
 summarise(ab.n = sum(sizeclass_freq)) %>% 
 group_by(site, legal.size, cell.ntile, sam.count) %>% 
 summarise(mean.ab.n = mean(ab.n)) %>% 
 filter(!is.na(sam.count) &
         sam.count <= 60) %>% #remove outlier from SAM data
 ggplot(aes(x = sam.count, y = mean.ab.n, colour = legal.size))+
 geom_point(size = 3)+
 scale_colour_manual(values = c("#999999", "#56B4E9"))+
 geom_smooth(method = 'lm', formula = y~x, se = F)+
 stat_poly_eq(formula = y~x, aes(label = paste(..rr.label.., p.value.label, sep = "~~~")), 
              parse = TRUE) +
 theme_bw()+
 ylab(bquote('Timed Swim average count (abalone.10'*~min^-1*')'))+
 xlab(bquote('SAM average count (abalone.10'*~min^-1*')'))+
 ylim(0, 125)+
 # geom_text(data = time.swim.dat.n, aes(y = 200, label = n), color = 'black', size = 3)+
 theme(legend.position = c(0.9, 0.9),
       legend.title = element_blank())

setwd('C:/CloudStor/Shared/DiveFisheries/Abalone/FISdata/FIS_TimedSwimSurveys2021/FIS_TimedSwimSurvey2021_Plots')
ggsave(filename = paste('TimedSwimSurvey_2021_TenMinuteCountvsCPUE', '.pdf', sep = ''), 
       plot = time.swim.dat.vs.cpue.plot, units = 'mm', width = 190, height = 120)
ggsave(filename = paste('TimedSwimSurvey_2021_TenMinuteCountvsCPUE', '.png', sep = ''), 
       plot = time.swim.dat.vs.cpue.plot, units = 'mm', width = 190, height = 120)
ggsave(filename = paste('TimedSwimSurvey_2021_TenMinuteCountvsSAM', '.pdf', sep = ''), 
       plot = time.swim.dat.vs.sam.plot, units = 'mm', width = 190, height = 120)
ggsave(filename = paste('TimedSwimSurvey_2021_TenMinuteCountvsSAM', '.png', sep = ''), 
       plot = time.swim.dat.vs.sam.plot, units = 'mm', width = 190, height = 120)

##---------------------------------------------------------------------------##
## PLOT 4: Counts x year x block 13 ####
# Compare total counts of size class at block 13 sites pre and post fishing in 2021

# Set size class
plot.size.class <- '>140 mm'

df.3 <- std.ts.dat %>% 
 filter(blockno == 13) %>%
 group_by(blockno, site, sampyear, sampdate, legal.size) %>% 
 summarise(ab.n = sum(sizeclass_freq_10)) %>% 
 group_by(blockno, site, sampyear, sampdate, legal.size) %>% 
 group_by(site) %>%   
 filter(n() > 2) %>% 
 mutate(status = ifelse(sampdate <= as.Date('2021-03-31'), 'closed', 'open'))

plot.closed <- df.3 %>% 
 filter(status == 'closed', legal.size == plot.size.class) %>%  
 ggplot(aes(x = reorder(site, -ab.n), y = ab.n))+
 geom_bar(stat = 'identity')+
 ylim(0, 150)+
 xlab('Site')+
 ylab(bquote('Total count (abalone.10'*~min^-1*')'))+
 ggtitle(paste('Closed ', plot.size.class))+
 theme_bw()+
 theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

plot.open <- df.3 %>% 
 filter(status == 'open', legal.size == plot.size.class) %>% 
 ggplot(aes(x = reorder(site, -ab.n), y = ab.n))+
 geom_bar(stat = 'identity')+
 ylim(0, 150)+
 xlab('Site')+
 ylab(bquote('Total count (abalone.10'*~min^-1*')'))+
 ggtitle(paste('Open ', plot.size.class))+
 theme_bw()+
 theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

block.13.plot <- grid.arrange(plot.closed, plot.open, nrow = 1)

# save plot
setwd('C:/CloudStor/Shared/DiveFisheries/Abalone/FISdata/FIS_TimedSwimSurveys2021/FIS_TimedSwimSurvey2021_Plots')
ggsave(filename = paste('TimedSwimSurvey_Block13_OpenvsClosed2021', '.pdf', sep = ''),
       plot = block.13.plot, units = 'mm', width = 190, height = 120)

ggsave(filename = paste('TimedSwimSurvey_Block13_OpenvsClosed2021', '.png', sep = ''),
       plot = block.13.plot, units = 'mm', width = 190, height = 120)

        