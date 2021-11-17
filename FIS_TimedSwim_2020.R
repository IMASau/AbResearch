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
})

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

# save files
saveRDS(time.swim.dat, 'C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/time.swim.dat.RDS')
saveRDS(time.swim.dat.df, 'C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/time.swim.dat.df.RDS')
saveRDS(time.swim.meta.dat, 'C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/time.swim.meta.dat.RDS')

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
morana.gps.2020 <- st_read('C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/MORANAII-2020-10-09_download.gpx', layer = 'waypoints')

# vessel data for 2021 reference site surveys (i.e. block 13-14)
morana.gps.ref <- st_read('C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/MORANAII-2021-04-06_download.gpx', layer = 'waypoints')

# vessel data for 2021 surveys
morana.gps.2021.a <- st_read('C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/MORANAII-2021-07-15_download.gpx', layer = 'waypoints')

morana.gps.2021.b <- st_read('C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/MORANAII-2021-08-12_download.gpx', layer = 'waypoints')

morana.gps.2021.c <- st_read('C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/MORANAII-2021-09-08_download.gpx', layer = 'waypoints')

morana.gps.2021.d <- st_read('C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/MORANAII-2021-10-07_download.gpx', layer = 'waypoints')

taroona.gps.2021.a <- st_read('C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/TAROONA-2021-10-15_download.gpx', layer = 'waypoints')


morana.gps.2021 <- bind_rows(morana.gps.2021.a, morana.gps.2021.b, morana.gps.2021.c, 
                             morana.gps.2021.d) %>% 
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

# join GPX files
vessel.gps <- bind_rows(morana.gps.2020, morana.gps.ref, morana.gps.2021, taroona.gps.2021) %>%  
        mutate(gpsdate = as.Date(time),
               gpstime = time) %>% 
        select(c(name, sampyear, gpstime, gpsdate, geometry, vesselname))

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
saveRDS(vessel.gps.dat, 'C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/vessel.gps.dat.RDS')

# save spatial layer for QGIS
st_write(vessel.gps.dat,
         dsn = "C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/vessel.gps.dat_2021-08-10.gpkg",
         layer = "vessel.gps.dat", driver = "GPKG", overwrite = T, delete_dsn = T)


##---------------------------------------------------------------------------##
## 6. Sites sampled - actual ####
## create map of sites sampled within each Block with recorded GPS positions and 
## add subblock to site name based on vessel GPS data and start position
## use these data for 'actual geometry' when joining to raw data 

vessel.gps.dat <- readRDS('C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/vessel.gps.dat.RDS')

# read in Subblock map as an sf::sfc polygon object
sf.subblock.map <- st_read("C:/Users/jaimem/Dropbox/AbaloneData/SpatialLayers/SubBlockMaps.gpkg")

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


# extract reference sites
df.1 <- site.samp.start.loc %>% 
    filter(blockno %in% c(13, 14) &
               sampperiod == 'start')

WGS84 <- st_crs(4326)
df.2 <- st_transform(df.1, WGS84) %>% 
    st_coordinates()

st_write(df.1,
         dsn = "C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/block13-14_refsites.gpkg",
         layer = "df.1", driver = "GPKG", overwrite = T, delete_dsn = T)



# load proposed site file
ts.sites.final.sf <- readRDS('C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/ts.sites.final.sf.RDS')

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
saveRDS(ts.actual.geom, 'C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/ts.actual.geom.RDS')

# create maps for each block (#note: sample year and/or reference sites)

# identify blocks sampled
blocks.sampled <- unique(site.samp.start.subblock.loc.ref$blockno)

# select sampling year
samp.year <- 2021

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
        
        setwd('C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/FIS_TimedSwimSurvey2021_Plots')
        ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_SitesSampled_GPS_BlockNo_', i, '.pdf', sep = ''),
               plot = site.samp.map.blockno, units = 'mm', width = 190, height = 250)
        ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_SitesSampled_GPS_BlockNo_', i, '.png', sep = ''),
               plot = site.samp.map.blockno, units = 'mm', width = 190, height = 150)
        
}

# save spatial layer for QGIS
st_write(ts.actual.geom, 
         dsn = "C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/ts.actual.geom_2021-08-10.gpkg", 
         layer = "ts.actual.geom", driver = "GPKG", overwrite = T, delete_dsn = T)

##---------------------------------------------------------------------------##
## 7. Sites sampled - proposed ####
## identify sites sampled based on proposed GPS site data and
## create map of sites sampled within each Block adding subblock to site name 
## based on proposed position
## use these data for 'proposed geometry' when joining to raw data 

# load proposed site file
ts.sites.final.sf <- readRDS('C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/ts.sites.final.sf.RDS')

time.swim.dat <- readRDS('C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/time.swim.dat.RDS')


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
# compile joint site files
ts.dat.site.join <- bind_rows(ts.sites.join.2020, ts.sites.join.2021)

# identity sites sampled 
ts.dat.site.sampled <- ts.dat.site.join %>% 
        mutate(sampled = ifelse(is.na(sampdate), 0, 1)) %>%  
        select(c(site, site.old, sampled, sampyear, geometry))

# transform to GDA2020
GDA2020 <- st_crs(7855)
ts.dat.site.sampled <- st_transform(ts.dat.site.sampled, GDA2020)

# read in Subblock map as an sf::sfc polygon object
sf.subblock.map <- st_read("C:/Users/jaimem/Dropbox/AbaloneData/SpatialLayers/SubBlockMaps.gpkg")

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
saveRDS(ts.proposed.geom, 'C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/ts.proposed.geom.RDS')

# create maps for each block (#note: sample year and/or reference sites)

# identify blocks sampled
blocks.sampled <- unique(site.samp.loc$blockno)

# select sampling year
samp.year <- 2021

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
        
        setwd('C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/FIS_TimedSwimSurvey2021_Plots')
        ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_SitesSampled_Proposed_BlockNo_', i, '.pdf', sep = ''),
               plot = site.samp.map.blockno, units = 'mm', width = 190, height = 250)
        ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_SitesSampled_Proposed_BlockNo_', i, '.png', sep = ''),
               plot = site.samp.map.blockno, units = 'mm', width = 190, height = 150)
        
}


# save spatial layer for QGIS
st_write(ts.proposed.geom, 
         dsn = "C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/ts.proposed.geom_2021-08-09.gpkg", 
         layer = "ts.proposed.geom", driver = "GPKG", overwrite = T, delete_dsn = T)

##---------------------------------------------------------------------------##
## 8. Final data frame ####
## Join spatial site data (proposed and actual GPS sites) to count data and save final dataframe

time.swim.dat <- readRDS('C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/time.swim.dat.RDS')
time.swim.dat.df <- readRDS('C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/time.swim.dat.df.RDS')
ts.actual.geom <- readRDS('C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/ts.actual.geom.RDS')
ts.proposed.geom <- readRDS('C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/ts.proposed.geom.RDS')

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
        
        ts.dat.prop <- bind_rows(ts.dat.prop.2020, ts.dat.prop.2021) %>%   
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
        
        ts.dat.act <- bind_rows(ts.dat.act.2020, ts.dat.act.2021) %>%    
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

ts.dat.prop.df <- bind_rows(ts.dat.prop.df.2020, ts.dat.prop.df.2021) %>%
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

ts.dat.act.df <- bind_rows(ts.dat.act.df.2020, ts.dat.act.df.2021) %>%
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
ts.site.cpue.2020.join <- readRDS('C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/ts.site.cpue.2020.join.RDS')
ts.site.cpue.2021.join <- readRDS('C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/ts.site.cpue.2021.join.RDS')
ts.site.cpue.join <- bind_rows(ts.site.cpue.2020.join, ts.site.cpue.2021.join)

# load sam data and remove duplicate sites with same site name
ts.site.sam.join <- readRDS('C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/ts.site.sam.2020.join.RDS')
ts.site.sam.join <- ts.site.sam.join %>% 
        distinct(site, .keep_all = T)

# join oid and sam data to final size frequency data
time.swim.dat.final <- left_join(time.swim.dat.act.prop, select(ts.site.cpue.join, site, site.old, sampyear, oid, cell.ntile), by = c('site', 'sampyear')) %>%
        left_join(., select(ts.site.sam.join, site, sampyear, site.old, sam.count), by = c('site' = 'site')) %>%
    dplyr::rename(sampyear = 'sampyear.x') %>% 
        select(-c(site.old.x, geometry.x, sampyear.y, site.old.y, geometry.y))

# join oid and sam data to final individual size data
time.swim.dat.df.final <- left_join(time.swim.dat.df.act.prop, select(ts.site.cpue.join, site, site.old, sampyear, oid, cell.ntile), by = c('site', 'sampyear')) %>%
    left_join(., select(ts.site.sam.join, site, sampyear, site.old, sam.count), by = c('site' = 'site')) %>% 
    dplyr::rename(sampyear = 'sampyear.x') %>% 
    select(-c(site.old.x, geometry.x, sampyear.y, site.old.y, geometry.y))


saveRDS(time.swim.dat.final, 'C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/time.swim.dat.final.RDS')
saveRDS(time.swim.dat.df.final, 'C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/time.swim.dat.df.final.RDS')

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
rm(list = ls())

# Import final dataframes 
time.swim.dat.final <-
        readRDS('C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/time.swim.dat.final.RDS')

time.swim.dat.df.final <-
        readRDS('C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/time.swim.dat.df.final.RDS')

##---------------------------------------------------------------------------##
## 10.1 Summarise counts #### 

# Standardise counts for 10 minute swim
df.1 <- time.swim.dat.final %>% 
    mutate(sizeclass_freq_10 = round((sizeclass_freq / time.elapsed) * 10))

# Summarise total count for blockno x site x sampyear x legal.size
df.2 <- df.1 %>% 
    filter(!subblockno %in% c('28B', '28C')) %>%
    group_by(blockno, site, sampyear, legal.size) %>% 
    summarise(ab.n = sum(sizeclass_freq_10)) %>% 
    group_by(blockno, site, sampyear, legal.size) %>% 
    group_by(site)
##---------------------------------------------------------------------------##
# PLOT: Diver deviation ####

# Load timed swim diver pair data
time.swim.divers <- read.xlsx("C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/timed_swim_diver_details_2021.xlsx",
                                detectDates = T)

# Add end date for divers still participating
time.swim.divers <- time.swim.divers %>% 
  mutate(end.date = if_else(is.na(end.date), Sys.Date(), end.date))

# Summarise total count for site x blockno x sampyear x sampdate x diver x legal.size
df.5 <- df.1 %>% 
  filter(!subblockno %in% c('28B', '28C')) %>%
  group_by(site, blockno, sampyear, sampdate, diver, legal.size) %>% 
  summarise(ab.n = sum(sizeclass_freq_10)) %>%
  as.data.frame()

# Add diver pairing details to summary data
df.7 <-  fuzzy_left_join(df.5, time.swim.divers, 
                          by = c("diver" = "diver", 
                                 "sampdate" = "start.date",
                                 "sampdate" = "end.date"),
                          match_fun = list(`==`, `>=`, `<=`))

# Identify diver pair IDs
df.7 %>% 
  group_by(sampyear, diver.x, dive.pair.id) %>%  
  distinct(dive.pair.id)

# Select pair and sample year
diver.pair <- 5
samp.year <- 2021

# Select data for dive pair and sample year
df.8 <- df.7 %>% 
  filter(dive.pair.id == diver.pair, sampyear == samp.year) %>% 
  select(c(site, blockno, legal.size, sampyear, sampdate, diver.x, ab.n)) %>% 
  spread(key = diver.x, value = ab.n) %>%  
  mutate(dive.diff = abs(.[[6]] - .[[7]]))

# Identify diver names for plot title
dive.dev.divers <- df.8 %>% 
  select(c(6,7))

# determine number of sites surveyed by diver pair for plot
divers.site.n <- df.8 %>%
  filter(legal.size == '<140 mm') %>% 
  group_by(blockno) %>% 
  summarise(site.n = paste(n_distinct(site), sep = ''))

# create plot
dive.dev.plot <- ggplot(data = df.8, aes(x = blockno, y = dive.diff))+
  geom_boxplot(aes(fill = factor(legal.size)))+
  xlab('BlockNo')+
  ylab('Diver Total Count Deviation')+
  ylim(0, 90)+
  theme_bw()+
  ggtitle(paste(samp.year, names(dive.dev.divers[1]), 'vs',
                          names(dive.dev.divers[2])))+
  geom_text(data = divers.site.n, aes(y = 90, label = site.n), size = 3)+
  scale_fill_manual(values = c("#999999", "#56B4E9"))+
  theme(legend.title = element_blank(),
        legend.position = c(0.1, 0.8))

# save plot
setwd('C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/FIS_TimedSwimSurvey2021_Plots')
ggsave(filename = paste('TimedSwimSurvey_DiverDeviation_', samp.year, '_', 
                        names(dive.dev.divers[1]), 'vs',
                        names(dive.dev.divers[2]), '.pdf', sep = ''),
       plot = dive.dev.plot, units = 'mm', width = 190, height = 120)

ggsave(filename = paste('TimedSwimSurvey_DiverDeviation_', samp.year, '_', 
                        names(dive.dev.divers[1]), 'vs',
                        names(dive.dev.divers[2]), '.png', sep = ''),
       plot = dive.dev.plot, units = 'mm', width = 190, height = 120)


##---------------------------------------------------------------------------##
# PLOT: Counts x year x repeat sites ####
# Compare total counts of size class between years for repeat site

# Select repeat sites between years
df.3 <- df.2 %>% 
    filter(n() > 2)
    
# Set blockno and size class     
size.class <- '<140 mm'
block.no <- 16

# Create plot for 2020
plot.2020 <- df.3 %>% 
    filter(blockno == block.no, sampyear == 2020, legal.size == size.class) %>% 
ggplot(aes(x = reorder(site, -ab.n), y = ab.n))+
    geom_bar(stat = 'identity')+
    ylim(0, 350)+
    xlab('Site')+
    ylab(bquote('Total count (abalone.10'*~min^-1*')'))+
    ggtitle(paste('2020 '))+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
    annotate('text', x = 2, y = 350, label = size.class, 
            color = 'black', size = 4)

# Create plot for 2021
plot.2021 <- df.3 %>% 
    filter(blockno == block.no, sampyear == 2021, legal.size == size.class) %>% 
    ggplot(aes(x = reorder(site, -ab.n), y = ab.n))+
    geom_bar(stat = 'identity')+
    ylim(0, 350)+
    xlab('Site')+
  ylab(bquote('Total count (abalone.10'*~min^-1*')'))+
  ggtitle(paste('2021 '))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  annotate('text', x = 2, y = 350, label = size.class, 
           color = 'black', size = 4)

# Join plots
grid.arrange(plot.2020, plot.2021, nrow = 1)

##---------------------------------------------------------------------------##
## PLOT: Counts x year x top ten sites ####
# Determine and compare top ten sites in each block and year based on total 
# counts of size class

# Set blockno and size class
size.class <- '>140 mm'
block.no <- 24

# Create plot for 2020
plot.2020 <- df.2 %>% 
  filter(blockno == block.no, sampyear == 2020, legal.size == size.class) %>%
  ungroup() %>% 
  slice_max(order_by = ab.n, n = 10, with_ties = F) %>%  
  ggplot(aes(x = reorder(site, -ab.n), y = ab.n))+
  geom_bar(stat = 'identity')+
  ylim(0, 350)+
  xlab('Site')+
  ylab(bquote('Total count (abalone.10'*~min^-1*')'))+
  ggtitle(paste('2020 '))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  annotate('text', x = 1.5, y = 350, label = size.class, 
           color = 'black', size = 4)

# Create plot for 2021
plot.2021 <- df.2 %>% 
  filter(blockno == block.no, sampyear == 2021, legal.size == size.class) %>%
  ungroup() %>% 
  slice_max(order_by = ab.n, n = 10, with_ties = F) %>% 
  ggplot(aes(x = reorder(site, -ab.n), y = ab.n))+
  geom_bar(stat = 'identity')+
  ylim(0, 350)+
  xlab('Site')+
  ylab(bquote('Total count (abalone.10'*~min^-1*')'))+
  ggtitle(paste('2021 '))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  annotate('text', x = 1.5, y = 350, label = size.class, 
           color = 'black', size = 4)

# Join plots
grid.arrange(plot.2020, plot.2021, nrow = 1)
##---------------------------------------------------------------------------##
## PLOT: Counts x year x block 13 ####
# Compare total counts of size class at block 13 sites pre and post fishing in 2021

# Set size class
plot.size.class <- '>140 mm'

df.3 <- df.1 %>% 
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
setwd('C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/FIS_TimedSwimSurvey2021_Plots')
ggsave(filename = paste('TimedSwimSurvey_Block13_OpenvsClosed2021', '.pdf', sep = ''),
       plot = block.13.plot, units = 'mm', width = 190, height = 120)

ggsave(filename = paste('TimedSwimSurvey_Block13_OpenvsClosed2021', '.png', sep = ''),
       plot = block.13.plot, units = 'mm', width = 190, height = 120)

##---------------------------------------------------------------------------##
# ## Join to historic length frequency data ##
# # load most recent compiled MM dataframe
# compiledMM.df.final <-
#         readRDS('C:/CloudStor/R_Stuff/MMLF/compiledMM.df.final.RDS')
# 
# df.1 <- time.swim.dat.df.final %>% 
#         dplyr::rename(msr.date = 'sampdate',
#                       blocklist = 'blockno',
#                       subblocklist = 'subblockno',
#                       shell.length = 'shelllength') %>% 
#         mutate(newzone = 'E',
#                fishyear = 2020,
#                daylist = as.character(msr.date),
#                daylist_max = msr.date,
#                numblocks = 1,
#                numsubblocks = 1,
#                species = 1,
#                blocklist_1 = as.integer(blocklist),
#                sizelimit = ifelse(blocklist %in% c(27, 28), 145, 138),
#                datasource = 'ts2020') %>% 
#         select(c(msr.date, fishyear, newzone, daylist, daylist_max, numblocks, blocklist, numsubblocks, 
#                  subblocklist, shell.length, species, blocklist_1, sizelimit, datasource))
# 
# df.2 <- compiledMM.df.final %>% 
#         mutate(datasource = 'mm')
# 
# df.3 <- bind_rows(df.2, df.1)
# 
# ## create box plots for each block sampled in the stock assessment year and compare with historical records
# unique.zones <- 'E'
# unique.blocks <- c(16, 22, 23, 24, 27, 28)
# 
# for (i in unique.zones) {
#         for (j in unique.blocks) {
#                 # select data for block
#                 plotdat.block <- df.3 %>%
#                         filter(
#                                 newzone == i
#                                 & blocklist == j
#                                 & datasource == 'ts2020'
#                                 & between(shell.length, 0, 220))
#                 
#                 plotdat.block.historic <- df.3 %>%
#                         filter(
#                                 newzone == i
#                                 & blocklist == j
#                                 & between(fishyear, 1980, stock.assessment.year - 1)
#                                 & between(shell.length, sizelimit - 5, 220)
#                         )
#                 
#                 plotdat.block <- bind_rows(plotdat.block, plotdat.block.historic)
#                 
#                 # determine last sampling year
#                 last.fishyear <- max(plotdat.block.historic$fishyear)
#                 
#                 # determine number of sampling years
#                 n.fishyear <- length(unique(plotdat.block$fishyear))
#                 
#                 # generate plot for zone and block combination only if data is present
#                 
#                 if (nrow(plotdat.block) != 0) {
#                         # convert required grouping variable to factor for boxplot
#                         
#                         plotdat.block$fishyear <- as.factor(plotdat.block$fishyear)
#                         
#                         # generate a count of records for each year to add to boxplot
#                         
#                         plotdat.n <- plotdat.block %>%
#                                 group_by(fishyear, blockno) %>%
#                                 summarize(n = n())
#                         
#                         # generate table of size limits for each year to add to boxplot
#                         
#                         size.limit <- plotdat.block %>% 
#                                 group_by(fishyear, blockno) %>% 
#                                 summarise(legal.min.length = max(sizelimit))
#                         
#                         # generate boxplot of shell lengths for chosen grouping variable
#                         
#                         mm.zone.boxplot <-
#                                 ggplot(plotdat.block, aes(x = fishyear, y = shell.length)) +
#                                 geom_boxplot(outlier.colour = "orange", outlier.size = 1.5) +
#                                 geom_text(
#                                         data = plotdat.n,
#                                         aes(y = 220, label = n),
#                                         size = 3,
#                                         angle = 90
#                                 ) +
#                                 # geom_hline(aes(yintercept = 132), colour = 'red', linetype = 'dotted')+
#                                 geom_point(data = size.limit, aes(x = fishyear, y = legal.min.length), 
#                                            shape = 95, size = 7, colour = "red")+
#                                 geom_vline(
#                                         aes(xintercept = ifelse(stock.assessment.year - last.fishyear >= 2,
#                                                                 n.fishyear - 0.5, '')),
#                                         linetype = 'dashed',
#                                         colour = 'red',
#                                         size = 0.5)+
#                                 xlab('Year') +
#                                 ylab(paste('BlockNo', j, 'Shell Length (mm)')) +
#                                 coord_cartesian(ylim = c(0, 225)) +
#                                 theme_bw() +
#                                 theme(
#                                         plot.title = element_text(hjust = 0.5),
#                                         panel.grid.major = element_blank(),
#                                         panel.grid.minor = element_blank(),
#                                         axis.line = element_line(colour = "black"),
#                                         axis.text.x = element_text(angle = 45, vjust = 0.5)
#                                 )
#                         
#                         # print(mm.zone.boxplot)
#                         setwd('C:/CloudStor/R_Stuff/FIS/FIS_2020')
#                         ggsave(
#                                 filename = paste(i, '_BlockNo', j, '_TimedSwim_Boxplot_2020', '.pdf', sep = ''),
#                                 plot = mm.zone.boxplot,
#                                 width = 7.4,
#                                 height = 5.57,
#                                 units = 'in'
#                         )
#                         ggsave(
#                                 filename = paste(i, '_BlockNo', j, '_TimedSwim_Boxplot_2020', '.png', sep = ''),
#                                 plot = mm.zone.boxplot,
#                                 width = 7.4,
#                                 height = 5.57,
#                                 units = 'in'
#                         )
#                 }
#                 else{
#                 }
#                 
#         }
# }

##---------------------------------------------------------------------------##
## PLOT 1: LF ####
## Length frequency plot by block (mid-points)

# determine number of abalone recorded and number of sites sampled per block
block.ab.n <- time.swim.dat.df.final %>% 
        filter(!subblockno %in% c('28B', '28C'),
               sampdate > as.Date('2021-01-01')) %>% 
        group_by(blockno) %>% 
        summarise(ab.n = paste('n = ', n()))

block.site.n <- time.swim.dat.final %>% 
        filter(!subblockno %in% c('28B', '28C'),
               sampdate > as.Date('2021-01-01')) %>%
        group_by(blockno) %>% 
        summarise(site.n = paste('(', n_distinct(site), ')', sep = ''))

block.ab.site.n <- left_join(block.ab.n, block.site.n) %>% 
        mutate(n = paste(ab.n, site.n, sep = ' '))

# create length frequency plot
lf.plot <- time.swim.dat.df.final %>% 
        filter(!subblockno %in% c('28B', '28C'),
               sampdate > as.Date('2021-01-01')) %>% 
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


setwd('C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/FIS_TimedSwimSurvey2021_Plots')
ggsave(filename = paste('TimedSwimSurvey_2021_SizeFrequencyPlot', '.pdf', sep = ''),
       plot = lf.plot, units = 'mm', width = 190, height = 250)
ggsave(filename = paste('TimedSwimSurvey_2021_SizeFrequencyPlot', '.png', sep = ''),
       plot = lf.plot, units = 'mm', width = 190, height = 250)
##---------------------------------------------------------------------------##

##---------------------------------------------------------------------------##
## PLOT 2: LF x YEAR ####
## Length frequency plot by block and year (mid-points)

# determine number of abalone recorded and number of sites sampled per block
block.ab.n <- time.swim.dat.df.final %>% 
    filter(!subblockno %in% c('28B', '28C'),
           sampdate > as.Date('2021-01-01'),
           !blockno %in% c(13, 14, 29, 30)) %>% 
    group_by(blockno) %>% 
    summarise(ab.n = paste('n = ', n()))

block.site.n <- time.swim.dat.final %>% 
    filter(!subblockno %in% c('28B', '28C'),
           sampdate > as.Date('2021-01-01'),
           !blockno %in% c(13, 14, 29, 30)) %>%
    group_by(blockno) %>% 
    summarise(site.n = paste('(', n_distinct(site), ')', sep = ''))

block.ab.site.n <- left_join(block.ab.n, block.site.n) %>% 
    mutate(n = paste(ab.n, site.n, sep = ' '))

df.1 <- time.swim.dat.final %>%
    filter(sampyear == 2020,
           (!subblockno %in% c('28B', '28C'))) %>% 
    group_by(sampyear, blockno, sizeclass.2021) %>% 
    summarise(n = sum(sizeclass_freq)) %>% 
    mutate(freq = n / sum(n))

df.2 <- time.swim.dat.final %>%
    filter(sampyear == 2021,
           (!subblockno %in% c('28B', '28C')),
           !blockno %in% c(13, 14, 29, 30)) %>% 
    group_by(sampyear, blockno, sizeclass.2021) %>% 
    summarise(n = sum(sizeclass_freq)) %>% 
    mutate(freq = n / sum(n))

df.3 <- bind_rows(df.1, df.2)

# create length frequency plot
lf.plot <- ggplot()+
    geom_bar(data = df.1, aes(x = sizeclass.2021, y = freq*100, fill = factor(sampyear)), 
             stat = 'identity', position = 'identity', alpha = 1) +
    geom_bar(data = df.2, aes(x = sizeclass.2021, y = freq*100, fill = factor(sampyear)), 
             stat = 'identity', position = 'identity', alpha = 0.5)+
    geom_vline(data = df.1, aes(xintercept = ifelse(blockno %in% c('27', '28'), 3.8, 3.5)), linetype = 'dashed', colour = 'red', size = 0.5) +
    theme_bw() +
    scale_fill_manual(values = c("#77AADD", "#BBCC33"))+
    # scale_fill_manual(values = c("#7AA6DC99", "#8F770099"))+
    guides(fill = guide_legend(title = 'Year'))+
    facet_grid(blockno ~ .) +
    scale_y_continuous(breaks = seq(0, 40, 10), labels = seq(0, 40, 10))+
    xlab("Shell Length (mm)")+
    ylab(paste("Percentage (%)"))+
    geom_text(data = block.ab.site.n, aes(x = 7, y = 10, label = n), color = 'black', size = 3)

df.3.plot <- ggplot()+
    geom_bar(data = df.3, aes(x = sizeclass.2021, y = freq*100, fill = factor(sampyear)), 
             stat = 'identity', position = 'dodge', alpha = 1) +
    geom_vline(data = df.3, aes(xintercept = ifelse(blockno %in% c('27', '28'), 3.8, 3.5)), linetype = 'dashed', colour = 'red', size = 0.5) +
    theme_bw() +
    scale_fill_manual(values = c("#77AADD", "#BBCC33"))+
    # scale_fill_manual(values = c("#7AA6DC99", "#8F770099"))+
    guides(fill = guide_legend(title = 'Year'))+
    facet_grid(blockno ~ .) +
    scale_y_continuous(breaks = seq(0, 40, 10), labels = seq(0, 40, 10))+
    xlab("Shell Length (mm)")+
    ylab(paste("Percentage (%)"))+
    geom_text(data = block.ab.site.n, aes(x = 7, y = 10, label = n), color = 'black', size = 3)

setwd('C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/FIS_TimedSwimSurvey2021_Plots')
ggsave(filename = paste('TimedSwimSurvey_2021_SizeFrequencyPlot_BLOCKS16-28', '.pdf', sep = ''),
       plot = df.3.plot, units = 'mm', width = 190, height = 250)
ggsave(filename = paste('TimedSwimSurvey_2021_SizeFrequencyPlot_BLOCKS16-28', '.png', sep = ''),
       plot = df.3.plot, units = 'mm', width = 190, height = 250)
##---------------------------------------------------------------------------##
## PLOT 3: BP x YEAR ####
## Box plot by block and year (mid-points)

ts.dat.n <- time.swim.dat.final %>% 
    filter(!subblockno %in% c('28B', '28C'),
           legal.size == '>140 mm') %>% 
    group_by(sampyear, blockno) %>% 
    summarise(n = n_distinct(site))


ts.bp <- time.swim.dat.df.final %>% 
    filter(!subblockno %in% c('28B', '28C')) %>% 
    ggplot(aes(x = blockno, y = shelllength.2021, fill = factor(sampyear))) +
    geom_boxplot(position = position_dodge2(1, preserve = 'single'),
                 outlier.colour = '#EE8866') +
    scale_fill_manual(values = c("#77AADD", "#BBCC33"))+
    ylim(0, 220)+
    geom_hline(aes(yintercept = 140), linetype = 'dashed', colour = 'red', size = 0.5)+
    xlab('BlockNo')+
    ylab('Shell length (mm)')+
    theme_bw()+
    geom_text(data = ts.dat.n, aes(y = 220, label = n, colour = factor(sampyear, levels = c('2020', '2021'))), size = 3, 
              position = position_dodge2(0.8))+
    scale_colour_manual(values = c("#77AADD", "#BBCC33"))+
    guides(size = 'legend', colour = 'none',
           fill = guide_legend(title = 'Year'))

setwd('C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/FIS_TimedSwimSurvey2021_Plots')
ggsave(filename = paste('TimedSwimSurvey_2021_SizeBoxPlot_BLOCKS13-30', '.pdf', sep = ''),
       plot = ts.bp, units = 'mm', width = 190, height = 120)
ggsave(filename = paste('TimedSwimSurvey_2021_SizeBoxPlot_BLOCKS13-30', '.png', sep = ''),
       plot = ts.bp, units = 'mm', width = 190, height = 120)    

##---------------------------------------------------------------------------##
## PLOT 4: COUNT PER TEN MIN YEARS ####
## average count of all legal and sub-legal abalone per 10 min by year for each site within each blockno 
## (i.e. the average between paired divers for each site)

plot.size.class <- '<140 mm'

# determine mean count per 10 min for legal abalone for each blockno
ten.min.mean.year <- time.swim.dat.final %>% 
    filter(!subblockno %in% c('28B', '28C'),
           legal.size == plot.size.class) %>% 
    group_by(blockno, site, diver, sampyear, time.elapsed) %>% 
    summarise(ab.n = sum(sizeclass_freq)) %>% 
    group_by(blockno, sampyear) %>% 
    summarise(mean.ab.n = mean(ab.n)) %>% 
    mutate(sampyear = factor(sampyear))

time.swim.dat.n <- time.swim.dat.final %>% 
    filter(!subblockno %in% c('28B', '28C'),
           legal.size == plot.size.class) %>% 
    group_by(sampyear, blockno) %>% 
    summarise(n = n_distinct(site))

count.plot.sizeclass <- time.swim.dat.final %>% 
    filter(!subblockno %in% c('28B', '28C'),
           legal.size == plot.size.class) %>%
    # filter(midsize < 150) %>% 
    group_by(blockno, site, diver, sampyear) %>% 
    summarise(ab.n = sum(sizeclass_freq)) %>% 
    group_by(blockno, site, sampyear) %>% 
    summarise(mean.ab.n = mean(ab.n)) %>% 
    mutate(sampyear = factor(sampyear, levels = c('2020', '2021'))) %>%  
    ggplot(aes(x = blockno, y = mean.ab.n))+
    geom_boxplot(aes(fill = sampyear), position = position_dodge2(1, preserve = 'single'),
                 outlier.colour = '#EE8866') +
    scale_fill_manual(values = c("#77AADD", "#BBCC33"))+
    geom_point(data = ten.min.mean.year, aes(group = factor(sampyear, levels = c('2020', '2021'))), shape = 19,
               size = 2, colour = 'red', fill = 'red', position = position_dodge2(0.8))+
    theme_bw()+
    ylab(bquote('Average count (abalone.10'*~min^-1*')'))+
    xlab('Blockno')+
    ylim(0, 150)+
    geom_text(data = time.swim.dat.n, aes(y = 150, label = n, colour = factor(sampyear, levels = c('2020', '2021'))), size = 3, 
              position = position_dodge2(0.8))+
    scale_colour_manual(values = c("#77AADD", "#BBCC33"))+
    guides(size = 'legend', colour = 'none',
           fill = guide_legend(title = 'Year'))+
    ggtitle(paste(ifelse(plot.size.class == '<140 mm', 'Sublegal', 'Legal'), plot.size.class))+
    theme(plot.title = element_text(vjust = -15, hjust = 0.05))
    # theme(legend.position = "none")

setwd('C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/FIS_TimedSwimSurvey2021_Plots')
ggsave(filename = paste('TimedSwimSurvey_2021_TenMinuteCount', 
ifelse(plot.size.class == '<140 mm', 'Sublegal', 'Legal'),'Year', '.pdf', sep = ''), 
       plot = count.plot.sizeclass, units = 'mm', width = 190, height = 120)
ggsave(filename = paste('TimedSwimSurvey_2021_TenMinuteCount', 
                        ifelse(plot.size.class == '<140 mm', 'Sublegal', 'Legal'),'Year', '.png', sep = ''), 
       plot = count.plot.sizeclass, units = 'mm', width = 190, height = 120)

##---------------------------------------------------------------------------##
## PLOT 4: REPEAT COUNT PER TEN MIN YEARS ####
# compare counts at repeat sites between 2020 and 2021

plot.size.class <- '<140 mm'

count.plot.rep.dat <- time.swim.dat.final %>% 
    filter(!subblockno %in% c('28B', '28C'),
           legal.size == plot.size.class) %>%
    group_by(blockno, site, diver, sampyear) %>% 
    summarise(ab.n = sum(sizeclass_freq)) %>% 
    group_by(blockno, site, sampyear) %>% 
    summarise(mean.ab.n = mean(ab.n)) %>% 
    group_by(site) %>% 
    filter(n() > 1)

ten.min.mean.rep.year.sites <- count.plot.rep.dat %>% 
    group_by(blockno, sampyear) %>% 
    summarise(n = n_distinct(site))

count.plot.rep.mean <- time.swim.dat.final %>% 
    filter(!subblockno %in% c('28B', '28C'),
           !blockno %in% c(13, 14, 29, 30),
           legal.size == plot.size.class) %>%
    group_by(blockno, site, diver, sampyear) %>% 
    summarise(ab.n = sum(sizeclass_freq)) %>% 
    group_by(blockno, sampyear) %>%
    filter(n() > 1) %>% 
    summarise(mean.ab.n = mean(ab.n))
    
count.rep.plot <- count.plot.rep.dat %>%  
    mutate(sampyear = factor(sampyear)) %>%  
    ggplot(aes(x = blockno, y = mean.ab.n))+
    geom_boxplot(aes(fill = sampyear), position = position_dodge2(1, preserve = 'single'),
                 outlier.colour = '#EE8866') +
    scale_fill_manual(values = c("#77AADD", "#BBCC33"))+
    geom_point(data = count.plot.rep.mean, aes(group = factor(sampyear, levels = c('2020', '2021'))), shape = 19,
               size = 2, colour = 'red', fill = 'red', position = position_dodge2(0.8))+
    theme_bw()+
    ylab(bquote('Average count (abalone.10'*~min^-1*')'))+
    xlab('Blockno')+
    ylim(0, 100)+
    geom_text(data = ten.min.mean.rep.year.sites, aes(y = 100, label = n, colour = factor(sampyear, levels = c('2020', '2021'))), size = 3, 
              position = position_dodge2(0.8))+
    scale_colour_manual(values = c("#77AADD", "#BBCC33"))+
    guides(size = 'legend', colour = 'none',
           fill = guide_legend(title = 'Year'))+
    ggtitle(paste(ifelse(plot.size.class == '<140 mm', 'Sublegal ', 'Legal '), plot.size.class,'\nRepeat Sites', sep = ''))+
    # ggtitle('Sub-Legal <140 mm\nRepeat Sites')+
    theme(plot.title = element_text(vjust = -20, hjust = 0.95))
    # theme(legend.position = "none")

    setwd('C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/FIS_TimedSwimSurvey2021_Plots')
    ggsave(filename = paste('TimedSwimSurvey_2021_TenMinuteCount_RepeatSites', 
                            ifelse(plot.size.class == '<140 mm', 'Sublegal', 'Legal'),'Year', '.pdf', sep = ''), 
           plot = count.rep.plot, units = 'mm', width = 190, height = 120)
    ggsave(filename = paste('TimedSwimSurvey_2021_TenMinuteCount_RepeatSites', 
                            ifelse(plot.size.class == '<140 mm', 'Sublegal', 'Legal'),'Year', '.png', sep = ''), 
           plot = count.rep.plot, units = 'mm', width = 190, height = 120)

##---------------------------------------------------------------------------##
## TAB 1: SUMMARY ####
## summary table of counts and CPUE by size class and block (NOTE: run script for CPUE above)

# arrange size classes in order
sizeclasses <- c("0-20", "20-40", "40-60", "60-80", "80-100", "100-120", "120-140", "140-160", "160-180", "180-200", "200-220")

time.swim.count.blockno <- time.swim.dat.final %>% 
    filter(!subblockno %in% c('28B', '28C'),
           sampdate > as.Date('2021-01-01')) %>% 
    group_by(blockno, site, diver, sampyear, legal.size, time.elapsed) %>% 
    summarise(ab.n = sum(sizeclass_freq)) %>% 
    group_by(blockno, legal.size) %>% 
    # summarise(ab.cpue = mean(ab.n))
    summarise(sites = n_distinct(site),
              ab.min = round(mean(ab.n), digits = 2)) %>% 
    spread(legal.size, ab.min) %>%
    as.data.frame() %>% 
    dplyr::rename('Blockno' = blockno,
                  'Sites' = sites,
                  'Average\ncount\n<140mm' = '<140 mm',
                  'Average\ncount\n>140mm' = '>140 mm') %>% 
    adorn_totals(fill = '',,,, contains('Sites'))

# time.swim.summary <- left_join(time.swim.count.blockno, time.swim.cpue.blockno) %>% 
#         dplyr::rename('Blockno' = blockno,
#                'Sites' = sites,
#                'Average\ncount\n<140mm' = '<140 mm',
#                'Average\ncount\n>140mm' = '>140 mm',
#                'CPUE' = est.kg.hr) %>% 
#         adorn_totals(fill = '',,,, contains('Sites'))

# create formated summary tables for report layout

time.swim.summary.tab <- time.swim.count.blockno %>% 
        ggpubr::ggtexttable(rows = NULL, theme = ggpubr::ttheme('mOrange'))

# time.swim.summary.tab <- time.swim.summary %>% 
#         ggpubr::ggtexttable(rows = NULL, theme = ggpubr::ttheme('mOrange'))

setwd('C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/FIS_TimedSwimSurvey2021_Plots')
# ggsave(filename = paste('TimedSwimSurvey_2020_SummaryTable', '.pdf', sep = ''), 
#        plot = time.swim.summary.tab)
# ggsave(filename = paste('TimedSwimSurvey_2020_SummaryTable', '.png', sep = ''), 
#        plot = time.swim.summary.tab)

write.xlsx(time.swim.count.blockno, 'TimedSwimSurvey_2021_SummaryTable.xlsx', sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

ggsave(filename = paste('TimedSwimSurvey_2021_SummaryTable', '.pdf', sep = ''), 
       plot = time.swim.summary.tab, units = 'mm', width = 190, height = 120)
ggsave(filename = paste('TimedSwimSurvey_2021_SummaryTable', '.png', sep = ''), 
       plot = time.swim.summary.tab, units = 'mm', width = 190, height = 120)

df.1 <- time.swim.dat.final %>%
        filter(!subblockno %in% c('28B', '28C')) %>%
        group_by(site, diver) %>% 
        summarise(ab.measured = sum(sizeclass_freq))

##---------------------------------------------------------------------------##
## MAP 3: Site Count ####
## average Count by site - possibly explore KUDs

sf.tas.map <- st_read("C:/Users/jaimem/Dropbox/AbaloneData/SpatialLayers/TasLand.gpkg")
sf.subblock.map <- st_read("C:/Users/jaimem/Dropbox/AbaloneData/SpatialLayers/SubBlockMaps.gpkg")

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
time.swim.sites <- time.swim.dat.final %>%
    filter(sampdate > as.Date('2021-01-01')) %>%
        distinct(site, .keep_all = T) %>% 
        select(c(site, blockno, subblockno, sampdate, actual.geom)) %>% 
        st_as_sf() %>% 
        st_set_crs(GDA2020)

ten.min.mean.site <- time.swim.dat.final %>% 
    filter(!subblockno %in% c('28B', '28C'),
           sampdate > as.Date('2021-01-01')) %>%
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
ts.proposed.geom <- readRDS('C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/ts.proposed.geom.RDS')

# not.surveyed.df <- site.samp.subblock.loc %>% 
#         filter(sampyear == 2021,
#                sampled == '0')

not.surveyed.df <- ts.proposed.geom %>% 
  filter(sampyear == 2021,
         sampled == '0')

# create maps

for (i in blocks.sampled){
        
        # filter for blockno and legal counts
        count.site.dat <- time.swim.count.site.loc %>% 
                filter(blockno == i &
                               legal.size == '>140 mm') %>% 
                st_as_sf
        
        # filter subblock map for blockno
        sf.subblock.map.crop <- sf.subblock.map %>%
                filter(blockno == i &
                               !subblockno %in% c('28B', '28C'))  %>% 
                st_as_sf() 
        
        # filter proposed sites not surveyed
        sites.not.surveyed <- not.surveyed.df %>% 
                filter(blockno == i)
        
        # crop tas map to blockno
        sf.tas.map.crop <- st_crop(sf.tas.map, sf.subblock.map.crop)
        
        count.site.map <- ggplot(data = st_geometry(sf.tas.map.crop)) +
                geom_sf(data = sf.subblock.map.crop, aes(label = subblockno), fill = NA)+
                geom_sf_text(data = sf.subblock.map.crop, aes(label = subblockno))+
                geom_sf(fill = 'grey') +
                geom_sf(data = sites.not.surveyed, shape = 17, size = 2, colour = 'red')+
                geom_sf(data = count.site.dat, aes(fill = mean.ab.n), shape = 21, size = 2)+
                scale_fill_gradientn(colours = c("navyblue", "blue", "cyan", "green", "yellow", "orange", "red"))+
                theme_bw() +
                annotation_scale(location = "bl", width_hint = 0.5) +
                annotation_north_arrow(location = "br", which_north = "true", 
                                       pad_x = unit(0.05, "cm"), pad_y = unit(0.1, "cm"),
                                       style = north_arrow_fancy_orienteering)+
                labs(fill = (bquote('Average\ncount')))+
                xlab('Longitude')+
                ylab('Latitude')
        
        setwd('C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/FIS_TimedSwimSurvey2021_Plots')
        ggsave(filename = paste('TimedSwimSurvey_2021_Site_LegalCount_BlockNo_', i, '.pdf', sep = ''),
               plot = count.site.map, units = 'mm', width = 190, height = 250)
        ggsave(filename = paste('TimedSwimSurvey_2021_Site_LegalCount_BlockNo_', i, '.png', sep = ''),
               plot = count.site.map, units = 'mm', width = 190, height = 150)
        
}

for (i in blocks.sampled){
        
        # filter for blockno and sub-legal counts
        count.site.dat.non.legal <- time.swim.count.site.loc %>% 
                filter(blockno == i &
                               legal.size == '<140 mm') %>% 
                st_as_sf
        
        count.site.dat.legal <- time.swim.count.site.loc %>% 
                filter(blockno == i &
                               legal.size == '>140 mm') %>% 
                st_as_sf
        
        # filter subblock map for blockno
        sf.subblock.map.crop <- sf.subblock.map %>%
                filter(blockno == i &
                               !subblockno %in% c('28B', '28C'))  %>% 
                st_as_sf() 
        
        # filter proposed sites not surveyed
        sites.not.surveyed <- not.surveyed.df %>% 
                filter(blockno == i)
        
        # crop tas map to blockno
        sf.tas.map.crop <- st_crop(sf.tas.map, sf.subblock.map.crop)
        
        count.site.map.non.legal <- ggplot(data = st_geometry(sf.tas.map.crop)) +
                geom_sf(data = sf.subblock.map.crop, aes(label = subblockno), fill = NA)+
                geom_sf_text(data = sf.subblock.map.crop, aes(label = subblockno))+
                geom_sf(fill = 'grey') +
                geom_sf(data = sites.not.surveyed, shape = 5, size = 0.8, colour = 'red')+
                geom_sf(data = count.site.dat.non.legal, aes(fill = mean.ab.n), shape = 21, size = 2)+
                scale_fill_gradientn(colours = c("navyblue", "blue", "cyan", "green", "yellow", "orange", "red"))+
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
                scale_fill_gradientn(colours = c("navyblue", "blue", "cyan", "green", "yellow", "orange", "red"))+
                theme_bw() +
                annotation_scale(location = "bl", width_hint = 0.5) +
                annotation_north_arrow(location = "br", which_north = "true", 
                                       pad_x = unit(0.05, "cm"), pad_y = unit(0.1, "cm"),
                                       style = north_arrow_fancy_orienteering)+
                labs(title = '>140 mm', fill = (bquote('Average\ncount')))+
                xlab('Longitude')+
                scale_x_continuous(breaks = seq(140, 149, by = 0.1))+
                ylab('Latitude')
        
        count.site.map <- grid.arrange(
                arrangeGrob(cowplot::plot_grid(count.site.map.non.legal, count.site.map.legal, align = 'v',
                                               ncol = 1, nrow = 2), ncol = 1))
        
        # count.site.map <- ifelse(i != '16', grid.arrange(
        #         arrangeGrob(cowplot::plot_grid(count.site.map.non.legal, count.site.map.legal, align = 'v',
        #                                        ncol = 1, nrow = 2), ncol = 2)),
        #         grid.arrange(
        #                 arrangeGrob(cowplot::plot_grid(count.site.map.non.legal, count.site.map.legal, align = 'v',
        #                                                ncol = 2, nrow = 1), ncol = 1)))

        setwd('C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/FIS_TimedSwimSurvey2021_Plots')
        ggsave(filename = paste('TimedSwimSurvey_2021_SiteCountMap_BlockNo_', i, '.pdf', sep = ''),
               plot = count.site.map, units = 'mm', width = 250, height = 250)
        ggsave(filename = paste('TimedSwimSurvey_2021_SiteCountMap_BlockNo_', i, '.png', sep = ''),
               plot = count.site.map, units = 'mm', width = 250, height = 250)
        
}

##---------------------------------------------------------------------------##
## MAP 4: Site CPUE ####
## average CPUE by site

sf.tas.map <- st_read("C:/Users/jaimem/Dropbox/AbaloneData/SpatialLayers/TasLand.gpkg")
sf.subblock.map <- st_read("C:/Users/jaimem/Dropbox/AbaloneData/SpatialLayers/SubBlockMaps.gpkg")

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
time.swim.sites <- time.swim.dat.final %>% 
        distinct(site, .keep_all = T) %>% 
        select(c(site, blockno, subblockno, sampdate, actual.geom)) %>% 
        st_as_sf() %>% 
        st_set_crs(GDA2020)

# # transform to GDA2020
# time.swim.sites <- st_transform(time.swim.sites, GDA2020)

time.swim.cpue.site.loc <- left_join(time.swim.cpue.site, time.swim.sites) %>% 
        st_as_sf() 

# identify blocks sampled
blocks.sampled <- unique(time.swim.cpue.site.loc$blockno)

# create maps

for (i in blocks.sampled){

# filter for blockno and remove really high CPUE
cpue.site.dat <- time.swim.cpue.site.loc %>% 
        filter(blockno == i &
                       est.kg.hr < 200) %>% 
        st_as_sf

# filter subblock map for blockno
sf.subblock.map.crop <- sf.subblock.map %>% 
        filter(blockno == i &
                       !subblockno %in% c('28B', '28C')) %>% 
        st_as_sf() 

# crop tas map to blockno
sf.tas.map.crop <- st_crop(sf.tas.map, sf.subblock.map.crop)

cpue.site.map <- ggplot(data = st_geometry(sf.tas.map.crop)) +
        geom_sf(data = sf.subblock.map.crop, aes(label = subblockno), fill = NA)+
        geom_sf_text(data = sf.subblock.map.crop, aes(label = subblockno))+
        geom_sf(fill = 'grey') +
        geom_sf(data = cpue.site.dat, aes(fill = est.kg.hr), shape = 21, size = 2)+
        scale_fill_gradientn(colours = c("navyblue", "blue", "cyan", "green", "yellow", "orange", "red"))+
        theme_bw() +
        annotation_scale(location = "bl", width_hint = 0.5) +
        annotation_north_arrow(location = "br", which_north = "true", 
                               pad_x = unit(0.05, "cm"), pad_y = unit(0.1, "cm"),
                               style = north_arrow_fancy_orienteering)+
        labs(fill = 'Kg/Hr')+
        xlab('Longitude')+
        ylab('Latitude')

setwd('C:/CloudStor/R_Stuff/FIS/FIS_2020')
ggsave(filename = paste('TimedSwimSurvey_2020_Site_CPUE_BlockNo_', i, '.pdf', sep = ''),
       plot = cpue.site.map, units = 'mm', width = 190, height = 250)
ggsave(filename = paste('TimedSwimSurvey_2020_Site_CPUE_BlockNo_', i, '.png', sep = ''),
       plot = cpue.site.map, units = 'mm', width = 190, height = 150)

}

##---------------------------------------------------------------------------##

# ## create chlorpleth map of count for each block (i.e. interpolate CPUE)
sf.tas.map.crop <- st_crop(sf.tas.map, sf.subblock.map.crop)
sf.tas.map.crop.inverse <- st_difference(sf.subblock.map.crop, sf.tas.map.crop)

# convert maps to sp
sp.tas.map.crop <- as_Spatial(sf.tas.map.crop)
sp.count.site.dat <- as_Spatial(count.site.dat)
sp.tas.map.crop.inverse <- as_Spatial(sf.tas.map.crop.inverse)

# grid (1 ha side = 402.0673 m ?)
idw_grid <- st_make_grid(time.swim.cpue.site.loc, cellsize = 402.0673, square = FALSE)

idw_grid <- st_make_grid(sf.tas.map.crop, cellsize = 402.0673, square = FALSE)

idw_grid <- st_make_grid(sf.tas.map.crop, cellsize = 402.0673, square = FALSE)

idw_grid <- st_make_grid(sf.tas.map.crop.inverse, cellsize = 402.0673, square = FALSE)


coast_buffer <- sf.tas.map.crop %>% 
        st_buffer(dist = units::set_units(500, m))

ggplot(data = sf.tas.map.crop)+
        geom_sf()+
        geom_sf(data = coast_buffer, fill = NA)



# idw
P_idw_hex <- gstat::idw(mean.ab.n ~ 1, sp.count.site.dat, newdata = idw_grid, idp = 2)

rslt_hex <- st_as_sf(P_idw_hex)

ggplot(data = st_geometry(sf.tas.map.crop)) +
        geom_sf(data = rslt_hex, aes(fill = var1.pred), col = "grey60", size = 0.1) +
        scale_fill_gradientn(colours = c("navyblue", "blue", "cyan", "green", "yellow", "orange", "red")) +
        # scale_fill_viridis_c()+
        # geom_sf(data = df.1) +
        geom_sf() +
        # geom_sf(data = df.1) +
        # scale_colour_viridis_c(option = "viridis") +
        theme_bw() +
        annotation_scale(location = "bl", width_hint = 0.5) +
        annotation_north_arrow(location = "br", which_north = "true", 
                               pad_x = unit(0.05, "cm"), pad_y = unit(0.1, "cm"),
                               style = north_arrow_fancy_orienteering)+
        labs(fill = 'Kg/Hr')+
        xlab('Longitude')+
        ylab('Latitude')

##---------------------------------------------------------------------------##
## re-order FIS reference sites data for for sequential site names running south to north (i.e. clockwise)
# read original site data
fis.site.ref <- st_read("C:/Users/jaimem/Documents/Abalone_research/AB_TimedSwimFIS/TimedSwim_Block13_14_300m_2.gpkg")
fis.site.ref.names <- read.xlsx("C:/Users/jaimem/Documents/Abalone_research/AB_TimedSwimFIS/FIS_ReferenceSiteNames_ordered.xlsx",
                                detectDates = T)

fis.site.ref.df <- fis.site.ref.names %>% 
        mutate(oid = as.character(oid)) %>% 
                       left_join(fis.site.ref, .) %>% 
        dplyr::rename('name' = site) %>% 
        select(c(name, geom))

# transform to GDA2020
# fis.site.ref.df <- st_transform(fis.site.ref.df, WGS84) %>% 
#         as_Spatial() %>% 
#         as.data.frame() %>% 
#         dplyr::rename('latitude' = coords.x2,
#                       'longitude' = coords.x1)

fis.site.ref.df <- st_transform(fis.site.ref.df, WGS84)

st_write(fis.site.ref.df, 
         dsn = "C:/Users/jaimem/Documents/Abalone_research/AB_TimedSwimFIS/FIS_TimedSwimReferenceSites.gpkg", 
         layer = "fis.site.ref.df", driver = "GPKG", overwrite = T, delete_dsn = T)

st_write(fis.site.ref.df, 
         dsn = "C:/Users/jaimem/Documents/Abalone_research/AB_TimedSwimFIS/FIS_TimedSwimReferenceSites.kml", 
         layer = "fis.site.ref.df", driver = "kml", overwrite = T, delete_dsn = T)

##---------------------------------------------------------------------------##
##PLOT 5: Historic SAM vs Timed count ####

# load original sam site details and count data
fis.sam.data <- read.xlsx("C:/Users/jaimem/OneDrive - University of Tasmania/Documents/AB_TimedSwimFIS/SampleDetailsforTimedSwimV2_JM.xlsx",
                               detectDates = T)

colnames(fis.sam.data) <- tolower(colnames(fis.sam.data))

# join original and 2020 sam survey site data 

df.1 <- fis.sam.data %>% 
        filter(stat.block %in% c(16, 22, 23, 24, 27, 28)) %>% 
        left_join(., fis.site.sam.data %>% select(c(SAM_Id, name)), by = c('sam_id' = 'SAM_Id')) %>%
        dplyr::rename(site.sam = site,
                      site = name) %>% 
        mutate(sampyear = year(date)) %>% 
        filter(sampyear >= 2001) %>% 
        dplyr::rename(sampdate = date,
                      blockno = stat.block,
                      ab.n = avg_count_per.10min,
                      diver = collector) %>% 
        select(c(blockno, site, site.sam, diver, ab.n, sampyear)) %>% 
        filter(!is.na(ab.n))

df.1 %>% 
        ggplot(aes(x = as.factor(blockno), y = ab.n))+
        geom_jitter(aes(colour = sampyear), width = 0.2, size = 3)+
        scale_color_gradientn(colours = rainbow(11))


df.2 <- time.swim.dat.final %>% 
        filter(!subblockno %in% c('28B', '28C'),
               sampdate < as.Date('2021-01-01')) %>%
        group_by(blockno, site, diver) %>% 
        summarise(ab.n = sum(sizeclass_freq)) %>% 
        mutate(sampyear = 2020,
               blockno = as.numeric(blockno))

df.3 <- bind_rows(df.1, df.2) %>% 
        mutate(site = if_else(!is.na(site), site, site.sam))

hist.count.plot <- df.3 %>%
        group_by(blockno, sampyear, site) %>% 
        # summarise(mean.ab.n = mean(ab.n)) %>% 
        ggplot(aes(x = as.factor(blockno), y = ab.n, fill = as.factor(sampyear)))+
        geom_boxplot(position = position_dodge2(preserve = "single"))+
        theme_bw()+
        ylab(bquote('Average count (abalone.10'*~min^-1*')'))+
        xlab('Blockno')+
        geom_text(data = df.3 %>% 
                          group_by(blockno, sampyear) %>% 
                          summarise(n = n_distinct(site), lab.pos = max(ab.n) + 1),
                  aes(y = lab.pos, label = paste0(n, '\n')),
                  position = position_dodge2(width = 0.75, preserve = 'single'),
                  size = 2)+
        labs(fill = 'Year')+
        scale_fill_viridis(discrete = TRUE)


setwd('C:/CloudStor/R_Stuff/FIS/FIS_2020')
ggsave(filename = paste('TimedSwimSurvey_2020_TenMinuteCountHistoricPlot', '.pdf', sep = ''), 
       plot = hist.count.plot, units = 'mm', width = 190, height = 120)
ggsave(filename = paste('TimedSwimSurvey_2020_TenMinuteCountHistoricPlot', '.png', sep = ''), 
       plot = hist.count.plot, units = 'mm', width = 190, height = 120)


##---------------------------------------------------------------------------##

## RANDOM STUFF ####

# time.swim.dat %>%
#         mutate(sizeclass = factor(sizeclass, levels = sizeclasses)) %>%
#         group_by(sizeclass, blockno) %>%
#         summarise(n = sum(sizeclass_freq)) %>%
#         spread(blockno, n) %>%
#         as.data.frame()

# # summary table of abalone/minute by block and size class
# time.swim.dat %>%
#         group_by(blockno, site, diver) %>%
#         summarise(ab.measured = sum(sizeclass_freq),
#                   swim.time = max(time.elapsed)) %>%
#         mutate(ab.cpue = ab.measured / swim.time) %>%
#         group_by(blockno) %>%
#         summarise(sites = n_distinct(site),
#                   ab.min = mean(ab.cpue),) %>%
#         as.data.frame()
time.swim.dat %>%
        group_by(blockno, site, diver) %>%
        summarise(ab.measured = sum(sizeclass_freq),
                  swim.time = max(time.elapsed)) %>%
        mutate(ab.cpue = ab.measured / swim.time) %>%
        group_by(diver) %>%
        summarise(ab.min = mean(ab.cpue))

##---------------------------------------------------------------------------##
## Summary statistics for survey times etc
time.swim.dat.df.final %>% 
        filter(!subblockno %in% c('28B', '28C')) %>%
        summarise(samp.days = n_distinct(sampdate),
                  sites = n_distinct(site),
                  sites.day = sites / samp.days)

time.swim.dat.df.final %>% 
        filter(!subblockno %in% c('28B', '28C')) %>%
        group_by(sampdate) %>% 
        summarise(divers = n_distinct(diver),
                  start.time = min(starttime),
                  end.time = max(finishtime),
                  day.time = end.time - start.time) %>% 
        mutate(divers.day = mean(divers),
               av.day.time = mean(day.time)) %>% 
        ungroup()
        
##---------------------------------------------------------------------------##        
## PLOT 2: Diver bias 
## Diver A vs Diver B comparison of sizeclass counts per site

# create datafame of diver details and pairs
diver.df <- data.frame('diver' = c('LT', 'SI', 'SL', 'JM', 'SL', 'SL', 'BD', 'NW', 'GP', 'PA', 'PA', 'AD'),
                       'diver.id' = c(1, 2, 3, 4, 3, 3, 5, 6, 7, 8, 8, 9),
                       'dive.pair.id' = c(1, 2, 3, 4, 5, 6, 1, 2, 3, 4, 5, 6),
                       'dive.buddy.id' = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2))

# create plot labels for size classes
sizeclasses <- c("0-20", "20-40", "40-60", "60-80", "80-100", "100-120", 
                 "120-140", "140-160", "160-180", "180-200", "200-220")

sizeclasses.2021 <- c("0-100", "100-120", 
                 "120-140", "140-160", "160-180", "180-200", "200-220")

# create dataframe to draw reference regresssion line
ref.df <- data.frame('diver.a' = c(0, 10, 20, 30, 40),
                     'diver.b' = c(0, 10, 20, 30, 40))

# df.4 <- time.swim.dat.final %>% 
#         left_join(diver.df) %>%
#         spread(sizeclass, sizeclass_freq)

# select data for diver pair of interest and diver A
diver.a.df <- time.swim.dat.final %>% 
        filter(sampdate > as.Date('2021-01-01')) %>%
        left_join(diver.df) %>% 
        filter(dive.pair.id == 4 & dive.buddy.id == 1) %>%
        select(c(site, sizeclass.2021, sizeclass_freq, diver)) %>% 
        dplyr::rename('diver.a' = sizeclass_freq)

# select data for diver pair of interest and diver B
diver.b.df <- time.swim.dat.final %>%
        filter(sampdate > as.Date('2021-01-01')) %>%
        left_join(diver.df) %>% 
        filter(dive.pair.id == 4 & dive.buddy.id == 2) %>% 
        select(c(site, sizeclass.2021, sizeclass_freq)) %>% 
        dplyr::rename('diver.b' = sizeclass_freq)

# join diver pair data
diver.pair <- left_join(diver.b.df, diver.a.df)

# create plot and label with diver pair
JM_PA <- diver.pair %>% 
        mutate(sizeclass = factor(sizeclass.2021, levels = sizeclasses.2021)) %>%
        ggplot(aes(diver.a, diver.b))+
        geom_point(aes(colour = factor(sizeclass.2021)), alpha = 1)+
        scale_colour_viridis_d()+
        geom_smooth(method='lm')+
        stat_poly_eq(formula = y~x, 
                     aes(label = paste(..rr.label.., p.value.label, sep = "~~~")), 
                     parse = TRUE) +
        geom_line(data = ref.df, aes(diver.a, diver.b), 
                  linetype = 'dashed', colour = 'blue', size = 1)+
        coord_cartesian(xlim = c(0, 40), ylim = c(0, 40))+
        xlab(paste('Diver A', 'count'))+
        ylab(paste('Diver B', 'count'))+
        labs(colour = 'Size class (mm)')+
        theme_bw()+
        guides(colour = guide_legend(ncol=2))

# extract common legend from one of the diver pair plots
leg <- get_legend(LT_BD)

# reformat plots by removing the legend
LT_BD_leg <- LT_BD +
        theme(legend.position = "none")
# NW_SI_leg <- SI_NW +
#         theme(legend.position = "none")
# SL_GP_leg <- SL_GP +
#         theme(legend.position = "none")
SL_AD_leg <- SL_AD +
    theme(legend.position = "none")
SL_PA_leg <- SL_PA +
    theme(legend.position = "none")
JM_PA_leg <- JM_PA +
    theme(legend.position = "none")

#arrange plots on the same page and add common legend in blank facet
# diver.bias.plot <- ggarrange(LT_BD_leg, NW_SI_leg, SL_GP_leg, leg, labels = c('A', 'B', 'C'))
diver.bias.plot <- ggarrange(LT_BD_leg, JM_PA_leg, SL_PA_leg, SL_AD_leg, leg, 
                             labels = c('A', 'B', 'C', 'D'))


# note: A = LT + BD, B = NW + SI, c = GP + SL

setwd('C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/FIS_TimedSwimSurvey2021_Plots')
ggsave(filename = paste('TimedSwimSurvey_2021_DiverBiasPlot', '.pdf', sep = ''), 
       plot = diver.bias.plot, units = 'mm', width = 250, height = 190)
ggsave(filename = paste('TimedSwimSurvey_2021_DiverBiasPlot', '.png', sep = ''), 
       plot = diver.bias.plot, units = 'mm', width = 250, height = 190)

##---------------------------------------------------------------------------##
df.1 <- time.swim.dat.final %>%
        left_join(diver.df, by = 'diver') %>%
        # mutate(sizeclass = factor(sizeclass, levels = sizeclasses)) %>%
        filter(diver.id) %>% 
        select(c(site, sizeclass, sizeclass_freq)) %>%
        dplyr::rename('DIVER A' = sizeclass_freq)

df.2 <- time.swim.dat %>% 
        mutate(sizeclass = factor(sizeclass, levels = sizeclasses)) %>%
        filter(diver %in% c(diver.b)) %>% 
        select(c(site, sizeclass, sizeclass_freq)) %>%
        dplyr::rename('DIVER B' = sizeclass_freq)

df.3 <- left_join(df.1, df.2)

LT_BD <- df.3 %>% 
        mutate(sizeclass = factor(sizeclass, levels = sizeclasses)) %>%
        ggplot(aes(`DIVER A`, `DIVER B`))+
        geom_point(aes(colour = factor(sizeclass)))+
        geom_smooth(method='lm')+
        coord_cartesian(xlim = c(0, 40), ylim = c(0, 40))+
        xlab(paste(diver.a, 'abalone_count'))+
        ylab(paste(diver.b, 'abalone_count'))+
        labs(colour = 'Size class (mm)')+
        theme_bw()

grid.arrange(LT_BD, NW_SI, SL_GP, ncol = 1)

catch <- 1905+11557+38908+119190+490208
topthree<- 38908+119190+490208
toptwo <- 119190+490208

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
## 2021 site selection ####

# determine hexcell IDs sampled for each block in 2020
sites.2020.keep <- time.swim.dat.final %>%
        filter(!subblockno %in% c('28B', '28C')) %>% 
        group_by(blockno) %>% 
        distinct(site.new, .keep_all = T) %>% 
        sample_n(15) %>% 
        select(-proposed.geom) %>% 
        st_as_sf()

st_write(sites.2020.keep,
         dsn = "C:/Users/jaimem/Documents/Abalone_research/AB_TimedSwimFIS/FIS_TIMEDSWIMSITES_2020REFERENCE_2021-05-17.gpkg",
         layer = "sites.2020.keep", driver = "GPKG", overwrite = T, delete_dsn = T)

# create vector of oid to be sampled in 2021 from 2020 sites sampled
sites.2020.keep.oid <- sites.2020.keep %>% 
        filter(!is.na(oid)) %>% 
        ungroup() %>% 
        pull(oid)

# create vector of oid sampled in 2020 to filter from 2021 random site selection
sites.2020.oid <- time.swim.dat.final %>%
        filter(!subblockno %in% c('28B', '28C')) %>% 
        group_by(blockno) %>% 
        distinct(site.new, .keep_all = T) %>% 
        filter(!is.na(oid)) %>% 
        ungroup() %>% 
        pull(oid)
##---------------------------------------------------------------------------##
## PLOT 2: CPUE ####
## estimate CPUE in kg/hr for each block using length-weight data from commercial catch sampling length-weight relationships

# load most recent commercial catch sampling compiled MM dataframe
compiledMM.df.final <- readRDS('C:/CloudStor/R_Stuff/MMLF/compiledMM.df.final.RDS')

# select length-weight data, removing obvious erroneous data or data collected from multiple blocks in a trip
lw.dat <- compiledMM.df.final %>% 
    filter(between(whole.weight, 200, 1500) &
               between(shell.length, 120, 210) &
               numblocks == 1) 

# calculate log of length and weight
lw.dat.log <- lw.dat %>% 
    mutate(log.sl = log(shell.length),
           log.wt = log(whole.weight))

# lw.dat.log %>% 
#         ggplot(aes(x = shell.length, y = whole.weight)) +
#         geom_point()
# 
# lw.dat.log %>% ggplot(aes(x = log.sl, y = log.wt, colour = blockno))+
#         geom_point()+
#         geom_smooth(method = lm)+
#         facet_wrap(~ blockno, ncol = 2)

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
lw.coeff <- lw.dat.coeff.blockno %>% 
    filter(blockno == 13) %>% 
    select(a, b) %>% 
    mutate(join.id = 1)

# join chosen regression parameters to timed swim data
time.swim.dat.df.lw <- time.swim.dat.final %>% 
    mutate(join.id = 1) %>% 
    left_join(., lw.coeff)

# saveRDS(time.swim.dat.df.lw, 'C:/Users/jaimem/Documents/Abalone_research/AB_TimedSwimFIS/TimedSwimData_LW_2020-09-16.RDS')

# estimate total weight of each sizeclass above 140 mm
time.swim.dat.df.wt <- time.swim.dat.df.lw %>% 
    filter(midsize >= 150) %>%
    mutate(est.weight = ((a * (midsize ^ b)) * sizeclass_freq) / 1000)

# quick summary of total kg estimated by block
time.swim.dat.df.wt %>%
    filter(!subblockno %in% c('28B', '28C')) %>%
    dplyr::group_by(blockno) %>% 
    dplyr::summarise(total.kg = round(sum(est.weight), digits = 2)) %>% 
    as.data.frame()

# estimate CPUE for each site
time.swim.cpue.site <- time.swim.dat.df.wt %>%
    filter(!subblockno %in% c('28B', '28C')) %>%
    dplyr::group_by(blockno, site, diver) %>% 
    dplyr::summarise(total.kg = round(sum(est.weight), digits = 2),
                     est.kg.hr = round((total.kg / max(time.elapsed)) * 60, digits = 2)) %>%   
    group_by(blockno, site) %>% 
    summarise(est.kg.hr = round(mean(est.kg.hr), digits = 2)) %>% 
    as.data.frame()

# estimate CPUE for each blockno
time.swim.cpue.blockno <- time.swim.cpue.site %>%
    group_by(blockno) %>% 
    summarise(est.kg.hr = round(mean(est.kg.hr), digits = 2)) %>% 
    as.data.frame()

# determine number of sites sampled for each blockno
time.swim.cpue.n <- time.swim.cpue.site %>% 
    group_by(blockno) %>% 
    summarise(n = n_distinct(site))

# plot CPUE estimate for each blocnkno
cpue.plot <- time.swim.cpue.site %>% 
    ggplot(aes(x = blockno, y = est.kg.hr)) +
    geom_boxplot(fill = "#56B4E9") +
    stat_summary(fun.y = mean, geom = 'point', shape = 19, size = 2, colour = 'red', fill = 'red')+
    theme_bw() +
    ylim(0, 200)+
    ylab(bquote('CPUE ('*~kg.hr^-1*')'))+
    xlab('Blockno')+
    geom_text(data = time.swim.cpue.n, aes(y = 200, label = n), color = 'black', size = 3)

setwd('C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/FIS_TimedSwimSurvey2021_Plots')
ggsave(filename = paste('TimedSwimSurvey_2021_CPUEPlot', '.pdf', sep = ''), 
       plot = cpue.plot, units = 'mm', width = 190, height = 120)
ggsave(filename = paste('TimedSwimSurvey_2021_CPUEPlot', '.png', sep = ''), 
       plot = cpue.plot, units = 'mm', width = 190, height = 120)

## PLOT 3: COUNT PER TEN MIN ####
## average count of all legal and sub-legal abalone per 10 min for each site within each blockno (i.e. the average between
## paired divers for each site)

# determine number of sites sampled for each blockno
time.swim.dat.n <- time.swim.dat.final %>% 
    filter(!subblockno %in% c('28B', '28C'),
           sampdate > as.Date('2021-01-01')) %>% 
    group_by(blockno) %>% 
    summarise(n = n_distinct(site))

## average count per 10 min for each blockno
count.plot <- time.swim.dat.final %>% 
    filter(!subblockno %in% c('28B', '28C'),
           sampdate > as.Date('2021-01-01')) %>% 
    # filter(midsize >= 150) %>% 
    group_by(blockno, site, diver) %>% 
    summarise(ab.n = sum(sizeclass_freq)) %>% 
    group_by(blockno, site, diver) %>% 
    summarise(mean.ab.n = mean(ab.n)) %>%  
    ggplot(aes(x = blockno, y = mean.ab.n, colour = diver))+
    geom_point(size = 3)+
    scale_colour_grey()+
    stat_summary(fun.y = mean, geom = 'point', shape = 19, size = 4, colour = 'red', fill = 'red')+
    theme_bw()+
    ylab(bquote('Average count (abalone.10'*~min^-1*')'))+
    xlab('Blockno')+
    ylim(0, 200)+
    geom_text(data = time.swim.dat.n, aes(y = 200, label = n), color = 'black', size = 3)+
    theme(legend.position = 'none')

setwd('C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/FIS_TimedSwimSurvey2021_Plots')
ggsave(filename = paste('TimedSwimSurvey_2020_TenMinuteCountPlot', '.pdf', sep = ''), 
       plot = count.plot, units = 'mm', width = 190, height = 120)
ggsave(filename = paste('TimedSwimSurvey_2020_TenMinuteCountPlot', '.png', sep = ''), 
       plot = count.plot, units = 'mm', width = 190, height = 120)

##---------------------------------------------------------------------------##
## PLOT: TIMED VS HISTROIC ####
## compare average timed swim counts with available historical count data at SAM research sites and the rating score used
## to select timed swim sites from GPS logger data

## average count vs cpue rating score
time.swim.dat.vs.cpue.plot <- time.swim.dat.final %>% 
    filter(!subblockno %in% c('28B', '28C'),
           sampdate < as.Date('2021-01-01')) %>% 
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
time.swim.dat.vs.sam.plot <- time.swim.dat.final %>%
    filter(!subblockno %in% c('28B', '28C'),
           sampdate < as.Date('2021-01-01')) %>% 
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

setwd('C:/CloudStor/R_Stuff/FIS/FIS_2020')
ggsave(filename = paste('TimedSwimSurvey_2020_TenMinuteCountvsCPUE', '.pdf', sep = ''), 
       plot = time.swim.dat.vs.cpue.plot, units = 'mm', width = 190, height = 120)
ggsave(filename = paste('TimedSwimSurvey_2020_TenMinuteCountvsCPUE', '.png', sep = ''), 
       plot = time.swim.dat.vs.cpue.plot, units = 'mm', width = 190, height = 120)
ggsave(filename = paste('TimedSwimSurvey_2020_TenMinuteCountvsSAM', '.pdf', sep = ''), 
       plot = time.swim.dat.vs.sam.plot, units = 'mm', width = 190, height = 120)
ggsave(filename = paste('TimedSwimSurvey_2020_TenMinuteCountvsSAM', '.png', sep = ''), 
       plot = time.swim.dat.vs.sam.plot, units = 'mm', width = 190, height = 120)

size.class <- '>140 mm'

time.swim.dat.vs.cpue.kg.hr.plot <- left_join(time.swim.dat.final, fis.site.cpue.oid.kg.hr) %>% 
    filter(!subblockno %in% c('28B', '28C'),
           sampdate < as.Date('2021-01-01')) %>% 
    group_by(site, diver, legal.size, cell.ntile, sam.count, cpue.kg.hr) %>% 
    summarise(ab.n = sum(sizeclass_freq)) %>% 
    group_by(site, legal.size, cell.ntile, sam.count, cpue.kg.hr) %>% 
    summarise(mean.ab.n = mean(ab.n))%>% 
    filter(!is.na(cell.ntile) &
               legal.size == size.class) %>%  
    ggplot(aes(x = cpue.kg.hr, y = mean.ab.n))+
    geom_point(aes(colour = cell.ntile), size = 3)+
    # scale_colour_manual(values = c("#999999", "#56B4E9"))+
    geom_smooth(method = 'lm', formula = y~x, se = T)+
    stat_poly_eq(formula = y~x, aes(label = paste(..rr.label.., p.value.label, sep = "~~~")), 
                 parse = TRUE, label.y = 0.9) +
    theme_bw()+
    ggtitle(size.class)+
    xlab(bquote('CPUE ('*~kg.hr^-1*')'))+
    ylab(bquote('Timed Swim average count (abalone.10'*~min^-1*')'))+
    labs(colour = 'Rank')+
    ylim(0, 60)+
    # geom_text(data = time.swim.dat.n, aes(y = 200, label = n), color = 'black', size = 3)+
    theme(legend.position = c(0.95, 0.85),
          plot.title = element_text(hjust = 0.05, vjust = -10))

ggsave(filename = paste('TimedSwimSurvey_2020_TenMinuteCountvsCPUE.kg.hr_legal', '.pdf', sep = ''), 
       plot = time.swim.dat.vs.cpue.kg.hr.plot, units = 'mm', width = 190, height = 120)
ggsave(filename = paste('TimedSwimSurvey_2020_TenMinuteCountvsCPUE.kg.hr_legal', '.png', sep = ''), 
       plot = time.swim.dat.vs.cpue.kg.hr.plot, units = 'mm', width = 190, height = 120)

##---------------------------------------------------------------------------##


## PLOT 4: COUNT PER TEN MIN SIZECLASS ####
## average count of all legal and sub-legal abalone per 10 min by sizeclass for each site within each blockno 
## (i.e. the average between paired divers for each site)

# determine mean count per 10 min by size class for each blockno
ten.min.mean <- time.swim.dat.final %>% 
    filter(!subblockno %in% c('28B', '28C'),
           sampdate > as.Date('2021-01-01')) %>% 
    # filter(midsize < 150) %>% 
    group_by(blockno, site, diver, legal.size) %>% 
    summarise(ab.n = sum(sizeclass_freq)) %>% 
    group_by(blockno, legal.size) %>% 
    summarise(mean.ab.n = mean(ab.n)) %>% 
    mutate(legal.size = factor(legal.size))

ten.min.mean.site <- time.swim.dat.final %>% 
    filter(!subblockno %in% c('28B', '28C'),
           sampdate > as.Date('2021-01-01')) %>% 
    # filter(midsize < 150) %>% 
    group_by(site, blockno, subblockno, diver, legal.size) %>% 
    summarise(ab.n = sum(sizeclass_freq)) %>% 
    group_by(site, blockno, subblockno, legal.size) %>% 
    summarise(mean.ab.n = mean(ab.n)) %>% 
    mutate(legal.size = factor(legal.size))%>% 
    as.data.frame()

count.plot.sizeclass <- time.swim.dat.final %>% 
    filter(!subblockno %in% c('28B', '28C'),
           sampdate > as.Date('2021-01-01')) %>% 
    # filter(midsize < 150) %>% 
    group_by(blockno, site, diver, legal.size) %>% 
    summarise(ab.n = sum(sizeclass_freq)) %>% 
    group_by(blockno, site, legal.size) %>% 
    summarise(mean.ab.n = mean(ab.n)) %>% 
    mutate(legal.size = factor(legal.size)) %>% 
    ggplot(aes(x = blockno, y = mean.ab.n))+
    # geom_boxplot()+
    geom_boxplot(aes(fill = legal.size), position = position_dodge(0.9))+
    scale_fill_manual(values = c("#999999", "#56B4E9"))+
    # stat_summary(fun.y = mean, geom = 'point', shape = 19, size = 4, colour = 'red', fill = 'red')+
    geom_point(data = ten.min.mean, aes(group = legal.size), shape = 19, size = 2, colour = 'red', fill = 'red', position = position_dodge(0.9))+
    theme_bw()+
    ylab(bquote('Average count (abalone.10'*~min^-1*')'))+
    xlab('Blockno')+
    ylim(0, 125)+
    geom_text(data = time.swim.dat.n, aes(y = 125, label = n), color = 'black', size = 3)+
    theme(legend.position="none")

setwd('C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/FIS_TimedSwimSurvey2021_Plots')
ggsave(filename = paste('TimedSwimSurvey_2021_TenMinuteCountSizeClassPlot', '.pdf', sep = ''), 
       plot = count.plot.sizeclass, units = 'mm', width = 190, height = 120)
ggsave(filename = paste('TimedSwimSurvey_2021_TenMinuteCountSizeClassPlot', '.png', sep = ''), 
       plot = count.plot.sizeclass, units = 'mm', width = 190, height = 120)

##---------------------------------------------------------------------------##

        