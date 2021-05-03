##---------------------------------------------------------------------------##
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
})

##---------------------------------------------------------------------------##
## 2. Load data ####

# load timed swim length frequency raw data
time.swim.dat <- read.xlsx("R:/TAFI/TAFI_MRL_Sections/Abalone/Section Shared/Abalone_databases/Data/Data for Transfer/2020/FIS_TimedSwim_RawData_2020.xlsx",
                       detectDates = T)

# load timed swim meta data
time.swim.meta.dat <- read.xlsx("R:/TAFI/TAFI_MRL_Sections/Abalone/Section Shared/Abalone_databases/Data/Data for Transfer/2020/FIS_TimedSwim_MetaData_2020.xlsx",
                           detectDates = T)
##---------------------------------------------------------------------------##
## 3. Data conversions ####

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
                      'finishtime' = time.out)

# calculate elapsed dive time
time.swim.dat <- time.swim.dat %>% 
        mutate(time.elapsed = ifelse(is.na(firstabtime), finishtime - starttime, finishtime - firstabtime))

# separate site names to identify blockno, including random or additional sites
time.swim.dat <- time.swim.dat %>% 
        mutate(site2 = site) %>% 
        separate(col = site2, into = c('ab', 'blockno'), sep = '-') %>% 
                select(-ab) %>% 
        mutate(blockno = ifelse(blockno %in% c(72, 74, 'LEG', 'THU'), 22, blockno))

# determine legal and sub-legal abalone
time.swim.dat <- time.swim.dat %>% 
        mutate(legal.size = ifelse(sizeclass %in% c("0-20", "20-40", "40-60", "60-80", "80-100", "100-120", "120-140"), '<140 mm', '>140 mm'))

# add mid-point to size classes
time.swim.dat <- time.swim.dat %>% 
        separate(sizeclass, into = c('minsize', 'maxsize'), sep = '-', convert = TRUE, remove = F) %>% 
        mutate(midsize = (minsize + maxsize)/2)

# create data frame of individual abalone lengths
time.swim.dat.df <- time.swim.dat %>% 
        uncount(sizeclass_freq, .remove = F) %>% 
        dplyr::rename('shelllength' = midsize)

##---------------------------------------------------------------------------##
## 4. Site rename ####
## the original site files used for the survey were a combination of historical SAM research data
## and fishery CPUE spatial data. Site names were given to these files prior to compiling them as a single
## file and consequently resulted in site numbering being randaom across the spatial extent of each block. The
## following code joins the original site files with a compiled version where the numbering of sites is in
## sequence from south to north, and re-labels the site names for future surveys. Several historical SAM
## sites also have duplicate site names; this code compiles these into a single site name.

# load final site file with corrected site names
# this file includes CPUE and historical SAM sites labelled with site numbers running in sequence
# from south to north
fis.sites.final <- read.xlsx("C:/Users/jaimem/Documents/Abalone_research/AB_TimedSwimFIS/timed swim sites_final.xlsx",
                             detectDates = T)

# load site file generated from spatial fishing data used on vessel plotter to conduct the timed swim surveys
# these sites are incorrectly ordered and were used for the 2020 surveys  
fis.site.cpue <- read.xlsx("C:/Users/jaimem/Documents/Abalone_research/AB_TimedSwimFIS/timed swim sites_CPUE_original.xlsx",
                           detectDates = T)

# load site file generated from historical SAM research data used on vessel plotter to conduct the timed swim surveys
# these sites are incorrectly ordered and were used for the 2020 surveys 
fis.site.sam <- read.xlsx("C:/Users/jaimem/Documents/Abalone_research/AB_TimedSwimFIS/timed swim sites_SAM_original.xlsx",
                          detectDates = T)

# set CRS
GDA2020 <- st_crs(7855)
WGS84 <- st_crs(4326)
UTM55S <- st_crs(32755)

# clean the SAM site file to match CPUE site data frame
# remove characters from sites names and rename coordinates
fis.site.sam <- fis.site.sam %>% 
        mutate(str_trim(name)) %>% 
        # mutate(name2 = paste(name,'-B', sep = '')) %>% 
        dplyr::rename('lat' = Latitude,
                      'long' = Longitude) %>% 
        # mutate(site = name) %>% 
        select(c(name, lat, long))

# identify duplicate site names which have the same coordinates
fis.site.sam.dup <- fis.site.sam %>%
        group_by(lat, long) %>%
        summarise(n = n()) %>% 
        left_join(fis.site.sam, .)

# join final site file with the site file used for the 2020 surveys and re-label with new site names
fis.sites.new.df <- left_join(fis.sites.final, fis.site.cpue, by = c('lat', 'long')) %>%    
        left_join(fis.site.sam.dup, by = c('lat', 'long')) %>% 
        mutate(name.y = ifelse(is.na(name.y), name, name.y)) %>% 
        dplyr::rename('site.new' = name.x,
                      'site' = name.y) %>%  
        select(-c(desc.y, desc.x, name))

fis.sites.new.sf <- fis.sites.new.df %>% 
        st_as_sf(coords = c("long", "lat"), crs = WGS84)

##---------------------------------------------------------------------------##
## 4.1 Site OID ####
## join original (unordered) cpue site data file used in 2020 survey to site data generated from GPS logger data
## to extract 'oid' and scoring metric rating/category (i.e. cell.ntile 3-5)

# load original site details generated from GPS logger data
fis.site.cpue.oid <- read.xlsx("C:/Users/jaimem/Documents/Abalone_research/AB_TimedSwimFIS/timed swim sites_CPUE_original_metadata.xlsx",
                               detectDates = T)

fis.site.cpue.oid.kg.hr <- fis.site.cpue.oid %>% 
        mutate(cpue.kg.hr = blkgtotal / (minstotal/60))

# convert original cpue site data to sf
fis.site.cpue.oid.sf <- fis.site.cpue.oid %>% 
        st_as_sf(coords = c("xcoord", "ycoord"), crs = GDA2020)

# convert cpue site data used for 2020 to sf
fis.site.cpue.sf <- fis.site.cpue %>% 
        st_as_sf(coords = c("long", "lat"), crs = WGS84) %>% 
        st_transform(GDA2020)

# join original and 2020 survey site data 
fis.site.cpue.join <- st_join(fis.site.cpue.oid.sf, fis.site.cpue.sf, join = st_nearest_feature) %>% 
        dplyr::rename('site' = name)

##---------------------------------------------------------------------------##
## 4.2 Site SAM ####
## join original (unordered) sam site data file to sam sites used in 2020 survey to extract historical  average counts

# load original sam site details and count data
fis.site.sam.data <- read.xlsx("C:/Users/jaimem/Documents/Abalone_research/AB_TimedSwimFIS/timed swim sites_SAM_original.xlsx",
                               detectDates = T)

# join original and 2020 sam survey site data 
fis.site.sam.join <- left_join(fis.site.sam.data, fis.site.sam)%>% 
        filter(!(is.na(Longitude) & is.na(Latitude))) %>% 
        st_as_sf(coords = c("Longitude", "Latitude"), crs = WGS84) %>% 
        st_transform(GDA2020)%>% 
        dplyr::rename('site' = name,
                      'sam.count' = Avg_count_per.10min)

##---------------------------------------------------------------------------##
## 5. Sites completed - proposed ####

# identify unique sites sampled from timed swim dataframe and join to renamed site file
time.swim.dat.unique <- time.swim.dat %>% 
        distinct(site, .keep_all = T) %>% 
        select(c(site, sampdate))

time.swim.dat.site.join <- left_join(fis.sites.new.sf, time.swim.dat.unique, by = c('site' = 'site'))

# identity sites sampled and colour code (green = sampled, red = not sampled)
# NOTE: these are colour coded for export to GPX and loading onto Lowrance plotter
time.swim.dat.site.sampled <- time.swim.dat.site.join %>% 
        mutate(color = ifelse(is.na(sampdate), 'red', 'green'),
               symbol = 'circle',
               sym.col = paste(symbol, color, sep = ',')) %>% 
        select(c(site.new, site, sym.col, geometry))

# transform to GDA2020
time.swim.dat.site.sampled <- st_transform(time.swim.dat.site.sampled, GDA2020)

# save spatial layer for QGIS
st_write(time.swim.dat.site.sampled, 
         dsn = "C:/Users/jaimem/Documents/Abalone_research/AB_TimedSwimFIS/FIS_TIMEDSWIMSITES_COMPLETED_2020-09-23.gpkg", 
         layer = "time.swim.dat.site.sampled", driver = "GPKG", overwrite = T, delete_dsn = T)

##---------------------------------------------------------------------------##
## 6. Sites completed - actual ####
## match timed swim data to GPS positions from vessel plotter downloads

# read GPX file from plotter
morana.gps.2020 <- st_read('C:/Users/jaimem/Documents/Abalone_research/AB_TimedSwimFIS/MORANAII-2020-10-09_download.gpx', layer = 'waypoints')
morana.gps.ref <- st_read('C:/Users/jaimem/Documents/Abalone_research/AB_TimedSwimFIS/MORANAII-2021-04-06.gpx', layer = 'waypoints')

# join GPX files
morana.gps <- bind_rows(morana.gps.2020, morana.gps.ref)

# filter GPX file for latest data
morana.gps <- morana.gps %>% 
        filter(time >= as.Date('2020-08-12'))

# separate start positions
time.swim.site.start <- time.swim.meta.dat %>%
        select(-c(waypoint.finish, finishtime)) %>% 
        dplyr::rename('waypoint' = waypoint.start,
                      'samptime' = starttime) %>% 
        mutate(sampperiod = 'start')

# separate finish positions
time.swim.site.finish <- time.swim.meta.dat %>%
        select(-c(waypoint.start, starttime)) %>% 
        dplyr::rename('waypoint' = waypoint.finish,
                      'samptime' = finishtime) %>% 
        mutate(sampperiod = 'finish')

# re-join start and finish positions  and remove non-sampled sites        
time.swim.site.start.finish <- bind_rows(time.swim.site.start, time.swim.site.finish) %>%
        mutate(waypoint = if_else(waypoint < 10, as.character(paste('00', waypoint, sep = '')),
                                  if_else(between(waypoint, 10, 100), as.character(paste(0, waypoint, sep = '')),
                                          as.character(waypoint)))) %>%  
        select(c(sampdate, samptime, sampperiod, site, waypoint)) %>% 
        filter(site != 'Betsey')

# join geometry where start waypoint recorded as being on the original GPS position/waypoint mark
time.swim.site.start.finish.mark <- time.swim.site.start.finish %>% 
        filter(is.na(waypoint) & sampperiod == 'start') %>% 
        left_join(., morana.gps, by = c('site' = 'name')) %>% 
        dplyr::rename('gpstime' = time,
                      'site' = site) %>%
        select(c(sampdate, samptime, gpstime, sampperiod, site, waypoint, geometry))

# join geometry where waypoints were recorded 
time.swim.site.start.finish.wp <- time.swim.site.start.finish %>% 
        filter(!is.na(waypoint)) %>% 
        left_join(., morana.gps, by = c('waypoint' = 'name')) %>% 
        dplyr::rename('gpstime' = time,
                      'site' = site) %>% 
        select(c(sampdate, samptime, gpstime, sampperiod, site, waypoint, geometry))

# re-join all waypoint data
time.swim.site.start.finish.loc <- bind_rows(time.swim.site.start.finish.mark, time.swim.site.start.finish.wp) %>% 
        st_as_sf()

# transform to GDA2020
time.swim.site.start.finish.loc <- st_transform(time.swim.site.start.finish.loc, GDA2020)

# save spatial layer for QGIS
# st_write(time.swim.site.start.finish.loc,
#          dsn = "C:/Users/jaimem/Documents/Abalone_research/AB_TimedSwimFIS/FIS_TIMEDSWIMSITES_STARTFINISH_2020-09-23.gpkg",
#          layer = "time.swim.site.start.finish.loc", driver = "GPKG", overwrite = T, delete_dsn = T)

##---------------------------------------------------------------------------##
## 7. MAP 1: Sites sampled - proposed ####
## create map of proposed sites sampled within each Block and identify subblockno

# read in Subblock map as an sf::sfc polygon object
sf.subblock.map <- st_read("C:/Users/jaimem/Dropbox/AbaloneData/SpatialLayers/SubBlockMaps.gpkg")

# transform map to GDA2020
sf.subblock.map <- st_transform(sf.subblock.map, GDA2020)

# separate site names to identify blockno, including random or additional sites
site.samp.loc <- time.swim.dat.site.sampled %>% 
        mutate(site2 = site) %>% 
        separate(col = site2, into = c('ab', 'blockno'), sep = '-') %>%  
        select(-ab) %>% 
        mutate(blockno = ifelse(blockno %in% c('THU'), 22, blockno))

# join data to subblock map to identify subblockno and remove fished sites in blockno 28
site.samp.subblock.loc <- st_join(site.samp.loc, sf.subblock.map, join = st_nearest_feature) %>% 
        select(-c(version, area, blockno.y)) %>% 
        dplyr::rename(blockno = blockno.x) %>% 
        rename_all(tolower)%>% 
        mutate(subblockno = ifelse(subblockno == '29A', '28C', subblockno)) %>% 
        filter(!subblockno %in% c('28C', '28B'))

# join data to subblock map to identify subblockno (inc. all block 28)
site.samp.subblock.loc.28 <- st_join(site.samp.loc, sf.subblock.map, join = st_nearest_feature) %>% 
        select(-c(version, area, blockno.y)) %>% 
        dplyr::rename(blockno = blockno.x) %>% 
        rename_all(tolower) %>% 
        mutate(subblockno = ifelse(subblockno == '29A', '28C', subblockno))

# identify blocks sampled
blocks.sampled <- unique(site.samp.loc$blockno)

# create maps

for (i in blocks.sampled){

site.samp.loc.blockno <- site.samp.subblock.loc %>% 
        filter(blockno == i) %>%  
        mutate(sym.col = factor(sym.col, levels = c('circle,red', 'circle,green')))

sf.subblock.map.crop <- sf.subblock.map %>% 
        filter(blockno == i &
                       !subblockno %in% c('28B', '28C'))

site.samp.map.blockno <- ggplot(data = st_geometry(sf.subblock.map.crop)) +
        geom_sf(fill = NA) +
        geom_sf_text(data = sf.subblock.map.crop, aes(label = subblockno))+
        geom_sf(data = site.samp.loc.blockno, aes(colour = sym.col)) +
        scale_colour_manual(values = c('red', 'blue'))+
        theme_bw() +
        annotation_scale(location = "bl", width_hint = 0.5) +
        annotation_north_arrow(location = "br", which_north = "true", 
                               pad_x = unit(0.05, "cm"), pad_y = unit(0.1, "cm"),
                               style = north_arrow_fancy_orienteering)+
        theme(legend.position="none")+
        xlab('Longitude')+
        ylab('Latitude')

setwd('C:/CloudStor/R_Stuff/FIS/FIS_2020')
ggsave(filename = paste('TimedSwimSurvey_2020_SitesSampled_Proposed_BlockNo_', i, '.pdf', sep = ''),
       plot = site.samp.map.blockno, units = 'mm', width = 190, height = 250)
ggsave(filename = paste('TimedSwimSurvey_2020_SitesSampled_Proposed_BlockNo_', i, '.png', sep = ''),
       plot = site.samp.map.blockno, units = 'mm', width = 190, height = 150)

}

not.surveyed.df <- site.samp.subblock.loc %>% 
        filter(sym.col == 'circle,red')

##---------------------------------------------------------------------------##
## 8. MAP 2: Sites sampled - actual ####
## create map of sites sampled within each Block with recorded GPS positions and identify subblockno

# read in Subblock map as an sf::sfc polygon object
sf.subblock.map <- st_read("C:/Users/jaimem/Dropbox/AbaloneData/SpatialLayers/SubBlockMaps.gpkg")

# transform map to GDA2020
sf.subblock.map <- st_transform(sf.subblock.map, GDA2020)

# separate site names to identify blockno, including random or additional sites
site.samp.start.loc <- time.swim.site.start.finish.loc %>% 
        mutate(site2 = site) %>% 
        separate(col = site2, into = c('ab', 'blockno'), sep = '-') %>%  
        select(-ab) %>% 
        mutate(blockno = ifelse(blockno %in% c('THU'), 22, blockno))

# join data to subblock map to identify subblockno and remove sites sampled in blockno 28B and 28C 
# (i.e. these subblocks were open to fishing)
site.samp.start.subblock.loc <- st_join(site.samp.start.loc, sf.subblock.map, join = st_nearest_feature) %>% 
        select(-c(version, area, blockno.y)) %>% 
        dplyr::rename(blockno = blockno.x) %>% 
        rename_all(tolower) %>% 
        filter(!subblockno %in% c('28C', '28B') &
                       sampperiod == 'start')

# join data to subblock map to identify subblockno (inc. all block 28)
site.samp.start.subblock.loc.28 <- st_join(site.samp.start.loc, sf.subblock.map, join = st_nearest_feature) %>% 
        select(-c(version, area, blockno.y)) %>% 
        dplyr::rename(blockno = blockno.x) %>% 
        rename_all(tolower) %>% 
        filter(sampperiod == 'start')

# identify blocks sampled
blocks.sampled <- unique(site.samp.start.subblock.loc$blockno)

# create maps

for (i in blocks.sampled){
        
        site.samp.loc.blockno <- site.samp.start.subblock.loc %>% 
                filter(blockno == i)
        
        sf.subblock.map.crop <- sf.subblock.map %>% 
                filter(blockno == i &
                               !subblockno %in% c('28B', '28C'))
        
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
        
        setwd('C:/CloudStor/R_Stuff/FIS/FIS_2020')
        ggsave(filename = paste('TimedSwimSurvey_2020_SitesSampled_GPS_BlockNo_', i, '.pdf', sep = ''),
               plot = site.samp.map.blockno, units = 'mm', width = 190, height = 250)
        ggsave(filename = paste('TimedSwimSurvey_2020_SitesSampled_GPS_BlockNo_', i, '.png', sep = ''),
               plot = site.samp.map.blockno, units = 'mm', width = 190, height = 150)
        
}


##---------------------------------------------------------------------------##
## 9. Final dataframe ####
## Join spatial site data (proposed and actual GPS sites) to count data and save final dataframe

time.swim.dat.final <- left_join(time.swim.dat, site.samp.subblock.loc.28, by = 'site') %>%   
        left_join(site.samp.start.subblock.loc.28, by = 'site') %>%
        select(-blockno) %>% 
        mutate(subblockno.x = ifelse(is.na(subblockno.x), subblockno.y, subblockno.x),
               zone.x = ifelse(is.na(zone.x), 'E', zone.x),
               sym.col = ifelse(is.na(sym.col), 'circle,green', sym.col)) %>% 
        dplyr::rename(proposed.geom = 'geometry.x',
                       actual.geom = 'geometry.y',
                      zone = 'zone.x',
                      subblockno = 'subblockno.x',
                      sampdate = 'sampdate.x',
                      blockno = 'blockno.x') %>% 
        mutate(actual.geom = ifelse(st_is_empty(actual.geom), proposed.geom, actual.geom)) %>% 
        select(c(site, sampdate, site.new, blockno, subblockno, diver, starttime, firstabtime, finishtime, time.elapsed,
                 sizeclass, sizeclass_freq, minsize, midsize, maxsize, legal.size, waypoint, actual.geom, proposed.geom))


time.swim.dat.df.final <- left_join(time.swim.dat.df, site.samp.subblock.loc.28, by = 'site') %>% 
        left_join(site.samp.start.subblock.loc.28, by = 'site') %>% 
        select(-blockno) %>% 
        mutate(subblockno.x = ifelse(is.na(subblockno.x), subblockno.y, subblockno.x),
               zone.x = ifelse(is.na(zone.x), 'E', zone.x),
               sym.col = ifelse(is.na(sym.col), 'circle,green', sym.col)) %>% 
        dplyr::rename(proposed.geom = 'geometry.x',
                      actual.geom = 'geometry.y',
                      zone = 'zone.x',
                      subblockno = 'subblockno.x',
                      sampdate = 'sampdate.x',
                      blockno = 'blockno.x') %>%
        mutate(actual.geom = ifelse(st_is_empty(actual.geom), proposed.geom, actual.geom)) %>%
        select(c(site, sampdate, site.new, blockno, subblockno, diver, starttime, firstabtime, finishtime, time.elapsed,
                sizeclass, sizeclass_freq, minsize, maxsize, shelllength, legal.size, waypoint, actual.geom, proposed.geom))

## join time swim data to original site data to include 'oid', 'cell.ntile' and 'sam.count'
time.swim.dat.final <- left_join(time.swim.dat.final, select(fis.site.cpue.join, site, oid, cell.ntile), by = ('site')) %>% 
        select(-c(geometry)) %>% 
        left_join(., select(fis.site.sam.join, site, sam.count), by = ('site')) %>% 
        select(-c(geometry))

time.swim.dat.df.final <- left_join(time.swim.dat.df.final, select(fis.site.cpue.join, site, oid, cell.ntile), by = ('site')) %>% 
        select(-c(geometry)) %>% 
        left_join(., select(fis.site.sam.join, site, sam.count), by = ('site')) %>% 
        select(-c(geometry))

saveRDS(time.swim.dat.final, 'C:/Users/jaimem/Documents/Abalone_research/AB_TimedSwimFIS/TimedSwimData_2020-10-05.RDS')
saveRDS(time.swim.dat.df.final, 'C:/Users/jaimem/Documents/Abalone_research/AB_TimedSwimFIS/TimedSwimData.df_2020-10-05.RDS')

time.swim.dat.final <-
        readRDS('C:/Users/jaimem/Documents/Abalone_research/AB_TimedSwimFIS/TimedSwimData_2020-10-05.RDS')

time.swim.dat.df.final <-
        readRDS('C:/Users/jaimem/Documents/Abalone_research/AB_TimedSwimFIS/TimedSwimData.df_2020-10-05.RDS')

##---------------------------------------------------------------------------##
## Join to historic length frequency data ####
# load most recent compiled MM dataframe
compiledMM.df.final <-
        readRDS('C:/CloudStor/R_Stuff/MMLF/compiledMM.df.final.RDS')

df.1 <- time.swim.dat.df.final %>% 
        dplyr::rename(msr.date = 'sampdate',
                      blocklist = 'blockno',
                      subblocklist = 'subblockno',
                      shell.length = 'shelllength') %>% 
        mutate(newzone = 'E',
               fishyear = 2020,
               daylist = as.character(msr.date),
               daylist_max = msr.date,
               numblocks = 1,
               numsubblocks = 1,
               species = 1,
               blocklist_1 = as.integer(blocklist),
               sizelimit = ifelse(blocklist %in% c(27, 28), 145, 138),
               datasource = 'ts2020') %>% 
        select(c(msr.date, fishyear, newzone, daylist, daylist_max, numblocks, blocklist, numsubblocks, 
                 subblocklist, shell.length, species, blocklist_1, sizelimit, datasource))

df.2 <- compiledMM.df.final %>% 
        mutate(datasource = 'mm')

df.3 <- bind_rows(df.2, df.1)

## create box plots for each block sampled in the stock assessment year and compare with historical records
unique.zones <- 'E'
unique.blocks <- c(16, 22, 23, 24, 27, 28)

for (i in unique.zones) {
        for (j in unique.blocks) {
                # select data for block
                plotdat.block <- df.3 %>%
                        filter(
                                newzone == i
                                & blocklist == j
                                & datasource == 'ts2020'
                                & between(shell.length, 0, 220))
                
                plotdat.block.historic <- df.3 %>%
                        filter(
                                newzone == i
                                & blocklist == j
                                & between(fishyear, 1980, stock.assessment.year - 1)
                                & between(shell.length, sizelimit - 5, 220)
                        )
                
                plotdat.block <- bind_rows(plotdat.block, plotdat.block.historic)
                
                # determine last sampling year
                last.fishyear <- max(plotdat.block.historic$fishyear)
                
                # determine number of sampling years
                n.fishyear <- length(unique(plotdat.block$fishyear))
                
                # generate plot for zone and block combination only if data is present
                
                if (nrow(plotdat.block) != 0) {
                        # convert required grouping variable to factor for boxplot
                        
                        plotdat.block$fishyear <- as.factor(plotdat.block$fishyear)
                        
                        # generate a count of records for each year to add to boxplot
                        
                        plotdat.n <- plotdat.block %>%
                                group_by(fishyear, blockno) %>%
                                summarize(n = n())
                        
                        # generate table of size limits for each year to add to boxplot
                        
                        size.limit <- plotdat.block %>% 
                                group_by(fishyear, blockno) %>% 
                                summarise(legal.min.length = max(sizelimit))
                        
                        # generate boxplot of shell lengths for chosen grouping variable
                        
                        mm.zone.boxplot <-
                                ggplot(plotdat.block, aes(x = fishyear, y = shell.length)) +
                                geom_boxplot(outlier.colour = "orange", outlier.size = 1.5) +
                                geom_text(
                                        data = plotdat.n,
                                        aes(y = 220, label = n),
                                        size = 3,
                                        angle = 90
                                ) +
                                # geom_hline(aes(yintercept = 132), colour = 'red', linetype = 'dotted')+
                                geom_point(data = size.limit, aes(x = fishyear, y = legal.min.length), 
                                           shape = 95, size = 7, colour = "red")+
                                geom_vline(
                                        aes(xintercept = ifelse(stock.assessment.year - last.fishyear >= 2,
                                                                n.fishyear - 0.5, '')),
                                        linetype = 'dashed',
                                        colour = 'red',
                                        size = 0.5)+
                                xlab('Year') +
                                ylab(paste('BlockNo', j, 'Shell Length (mm)')) +
                                coord_cartesian(ylim = c(0, 225)) +
                                theme_bw() +
                                theme(
                                        plot.title = element_text(hjust = 0.5),
                                        panel.grid.major = element_blank(),
                                        panel.grid.minor = element_blank(),
                                        axis.line = element_line(colour = "black"),
                                        axis.text.x = element_text(angle = 45, vjust = 0.5)
                                )
                        
                        # print(mm.zone.boxplot)
                        setwd('C:/CloudStor/R_Stuff/FIS/FIS_2020')
                        ggsave(
                                filename = paste(i, '_BlockNo', j, '_TimedSwim_Boxplot_2020', '.pdf', sep = ''),
                                plot = mm.zone.boxplot,
                                width = 7.4,
                                height = 5.57,
                                units = 'in'
                        )
                        ggsave(
                                filename = paste(i, '_BlockNo', j, '_TimedSwim_Boxplot_2020', '.png', sep = ''),
                                plot = mm.zone.boxplot,
                                width = 7.4,
                                height = 5.57,
                                units = 'in'
                        )
                }
                else{
                }
                
        }
}

##---------------------------------------------------------------------------##

## PLOT 1: LF ####
## Length frequency plot by block (mid-points)

# determine number of abalone recorded and number of sites sampled per block
block.ab.n <- time.swim.dat.df.final %>% 
        filter(!subblockno %in% c('28B', '28C')) %>% 
        group_by(blockno) %>% 
        summarise(ab.n = paste('n = ', n()))

block.site.n <- time.swim.dat.final %>% 
        filter(!subblockno %in% c('28B', '28C')) %>%
        group_by(blockno) %>% 
        summarise(site.n = paste('(', n_distinct(site), ')', sep = ''))

block.ab.site.n <- left_join(block.ab.n, block.site.n) %>% 
        mutate(n = paste(ab.n, site.n, sep = ' '))

# create length frequency plot
lf.plot <- time.swim.dat.df %>% ggplot(aes(shelllength, group = blockno)) +
        geom_bar(aes(y = ..prop.., stat = 'count'), width = 20, col = 'black', fill = '#EFC000FF')+
        # scale_x_binned()+
        geom_vline(aes(xintercept = ifelse(blockno %in% c('27', '28'), 145, 138)), linetype = 'dashed', colour = 'red', size = 0.5) +
        theme_bw() +
        facet_grid(blockno ~ .) +
        scale_y_continuous(breaks = seq(0, 0.4, 0.1), labels = seq(0, 40, 10))+
        xlab("Shell Length (mm)")+
        ylab(paste("Percentage (%)"))+
        geom_text(data = block.ab.site.n, aes(x = 200, y = 0.2, label = n), color = 'black', size = 3)

setwd('C:/CloudStor/R_Stuff/FIS/FIS_2020')
ggsave(filename = paste('TimedSwimSurvey_2020_SizeFrequencyPlot', '.pdf', sep = ''),
       plot = lf.plot, units = 'mm', width = 190, height = 250)
ggsave(filename = paste('TimedSwimSurvey_2020_SizeFrequencyPlot', '.png', sep = ''),
       plot = lf.plot, units = 'mm', width = 190, height = 250)
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

setwd('C:/CloudStor/R_Stuff/FIS/FIS_2020')
ggsave(filename = paste('TimedSwimSurvey_2020_CPUEPlot', '.pdf', sep = ''), 
       plot = cpue.plot, units = 'mm', width = 190, height = 120)
ggsave(filename = paste('TimedSwimSurvey_2020_CPUEPlot', '.png', sep = ''), 
       plot = cpue.plot, units = 'mm', width = 190, height = 120)

##---------------------------------------------------------------------------##
## PLOT 3: COUNT PER TEN MIN ####
## average count of all legal and sub-legal abalone per 10 min for each site within each blockno (i.e. the average between
## paired divers for each site)

# determine number of sites sampled for each blockno
time.swim.dat.n <- time.swim.dat.final %>% 
        filter(!subblockno %in% c('28B', '28C')) %>%
        group_by(blockno) %>% 
        summarise(n = n_distinct(site))

## average count per 10 min for each blockno
count.plot <- time.swim.dat.final %>% 
        filter(!subblockno %in% c('28B', '28C')) %>%
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

setwd('C:/CloudStor/R_Stuff/FIS/FIS_2020')
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
        filter(!subblockno %in% c('28B', '28C')) %>%
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
        filter(!subblockno %in% c('28B', '28C')) %>%
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
        filter(!subblockno %in% c('28B', '28C')) %>%
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
        filter(!subblockno %in% c('28B', '28C')) %>%
        # filter(midsize < 150) %>% 
        group_by(blockno, site, diver, legal.size) %>% 
        summarise(ab.n = sum(sizeclass_freq)) %>% 
        group_by(blockno, legal.size) %>% 
        summarise(mean.ab.n = mean(ab.n)) %>% 
        mutate(legal.size = factor(legal.size))

ten.min.mean.site <- time.swim.dat.final %>% 
        filter(!subblockno %in% c('28B', '28C')) %>%
        # filter(midsize < 150) %>% 
        group_by(site, blockno, subblockno, diver, legal.size) %>% 
        summarise(ab.n = sum(sizeclass_freq)) %>% 
        group_by(site, blockno, subblockno, legal.size) %>% 
        summarise(mean.ab.n = mean(ab.n)) %>% 
        mutate(legal.size = factor(legal.size))%>% 
        as.data.frame()

count.plot.sizeclass <- time.swim.dat.final %>% 
        filter(!subblockno %in% c('28B', '28C')) %>%
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

setwd('C:/CloudStor/R_Stuff/FIS/FIS_2020')
ggsave(filename = paste('TimedSwimSurvey_2020_TenMinuteCountSizeClassPlot', '.pdf', sep = ''), 
       plot = count.plot.sizeclass, units = 'mm', width = 190, height = 120)
ggsave(filename = paste('TimedSwimSurvey_2020_TenMinuteCountSizeClassPlot', '.png', sep = ''), 
       plot = count.plot.sizeclass, units = 'mm', width = 190, height = 120)
        
##---------------------------------------------------------------------------##
## TAB 1: SUMMARY ####
## summary table of counts and CPUE by size class and block (NOTE: run script for CPUE above)

# arrange size classes in order
sizeclasses <- c("0-20", "20-40", "40-60", "60-80", "80-100", "100-120", "120-140", "140-160", "160-180", "180-200", "200-220")

time.swim.count.blockno <- time.swim.dat.final %>%
        filter(!subblockno %in% c('28B', '28C')) %>%
        group_by(blockno, site, diver, legal.size) %>% 
        summarise(ab.measured = sum(sizeclass_freq),
                  swim.time = max(time.elapsed)) %>% 
        mutate(ab.cpue = (ab.measured / swim.time) * 10) %>% 
        group_by(blockno, legal.size) %>% 
        summarise(sites = n_distinct(site),
                  ab.min = round(mean(ab.cpue), digits = 2)) %>% 
        spread(legal.size, ab.min) %>%
        as.data.frame() %>% 
        dplyr::rename('Blockno' = blockno,
                      'Sites' = sites,
                      'Average\ncount\n<140mm' = '<140 mm',
                      'Average\ncount\n>140mm' = '>140 mm') %>% 
        adorn_totals(fill = '',,,, contains('Sites'))

time.swim.summary <- left_join(time.swim.count.blockno, time.swim.cpue.blockno) %>% 
        dplyr::rename('Blockno' = blockno,
               'Sites' = sites,
               'Average\ncount\n<140mm' = '<140 mm',
               'Average\ncount\n>140mm' = '>140 mm',
               'CPUE' = est.kg.hr) %>% 
        adorn_totals(fill = '',,,, contains('Sites'))

# create formated summary tables for report layout

time.swim.summary.tab <- time.swim.count.blockno %>% 
        ggpubr::ggtexttable(rows = NULL, theme = ggpubr::ttheme('mOrange'))

time.swim.summary.tab <- time.swim.summary %>% 
        ggpubr::ggtexttable(rows = NULL, theme = ggpubr::ttheme('mOrange'))

setwd('C:/CloudStor/R_Stuff/FIS/FIS_2020')
# ggsave(filename = paste('TimedSwimSurvey_2020_SummaryTable', '.pdf', sep = ''), 
#        plot = time.swim.summary.tab)
# ggsave(filename = paste('TimedSwimSurvey_2020_SummaryTable', '.png', sep = ''), 
#        plot = time.swim.summary.tab)

write.xlsx(time.swim.count.blockno, 'TimedSwimSurvey_2020_SummaryTable.xlsx', sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

ggsave(filename = paste('TimedSwimSurvey_2020_SummaryTable', '.pdf', sep = ''), 
       plot = time.swim.summary.tab, units = 'mm', width = 190, height = 120)
ggsave(filename = paste('TimedSwimSurvey_2020_SummaryTable', '.png', sep = ''), 
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
        distinct(site, .keep_all = T) %>% 
        select(c(site, blockno, subblockno, sampdate, actual.geom)) %>% 
        st_as_sf() %>% 
        st_set_crs(GDA2020)

# # transform to GDA2020
# time.swim.sites <- st_transform(time.swim.sites, GDA2020)

time.swim.count.site.loc <- left_join(ten.min.mean.site, time.swim.sites) %>% 
        st_as_sf() 

# identify blocks sampled
blocks.sampled <- unique(time.swim.count.site.loc$blockno)

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
        
        setwd('C:/CloudStor/R_Stuff/FIS/FIS_2020')
        ggsave(filename = paste('TimedSwimSurvey_2020_Site_LegalCount_BlockNo_', i, '.pdf', sep = ''),
               plot = count.site.map, units = 'mm', width = 190, height = 250)
        ggsave(filename = paste('TimedSwimSurvey_2020_Site_LegalCount_BlockNo_', i, '.png', sep = ''),
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
                geom_sf(data = sites.not.surveyed, shape = 17, size = 1.5, colour = 'red')+
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
                geom_sf(data = sites.not.surveyed, shape = 17, size = 1.5, colour = 'red')+
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

        setwd('C:/CloudStor/R_Stuff/FIS/FIS_2020')
        ggsave(filename = paste('TimedSwimSurvey_2020_SiteCountMap_BlockNo_', i, '.pdf', sep = ''),
               plot = count.site.map, units = 'mm', width = 250, height = 250)
        ggsave(filename = paste('TimedSwimSurvey_2020_SiteCountMap_BlockNo_', i, '.png', sep = ''),
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
fis.sam.data <- read.xlsx("C:/Users/jaimem/Documents/Abalone_research/AB_TimedSwimFIS/SampleDetailsforTimedSwimV2_JM.xlsx",
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
        filter(!subblockno %in% c('28B', '28C')) %>%
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

diver.df <- data.frame('diver' = c('LT', 'SI', 'SL', 'BD', 'NW', 'GP'),
                       'diver.id' = c(1, 2, 3, 4, 5, 6),
                       'dive.pair.id' = c(1, 2, 3, 1, 2, 3),
                       'dive.buddy.id' = c(1, 1, 1, 2, 2, 2))

sizeclasses <- c("0-20", "20-40", "40-60", "60-80", "80-100", "100-120", 
                 "120-140", "140-160", "160-180", "180-200", "200-220")

df.4 <- time.swim.dat.final %>% 
        left_join(diver.df) %>%
        spread(sizeclass, sizeclass_freq)

df.1 <- time.swim.dat.final %>% 
        left_join(diver.df) %>% 
        filter(dive.pair.id == 1 & dive.buddy.id == 1) %>% 
        select(c(site, sizeclass, sizeclass_freq)) %>% 
        dplyr::rename('diver.a' = sizeclass_freq)

df.2 <- time.swim.dat.final %>% 
        left_join(diver.df) %>% 
        filter(dive.pair.id == 1 & dive.buddy.id == 2) %>% 
        select(c(site, sizeclass, sizeclass_freq)) %>% 
        dplyr::rename('diver.b' = sizeclass_freq)

df.3 <- left_join(df.2, df.1)


df.3 %>% 
        mutate(sizeclass = factor(sizeclass, levels = sizeclasses)) %>%
        ggplot(aes(diver.a, diver.b))+
        geom_point(aes(colour = factor(sizeclass)))+
        geom_smooth(method='lm')+
        coord_cartesian(xlim = c(0, 40), ylim = c(0, 40))+
        xlab(paste('diver.a', 'abalone_count'))+
        ylab(paste('diver.b', 'abalone_count'))+
        labs(colour = 'Size class (mm)')+
        theme_bw()

grid.arrange(LT_BD, NW_SI, SL_GP, ncol = 1)

##---------------------------------------------------------------------------##
df.1 <- time.swim.dat.final %>%
        left_join(diver.df, by = 'diver') %>%
        # mutate(sizeclass = factor(sizeclass, levels = sizeclasses)) %>%
        filter(diver.id %>% 
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

        