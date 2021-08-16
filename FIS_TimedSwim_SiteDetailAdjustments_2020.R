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
## 2. Site rename ####
## the original site files used for the 2020 survey were a combination of historical SAM research data
## and fishery CPUE spatial data. Site names were given to these files prior to compiling them as a single
## file and consequently resulted in site numbering being randaom across the spatial extent of each block. The
## following code joins the original site files with a compiled version where the numbering of sites is in
## sequence from south to north, and re-labels the site names for future surveys. Several historical SAM
## sites also have duplicate site names; this code compiles these into a single site name.

# load final site file with corrected site names
# this file includes CPUE and historical SAM sites labelled with site numbers running in sequence
# from south to north
ts.sites.2020 <- read.xlsx("C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/timed swim sites_final.xlsx",
                             detectDates = T)

# load site file generated from spatial fishing data used on vessel plotter to conduct the timed swim surveys
# these sites are incorrectly ordered and were used for the 2020 surveys  
ts.site.cpue.2020 <- read.xlsx("C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/timed swim sites_CPUE_original.xlsx",
                           detectDates = T)

# load site file generated from historical SAM research data used on vessel plotter to conduct the timed swim surveys
# these sites are incorrectly ordered and were used for the 2020 surveys 
ts.site.sam.2020 <- read.xlsx("C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/timed swim sites_SAM_original.xlsx",
                          detectDates = T)

# load existing FIS sites
ts.site.fis.2020 <- read.xlsx("C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/Abalone_FIS_Sites_Feb2021_JM.xlsx",
                          detectDates = T)

# set CRS
GDA2020 <- st_crs(7855)
WGS84 <- st_crs(4326)
UTM55S <- st_crs(32755)

# clean the SAM site file to match CPUE site data frame
# remove characters from sites names and rename coordinates
ts.site.sam.2020 <- ts.site.sam.2020 %>% 
 mutate(str_trim(name)) %>% 
 # mutate(name2 = paste(name,'-B', sep = '')) %>% 
 dplyr::rename('lat' = Latitude,
               'long' = Longitude) %>% 
 # mutate(site = name) %>% 
 select(c(name, lat, long))

# identify duplicate site names which have the same coordinates
ts.site.sam.final.2020 <- ts.site.sam.2020 %>%
 group_by(lat, long) %>%
 summarise(n = n()) %>% 
 left_join(ts.site.sam.2020, .) %>% 
        dplyr::rename('site.old' = name) %>% 
        left_join(., ts.sites.2020, by = c('lat', 'long')) %>% 
        dplyr::rename('site' = name) %>% 
        select(site, site.old, lat, long, n)

# save file
saveRDS(ts.site.sam.final.2020, 'C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/ts.site.sam.final.2020.RDS')

# join final site file with the site file used for the 2020 surveys and re-label with new site names
ts.sites.new.2020.df <- left_join(ts.sites.2020, ts.site.cpue.2020, by = c('lat', 'long')) %>%    
 left_join(ts.site.sam.final.2020, by = c('lat', 'long')) %>%  
 mutate(name.y = ifelse(is.na(name.y), site.old, name.y)) %>% 
        select(-c(site.old, site)) %>% 
 dplyr::rename('site' = name.x,
               'site.old' = name.y) %>% 
 select(-c(desc.y, desc.x))

ts.sites.final.2020 <- ts.sites.new.2020.df %>% 
 st_as_sf(coords = c("long", "lat"), crs = WGS84) %>% 
 mutate(sampyear = 2020) %>% 
        select(-n)

# save file
saveRDS(ts.sites.final.2020, 'C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/ts.sites.final.2020.RDS')

# load sites for 2021
ts.sites.2021 <- st_read('C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/AB-FIS-TIMEDSWIMSITES-BLOCK16-28-2021-05-17.gpx', layer = 'waypoints')
ts.sites.2021.ref.29.30 <- st_read('C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/AB-FIS-TIMEDSWIMSITES-BLOCK29-30-2021-05-17.gpx', layer = 'waypoints')
ts.sites.2021.ref.13.14 <- st_read('C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/AB-FIS-TIMEDSWIMSITES-REFERENCE-2021-03-27.gpx', layer = 'waypoints')

ts.sites.2021 <- ts.sites.2021 %>% 
 mutate(name = gsub('AB2', 'AB-2', name,))

ts.sites.final.2021 <- bind_rows(ts.sites.2021, ts.sites.2021.ref.29.30, ts.sites.2021.ref.13.14) %>% 
 select(c(name, desc, geometry)) %>% 
 dplyr::rename('site' = name,
               'site.old' = desc) %>% 
 mutate(site = ifelse(is.na(site), site.new, site)) %>%
 st_as_sf() %>% 
 mutate(sampyear = 2021)


# add additional sites accidentally sampled in 2021 due to old site file loaded on vessel plotter
ts.add.sites.2021 <- ts.sites.final.2020 %>% 
        filter(site %in% c('AB-22-52-A',
                           'AB-22-54-A',
                           'AB-22-56-A',
                           'AB-22-57-A',
                           'AB-22-58-A')) %>% 
        mutate(sampyear = 2021)

ts.sites.final.2021 <- bind_rows(ts.sites.final.2021, ts.add.sites.2021)

# save file
saveRDS(ts.sites.final.2021, 'C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/ts.sites.final.2021.RDS')

# join site files from each survey year
ts.sites.final.sf <- bind_rows(ts.sites.final.2020, ts.sites.final.2021) %>% 
        select(c(site, site.old, sampyear, geometry))

# save file
saveRDS(ts.sites.final.sf, 'C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/ts.sites.final.sf.RDS')

# save GPKG file
ts.sites.final.sf <- readRDS('C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/ts.sites.final.sf.RDS')

# transform final site data used for 2020 to GDA2020
ts.sites.final.sf <-ts.sites.final.sf %>% 
        st_transform(crs = GDA2020)

st_write(ts.sites.final.sf, 
         dsn = 'C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/ts.sites.final.sf.gpkg', 
         layer = "ts.sites.final.sf", driver = "GPKG", overwrite = T, delete_dsn = T)

##---------------------------------------------------------------------------##
## 3. Site OID ####
## join original (unordered) cpue site data file used in 2020 survey to site data generated from GPS logger data
## to extract 'oid' and scoring metric rating/category (i.e. cell.ntile 3-5)

# load original site details generated from GPS logger data
ts.site.cpue.2020.oid <- read.xlsx("C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/timed swim sites_CPUE_original_metadata.xlsx",
                               detectDates = T)

# load final site file with corrected site names
ts.sites.final.sf <- readRDS('C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/ts.sites.final.sf.RDS')

# calculate cpue
ts.site.cpue.2020.oid <- ts.site.cpue.2020.oid %>% 
 mutate(cpue.kg.hr = blkgtotal / (minstotal/60))

# set CRS
GDA2020 <- st_crs(7855)
WGS84 <- st_crs(4326)
UTM55S <- st_crs(32755)

# convert original cpue site data to sf
ts.site.cpue.2020.oid.sf <- ts.site.cpue.2020.oid %>% 
 st_as_sf(coords = c("xcoord", "ycoord"), crs = GDA2020)

# transform final site data used for 2020 to GDA2020
ts.sites.final.sf <-ts.sites.final.sf %>% 
        st_transform(crs = GDA2020)

# join original cpue oid data and 2020 corrected final site data 
ts.site.cpue.2020.join <- st_join(ts.site.cpue.2020.oid.sf, ts.sites.final.sf, join = st_nearest_feature) %>%
        mutate(sampyear = 2020) %>%  
        select(c(site, site.old, sampyear, zone, blockno, subblockno, dist_to_coast, oid, cell.ntile,
                 blkgtotal, daystotal, minstotal, cpue.kg.hr, geometry))

# save file
saveRDS(ts.site.cpue.2020.join, 'C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/ts.site.cpue.2020.join.RDS')

# save GPKG file
ts.site.cpue.2020.join <- readRDS('C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/ts.site.cpue.2020.join.RDS')

st_write(ts.site.cpue.2020.join, 
         dsn = 'C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/ts.site.cpue.2020.join.gpkg', 
         layer = "ts.site.cpue.2020.join", driver = "GPKG", overwrite = T, delete_dsn = T)

##---------------------------------------------------------------------------##
## 4. Site SAM ####
## join original (unordered) sam site data file to sam sites used in 2020 survey to extract historical average counts

# load original sam site details and count data
ts.site.sam.2020 <- read.xlsx("C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/timed swim sites_SAM_original.xlsx",
                               detectDates = T)

ts.site.sam.final.2020 <- readRDS('C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/ts.site.sam.final.2020.RDS')

# load final site file with corrected site names
ts.sites.final.sf <- readRDS('C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/ts.sites.final.sf.RDS')


# join original and 2020 sam survey site data 

ts.site.sam.2020.join <- ts.sites.final.sf %>% 
        filter(sampyear == 2020) %>% 
        left_join(ts.site.sam.2020, ., by = c('name' = 'site.old')) %>% 
        filter(!(is.na(Longitude) & is.na(Latitude))) %>% 
        st_as_sf(coords = c("Longitude", "Latitude"), crs = WGS84) %>% 
        st_transform(GDA2020)%>% 
        dplyr::rename('sam.site' = Site,
                      'sam.date' = Date,
                      'site.old' = name,
                      'sam.count' = Avg_count_per.10min) %>% 
        rename_all(tolower) %>% 
        select(site, site.old, sam.site, sam.date, activity, proj.code, site.id,
               stat.block, sub.block, collector, sam_comments, species, number_of_abs,
               avg_shelllength, total_dive_time, sam.count, sampyear, geometry)

# save file
saveRDS(ts.site.sam.2020.join, 'C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/ts.site.sam.2020.join.RDS')

##---------------------------------------------------------------------------##
# join OID, SAM counts and CPUE data to 2021 site data

# load original cpue spatial site selection data used to generate new sites for 2021
# closed blocks 16-28
ts.2021.sites.16.28 <- read.xlsx('C:/Users/jaimem/OneDrive - University of Tasmania/Documents/AB_TimedSwimFIS/TimedSwimSites_Block16-28_2021.xlsx', 
                                 detectDates = T)
# reference blocks 29-30
ts.2021.sites.29.30 <- read.xlsx('C:/Users/jaimem/OneDrive - University of Tasmania/Documents/AB_TimedSwimFIS/TimedSwimSites_Block29-30.xlsx', 
                                 detectDates = T)
# reference blocks 13-14
ts.2021.sites.13.14 <- read.xlsx('C:/Users/jaimem/OneDrive - University of Tasmania/Documents/AB_TimedSwimFIS/FIS_ReferenceSites_Block13-14.xlsx', 
                                 detectDates = T)

# load final site file with corrected site names
ts.sites.final.sf <- readRDS('C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/ts.sites.final.sf.RDS')

# join all new sites for 2021
ts.2021.sites.13.14 <- ts.2021.sites.13.14 %>% 
        mutate(xcoord_3 = xcoord_2,
               ycoord_3 = ycoord_2,
               xcoord = xcoord_3,
               ycoord = ycoord_3) %>% 
        select(-c(xcoord_3, ycoord_3))

ts.2021.sites <- bind_rows(ts.2021.sites.16.28, ts.2021.sites.29.30, ts.2021.sites.13.14)

# calculate cpue
ts.2021.sites.kg.hr <- ts.2021.sites %>% 
        mutate(cpue.kg.hr = blkgtotal / (minstotal/60))

# set CRS
GDA2020 <- st_crs(7855)
WGS84 <- st_crs(4326)
UTM55S <- st_crs(32755)

# convert new cpue 2021 site data to sf
ts.sites.cpue.new.2021.sf <- ts.2021.sites.kg.hr %>% 
        st_as_sf(coords = c("xcoord", "ycoord"), crs = GDA2020)

# transform final site data used for 2021 to GDA2020
ts.sites.final.sf <-ts.sites.final.sf %>% 
        st_transform(crs = GDA2020)

# join new cpue 2021 data to original cpue site selection survey site data 

ts.sites.cpue.new.2021.join <- ts.sites.final.sf %>% 
        filter(sampyear == 2021) %>% 
        st_join(ts.sites.cpue.new.2021.sf, ., join = st_nearest_feature) %>% 
        select(c(site, sampyear, zone, blockno, subblockno, dist_to_coast, oid, cell.ntile,
                 blkgtotal, daystotal, minstotal, cpue.kg.hr, geometry))

# identify sites from cpue 2020 data resurveyed in 2021

ts.sites.cpue.old.2021 <- ts.sites.final.sf %>% 
        filter(!is.na(site.old) &
                       sampyear == 2021) %>% 
        st_set_geometry(NULL) %>% 
        select(site)

# load corrected final site data with original cpue oid data 2020  

ts.site.cpue.2020.join <- readRDS('C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/ts.site.cpue.2020.join.RDS')

# join resurveyed sites to original site selection survey site data 2020

ts.sites.cpue.old.2021.join <- left_join(ts.sites.cpue.old.2021, ts.site.cpue.2020.join, by = 'site') %>% 
        filter(!is.na(oid)) %>% 
        select(-site.old)

# join new site data and resurveyed site data 2021

ts.site.cpue.2021.join <- bind_rows(ts.sites.cpue.new.2021.join, ts.sites.cpue.old.2021.join)

# correct sample year for 2020 re-surveyed sites
ts.site.cpue.2021.join <- ts.site.cpue.2021.join %>% 
        mutate(sampyear = 2021)

# save file
saveRDS(ts.site.cpue.2021.join, 'C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/ts.site.cpue.2021.join.RDS')

##---------------------------------------------------------------------------##


