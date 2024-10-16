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

data_folder <- file.path(paste(sprintf('C:/Users/%s/Dropbox (UTAS Research)/DiveFisheries/Abalone/FISdata', 
                                            Sys.info()[["user"]])), paste('FIS_TimedSwimSurveys2021', sep = ''))

# load final site file with corrected site names
# this file includes CPUE and historical SAM sites labelled with site numbers running in sequence
# from south to north
ts.sites.2020 <- read.xlsx(paste(data_folder, "/timed swim sites_final.xlsx", sep = ''),
                             detectDates = T)

# load site file generated from spatial fishing data used on vessel plotter to conduct the timed swim surveys
# these sites are incorrectly ordered and were used for the 2020 surveys  
ts.site.cpue.2020 <- read.xlsx(paste(data_folder, "/timed swim sites_CPUE_original.xlsx", sep = ''),
                               detectDates = T)

# load site file generated from historical SAM research data used on vessel plotter to conduct the timed swim surveys
# these sites are incorrectly ordered and were used for the 2020 surveys 
ts.site.sam.2020 <- read.xlsx(paste(data_folder, "/timed swim sites_SAM_original.xlsx", sep = ''),
                              detectDates = T)

# load existing FIS sites
ts.site.fis.2020 <- read.xlsx(paste(data_folder, "/Abalone_FIS_Sites_Feb2021_JM.xlsx", sep = ''),
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
saveRDS(ts.site.sam.final.2020, paste(data_folder, '/ts.site.sam.final.2020.RDS', sep = ''))

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
saveRDS(ts.sites.final.2020, paste(data_folder, '/ts.sites.final.2020.RDS', sep = ''))

# load sites for 2021
ts.sites.2021 <- st_read(paste(data_folder, '/AB-FIS-TIMEDSWIMSITES-BLOCK16-28-2021-05-17.gpx', sep = ''), layer = 'waypoints')
ts.sites.2021.ref.29.30 <- st_read(paste(data_folder, '/AB-FIS-TIMEDSWIMSITES-BLOCK29-30-2021-05-17.gpx', sep = ''), layer = 'waypoints')
ts.sites.2021.ref.13.14 <- st_read(paste(data_folder, '/AB-FIS-TIMEDSWIMSITES-REFERENCE-2021-03-27.gpx', sep = ''), layer = 'waypoints')

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
saveRDS(ts.sites.final.2021, paste(data_folder, '/ts.sites.final.2021.RDS', sep = ''))

# join site files from each survey year
ts.sites.final.sf <- bind_rows(ts.sites.final.2020, ts.sites.final.2021) %>% 
        select(c(site, site.old, sampyear, geometry))

# save file
saveRDS(ts.sites.final.sf, paste(data_folder, '/ts.sites.final.sf.RDS', sep = ''))

# save GPKG file
ts.sites.final.sf <- readRDS(paste(data_folder, '/ts.sites.final.sf.RDS', sep = ''))

# transform final site data used for 2020 to GDA2020
ts.sites.final.sf <-ts.sites.final.sf %>% 
        st_transform(crs = GDA2020)

st_write(ts.sites.final.sf, 
         dsn = paste(data_folder, '/ts.sites.final.sf_2020-2021.gpkg', sep = ''), 
         layer = "ts.sites.final.sf", driver = "GPKG", overwrite = T, delete_dsn = T)

##---------------------------------------------------------------------------##
## 3. Site OID ####
## join original (unordered) cpue site data file used in 2020 survey to site data generated from GPS logger data
## to extract 'oid' and scoring metric rating/category (i.e. cell.ntile 3-5)

# load original site details generated from GPS logger data
ts.site.cpue.2020.oid <- read.xlsx(paste(data_folder, "/timed swim sites_CPUE_original_metadata.xlsx", sep = ''),
                               detectDates = T)

# load final site file with corrected site names
ts.sites.final.sf <- readRDS(paste(data_folder, '/ts.sites.final.sf.RDS', sep = ''))

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
saveRDS(ts.site.cpue.2020.join, paste(data_folder, '/ts.site.cpue.2020.join.RDS', sep = ''))

# save GPKG file
ts.site.cpue.2020.join <- readRDS(paste(data_folder, '/ts.site.cpue.2020.join.RDS', sep = ''))

st_write(ts.site.cpue.2020.join, 
         dsn = paste(data_folder, '/ts.site.cpue.2020.join.gpkg', sep = ''), 
         layer = "ts.site.cpue.2020.join", driver = "GPKG", overwrite = T, delete_dsn = T)

ts.site.cpue.2020.join %>% 
 st_transform(crs = st_crs(4326)) %>%
 sfheaders::sf_to_df(fill = T) %>% 
 dplyr::rename('latitude' = y,
               'longitude' = x) %>% 
 select(-c(sfg_id, point_id)) %>% 
 write.xlsx(file = paste(data_folder_2020, paste('/TimedSwimSites_EastCoast_CPUE_2020.xlsx', sep = ''), sep = ''), 
            sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)

##---------------------------------------------------------------------------##
## 4. Site SAM ####
## join original (unordered) sam site data file to sam sites used in 2020 survey to extract historical average counts

# load original sam site details and count data
ts.site.sam.2020 <- read.xlsx(paste(data_folder, "/timed swim sites_SAM_original.xlsx", sep = ''),
                               detectDates = T)

ts.site.sam.final.2020 <- readRDS(paste(data_folder, '/ts.site.sam.final.2020.RDS', sep = ''))

# load final site file with corrected site names
ts.sites.final.sf <- readRDS(paste(data_folder, '/ts.sites.final.sf.RDS', sep = ''))


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
saveRDS(ts.site.sam.2020.join, paste(data_folder, '/ts.site.sam.2020.join.RDS', sep = ''))

ts.site.sam.2020.join %>% 
 st_transform(crs = st_crs(4326)) %>%
 sfheaders::sf_to_df(fill = T) %>% 
 dplyr::rename('latitude' = y,
               'longitude' = x) %>% 
 select(-c(sfg_id, point_id)) %>% 
 write.xlsx(file = paste(data_folder_2020, paste('/TimedSwimSites_EastCoast_SAM_2020.xlsx', sep = ''), sep = ''), 
            sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)

##---------------------------------------------------------------------------##
# join 2020 cpue and sam data

ts_site_cpue_sam_2020 <- bind_rows(ts.site.cpue.2020.join, ts.site.sam.2020.join)

ts_site_cpue_sam_2020 <- ts_site_cpue_sam_2020 %>% 
 mutate(blockno = ifelse(!is.na(blockno), blockno, stat.block),
                          subblockno = ifelse(!is.na(subblockno), subblockno, paste(stat.block, sub.block, sep = ''))) %>% 
 st_transform(crs = st_crs(4326)) %>%
 sfheaders::sf_to_df(fill = T) %>% 
 dplyr::rename('latitude' = y,
                'longitude' = x) %>% 
 select(-c(stat.block, sub.block, sfg_id, point_id))

# remove periods from column names
names(ts_site_cpue_sam_2020) <- gsub(x = names(ts_site_cpue_sam_2020), pattern = '\\.', replacement = '_')

# Export Excel version of original 2020 CPUE site data
data_folder_2020 <- file.path(paste(sprintf('C:/Users/%s/Dropbox (UTAS Research)/DiveFisheries/Abalone/FISdata', 
                                            Sys.info()[["user"]])), paste('FIS_TimedSwimSurveys2020', sep = ''))

ts_site_cpue_sam_2020 %>% 
 write.xlsx(file = paste(data_folder_2020, paste('/TimedSwimSites_EastCoast_Final_2020.xlsx', sep = ''), sep = ''), 
            sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)

# filter five additional sites surveyed in 2021
ts_sites_add_2021 <- ts_site_cpue_sam_2020 %>% 
 filter(site %in% c('AB-22-52-A',
                    'AB-22-54-A',
                    'AB-22-56-A',
                    'AB-22-57-A',
                    'AB-22-58-A')) %>% 
 mutate(sampyear = 2021)

# Export Excel version of original 2020 CPUE site data
data_folder_2021 <- file.path(paste(sprintf('C:/Users/%s/Dropbox (UTAS Research)/DiveFisheries/Abalone/FISdata', 
                                            Sys.info()[["user"]])), paste('FIS_TimedSwimSurveys2021', sep = ''))

ts_sites_add_2021 %>% 
 write.xlsx(file = paste(data_folder_2021, paste('/TimedSwimSites_EastCoast_Add_2021.xlsx', sep = ''), sep = ''), 
            sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)

##---------------------------------------------------------------------------##
# join OID, SAM counts and CPUE data to 2021 site data

# load original cpue spatial site selection data used to generate new sites for 2021
# closed blocks 16-28
ts.2021.sites.16.28 <- read.xlsx(paste(data_folder, '/TimedSwimSites_Block16-28_2021.xlsx', sep = ''), 
                                 detectDates = T)
# reference blocks 29-30
ts.2021.sites.29.30 <- read.xlsx(paste(data_folder, '/TimedSwimSites_Block29-30.xlsx', sep = ''), 
                                 detectDates = T)
# reference blocks 13-14
ts.2021.sites.13.14 <- read.xlsx(paste(data_folder, '/FIS_ReferenceSites_Block13-14.xlsx', sep = ''), 
                                 detectDates = T)
# # reference sites
ts_ref_sites <- read.xlsx(paste(data_folder, '/TimedSwimSites_EastCoast_ReferenceSites_Final.xlsx', sep = ''),
                                 detectDates = T) %>% .[,-1] %>%
 dplyr::mutate(xcoord = longitude,
               ycoord = latitude) %>%
 st_as_sf(coords = c("xcoord", "ycoord"), crs = WGS84) %>%
 st_transform(crs = GDA2020) %>%
 sfheaders::sf_to_df(fill = T) %>%
 dplyr::rename('ycoord' = y,
               'xcoord' = x)
# 
# # five additional sites
# ts_add_2021 <- read.xlsx(paste(data_folder, '/TimedSwimSites_EastCoast_Add_2021.xlsx', sep = ''), 
#                           detectDates = T) %>% .[,-1] %>%
#  dplyr::mutate(xcoord = longitude,
#                ycoord = latitude) %>% 
#  st_as_sf(coords = c("xcoord", "ycoord"), crs = WGS84) %>% 
#  st_transform(crs = GDA2020) %>% 
#  sfheaders::sf_to_df(fill = T) %>% 
#  dplyr::rename('ycoord' = y,
#                'xcoord' = x)

# load final site file with corrected site names
ts.sites.final.sf <- readRDS(paste(data_folder, '/ts.sites.final.sf.RDS', sep = ''))


# join all new sites for 2021
ts.2021.sites.13.14 <- ts.2021.sites.13.14 %>% 
        mutate(xcoord_3 = xcoord_2,
               ycoord_3 = ycoord_2,
               xcoord = xcoord_3,
               ycoord = ycoord_3) %>% 
        select(-c(xcoord_3, ycoord_3))

ts.2021.sites <- bind_rows(ts.2021.sites.16.28, 
                           ts.2021.sites.29.30, 
                           ts.2021.sites.13.14)

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

ts.site.cpue.2020.join <- readRDS(paste(data_folder, '/ts.site.cpue.2020.join.RDS', sep = ''))

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
saveRDS(ts.site.cpue.2021.join, paste(data_folder, '/ts.site.cpue.2021.join.RDS', sep = ''))

st_write(ts.site.cpue.2021.join, 
         dsn = paste(data_folder, '/ts.site.cpue.2021.join.gpkg', sep = ''), 
         layer = "ts.site.cpue.2020.join", driver = "GPKG", overwrite = T, delete_dsn = T)


# replace reference sites with adjusted coordinates

ref_sites <- ts_ref_sites %>% 
 select(site_new) %>% 
 pull()

ts_site_cpue_2021_less_ref <- ts.site.cpue.2021.join %>% 
 filter(!(site %in% df_2)) %>% 
 dplyr::rename(cell_ntile = 'cell.ntile',
               cpue_kg_hr = 'cpue.kg.hr')

ts_ref_sites_sf <- ts_ref_sites %>% 
 st_as_sf(coords = c("xcoord", "ycoord"), crs = GDA2020) %>% 
 dplyr::rename(site = 'site_new')

ts_sites_2021_sf <- bind_rows(ts_site_cpue_2021_less_ref, ts_ref_sites_sf) %>% 
 select(-c(longitude, latitude, sfg_id, point_id))


# Export Excel version of original 2021 CPUE site data
data_folder_2021 <- file.path(paste(sprintf('C:/Users/%s/Dropbox (UTAS Research)/DiveFisheries/Abalone/FISdata', 
                                            Sys.info()[["user"]])), paste('FIS_TimedSwimSurveys2021', sep = ''))

ts_sites_2021_sf %>% 
 st_transform(crs = st_crs(4326)) %>%
 sfheaders::sf_to_df(fill = T) %>% 
 dplyr::rename('latitude' = y,
               'longitude' = x) %>% 
 select(-c(sfg_id, point_id)) %>% 
 write.xlsx(file = paste(data_folder_2021, paste('/TimedSwimSites_EastCoast_Final_2021.xlsx', sep = ''), sep = ''), 
            sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)

##---------------------------------------------------------------------------##

# re-examining data JM 2024-10-09

dat_check_folder <- file.path(paste(sprintf('C:/Users/%s/OneDrive - University of Tasmania/Desktop/TimedSwimSiteEmails/TS_Sites_Original', 
                                       Sys.info()[["user"]])), sep = '')

# Load hex cell data
hex_cell_dat <- st_read(file.path(paste(sprintf('C:/Users/%s/Dropbox (UTAS Research)/DiveFisheries/GIS/SpatialLayers/TimedSwimSites_Hexcell_BL_quantiles_Block16_28A_2020.gpkg', 
                                                Sys.info()[["user"]])), sep = ''))

# Find the centroid of each hex cell to create point/site location coordinates
hex_centroid <- st_centroid(hex_cell_dat)

# Re-create 2023 proposed site data from original GPX file due to original Excel data being overwritten 
# when generating 2023 Actaeons experimental survey sites.

# Load 2023 gpx site data
ts_sites_2023_gpx <- st_read(paste(dat_check_folder, '/TimedSwimSites_EastCoast_Final_2023_GPX.gpx', sep = ''), layer = 'waypoints') %>% 
 select(name, geometry) %>% 
 dplyr::rename(site = 'name') %>% 
 st_transform(crs = GDA2020) %>% 
 sfheaders::sf_to_df(fill = T) %>% 
 dplyr::rename('latitude' = y, 'longitude' = x) %>% 
 select(-c(sfg_id, point_id))
 
# Load reference sites
ts_ref_sites <- read.xlsx(paste(dat_check_folder, '/TimedSwimSites_EastCoast_ReferenceSites_Final.xlsx', sep = ''), 
                          detectDates = T)

# Convert reference sites to sf
ts_ref_site_dat <- ts_ref_sites %>% 
 select(site_new, blockno, subblockno, oid, cell_ntile, sam_count, longitude, latitude, ref_site) %>% 
 dplyr::rename(site = 'site_new') %>% 
 st_as_sf(coords = c("longitude", "latitude"), crs = GDA2020)

# Extract reference site IDs 
ts_ref_id <- ts_ref_site_dat %>% 
 select(site, ref_site)

# Use reference site IDs to filter from 2023 GPX proposed files as the OID will not
# match where the final reference site landed in the 2020 baseline survey. Reference site surveys
# post 2020 use the adjusted site coordinates.
ts_site_2023_no_ref <- left_join(ts_sites_2023_gpx, ts_ref_id) %>% 
 filter(is.na(ref_site)) %>% 
 select(-c(ref_site, geometry)) %>% 
 st_as_sf(coords = c("longitude", "latitude"), crs = GDA2020)

# Re-join GPX proposed 2023 site data to original hexcell data to compile the historical CPUE
# data
ts_site_gpx_join <- st_join(ts_site_2023_no_ref, hex_centroid, join = st_nearest_feature) %>% 
 mutate(blockno = as.character(blockno))

# Add reference sites to gpx data
ts_sites_2023_sf <- bind_rows(ts_site_gpx_join, ts_ref_site_dat)

# Create dataframe for Excel
ts_sites_2023_df <- ts_sites_2023_sf %>% 
 st_transform(crs = st_crs(4326)) %>%
 sfheaders::sf_to_df(fill = T) %>% 
 dplyr::rename('latitude' = y,
               'longitude' = x) %>% 
 select(-c(sfg_id, point_id))

# Save spatial layer
st_write(ts_sites_2023_sf, 
         dsn = paste(dat_check_folder, '/TimedSwimSites_EastCoast_Final_2023.gpkg', sep = ''), 
         layer = "TimedSwimSites_EastCoast_Final_2023", driver = "GPKG", overwrite = T, delete_dsn = T)

# Save Excel file
ts_sites_2023_df %>% 
 write.xlsx(file = paste(dat_check_folder, paste('/TimedSwimSites_EastCoast_Final_2023.xlsx', sep = ''), sep = ''), 
           sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)



