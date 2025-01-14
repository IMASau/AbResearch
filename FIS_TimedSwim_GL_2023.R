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
        library(RColorBrewer)
        library(viridis)
        library(ggpmisc)
        library(arsenal)
 library(GGally)
 library(ggExtra)
  library(fuzzyjoin)
 library(tidytext)
})

source("C:/GitCode/AbResearch/getLegend.r")
source("C:/GitCode/AbResearch/StandardError_Functions.r")

##---------------------------------------------------------------------------##
## 1. Set sample year and file paths ####

# identify sampling year of interest
samp_year <- 2024

# identify associated sampling year folder path to save dataframes
data_folder <- file.path(paste(sprintf('C:/Users/%s/Dropbox (UTAS Research)/DiveFisheries/Abalone/FISdata', 
                                            Sys.info()[["user"]])), paste('FIS_TimedSwimSurveys', samp_year, sep = ''),
                         'TimedSwim_NEGreens')

# identify associated sampling year folder path to save plots
plots_folder <- file.path(paste(sprintf('C:/Users/%s/Dropbox (UTAS Research)/DiveFisheries/Projects/AIRF_2022_53/DataAnalysis/Figures',
                                        Sys.info()[["user"]])), sep = '')
# identify associated spatial layers folder
spat_layer_folder <- file.path(paste(sprintf('C:/Users/%s/Dropbox (UTAS Research)/DiveFisheries/GIS/SpatialLayers/TimeSwimLayers', 
                        Sys.info()[["user"]])))
##---------------------------------------------------------------------------##
## 2. Load raw data ####

# load timed swim length frequency raw data
ts_dat <- read.xlsx("R:/TAFI/TAFI_MRL_Sections/Abalone/Section Shared/Abalone_databases/Data/Data for Transfer/2020/FIS_TimedswimsGreens_rawdata_2023.xlsx",
                           detectDates = T)

# load timed swim meta data
ts_meta_dat <- read.xlsx("R:/TAFI/TAFI_MRL_Sections/Abalone/Section Shared/Abalone_databases/Data/Data for Transfer/2020/FIS_TimedSwimGreens_metadata_2023.xlsx",
                                detectDates = T)
##---------------------------------------------------------------------------##
# remove blank or additional rows from raw data where cells have been pre-filled
# for data entry.This step can be deleted once data entry is complete.
ts_dat <- ts_dat %>% 
        filter(!is.na(start_time) &
                       !is.na(finish_time) &
                       !is.na(size_class_freq))
##---------------------------------------------------------------------------##
# clean site data by removing white space from Excel data entry
ts_dat <- ts_dat %>% 
        mutate(site = str_trim(site))

ts_meta_dat <- ts_meta_dat %>% 
        mutate(site = str_trim(site))

##---------------------------------------------------------------------------##
## 3. Raw data conversions ####

# re-format Excel times for ts_dat
ts_dat$start_time <- convertToDateTime(ts_dat$start_time, origin = "1970-01-01", tz = "Australia/HOBART")
ts_dat$first_ab_time <- convertToDateTime(ts_dat$first_ab_time, origin = "1970-01-01", tz = "Australia/HOBART")
ts_dat$finish_time <- convertToDateTime(ts_dat$finish_time, origin = "1970-01-01", tz = "Australia/HOBART")

ts_dat <- ts_dat %>% 
        mutate(start_time = strftime(start_time, format="%H:%M:%S"),
               first_ab_time = strftime(first_ab_time, format="%H:%M:%S"),
               finish_time = strftime(finish_time, format="%H:%M:%S"))

ts_dat$start_time <- as.POSIXct(paste(ts_dat$samp_date, ts_dat$start_time), format = "%Y-%m-%d %H:%M:%S")
ts_dat$first_ab_time <- as.POSIXct(paste(ts_dat$samp_date, ts_dat$first_ab_time), format = "%Y-%m-%d %H:%M:%S")
ts_dat$finish_time <- as.POSIXct(paste(ts_dat$samp_date, ts_dat$finish_time), format = "%Y-%m-%d %H:%M:%S")

# re-format Excel times for ts_meta_dat
ts_meta_dat$time_in <- convertToDateTime(ts_meta_dat$time_in, origin = "1970-01-01", tz = "Australia/HOBART")
ts_meta_dat$time_out <- convertToDateTime(ts_meta_dat$time_out, origin = "1970-01-01", tz = "Australia/HOBART")

ts_meta_dat <- ts_meta_dat %>% 
        mutate(time_in = strftime(time_in, format="%H:%M:%S"),
               time_out = strftime(time_out, format="%H:%M:%S"))

ts_meta_dat$time_in <- as.POSIXct(paste(ts_meta_dat$date, ts_meta_dat$time_in), format = "%Y-%m-%d %H:%M:%S")
ts_meta_dat$time_out <- as.POSIXct(paste(ts_meta_dat$date, ts_meta_dat$time_out), format = "%Y-%m-%d %H:%M:%S")

# re-label meta data date and time labels to match raw data
ts_meta_dat <- ts_meta_dat %>% 
        dplyr::rename('samp_date' = date,
                      'start_time' = time_in,
                      'finish_time' = time_out)

# calculate elapsed dive time (seconds)
ts_dat <- ts_dat %>% 
        mutate(time_elapsed = ifelse(!is.na(first_ab_time), finish_time - first_ab_time, finish_time - start_time))

# add sample year
ts_dat <- ts_dat %>% 
        mutate(sampyear = year(samp_date))

# extract blockno from site name
ts_dat <- ts_dat %>% 
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
ts_dat <- ts_dat %>% 
        mutate(legal_size = ifelse(size_class %in% c("0-70", "70-90", "90-110", "110-130", "130-150"), '<150 mm', '>150 mm'))

# add mid-point to size classes
ts_dat <- ts_dat %>% 
        separate(size_class, into = c('min_size', 'max_size'), sep = '-', convert = TRUE, remove = F) %>% 
        mutate(mid_size = (min_size + max_size)/2)

# convert counts to numeric
ts_dat <- ts_dat %>% 
 mutate(size_class_freq = as.numeric(size_class_freq))

# create data frame of individual abalone lengths
ts_dat_df <- ts_dat %>% 
 mutate(size_class_freq = as.numeric(size_class_freq)) %>% 
        uncount(size_class_freq, .remove = F) %>% 
        dplyr::rename('shell_length' = mid_size)

##---------------------------------------------------------------------------##
## 11. Standardise data #### 

# Standardise counts for 10 minute swim (i.e. some swims marginally shorter or longer duration)
std_ts_dat <- ts_dat %>% 
 mutate(sizeclass_freq_10 = round((size_class_freq / time_elapsed) * 10),
        sizeclass_freq_10 = replace(sizeclass_freq_10, is.na(sizeclass_freq_10), 0))

##---------------------------------------------------------------------------##
# save dataframes
saveRDS(ts_dat, paste(data_folder, '/ts_dat.RDS', sep = ''))
saveRDS(std_ts_dat, paste(data_folder, '/std_ts_dat.RDS', sep = ''))
saveRDS(ts_dat_df, paste(data_folder, '/ts_dat_df.RDS', sep = ''))
saveRDS(ts_meta_dat, paste(data_folder, '/ts_meta_dat.RDS', sep = ''))

##---------------------------------------------------------------------------##
## 4. Data check #### 
#quick check that sites match between raw and meta data

# create summary data frames of sites sampled by date
ts_dat_sites <- ts_dat %>% 
        select(samp_date, site) %>% 
        distinct_all()

ts_meta_dat_sites <- ts_meta_dat %>% 
        select(samp_date, site) %>% 
        distinct_all()

# compare data frames to see if the site-date combinations match
summary(arsenal::comparedf(ts_dat_sites, ts_meta_dat_sites, by.x = c('samp_date', 'site'), by.y = c('samp_date', 'site')))

##---------------------------------------------------------------------------##
## 5. Vessel GPS data ####
## load data from vessel GPS downloads and match to raw data

# define CRS
GDA2020 <- st_crs(7855)

# read GPX files from vessel plotter

# identify gps downloads folder
gps_downloads_folder <- paste(sprintf("C:/Users/%s/OneDrive - University of Tasmania/IMAS-DiveFisheries-FIS-Data/FIS_VesselGPS_Downloads", Sys.info()[["user"]]))

# vessel data for surveys
morana_gps_2023 <- st_read(file.path(gps_downloads_folder, 'MORANAII-2023-06-24_download.gpx'), layer = 'waypoints')
morana_gps_2024a <- st_read(file.path(gps_downloads_folder, 'MORANAII-2024-06-07_download.gpx'), layer = 'waypoints')
morana_gps_2024b <- st_read(file.path(gps_downloads_folder, 'MORANAII-2024-07-01_download.gpx'), layer = 'waypoints')
morana_gps_2024c <- st_read(file.path(gps_downloads_folder, 'MORANAII-2024-07-08_download.gpx'), layer = 'waypoints')
morana_gps_2024d <- st_read(file.path(gps_downloads_folder, 'MORANAII-2024-08-24_download.gpx'), layer = 'waypoints')


# add sample year (note: gps time refers to time waypoint was uploaded or taken,
# therefore DO NOT use this column to determine year. Waypoint numbers should 
# always correspond to the actual sample time whereas names will generally 
# correspond to an upload time unless they have been manually entered)

morana_gps_2023 <- morana_gps_2023 %>%
 mutate(sampyear = 2023,
        vesselname = 'MoranaII')

morana_gps_2024a <- morana_gps_2024a %>%
 mutate(sampyear = 2024,
        vesselname = 'MoranaII')

morana_gps_2024b <- morana_gps_2024b %>%
 mutate(sampyear = 2024,
        vesselname = 'MoranaII')

morana_gps_2024c <- morana_gps_2024c %>%
 mutate(sampyear = 2024,
        vesselname = 'MoranaII')

# join GPX files
vessel_gps <- bind_rows(morana_gps_2023,
                        morana_gps_2024a,
                        morana_gps_2024b,
                        morana_gps_2024c,
                        morana_gps_2024d) %>%  
        mutate(gpsdate = as.Date(time),
               gpstime = time) %>% 
        select(c(name, sampyear, gpstime, gpsdate, geometry, vesselname))

ts_meta_dat <- readRDS(paste(data_folder, '/ts_meta_dat.RDS', sep = ''))

# separate start positions
ts_site_start <- ts_meta_dat %>%
        select(-c(finish_waypoint, finish_time)) %>% 
        dplyr::rename('waypoint' = start_waypoint,
                      'samptime' = start_time) %>% 
        mutate(sampperiod = 'start',
               sampyear = year(samp_date))

# separate finish positions
ts_site_finish <- ts_meta_dat %>%
        select(-c(start_waypoint, start_time)) %>% 
        dplyr::rename('waypoint' = finish_waypoint,
                      'samptime' = finish_time) %>% 
        mutate(sampperiod = 'finish',
               sampyear = year(samp_date))

# re-join start and finish positions and remove non-sampled sites        
ts_site_start_finish <- bind_rows(ts_site_start, ts_site_finish) %>%
        mutate(waypoint = if_else(waypoint < 10, as.character(paste('00', waypoint, sep = '')),
                                  if_else(between(waypoint, 10, 99), as.character(paste(0, waypoint, sep = '')),
                                          as.character(waypoint)))) %>%  
        select(c(samp_date, samptime, sampperiod, site, waypoint, sampyear))


# join geometry where start and finish waypoints were recorded 
ts_site_start_finish_wp <- ts_site_start_finish %>% 
        filter(!is.na(waypoint)) %>% 
        left_join(., vessel_gps, by = c('waypoint' = 'name', 'samp_date' = 'gpsdate')) %>% 
        dplyr::rename('sampyear' = sampyear.x) %>% 
        select(c(sampyear, samptime, gpstime, sampperiod, site, waypoint, geometry))


# re-join all waypoint data and transform to GDA2020

ts_site_start_finish_loc <- bind_rows(ts_site_start_finish_wp) %>% 
        st_as_sf() %>% 
        st_transform(GDA2020)

vessel_gps_dat <- ts_site_start_finish_loc

# save files
saveRDS(vessel_gps_dat, paste(data_folder, '/vessel_gps_dat.RDS', sep = ''))

# vessel_gps_dat <- readRDS(paste(data_folder, '/vessel_gps_dat.RDS', sep = ''))

# saveRDS(meta_dat_start, paste(data_folder, '/meta_dat_start.RDS', sep = ''))

# save spatial layer for QGIS

st_write(vessel_gps_dat,
         dsn = paste(data_folder, '/vessel_gps_dat_', Sys.Date(), '.gpkg', sep = ''),
         layer = "vessel_gps_dat", driver = "GPKG", overwrite = T, delete_dsn = T)

##---------------------------------------------------------------------------##



##---------------------------------------------------------------------------##
## 6. Sites sampled - proposed ####
## identify sites sampled based on proposed GPS site data and
## create map of sites sampled within based on proposed position
## use these data for 'proposed geometry' when joining to raw data 

# load final proposed sites
# ts_NE_GL_sites_2023 <- read.xlsx(paste(sprintf("C:/Users/%s/University of Tasmania/IMAS-DiveFisheries - Assessments - Documents/Assessments/GIS/SpatialLayers/TimeSwimLayers/", 
#               Sys.info()[["user"]]), 'TimedSwim_NE_GL_2023_50m', '.xlsx', sep = ''))

ts_NE_GL_sites_2023 <- read.xlsx(file.path(spat_layer_folder, 'TimedSwim_NE_GL_2023_50m.xlsx'))

# set CRS
GDA2020 <- st_crs(7855)
GDA94 <- st_crs(28355)
WGS84 <- st_crs(4326)

# convert 2022 site data to sf
ts_NE_GL_sites_sf <- ts_NE_GL_sites_2023 %>% 
 st_as_sf(coords = c("longitude", "latitude"), crs = WGS84) %>% 
 st_transform(., crs = GDA2020) %>% 
 select(c(site, geometry)) %>% 
 mutate(sampyear = 2024) 

# save final proposed sites up to sample year
saveRDS(ts_NE_GL_sites_sf, paste(data_folder, '/ts_NE_GL_sites_sf.RDS', sep = ''))

# load timed swim raw dataframe
ts_dat <- readRDS(file.path(data_folder, '/ts_dat.RDS'))

# identify unique sites sampled from timed swim dataframe
ts_dat_unique <- ts_dat %>%
 filter(sampyear == samp_year) %>% 
 distinct(site, .keep_all = T) %>% 
 select(c(site, samp_date, sampyear))

# join sites sampled to site file
ts_sites_join <- ts_NE_GL_sites_sf %>%
 dplyr::left_join(., ts_dat_unique, by = c('site', 'sampyear'))

# identity sites sampled 
ts_sites_sampled <- ts_sites_join %>% 
 mutate(sampled = ifelse(is.na(samp_date), 0, 1)) %>%  
 select(c(site, sampled, sampyear, geometry))

# transform to GDA2020
GDA2020 <- st_crs(7855)
ts_sites_sampled <- st_transform(ts_sites_sampled, GDA2020)

# read in Subblock map as an sf::sfc polygon object
# sf.subblock.map <- st_read("C:/Users/jaimem/Dropbox/AbaloneData/SpatialLayers/SubBlockMaps.gpkg")
# sf_subblock_map <- st_read(paste(sprintf("C:/Users/%s/University of Tasmania/IMAS-DiveFisheries - Assessments - Documents/Assessments/GIS/SpatialLayers/IMAS_Layers/", 
#                                          Sys.info()[["user"]]), 'IMAS_subblock_rev2022', '.gpkg', sep = ''))
sf_subblock_map <- st_read(paste(sprintf("C:/Users/%s/Dropbox (UTAS Research)/DiveFisheries/GIS/SpatialLayers/IMAS_Layers/IMAS_subblock_rev2022.gpkg", Sys.info()[["user"]])))


# transform map to GDA2020
sf_subblock_map <- st_transform(sf_subblock_map, GDA2020)

# add variable to identify sites sampled
ts_sites_sampled <- ts_sites_sampled %>% 
  mutate(sampled = factor(sampled, levels = c('0', '1')))

# create approx bbox to crop maps and zoom (use QGIS to manually get bbox coordinates)
ne_gl_bbox <- st_bbox(c(xmin = 577919.4788533378,
                             ymin = 5482577.2994562695,
                             xmax = 596143.3918643385,
                             ymax = 5497344.001926334),
                      crs = GDA2020)

# crop subblock map and sites sampled to maximise zoom                           
ts_sites_crop <- st_crop(ts_sites_sampled, ne_gl_bbox)
sf_subblock_map_crop <- st_crop(sf_subblock_map, ne_gl_bbox)

# create plot 
ne_gl_plot <- ggplot(data = st_geometry(sf_subblock_map_crop)) +
  geom_sf(fill = NA)+
  geom_sf_text(data = sf_subblock_map_crop, aes(label = SubBlockNo))+
geom_sf(data = ts_sites_crop, aes(colour = sampled)) +
  scale_colour_manual(values = c('red', 'blue'))+
  theme_bw()+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.05, "cm"), pad_y = unit(0.1, "cm"),
                         style = north_arrow_fancy_orienteering)+
  theme(legend.position="none")+
  xlab('Longitude')+
  ylab('Latitude')
 
# save plot
 ggsave(filename = paste(plots_folder, '/TimedSwimSurvey_', 'SitesSampled_Proposed_NE_GL_', Sys.Date(), '.pdf', sep = ''),
        plot = ne_gl_plot, units = 'mm', width = 190, height = 250)
 ggsave(filename = paste(plots_folder, '/TimedSwimSurvey_', 'SitesSampled_Proposed_NE_GL_', Sys.Date(), '.png', sep = ''),
        plot = ne_gl_plot, units = 'mm', width = 190, height = 150)

# save spatial layer for QGIS
st_write(ts_sites_sampled, 
         dsn = paste(spat_layer_folder, '/TimedSwim_NE_GL_2023_sites_sampled_', Sys.Date(), '.gpkg', sep = ''),
         layer = "TimedSwim_NE_GL_2023_sites_sampled.gpkg", driver = "GPKG", overwrite = T, delete_dsn = T)

# GPX of sites sampled for repeat survey
gl_resurvey_gpx <- ts_sites_sampled %>% 
 filter(sampled == 1) %>% 
 st_transform(., WGS84) %>% 
 mutate(longitude = unlist(map(geometry, 1)),
        latitude = unlist(map(geometry, 2))) %>% 
 as.data.frame() %>% 
 select(site, longitude, latitude)

write.xlsx(gl_resurvey_gpx, paste(spat_layer_folder, '/TimedSwim_NE_GL_2024_GPX_POST-SURVEY-SITES.xlsx', sep = ''), 
           sheetName = "Sheet1",
           colNames = TRUE, rowNames = F, append = FALSE)

##---------------------------------------------------------------------------##
## 7. Sites sampled - actual ####
## create map of sites sampled within each Block with recorded GPS positions and 
## use these data for 'actual geometry' when joining to raw data 

vessel_gps_dat <- readRDS(paste(data_folder, '/vessel_gps_dat.RDS', sep = ''))

# read in Subblock map as an sf::sfc polygon object
# sf_subblock_map <- st_read(paste(sprintf("C:/Users/%s/University of Tasmania/IMAS-DiveFisheries - Assessments - Documents/Assessments/GIS/SpatialLayers/IMAS_Layers/", 
#                                          Sys.info()[["user"]]), 'IMAS_subblock_rev2022', '.gpkg', sep = ''))

sf_subblock_map <- st_read(paste(sprintf("C:/Users/%s/Dropbox (UTAS Research)/DiveFisheries/GIS/SpatialLayers/IMAS_Layers/IMAS_subblock_rev2022.gpkg", Sys.info()[["user"]])))


# transform map to GDA2020
sf_subblock_map <- st_transform(sf_subblock_map, GDA2020)

site_sampled_start <- vessel_gps_dat %>% 
 filter(sampperiod == 'start')

site_sampled_finish <- vessel_gps_dat %>% 
 filter(sampperiod == 'finish')

# create approx bbox to crop maps and zoom (use QGIS to manually get bbox coordinates)
ne_gl_bbox <- st_bbox(c(xmin = 577919.4788533378,
                        ymin = 5482577.2994562695,
                        xmax = 596143.3918643385,
                        ymax = 5497344.001926334),
                      crs = GDA2020)

# crop subblock map and sites sampled to maximise zoom                           
site_sampled_start_crop <- st_crop(site_sampled_start, ne_gl_bbox)
site_sampled_finish_crop <- st_crop(site_sampled_finish, ne_gl_bbox)
sf_subblock_map_crop <- st_crop(sf_subblock_map, ne_gl_bbox)
 
ne_gl_sites_sampled_plot <- ggplot(data = st_geometry(sf_subblock_map_crop)) +
                geom_sf(fill = NA) +
                geom_sf_text(data = sf_subblock_map_crop, aes(label = SubBlockNo))+
                geom_sf(data = site_sampled_start_crop, colour = 'darkgreen') +
                geom_sf(data = site_sampled_finish_crop, colour = 'red') +
                theme_bw() +
                annotation_scale(location = "bl", width_hint = 0.5) +
                annotation_north_arrow(location = "br", which_north = "true", 
                                       pad_x = unit(0.05, "cm"), pad_y = unit(0.1, "cm"),
                                       style = north_arrow_fancy_orienteering)+
                theme(legend.position="none")+
                xlab('Longitude')+
                ylab('Latitude')
        
# save plot
ggsave(filename = paste(plots_folder, '/TimedSwimSurvey_', 'SitesSampled_Actual_NE_GL_', Sys.Date(), '.pdf', sep = ''),
       plot = ne_gl_sites_sampled_plot, units = 'mm', width = 190, height = 250)
ggsave(filename = paste(plots_folder, '/TimedSwimSurvey_', 'SitesSampled_Actual_NE_GL_', Sys.Date(), '.png', sep = ''),
       plot = ne_gl_sites_sampled_plot, units = 'mm', width = 190, height = 150)

# save spatial layer for QGIS

gl_sites_2024 <- site_sampled_start_crop %>% 
 filter(between(samptime, as.Date('2024-01-01'), as.Date('2024-06-30'))) %>% 
 distinct(site, .keep_all = T)

st_write(gl_sites_2024, 
         dsn = paste(spat_layer_folder, '/TimedSwim_NE_GL_2024_sites_sampled_', Sys.Date(), '.gpkg', sep = ''),
         layer = "TimedSwim_NE_GL_2024_sites_sampled.gpkg", driver = "GPKG", overwrite = T, delete_dsn = T)

##---------------------------------------------------------------------------##
# GPS site file for pilot 2023 post-survey of baseline 32 sites

vessel_gps_dat <- readRDS(paste(data_folder, '/vessel_gps_dat.RDS', sep = ''))

df_1 <- vessel_gps_dat %>% 
 mutate(start_finish = ifelse(sampperiod == 'start', 'S', 'F'),
  gps_name = paste0(site, '-', start_finish, sep = '')) %>% 
 select(c(gps_name, geometry))

df_2 <- df_1 %>% 
 st_transform(crs = st_crs(4326)) %>%
 sfheaders::sf_to_df(fill = T) %>% 
 select(c(gps_name, x, y)) %>%
 dplyr::rename('name' = gps_name,
               'latitude' = y,
               'longitude' = x) %>% 
 add_column(description = NA, 
            comment = NA)

write.xlsx(df_2, sprintf("C:/Users/%s/University of Tasmania/IMAS-DiveFisheries - Assessments - Documents/Assessments/GIS/SpatialLayers/TimeSwimLayers/TimedSwim_NE_GL_2023_GPX_POST-SURVEY.xlsx", Sys.info()[["user"]]), 
           sheetName = "Sheet1",
           colNames = TRUE, rowNames = F, append = FALSE)


##---------------------------------------------------------------------------##
# 10. Data Analysis ####

# clear list
# rm(list = ls())

# Import final dataframes 
ts_dat <- readRDS(file.path(data_folder, '/ts_dat.RDS'))
ts_dat_df <- readRDS(paste(data_folder, '/ts_dat_df.RDS', sep = ''))

# Import metadata frame
ts_meta_dat <- readRDS(paste(data_folder, '/ts_meta_dat.RDS', sep = ''))

##---------------------------------------------------------------------------##
std_ts_dat <- readRDS(paste(data_folder, '/std_ts_dat.RDS', sep = ''))

# Summarise total count for blockno x site x sampyear x legal.size
ts_count_sum <- std_ts_dat %>% 
    group_by(blockno, site, sampyear, legal_size) %>% 
    summarise(ab_n = sum(sizeclass_freq_10)) %>%  
    group_by(blockno, site, sampyear, legal_size) %>% 
    group_by(site)

ts_count_order <- std_ts_dat %>% 
 filter(sampyear == 2024) %>% 
 group_by(blockno, site, sampyear) %>% 
 summarise(ab_n = sum(sizeclass_freq_10)) %>%  
 group_by(blockno, site, sampyear) %>% 
 group_by(site) %>% 
 arrange(desc(ab_n))

ts_av_count <- std_ts_dat %>%
  group_by(blockno, site, diver, sampyear, legal_size) %>% 
  summarise(ab_n = sum(sizeclass_freq_10)) %>%  
  group_by(blockno, sampyear, legal_size) %>%
  summarise(av_count = mean(replace(ab_n, is.na(ab_n), 0)),
            sites = n_distinct(site)) %>% 
  pivot_wider(id_cols = c(blockno),
              names_from = c(legal_size, sampyear),
              values_from = c('av_count', 'sites'))
##---------------------------------------------------------------------------##
std_ts_dat %>%
 group_by(blockno, sampyear) %>% 
 summarise(days = n_distinct(samp_date),
           sites = n_distinct(site)) %>% 
 pivot_wider(id_cols = c(blockno),
             names_from = c(sampyear),
             values_from = c('days', 'sites')) %>% 
 adorn_totals()

std_ts_dat %>%
 filter(sampyear == 2024) %>% 
 mutate(samp_period = ifelse(samp_date <= as.Date('2024-06-30'), 'pre', 'post')) %>% 
 group_by(samp_period, samp_date) %>% 
 summarise(sites = n_distinct(site)) %>% 
 pivot_wider(id_cols = c(samp_date),
             names_from = c(samp_period),
             values_from = c('sites')) %>%
 arrange(samp_date) %>% 
 adorn_totals()



##---------------------------------------------------------------------------##
# Pre vs Post site counts and order of post site survey priority

pre_post_sites_count <- std_ts_dat %>%
 filter(sampyear == 2024) %>% 
 mutate(samp_period = ifelse(samp_date <= as.Date('2024-06-30'), 'pre', 'post')) %>% 
 group_by(samp_period, site) %>% 
 summarise(ab_n = sum(sizeclass_freq_10)) %>% 
 pivot_wider(id_cols = c(site),
             names_from = c(samp_period),
             values_from = c('ab_n')) %>%
 # arrange(samp_date) %>% 
 adorn_totals() %>% 
 select(site, pre, post)

write.xlsx(pre_post_sites_count, paste('Greenlip_PrePost_Abundance_PostSurveyPriorityList.xlsx'), sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)
##---------------------------------------------------------------------------##
# Pre vs Post site count deviation over time

pre_post_site_count <- std_ts_dat %>%
 filter(sampyear == 2024) %>% 
 mutate(samp_period = ifelse(samp_date <= as.Date('2024-06-30'), 'pre', 'post')) %>% 
 group_by(samp_period, site, samp_date) %>% 
 summarise(ab_n = sum(sizeclass_freq_10)) %>% 
 pivot_wider(id_cols = c(site),
             names_from = c(samp_period),
             values_from = c('ab_n')) %>% 
 mutate(count_diff = pre - post,
        rel_diff = ((post - pre) / pre)) %>% 
 dplyr::rename(pre_count = 'pre',
               post_count = 'post')

pre_post_site_date <- std_ts_dat %>%
 filter(sampyear == 2024) %>% 
 mutate(samp_period = ifelse(samp_date <= as.Date('2024-06-30'), 'pre', 'post')) %>% 
 group_by(samp_period, site, samp_date) %>% 
 summarise(max_date = max(samp_date)) %>% 
 pivot_wider(id_cols = c(site),
             names_from = c(samp_period),
             values_from = c('max_date')) %>%
 mutate(days_diff = as.numeric(difftime(post, pre, units = 'days'))) %>% 
 dplyr::rename(pre_date = 'pre',
               post_date = 'post')

pre_post_site_dev <- left_join(pre_post_site_date, pre_post_site_count) %>% 
 select(site, pre_date, post_date, pre_count, post_count, days_diff, count_diff, rel_diff)

pre_post_site_dev %>% 
 filter(!is.na(days_diff)) %>% 
 ggplot()+
 geom_point(aes(x = days_diff, y = count_diff))+
 theme_bw()+
 ylim(-120, 120)

dev_plot_box <- pre_post_site_dev %>% 
 filter(!is.na(days_diff)) %>% 
 mutate(post_days = case_when(between(days_diff, 0,  20) ~ '10',
                              between(days_diff, 21, 50) ~ '45',
                              between(days_diff, 51, 90) ~ '85')) %>% 
 ggplot(aes(x = post_days, y = count_diff))+
 # geom_violin()+
 geom_boxplot(outlier.colour = 'orange')+
 # geom_jitter(height = 0, width = 0.1)+
 theme_bw()+
 ylim(-120, 120)+
 ylab('Count Difference')+
 xlab('Post-survey Days Difference')

days_diff_mean <- pre_post_site_dev %>% 
 filter(!is.infinite(rel_diff)) %>% 
 group_by(days_diff) %>% 
 summarise(mean_ab_n = mean(rel_diff),
          median.ab.n = median(rel_diff),
          std_err = sd(rel_diff)/sqrt(n())) %>% 
 filter(!is.na(days_diff))

days_diff_mean %>% 
 filter(!is.na(days_diff)) %>% 
 ggplot(aes(x = days_diff, y = mean_ab_n))+
 geom_point()+
 geom_point(data = pre_post_site_dev, aes(x = days_diff, y = count_diff), colour = 'blue', alpha = 0.3)+
 geom_errorbar(aes(ymin = mean_ab_n -  std_err, ymax = mean_ab_n + std_err), width = 0.2,
               position = position_dodge(0.05))+
 theme_bw()+
 ylim(-1, 1)+
 ylab('Mean Count Difference')+
 xlab('Days Difference')

dev_plot <- days_diff_mean %>% 
 filter(!is.na(days_diff)) %>% 
 ggplot(aes(x = days_diff, y = mean_ab_n))+
 geom_point(data = pre_post_site_dev, aes(x = days_diff, y = rel_diff), colour = 'blue', alpha = 0.3)+
 geom_pointrange(aes(ymin = mean_ab_n -  std_err, ymax = mean_ab_n + std_err), width = 0.2,
               position = position_dodge(0.05))+
 theme_bw()+
 ylim(-2, 2)+
 ylab('Mean Count Difference')+
 xlab('Days Difference')

days_diff_mean %>% 
 filter(!is.na(days_diff)) %>% 
 ggplot(aes(x = days_diff, y = mean_ab_n))+
 geom_bar(stat = 'identity', colour = 'black', fill = 'lightgreen')+
 geom_errorbar(aes(ymin = mean_ab_n -  std_err, ymax = mean_ab_n + std_err), width = 0.2,
               position = position_dodge(0.05))+
 theme_bw()+
 ylim(-75, 75)+
 ylab('Mean Count Difference')+
 xlab('Days Difference')

setwd(plots_folder)
ggsave(filename = paste('TimedSwimSurvey_NE_GL', samp_year, '_PrePost_Deviation', '.pdf', sep = ''), 
       plot = dev_plot, units = 'mm', width = 190, height = 200)
ggsave(filename = paste('TimedSwimSurvey_NE_GL', samp_year, '_PrePost_Deviation', '.png', sep = ''), 
       plot = dev_plot, units = 'mm', width = 190, height = 200)

ggsave(filename = paste('TimedSwimSurvey_NE_GL', samp_year, '_PrePost_Deviation_Boxplot', '.pdf', sep = ''), 
       plot = dev_plot_box, units = 'mm', width = 190, height = 200)
ggsave(filename = paste('TimedSwimSurvey_NE_GL', samp_year, '_PrePost_Deviation_Boxplot', '.png', sep = ''), 
       plot = dev_plot_box, units = 'mm', width = 190, height = 200)
##---------------------------------------------------------------------------##
## PLOT 1: COUNT PER TEN MIN YEARS ####
## average count of all legal and sub-legal abalone per 10 min
## (i.e. the average count between paired divers for each site)

# determine mean abalone abundance for block x sampyear x size class
ten_min_mean <- std_ts_dat %>%
 group_by(sampyear, site, diver, time_elapsed, legal_size) %>% 
 summarise(ab_n = sum(sizeclass_freq_10)) %>% 
 group_by(sampyear, legal_size) %>% 
 summarise(mean_ab_n = mean(ab_n),
           median_ab_n = median(ab_n),
           n = n_distinct(site))

# df.1 <- ten.min.mean.year %>% 
#  select(-mean.ab.n) %>% 
#  spread(sampyear, median.ab.n) %>% 
#  dplyr::rename(FY2020 = '2020',
#                FY2021 = '2021',
#                FY2022 = '2022') %>%  
#  mutate(perc.change = round((1 - (FY2021 / FY2022)), 3) * 100) %>% 
#  spread(legal.size, perc.change) 
#  # select(-c('FY2020', 'FY2021', 'FY2022')) %>% 
#  # mutate(`>140 mm2` = ifelse(is.na(`>140 mm`), lag(`>140 mm`), `>140 mm`)) %>% 
#  # select(-`>140 mm`) %>% 
#  # filter(!is.na(`<140 mm`)) %>% 
#  # dplyr::rename(`>140 mm` = `>140 mm2`,
#  #               'BlockNo' = 'blockno') %>%  
#  # ggpubr::ggtexttable(rows = NULL, theme = ggpubr::ttheme('mOrange'))
# setwd(ts.plots.folder)
# write.xlsx(df.1, paste('TimedSwimSurvey_', samp.year-1, 'vs', samp.year, '_PercentChange.xlsx'), sheetName = "Sheet1", 
#            col.names = TRUE, row.names = TRUE, append = FALSE)
# ggsave(filename = paste('TimedSwimSurvey_',  samp.year-1, 'vs', samp.year, '_PercentChange', '.pdf', sep = ''), 
#        plot = df.1, units = 'mm', width = 190, height = 120)
# ggsave(filename = paste('TimedSwimSurvey_',  samp.year-1, 'vs', samp.year, '_PercentChange', '.png', sep = ''), 
#        plot = df.1, units = 'mm', width = 190, height = 120)

sub_legal_plot <- std_ts_dat %>% 
 filter(legal_size == '<150 mm') %>% 
 group_by(sampyear, site, diver) %>% 
 summarise(ab_n = sum(sizeclass_freq_10)) %>% 
 group_by(sampyear, site) %>% 
 summarise(mean_ab_n = mean(ab_n)) %>% 
 mutate(sampyear = factor(sampyear, levels = c('2023', '2024'))) %>% 
 ggplot(aes(x = sampyear, y = mean_ab_n))+
 geom_boxplot(aes(fill = sampyear), position = position_dodge2(1, preserve = 'single'),
              outlier.colour = '#EE8866') +
 scale_fill_manual(values = c('#44BB99', '#BBCC33'))+
 # geom_point(data = ten_min_mean %>% filter(legal_size == '<150 mm'), aes(group = factor(sampyear, levels = c('2023', '2024'))), shape = 19,
 #            size = 2, colour = 'red', fill = 'red', position = position_dodge2(0.8))+
  geom_point(data = ten_min_mean %>% filter(legal_size == '<150 mm'), aes(x = factor(sampyear)), shape = 19,
             size = 2, colour = 'red', fill = 'red', position = position_dodge2(0.8))+
 theme_bw()+
 ylab(bquote('Average count (abalone.10'*~min^-1*')'))+
 xlab('Blockno')+
 ylim(0, 80)+
 geom_text(data = ten_min_mean %>% filter(legal_size == '<150 mm'), aes(y = 80, label = n, x = factor(sampyear)), size = 3, 
           position = position_dodge2(0.8))+
 scale_colour_manual(values = c('#44BB99', '#BBCC33'))+
 guides(size = 'legend', colour = 'none',
        fill = guide_legend(title = 'Year'))+
 ggtitle('Sub-legal <150 mm')+
 theme(plot.title = element_text(vjust = 0, hjust = 0))+
 theme(legend.position = 'none')

legal_plot <- std_ts_dat %>% 
 filter(legal_size == '>150 mm') %>% 
 group_by(sampyear, site, diver) %>% 
 summarise(ab_n = sum(sizeclass_freq_10)) %>% 
 group_by(sampyear, site) %>% 
 summarise(mean_ab_n = mean(ab_n)) %>% 
 mutate(sampyear = factor(sampyear, levels = c('2023', '2024'))) %>% 
 ggplot(aes(x = sampyear, y = mean_ab_n))+
 geom_boxplot(aes(fill = sampyear), position = position_dodge2(1, preserve = 'single'),
              outlier.colour = '#EE8866') +
 scale_fill_manual(values = c('#44BB99', '#BBCC33'))+
 # geom_point(data = ten_min_mean %>% filter(legal_size == '<150 mm'), aes(group = factor(sampyear, levels = c('2023', '2024'))), shape = 19,
 #            size = 2, colour = 'red', fill = 'red', position = position_dodge2(0.8))+
 geom_point(data = ten_min_mean %>% filter(legal_size == '>150 mm'), aes(x = factor(sampyear)), shape = 19,
            size = 2, colour = 'red', fill = 'red', position = position_dodge2(0.8))+
 theme_bw()+
 ylab(bquote('Average count (abalone.10'*~min^-1*')'))+
 xlab('Blockno')+
 ylim(0, 80)+
 geom_text(data = ten_min_mean %>% filter(legal_size == '>150 mm'), aes(y = 80, label = n, x = factor(sampyear)), size = 3, 
           position = position_dodge2(0.8))+
 scale_colour_manual(values = c('#44BB99', '#BBCC33'))+
 guides(size = 'legend', colour = 'none',
        fill = guide_legend(title = 'Year'))+
 ggtitle('Legal >150 mm')+
 theme(plot.title = element_text(vjust = 0, hjust = 0))+
 theme(legend.title = element_blank(),
       legend.position = c(0.9, 0.7))

count.plot.sizeclass <- grid.arrange(sub_legal_plot, legal_plot, nrow = 2)

setwd(plots_folder)
ggsave(filename = paste('TimedSwimSurvey_NE_GL', samp_year, '_TenMinuteCount_LegalSubLegal', '.pdf', sep = ''), 
       plot = count.plot.sizeclass, units = 'mm', width = 190, height = 200)
ggsave(filename = paste('TimedSwimSurvey_NE_GL', samp_year, '_TenMinuteCount_LegalSubLegal', '.png', sep = ''), 
       plot = count.plot.sizeclass, units = 'mm', width = 190, height = 200)
##---------------------------------------------------------------------------##
## PLOT 2: COUNT PER TEN MIN PRE POST ####
## average count of all legal and sub-legal abalone per 10 min
## (i.e. the average count between paired divers for each site)

# determine mean abalone abundance for samp_period x size class

df_1 <- std_ts_dat %>% 
 filter(sampyear == 2024) %>% 
 group_by(site) %>% 
 summarise(sampled_n = n_distinct(samp_date))

df_2 <- std_ts_dat %>% 
 filter(sampyear == 2024) %>% 
 left_join(., df_1) %>% 
 filter(sampled_n >= 2)


ten_min_mean <- df_2 %>% 
 mutate(samp_period = ifelse(samp_date <= as.Date('2024-06-30'), 'pre', 'post')) %>% 
 group_by(samp_period, site, diver, time_elapsed, legal_size) %>% 
 summarise(ab_n = sum(sizeclass_freq_10)) %>% 
 group_by(samp_period, legal_size) %>% 
 summarise(mean_ab_n = mean(ab_n),
           median_ab_n = median(ab_n),
           n = n_distinct(site))

sub_legal_plot <- df_2 %>% 
 filter(legal_size == '<150 mm') %>% 
 mutate(samp_period = ifelse(samp_date <= as.Date('2024-06-30'), 'pre', 'post')) %>%
 group_by(samp_period, site, diver) %>% 
 summarise(ab_n = sum(sizeclass_freq_10)) %>% 
 group_by(samp_period, site) %>% 
 summarise(mean_ab_n = mean(ab_n)) %>% 
 mutate(samp_period = factor(samp_period, levels = c('pre', 'post'))) %>% 
 ggplot(aes(x = samp_period, y = mean_ab_n))+
 geom_boxplot(aes(fill = samp_period), position = position_dodge2(1, preserve = 'single'),
              outlier.colour = '#EE8866') +
 scale_fill_manual(values = c('#44BB99', '#BBCC33'))+
 # geom_point(data = ten_min_mean %>% filter(legal_size == '<150 mm'), aes(group = factor(sampyear, levels = c('2023', '2024'))), shape = 19,
 #            size = 2, colour = 'red', fill = 'red', position = position_dodge2(0.8))+
 geom_point(data = ten_min_mean %>% filter(legal_size == '<150 mm'), aes(x = factor(samp_period)), shape = 19,
            size = 2, colour = 'red', fill = 'red', position = position_dodge2(0.8))+
 theme_bw()+
 ylab(bquote('Average count (abalone.10'*~min^-1*')'))+
 xlab('Sampling Period')+
 ylim(0, 80)+
 geom_text(data = ten_min_mean %>% filter(legal_size == '<150 mm'), aes(y = 80, label = n, x = factor(samp_period)), size = 3, 
           position = position_dodge2(0.8))+
 scale_colour_manual(values = c('#44BB99', '#BBCC33'))+
 guides(size = 'legend', colour = 'none',
        fill = guide_legend(title = 'Year'))+
 ggtitle('Sub-legal <150 mm')+
 theme(plot.title = element_text(vjust = 0, hjust = 0))+
 theme(legend.position = 'none')

legal_plot <- df_2 %>% 
 filter(legal_size == '>150 mm') %>% 
 mutate(samp_period = ifelse(samp_date <= as.Date('2024-06-30'), 'pre', 'post')) %>%
 group_by(samp_period, site, diver) %>% 
 summarise(ab_n = sum(sizeclass_freq_10)) %>% 
 group_by(samp_period, site) %>% 
 summarise(mean_ab_n = mean(ab_n)) %>% 
 mutate(samp_period = factor(samp_period, levels = c('pre', 'post'))) %>% 
 ggplot(aes(x = samp_period, y = mean_ab_n))+
 geom_boxplot(aes(fill = samp_period), position = position_dodge2(1, preserve = 'single'),
              outlier.colour = '#EE8866') +
 scale_fill_manual(values = c('#44BB99', '#BBCC33'))+
 # geom_point(data = ten_min_mean %>% filter(legal_size == '<150 mm'), aes(group = factor(sampyear, levels = c('2023', '2024'))), shape = 19,
 #            size = 2, colour = 'red', fill = 'red', position = position_dodge2(0.8))+
 geom_point(data = ten_min_mean %>% filter(legal_size == '>150 mm'), aes(x = factor(samp_period)), shape = 19,
            size = 2, colour = 'red', fill = 'red', position = position_dodge2(0.8))+
 theme_bw()+
 ylab(bquote('Average count (abalone.10'*~min^-1*')'))+
 xlab('Sampling Period')+
 ylim(0, 80)+
 geom_text(data = ten_min_mean %>% filter(legal_size == '>150 mm'), aes(y = 80, label = n, x = factor(samp_period)), size = 3, 
           position = position_dodge2(0.8))+
 scale_colour_manual(values = c('#44BB99', '#BBCC33'))+
 guides(size = 'legend', colour = 'none',
        fill = guide_legend(title = 'Year'))+
 ggtitle('Sub-legal <150 mm')+
 theme(plot.title = element_text(vjust = 0, hjust = 0))+
 ggtitle('Legal >150 mm')+
 theme(plot.title = element_text(vjust = 0, hjust = 0))+
 theme(legend.title = element_blank(),
       legend.position = c(0.9, 0.7))

count.plot.sizeclass <- grid.arrange(sub_legal_plot, legal_plot, nrow = 2)

setwd(plots_folder)
ggsave(filename = paste('TimedSwimSurvey_NE_GL', samp_year, '_TenMinuteCount_LegalSubLegal_PrePost', '.pdf', sep = ''), 
       plot = count.plot.sizeclass, units = 'mm', width = 190, height = 200)
ggsave(filename = paste('TimedSwimSurvey_NE_GL', samp_year, '_TenMinuteCount_LegalSubLegal_PrePost', '.png', sep = ''), 
       plot = count.plot.sizeclass, units = 'mm', width = 190, height = 200)
##---------------------------------------------------------------------------##
##PLOT 3: SITE vs SITE ####

df_1 <- std_ts_dat %>% 
 filter(sampyear == 2024) %>% 
 group_by(site) %>% 
 summarise(sampled_n = n_distinct(samp_date))

df_2 <- std_ts_dat %>% 
 filter(sampyear == 2024) %>% 
 left_join(., df_1) %>% 
 filter(sampled_n >= 2)


df_3 <- df_2 %>% 
 mutate(samp_period = ifelse(samp_date <= as.Date('2024-06-30'), 'pre', 'post'))

 group_by(samp_period, site, diver, time_elapsed, legal_size) %>% 
 summarise(ab_n = sum(sizeclass_freq_10)) %>%  
 group_by(samp_period, legal_size) %>% 
 summarise(mean_ab_n = mean(ab_n),
           median_ab_n = median(ab_n),
           n = n_distinct(site))

 limitRange <- function(data, mapping, ...) { 
  ggplot(data = data, mapping = mapping, ...) + 
   geom_point(...) + 
   geom_smooth(method = "lm", se = FALSE) +
   scale_y_continuous(limits = c(0, 160)) +
   scale_x_continuous(limits = c(0, 160)) 
 }

size_class <- '>150 mm'

df_4 <- df_3 %>% 
 filter(sampyear >= 2024,
        !is.na(sizeclass_freq_10),
        !is.infinite(sizeclass_freq_10),
        sizeclass_freq_10 >= 0) %>%
 group_by(samp_period, legal_size, site, blockno) %>% 
 summarise(ab.n = sum(sizeclass_freq_10)) %>% 
 filter(legal_size == size_class) %>% 
 spread(key = samp_period, value = ab.n) %>% 
 select(site, blockno, pre, post)

corr_plot <- df_4 %>% 
 filter(legal_size == size_class) %>% 
 GGally::ggpairs(columns = 4:5, title = size_class, 
                 ggplot2::aes(colour = blockno, alpha = 0.5),
                 lower = list(continuous = limitRange))+
 theme_bw()+
 xlab('Abalone Count')+
 ylab('Abalone Count')+
 scale_x_continuous(limits = c(0, 160))

setwd(plots_folder)
ggsave(filename = paste('TimedSwimSurvey_NE_GL', samp_year, '_Legal_PrePost_Correlation', '.pdf', sep = ''), 
       plot = corr_plot, units = 'mm', width = 190, height = 200)
ggsave(filename = paste('TimedSwimSurvey_NE_GL', samp_year, '_Legal_PrePost_Correlation', '.png', sep = ''), 
       plot = corr_plot, units = 'mm', width = 190, height = 200)
##---------------------------------------------------------------------------##
# Site vs site marginal histogram

df_1 <- std_ts_dat %>% 
 filter(sampyear == 2024) %>% 
 group_by(site) %>% 
 summarise(sampled_n = n_distinct(samp_date))

df_2 <- std_ts_dat %>% 
 filter(sampyear == 2024) %>% 
 left_join(., df_1) %>% 
 filter(sampled_n >= 2)

df_3 <- df_2 %>% 
 mutate(samp_period = ifelse(samp_date <= as.Date('2024-06-30'), 'pre', 'post'))

size_class <- '>150 mm'

df_4 <- df_3 %>% 
 filter(sampyear >= 2024,
        !is.na(sizeclass_freq_10),
        !is.infinite(sizeclass_freq_10),
        sizeclass_freq_10 >= 0) %>%
 group_by(samp_period, legal_size, site, blockno) %>% 
 summarise(ab.n = sum(sizeclass_freq_10)) %>% 
 filter(legal_size == size_class) %>% 
 spread(key = samp_period, value = ab.n) %>% 
 select(site, blockno, pre, post)

pre_post_plot <- df_4 %>% 
 filter(legal_size == size_class) %>% 
 ggplot(aes(x = pre, y = post, colour = blockno))+
 geom_point()+
 theme_bw()+
 theme(legend.position = 'none')+
 xlab('Pre-season Abalone Count')+
 ylab('Post-season Abalone Count')+
 geom_text(aes(x = 125, y = 10, label = size_class), stat = 'unique', colour = 'black', size = 5)+
 theme(plot.title = element_text(hjust = 1, vjust = -100))+
 scale_x_continuous(limits = c(0, 160))+
 geom_smooth(method = 'lm', formula = y~x, se = F)+
 stat_poly_eq(formula = y~x, aes(label = paste(..rr.label.., p.value.label, sep = "~~~")), 
              parse = TRUE, label.y = c(0.95, 0.90))+
 xlim(0, 160)+
 ylim(0, 160)

pre_post_plot <- ggMarginal(pre_post_plot, groupColour = T, groupFill = T)
 
setwd(plots_folder)
ggsave(filename = paste('TimedSwimSurvey_NE_GL', samp_year, '_Legal_PrePost_Correlation_Marginal', '.pdf', sep = ''), 
       plot = pre_post_plot, units = 'mm', width = 190, height = 200)
ggsave(filename = paste('TimedSwimSurvey_NE_GL', samp_year, '_Legal_PrePost_Correlation_Marginal', '.png', sep = ''), 
       plot = pre_post_plot, units = 'mm', width = 190, height = 200)

pre_post_plot_overall <- df_4 %>% 
 filter(legal_size == size_class) %>% 
 ggplot(aes(x = pre, y = post))+
 geom_point(size = 3)+
 theme_bw(base_size = 25)+
 theme(legend.position = 'none')+
 xlab('Pre-season Abalone Count')+
 ylab('Post-season Abalone Count')+
 geom_text(aes(x = 125, y = 10, label = size_class), stat = 'unique', colour = 'black', size = 10)+
 theme(plot.title = element_text(hjust = 1, vjust = -100))+
 scale_x_continuous(limits = c(0, 150))+
 geom_smooth(method = 'lm', formula = y~x, se = F, size = 2)+
 stat_poly_eq(formula = y~x, aes(label = paste(..rr.label.., p.value.label, sep = "~~~")), 
              parse = TRUE, label.y = c(0.95, 0.90), size = 10)+
 xlim(0, 160)+
 ylim(0, 160)

ggsave(filename = paste(plots_folder, paste('/TimedSwimSurvey_NE_GL', samp_year, '_Legal_PrePost_Correlation', '.pdf', sep = ''), sep = ''), plot = pre_post_plot_overall, units = 'mm', width = 190, height = 200)
ggsave(filename = paste(plots_folder, paste('/TimedSwimSurvey_NE_GL', samp_year, '_Legal_PrePost_Correlation', '.png', sep = ''), sep = ''), plot = pre_post_plot_overall, units = 'mm', width = 190, height = 200)
