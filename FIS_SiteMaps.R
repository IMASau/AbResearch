library(sp)
library(sf)
library(tmaptools)
library(maptools)
library(rgeos)
library(tidyverse)
library(lubridate)
library(tmap)
library(spdplyr)
library(readxl)
library(splitstackshape)
library(geosphere)
library(data.table)
library(rgdal)
library(tidyr)
library(dplyr)
library(raster)
library(ggmap)
library(broom)

##----------------------------------------------------------------------------##
## source data ####

## source site data for FIS sites

fis.sites <- read_xlsx("C:/CloudStor/Shared/Fisheries/Research/Abalone/AbResearchData/pop/Abalone_FIS_Sites_lastupdate12-10-2019.xlsx",
                       sheet = "ARM_LEG")

##----------------------------------------------------------------------------##
## clean FIS site data ####

## rename site column
fis.sites <- dplyr::rename(fis.sites, site = name)

## split current site names and identify variables (i.e. string, position, etc)
fis.sites <- cSplit(fis.sites, 'site', '-', drop = F)

fis.sites <- fis.sites %>%
 mutate(fis.site = site_2, fis.method = site_3, fis.string = site_4, fis.position = site_5) %>%
 dplyr::select(-c(site_1, site_2, site_3, site_4, site_5))

## re-label start and end points of ARM and LEG strings
unique(fis.sites$fis.position)
fis.sites$fis.position <- gsub(1, 0, fis.sites$fis.position)
fis.sites$fis.position <- gsub(20, 1, fis.sites$fis.position)
fis.sites$fis.position <- gsub(25, 1, fis.sites$fis.position)
fis.sites$fis.position <- gsub(60, 1, fis.sites$fis.position)
fis.sites$fis.position <- gsub(2, 0, fis.sites$fis.position)
fis.sites$fis.position <- gsub(3, 0, fis.sites$fis.position)
fis.sites$fis.position[is.na(fis.sites$fis.position)] <- 0
fis.sites$fis.position <- as.numeric(fis.sites$fis.position)

# subset data for selected site
fis.selected.site <- fis.sites %>% 
 filter(fis.site %in% c('BET'))

##----------------------------------------------------------------------------##
## create dataframe of sites using midpoints ####

## add unique index code to identify site and method
fis.sites$site.index <- as.factor(paste(fis.sites$fis.site, fis.sites$fis.method, fis.sites$fis.string, sep="_"))

## calculate the mid-points of each site ARM and LEG string
fis.sites.mid <- fis.sites %>%
 group_by(id) %>%
 summarise(lat.mid = mean(latitude),
           long.mid = mean(longitude))

##----------------------------------------------------------------------------##
## convert to spatial points dataframe
fis.selected.site.sp <- fis.selected.site
fis.sites.sp <- fis.sites
fis.sites.mid.sp <- fis.sites.mid
coordinates(fis.selected.site.sp) <- ~longitude+latitude
coordinates(fis.sites.sp) <- ~longitude+latitude
coordinates(fis.sites.mid.sp) <- ~long.mid+lat.mid

## set projection
projstring.mga55 <- CRS("+proj=utm +zone=55 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
projstring.wgs84 <-  CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
proj4string(fis.sites.sp) <- projstring.wgs84
proj4string(fis.selected.site.sp) <- projstring.wgs84
proj4string(fis.sites.mid.sp) <- projstring.wgs84

## convert to UTM
fis.sites.sp <- spTransform(fis.sites.sp, projstring.mga55)
fis.selected.site.sp <- spTransform(fis.selected.site.sp, projstring.mga55)
fis.sites.mid.sp <- spTransform(fis.sites.mid.sp, projstring.mga55)
##----------------------------------------------------------------------------##
## plot positions ####

## load map of Tasmania
tas.sp <- readOGR(dsn = 'C:/Users/jaimem/Documents/ArcGIS/GIS_files/Coastlines_GIS/Tasmania_ll_(WGS 84)/Tas_25k_land_ll_wgs84.shp'
               , layer = 'Tas_25k_land_ll_wgs84', verbose = F)

ab.blocks <- readOGR(dsn = 'C:/CloudStor/R_Stuff/BlockSHapefile/Ab_Blocks_MGAZ55.shp'
                  , layer = 'Ab_Blocks_MGAZ55', verbose = F)

## convert to map projection
tas.sp <- spTransform(tas.sp, projstring.mga55)
ab.blocks <- spTransform(ab.blocks, projstring.mga55)

## plot map of Tasmania and site positions
plot(tas.sp)
points(fis.sites.sp, cex = 2, pch = 16, col = 'red')
plot(ab.blocks)

########################
crs(tas.sp)
crs(fis.sites.sp)
crs(fis.selected.site.sp)

tas.sp_fis.sites.sp_crop <- crop(tas.sp, fis.sites.sp)
plot(tas.sp_fis.sites.sp_crop)
points(fis.sites.sp, cex = 2, pch = 16, col = 'red')

# tas.sp_fis.site.sp_crop <- crop(tas.sp, fis.selected.site.sp)
# plot(tas.sp_fis.site.sp_crop)
# points(fis.selected.site.sp, cex = 2, pch = 16, col = 'blue')

# convert sp to sf
fis.selected.site.sf <- st_as_sf(fis.selected.site.sp)

# map.df <- crop_shape(tas.sp, fis.selected.site.sp)
# 
# tm_shape(tas.sp, bbox = fis.selected.site.sf)+
#         tm_polygons()+
#         tm_shape(fis.selected.site.sf)+
#         tm_symbols(size = 2, col = 'red')

# add buffer around sites so that map has border
buffer <- 200

# extract bbox of selected site data
site.bounds <- as.data.frame(bbox(fis.selected.site.sp))

site.bounds <- site.bounds$min - buffer

site.bounds <- c(left = 539317.9 - buffer, bottom = 5234232.0 - buffer, top = 539592.6 + buffer, 5234333 + buffer)
site.grid <- expand.grid(lon_bound = c(site.bounds[1], site.bounds[3]), 
                         lat_bound = c(site.bounds[2], site.bounds[4]))
coordinates(site.grid) <- ~ lon_bound + lat_bound
proj4string(site.grid) <- projstring.mga55

tm_shape(tas.sp, bbox = site.grid)+
        tm_polygons()+
        tm_shape(fis.selected.site.sf)+
        tm_symbols(size = 2, col = 'red')+
        tm_compass()+
        tm_scale_bar()




# ########################
# buffer.zone <- 0.11
# 
# df.2 <- fis.site.sp %>%
#  filter(fis.site == 'BRB')
# 
# df.3 <- df.2 
# str(df.3)
# 
# geo_bounds <- c(left = min(df.3$longitude) - buffer.zone,
#                 bottom = min(df.3$latitude) - buffer.zone,
#                 right = max(df.3$longitude) + buffer.zone,
#                 top = max(df.3$latitude) + buffer.zone)
# 
# 
# sites.grid <- expand.grid(lon_bound = c(geo_bounds[1], geo_bounds[3]),
#                           lat_bound = c(geo_bounds[2], geo_bounds[4]))
# 
# coordinates(sites.grid) <- ~ lon_bound + lat_bound
# 
# tas.crop <- crop(tas, extent(sites.grid))
# plot(tas.crop)
# points(df.6, cex = 1, pch = 16, col = 'red')
