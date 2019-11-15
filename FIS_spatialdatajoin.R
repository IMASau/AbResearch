library(readxl)
library(splitstackshape)
library(geosphere)
library(data.table)
library(sp)
library(rgdal)
library(rgeos)
library(tidyr)
library(dplyr)

setwd('C:/CloudStor/R_Stuff/FIS')

##----------------------------------------------------------------------------##
## source data ####

## source site data for FIS sites

fis.sites <- read_xlsx("C:/CloudStor/Shared/Fisheries/Research/Abalone/AbResearchData/pop/Abalone_FIS_Sites_lastupdate12-10-2019.xlsx",
                    sheet = "ARM_LEG")

## source most recent juvenile and adult data sets

# bigabcounts <- readRDS('C:/CloudStor/R_Stuff/FIS/bigabcounts.RDS')
# abcounts <- readRDS('C:/CloudStor/R_Stuff/ARMs/abcounts.RDS')

arm.leg.density <- readRDS('C:/Cloudstor/R_Stuff/FIS/arm.leg.counts.RDS')

# ##----------------------------------------------------------------------------##
# ## clean FIS transect survey data ####
# 
# ## add column to identify FIS and ARM data
# bigabcounts$sampmethod <- 'FIS'
# abcounts$sampmethod <- 'ARM'
# 
# ## convert sampyear to factor from bigabcounts df
# bigabcounts$sampyear <- as.factor(bigabcounts$sampyear)
# 
# ## join FIS and ARM data
# fisarm.abund <- bind_rows(bigabcounts, abcounts)
# 
# ## rename duplicated sites (NOTE: need to update these in FIS and ARM analysis scripts)
# unique(fisarm.abund$site)
# fisarm.abund$site <- gsub('MP', 'IB', fisarm.abund$site)
# fisarm.abund$site <- gsub('SB', 'IB', fisarm.abund$site)
# fisarm.abund$site <- gsub('OS', 'OB', fisarm.abund$site)
# 
# ## re-label sampling method
# unique(fisarm.abund$sampmethod)
# fisarm.abund$sampmethod <- gsub('FIS', 'LEG', fisarm.abund$sampmethod)
# 
# ## add unique index code to identify site and method
# 
# fisarm.abund$site.index <- as.factor(paste(fisarm.abund$site, fisarm.abund$sampmethod, fisarm.abund$string, sep="_"))

##----------------------------------------------------------------------------##
## clean FIS site data ####

## rename site column

fis.sites <- dplyr::rename(fis.sites, site = name)

# fis.sites <- fis.sites %>%
#  mutate(site = name) %>%
#  dplyr::select(-name)

## split current site names and identify variables (i.e. string, position, etc)

df.1 <- cSplit(fis.sites, 'site', '-', drop = F)
str(df.1)
df.2 <- df.1 %>%
 mutate(fis.site = site_2, fis.method = site_3, fis.string = site_4, fis.position = site_5) %>%
 dplyr::select(-c(site_1, site_2, site_3, site_4, site_5))

# ## re-label FIS methods
# 
# unique(df.2$fis.method)
# df.2$fis.method <- gsub('J', 'ARM', df.2$fis.method)
# df.2$fis.method <- gsub('PB', 'LEG', df.2$fis.method)
# df.2$fis.method <- gsub('LF', 'LEG', df.2$fis.method)

# ## re-label site names to match FIS data
# 
# sort(unique(fisarm.abund$site))
# sort(unique(df.2$site.name))
# 
# df.2$site.name <- gsub('SEYMOUR', 'SEY', df.2$site.name)
# df.2$site.name <- gsub('GEO', 'GEO', df.2$site.name)
# df.2$site.name <- gsub('GARDENS', 'GAR', df.2$site.name)
# df.2$site.name <- gsub('MUNROES', 'MUN', df.2$site.name)
# df.2$site.name <- gsub('OHARA', 'OHA', df.2$site.name)
# df.2$site.name <- gsub('SADINNER', 'INN', df.2$site.name)
# df.2$site.name <- gsub('SADOUTER', 'OUT', df.2$site.name)
# df.2$site.name <- gsub('BET', 'BET', df.2$site.name)
# df.2$site.name <- gsub('THUMBS', 'THU', df.2$site.name)
# df.2$site.name <- gsub('TELOPEA', 'TEL', df.2$site.name)

# ## re-label string number
# 
# unique(df.2$fis.string)
# df.2$fis.string[is.na(df.2$fis.string)] <- 1

## re-label start and end points of ARM and LEG strings

unique(df.2$fis.position)
df.2$fis.position <- gsub(1, 0, df.2$fis.position)
df.2$fis.position <- gsub(20, 1, df.2$fis.position)
df.2$fis.position <- gsub(25, 1, df.2$fis.position)
df.2$fis.position <- gsub(60, 1, df.2$fis.position)
df.2$fis.position <- gsub(2, 0, df.2$fis.position)
df.2$fis.position <- gsub(3, 0, df.2$fis.position)
df.2$fis.position[is.na(df.2$fis.position)] <- 0

## add unique index code to identify site and method

df.2$site.index <- as.factor(paste(df.2$fis.site, df.2$fis.method, df.2$fis.string, sep="_"))

##----------------------------------------------------------------------------##
# ## create file for Lowrance GPS plotter
# df.gpx <- df.2
# df.gpx$gps.index <- as.factor(paste('AB', df.gpx$site.name, df.gpx$fis.method, df.gpx$fis.string, sep="-"))

##----------------------------------------------------------------------------##
## add midpoints ####

## calculate the mid-points of each site ARM and LEG string

df.3 <- df.2 %>%
 group_by(site.index) %>%
 summarise(lat.mid = mean(latitude),
           long.mid = mean(longitude))

##----------------------------------------------------------------------------##
## summarise FIS ARM and LEG data ####
df.4 <- fisarm.abund %>%
 mutate(absm.fis = if_else(!is.na(absm), absm, bigabdat.join.absm),
        ab_n.fis = if_else(!is.na(ab_n), ab_n, bigabdat.join.ab_n)) %>%
 group_by(site.index, survdate) %>%
 summarise(mean.absm = mean(absm.fis),
           n.absm = length(ab_n.fis),
           sd.absm = sd(absm.fis))

df.4 <- arm.leg.density %>% 
        mutate(absm.fis = if_else(!is.na(absm.leg), absm.leg, absm.arm),
               abn.fis = if_else(!is.na(ab_n), ab_n, dat.ab_n)) %>% 
        group_by(dat.survindex, survdate) %>%
        summarise(mean.absm = mean(absm.fis),
                  n.absm = length(abn.fis),
                  sd.absm = sd(absm.fis))
        

##----------------------------------------------------------------------------##
## join site and FIS ARM and LEG data ####

df.5 <- left_join(df.4, df.3, by = "site.index")

##----------------------------------------------------------------------------##
## create spatial data ####

## remove Louisa Bay site as there are no coordinates available

df.5 <- df.5 %>%
 filter(!grepl('LB', site.index))

## convert date to POSIXct

df.5$survdate <- strptime(df.5$survdate, format = '%Y-%m-%d')

## rename dataframe variables

df.6 <- df.5 %>%
 mutate(site = site.index,
        lat = lat.mid,
        long = long.mid,
        date = survdate,
        absm2 = mean.absm,
        n = n.absm,
        sd = sd.absm) %>%
 ungroup() %>%
 dplyr::select(-c(site.index, lat.mid, long.mid, survdate, mean.absm, n.absm, sd.absm))

## convert to spatialpoints dataframe

coordinates(df.6) <- ~long + lat

##----------------------------------------------------------------------------##
## save RDS ####

saveRDS(df.6, file = 'tas_fis_survey_data_2019-07-05.rds')

writeOGR(df.6, '.', 'tas_fis_survey_data_2019-07-05', 'ESRI Shapefile')

##----------------------------------------------------------------------------##
## check positions ####

tas <- readOGR(dsn = 'C:/Users/jaimem/Documents/ArcGIS/GIS_files/Coastlines_GIS/Tasmania_ll_(WGS 84)/Tas_25k_land_ll_wgs84.shp'
               , layer = 'Tas_25k_land_ll_wgs84', verbose = F)
plot(tas)
points(df.6, cex = 1.4, pch = 16)
