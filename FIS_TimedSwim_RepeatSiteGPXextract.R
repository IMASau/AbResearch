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
# 3. Import metadata frame ####

time.swim.meta.dat.final <- readRDS(paste(samp.year.folder, '/time.swim.meta.dat.final.RDS', sep = ''))

##---------------------------------------------------------------------------##
# 4. Extract block 13 coordinates ####

actaeons_sites_gpx <- time.swim.meta.dat.final %>%
 mutate(sampyear = year(starttime)) %>% 
  filter(blockno %in% c(13) &
           sampyear == 2023) %>%
 select(c(site, finish_geom)) %>% 
 st_as_sf() %>% 
 st_transform(crs = st_crs(4326)) %>% 
 sfheaders::sf_to_df(fill = T) %>% 
 select(site, x, y) %>% 
 dplyr::rename('latitude' = y,
               'longitude' = x,
               'name' = site) %>% 
 add_column(description = NA, 
            comment = NA)
##---------------------------------------------------------------------------##
# 5. Export Excel file for GPX conversion ####

write.xlsx(actaeons_sites_gpx, paste0('C:/CloudStor/DiveFisheries/Abalone/FISdata/FIS_TimedSwimSurveys', 2023, '/TimedSwim_Actaeons/TimedSwimSites_Actaeons_Repeat_Aug2023_END', '_GPX', ".xlsx", sep = ""), 
           sheetName = "Sheet1", 
           colNames = TRUE, rowNames = FALSE, append = FALSE)