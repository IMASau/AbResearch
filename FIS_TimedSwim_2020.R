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
})

##---------------------------------------------------------------------------##
## 2. Load data ####

# load timed swim length frequency raw data
time.swim.dat <- read.xlsx("C:/Users/jaimem/Documents/Abalone_research/AB_TimedSwimFIS/FIS_TimedSwim_RawData_2020.xlsx",
                       detectDates = T)

# load timed swim meta data
time.swim.meta.dat <- read.xlsx("C:/Users/jaimem/Documents/Abalone_research/AB_TimedSwimFIS/FIS_TimedSwim_MetaData_2020.xlsx",
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
        mutate(blockno = ifelse(blockno %in% c(72, 74, 'LEG'), 22, blockno))

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

saveRDS(time.swim.dat, 'C:/Users/jaimem/Documents/Abalone_research/AB_TimedSwimFIS/TimedSwimData_2020-10-05.RDS')
saveRDS(time.swim.dat.df, 'C:/Users/jaimem/Documents/Abalone_research/AB_TimedSwimFIS/TimedSwimData.df_2020-10-05.RDS')


##---------------------------------------------------------------------------##
## 4. Site rename ####
## Match completed survey sites with corrected site names (i.e. site numbers in sequence from south to north)

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

# join final site file with the site file used for the 2020 surveys to re-label the sites
fis.sites.new.df <- left_join(fis.sites.final, fis.site.cpue, by = c('lat', 'long')) %>%    
        left_join(fis.site.sam.dup, by = c('lat', 'long')) %>% 
        mutate(name.y = ifelse(is.na(name.y), name, name.y)) %>% 
        dplyr::rename('site.new' = name.x,
                      'site' = name.y) %>%  
        select(-c(desc.y, desc.x, name))

fis.sites.new.sf <- fis.sites.new.df %>% 
        st_as_sf(coords = c("long", "lat"), crs = WGS84)

##---------------------------------------------------------------------------##
## 5. Sites completed ####
## join re-labelled site names to completed FIS sites

# identify unique sites sampled from timed swim dataframe and join to corrected site file
time.swim.dat.unique <- time.swim.dat %>% 
        distinct(site, .keep_all = T) %>% 
        select(c(site, sampdate))

time.swim.dat.site.join <- left_join(fis.sites.new.sf, time.swim.dat.unique, by = c('site' = 'site'))

# identity sites sampled and colour code (green = sampled, red = not sampled)
# TO DO: investigate a way to GPX export for loading onto vessel plotter directly from R rather than through QGIS 
time.swim.dat.site.sampled <- time.swim.dat.site.join %>% 
        mutate(color = ifelse(is.na(sampdate), 'red', 'green'),
               symbol = 'circle',
               sym.col = paste(symbol, color, sep = ',')) %>% 
        select(c(site.new, site, sym.col, geometry))

# transform to GDA2020
time.swim.dat.site.sampled <- st_transform(time.swim.dat.site.sampled, GDA2020)

st_write(time.swim.dat.site.sampled, 
         dsn = "C:/Users/jaimem/Documents/Abalone_research/AB_TimedSwimFIS/FIS_TIMEDSWIMSITES_COMPLETED_2020-09-23.gpkg", 
         layer = "time.swim.dat.site.sampled", driver = "GPKG", overwrite = T, delete_dsn = T)

##---------------------------------------------------------------------------##
## 5. Site position ####
## match timed swim data to GPS positions from vessel plotter downloads

# read GPX file from plotter
morana.gps <- st_read('C:/Users/jaimem/Documents/Abalone_research/AB_TimedSwimFIS/MORANAII-2020-10-09_download.gpx', layer = 'waypoints')

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
        mutate(waypoint = if_else(waypoint < 100, as.character(paste(0, waypoint, sep = '')), as.character(waypoint))) %>%  
        select(c(sampdate, samptime, sampperiod, site, waypoint)) %>% 
        filter(site != 'Betsey')

# join geometry where start waypoint recorded as being on the original GPS position/waypoint mark
time.swim.site.start.finish.mark <- time.swim.site.start.finish %>% 
        filter(is.na(waypoint) & sampperiod == 'start') %>% 
        left_join(., morana.gps, by = c('site' = 'name')) %>% 
        dplyr::rename('gpstime' = time,
                      'site.old' = site) %>%
        select(c(sampdate, samptime, gpstime, sampperiod, site.old, waypoint, geometry))

# join geometry where waypoints were recorded 
time.swim.site.start.finish.wp <- time.swim.site.start.finish %>% 
        filter(!is.na(waypoint)) %>% 
        left_join(., morana.gps, by = c('waypoint' = 'name')) %>% 
        dplyr::rename('gpstime' = time,
                      'site.old' = site) %>% 
        select(c(sampdate, samptime, gpstime, sampperiod, site.old, waypoint, geometry))

# re-join all waypoint data
time.swim.site.start.finish.loc <- bind_rows(time.swim.site.start.finish.mark, time.swim.site.start.finish.wp) %>% 
        st_as_sf()

# transform to GDA2020
time.swim.site.start.finish.loc <- st_transform(time.swim.site.start.finish.loc, GDA2020)

st_write(time.swim.site.start.finish.loc, 
         dsn = "C:/Users/jaimem/Documents/Abalone_research/AB_TimedSwimFIS/FIS_TIMEDSWIMSITES_STARTFINISH_2020-09-23.gpkg", 
         layer = "time.swim.site.start.finish.loc", driver = "GPKG", overwrite = T, delete_dsn = T)

##---------------------------------------------------------------------------##
## MAP 1: Sites sampled ####
## create map of sites sampled within each Block

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

# identify blocks sampled
blocks.sampled <- unique(site.samp.loc$blockno)

# create maps

for (i in blocks.sampled){

site.samp.loc.blockno <- site.samp.loc %>% 
        filter(blockno == i) %>% 
        mutate(sym.col = factor(sym.col, levels = c('circle,red', 'circle,green')))

sf.subblock.map.crop <- sf.subblock.map %>% 
        filter(blockno == i) 
        # st_crop(., site.samp.loc.blockno)

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
ggsave(filename = paste('TimedSwimSurvey_2020_SitesSampled_BlockNo_', i, '.pdf', sep = ''),
       plot = site.samp.map.blockno, units = 'mm', width = 190, height = 250)
ggsave(filename = paste('TimedSwimSurvey_2020_SitesSampled_BlockNo_', i, '.png', sep = ''),
       plot = site.samp.map.blockno, units = 'mm', width = 190, height = 150)

}

##---------------------------------------------------------------------------##
## PLOT 1: LF ####
## Length frequency plot by block (mid-points)

# determine number of abalone recorded and number of sites sampled per block
block.ab.n <- time.swim.dat.df %>% 
        group_by(blockno) %>% 
        summarise(ab.n = paste('n = ', n()))

block.site.n <- time.swim.dat %>% 
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
time.swim.dat.df.lw <- time.swim.dat %>% 
        mutate(join.id = 1) %>% 
        left_join(., lw.coeff)

# saveRDS(time.swim.dat.df.lw, 'C:/Users/jaimem/Documents/Abalone_research/AB_TimedSwimFIS/TimedSwimData_LW_2020-09-16.RDS')

# estimate total weight of each sizeclass above 140 mm
time.swim.dat.df.wt <- time.swim.dat.df.lw %>% 
        filter(midsize >= 150) %>%
        mutate(est.weight = ((a * (midsize ^ b)) * sizeclass_freq) / 1000)

# quick summary of total kg estimated by block
time.swim.dat.df.wt %>%
        dplyr::group_by(blockno) %>% 
        dplyr::summarise(total.kg = round(sum(est.weight), digits = 2)) %>% 
        as.data.frame()

# estimate CPUE for each site
time.swim.cpue.site <- time.swim.dat.df.wt %>%
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
        geom_boxplot() +
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
time.swim.dat.n <- time.swim.dat %>% 
        group_by(blockno) %>% 
        summarise(n = n_distinct(site))

## average count per 10 min for each blockno
count.plot <- time.swim.dat %>% 
        # filter(midsize >= 150) %>% 
        group_by(blockno, site, diver) %>% 
        summarise(ab.n = sum(sizeclass_freq)) %>% 
        group_by(blockno, site) %>% 
        summarise(mean.ab.n = mean(ab.n)) %>% 
        ggplot(aes(x = blockno, y = mean.ab.n))+
        geom_point(size = 3)+
        stat_summary(fun.y = mean, geom = 'point', shape = 19, size = 4, colour = 'red', fill = 'red')+
        theme_bw()+
        ylab(bquote('Average count (abalone.10'*~min^-1*')'))+
        xlab('Blockno')+
        ylim(0, 200)+
        geom_text(data = time.swim.dat.n, aes(y = 200, label = n), color = 'black', size = 3)

setwd('C:/CloudStor/R_Stuff/FIS/FIS_2020')
ggsave(filename = paste('TimedSwimSurvey_2020_TenMinuteCountPlot', '.pdf', sep = ''), 
       plot = count.plot, units = 'mm', width = 190, height = 120)
ggsave(filename = paste('TimedSwimSurvey_2020_TenMinuteCountPlot', '.png', sep = ''), 
       plot = count.plot, units = 'mm', width = 190, height = 120)

##---------------------------------------------------------------------------##
## PLOT 4: COUNT PER TEN MIN SIZECLASS ####
## average count of all legal and sub-legal abalone per 10 min by sizeclass for each site within each blockno 
## (i.e. the average between paired divers for each site)

# determine mean count per 10 min by size class for each blockno
ten.min.mean <- time.swim.dat %>% 
        # filter(midsize < 150) %>% 
        group_by(blockno, site, diver, legal.size) %>% 
        summarise(ab.n = sum(sizeclass_freq)) %>% 
        group_by(blockno, legal.size) %>% 
        summarise(mean.ab.n = mean(ab.n)) %>% 
        mutate(legal.size = factor(legal.size))

count.plot.sizelass <- time.swim.dat %>% 
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
       plot = count.plot.sizelass, units = 'mm', width = 190, height = 120)
ggsave(filename = paste('TimedSwimSurvey_2020_TenMinuteCountSizeClassPlot', '.png', sep = ''), 
       plot = count.plot.sizelass, units = 'mm', width = 190, height = 120)
        
##---------------------------------------------------------------------------##
## TAB 1: SUMMARY ####
## summary table of counts and CPUE by size class and block (NOTE: run script for CPUE above)

# arrange size classes in order
sizeclasses <- c("0-20", "20-40", "40-60", "60-80", "80-100", "100-120", "120-140", "140-160", "160-180", "180-200", "200-220")

time.swim.count.blockno <- time.swim.dat %>%
        group_by(blockno, site, diver, legal.size) %>%
        summarise(ab.measured = sum(sizeclass_freq),
                  swim.time = max(time.elapsed)) %>%
        mutate(ab.cpue = (ab.measured / swim.time) * 10) %>%
        group_by(blockno, legal.size) %>%
        summarise(sites = n_distinct(site),
                  ab.min = round(mean(ab.cpue), digits = 2)) %>%
        spread(legal.size, ab.min) %>%
        as.data.frame()

time.swim.summary <- left_join(time.swim.count.blockno, time.swim.cpue.blockno) %>% 
        dplyr::rename('Blockno' = blockno,
               'Sites' = sites,
               'Average\ncount\n<140mm' = '<140 mm',
               'Average\ncount\n>140mm' = '>140 mm',
               'CPUE' = est.kg.hr)

# create formated summary tables for report layout
time.swim.summary.tab <- time.swim.summary %>% 
        ggpubr::ggtexttable(rows = NULL, theme = ggpubr::ttheme('mOrange'))

setwd('C:/CloudStor/R_Stuff/FIS/FIS_2020')
# ggsave(filename = paste('TimedSwimSurvey_2020_SummaryTable', '.pdf', sep = ''), 
#        plot = time.swim.summary.tab)
# ggsave(filename = paste('TimedSwimSurvey_2020_SummaryTable', '.png', sep = ''), 
#        plot = time.swim.summary.tab)

ggsave(filename = paste('TimedSwimSurvey_2020_SummaryTable', '.pdf', sep = ''), 
       plot = time.swim.summary.tab, units = 'mm', width = 190, height = 120)
ggsave(filename = paste('TimedSwimSurvey_2020_SummaryTable', '.png', sep = ''), 
       plot = time.swim.summary.tab, units = 'mm', width = 190, height = 120)

##---------------------------------------------------------------------------##
## MAP 2: CPUE ####
## average CPUE by site - possibly explore KUDs

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
time.swim.cpue.site.loc <- left_join(time.swim.cpue.site, site.samp.loc) %>% 
        st_as_sf() 

# filter for blockno and remove really high CPUE
time.swim.cpue.site.loc <- time.swim.cpue.site.loc %>% 
        filter(blockno == 24 &
                       est.kg.hr < 200) %>% 
        st_as_sf

# df.1.bbox <- st_bbox(time.swim.cpue.site.loc)

# filter subblock map for blockno
sf.subblock.map.crop <- sf.subblock.map %>% 
        filter(blockno == 24) %>% 
        st_as_sf() 

# crop tas map to blockno
sf.tas.map.crop <- st_crop(sf.tas.map, sf.subblock.map.crop)

# convert to sp
sp.tas.map.crop <- as_Spatial(sf.tas.map.crop)
sp.time.swim.cpue.site.loc <- as_Spatial(time.swim.cpue.site.loc)

# grid (1 ha side = 402.0673 m ?)
idw_grid <- st_make_grid(time.swim.cpue.site.loc, cellsize = 402.0673, square = FALSE)

# idw
P_idw_hex <- gstat::idw(est.kg.hr ~ 1, sp.time.swim.cpue.site.loc, newdata = idw_grid, idp = 2)

rslt_hex <- st_as_sf(P_idw_hex)

ggplot(data = st_geometry(sf.tas.map.crop)) +
        geom_sf(fill = NA) +
        geom_sf(data = sf.subblock.map.crop, aes(label = subblockno))+
        geom_sf_text(data = sf.subblock.map.crop, aes(label = subblockno))+
        geom_sf(data = time.swim.cpue.site.loc, aes(fill = est.kg.hr), shape = 21, size = 3)+
        scale_fill_gradientn(colours = c("navyblue", "blue", "cyan", "green", "yellow", "orange", "red"))+
        theme_bw() +
        annotation_scale(location = "bl", width_hint = 0.5) +
        annotation_north_arrow(location = "br", which_north = "true", 
                               pad_x = unit(0.05, "cm"), pad_y = unit(0.1, "cm"),
                               style = north_arrow_fancy_orienteering)+
        labs(fill = 'Kg/Hr')+
        xlab('Longitude')+
        ylab('Latitude')

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
## PLOT 2: Diver bias 
## Diver A vs Diver B comparison of sizeclass counts per site

diver.a <- 'LT'
diver.b <- 'BD'

df.1 <- time.swim.dat %>% 
        mutate(sizeclass = factor(sizeclass, levels = sizeclasses)) %>%
        filter(diver %in% c(diver.a)) %>% 
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
        