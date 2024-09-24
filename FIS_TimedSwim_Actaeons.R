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
 # library(rgdal)
 library(RColorBrewer)
 library(viridis)
 library(ggpmisc)
 library(arsenal)
 library(fuzzyjoin)
 library(tidytext)
 library(ggExtra)
})

source("C:/GitCode/AbResearch/getLegend.r")
source("C:/GitCode/AbResearch/StandardError_Functions.r")

##---------------------------------------------------------------------------##
## 2. Set sample year and file paths ####

# identify sampling year of interest
samp.year <- 2024

# identify folder path to import data
act_data_input_folder <- file.path(paste(sprintf('C:/Users/%s/Dropbox (UTAS Research)/DiveFisheries/Abalone/FISdata', 
                                            Sys.info()[["user"]])), paste('FIS_TimedSwimSurveys', samp.year, sep = ''))

# identify folder path to save output data
act_ts_data_folder <- file.path(paste(sprintf('C:/Users/%s/Dropbox (UTAS Research)/DiveFisheries/Projects/AIRF_2022_53/Data',
                                              Sys.info()[["user"]])), sep = '')

# identify folder path to save figures
act_ts_figures_folder <- file.path(paste(sprintf('C:/Users/%s/Dropbox (UTAS Research)/DiveFisheries/Projects/AIRF_2022_53/DataAnalysis/Figures',
                                           Sys.info()[["user"]])), sep = '')

# identify associated spatial layers folder
spat_layer_folder <- file.path(paste(sprintf('C:/Users/%s/Dropbox (UTAS Research)/DiveFisheries/GIS/SpatialLayers/TimeSwimLayers', 
                                             Sys.info()[["user"]])))

##---------------------------------------------------------------------------##
# 3. Load data ####

# clear list
# rm(list = setdiff(ls(), c('samp.year', 'act_data_input_folder', 'ts.plots.folder')))

# Import final dataframes 
time.swim.dat.final <-
 readRDS(paste(act_data_input_folder, '/time.swim.dat.final.RDS', sep = ''))

time.swim.dat.df.final <-
 readRDS(paste(act_data_input_folder, '/time.swim.dat.df.final.RDS', sep = ''))

# Import metadata frame
time.swim.meta.dat.final <- readRDS(paste(act_data_input_folder, '/time.swim.meta.dat.final.RDS', sep = ''))

##---------------------------------------------------------------------------##
# Filter Block 13 data and identify sample periods and size classes

act_ts_dat <- time.swim.dat.final %>% 
 filter(blockno == 13) %>% 
 mutate(samp_period = ifelse(between(sampdate, as.Date('2021-01-01'), as.Date('2021-03-31'))|
                             between(sampdate, as.Date('2023-01-01'), as.Date('2023-03-31')), 'Pre', 
                             ifelse(between(sampdate, as.Date('2023-04-01'), as.Date('2023-12-31')), 'Mid', 'Post')))


act_ts_dat <- act_ts_dat %>% 
 mutate(sizeclass_actaeons = ifelse(sizeclass %in% c("0-20", "20-40", "40-60", "60-80", "80-100", "100-120"), '0-120 mm',
                                   ifelse(sizeclass %in% c("120-140"), '120-140 mm', '>140 mm')))

##---------------------------------------------------------------------------##
# quick summary of sample periods and sites surveyed

act_ts_dat %>%
 filter(sampyear >= 2023) %>% 
 group_by(samp_period) %>% 
 summarise(start_date = min(sampdate),
           end_date = max(sampdate),
           sites = n_distinct(site))

##---------------------------------------------------------------------------##
# plot abundance by site for each sample period

# need to take average of site AB-2023-13-45 as was surveyed twice mid season
df_4 <- act_ts_dat %>% 
 filter(sampyear >= 2023 &
        !(site == 'AB-2023-13-45' &
        samp_period == 'Mid')) %>%
 group_by(blockno, site, sampyear, sampdate, samp_period, sizeclass_actaeons, diver) %>% 
 summarise(ab_n = sum(sizeclass_freq_10)) %>% 
 group_by(samp_period, site, sizeclass_actaeons) %>% 
 summarise(mean_ab_n = mean(ab_n))

# average for AB-2023-13-45 mid season
df_3 <- act_ts_dat %>% 
 filter(sampyear >= 2023,
        site == 'AB-2023-13-45',
        samp_period == 'Mid') %>% 
 group_by(blockno, site, sampyear, samp_period, sizeclass_actaeons, diver) %>% 
 summarise(ab_n = mean(sizeclass_freq_10)) %>% 
 group_by(samp_period, site, sizeclass_actaeons) %>% 
 summarise(mean_ab_n = mean(ab_n))

# re-combine data
df_2 <- bind_rows(df_4, df_3)

# determine number of sampling periods for each site
df_5 <- df_2 %>% 
 group_by(site) %>% 
 summarise(samp_periods = n_distinct(samp_period)) %>% 
 arrange(samp_periods)

# join sampling periods to original data
df_6 <- left_join(df_2, df_5)

# filter data where sites were surveyed in all three periods
df_7 <- df_6 %>% 
 filter(samp_periods >= 3)

# identify size class for plot
plot_size_class <- '>140 mm'

plot_pre <- df_7 %>% 
 filter(samp_period == 'Pre', sizeclass_actaeons == plot_size_class) %>%  
 ggplot(aes(x = reorder(site, -mean_ab_n), y = mean_ab_n))+
 geom_bar(stat = 'identity')+
 ylim(0, 100)+
 xlab('Site')+
 ylab(bquote('Total count (abalone.10'*~min^-1*')'))+
 ggtitle(paste('Pre-season ', plot_size_class))+
 theme_bw()+
 theme(axis.text.x = element_text(angle = 90, vjust = 0.5))  

plot_mid <- df_7 %>% 
 filter(samp_period == 'Mid', sizeclass_actaeons == plot_size_class) %>%  
 ggplot(aes(x = reorder(site, -mean_ab_n), y = mean_ab_n))+
 geom_bar(stat = 'identity')+
 ylim(0, 100)+
 xlab('Site')+
 ylab(bquote('Total count (abalone.10'*~min^-1*')'))+
 ggtitle(paste('Mid-season ', plot_size_class))+
 theme_bw()+
 theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

plot_post <- df_7 %>% 
 filter(samp_period == 'Post', sizeclass_actaeons == plot_size_class) %>%  
 ggplot(aes(x = reorder(site, -mean_ab_n), y = mean_ab_n))+
 geom_bar(stat = 'identity')+
 ylim(0, 100)+
 xlab('Site')+
 ylab(bquote('Total count (abalone.10'*~min^-1*')'))+
 ggtitle(paste('Post-season ', plot_size_class))+
 theme_bw()+
 theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

samp_period_plot <- grid.arrange(plot_pre, plot_mid, plot_post, nrow = 3)   

##---------------------------------------------------------------------------##
# plot site difference for each sample period and size class

# spread data by sample period and size class
df_8 <- df_7 %>% 
 pivot_wider(., id_cols = site,
             names_from = c(samp_period, sizeclass_actaeons),
             values_from = mean_ab_n)

names(df_8)

df_9 <- df_8 %>% 
 mutate('Pre_Post_0-120' = (`Post_0-120 mm`) - (`Pre_0-120 mm`),
        'Pre_Post_120-140' = (`Post_120-140 mm`) - (`Pre_120-140 mm`),
        'Pre_Post_>140' = (`Post_>140 mm`) - (`Pre_>140 mm`),
        'Mid_Post_0-120' = (`Post_0-120 mm`) - (`Mid_0-120 mm`),
        'Mid_Post_120-140' = (`Post_120-140 mm`) - (`Mid_120-140 mm`),
        'Mid_Post_>140' = (`Post_>140 mm`) - (`Mid_>140 mm`),
        'Pre_Mid_0-120' = (`Mid_0-120 mm`) - (`Pre_0-120 mm`),
        'Pre_Mid_120-140' = (`Mid_120-140 mm`) - (`Pre_120-140 mm`),
        'Pre_Mid_>140' = (`Mid_>140 mm`) - (`Pre_>140 mm`)) %>% 
 select('Pre_Post_0-120', 'Pre_Post_120-140', 'Pre_Post_>140',
        'Mid_Post_0-120', 'Mid_Post_120-140', 'Mid_Post_>140',
        'Pre_Mid_0-120', 'Pre_Mid_120-140', 'Pre_Mid_>140')

act_ts_dat0 <- df_9 %>% 
 gather(key = 'samp_period_size', 
        value = 'count_diff',
        2:10) %>% 
 mutate(size_class = ifelse(samp_period_size %in% c('Pre_Post_0-120', 'Mid_Post_0-120', 'Pre_Mid_0-120'), '0-120 mm',
                            ifelse(samp_period_size %in% c('Pre_Post_120-140', 'Mid_Post_120-140', 'Pre_Mid_120-140'), '120-140 mm', '>140 mm')),
        samp_diff = ifelse(grepl('Mid_Post', samp_period_size), 'Mid_Post',
                           ifelse(grepl('Pre_Mid', samp_period_size), 'Pre_Mid', 'Pre_Post')))

site_diff_plot <- df_10 %>% 
 filter(samp_diff == 'Pre_Post') %>% 
 ggplot(aes(x = reorder_within(site, -count_diff, size_class), y = count_diff))+
 geom_bar(stat = 'identity')+
 scale_x_reordered()+
 ylim(-70, 70)+
 xlab('Site')+
 ylab(bquote('Count difference (abalone.10'*~min^-1*')'))+
 theme_bw()+
 geom_vline(xintercept = 50, linetype = 'dashed', colour = 'red', size = 0.5)+
 facet_grid(. ~ factor(size_class, levels = c('0-120 mm', '120-140 mm', '>140 mm')), scales = 'free_x')+
 # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 2), panel.grid.major.x = element_blank())+
 theme(axis.text.x = element_blank(), 
       panel.grid.major.x = element_blank(), 
       axis.ticks.x = element_line(colour = "black", size = 0.1))

# save figure
ggsave(path = act_ts_figures_folder, filename = paste('Actaeons_TimedSwimSurvey_', samp.year, '_PrePost_Site_SizeClass_Abundance', '.pdf', sep = ''),
       plot = site_diff_plot, units = 'mm', width = 200, height = 150)
ggsave(path = act_ts_figures_folder, filename = paste('Actaeons_TimedSwimSurvey_', samp.year, '_PrePost_Site_SizeClass_Abundance', '.png', sep = ''),
       plot = site_diff_plot, units = 'mm', width = 200, height = 150)

##---------------------------------------------------------------------------##
# Figure 1: size class abundance for sample period

# Determine mean abalone abundance in each sample period and size class
ten_min_mean_period <- act_ts_dat %>% 
 filter(sampyear >= 2023,
        !is.na(sizeclass_freq_10),
        !is.infinite(sizeclass_freq_10),
        sizeclass_freq_10 >= 0) %>%
 group_by(samp_period, sizeclass_actaeons, site, diver) %>% 
 summarise(ab.n = sum(sizeclass_freq_10)) %>% 
 group_by(samp_period, sizeclass_actaeons) %>% 
 summarise(mean_ab_n = mean(ab.n),
           median_ab_n = median(ab.n))

# Determine number of sites surveyed in each block, year and size class
time_swim_site_n <- act_ts_dat %>% 
 filter(sampyear >= 2023) %>%
 group_by(samp_period) %>% 
 summarise(n = n_distinct(site))

# figure for 0-120 mm
sub_legal_plot <- act_ts_dat %>% 
 filter(sizeclass_actaeons == '0-120 mm' &
         sampyear >= 2023) %>% 
 group_by(samp_period, site, diver) %>% 
 summarise(ab_n = sum(sizeclass_freq_10)) %>% 
 group_by(samp_period, site) %>% 
 summarise(mean_ab_n = mean(ab_n)) %>% 
 mutate(samp_period = factor(samp_period, levels = c('Pre', 'Mid', 'Post'))) %>%     
 ggplot(aes(x = samp_period, y = mean_ab_n))+
 geom_boxplot(aes(fill = samp_period), position = position_dodge2(1, preserve = 'single'),
              outlier.colour = '#EE8866') +
 scale_fill_manual(values = c("#77AADD", "#BBCC33", "#DDDDDD"))+
 geom_point(data = ten_min_mean_period %>% filter(sizeclass_actaeons == '0-120 mm'), aes(group = factor(samp_period, levels = c('Pre', 'Mid', 'Post'))), shape = 19,
            size = 2, colour = 'red', fill = 'red', position = position_dodge2(0.8, preserve = 'single'))+
 theme_bw()+
 ylab(bquote('Average count (abalone.10'*~min^-1*')'))+
 xlab('Sample Period')+
 coord_cartesian(ylim = c(0, 150))+
 geom_text(data = time_swim_site_n, aes(y = 150, label = n, colour = factor(samp_period, levels = c('Pre', 'Mid', 'Post'))), size = 3,
           position = position_dodge2(0.8))+
 scale_colour_manual(values = c("#77AADD", "#BBCC33", "#DDDDDD"))+
 ggtitle('Sub-legal 0-120 mm')+
 theme(plot.title = element_text(vjust = 0, hjust = 0))+
 theme(legend.position = 'none')

# figure for 120-140 mm
sub_legal_plot_2 <- act_ts_dat %>% 
 filter(sizeclass_actaeons == '120-140 mm' &
         sampyear >= 2023) %>% 
 group_by(samp_period, site, diver) %>% 
 summarise(ab_n = sum(sizeclass_freq_10)) %>% 
 group_by(samp_period, site) %>% 
 summarise(mean_ab_n = mean(ab_n)) %>% 
 mutate(samp_period = factor(samp_period, levels = c('Pre', 'Mid', 'Post'))) %>%     
 ggplot(aes(x = samp_period, y = mean_ab_n))+
 geom_boxplot(aes(fill = samp_period), position = position_dodge2(1, preserve = 'single'),
              outlier.colour = '#EE8866') +
 scale_fill_manual(values = c("#77AADD", "#BBCC33", "#DDDDDD"))+
 geom_point(data = ten_min_mean_period %>% filter(sizeclass_actaeons == '120-140 mm'), aes(group = factor(samp_period, levels = c('Pre', 'Mid', 'Post'))), shape = 19,
            size = 2, colour = 'red', fill = 'red', position = position_dodge2(0.8, preserve = 'single'))+
 theme_bw()+
 ylab(bquote('Average count (abalone.10'*~min^-1*')'))+
 xlab('Sample Period')+
 coord_cartesian(ylim = c(0, 150))+
 geom_text(data = time_swim_site_n, aes(y = 150, label = n, colour = factor(samp_period, levels = c('Pre', 'Mid', 'Post'))), size = 3,
           position = position_dodge2(0.8))+
 scale_colour_manual(values = c("#77AADD", "#BBCC33", "#DDDDDD"))+
 ggtitle('Sub-legal 120-140 mm')+
 theme(plot.title = element_text(vjust = 0, hjust = 0))+
 theme(legend.position = 'none')

# figure for >140 mm
legal_plot <- act_ts_dat %>% 
 filter(sizeclass_actaeons == '>140 mm' &
         sampyear >= 2023) %>% 
 group_by(samp_period, site, diver) %>% 
 summarise(ab_n = sum(sizeclass_freq_10)) %>% 
 group_by(samp_period, site) %>% 
 summarise(mean_ab_n = mean(ab_n)) %>% 
 mutate(samp_period = factor(samp_period, levels = c('Pre', 'Mid', 'Post'))) %>%     
 ggplot(aes(x = samp_period, y = mean_ab_n))+
 geom_boxplot(aes(fill = samp_period), position = position_dodge2(1, preserve = 'single'),
              outlier.colour = '#EE8866') +
 scale_fill_manual(values = c("#77AADD", "#BBCC33", "#DDDDDD"))+
 geom_point(data = ten_min_mean_period %>% filter(sizeclass_actaeons == '>140 mm'), aes(group = factor(samp_period, levels = c('Pre', 'Mid', 'Post'))), shape = 19,
            size = 2, colour = 'red', fill = 'red', position = position_dodge2(0.8, preserve = 'single'))+
 theme_bw()+
 ylab(bquote('Average count (abalone.10'*~min^-1*')'))+
 xlab('Sample Period')+
 coord_cartesian(ylim = c(0, 150))+
 geom_text(data = time_swim_site_n, aes(y = 150, label = n, colour = factor(samp_period, levels = c('Pre', 'Mid', 'Post'))), size = 3,
           position = position_dodge2(0.8))+
 scale_colour_manual(values = c("#77AADD", "#BBCC33", "#DDDDDD"))+
 ggtitle('Legal >140 mm')+
 theme(plot.title = element_text(vjust = 0, hjust = 0))+
 theme(legend.position = 'none')

# Join plots
count_plot_samp_period <- grid.arrange(sub_legal_plot, sub_legal_plot_2, legal_plot, nrow = 1)  

# save figure
ggsave(path = act_ts_figures_folder, filename = paste('Actaeons_TimedSwimSurvey_', samp.year, '_PreMidPost_SizeClass_Abundance', '.pdf', sep = ''),
       plot = count_plot_samp_period, units = 'mm', width = 200, height = 150)
ggsave(path = act_ts_figures_folder, filename = paste('Actaeons_TimedSwimSurvey_', samp.year, '_PreMidPost_SizeClass_Abundance', '.png', sep = ''),
       plot = count_plot_samp_period, units = 'mm', width = 200, height = 150)
##---------------------------------------------------------------------------##

act_ts_dat %>% 
 filter(sampyear >= 2023) %>% 
 group_by(samp_period, site, legal.size, diver) %>% 
 summarise(ab_n = sum(sizeclass_freq_10)) %>% 
 group_by(samp_period,legal.size, site) %>% 
 summarise(mean_ab_n = mean(ab_n)) %>% 
 mutate(samp_period = factor(samp_period, levels = c('Pre', 'Mid', 'Post'))) %>% 
 spread(samp_period, mean_ab_n) %>% 
 ggplot(aes(x = pre, y = post, colour = legal.size))+
 geom_point(size = 2, shape = 19)+
 geom_smooth(method = 'lm', formula = y~x, se = F)+
 stat_poly_eq(formula = y~x, aes(label = paste(after_stat(rr.label), p.value.label, sep = "~~~")),
              parse = TRUE) +
 theme_bw()+
 guides(colour = guide_legend(title = "Size Class"))
##---------------------------------------------------------------------------##                                    
# MAP 1: Site Abundance Sample Period ####

# Average count by site for each sample period represented by a coloured circle
# run for both size classes

# sf.tas.map <- st_read(paste(sprintf("C:/Users/%s/University of Tasmania/IMAS-DiveFisheries - Assessments - Documents/Assessments/GIS/SpatialLayers/OldAbLayers/TasLand.gpkg", Sys.info()[["user"]])))
sf.tas.map <- st_read(paste(sprintf("C:/Users/%s/Dropbox (UTAS Research)/DiveFisheries/GIS/SpatialLayers/OldAbLayers/TasLand.gpkg", Sys.info()[["user"]])))
# sf.subblock.map <- st_read(paste(sprintf("C:/Users/%s/University of Tasmania/IMAS-DiveFisheries - Assessments - Documents/Assessments/GIS/SpatialLayers/IMAS_Layers/IMAS_subblock_rev2022.gpkg", Sys.info()[["user"]])))
sf.subblock.map <- st_read(paste(sprintf("C:/Users/%s/Dropbox (UTAS Research)/DiveFisheries/GIS/SpatialLayers/IMAS_Layers/IMAS_subblock_rev2022.gpkg", Sys.info()[["user"]])))

# transform maps to GDA2020

# set CRS
GDA2020 <- st_crs(7855)
GDA94 <- st_crs(28355)
WGS84 <- st_crs(4326)

sf.tas.map <- sf.tas.map %>% 
 st_set_crs(GDA2020)

sf.subblock.map <- st_transform(sf.subblock.map, GDA2020) %>% 
 rename_all(., .funs = tolower)

# Identify unique sites sampled from timed swim dataframe and join to renamed site file
act_ts_sites <- act_ts_dat %>% 
 filter(sampyear >= 2023) %>% 
 distinct(site, .keep_all = T) %>% 
 dplyr::select(c(sampyear, site, blockno, subblockno, sampdate, actual.geom)) %>% 
 st_as_sf() %>% 
 st_set_crs(GDA2020)

ten_min_mean_site_period <- act_ts_dat %>% 
 filter(sampyear >= 2023,
        !is.na(sizeclass_freq_10),
        !is.infinite(sizeclass_freq_10),
        sizeclass_freq_10 >= 0) %>%
 group_by(samp_period, sizeclass_actaeons, site, diver) %>% 
 summarise(ab.n = sum(sizeclass_freq_10)) %>% 
 group_by(site, samp_period, sizeclass_actaeons) %>% 
 summarise(mean_ab_n = mean(ab.n),
           median_ab_n = median(ab.n)) 

ts_count_site_loc <- left_join(ten_min_mean_site_period, act_ts_sites) %>% 
 st_as_sf() 

# crop sub-block map
 ts.tas.subblockmap.crop <- sf.subblock.map %>%  
  filter(., zone == 'E', blockno == 13)
 
# crop tas land map to subblock
 ts.tas.coast.crop <- st_crop(sf.tas.map, ts.tas.subblockmap.crop)

# add buffer to site data for map crop
 act_buffer <- st_buffer(ts_count_site_loc, 500)

# crop map to site data with buffer  
ts.tas.coast.crop <- st_crop(sf.tas.map, act_buffer)

 # create plot
 count_site_map <- ggplot(data = st_geometry(ts.tas.coast.crop)) +
  # geom_sf(data = ts_count_site_loc %>% 
  #          filter(sizeclass_actaeons == ts_legal_size_plot), fill = NA)+
  geom_sf(fill = 'grey') +
  geom_sf(data = ts_count_site_loc, aes(fill = mean_ab_n), shape = 21, size = 1.5)+
  scale_fill_gradientn(colours = c("navyblue", "blue", "cyan", "green", "yellow", "orange", "red"),
                       limits = c(0, 125),
                       breaks = c(0, 50, 100),
                       labels = c(0, 50, 100))+
  theme_bw() +
  labs(fill = (bquote('Average\ncount')))+
  scale_x_continuous(breaks = seq(140, 149, by = 0.1))+
  metR::scale_y_latitude(breaks = seq(-43.7, -43.5, 0.05))+
  ylab('Latitude')+
  facet_grid(factor(sizeclass_actaeons, levels = c('0-120 mm', '120-140 mm', '>140 mm')) ~ 
              factor(samp_period, levels = c('Pre', 'Mid', 'Post')))+
  theme(axis.text.y = element_text(angle = 90))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.05, "cm"), pad_y = unit(0.4, "cm"),
                         style = north_arrow_fancy_orienteering)
  # theme(plot.margin = grid::unit(c(0,0,0,0), "mm"))
 
 # save figure
 ggsave(path = act_ts_figures_folder, filename = paste('Actaeons_TimedSwimSurvey_', samp.year, '_Site_PreMidPost_SizeClass_Abundance', '.pdf', sep = ''),
        plot = count_site_map, units = 'mm', width = 300, height = 200)
 ggsave(path = act_ts_figures_folder, filename = paste('Actaeons_TimedSwimSurvey_', samp.year, '_Site_PreMidPost_SizeClass_Abundance', '.png', sep = ''),
        plot = count_site_map, units = 'mm', width = 300, height = 200)
 
##----------------------------------------------------------------------------##
 ## Compare pre and post fishing swim direction and distance
 ## Are we covering the same site in our surveys?
 
 df_3 <- time.swim.meta.dat.final %>% 
  filter(blockno == 13) %>% 
  mutate(sampdate = as.Date(starttime),
         samp_period = ifelse(between(sampdate, as.Date('2021-01-01'), as.Date('2021-03-31'))|
                               between(sampdate, as.Date('2023-01-01'), as.Date('2023-03-31')), 'Pre', 
                              ifelse(between(sampdate, as.Date('2023-04-01'), as.Date('2023-12-31')), 'Mid', 'Post'))) %>% 
  filter(!st_is_empty(start_geom), !st_is_empty(finish_geom),
         sampdate != as.Date('2023-08-29') & site != 'AB-2023-13-45') %>% 
  mutate(dive_dist = units::drop_units(st_distance(start_geom, finish_geom, by_element = T)),
         dive_dir = nngeo::st_azimuth(start_geom, finish_geom))

 # distance of swim in pre vs post survey                                   
df_3 %>% 
  select(site, blockno, samp_period, dive_dist) %>% 
  spread(key = samp_period, value = dive_dist) %>%  
  filter(Pre != 'NA', Post != 'NA') %>% 
  ggplot(aes(x = Pre, y = Post))+
  geom_point()+
  theme_bw()+
  geom_smooth(method = 'lm', formula = y~x, se = F)+
  stat_poly_eq(formula = y~x, aes(label = paste(after_stat(rr.label), p.value.label, sep = "~~~")),
               parse = TRUE) +
  xlab('Pre-season Distance (m)')+
  ylab('Post-season Distance (m)')+
  ylim(0, 160)+
  xlim(0, 160)

# direction of swim in pre vs post survey
df_3 %>% 
 select(site, blockno, samp_period, dive_dir) %>% 
 spread(key = samp_period, value = dive_dir) %>%  
 filter(Pre != 'NA', Post != 'NA') %>% 
 ggplot(aes(x = Pre, y = Post))+
 geom_point()+
 theme_bw()+
 geom_smooth(method = 'lm', formula = y~x, se = F)+
 stat_poly_eq(formula = y~x, aes(label = paste(after_stat(rr.label), p.value.label, sep = "~~~")),
              parse = TRUE) +
 xlab('Pre-season Direction')+
 ylab('Post-season Direction')+
 ylim(0, 400)+
 xlim(0, 400)

##----------------------------------------------------------------------------##
## Relationship between pre, mid and post season surveys between sites

ten_min_mean_period <- act_ts_dat %>% 
 filter(sampyear >= 2023,
        !is.na(sizeclass_freq_10),
        !is.infinite(sizeclass_freq_10),
        sizeclass_freq_10 >= 0) %>%
 group_by(samp_period, sizeclass_actaeons, site, diver) %>% 
 summarise(ab.n = sum(sizeclass_freq_10)) %>%  
 group_by(site, samp_period, sizeclass_actaeons) %>% 
 summarise(mean_ab_n = mean(ab.n)) %>% 
 spread(key = samp_period, value = mean_ab_n)

ten_min_mean_period %>%
 ggplot()+
 geom_point(aes(x = Pre, y = Post))+
 facet_grid(factor(sizeclass_actaeons, levels = c('0-120 mm', '120-140 mm', '>140 mm')) ~ .) +
 theme_bw()

size_class <- '120-140 mm'

df_2 <- act_ts_dat %>% 
 filter(sampyear >= 2023,
        !is.na(sizeclass_freq_10),
        !is.infinite(sizeclass_freq_10),
        sizeclass_freq_10 >= 0) %>%
 group_by(samp_period, sizeclass_actaeons, site) %>% 
 summarise(ab.n = sum(sizeclass_freq_10)) %>% 
 filter(sizeclass_actaeons == size_class) %>% 
 spread(key = samp_period, value = ab.n) %>% 
 select(site, Pre, Mid, Post)

limitRange <- function(data, mapping, ...) { 
 ggplot(data = data, mapping = mapping, ...) + 
  geom_point(...) + 
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(limits = c(0, 160)) +
  scale_x_continuous(limits = c(0, 160)) 
}

corr_plot <- df_2 %>% 
 # filter(legal.size == size_class) %>% 
 GGally::ggpairs(columns = 3:5, title = size_class, 
                 lower = list(continuous = limitRange))+
 theme_bw()+
 xlab('Abalone Count')+
 ylab('Abalone Count')+
 scale_x_continuous(limits = c(0, 160))

ggsave(path = act_ts_figures_folder, filename = paste('Actaeons_TimedSwimSurvey_', samp.year, '_Site_PreMidPost_Correlation_Abundance_120-140mm', '.pdf', sep = ''),
       plot = corr_plot, units = 'mm', width = 300, height = 200)
ggsave(path = act_ts_figures_folder, filename = paste('Actaeons_TimedSwimSurvey_', samp.year, '_Site_PreMidPost_Correlation_Abundance_120-140mm', '.png', sep = ''),
       plot = corr_plot, units = 'mm', width = 300, height = 200)
##---------------------------------------------------------------------------##
## Relationship between pre, mid and post season surveys between sites - marginal histogram

# Site vs site marginal histogram

act_site_rep <- act_ts_dat %>% 
 filter(sampyear >= 2023) %>% 
 group_by(site) %>% 
 summarise(sampled_n = n_distinct(sampdate))

act_site_dat <- act_ts_dat %>% 
 filter(sampyear >= 2023) %>% 
 left_join(., act_site_rep) %>% 
 filter(sampled_n >= 2)

size_class <- '120-140 mm'

act_plot_dat <- act_site_dat %>% 
 filter(!is.na(sizeclass_freq_10),
        !is.infinite(sizeclass_freq_10),
        sizeclass_freq_10 >= 0,
        sampled_n == 3) %>%
 group_by(samp_period, sizeclass_actaeons, site) %>% 
 summarise(ab.n = sum(sizeclass_freq_10)) %>%  
 filter(sizeclass_actaeons == size_class) %>% 
 spread(key = samp_period, value = ab.n) %>% 
 select(site, Pre, Mid, Post)

x_axis <- 'Pre'
y_axis <- 'Post'

act_plot <- act_plot_dat %>% 
 ggplot(aes_string(x = x_axis, y = y_axis))+
 geom_point()+
 theme_bw()+
 theme(legend.position = 'none')+
 xlab(paste(x_axis, '-season Abalone Count', sep = ''))+
 ylab(paste(y_axis, '-season Abalone Count', sep = ''))+
 geom_text(aes(x = 125, y = 10, label = size_class), stat = 'unique', colour = 'black', size = 5)+
 theme(plot.title = element_text(hjust = 1, vjust = -100))+
 scale_x_continuous(limits = c(0, 160))+
 geom_smooth(method = 'lm', formula = y~x, se = F)+
 stat_poly_eq(formula = y~x, aes(label = paste(..rr.label.., p.value.label, sep = "~~~")), 
              parse = TRUE, label.y = c(0.95, 0.90))+
 xlim(0, 160)+
 ylim(0, 160)

act_marg_plot_1 <- ggMarginal(act_plot, type = 'density', groupColour = F, groupFill = F)

x_axis <- 'Pre'
y_axis <- 'Mid'

act_plot <- act_plot_dat %>% 
 ggplot(aes_string(x = x_axis, y = y_axis))+
 geom_point()+
 theme_bw()+
 theme(legend.position = 'none')+
 xlab(paste(x_axis, '-season Abalone Count', sep = ''))+
 ylab(paste(y_axis, '-season Abalone Count', sep = ''))+
 geom_text(aes(x = 125, y = 10, label = size_class), stat = 'unique', colour = 'black', size = 5)+
 theme(plot.title = element_text(hjust = 1, vjust = -100))+
 scale_x_continuous(limits = c(0, 160))+
 geom_smooth(method = 'lm', formula = y~x, se = F)+
 stat_poly_eq(formula = y~x, aes(label = paste(..rr.label.., p.value.label, sep = "~~~")), 
              parse = TRUE, label.y = c(0.95, 0.90))+
 xlim(0, 160)+
 ylim(0, 160)

act_marg_plot_2 <- ggMarginal(act_plot, type = 'density', groupColour = F, groupFill = F)

x_axis <- 'Mid'
y_axis <- 'Post'

act_plot <- act_plot_dat %>% 
 ggplot(aes_string(x = x_axis, y = y_axis))+
 geom_point()+
 theme_bw()+
 theme(legend.position = 'none')+
 xlab(paste(x_axis, '-season Abalone Count', sep = ''))+
 ylab(paste(y_axis, '-season Abalone Count', sep = ''))+
 geom_text(aes(x = 125, y = 10, label = size_class), stat = 'unique', colour = 'black', size = 5)+
 theme(plot.title = element_text(hjust = 1, vjust = -100))+
 scale_x_continuous(limits = c(0, 160))+
 geom_smooth(method = 'lm', formula = y~x, se = F)+
 stat_poly_eq(formula = y~x, aes(label = paste(..rr.label.., p.value.label, sep = "~~~")), 
              parse = TRUE, label.y = c(0.95, 0.90))+
 xlim(0, 160)+
 ylim(0, 160)

act_marg_plot_3 <- ggMarginal(act_plot, type = 'density', groupColour = F, groupFill = F)

act_marg_plot <- grid.arrange(act_marg_plot_1, act_marg_plot_2, act_marg_plot_3, ncol = 3)

ggsave(path = act_ts_figures_folder, filename = paste('Actaeons_TimedSwimSurvey_', samp.year, '_CorrelationPlot_120-140mm', '.pdf', sep = ''),
       plot = act_marg_plot, units = 'mm', width = 300, height = 200)
ggsave(path = act_ts_figures_folder, filename = paste('Actaeons_TimedSwimSurvey_', samp.year, '_CorrelationPlot_120-140mm', '.png', sep = ''),
       plot = act_marg_plot, units = 'mm', width = 300, height = 200)
##---------------------------------------------------------------------------##
## Relationship between pre, mid and post season surveys between sites - marginal histogram
## <140 and >140 mm only

# Site vs site marginal histogram

act_site_rep <- act_ts_dat %>% 
 filter(sampyear >= 2023) %>% 
 group_by(site) %>% 
 summarise(sampled_n = n_distinct(sampdate))

act_site_dat <- act_ts_dat %>% 
 filter(sampyear >= 2023) %>% 
 left_join(., act_site_rep) %>% 
 filter(sampled_n >= 2)

size_class <- '>140 mm'

act_plot_dat <- act_site_dat %>% 
 filter(!is.na(sizeclass_freq_10),
        !is.infinite(sizeclass_freq_10),
        sizeclass_freq_10 >= 0,
        sampled_n == 3) %>%
 group_by(samp_period, legal.size, site) %>% 
 summarise(ab.n = sum(sizeclass_freq_10)) %>%  
 filter(legal.size == size_class) %>% 
 spread(key = samp_period, value = ab.n) %>% 
 select(site, Pre, Mid, Post)

x_axis <- 'Pre'
y_axis <- 'Post'

act_plot <- act_plot_dat %>% 
 ggplot(aes_string(x = x_axis, y = y_axis))+
 geom_point()+
 theme_bw()+
 theme(legend.position = 'none')+
 xlab(paste(x_axis, '-season Abalone Count', sep = ''))+
 ylab(paste(y_axis, '-season Abalone Count', sep = ''))+
 geom_text(aes(x = 125, y = 10, label = size_class), stat = 'unique', colour = 'black', size = 5)+
 theme(plot.title = element_text(hjust = 1, vjust = -100))+
 scale_x_continuous(limits = c(0, 160))+
 geom_smooth(method = 'lm', formula = y~x, se = F)+
 stat_poly_eq(formula = y~x, aes(label = paste(..rr.label.., p.value.label, sep = "~~~")), 
              parse = TRUE, label.y = c(0.95, 0.90))+
 xlim(0, 160)+
 ylim(0, 160)

act_marg_plot_1 <- ggMarginal(act_plot, type = 'density', groupColour = F, groupFill = F)

x_axis <- 'Pre'
y_axis <- 'Mid'

act_plot <- act_plot_dat %>% 
 ggplot(aes_string(x = x_axis, y = y_axis))+
 geom_point()+
 theme_bw()+
 theme(legend.position = 'none')+
 xlab(paste(x_axis, '-season Abalone Count', sep = ''))+
 ylab(paste(y_axis, '-season Abalone Count', sep = ''))+
 geom_text(aes(x = 125, y = 10, label = size_class), stat = 'unique', colour = 'black', size = 5)+
 theme(plot.title = element_text(hjust = 1, vjust = -100))+
 scale_x_continuous(limits = c(0, 160))+
 geom_smooth(method = 'lm', formula = y~x, se = F)+
 stat_poly_eq(formula = y~x, aes(label = paste(..rr.label.., p.value.label, sep = "~~~")), 
              parse = TRUE, label.y = c(0.95, 0.90))+
 xlim(0, 160)+
 ylim(0, 160)

act_marg_plot_2 <- ggMarginal(act_plot, type = 'density', groupColour = F, groupFill = F)

x_axis <- 'Mid'
y_axis <- 'Post'

act_plot <- act_plot_dat %>% 
 ggplot(aes_string(x = x_axis, y = y_axis))+
 geom_point()+
 theme_bw()+
 theme(legend.position = 'none')+
 xlab(paste(x_axis, '-season Abalone Count', sep = ''))+
 ylab(paste(y_axis, '-season Abalone Count', sep = ''))+
 geom_text(aes(x = 125, y = 10, label = size_class), stat = 'unique', colour = 'black', size = 5)+
 theme(plot.title = element_text(hjust = 1, vjust = -100))+
 scale_x_continuous(limits = c(0, 160))+
 geom_smooth(method = 'lm', formula = y~x, se = F)+
 stat_poly_eq(formula = y~x, aes(label = paste(..rr.label.., p.value.label, sep = "~~~")), 
              parse = TRUE, label.y = c(0.95, 0.90))+
 xlim(0, 160)+
 ylim(0, 160)

act_marg_plot_3 <- ggMarginal(act_plot, type = 'density', groupColour = F, groupFill = F)

act_marg_plot <- grid.arrange(act_marg_plot_2, act_marg_plot_3, act_marg_plot_1, ncol = 3)

ggsave(path = act_ts_figures_folder, filename = paste('Actaeons_TimedSwimSurvey_', samp.year, '_CorrelationPlot_Legal', '.pdf', sep = ''),
       plot = act_marg_plot, units = 'mm', width = 300, height = 200)
ggsave(path = act_ts_figures_folder, filename = paste('Actaeons_TimedSwimSurvey_', samp.year, '_CorrelationPlot_Legal', '.png', sep = ''),
       plot = act_marg_plot, units = 'mm', width = 300, height = 200)
##---------------------------------------------------------------------------##

## CPUE vs TS ####
## plot timed swim average counts for legal and sub-legall
## against historical fishery CPUE for each site

ts_oid <- read.xlsx(paste(sprintf("C:/Users/%s/Dropbox (UTAS Research)/DiveFisheries/GIS/SpatialLayers/TimeSwimLayers/TimedSwimSites_OID_CPUE.xlsx", Sys.info()[["user"]])),
                    detectDates = T)

## join original (unordered) cpue site data file used in 2020 survey to site data generated from GPS logger data
## to extract 'oid' and scoring metric rating/category (i.e. cell.ntile 3-5)


# # set CRS
# GDA2020 <- st_crs(7855)
# WGS84 <- st_crs(4326)
# UTM55S <- st_crs(32755)
# 
# # convert original cpue site data to sf
# df_cpue_sf <- df_cpue %>% 
#  st_as_sf(coords = c("xcoord", "ycoord"), crs = GDA2020) %>%
#  st_centroid(.) %>% 
#  select(zone, blockno, subblockno, oid, cell.ntile, geom) %>% 
#  filter(zone == 'E' & blockno == 13)
# 
# # transform final data to GDA2020
# df_3 <- df_1 %>% 
#  dplyr::rename('geom' = 'actual.geom') %>% 
#  filter(blockno == 13) %>% 
#  st_as_sf()

# join original cpue oid data and 2020 corrected final site data 
# df_4 <- st_join(df_3, df_cpue_sf, join = st_nearest_feature)

ts_cpue_dat_12_19 <- left_join(act_ts_dat, ts_oid %>% select(-c(zone, blockno, subblockno)), by = 'oid')

ts.vs.cpue.plot <- ts_cpue_dat_12_19 %>% 
 filter(samp_period == 'Mid') %>% 
 group_by(site, diver, legal.size, cell.ntile, cpue_kg_hr) %>% 
 summarise(ab.n = sum(sizeclass_freq_10)) %>% 
 group_by(site, legal.size, cell.ntile, cpue_kg_hr) %>% 
 summarise(mean.ab.n = mean(ab.n))%>%  
 filter(!is.na(cell.ntile)) %>% 
 ggplot(aes(x = cpue_kg_hr, y = mean.ab.n))+
 # geom_point(aes(colour = as.factor(cell.ntile)), size = 3)+
 geom_point(size = 3)+
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

setwd(act_ts_figures_folder)
ggsave(filename = paste('TimedSwimSurvey_TenMinuteCountvsCPUE_Actaeons_2012-2019', '.pdf', sep = ''), 
       plot = ts.vs.cpue.plot, units = 'mm', width = 190, height = 120)
ggsave(filename = paste('TimedSwimSurvey_TenMinuteCountvsCPUE_Actaeons_2012-2019', '.png', sep = ''), 
       plot = ts.vs.cpue.plot, units = 'mm', width = 190, height = 120)

##---------------------------------------------------------------------------## 
## CPUE vs TS for 2021-2023

hex_2021 <- readRDS(paste(sprintf("C:/Users/%s/Dropbox (UTAS Research)/DiveFisheries/GIS/SpatialLayers/TimeSwimLayers/hex2021.RDS", Sys.info()[["user"]])))
hex_2023 <- readRDS(paste(sprintf("C:/Users/%s/Dropbox (UTAS Research)/DiveFisheries/GIS/SpatialLayers/TimeSwimLayers/hex2023.RDS", Sys.info()[["user"]])))
hex_2024 <- readRDS(paste(sprintf("C:/Users/%s/Dropbox (UTAS Research)/DiveFisheries/GIS/SpatialLayers/TimeSwimLayers/hex2024.RDS", Sys.info()[["user"]])))

hex_2021 <- hex_2021 %>% 
 mutate(samp_year = 2021)

hex_2023 <- hex_2023 %>% 
 mutate(samp_year = 2023)

hex_2024 <- hex_2024 %>% 
 mutate(samp_year = 2024)

hex_2021_2024 <- bind_rows(hex_2021, hex_2023, hex_2024) %>% 
 st_set_geometry(NULL)

hex_2021_2024_blk_1 <- hex_2021_2024 %>% 
 filter(catch_species_1 == 'abb') %>% 
 select(oid, samp_year, catch, minutes) %>% 
 mutate(catch_blk = catch) %>% 
 select(-catch)

hex_2021_2024_blk_2 <- hex_2021_2024 %>% 
 filter(catch_species_2 == 'abb') %>% 
 select(oid, samp_year, catch, catch_sps_prop_2, minutes) %>% 
 mutate(catch_blk = catch * catch_sps_prop_2) %>% 
 select(-c(catch_sps_prop_2, catch))

hex_2021_2024_blk <- bind_rows(hex_2021_2024_blk_1, hex_2021_2024_blk_2)
 

hex_2021_2024_blk_cpue <- hex_2021_2024_blk %>% 
 group_by(oid) %>% 
 summarise(catch_blk = sum(catch_blk),
           minutes = sum(minutes)) %>% 
 mutate(cpue_blk = catch_blk / (minutes / 60),
        oid = as.character(oid))

ts_cpue_dat_21_24 <- left_join(act_ts_dat, hex_2021_2024_blk_cpue, by = 'oid')

ts_cpue_plot_21_24 <- ts_cpue_dat_21_24 %>% 
 filter(sampdate >= as.Date('2023-01-01') &
        samp_period == 'Mid') %>% 
 group_by(site, diver, legal.size, cpue_blk) %>% 
 summarise(ab.n = sum(sizeclass_freq_10)) %>% 
 group_by(site, legal.size, cpue_blk) %>% 
 summarise(mean.ab.n = mean(ab.n))%>%
 ggplot(aes(x = cpue_blk, y = mean.ab.n))+
 geom_point(size = 3)+
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

setwd(act_ts_figures_folder)
ggsave(filename = paste('TimedSwimSurvey_TenMinuteCountvsCPUE_Actaeons_2021-2024', '.pdf', sep = ''), 
       plot = ts_cpue_plot_21_24, units = 'mm', width = 190, height = 120)
ggsave(filename = paste('TimedSwimSurvey_TenMinuteCountvsCPUE_Actaeons_2021-2024', '.png', sep = ''), 
       plot = ts_cpue_plot_21_24, units = 'mm', width = 190, height = 120)

##---------------------------------------------------------------------------##
# 2012-19 vs 2021-24 hex oid

df_1 <- left_join(ts_cpue_dat_12_19, ts_cpue_dat_21_24) %>% 
 filter(sampdate >= as.Date('2023-01-01') &
         samp_period == 'Mid',
        !is.na(cpue_kg_hr),
        !is.na(cpue_blk)) %>% 
 distinct(oid, .keep_all = T) %>% 
 ggplot(aes(x = cpue_kg_hr, y = cpue_blk))+
 geom_point()+
 xlab('Average Hex CPUE 2012-2019')+
 ylab('Average Hex CPUE 2021-2024')+
 theme_bw()+
 xlim(40, 80)+
 ylim(40, 175)+
 geom_smooth(method = 'lm', formula = y~x, se = T)+
 stat_poly_eq(formula = y~x, aes(label = paste(..rr.label.., p.value.label, sep = "~~~")), 
              parse = TRUE, label.y = 0.95)

setwd(act_ts_figures_folder)
ggsave(filename = paste('TimedSwimSurvey_HexCPUE_Actaeons_12-19vs21-24', '.pdf', sep = ''), 
       plot = df_1, units = 'mm', width = 190, height = 120)
ggsave(filename = paste('TimedSwimSurvey_HexCPUE_Actaeons_12-19vs21-24', '.png', sep = ''), 
       plot = df_1, units = 'mm', width = 190, height = 120)

##---------------------------------------------------------------------------##
## Sites sampled map for QGIS

act_site_rep <- act_ts_dat %>% 
 filter(sampyear >= 2023) %>% 
 group_by(site) %>% 
 summarise(sampled_n = n_distinct(sampdate))

act_site_sampled <- act_ts_dat %>% 
 filter(sampyear >= 2023) %>% 
 left_join(., act_site_rep) %>% 
 filter(sampled_n == 3) %>% 
 distinct(site, .keep_all = T) %>% 
 select(site, actual.geom)

# save spatial layer for QGIS
st_write(act_site_sampled, 
         dsn = paste(spat_layer_folder, '/TimedSwim_Actaeons_2024_sites_sampled_', Sys.Date(), '.gpkg', sep = ''),
         layer = "TimedSwim_Actaeons_2024_sites_sampled.gpkg", driver = "GPKG", overwrite = T, delete_dsn = T)
