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
samp.year.folder <- file.path(paste(sprintf('C:/Users/%s/Dropbox (UTAS Research)', 
                                            Sys.info()[["user"]])), paste('FIS_TimedSwimSurveys', samp.year, sep = ''))

# identify associated sampling year folder path to save plots
ts.plots.folder <- file.path(paste(sprintf('C:/Users/%s/Dropbox (UTAS Research)', 
                                           Sys.info()[["user"]])), paste('FIS_TimedSwimSurveys', samp.year, '_Plots', sep = ''))

##---------------------------------------------------------------------------##
# 3. Load data ####

# clear list
rm(list = setdiff(ls(), c('samp.year', 'samp.year.folder', 'ts.plots.folder')))

# Import final dataframes 
time.swim.dat.final <-
 readRDS(paste(samp.year.folder, '/time.swim.dat.final.RDS', sep = ''))

time.swim.dat.df.final <-
 readRDS(paste(samp.year.folder, '/time.swim.dat.df.final.RDS', sep = ''))

# Import metadata frame
time.swim.meta.dat.final <- readRDS(paste(samp.year.folder, '/time.swim.meta.dat.final.RDS', sep = ''))

##---------------------------------------------------------------------------##
# Filter Block 13 data and identify sample period

df_1 <- time.swim.dat.final %>% 
 filter(blockno == 13) %>% 
 mutate(samp_period = ifelse(between(sampdate, as.Date('2021-01-01'), as.Date('2021-03-31'))|
                             between(sampdate, as.Date('2023-01-01'), as.Date('2023-03-31')), 'pre', 
                             ifelse(between(sampdate, as.Date('2023-04-01'), as.Date('2023-12-31')), 'mid', 'post')))


df_1 <- df_1 %>% 
 mutate(sizeclass_actaeons = ifelse(sizeclass %in% c("0-20", "20-40", "40-60", "60-80", "80-100", "100-120"), '0-120 mm',
                                   ifelse(sizeclass %in% c("120-140"), '120-140 mm', '>140 mm')))

df_2 <- df_1 %>% 
 filter(sampyear == 2023) %>% 
 group_by(blockno, site, sampyear, sampdate, samp_period, sizeclass_actaeons) %>% 
 summarise(ab_n = sum(sizeclass_freq_10))

plot_size_class <- '>140 mm'

plot_pre <- df_2 %>% 
 filter(samp_period == 'pre', legal.size == plot_size_class) %>%  
 ggplot(aes(x = reorder(site, -ab_n), y = ab_n))+
 geom_bar(stat = 'identity')+
 ylim(0, 350)+
 xlab('Site')+
 ylab(bquote('Total count (abalone.10'*~min^-1*')'))+
 ggtitle(paste('Pre-season ', plot_size_class))+
 theme_bw()+
 theme(axis.text.x = element_text(angle = 90, vjust = 0.5))  

plot_mid <- df_2 %>% 
 filter(samp_period == 'mid', legal.size == plot_size_class) %>%  
 ggplot(aes(x = reorder(site, -ab_n), y = ab_n))+
 geom_bar(stat = 'identity')+
 ylim(0, 350)+
 xlab('Site')+
 ylab(bquote('Total count (abalone.10'*~min^-1*')'))+
 ggtitle(paste('Mid-season ', plot_size_class))+
 theme_bw()+
 theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

samp_period_plot <- grid.arrange(plot_pre, plot_mid, nrow = 1)   

# Determine mean abalone abundance in each sample period and size class
ten_min_mean_period <- df_1 %>% 
 filter(sampyear == 2023,
        !is.na(sizeclass_freq_10)) %>%
 group_by(samp_period, sizeclass_actaeons, site, diver) %>% 
 summarise(ab.n = sum(sizeclass_freq_10)) %>% 
 group_by(samp_period, sizeclass_actaeons) %>% 
 summarise(mean_ab_n = mean(ab.n),
           median_ab_n = median(ab.n))

# Determine number of sites surveyed in each block, year and size class
time_swim_site_n <- df_1 %>% 
 filter(sampyear == 2023) %>%
 group_by(samp_period) %>% 
 summarise(n = n_distinct(site))

sub_legal_plot <- df_1 %>% 
 filter(sizeclass_actaeons == '0-120 mm' &
         sampyear == 2023) %>% 
 group_by(samp_period, site, diver) %>% 
 summarise(ab_n = sum(sizeclass_freq_10)) %>% 
 group_by(samp_period, site) %>% 
 summarise(mean_ab_n = mean(ab_n)) %>% 
 mutate(samp_period = factor(samp_period, levels = c('pre', 'mid', 'post'))) %>%     
 ggplot(aes(x = samp_period, y = mean_ab_n))+
 geom_boxplot(aes(fill = samp_period), position = position_dodge2(1, preserve = 'single'),
              outlier.colour = '#EE8866') +
 scale_fill_manual(values = c("#77AADD", "#BBCC33", "#DDDDDD"))+
 geom_point(data = ten_min_mean_period %>% filter(sizeclass_actaeons == '0-120 mm'), aes(group = factor(samp_period, levels = c('pre', 'mid', 'post'))), shape = 19,
            size = 2, colour = 'red', fill = 'red', position = position_dodge2(0.8, preserve = 'single'))+
 theme_bw()+
 ylab(bquote('Average count (abalone.10'*~min^-1*')'))+
 xlab('Sample Period')+
 coord_cartesian(ylim = c(0, 250))+
 geom_text(data = time_swim_site_n, aes(y = 250, label = n, colour = factor(samp_period, levels = c('pre', 'mid', 'post'))), size = 3,
           position = position_dodge2(0.8))+
 scale_colour_manual(values = c("#77AADD", "#BBCC33", "#DDDDDD"))+
 ggtitle('Sub-legal 0-120 mm')+
 theme(plot.title = element_text(vjust = 0, hjust = 0))+
 theme(legend.position = 'none')

sub_legal_plot_2 <- df_1 %>% 
 filter(sizeclass_actaeons == '120-140 mm' &
         sampyear == 2023) %>% 
 group_by(samp_period, site, diver) %>% 
 summarise(ab_n = sum(sizeclass_freq_10)) %>% 
 group_by(samp_period, site) %>% 
 summarise(mean_ab_n = mean(ab_n)) %>% 
 mutate(samp_period = factor(samp_period, levels = c('pre', 'mid', 'post'))) %>%     
 ggplot(aes(x = samp_period, y = mean_ab_n))+
 geom_boxplot(aes(fill = samp_period), position = position_dodge2(1, preserve = 'single'),
              outlier.colour = '#EE8866') +
 scale_fill_manual(values = c("#77AADD", "#BBCC33", "#DDDDDD"))+
 geom_point(data = ten_min_mean_period %>% filter(sizeclass_actaeons == '120-140 mm'), aes(group = factor(samp_period, levels = c('pre', 'mid', 'post'))), shape = 19,
            size = 2, colour = 'red', fill = 'red', position = position_dodge2(0.8, preserve = 'single'))+
 theme_bw()+
 ylab(bquote('Average count (abalone.10'*~min^-1*')'))+
 xlab('Sample Period')+
 coord_cartesian(ylim = c(0, 250))+
 geom_text(data = time_swim_site_n, aes(y = 250, label = n, colour = factor(samp_period, levels = c('pre', 'mid', 'post'))), size = 3,
           position = position_dodge2(0.8))+
 scale_colour_manual(values = c("#77AADD", "#BBCC33", "#DDDDDD"))+
 ggtitle('Sub-legal 120-140 mm')+
 theme(plot.title = element_text(vjust = 0, hjust = 0))+
 theme(legend.position = 'none')

legal_plot <- df_1 %>% 
 filter(sizeclass_actaeons == '>140 mm' &
         sampyear == 2023) %>% 
 group_by(samp_period, site, diver) %>% 
 summarise(ab_n = sum(sizeclass_freq_10)) %>% 
 group_by(samp_period, site) %>% 
 summarise(mean_ab_n = mean(ab_n)) %>% 
 mutate(samp_period = factor(samp_period, levels = c('pre', 'mid', 'post'))) %>%     
 ggplot(aes(x = samp_period, y = mean_ab_n))+
 geom_boxplot(aes(fill = samp_period), position = position_dodge2(1, preserve = 'single'),
              outlier.colour = '#EE8866') +
 scale_fill_manual(values = c("#77AADD", "#BBCC33", "#DDDDDD"))+
 geom_point(data = ten_min_mean_period %>% filter(sizeclass_actaeons == '>140 mm'), aes(group = factor(samp_period, levels = c('pre', 'mid', 'post'))), shape = 19,
            size = 2, colour = 'red', fill = 'red', position = position_dodge2(0.8, preserve = 'single'))+
 theme_bw()+
 ylab(bquote('Average count (abalone.10'*~min^-1*')'))+
 xlab('Sample Period')+
 coord_cartesian(ylim = c(0, 250))+
 geom_text(data = time_swim_site_n, aes(y = 250, label = n, colour = factor(samp_period, levels = c('pre', 'mid', 'post'))), size = 3,
           position = position_dodge2(0.8))+
 scale_colour_manual(values = c("#77AADD", "#BBCC33", "#DDDDDD"))+
 ggtitle('Legal >140 mm')+
 theme(plot.title = element_text(vjust = 0, hjust = 0))+
 theme(legend.position = 'none')

# Join plots
count_plot_samp_period <- grid.arrange(sub_legal_plot, sub_legal_plot_2, legal_plot, nrow = 1)  


df_1 %>% 
 filter(sampyear == 2023) %>% 
 group_by(samp_period, site, legal.size, diver) %>% 
 summarise(ab_n = sum(sizeclass_freq_10)) %>% 
 group_by(samp_period,legal.size, site) %>% 
 summarise(mean_ab_n = mean(ab_n)) %>% 
 mutate(samp_period = factor(samp_period, levels = c('pre', 'mid', 'post'))) %>% 
 spread(samp_period, mean_ab_n) %>% 
 ggplot(aes(x = pre, y = mid, colour = legal.size))+
 geom_point(size = 2, shape = 19)+
 geom_smooth(method = 'lm', formula = y~x, se = F)+
 stat_poly_eq(formula = y~x, aes(label = paste(after_stat(rr.label), p.value.label, sep = "~~~")),
              parse = TRUE) +
 theme_bw()+
 guides(colour = guide_legend(title = "Size Class"))
                                    
                                    
                                    
                                   







