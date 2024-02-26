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
# samp.year.folder <- file.path('C:', 'CloudStor', 'DiveFisheries', 
#                               'Abalone', 'FISdata',
#                               paste('FIS_TimedSwimSurveys', samp.year, sep = ''))

samp.year.folder <- file.path(paste(sprintf('C:/Users/%s/Dropbox (UTAS Research)/DiveFisheries/Abalone/FISdata', 
                                            Sys.info()[["user"]])), paste('FIS_TimedSwimSurveys', samp.year, sep = ''))

# identify associated sampling year folder path to save plots
# ts.plots.folder <- file.path('C:', 'CloudStor', 'DiveFisheries', 
#                              'Abalone', 'Assessment', 'Figures', 'FIS',
#                              paste('FIS_TimedSwimSurvey', samp.year, '_Plots', sep = ''))

ts.plots.folder <- file.path(paste(sprintf('C:/Users/%s/Dropbox (UTAS Research)/DiveFisheries/Abalone/Assessment/Figures/FIS', 
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
## Standardise plot data #

# # Standardise counts for 10 minute swim (i.e. some swims marginally shorter or longer duration)
# time.swim.dat.final <- time.swim.dat.final %>% 
#  mutate(sizeclass_freq_10 = round((sizeclass_freq / time.elapsed) * 10))
# 
# saveRDS(time.swim.dat.final, paste(samp.year.folder, '/time.swim.dat.final.RDS', sep = ''))

# 4. Summarise count data ####

# Summarise total count for blockno x site x sampyear x legal.size
ts.count.sum <- time.swim.dat.final %>% 
 filter(!subblockno %in% c('28B', '28C')) %>%
 group_by(blockno, site, sampyear, legal.size) %>% 
 summarise(ab.n = sum(sizeclass_freq_10)) %>%  
 group_by(blockno, site, sampyear, legal.size) %>% 
 group_by(site)

ts.av.count <- time.swim.dat.final %>% 
 filter(!subblockno %in% c('28B', '28C')) %>%
 group_by(blockno, site, diver, sampyear, legal.size) %>% 
 summarise(ab.n = sum(sizeclass_freq_10)) %>%  
 group_by(blockno, sampyear, legal.size) %>%
 summarise(av.count = mean(ab.n),
           sites = n_distinct(site)) %>% 
 pivot_wider(id_cols = c(blockno),
             names_from = c(legal.size, sampyear),
             values_from = c('av.count', 'sites'))
##---------------------------------------------------------------------------##
time.swim.dat.final %>% 
 filter(!subblockno %in% c('28B', '28C')) %>%
 group_by(blockno, sampyear) %>% 
 summarise(n = n_distinct(sampdate),
           sites = n_distinct(site)) %>% 
 pivot_wider(id_cols = c(blockno),
             names_from = c(sampyear),
             values_from = c('n', 'sites')) %>% 
 adorn_totals()

time.swim.dat.final %>% 
 filter(blockno == 13 &
         sampdate >= as.Date('2023-04-01')) %>% 
 summarise(n = n_distinct(sampdate),
           sites = n_distinct(site))

##---------------------------------------------------------------------------##
## TAB 1: Site Abundance % Change ####

# Summary table of sites surveyed in sample year and percentage change in abundance
# of legal and sub-legal abalone between sample year and previous year.

# Determine mean abalone abundance for block x sampyear x size class
ten.min.mean.year <- time.swim.dat.final %>% 
 filter(!subblockno %in% c('28B', '28C') & 
         !blockno %in% c('13', '14', '29', '30') &
         !is.na(sizeclass_freq_10)) %>%
 group_by(blockno, site, diver, sampyear, time.elapsed, legal.size) %>% 
 summarise(ab.n = sum(sizeclass_freq_10)) %>% 
 group_by(blockno, sampyear, legal.size) %>% 
 summarise(mean.ab.n = mean(ab.n),
           median.ab.n = median(ab.n))

# Determine percentage change in abundance between current and previous year
perc_change <- ten.min.mean.year %>% 
 select(-mean.ab.n) %>% 
 spread(sampyear, median.ab.n) %>%  
 dplyr::rename(FY2020 = '2020',
               FY2021 = '2021',
               FY2022 = '2022',
               FY2023 = '2023') %>%  
 mutate(perc.change = round((1 - (FY2022 / FY2023)), 3) * 100) %>% 
 spread(legal.size, perc.change) %>% 
 select(-c('FY2020', 'FY2021', 'FY2022', 'FY2023')) %>%
 mutate(`>140 mm2` = ifelse(is.na(`>140 mm`), lag(`>140 mm`), `>140 mm`)) %>%
 select(-`>140 mm`) %>%
 filter(!is.na(`<140 mm`)) %>%
 dplyr::rename(`>140 mm` = `>140 mm2`,
               'BlockNo' = 'blockno')

# Determine number of sites surveyed in sample year
sites_year <- time.swim.dat.final %>% 
 filter(!subblockno %in% c('28B', '28C') & 
         !blockno %in% c('13', '14', '29', '30') &
         !is.na(sizeclass_freq_10) &
         sampyear == samp.year) %>%
 group_by(blockno) %>% 
 summarise(Sites = n_distinct(site))

# Join percentage changes and number of sites surveyed dataframes
year_tab <- left_join(sites_year, perc_change, by = c('blockno' = 'BlockNo')) %>%
 dplyr::rename('BlockNo' = 'blockno')

# Create formatted summary table
year_tab_format <- year_tab %>% 
 ggpubr::ggtexttable(rows = NULL, theme = ggpubr::ttheme('mOrange'))

# Save summary tables
setwd(ts.plots.folder)
write.xlsx(year_tab, paste('TimedSwimSurvey_', samp.year-1, 'vs', samp.year, '_PercentChange.xlsx'), sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)
ggsave(filename = paste('TimedSwimSurvey_',  samp.year-1, 'vs', samp.year, '_PercentChange', '.pdf', sep = ''), 
       plot = year_tab_format, units = 'mm', width = 190, height = 120)
ggsave(filename = paste('TimedSwimSurvey_',  samp.year-1, 'vs', samp.year, '_PercentChange', '.png', sep = ''), 
       plot = year_tab_format, units = 'mm', width = 190, height = 120)

rm('perc_change', 'sites_year', 'year_tab', 'year_tab_format')
##---------------------------------------------------------------------------##
## TAB 2: Repeat Site % Change ####

# Summary table of sites surveyed in sample year and percentage change in abundance
# of legal and sub-legal abalone between sample year and previous year.

# Determine mean abalone abundance for block x sampyear x size class
ten.min.mean.year <- time.swim.dat.final %>% 
 filter(!subblockno %in% c('28B', '28C') & 
         !blockno %in% c('13', '14', '21', '29', '30') &
         !is.na(sizeclass_freq_10) &
         ref_site == 1) %>%
 group_by(blockno, site, diver, sampyear, time.elapsed, legal.size) %>% 
 summarise(ab.n = sum(sizeclass_freq_10)) %>% 
 group_by(blockno, sampyear, legal.size) %>% 
 summarise(mean.ab.n = mean(ab.n),
           median.ab.n = median(ab.n))

# Determine percentage change in abundance between current and previous year
perc_change <- ten.min.mean.year %>% 
 select(-mean.ab.n) %>% 
 spread(sampyear, median.ab.n) %>%  
 dplyr::rename(FY2020 = '2020',
               FY2021 = '2021',
               FY2022 = '2022',
               FY2023 = '2023') %>%  
 mutate(perc.change = round((1 - (FY2022 / FY2023)), 3) * 100) %>% 
 spread(legal.size, perc.change) %>% 
 select(-c('FY2020', 'FY2021', 'FY2022', 'FY2023')) %>%
 mutate(`>140 mm2` = ifelse(is.na(`>140 mm`), lag(`>140 mm`), `>140 mm`)) %>%
 select(-`>140 mm`) %>%
 filter(!is.na(`<140 mm`)) %>%
 dplyr::rename(`>140 mm` = `>140 mm2`,
               'BlockNo' = 'blockno')

# Determine number of sites surveyed in sample year
sites_year <- time.swim.dat.final %>% 
 filter(!subblockno %in% c('28B', '28C') & 
         !blockno %in% c('13', '14', '29', '30') &
         !is.na(sizeclass_freq_10) &
         sampyear == samp.year &
         ref_site == 1) %>%
 group_by(blockno) %>% 
 summarise(Sites = n_distinct(site))

# Join percentage changes and number of sites surveyed dataframes
year_tab <- left_join(sites_year, perc_change, by = c('blockno' = 'BlockNo')) %>%
 dplyr::rename('BlockNo' = 'blockno')

# Create formatted summary table
year_tab_format <- year_tab %>% 
 ggpubr::ggtexttable(rows = NULL, theme = ggpubr::ttheme('mOrange'))

# Save summary tables
setwd(ts.plots.folder)
write.xlsx(year_tab, paste('TimedSwimSurvey_', samp.year-1, 'vs', samp.year, '_PercentChange_RepeatSites.xlsx'), sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)
ggsave(filename = paste('TimedSwimSurvey_',  samp.year-1, 'vs', samp.year, '_PercentChange_RepeatSites', '.pdf', sep = ''), 
       plot = year_tab_format, units = 'mm', width = 190, height = 120)
ggsave(filename = paste('TimedSwimSurvey_',  samp.year-1, 'vs', samp.year, '_PercentChange_RepeatSites', '.png', sep = ''), 
       plot = year_tab_format, units = 'mm', width = 190, height = 120)

rm('perc_change', 'sites_year', 'year_tab', 'year_tab_format')
##---------------------------------------------------------------------------##
## TAB 2: Site Abundance ####

## summary table of counts and CPUE by size class and block 
## (NOTE: run script for CPUE above)

# Arrange size classes in order
sizeclasses <- c("0-20", "20-40", "40-60", "60-80", "80-100", "100-120", "120-140", "140-160", "160-180", "180-200", "200-220")

# Create summary table
time.swim.count.blockno <- time.swim.dat.final %>% 
 filter(!subblockno %in% c('28B', '28C'),
        sampyear == samp.year & 
         !blockno %in% c('13', '14', '29', '30')) %>% 
 group_by(blockno, site, diver, sampyear, legal.size, time.elapsed) %>% 
 summarise(ab.n = sum(sizeclass_freq)) %>% 
 group_by(blockno, legal.size) %>%
 summarise(sites = n_distinct(site),
           ab.min = round(mean(ab.n), digits = 1)) %>% 
 spread(legal.size, ab.min) %>%
 as.data.frame() %>% 
 dplyr::rename('Blockno' = blockno,
               'Sites' = sites,
               'Average\ncount\n<140mm' = '<140 mm',
               'Average\ncount\n>140mm' = '>140 mm') %>% 
 adorn_totals(fill = '',,,, contains('Sites'))

# Create formatted summary table
time.swim.summary.tab <- time.swim.count.blockno %>% 
 ggpubr::ggtexttable(rows = NULL, theme = ggpubr::ttheme('mOrange'))

# Save summary tables
setwd(ts.plots.folder)
write.xlsx(time.swim.count.blockno, paste('TimedSwimSurvey_', samp.year, '_SummaryTable.xlsx'), sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)
ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_SummaryTable', '.pdf', sep = ''), 
       plot = time.swim.summary.tab, units = 'mm', width = 190, height = 120)
ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_SummaryTable', '.png', sep = ''), 
       plot = time.swim.summary.tab, units = 'mm', width = 190, height = 120)
##---------------------------------------------------------------------------##
# TAB 3: Sites Completed ####

ts.tab <- time.swim.dat.final %>% 
 filter(!subblockno %in% c('28B', '28C'),
        sampyear == samp.year & 
         !blockno %in% c('13', '14', '29', '30')) %>% 
 group_by(sampyear, blockno) %>%
 summarise(sites = n_distinct(site),
           field.days = n_distinct(sampdate),
           site.day = round(sites / field.days, digits = 1)) %>% 
 as.data.frame() %>% 
 dplyr::rename('Blockno' = blockno,
               'Sites' = sites,
               'Days' = field.days,
               'Sites.Day' = site.day) %>%
 add_row(Blockno = 'Total', Sites = sum(.$Sites), Days = sum(.$Days), 
         Sites.Day = round(mean(.$Sites.Day), digits = 1)) %>% 
 select(-sampyear)

# Create formatted summary table
ts.tab.format <- ts.tab %>% 
 ggpubr::ggtexttable(rows = NULL, theme = ggpubr::ttheme('mOrange'))

# Save summary tables
setwd(ts.plots.folder)
write.xlsx(ts.tab, paste('TimedSwimSurvey_', samp.year, '_SiteSummaryTable.xlsx'), sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)
ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_SiteSummaryTable', '.pdf', sep = ''), 
       plot = ts.tab.format, units = 'mm', width = 190, height = 120)
ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_SiteSummaryTable', '.png', sep = ''), 
       plot = ts.tab.format, units = 'mm', width = 190, height = 120)
##---------------------------------------------------------------------------##
# Colour palette for plots

col_light <- c('#77AADD', '#99DDFF', '#44BB99', '#BBCC33', 
               '#AAAA00', '#EEDD88', '#EE8866', '#FFAABB', 
               '#DDDDDD')

##---------------------------------------------------------------------------##

## PLOT 1: Abundance Years ####

# Average count of all legal and sub-legal abalone per 10 min by year for each 
# site within each block (i.e. the average count between paired divers for each site).

# Determine mean abalone abundance in each block, year and size class
ten.min.mean.year <- time.swim.dat.final %>% 
 filter(!subblockno %in% c('28B', '28C') & 
         !blockno %in% c('13', '14', '29', '30') &
         !is.na(sizeclass_freq_10)) %>%
 group_by(blockno, site, diver, sampyear, time.elapsed, legal.size) %>% 
 summarise(ab.n = sum(sizeclass_freq_10)) %>% 
 group_by(blockno, sampyear, legal.size) %>% 
 summarise(mean.ab.n = mean(ab.n),
           median.ab.n = median(ab.n)) 
# mutate(sampyear = factor(sampyear))

# Determine number of sites surveyed in each block, year and size class
time.swim.dat.n <- time.swim.dat.final %>% 
 filter(!subblockno %in% c('28B', '28C') & 
         !blockno %in% c('13', '14', '29', '30')) %>%
 group_by(sampyear, blockno, legal.size) %>% 
 summarise(n = n_distinct(site))

# Plot for sub-legal abundance
sub.legal.plot <- time.swim.dat.final %>% 
 filter(!subblockno %in% c('28B', '28C'),
        legal.size == '<140 mm' &
         !blockno %in% c('13', '14', '29', '30')) %>%
 # filter(midsize < 150) %>% 
 group_by(blockno, site, diver, sampyear) %>% 
 summarise(ab.n = sum(sizeclass_freq_10)) %>% 
 group_by(blockno, site, sampyear) %>% 
 summarise(mean.ab.n = mean(ab.n)) %>% 
 mutate(sampyear = factor(sampyear, levels = c('2020', '2021', '2022', '2023'))) %>%  
 ggplot(aes(x = blockno, y = mean.ab.n))+
 geom_boxplot(aes(fill = sampyear), position = position_dodge2(1, preserve = 'single'),
              outlier.colour = '#EE8866') +
 scale_fill_manual(values = c("#77AADD", "#BBCC33", "#DDDDDD", '#44BB99'))+
 geom_point(data = ten.min.mean.year %>% filter(legal.size == '<140 mm'), aes(group = factor(sampyear, levels = c('2020', '2021', '2022'))), shape = 19,
            size = 2, colour = 'red', fill = 'red', position = position_dodge2(0.8, preserve = 'single'))+
 theme_bw()+
 ylab(bquote('Average count (abalone.10'*~min^-1*')'))+
 xlab('Blockno')+
 coord_cartesian(ylim = c(0, 150))+
 geom_text(data = time.swim.dat.n %>% filter(legal.size == '<140 mm'), aes(y = 150, label = n, colour = factor(sampyear, levels = c('2020', '2021', '2022', '2023'))), size = 3, 
           position = position_dodge2(0.8))+
 scale_colour_manual(values = c("#77AADD", "#BBCC33", "#DDDDDD", '#44BB99'))+
 guides(size = 'legend', colour = 'none',
        fill = guide_legend(title = 'Year'))+
 ggtitle('Sub-legal <140 mm')+
 theme(plot.title = element_text(vjust = 0, hjust = 0))+
 theme(legend.position = 'none')

# Plot for legal abundance
legal.plot <- time.swim.dat.final %>% 
 filter(!subblockno %in% c('28B', '28C'),
        legal.size == '>140 mm' &
         !blockno %in% c('13', '14', '29', '30')) %>%
 # filter(midsize < 150) %>% 
 group_by(blockno, site, diver, sampyear) %>% 
 summarise(ab.n = sum(sizeclass_freq_10)) %>% 
 group_by(blockno, site, sampyear) %>% 
 summarise(mean.ab.n = mean(ab.n)) %>% 
 mutate(sampyear = factor(sampyear, levels = c('2020', '2021', '2022', '2023'))) %>%  
 ggplot(aes(x = blockno, y = mean.ab.n))+
 geom_boxplot(aes(fill = sampyear), position = position_dodge2(1, preserve = 'single'),
              outlier.colour = '#EE8866') +
 scale_fill_manual(values = c("#77AADD", "#BBCC33", "#DDDDDD", '#44BB99'))+
 geom_point(data = ten.min.mean.year %>% filter(legal.size == '>140 mm'), aes(group = factor(sampyear, levels = c('2020', '2021', '2022', '2023'))), shape = 19,
            size = 2, colour = 'red', fill = 'red', position = position_dodge2(0.8, preserve = 'single'))+
 theme_bw()+
 ylab(bquote('Average count (abalone.10'*~min^-1*')'))+
 xlab('Blockno')+
 coord_cartesian(ylim = c(0, 150))+
 guides(size = 'legend', colour = 'none',
        fill = guide_legend(title = 'Year'))+
 ggtitle('Legal >140 mm')+
 theme(plot.title = element_text(vjust = 0, hjust = 0))+
 theme(legend.title = element_blank(),
       legend.position = c(0.9, 0.7))

# Join plots
count.plot.sizeclass <- grid.arrange(sub.legal.plot, legal.plot, nrow = 2)

# Save plots 
setwd(ts.plots.folder)
ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_TenMinuteCount_LegalSubLegal', '.pdf', sep = ''), 
       plot = count.plot.sizeclass, units = 'mm', width = 190, height = 200)
ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_TenMinuteCount_LegalSubLegal', '.png', sep = ''), 
       plot = count.plot.sizeclass, units = 'mm', width = 190, height = 200)

rm('count.plot.sizeclass', 'legal.plot', 'sub.legal.plot', 'time.swim.dat.n', 
   'ten.min.mean.year')

##---------------------------------------------------------------------------##
## PLOT 2: Repeat Abundance Years ####
# compare counts at repeat sites between 2020 and 2021

count.plot.rep.dat <- time.swim.dat.final %>% 
 filter(!subblockno %in% c('28B', '28C') & 
         !blockno %in% c('13', '14', '21', '29', '30') &
         ref_site == 1) %>%
 group_by(blockno, site, diver, sampyear, legal.size) %>% 
 summarise(ab.n = sum(sizeclass_freq)) %>% 
 group_by(blockno, site, sampyear, legal.size) %>% 
 summarise(mean.ab.n = mean(ab.n)) %>% 
 group_by(site)
 # filter(n() > 2)

ten.min.mean.rep.year.sites <- count.plot.rep.dat %>% 
 group_by(blockno, sampyear, legal.size) %>% 
 summarise(n = n_distinct(site))

count.plot.rep.mean <- time.swim.dat.final %>% 
 filter(!subblockno %in% c('28B', '28C') & 
         !blockno %in% c('13', '14', '21', '29', '30') &
         ref_site == 1) %>%
 group_by(blockno, site, diver, sampyear, legal.size) %>% 
 summarise(ab.n = sum(sizeclass_freq)) %>% 
 group_by(blockno, sampyear, legal.size) %>%
 filter(n() > 1) %>% 
 summarise(mean.ab.n = mean(ab.n))

sub.legal.plot.rep <- count.plot.rep.dat %>% 
 filter(legal.size == '<140 mm') %>%
 mutate(sampyear = factor(sampyear, levels = c('2020', '2021', '2022', '2023'))) %>%  
 ggplot(aes(x = blockno, y = mean.ab.n))+
 geom_boxplot(aes(fill = sampyear), position = position_dodge2(1, preserve = 'single'),
              outlier.colour = '#EE8866') +
 scale_fill_manual(values = c("#77AADD", "#BBCC33", "#DDDDDD", '#44BB99'))+
 geom_point(data = count.plot.rep.mean %>% filter(legal.size == '<140 mm'), 
            aes(group = factor(sampyear, levels = c('2020', '2021', '2022', '2023'))), 
            shape = 19, size = 2, colour = 'red', fill = 'red', position = position_dodge2(0.8))+
 theme_bw()+
 ylab(bquote('Average count (abalone.10'*~min^-1*')'))+
 xlab('Blockno')+
 coord_cartesian(ylim = c(0, 100))+
 geom_text(data = ten.min.mean.rep.year.sites %>% filter(legal.size == '<140 mm'), 
           aes(y = 100, label = n, colour = factor(sampyear, levels = c('2020', '2021', '2022', '2023'))), size = 3, 
           position = position_dodge2(0.8))+
 scale_colour_manual(values = c("#77AADD", "#BBCC33", "#DDDDDD", '#44BB99'))+
 guides(size = 'legend', colour = 'none',
        fill = guide_legend(title = 'Year'))+
 ggtitle('Sub-legal <140 mm Repeat Sites')+
 theme(plot.title = element_text(vjust = 0, hjust = 0))+
 theme(legend.position = 'none')

legal.plot.rep <- count.plot.rep.dat %>% 
 filter(legal.size == '>140 mm') %>%
 mutate(sampyear = factor(sampyear, levels = c('2020', '2021', '2022', '2023'))) %>%  
 ggplot(aes(x = blockno, y = mean.ab.n))+
 geom_boxplot(aes(fill = sampyear), position = position_dodge2(1, preserve = 'single'),
              outlier.colour = '#EE8866') +
 scale_fill_manual(values = c("#77AADD", "#BBCC33", "#DDDDDD", '#44BB99'))+
 geom_point(data = count.plot.rep.mean %>% filter(legal.size == '>140 mm'), 
            aes(group = factor(sampyear, levels = c('2020', '2021', '2022', '2023'))), shape = 19,
            size = 2, colour = 'red', fill = 'red', position = position_dodge2(0.8))+
 theme_bw()+
 ylab(bquote('Average count (abalone.10'*~min^-1*')'))+
 xlab('Blockno')+
 coord_cartesian(ylim = c(0, 100))+
 guides(size = 'legend', colour = 'none',
        fill = guide_legend(title = 'Year'))+
 ggtitle('Legal >140 mm Repeat Sites')+
 theme(plot.title = element_text(vjust = 0, hjust = 0))+
 theme(legend.title = element_blank(),
       legend.position = c(0.9, 0.8))

count.plot.rep.sizeclass <- grid.arrange(sub.legal.plot.rep, legal.plot.rep, nrow = 2)

setwd(ts.plots.folder)
ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_TenMinuteCount_RepeatSites', '.pdf', sep = ''), 
       plot = count.plot.rep.sizeclass, units = 'mm', width = 190, height = 200)
ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_TenMinuteCount_RepeatSites', '.png', sep = ''), 
       plot = count.plot.rep.sizeclass, units = 'mm', width = 190, height = 200)

rm('count.plot.rep.sizeclass', 'legal.plot.rep', 'sub.legal.plot.rep', 'count.plot.rep.mean', 
   'ten.min.mean.rep.year.sites', 'count.plot.rep.dat')
##---------------------------------------------------------------------------##
## PLOT 4: Sizeclass Abundance Years ####
## average count of all legal and sub-legal abalone per 10 min by sizeclass for each site within each blockno 
## (i.e. the average between paired divers for each site)

# determine mean count per 10 min by size class for each blockno
ten.min.mean <- time.swim.dat.final %>% 
 filter(!subblockno %in% c('28B', '28C') & 
         !blockno %in% c('13', '14', '29', '30')) %>% 
 group_by(sampyear, site, blockno, subblockno, diver, legal.size) %>%  
 summarise(ab.n = sum(sizeclass_freq)) %>% 
 group_by(sampyear, blockno, legal.size) %>% 
 summarise(mean.ab.n = mean(ab.n)) %>% 
 mutate(legal.size = factor(legal.size))

ten.min.mean.site <- time.swim.dat.final %>% 
 filter(!subblockno %in% c('28B', '28C') & 
         !blockno %in% c('13', '14', '29', '30')) %>% 
 group_by(site, blockno, subblockno, diver, legal.size) %>% 
 summarise(ab.n = sum(sizeclass_freq)) %>% 
 group_by(site, blockno, subblockno, legal.size) %>% 
 summarise(mean.ab.n = mean(ab.n)) %>% 
 mutate(legal.size = factor(legal.size))%>% 
 as.data.frame()

time.swim.dat.n <- time.swim.dat.final %>% 
 filter(!subblockno %in% c('28B', '28C') & 
         !blockno %in% c('13', '14', '29', '30')) %>%
 group_by(sampyear, blockno, legal.size) %>% 
 summarise(n = n_distinct(site))

count.plot.sizeclass <- time.swim.dat.final %>% 
 filter(!subblockno %in% c('28B', '28C') & 
         !blockno %in% c('13', '14', '29', '30')) %>%
 group_by(blockno, site, diver, legal.size, sampyear) %>% 
 summarise(ab.n = sum(sizeclass_freq)) %>% 
 group_by(blockno, site, legal.size, sampyear) %>% 
 summarise(mean.ab.n = mean(ab.n)) %>% 
 mutate(legal.size = factor(legal.size)) %>% 
 ggplot(aes(x = blockno, y = mean.ab.n))+
 geom_boxplot(aes(fill = legal.size), position = position_dodge(0.9), outlier.colour = '#EE8866')+
 scale_fill_manual(values = c("#999999", "#56B4E9"))+
 # stat_summary(fun.y = mean, geom = 'point', shape = 19, size = 4, colour = 'red', fill = 'red')+
 geom_point(data = ten.min.mean, aes(group = legal.size), shape = 19, size = 2, colour = 'red', fill = 'red', position = position_dodge(0.9))+
 theme_bw()+
 ylab(bquote('Average count (abalone.10'*~min^-1*')'))+
 xlab('Blockno')+
 coord_cartesian(ylim = c(0, 125))+
 geom_text(data = time.swim.dat.n, aes(y = 125, label = n), color = 'black', size = 3)+
 theme(legend.position="none")+
 facet_wrap(. ~ sampyear)

setwd(ts.plots.folder)
ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_TenMinuteCountSizeClassPlot', '.pdf', sep = ''), 
       plot = count.plot.sizeclass, units = 'mm', width = 190, height = 160)
ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_TenMinuteCountSizeClassPlot', '.png', sep = ''), 
       plot = count.plot.sizeclass, units = 'mm', width = 190, height = 160)

rm('count.plot', 'count.plot.sizeclass', 'ten.min.mean', 'ten.min.mean.site',
   'time.swim.dat.n')
##---------------------------------------------------------------------------##
# PLOT 5: Repeat Site Count Year ####
# Compare total counts of size class between years for repeat sites

# Load reference sites
time_swim_ref_sites <- readRDS('C:/CloudStor/R_Stuff/FIS/FIS_2021/FIS_TimedSwimSurveys2021/time.swim.2020.repeat.sites.RDS')

# Extract expected reference site names and block
ref_sites <- time_swim_ref_sites %>% 
 select(site.new, blockno)

# Create vector of sample years and repeat for total expected reference sites (n = 90)
years <- rep(c(2020, 2021, 2022, 2023), 90)

# Create dataframe of expected reference sites for each sample year
ref_sites_year <- ref_sites %>% 
 slice(rep(1:n(), each = 4)) %>% 
 mutate(sampyear = years) %>% 
 dplyr::rename(site = 'site.new')

# Summarise total count for blockno x site x sampyear x legal.size
ts_count_sum_ref <- time.swim.dat.final %>% 
 filter(!subblockno %in% c('28B', '28C') &
         ref_site == 1) %>%
 group_by(blockno, site, sampyear, legal.size) %>% 
 summarise(ab.n = sum(sizeclass_freq_10)) %>%  
 group_by(blockno, site, sampyear, legal.size) %>% 
 group_by(site)

# Join summary to expected reference site dataframe to identify missed reference sites
ts_count_sum_ref_dat <- left_join(ref_sites_year, ts_count_sum_ref) %>% 
 mutate(plot_lab = if_else(is.na(ab.n), 'ns', NA_character_))

# Identify repeat blocks
ts.rep.blocks <- ts_count_sum_ref %>% 
 group_by(blockno) %>% 
 distinct(blockno) %>% 
 pull()

# Generate plot
for (i in ts.rep.blocks){
 
 rep.plot <- ts_count_sum_ref_dat %>% 
  filter(blockno == i) %>% 
  group_by(sampyear, site, legal.size) %>%
  mutate(site = factor(site)) %>% 
  ggplot(aes(reorder_within(site, -ab.n, sampyear, fun = 'sum'),
  y = ab.n, fill = legal.size))+
  geom_bar(stat = 'identity', position = 'stack')+
  geom_text(aes(y = 25, label = plot_lab), size = 2)+
  coord_cartesian(ylim = c(0, 500))+
  xlab('Site')+
  ylab(bquote('Total count (abalone.10'*~min^-1*')'))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  scale_fill_manual(values = c("#999999", "#56B4E9"))+
  scale_x_reordered()+
  facet_wrap(~sampyear, scales = 'free', drop = F)+
  theme(legend.position = 'none')
 
# Save plot
 setwd(ts.plots.folder)
 ggsave(filename = paste('TimedSwimSurvey_RepeatSites_', 'BlockNo', i,
                         '_TotalCount_', min(ts_count_sum_ref_dat$sampyear), '-',
                         max(ts_count_sum_ref_dat$sampyear), '.pdf', sep = ''),
        plot = rep.plot, units = 'mm', width = 190, height = 120)

 ggsave(filename = paste('TimedSwimSurvey_RepeatSites_', 'BlockNo', i,
                         '_TotalCount_', min(ts_count_sum_ref_dat$sampyear), '-',
                         max(ts_count_sum_ref_dat$sampyear), '.png', sep = ''),
        plot = rep.plot, units = 'mm', width = 190, height = 120)
 
}

##---------------------------------------------------------------------------##
## PLOT 6: Count Year Top Ten ####

# Determine and compare top ten sites in each block and year based on total 
# counts of size class

# Identify closed blocks sampled in 2020 only
ts.blocks <- c(16, 22, 23, 24, 27, 28)

# Generate plot
for (i in ts.blocks){
 
 top.ten.plot <- ts.count.sum %>% 
  filter(blockno == i) %>%
  group_by(sampyear, site) %>%
  summarise(ab.n.sum = sum(ab.n)) %>%
  slice_max(order_by = c(ab.n.sum), n = 10, with_ties = F) %>% 
  mutate(top.ten = 1) %>% 
  left_join(., ts.count.sum) %>% 
  ggplot(aes(reorder_within(site, -ab.n, sampyear, fun = 'sum'), 
             y = ab.n, fill = legal.size))+
  geom_bar(stat = 'identity', position = 'stack')+
  coord_cartesian(ylim = c(0, 500))+
  xlab('Site')+
  ylab(bquote('Total count (abalone.10'*~min^-1*')'))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  scale_fill_manual(values = c("#999999", "#56B4E9"))+
  scale_x_reordered()+
  facet_wrap(~sampyear, scales = 'free')+
  theme(legend.position = 'none')
 
 # save plot
 setwd(ts.plots.folder)
 ggsave(filename = paste('TimedSwimSurvey_TopTenSites_', 'BlockNo', i, 
                         '_TotalCount_', min(ts.count.sum$sampyear), '-',
                         max(ts.count.sum$sampyear), '.pdf', sep = ''),
        plot = top.ten.plot, units = 'mm', width = 190, height = 120)
 
 ggsave(filename = paste('TimedSwimSurvey_TopTenSites_', 'BlockNo', i, 
                         '_TotalCount_', min(ts.count.sum$sampyear), '-',
                         max(ts.count.sum$sampyear), '.png', sep = ''),
        plot = top.ten.plot, units = 'mm', width = 190, height = 120)
}

rm(top.ten.plot)

##---------------------------------------------------------------------------##
## PLOT 7: LF Block ####

## Length frequency plot by block (mid-points) for sampling year

# Determine number of abalone recorded and number of sites sampled per block
block.ab.n <- time.swim.dat.df.final %>% 
 filter(!subblockno %in% c('28B', '28C') &
         sampyear == samp.year &
         !blockno %in% c('13', '14', '29', '30')) %>% 
 group_by(blockno) %>% 
 summarise(ab.n = paste('n = ', n()))

block.site.n <- time.swim.dat.final %>% 
 filter(!subblockno %in% c('28B', '28C') &
         sampyear == samp.year &
         !blockno %in% c('13', '14', '29', '30')) %>%
 group_by(blockno) %>% 
 summarise(site.n = paste('(', n_distinct(site), ')', sep = ''))

block.ab.site.n <- left_join(block.ab.n, block.site.n) %>% 
 mutate(n = paste(ab.n, site.n, sep = ' '))

# Create length frequency plot
lf.plot <- time.swim.dat.df.final %>% 
 filter(!subblockno %in% c('28B', '28C')&
         sampyear == samp.year &
         !blockno %in% c('13')) %>%
 ggplot(aes(shelllength, group = blockno)) +
 geom_bar(aes(y = ..prop.., stat = 'count'), width = 20, col = 'black', fill = '#EFC000FF')+
 geom_vline(aes(xintercept = ifelse(blockno %in% c('27', '28'), 145, 138)), 
            linetype = 'dashed', colour = 'red', size = 0.5) +
 theme_bw() +
 facet_grid(blockno ~ .) +
 scale_y_continuous(breaks = seq(0, 0.4, 0.1), labels = seq(0, 40, 10))+
 xlab("Shell Length (mm)")+
 ylab(paste("Percentage (%)"))+
 geom_text(data = block.ab.site.n, aes(x = 180, y = 0.2, label = n), color = 'black', size = 3)

# Create length frequency plot (absolute)
lf_plot_abs <- time.swim.dat.df.final %>% 
 filter(!subblockno %in% c('28B', '28C')&
         sampyear == samp.year &
         !blockno %in% c('13')) %>%
 ggplot(aes(shelllength, group = blockno)) +
 # geom_bar(aes(y = ..prop.., stat = 'count'), width = 20, col = 'black', fill = '#EFC000FF')+
 geom_bar(width = 20, col = 'black', fill = '#EFC000FF')+
 geom_vline(aes(xintercept = ifelse(blockno %in% c('27', '28'), 145, 138)), 
            linetype = 'dashed', colour = 'red', size = 0.5) +
 theme_bw() +
 facet_grid(blockno ~ .) +
 # scale_y_continuous(breaks = seq(0, 0.4, 0.1), labels = seq(0, 40, 10))+
 xlab("Shell Length (mm)")+
 ylab(paste("Count"))+
 geom_text(data = block.ab.site.n, aes(x = 180, y = 1000, label = n), color = 'black', size = 3)

# Save plot
setwd(ts.plots.folder)
ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_SizeFrequencyPlot_Percent', '.pdf', sep = ''),
       plot = lf.plot, units = 'mm', width = 190, height = 250)
ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_SizeFrequencyPlot_Percent', '.png', sep = ''),
       plot = lf.plot, units = 'mm', width = 190, height = 250)

ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_SizeFrequencyPlot_Count', '.pdf', sep = ''),
       plot = lf_plot_abs, units = 'mm', width = 190, height = 250)
ggsave(filename = paste('TimedSwimSurvey_', samp.year, 'SizeFrequencyPlot_Count', '.png', sep = ''),
       plot = lf_plot_abs, units = 'mm', width = 190, height = 250)

rm(lf.plot, block.ab.n, block.ab.site.n, block.site.n)
##---------------------------------------------------------------------------##
## PLOT 8: LF Years ####

## Overlaid length frequency plot by block and year (size-classes)

# Determine number of abalone recorded and number of sites sampled per block
# and create plot labels
block.ab.n <- time.swim.dat.df.final %>% 
 filter(!subblockno %in% c('28B', '28C'),
        # sampdate > as.Date('2021-01-01'),
        !blockno %in% c(13, 14, 29, 30)) %>% 
 group_by(sampyear, blockno) %>% 
 summarise(ab.n = paste('n = ', n()))

block.site.n <- time.swim.dat.final %>% 
 filter(!subblockno %in% c('28B', '28C'),
        # sampdate > as.Date('2021-01-01'),
        !blockno %in% c(13, 14, 29, 30)) %>%
 group_by(sampyear, blockno) %>% 
 summarise(site.n = paste('(', n_distinct(site), ')', sep = ''))

block.ab.site.n <- left_join(block.ab.n, block.site.n) %>% 
 mutate(n = paste(ab.n, site.n, sep = ' '))

# Determine length frequency proportions for sampling years and combine
lf.df <- time.swim.dat.final %>%
 filter(!subblockno %in% c('28B', '28C'),
        # sampdate > as.Date('2021-01-01'),
        !blockno %in% c(13, 14, 29, 30)) %>%
 group_by(sampyear, blockno, sizeclass.2021) %>% 
 summarise(n = sum(sizeclass_freq)) %>% 
 mutate(freq = n / sum(n))

lf.plot <- ggplot()+
 geom_line(data = lf.df, aes(x = sizeclass.2021, y = freq*100, 
                             group = factor(sampyear), 
                             colour = factor(sampyear)),
           stat = 'identity', position = position_dodge2(0.1),
           size = 1)+
 geom_vline(data = lf.df, aes(xintercept = ifelse(blockno %in% c('27', '28'), 3.8, 3.5)),
            linetype = 'dashed', colour = 'red', size = 0.5) +
 theme_bw()+
 facet_grid(blockno ~ .)+
 scale_y_continuous(breaks = seq(0, 50, 10), labels = seq(0, 50, 10))+
 xlab("Shell Length (mm)")+
 ylab(paste("Percentage (%)"))+
 geom_text(data = block.ab.site.n, aes(x = 7, y = 10, label = n, colour = factor(sampyear, levels = c('2020', '2021', '2022', '2023'))), size = 3, 
           position = position_stack(vjust = 0.8), show.legend = F)+
 scale_colour_manual(values = c("#77AADD", "#BBCC33", "#EE8866", '#44BB99'))+
 guides(size = 'legend', 
        colour = guide_legend(title = 'Year'))

lf_plot_abs <- ggplot()+
 geom_line(data = lf.df, aes(x = sizeclass.2021, y = n, 
                             group = factor(sampyear), 
                             colour = factor(sampyear)),
           stat = 'identity', position = position_dodge2(0.1),
           size = 1)+
 geom_vline(data = lf.df, aes(xintercept = ifelse(blockno %in% c('27', '28'), 3.8, 3.5)),
            linetype = 'dashed', colour = 'red', size = 0.5) +
 theme_bw()+
 facet_grid(blockno ~ .)+
 # scale_y_continuous(breaks = seq(0, 50, 10), labels = seq(0, 50, 10))+
 xlab("Shell Length (mm)")+
 ylab(paste("Count"))+
 geom_text(data = block.ab.site.n, aes(x = 7, y = 500, label = n, colour = factor(sampyear, levels = c('2020', '2021', '2022', '2023'))), size = 3, 
           position = position_stack(vjust = 0.8), show.legend = F)+
 scale_colour_manual(values = c("#77AADD", "#BBCC33", "#EE8866", '#44BB99'))+
 guides(size = 'legend', 
        colour = guide_legend(title = 'Year'))

setwd(ts.plots.folder)
ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_SizeFrequencyPlot_BLOCKS16-28', '.pdf', sep = ''),
       plot = lf.plot, units = 'mm', width = 190, height = 250)
ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_SizeFrequencyPlot_BLOCKS16-28', '.png', sep = ''),
       plot = lf.plot, units = 'mm', width = 190, height = 250)
ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_SizeFrequencyPlot_BLOCKS16-28_Count', '.pdf', sep = ''),
       plot = lf_plot_abs, units = 'mm', width = 190, height = 250)
ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_SizeFrequencyPlot_BLOCKS16-28_Count', '.png', sep = ''),
       plot = lf_plot_abs, units = 'mm', width = 190, height = 250)

##---------------------------------------------------------------------------##
# Boxplot of legal abalone to compare with historic market measure pre 2019

df_1 <- time.swim.dat.df.final %>%
 filter(!subblockno %in% c('28B', '28C'),
        !blockno %in% c(13, 14, 29, 30))

df_1 %>% 
 mutate(sampyear = as.factor(sampyear)) %>% 
 ggplot(aes(x = sampyear, y = shelllength.2021)) +
 geom_boxplot(outlier.colour = "orange", outlier.size = 1.5)+
 ylim(0, 250)+
 facet_wrap(blockno ~ .)

##---------------------------------------------------------------------------##
# Compare size structure with equivalent size classes from market measure data

# load most recent commercial catch sampling compiled MM dataframe
compiledMM.df.final <- readRDS('C:/CloudStor/R_Stuff/MMLF/compiledMM.df.final.RDS')

# select length-weight data, removing obvious erroneous data or data collected from multiple blocks in a trip
length_dat <- compiledMM.df.final %>% 
 filter(between(shell.length, sizelimit - 5, 220) &
         numblocks == 1 &
         species == 1 &
         blocklist %in% c(16, 22, 23, 24, 27, 28)) %>% 
 mutate(ts_sizeclass = ifelse(between(shell.length, 140, 159), '140-160',
                              ifelse(between(shell.length, 160, 179), '160-180',
                                     ifelse(between(shell.length, 180, 199), '180-200',
                                            ifelse(between(shell.length, 200, 219), '200-220',
                                                   ifelse(shell.length >= 200, '>200', '<140'))))))

df_1 <- length_dat %>% 
 filter(ts_sizeclass != '<140' &
         fishyear <= 2019) %>% 
 group_by(fishyear, blockno, ts_sizeclass) %>% 
 summarise(n = n()) %>% 
 mutate(freq = n / sum(n))

df_2 <- time.swim.dat.final %>%
 filter(!subblockno %in% c('28B', '28C'),
        !blockno %in% c(13, 14, 29, 30),
        sizeclass.2021 %in% c('140-160', '160-180', '180-200', '200-220')) %>%
 group_by(sampyear, blockno, sizeclass.2021) %>% 
 summarise(n = sum(sizeclass_freq)) %>% 
 mutate(freq = n / sum(n)) %>% 
 dplyr::rename('ts_sizeclass' = sizeclass.2021,
               'fishyear' = sampyear)

df_3 <- bind_rows(df_1, df_2)

df_3 %>% 
 ggplot(aes(fill = ts_sizeclass, y = n, x = as.factor(fishyear))) + 
 geom_bar(position = position_fill(reverse = TRUE), stat = "identity")+
 facet_grid(blockno ~ .)



##---------------------------------------------------------------------------##
## PLOT 9: LF Repeat Year ####

ref.sites <- time.swim.dat.final %>%
 filter(!subblockno %in% c('28B', '28C'),
        !blockno %in% c(13, 14, 29, 30) &
         ref_site == 1) %>%
 group_by(sampyear, blockno, site, sizeclass.2021) %>% 
 summarise(n = sum(sizeclass_freq)) %>% 
 group_by(site) %>% 
 summarise(n = n())


ref.site.dat <- time.swim.dat.final %>%
 filter(!subblockno %in% c('28B', '28C'),
        !blockno %in% c(13, 14, 29, 30) &
         ref_site == 1) %>%
 group_by(sampyear, blockno, sizeclass.2021) %>% 
 summarise(n = sum(sizeclass_freq)) %>% 
 mutate(freq = n / sum(n)) 


lf.plot.2 <- ggplot()+
 geom_line(data = ref.site.dat, aes(x = sizeclass.2021, y = freq*100, 
                                    group = factor(sampyear), 
                                    colour = factor(sampyear)),
           stat = 'identity', position = position_dodge2(0.1),
           size = 1)+
 theme_bw()+
 facet_grid(blockno ~ .)+
 scale_y_continuous(breaks = seq(0, 50, 10), labels = seq(0, 50, 10))+
 geom_vline(data = ref.site.dat, aes(xintercept = ifelse(blockno %in% c('27', '28'), 3.8, 3.5)),
            linetype = 'dashed', colour = 'red', size = 0.5) +
 xlab("Shell Length (mm)")+
 ylab(paste("Percentage (%)"))+
 scale_colour_manual(values = c("#77AADD", "#BBCC33", "#EE8866", '#44BB99'))+
 guides(size = 'legend', 
        colour = guide_legend(title = 'Year'))

setwd(ts.plots.folder)
ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_SizeFrequencyPlot_Repeat_BLOCKS16-28', '.pdf', sep = ''),
       plot = lf.plot.2, units = 'mm', width = 190, height = 250)
ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_SizeFrequencyPlot_Repeat_BLOCKS16-28', '.png', sep = ''),
       plot = lf.plot.2, units = 'mm', width = 190, height = 250)

##---------------------------------------------------------------------------##
# PLOT 10: Density Years ####

# Average density determined by the count of all legal and sub-legal abalone per 
# 10 min by year for each site within each block relative to the straightline distance
# between the recorded start and finish vessel position.

# Determine mean abalone abundance in each block, year and size class
ten.min.mean.year <- time.swim.dat.final %>% 
 filter(!subblockno %in% c('28B', '28C') & 
         !blockno %in% c('13', '14', '29', '30') &
         !is.na(sizeclass_freq_10) &
         distance > 0) %>%
 group_by(blockno, site, diver, sampyear, time.elapsed, legal.size) %>% 
 summarise(ab.n = sum(sizeclass_freq_10)) %>% 
 group_by(blockno, sampyear, legal.size) %>% 
 summarise(mean.ab.n = mean(ab.n),
           median.ab.n = median(ab.n))

# Extract distance of swim at each site in each block, year and size class
site_distance <- time.swim.dat.final %>% 
 filter(!subblockno %in% c('28B', '28C') & 
         !blockno %in% c('13', '14', '29', '30') &
         !is.na(sizeclass_freq_10) &
         distance > 0) %>%
 group_by(blockno, site, sampyear, legal.size) %>% 
 summarise(ab.n = sum(sizeclass_freq_10),
           mean.dist = max(distance))

# Join mean abundance and distance of swim and calculate mean density in each 
# block, year and size class
mean_density <- left_join(ten.min.mean.year, site_distance) %>% 
 mutate(ab.m2 = mean.ab.n / mean.dist) %>%  
 group_by(blockno, sampyear, legal.size) %>% 
 mutate(ab.m2 = mean(ab.m2))

# Create a dodge for point in plot
pd <- position_dodge(0.1)

# Create plot
density_plot <- mean_density %>%
 ggplot(aes(x = factor(sampyear), y = ab.m2, colour = blockno, group = blockno)) + 
 geom_point(position = pd)+
 geom_smooth(method = lm, se = F)+
 facet_wrap(~ legal.size)+
 theme_bw()+
 ylab(bquote('Density (abalone.'*~m^-2*')'))+
 xlab('Year')+
 guides(colour = guide_legend(title = 'BlockNo'))

# Save plots 
setwd(ts.plots.folder)
ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_Density_LegalSubLegal', '.pdf', sep = ''), 
       plot = density_plot, units = 'mm', width = 190, height = 150)
ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_Density_LegalSubLegal', '.png', sep = ''), 
       plot = density_plot, units = 'mm', width = 190, height = 150)

rm('density_plot', 'pd', 'mean_density', 'site_distance', 
   'ten.min.mean.year')


##---------------------------------------------------------------------------##
## MAP 1: Site Abundance Year ####

# Average count by site for each block and year represented by a coloured circle
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
time.swim.sites <- time.swim.dat.final %>%
 filter(!subblockno %in% c('28B', '28C')) %>% 
 distinct(site, .keep_all = T) %>% 
 dplyr::select(c(sampyear, site, blockno, subblockno, sampdate, actual.geom)) %>% 
 st_as_sf() %>% 
 st_set_crs(GDA2020)

ten.min.mean.site <- time.swim.dat.final %>% 
 filter(!subblockno %in% c('28B', '28C')) %>% 
 group_by(sampyear, site, blockno, subblockno, diver, legal.size) %>% 
 summarise(ab.n = sum(sizeclass_freq)) %>% 
 group_by(sampyear, site, blockno, subblockno, legal.size) %>% 
 summarise(mean.ab.n = mean(ab.n)) %>% 
 mutate(legal.size = factor(legal.size))%>% 
 as.data.frame()

time.swim.count.site.loc <- left_join(ten.min.mean.site, time.swim.sites) %>% 
 st_as_sf() 

# Create parameter dataframe of block and size class combinations
blocknos <- c(16, 21, 22, 23, 24, 27, 28)
legal_sizes <- c('<140 mm', '>140 mm')

plot_parameters_data <- expand.grid(blockno = blocknos,
                                    legal_size = legal_sizes) %>% 
 as_tibble()

# Create plot function
ts_site_plot <- function(i){
 
 # Extract blockno and size class variables
 ts_blockno_plot <- plot_parameters_data[i, 'blockno'] %>% 
  pull()
 
 ts_legal_size_plot <- plot_parameters_data[i, 'legal_size'] %>% 
  pull()
 
 size_class <- ifelse(grepl('<140 mm',ts_legal_size_plot), 'SUB-LEGAL', 'LEGAL')
 
 ts.tas.subblockmap.crop <- sf.subblock.map %>%  
  {if(ts_blockno_plot == 28) filter(., subblockno == '28A') else filter(., blockno == ts_blockno_plot)}
 
 # crop tas land map to subblock
 ts.tas.coast.crop <- st_crop(sf.tas.map, ts.tas.subblockmap.crop)
 
 # create plot
 count_site_map <- ggplot(data = st_geometry(ts.tas.coast.crop)) +
  geom_sf(data = time.swim.count.site.loc %>% 
           filter(legal.size == ts_legal_size_plot &
                   blockno == ts_blockno_plot), aes(label = subblockno), fill = NA)+
  geom_sf(fill = 'grey') +
  geom_sf(data = time.swim.count.site.loc %>% 
           filter(legal.size == ts_legal_size_plot &
                   blockno == ts_blockno_plot), aes(fill = mean.ab.n), shape = 21, size = 2)+
  scale_fill_gradientn(colours = c("navyblue", "blue", "cyan", "green", "yellow", "orange", "red"),
                       limits = c(0, 125),
                       breaks = c(0, 50, 100),
                       labels = c(0, 50, 100))+
  theme_bw() +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = if_else(ts_blockno_plot == 23, 'bl', "br"), which_north = "true", 
                         pad_x = unit(0.05, "cm"), pad_y = unit(0.4, "cm"),
                         style = north_arrow_fancy_orienteering)+
  labs(title = ifelse(grepl('<140 mm',ts_legal_size_plot), 'SUB-LEGAL <140 mm', 'LEGAL >140 mm'), fill = (bquote('Average\ncount')))+
  scale_x_continuous(breaks = seq(140, 149, by = 0.1))+
  ylab('Latitude')+
  facet_wrap(~ sampyear, ncol = if_else(ts_blockno_plot %in% c(16, 22, 27, 28), 4, 2))
 
 # save plot
 setwd(ts.plots.folder)
 ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_SiteAbundance_BlockNo_',
                         ts_blockno_plot, '_', size_class, '.pdf', sep = ''),
        plot = count_site_map, units = 'mm', width = 250, height = 250)
 ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_SiteAbundance_BlockNo_',
                         ts_blockno_plot, '_', size_class, '.png', sep = ''),
        plot = count_site_map, units = 'mm', width = 250, height = 250)
 
}

# run function across all block, year and size class combinations
ts_site_plots <- lapply(1:nrow(plot_parameters_data), ts_site_plot)

##---------------------------------------------------------------------------##
## MAP 2: Site Count Size ####

# Average count by site for each block and size represented by a coloured circle
# for assessment year

# sf.tas.map <- st_read(paste(sprintf("C:/Users/%s/University of Tasmania/IMAS-DiveFisheries - Assessments - Documents/Assessments/GIS/SpatialLayers/OldAbLayers/TasLand.gpkg", Sys.info()[["user"]])))
# sf.subblock.map <- st_read(paste(sprintf("C:/Users/%s/University of Tasmania/IMAS-DiveFisheries - Assessments - Documents/Assessments/GIS/SpatialLayers/IMAS_Layers/IMAS_subblock_rev2022.gpkg", Sys.info()[["user"]])))
sf.tas.map <- st_read(paste(sprintf("C:/Users/%s/Dropbox (UTAS Research)/DiveFisheries/GIS/SpatialLayers/OldAbLayers/TasLand.gpkg", Sys.info()[["user"]])))
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

# identify unique sites sampled from timed swim dataframe and join to renamed site file
time.swim.sites <- time.swim.dat.final %>%
 filter(!subblockno %in% c('28B', '28C')) %>% 
 distinct(site, .keep_all = T) %>% 
 dplyr::select(c(sampyear, site, blockno, subblockno, sampdate, actual.geom)) %>% 
 st_as_sf() %>% 
 st_set_crs(GDA2020)

ten.min.mean.site <- time.swim.dat.final %>% 
 filter(!subblockno %in% c('28B', '28C')) %>% 
 group_by(sampyear, site, blockno, subblockno, diver, legal.size) %>% 
 summarise(ab.n = sum(sizeclass_freq)) %>% 
 group_by(sampyear, site, blockno, subblockno, legal.size) %>% 
 summarise(mean.ab.n = mean(ab.n)) %>% 
 mutate(legal.size = factor(legal.size))%>% 
 as.data.frame()

time.swim.count.site.loc <- left_join(ten.min.mean.site, time.swim.sites) %>% 
 st_as_sf() 

# Create parameter dataframe of block and year combinations
blocknos <- c(16, 21, 22, 23, 24, 27, 28)
samp_years <- c(2020, 2021, 2022, 2023)

plot_parameters_data <- expand.grid(blockno = blocknos,
                                    samp_year = samp_years) %>% 
 as_tibble()

# Select sample year
plot_parameters_data <- plot_parameters_data %>% 
 filter(samp_year == samp.year)

# Create plot function
ts_site_size_plot <- function(i){
 
 # extract variables to add to ipdw dataframe at end
 ts_blockno_plot <- plot_parameters_data[i, 'blockno'] %>% 
  pull()
 
 ts_samp_year_plot <- plot_parameters_data[i, 'samp_year'] %>% 
  pull()
 
 ts.tas.subblockmap.crop <- sf.subblock.map %>%  
  {if(ts_blockno_plot == 28) filter(., subblockno == '28A') else filter(., blockno == ts_blockno_plot)}
 
 # crop tas land map to subblock
 ts.tas.coast.crop <- st_crop(sf.tas.map, ts.tas.subblockmap.crop)
 
 # create plot
 count_site_size_map <- ggplot(data = st_geometry(ts.tas.coast.crop)) +
  geom_sf(fill = 'grey') +
  geom_sf(data = time.swim.count.site.loc %>% 
           filter(sampyear == ts_samp_year_plot &
                   blockno == ts_blockno_plot), aes(fill = mean.ab.n), shape = 21, size = 2)+
  scale_fill_gradientn(colours = c("navyblue", "blue", "cyan", "green", "yellow", "orange", "red"),
                       limits = c(0, 125),
                       breaks = c(0, 50, 100),
                       labels = c(0, 50, 100))+
  theme_bw() +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = if_else(ts_blockno_plot == 23, 'bl', "br"), which_north = "true", 
                         pad_x = unit(0.05, "cm"), pad_y = unit(0.4, "cm"),
                         style = north_arrow_fancy_orienteering)+
  labs(title = ts_samp_year_plot, fill = (bquote('Average\ncount')))+
  scale_x_continuous(breaks = seq(140, 149, by = 0.1))+
  ylab('Latitude')+
  facet_wrap(. ~ legal.size)
 
 # save plot
 setwd(ts.plots.folder)
 ggsave(filename = paste('TimedSwimSurvey_SiteSizeAbundance_BlockNo_',
                         ts_blockno_plot, '_', ts_samp_year_plot, '.pdf', sep = ''),
        plot = count_site_size_map, units = 'mm', width = 250, height = 250)
 ggsave(filename = paste('TimedSwimSurvey_SiteSizeAbundance_BlockNo_',
                         ts_blockno_plot, '_', ts_samp_year_plot, '.png', sep = ''),
        plot = count_site_size_map, units = 'mm', width = 250, height = 250)
 
}

# Run function across all block, year and size class combinations
ts_site_size_plots <- lapply(1:nrow(plot_parameters_data), ts_site_size_plot)


rm(list = setdiff(ls(), c('time.swim.dat.final', 'time.swim.meta.dat.final', 'time.swim.dat.df.final',
     'time.swim.dat.final', 'samp.year', 'samp.year.folder', 'ts.plots.folder')))

##---------------------------------------------------------------------------##
## MAP 3: Ref Site Count Size Change ####

# Average count by site for each block and size represented by a coloured circle
# for assessment year

# sf.tas.map <- st_read(paste(sprintf("C:/Users/%s/University of Tasmania/IMAS-DiveFisheries - Assessments - Documents/Assessments/GIS/SpatialLayers/OldAbLayers/TasLand.gpkg", Sys.info()[["user"]])))
# sf.subblock.map <- st_read(paste(sprintf("C:/Users/%s/University of Tasmania/IMAS-DiveFisheries - Assessments - Documents/Assessments/GIS/SpatialLayers/IMAS_Layers/IMAS_subblock_rev2022.gpkg", Sys.info()[["user"]])))
sf.tas.map <- st_read(paste(sprintf("C:/Users/%s/Dropbox (UTAS Research)/DiveFisheries/GIS/SpatialLayers/OldAbLayers/TasLand.gpkg", Sys.info()[["user"]])))
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

# identify unique sites sampled from timed swim dataframe and join to renamed site file
time.swim.sites <- time.swim.dat.final %>%
 filter(!subblockno %in% c('28B', '28C') &
         ref_site == 1) %>% 
 distinct(site, .keep_all = T) %>% 
 dplyr::select(c(sampyear, site, blockno, subblockno, sampdate, actual.geom)) %>% 
 st_as_sf() %>% 
 st_set_crs(GDA2020)

ten.min.mean.site <- time.swim.dat.final %>% 
 filter(!subblockno %in% c('28B', '28C') &
         ref_site == 1) %>% 
 group_by(sampyear, site, blockno, subblockno, diver, legal.size) %>% 
 summarise(ab.n = sum(sizeclass_freq)) %>% 
 group_by(sampyear, site, blockno, subblockno, legal.size) %>% 
 summarise(mean.ab.n = mean(ab.n)) %>% 
 mutate(legal.size = factor(legal.size)) %>% 
 spread(sampyear, mean.ab.n) %>% 
 as.data.frame()

df_1 <- ten.min.mean.site %>% 
 mutate(n_diff_23_22 = (`2023` - `2022`) / `2023` * 100,
        point_col_23_22 = ifelse(n_diff_23_22 < 1, 'red', 'darkgreen'),
        n_diff_22_21 = (`2022` - `2021`) / `2022` * 100,
        point_col_22_21 = ifelse(n_diff_22_21 < 1, 'red', 'darkgreen'), 
        n_diff_21_20 = (`2021` - `2020`) / `2021` * 100,
        point_col_21_20 = ifelse(n_diff_21_20 < 1, 'red', 'darkgreen')) %>%
 gather(., n_diff, point_col, point_col_23_22, point_col_22_21, point_col_21_20)

df_2 <- df_1 %>% 
 filter(!is.na(point_col)) %>% 
 group_by(n_diff, blockno, legal.size) %>% 
 mutate(improve = ifelse(point_col == 'darkgreen', 1, 0)) %>% 
 summarise(n = n(),
           sites_improve = sum(improve),
           perc_improve = round(((sites_improve / 15) * 100), 0),
           crit_col = ifelse(perc_improve >= 75, 'darkgreen', 'red'))


df_5 <- df_2 %>% 
 filter(n_diff == 'point_col_23_22') %>% 
 left_join(., df_3) %>% 
 mutate(trend_col = ifelse(perc_improve >= 75 &
                            mean_slope >= 0, 'green',
                           ifelse(perc_improve < 75 & 
                                   mean_slope >= 0, 'yellow', 'red')))


time.swim.count.site.loc <- left_join(df_1, time.swim.sites) %>% 
 st_as_sf() 

# Create parameter dataframe of block and year combinations
blocknos <- c(16, 22, 23, 24, 27, 28)
samp_years <- c(2020, 2021, 2022, 2023)
legal_sizes <- c('<140 mm', '>140 mm')

plot_parameters_data <- expand.grid(blockno = blocknos,
                                    samp_year = samp_years,
                                    legal_size = legal_sizes) %>% 
 as_tibble()

# Select sample year
plot_parameters_data <- plot_parameters_data %>%
 filter(samp_year == samp.year)

# Facet labels
facet_labs <- c('2020 v 2021', '2021 vs 2022', '2022 vs 2023')
names(facet_labs) <- c('point_col_21_20', 'point_col_22_21', 'point_col_23_22')

ts_blockno_plot <- 16
ts_samp_year_plot <- 2023
ts_legal_size_plot <- '>140 mm'

# Create plot function
ts_site_size_plot <- function(i){
 
 # extract variables to add to ipdw dataframe at end
 ts_blockno_plot <- plot_parameters_data[i, 'blockno'] %>% 
  pull()
 
 ts_samp_year_plot <- plot_parameters_data[i, 'samp_year'] %>% 
  pull()
 
 ts_legal_size_plot <- plot_parameters_data[i, 'legal_size'] %>% 
  pull()
 
 size_class <- ifelse(grepl('<140 mm',ts_legal_size_plot), 'SUB-LEGAL', 'LEGAL')
 
 ts.tas.subblockmap.crop <- sf.subblock.map %>%  
  {if(ts_blockno_plot == 28) filter(., subblockno == '28A') else filter(., blockno == ts_blockno_plot)}
 
 # crop tas land map to subblock
 ts.tas.coast.crop <- st_crop(sf.tas.map, ts.tas.subblockmap.crop)
 
 # create plot
 count_site_size_map <- ggplot(data = st_geometry(ts.tas.coast.crop)) +
  geom_sf(fill = 'grey') +
  geom_sf(data = time.swim.count.site.loc %>% 
           filter(blockno == ts_blockno_plot & 
                   legal.size == ts_legal_size_plot), 
          aes(col = point_col), shape = 19, size = 2)+
  scale_color_identity()+
  theme_bw() +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = if_else(ts_blockno_plot == 23, 'bl', "br"), which_north = "true", 
                         pad_x = unit(0.05, "cm"), pad_y = unit(0.4, "cm"),
                         style = north_arrow_fancy_orienteering)+
  scale_x_continuous(breaks = seq(140, 149, by = 0.1))+
  ylab('Latitude')+
  labs(title = size_class)+
  facet_wrap(. ~ n_diff, ncol = if_else(ts_blockno_plot %in% c(16, 22, 27, 28), 4, 2),
             labeller = labeller(n_diff = facet_labs))+
  geom_point(data = df_5 %>% filter(blockno == ts_blockno_plot & 
                                    legal.size == ts_legal_size_plot), 
            aes(col = trend_col, x = Inf, y = Inf), vjust = 1.5, hjust = 1.5)
 
 # save plot
 setwd(ts.plots.folder)
 ggsave(filename = paste('TimedSwimSurvey_ReferenceSiteAbundance_Change_BlockNo_',
                         ts_blockno_plot, '_', ts_samp_year_plot, '_', size_class, '.pdf', sep = ''),
        plot = count_site_size_map, units = 'mm', width = 250, height = 250)
 ggsave(filename = paste('TimedSwimSurvey_ReferenceSiteAbundance_Change_BlockNo_',
                         ts_blockno_plot, '_', ts_samp_year_plot, '_', size_class, '.png', sep = ''),
        plot = count_site_size_map, units = 'mm', width = 250, height = 250)
 
}

# Run function across all block, year and size class combinations
ts_site_size_plots <- lapply(1:nrow(plot_parameters_data), ts_site_size_plot)

##---------------------------------------------------------------------------##
# Determine trajectory (slope) of reference site abundance between years

ten.min.mean.site <- time.swim.dat.final %>% 
 filter(!subblockno %in% c('28B', '28C') &
         ref_site == 1) %>% 
 group_by(sampyear, site, blockno, subblockno, diver, legal.size) %>% 
 summarise(ab.n = sum(sizeclass_freq)) %>% 
 group_by(sampyear, site, blockno, subblockno, legal.size) %>% 
 summarise(mean.ab.n = mean(ab.n)) %>% 
 mutate(legal.size = factor(legal.size)) %>% 
 spread(sampyear, mean.ab.n) %>% 
 as.data.frame()

df_1 <- ten.min.mean.site %>% 
 mutate(n_diff_23_22 = (`2023` - `2022`) / `2023` * 100,
        point_col_23_22 = ifelse(n_diff_23_22 < 1, 'red', 'darkgreen'),
        n_diff_22_21 = (`2022` - `2021`) / `2022` * 100,
        point_col_22_21 = ifelse(n_diff_22_21 < 1, 'red', 'darkgreen'), 
        n_diff_21_20 = (`2021` - `2020`) / `2021` * 100,
        point_col_21_20 = ifelse(n_diff_21_20 < 1, 'red', 'darkgreen')) %>%
 gather(., n_diff, point_col, point_col_23_22, point_col_22_21, point_col_21_20)

# create a "slope" column
df_1$slope <- NA
# extract the slopes of the regressions with each row beginning from the 2nd as the y values and the first row as the x values
df_1[2:nrow(df_1), "slope"] <- apply(df_1[2:nrow(df_1), c("2020", "2021", "2022", "2022")], 1, function(row_i){
 lm(unlist(row_i) ~ unlist(df_1[1, c("2020", "2021", "2022", "2022")]))$coefficients[2]
})

df_3 <- df_1 %>% 
 group_by(n_diff, blockno, legal.size) %>% 
 summarise(mean_slope = mean(slope))

df_2 <- df_1 %>% 
 filter(!is.na(point_col)) %>% 
 group_by(n_diff, blockno, legal.size) %>% 
 mutate(improve = ifelse(point_col == 'darkgreen', 1, 0)) %>% 
 summarise(n = n(),
           sites_improve = sum(improve),
           perc_improve = round(((sites_improve / 15) * 100), 0),
           crit_col = ifelse(perc_improve >= 75, 'darkgreen', 'red'))

# determine if trend meets criteria of increasing abundance in sample year
df_2 %>% 
 filter(n_diff == 'point_col_23_22') %>% 
 left_join(., df_3) %>% 
 mutate(trend_col = ifelse(perc_improve >= 75 &
                            mean_slope >= 0, 'green',
                           ifelse(perc_improve < 75 & 
                                   mean_slope >= 0, 'yellow', 'red')))

##---------------------------------------------------------------------------##
# PLOT 11: Diver Deviation ####

# # Load timed swim diver pair data
# time.swim.divers <- read.xlsx("C:/CloudStor/DiveFisheries/Abalone/FISdata/FIS_TimedSwimSurveys2022/timed_swim_diver_details_2022.xlsx",
#                               detectDates = T)
# 
# # Add end date for divers still participating
# time.swim.divers <- time.swim.divers %>% 
#  mutate(end.date = if_else(is.na(end.date), Sys.Date(), end.date))

# Summarise total count for site x blockno x sampyear x sampdate x diver x legal.size
ts_sum_count <- time.swim.dat.final %>% 
 filter(!subblockno %in% c('28B', '28C')) %>%
 group_by(site, blockno, sampyear, sampdate, diver, legal.size) %>% 
 summarise(ab.n = sum(sizeclass_freq_10)) %>%
 as.data.frame()

# Identify divers one and two for each site
divers_id <- ts_sum_count %>%
 filter(legal.size == '>140 mm') %>% 
 select(c(site, sampdate, diver)) %>% 
 group_by(site, sampdate) %>% 
 mutate(diver_no = 1:n())

# Identify unique diver pair for each site
diver_pair_id <- divers_id %>% 
 spread(diver_no, diver) %>% 
 filter(!is.na(`2`)) %>% 
 mutate(dive_pair = paste(`1`, `2`, sep = '')) %>% 
 group_by(dive_pair) %>% 
 mutate(dive_pair_id = cur_group_id())

# unique(diver_pair_id$dive_pair)

# Join diver one and two identification to count summary
divers_id_count <- left_join(ts_sum_count, divers_id, by = c('site', 'sampdate', 'diver'))

# Join unique diver pair identification to count summary
diver_pair_count <- left_join(ts_sum_count, diver_pair_id, by = c('site', 'sampdate'))

# Join divers and diver pair summaries
ts.count.divers <- left_join(diver_pair_count, divers_id_count) %>% 
 dplyr::rename('diver_a' = '1',
               'diver_b' = '2')

# Add diver pairing details to summary data
# ts.count.divers <-  fuzzy_left_join(ts.count.sum.2, time.swim.divers, 
#                                     by = c("diver" = "diver", 
#                                            "sampdate" = "start.date",
#                                            "sampdate" = "end.date"),
#                                     match_fun = list(`==`, `>=`, `<=`))

# Identify diver pairs x year
# ts.count.divers %>% 
#  group_by(sampyear, diver.x, dive_pair_id) %>%
#  distinct(dive_pair_id) 

# Select sample year
samp.year <- c(2023)

# Identify diver pair IDs x chosen year (remove Jaime McAllister JM)
diver.ids <- ts.count.divers %>% 
 filter(sampyear == samp.year,
        dive_pair_id != 4) %>% 
 distinct(dive_pair_id) %>% 
 pull()

q <- list()

for (i in diver.ids){
 
 
 # Select data for dive pair and sample year
 ts.diver.dev.dat <- ts.count.divers %>% 
  filter(dive_pair_id == i, sampyear == samp.year) %>% 
  select(c(site, blockno, legal.size, sampyear, sampdate, diver, ab.n)) %>%  
  spread(key = diver, value = ab.n) %>%  
  mutate(dive.diff = abs(.[[6]] - .[[7]]))
 
 # Identify diver names for plot title
 dive.dev.divers <- ts.diver.dev.dat %>% 
  select(c(6,7))
 
 # determine number of sites surveyed by diver pair for plot
 divers.site.n <- ts.diver.dev.dat %>%
  filter(legal.size == '<140 mm') %>% 
  group_by(blockno) %>% 
  summarise(site.n = paste(n_distinct(site), sep = ''))
 
 q[[i]] <-
  # create plot
  dive.dev.plot <- ggplot(data = ts.diver.dev.dat, aes(x = blockno, y = dive.diff))+
  geom_boxplot(aes(fill = factor(legal.size)))+
  xlab('BlockNo')+
  ylab('Count Difference')+
  coord_cartesian(ylim = c(0, 90))+
  theme_bw()+
  ggtitle(paste(names(dive.dev.divers[1]), 'vs', names(dive.dev.divers[2])))+
  geom_text(data = divers.site.n, aes(y = 90, label = site.n), size = 3)+
  scale_fill_manual(values = c("#999999", "#56B4E9"))+
  # theme(legend.position = "none")
  theme(legend.title = element_blank(),
        legend.position = c(0.15, 0.8))
 
 # plot_legend <- get_legend(dive.dev.plot)+
 #  theme(legend.position = "bottom")
 
}

# q %>%
#  discard(is.null) %>%
#  cowplot::plot_grid(plotlist = c(map(.x = .,
#                            .f = function(x) x + 
#                             theme(legend.position = 'none')), list(get_legend(.[[1]]))), nrow = 3)

dive.dev.plot <- q %>%
discard(is.null) %>%
ggarrange(plotlist = .,
          common.legend = TRUE,
          legend = "bottom")


# dive.dev.plot <- q %>% 
#  discard(is.null) %>% 
#  cowplot::plot_grid(plotlist = ., plot_legend)



# save plot
setwd(ts.plots.folder)
ggsave(filename = paste('TimedSwimSurvey_DiverDeviation_', samp.year, '.pdf', sep = ''),
       plot = dive.dev.plot, units = 'mm', width = 210, height = 250)

ggsave(filename = paste('TimedSwimSurvey_DiverDeviation_', samp.year, '.png', sep = ''),
       plot = dive.dev.plot, units = 'mm', width = 210, height =250)

rm(list = setdiff(ls(), c('time.swim.dat.final',
                          'time.swim.dat.df.final',
                          'time.swim.meta.dat.final',
                          'ts.count.divers',
                          'samp.year',
                          'samp.year.folder',
                          'ts.plots.folder')))

##---------------------------------------------------------------------------##
# PLOT 12: Bland-Altman ####
# Bland-Altman plot of diver count differences
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4470095/
df.1 <- ts.count.divers %>% 
 filter(sampyear == samp.year) %>% 
 select(c(site, blockno, legal.size, sampyear, sampdate, dive_pair_id, diver_a, diver_b, ab.n)) %>%  
 group_by(site, blockno, legal.size, sampyear, sampdate, dive_pair_id) %>% 
 mutate(var = paste0('ab.n', row_number())) %>% 
 spread(var,ab.n) %>%  
 ungroup() %>% 
 mutate_at(c(9:10), ~replace(., is.na(.), 0)) %>% 
 mutate(dive.diff = .[[9]] - .[[10]],
        dive.avg = (.[[9]] + .[[10]]) / 2) %>%  
 select(c(site, blockno, legal.size, sampyear, sampdate, dive_pair_id,  diver_a, diver_b, dive.diff, dive.avg))

df.2 <- df.1 %>% group_by(dive_pair_id, legal.size) %>% 
 summarise(avg.diff = round(mean(dive.diff), 1),
           n = n(),
           pos.ci = round(mean(dive.diff) + 1.96 * sd(dive.diff)/sqrt(n()), 1),
           neg.ci = round(mean(dive.diff) - 1.96 * sd(dive.diff)/sqrt(n()), 1),
           pos.sd = round(mean(dive.diff) + (1.96 * sd(dive.diff)), 1),
           neg.sd = round(mean(dive.diff) - (1.96 * sd(dive.diff)), 1)) %>% 
 mutate(mean.diff = paste('Mean = ', avg.diff))

# diver.pairs <- c('Diver Pair 1', 'Diver Pair 6')
# names(diver.pairs) <- c('1', '6')

facet_labs <- ts.count.divers %>% 
 filter(!is.na(dive_pair_id)) %>% 
 select(dive_pair_id, diver_a, diver_b) %>% 
 distinct() %>% 
 arrange(dive_pair_id) %>% 
 mutate(facet_lab = paste(diver_a, 'vs', diver_b, sep = ' ')) %>% 
 pull(facet_lab)

names(facet_labs) <- c(1:n_distinct(facet_labs))

BA.plot <- df.1 %>% ggplot(aes(x = dive.avg, y = dive.diff, group = dive_pair_id))+
 # geom_point(aes(colour = blockno), alpha = 0.5) +
 geom_point(color = 'grey', alpha = 0.8) +
 geom_hline(data = df.2, aes(yintercept = avg.diff), colour = "blue", size = 0.5) +
 geom_hline(data = df.2, aes(yintercept = pos.sd), colour = "red", size = 0.5, linetype = 'dashed') +
 geom_hline(data = df.2, aes(yintercept = neg.sd), colour = "red", size = 0.5, linetype = 'dashed') +
 # geom_hline(data = df.2, aes(yintercept = pos.ci), colour = "blue", size = 0.5, linetype = 'dashed') +
 # geom_hline(data = df.2, aes(yintercept = neg.ci), colour = "blue", size = 0.5, linetype = 'dashed')+
 geom_hline(data = df.2, aes(yintercept = 0), colour = "black", size = 0.5, linetype = 'dashed') +
 # geom_hline(yintercept = mean(df.1$dive.diff), colour = "blue", size = 0.5) +
 # geom_hline(yintercept = mean(df.1$dive.diff) - (1.96 * sd(df.1$dive.diff)), colour = "red", size = 0.5, linetype = 'dashed') +
 # geom_hline(yintercept = mean(df.1$dive.diff) + (1.96 * sd(df.1$dive.diff)), colour = "red", size = 0.5, linetype = 'dashed') +
 ylab("Abalone Count Difference Between Divers") +
 xlab("Average Abalone Count")+
 labs(colour = 'BlockNo')+
 theme_bw()+
 geom_text(data = df.2, aes(x = 150, y = 50, label = mean.diff), size = 3, vjust = -0.5)+
 # ggtitle(paste('Diver', names(ts.diver.dev.dat[6]), 'vs', 'Diver', names(ts.diver.dev.dat[7])))+
 # theme(legend.position = 'none')+
 # facet_grid(dive_pair_id ~ legal.size)
 facet_grid(dive_pair_id ~ legal.size,
            labeller = labeller(dive_pair_id = facet_labs))

# save plot
setwd(ts.plots.folder)
ggsave(filename = paste('TimedSwimSurvey_Bland-Altman_DiverDifference_IAS2023_', samp.year, '.pdf', sep = ''),
       plot = BA.plot, units = 'mm', width = 210, height = 200)

ggsave(filename = paste('TimedSwimSurvey_Bland-Altman_DiverDifference_IAS2023_', samp.year, '.png', sep = ''),
       plot = BA.plot, units = 'mm', width = 210, height =200)

##---------------------------------------------------------------------------##
## PLOT 13: CPUE kg.hr ####
## estimate CPUE in kg/hr for each block using length-weight relationships from commercial catch sampling 
## to convert timed swim lengths to weight

# load most recent commercial catch sampling compiled MM dataframe
compiledMM.df.final <- readRDS('C:/CloudStor/R_Stuff/MMLF/compiledMM.df.final.RDS')

# select length-weight data, removing obvious erroneous data or data collected from multiple blocks in a trip
lw.dat <- compiledMM.df.final %>% 
 filter(between(whole.weight, 200, 1500) & 
         between(shell.length, sizelimit - 5, 220) & 
         !(shell.length > 175 & whole.weight < 600), 
        !(shell.length > 180 & whole.weight < 1000) &
         numblocks == 1,
        species == 1) 

# calculate log of length and weight
lw.dat.log <- lw.dat %>% 
 mutate(log.sl = log(shell.length),
        log.wt = log(whole.weight))

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
# block 13 used across all blocks given no length-weight data has been collected from
# the closed blocks since the re-commencement of commercial catch sampling in 2018. 
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

# quick summary of total kg estimate by block
time.swim.dat.df.wt %>%
 filter(!subblockno %in% c('28B', '28C')) %>%
 dplyr::group_by(sampyear, blockno) %>% 
 dplyr::summarise(total.kg = round(sum(est.weight), digits = 2)) %>% 
 as.data.frame()

# estimate CPUE for each site
time.swim.cpue.site <- time.swim.dat.df.wt %>%
 filter(!subblockno %in% c('28B', '28C'),
        !blockno %in% c('13', '14', '29', '30')) %>%
 dplyr::group_by(blockno, site, diver, sampyear) %>% 
 dplyr::summarise(total.kg = round(sum(est.weight), digits = 2),
                  est.kg.hr = round((total.kg / max(time.elapsed)) * 60, digits = 2)) %>%   
 group_by(blockno, site, sampyear) %>% 
 summarise(est.kg.hr = round(mean(est.kg.hr), digits = 2)) %>% 
 as.data.frame()

# estimate CPUE for each blockno
time.swim.cpue.blockno <- time.swim.cpue.site %>%
 group_by(blockno, sampyear) %>% 
 summarise(est_kg_hr = round(mean(est.kg.hr), digits = 2),
           kg_hr_sd = sd(est.kg.hr),
           kg_hr_n = n(),
           kg_hr_se = kg_hr_sd/sqrt(kg_hr_n)) %>% 
 as.data.frame()

# determine number of sites sampled for each blockno
time.swim.cpue.n <- time.swim.cpue.site %>% 
 group_by(blockno, sampyear) %>% 
 summarise(n = n_distinct(site))

# plot CPUE estimate for each blocnkno between years
cpue.plot <- time.swim.cpue.site %>%
 ggplot(aes(x = blockno, y = est.kg.hr)) +
 geom_boxplot(aes(fill = as.factor(sampyear)), position = position_dodge2(1, preserve = 'single'),
              outlier.colour = '#EE8866') +
 scale_fill_manual(values = c("#77AADD", "#BBCC33", "#DDDDDD", '#44BB99'))+
 stat_summary(aes(group = as.factor(sampyear)), fun.y = mean, 
              geom = 'point', shape = 19, size = 2, colour = 'red', fill = 'red',
              position = position_dodge2(0.8))+
 theme_bw() +
 # ylim(0, 450)+
 coord_cartesian(ylim = c(0, 200))+ 
 ylab(bquote('Estimated CPUE ('*~kg.hr^-1*')'))+
 xlab('Blockno')+
 geom_text(data = time.swim.cpue.n, aes(y = 200, label = n, 
                                        colour = factor(sampyear, levels = c('2020', '2021', '2022', '2023'))), size = 3, position = position_dodge2(0.8))+
 scale_colour_manual(values = c("#77AADD", "#BBCC33", "#DDDDDD", '#44BB99'))+
 guides(size = 'legend', colour = 'none',
        fill = guide_legend(title = 'Year'))

# save plots
setwd(ts.plots.folder)
ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_CPUEPlot', '.pdf', sep = ''), 
       plot = cpue.plot, units = 'mm', width = 190, height = 120)
ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_CPUEPlot', '.png', sep = ''), 
       plot = cpue.plot, units = 'mm', width = 190, height = 120)

## PLOT 14: CPUE fishery ####
# Plot historical fishery CPUE until closure of blocks in 2020 vs estimated timed swim CPUE

# Note: run code for Plot 13 above first

# Import data from HS_TACExtract2023.R code from line 284-286
tacadjust_e <- readRDS('C:/cloudstor/DiveFisheries/Abalone/FISdata/FIS_TimedSwimSurveys2022/TACAdjust_E.rds')

tacadjust_e %>% 
 filter(group %in% c('16', '22', '23', '24', '27', '28'),
        fishyear < 2020) %>%
 ggplot(aes(x = factor(fishyear), colour = factor(group)))+
 geom_point(aes(y = stndcpue), size = 3)+
 geom_line(aes(y = stndcpue, group = group))

# Filter fishery cpue data for closed blocks
ezfishery_dat <- tacadjust_e %>% 
 filter(group %in% c('16', '22', '23', '24', '27', '28'),
        fishyear < 2020) %>% 
 select(c(group, fishyear, stndcpue, stndlower, stndupper)) %>% 
 dplyr::rename(blockno = 'group') %>% 
 mutate(dat_source = 'ce')

# Calculate standard error bars for timed swim data
ts_dat <- time.swim.cpue.blockno %>% 
 dplyr::rename(fishyear = 'sampyear',
               stndcpue = 'est_kg_hr') %>% 
 mutate(stndlower = stndcpue - kg_hr_se,
        stndupper = stndcpue + kg_hr_se,
        dat_source = 'ts',
        blockno = as.numeric(blockno)) %>% 
 select(c(blockno, fishyear, stndcpue, stndlower, stndupper, dat_source))

# Join fishery and timed swim cpue data
cpue_dat <- bind_rows(ezfishery_dat, ts_dat) %>% 
 filter(stndcpue != 0)

# Determine 55th percentile catch target for fishery cpue data
cpue_ff <- cpue_dat %>% 
 filter(dat_source == 'ce') %>% 
 group_by(blockno) %>% 
 summarise(q_55 = quantile(stndcpue, probs =(0.55)))

# Create plot
cpue_plot <- cpue_dat %>%
 ggplot(aes(x = factor(fishyear), y = stndcpue, colour = factor(blockno)))+
 geom_point(data = filter(cpue_dat, dat_source == 'ce'), aes(y = stndcpue), size = 2, colour = 'black')+
 geom_errorbar(data = filter(cpue_dat, dat_source == 'ce'), aes(ymin = stndlower, ymax = stndupper), colour = 'black', width = 0.5)+
 geom_line(data = filter(cpue_dat, dat_source == 'ce'), aes(y = stndcpue, group = blockno), colour = 'black', size = 0.5)+
 geom_point(data = filter(cpue_dat, dat_source == 'ts'), aes(y = stndcpue), size = 2, colour = 'red')+
 geom_errorbar(data = filter(cpue_dat, dat_source == 'ts'), aes(ymin = stndlower, ymax = stndupper), colour = 'red', width = 0.5)+
 geom_line(data = filter(cpue_dat, dat_source == 'ts'), aes(y = stndcpue, group = blockno), colour = 'red')+
 # geom_line(data = filter(cpue_dat, fishyear %in% c(2019, 2020)), aes(y = stndcpue, group = blockno), colour = 'red')+
 geom_hline(data = cpue_ff, aes(yintercept = q_55), linetype = 'dashed', colour = 'blue')+
 theme_bw()+
 coord_cartesian(ylim = c(0, 150))+
 xlab('Year')+
 theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
 ylab(bquote('CPUE ('*~kg.hr^-1*')'))+
 facet_wrap(blockno ~ ., nrow = 3)

# save plots
setwd(ts.plots.folder)
ggsave(filename = paste('TimedSwimSurvey_', '1992-', samp.year, '_CPUEPlot', '.pdf', sep = ''), 
       plot = cpue_plot, units = 'mm', width = 190, height = 150)
ggsave(filename = paste('TimedSwimSurvey_', '1992-', samp.year, '_CPUEPlot', '.png', sep = ''), 
       plot = cpue_plot, units = 'mm', width = 190, height = 150)


##---------------------------------------------------------------------------##

## PLOT 14: CPUE vs TS ####
## plot timed swim average counts for legal and sub-leagl
## against historical fishery CPUE for each site

ts.vs.cpue.plot <- time.swim.dat.final %>% 
 filter(!subblockno %in% c('28B', '28C'),
        sampdate > as.Date('2020-01-01')) %>% 
 group_by(site, diver, legal.size, cell.ntile, sam.count, cpue.kg.hr) %>% 
 summarise(ab.n = sum(sizeclass_freq_10)) %>% 
 group_by(site, legal.size, cell.ntile, sam.count, cpue.kg.hr) %>% 
 summarise(mean.ab.n = mean(ab.n))%>%  
 filter(!is.na(cell.ntile)) %>%
 ggplot(aes(x = cpue.kg.hr, y = mean.ab.n))+
 geom_point(aes(colour = cell.ntile), size = 3)+
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

setwd(ts.plots.folder)
ggsave(filename = paste('TimedSwimSurvey_TenMinuteCountvsGPS.CPUE.kg.hr', '.pdf', sep = ''), 
       plot = ts.vs.cpue.plot, units = 'mm', width = 190, height = 120)
ggsave(filename = paste('TimedSwimSurvey_TenMinuteCountvsGPS.CPUE.kg.hr', '.png', sep = ''), 
       plot = ts.vs.cpue.plot, units = 'mm', width = 190, height = 120)

##---------------------------------------------------------------------------##
##PLOT 10: SAM vs Timed count ####
## Boxplot comparing average counts from historical SAM data and recent
## time swim survey data as block x year

# Load original SAM count data and adjusted/renamed site details 
fis.sam.data <- read.xlsx("C:/Users/jaimem/OneDrive - University of Tasmania/Documents/AB_TimedSwimFIS/SampleDetailsforTimedSwimV2_JM.xlsx",
                          detectDates = T)

ts.site.sam.dat <- readRDS('C:/CloudStor/Shared/DiveFisheries/Abalone/FISdata/FIS_TimedSwimSurveys2021/ts.site.sam.2020.join.RDS')

colnames(fis.sam.data) <- tolower(colnames(fis.sam.data))

# Join original SAM count and site data to get final time swim survey site names 
sam.dat <- fis.sam.data %>% 
 filter(stat.block %in% c(16, 22, 23, 24, 27, 28)) %>% 
 left_join(., ts.site.sam.dat %>% select(c(site.id, site)), by = c('site.id' = 'site.id')) %>% 
 dplyr::rename(site.sam = site.y) %>%   
 mutate(sampyear = year(date)) %>% 
 filter(sampyear >= 2001) %>% 
 dplyr::rename(sampdate = date,
               blockno = stat.block,
               ab.n = avg_count_per.10min,
               diver = collector) %>% 
 select(c(blockno, site.sam, diver, ab.n, sampyear)) %>% 
 filter(!is.na(ab.n))

# Quick plot examining counts by site in each block by year
sam.dat %>% 
 ggplot(aes(x = as.factor(blockno), y = ab.n))+
 geom_jitter(aes(colour = sampyear), width = 0.2, size = 3)+
 scale_color_gradientn(colours = rainbow(11))

# Select latest time swim survey data from standardised data
ts.dat <- time.swim.dat.final %>% 
 filter(!subblockno %in% c('28B', '28C')) %>%
 group_by(sampyear, blockno, site, diver) %>% 
 summarise(ab.n = sum(sizeclass_freq)) %>% 
 mutate(blockno = as.numeric(blockno))

# Combine SAM and latest time swim survey data
ts.sam.dat <- bind_rows(sam.dat, ts.dat) %>% 
 mutate(site = if_else(!is.na(site), site, site.sam))

# Create plot 
ts.sam.count.plot <- ts.sam.dat %>%
 filter(!blockno %in% c(13, 14, 29, 30) &
         ab.n < 200) %>% 
 group_by(blockno, sampyear, site) %>% 
 ggplot(aes(x = as.factor(blockno), y = ab.n, fill = as.factor(sampyear)))+
 geom_boxplot(position = position_dodge2(preserve = "single"))+
 theme_bw()+
 ylab(bquote('Average count (abalone.10'*~min^-1*')'))+
 xlab('Blockno')+
 geom_text(data = ts.sam.dat %>%
            filter(!blockno %in% c(13, 14, 29, 30) & ab.n < 200) %>%
            group_by(blockno, sampyear) %>% 
            summarise(n = n_distinct(site), lab.pos = max(ab.n) + 1),
           aes(y = lab.pos, label = paste0(n, '\n')),
           position = position_dodge2(width = 0.75, preserve = 'single'),
           size = 2)+
 labs(fill = 'Year')+
 scale_fill_viridis(discrete = TRUE)

# Save plot
setwd(ts.plots.folder)
ggsave(filename = paste('TimedSwimSurvey_2020_TenMinuteCountHistoricPlot', '.pdf', sep = ''), 
       plot = ts.sam.count.plot, units = 'mm', width = 190, height = 120)
ggsave(filename = paste('TimedSwimSurvey_2020_TenMinuteCountHistoricPlot', '.png', sep = ''), 
       plot = ts.sam.count.plot, units = 'mm', width = 190, height = 120)

##---------------------------------------------------------------------------##
## PLOT 11: TIMED VS HISTORIC ####
## compare average timed swim counts with available historical count data at SAM 
## research sites and the rating score used to select timed swim sites from GPS logger data

## average count vs cpue rating score
time.swim.dat.vs.cpue.plot <- time.swim.dat.final %>% 
 filter(!subblockno %in% c('28B', '28C'),
        sampdate < as.Date('2022-01-01')) %>% 
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
 filter(!subblockno %in% c('28B', '28C'),
        sampdate < as.Date('2022-01-01')) %>% 
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

setwd('C:/CloudStor/Shared/DiveFisheries/Abalone/FISdata/FIS_TimedSwimSurveys2021/FIS_TimedSwimSurvey2021_Plots')
ggsave(filename = paste('TimedSwimSurvey_2021_TenMinuteCountvsCPUE', '.pdf', sep = ''), 
       plot = time.swim.dat.vs.cpue.plot, units = 'mm', width = 190, height = 120)
ggsave(filename = paste('TimedSwimSurvey_2021_TenMinuteCountvsCPUE', '.png', sep = ''), 
       plot = time.swim.dat.vs.cpue.plot, units = 'mm', width = 190, height = 120)
ggsave(filename = paste('TimedSwimSurvey_2021_TenMinuteCountvsSAM', '.pdf', sep = ''), 
       plot = time.swim.dat.vs.sam.plot, units = 'mm', width = 190, height = 120)
ggsave(filename = paste('TimedSwimSurvey_2021_TenMinuteCountvsSAM', '.png', sep = ''), 
       plot = time.swim.dat.vs.sam.plot, units = 'mm', width = 190, height = 120)

##---------------------------------------------------------------------------##
## PLOT 4: Counts x year x block 13 ####
# Compare total counts of size class at block 13 sites pre and post fishing in 2021

# Set size class
plot.size.class <- '>140 mm'

df.3 <- time.swim.dat.final %>% 
 filter(blockno == 13 &
         sampyear == samp.year) %>%
 group_by(blockno, site, sampyear, sampdate, legal.size) %>% 
 summarise(ab.n = sum(sizeclass_freq_10)) %>% 
 group_by(blockno, site, sampyear, sampdate, legal.size) %>% 
 group_by(site) %>%   
 filter(n() > 2) %>% 
 mutate(status = ifelse(sampdate <= as.Date('2023-03-31'), 'closed', 'open'))

plot.closed <- df.3 %>% 
 filter(status == 'closed', legal.size == plot.size.class) %>%  
 ggplot(aes(x = reorder(site, -ab.n), y = ab.n))+
 geom_bar(stat = 'identity')+
 ylim(0, 150)+
 xlab('Site')+
 ylab(bquote('Total count (abalone.10'*~min^-1*')'))+
 ggtitle(paste('Closed ', plot.size.class))+
 theme_bw()+
 theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

plot.open <- df.3 %>% 
 filter(status == 'open', legal.size == plot.size.class) %>% 
 ggplot(aes(x = reorder(site, -ab.n), y = ab.n))+
 geom_bar(stat = 'identity')+
 ylim(0, 150)+
 xlab('Site')+
 ylab(bquote('Total count (abalone.10'*~min^-1*')'))+
 ggtitle(paste('Open ', plot.size.class))+
 theme_bw()+
 theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

block.13.plot <- grid.arrange(plot.closed, plot.open, nrow = 1)

# save plot
setwd('C:/CloudStor/Shared/DiveFisheries/Abalone/FISdata/FIS_TimedSwimSurveys2021/FIS_TimedSwimSurvey2021_Plots')
ggsave(filename = paste('TimedSwimSurvey_Block13_OpenvsClosed2021', '.pdf', sep = ''),
       plot = block.13.plot, units = 'mm', width = 190, height = 120)

ggsave(filename = paste('TimedSwimSurvey_Block13_OpenvsClosed2021', '.png', sep = ''),
       plot = block.13.plot, units = 'mm', width = 190, height = 120)

