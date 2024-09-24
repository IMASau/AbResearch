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
 library(fuzzyjoin)
 library(tidytext)
})

source("C:/GitCode/AbResearch/getLegend.r")
source("C:/GitCode/AbResearch/StandardError_Functions.r")

##---------------------------------------------------------------------------##
## 2. Set sample year and file paths ####

# identify sampling year of interest
samp.year <- 2024

samp.year.folder <- file.path(paste(sprintf('C:/Users/%s/Dropbox (UTAS Research)/DiveFisheries/Abalone/FISdata', 
                                            Sys.info()[["user"]])), paste('FIS_TimedSwimSurveys', samp.year, sep = ''))

ts.plots.folder <- file.path(paste(sprintf('C:/Users/%s/Dropbox (UTAS Research)/DiveFisheries/Abalone/Assessment/Figures/FIS', 
                                           Sys.info()[["user"]])), paste('FIS_TimedSwimSurveys', samp.year, '_Plots', sep = ''))
##---------------------------------------------------------------------------##
# 3. Load data ####

# Import final dataframes 
time.swim.dat.final <-
 readRDS(paste(samp.year.folder, '/time.swim.dat.final.RDS', sep = ''))

time.swim.dat.df.final <-
 readRDS(paste(samp.year.folder, '/time.swim.dat.df.final.RDS', sep = ''))

# Import metadata frame
time.swim.meta.dat.final <- readRDS(paste(samp.year.folder, '/time.swim.meta.dat.final.RDS', sep = ''))

##---------------------------------------------------------------------------##

## PLOT 1: Abundance Boxplot ####

# Average count of all legal and sub-legal abalone per 10 min by year for each 
# site within each block (i.e. the average count between paired divers for each site).

# Determine mean abalone abundance in each block, year and size class
ten.min.mean.year <- time.swim.dat.final %>% 
 filter(!subblockno %in% c('28B', '28C') & 
         !blockno %in% c('13', '14', '21', '29', '30') &
         !is.na(sizeclass_freq_10) &
         sampyear <= samp.year) %>%
 group_by(blockno, site, diver, sampyear, time.elapsed, legal.size) %>% 
 summarise(ab.n = sum(sizeclass_freq_10)) %>% 
 group_by(blockno, sampyear, legal.size) %>% 
 summarise(mean.ab.n = mean(ab.n),
           median.ab.n = median(ab.n)) 
# mutate(sampyear = factor(sampyear))

# Determine number of sites surveyed in each block, year and size class
time.swim.dat.n <- time.swim.dat.final %>% 
 filter(!subblockno %in% c('28B', '28C') & 
         !blockno %in% c('13', '14', '21', '29', '30') &
         sampyear <= samp.year) %>%
 group_by(sampyear, blockno, legal.size) %>% 
 summarise(n = n_distinct(site))

# Plot for sub-legal abundance
sub.legal.plot <- time.swim.dat.final %>% 
 filter(!subblockno %in% c('28B', '28C'),
        legal.size == '<140 mm' &
         !blockno %in% c('13', '14', '21', '29', '30') &
         sampyear <= samp.year) %>%
 # filter(midsize < 150) %>% 
 group_by(blockno, site, diver, sampyear) %>% 
 summarise(ab.n = sum(sizeclass_freq_10)) %>% 
 group_by(blockno, site, sampyear) %>% 
 summarise(mean.ab.n = mean(ab.n)) %>%  
 mutate(sampyear = factor(sampyear, levels = c('2020', '2021', '2022', '2023', '2024'))) %>%  
 ggplot(aes(x = blockno, y = mean.ab.n))+
 geom_boxplot(aes(fill = sampyear), position = position_dodge2(1, preserve = 'single'),
              outlier.colour = '#EE8866') +
 scale_fill_manual(values = c("#77AADD", "#BBCC33", "#DDDDDD", '#44BB99', '#EE8866'))+
 geom_point(data = ten.min.mean.year %>% filter(legal.size == '<140 mm'), aes(group = factor(sampyear, levels = c('2020', '2021', '2022', '2023', '2024'))), shape = 19,
            size = 2, colour = 'red', fill = 'red', position = position_dodge2(0.8, preserve = 'single'))+
 theme_bw()+
 ylab(bquote('Average count (abalone.10'*~min^-1*')'))+
 xlab('Blockno')+
 coord_cartesian(ylim = c(0, 150))+
 geom_text(data = time.swim.dat.n %>% filter(legal.size == '<140 mm'), aes(y = 150, label = n, colour = factor(sampyear, levels = c('2020', '2021', '2022', '2023', '2024'))), size = 3, 
           position = position_dodge2(0.8))+
 scale_colour_manual(values = c("#77AADD", "#BBCC33", "#DDDDDD", '#44BB99', '#EE8866'))+
 guides(size = 'legend', colour = 'none',
        fill = guide_legend(title = 'Year'))+
 ggtitle('Sub-legal <140 mm')+
 theme(plot.title = element_text(vjust = 0, hjust = 0))+
 theme(legend.position = 'none')

# Plot for legal abundance
legal.plot <- time.swim.dat.final %>% 
 filter(!subblockno %in% c('28B', '28C'),
        legal.size == '>140 mm' &
         !blockno %in% c('13', '14', '21', '29', '30') &
         sampyear <= samp.year) %>%
 # filter(midsize < 150) %>% 
 group_by(blockno, site, diver, sampyear) %>% 
 summarise(ab.n = sum(sizeclass_freq_10)) %>% 
 group_by(blockno, site, sampyear) %>% 
 summarise(mean.ab.n = mean(ab.n)) %>% 
 mutate(sampyear = factor(sampyear, levels = c('2020', '2021', '2022', '2023', '2024'))) %>%  
 ggplot(aes(x = blockno, y = mean.ab.n))+
 geom_boxplot(aes(fill = sampyear), position = position_dodge2(1, preserve = 'single'),
              outlier.colour = '#EE8866') +
 scale_fill_manual(values = c("#77AADD", "#BBCC33", "#DDDDDD", '#44BB99', '#EE8866'))+
 geom_point(data = ten.min.mean.year %>% filter(legal.size == '>140 mm'), aes(group = factor(sampyear, levels = c('2020', '2021', '2022', '2023', '2024'))), shape = 19,
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
## PLOT 2: Abundance Lineplot ####

act_dat <- time.swim.dat.final %>%
 mutate(samp_period = ifelse(between(sampdate, as.Date('2021-01-01'), as.Date('2021-03-31'))|
                              between(sampdate, as.Date('2023-01-01'), as.Date('2023-03-31')), 'Pre', 
                             ifelse(between(sampdate, as.Date('2023-04-01'), as.Date('2023-12-31')), 'Mid', 'Post')))

# Determine mean abalone abundance in each block, year and size class
ten.min.mean.year <- act_dat %>% 
 filter((!subblockno %in% c('28B', '28C') & 
          !blockno %in% c('13', '14', '21', '29', '30') &
          !is.na(sizeclass_freq_10) &
          sampyear <= samp.year) |
         (sampyear == 2021 & blockno == '13') |
         (sampyear == 2023 & blockno == '13' & samp_period == 'Mid')) %>%
 group_by(blockno, site, diver, sampyear, time.elapsed, legal.size) %>% 
 summarise(ab.n = sum(sizeclass_freq_10)) %>% 
 group_by(blockno, sampyear, legal.size) %>% 
 summarise(mean.ab.n = mean(ab.n),
           median.ab.n = median(ab.n),
           std_err = sd(ab.n)/sqrt(n())) 
# mutate(sampyear = factor(sampyear))

# Determine number of sites surveyed in each block, year and size class
time.swim.dat.n <- act_dat %>% 
 filter((!subblockno %in% c('28B', '28C') & 
          !blockno %in% c('13', '14', '21', '29', '30') &
          !is.na(sizeclass_freq_10) &
          sampyear <= samp.year) |
         (sampyear == 2021 & blockno == '13') |
         (sampyear == 2023 & blockno == '13' & samp_period == 'Mid')) %>%
 group_by(sampyear, blockno, legal.size) %>% 
 summarise(n = n_distinct(site))

abundance_plot <- ten.min.mean.year %>% 
 filter(blockno != 13) %>% 
 ggplot(aes(x = sampyear, y = mean.ab.n, group = legal.size, colour = legal.size))+
 geom_point(position = position_dodge(0.05))+
 geom_line()+
 geom_errorbar(aes(ymin = mean.ab.n -  std_err, ymax = mean.ab.n + std_err), width = 0.2,
               position = position_dodge(0.05))+
 scale_colour_manual(values = c('red', 'blue'))+
 theme_bw()+
 ylab(bquote('Average count (abalone.10'*~min^-1*')'))+
 xlab('Survey Year')+
 theme(legend.position = 'bottom',
       legend.background = element_rect(fill = "white", colour = NA))+
 guides(colour = guide_legend(title = "Size Class"))+
 facet_wrap(. ~ blockno, ncol = 3)

abundance_trend_plot <- ten.min.mean.year %>% 
 # filter(legal.size == '>140 mm') %>%
 filter(blockno != 13) %>% 
 ggplot(aes(x = sampyear, y = mean.ab.n, group = legal.size, colour = legal.size))+
 geom_point(position = position_dodge(0.05))+
 geom_smooth(method = 'lm', formula = y~x, se = T)+
 stat_poly_eq(formula = y~x, aes(label = paste(..rr.label.., p.value.label, sep = "~~~")), 
              parse = TRUE, label.y = 0.95) +
 scale_colour_manual(values = c('red', 'blue'))+
 theme_bw()+
 ylab(bquote('Average count (abalone.10'*~min^-1*')'))+
 xlab('Survey Year')+
 theme(legend.position = c(0.85, 0.35)
       ,legend.background = element_rect(fill = "white", colour = NA))+
 guides(colour = guide_legend(title = "Size Class"))+
 facet_wrap(. ~ blockno, ncol = 3)

setwd(ts.plots.folder)
ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_TenMinuteCount_LegalSubLegal_MeanLinePlot', '.pdf', sep = ''), 
       plot = abundance_plot, units = 'mm', width = 190, height = 200)
ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_TenMinuteCount_LegalSubLegal_MeanLinePlot', '.png', sep = ''), 
       plot = abundance_plot, units = 'mm', width = 190, height = 200)

# ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_TenMinuteCount_Legal_MeanTendPlot', '.pdf', sep = ''), 
#        plot = abundance_trend_plot, units = 'mm', width = 190, height = 200)
# ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_TenMinuteCount_Legal_MeanTendPlot', '.png', sep = ''), 
#        plot = abundance_trend_plot, units = 'mm', width = 190, height = 200)

##---------------------------------------------------------------------------##
## PLOT 3: Relative-Absolute Abundance ####

# Relative abundance to 2020 baselines and hypothetical percentage change.

# Determine mean abalone abundance in each block, year and size class
ten.min.mean.year <- time.swim.dat.final %>% 
 filter(!subblockno %in% c('28B', '28C') & 
         !blockno %in% c('13', '14', '21', '29', '30') &
         !is.na(sizeclass_freq_10)) %>% 
 group_by(blockno, site, diver, sampyear, time.elapsed, legal.size) %>% 
 summarise(ab.n = sum(sizeclass_freq_10)) %>% 
 group_by(blockno, sampyear, legal.size) %>% 
 summarise(mean.ab.n = mean(ab.n),
           median.ab.n = median(ab.n),
           std_err = sd(ab.n)/sqrt(n())) 

# Extract basline abudance 2020 values
base_dat_2020 <- ten.min.mean.year %>% 
 filter(sampyear == 2020) %>% 
 dplyr::rename(mean_ab_n_2020 = 'mean.ab.n') %>% 
 ungroup() %>%
 select(blockno, legal.size, mean_ab_n_2020)

# Re-join baseline data to all data
dat_base_year <- left_join(ten.min.mean.year, base_dat_2020)

# Relative and absolute difference
dat_diff <- dat_base_year %>% 
 mutate(rel_diff = (mean.ab.n - mean_ab_n_2020) / mean_ab_n_2020,
        abs_diff = mean.ab.n - mean_ab_n_2020)

# Create dataframe of hypothetical relative change scenarios
dat_hypo_rel <- data.frame(sampyear = c(2020, 2021, 2022, 2023, 2024),
                           hypo_05 = format(c(0, 0.05, 0.05 * 1.05, 0.05 * 1.05^2, 0.05 * 1.05^3), scientific = F),
                           hypo_10 = format(c(0, 0.10, 0.10 * 1.10, 0.10 * 1.10^2, 0.10 * 1.10^3), scientific = F),
                           hypo_15 = format(c(0, 0.15, 0.15 * 1.15, 0.15 * 1.15^2, 0.15 * 1.15^3), scientific = F),
                           hypo_20 = format(c(0, 0.20, 0.20 * 1.20, 0.20 * 1.20^2, 0.20 * 1.20^3), scientific = F),
                           hypo_25 = format(c(0, 0.25, 0.25 * 1.25, 0.25 * 1.25^2, 0.25 * 1.25^3), scientific = F)) %>% 
 pivot_longer(cols = starts_with('hypo_'),
              names_to = c('hypo', 'rate'),
              names_sep = '_',
              values_to = 'hypo_val',
              values_drop_na = T) %>% 
 select(sampyear, rate, hypo_val)

# Create dataframe of absolute change hypothetical scenarios.
dat_hypo_abs <- dat_base_year %>% 
 filter(sampyear == 2020) %>% 
 mutate(hypo_05_2020 = 0,
        hypo_05_2021 = (mean_ab_n_2020 * 1.05) - mean_ab_n_2020,
        hypo_05_2022 = (mean_ab_n_2020 * 1.05^2) - mean_ab_n_2020,
        hypo_05_2023 = (mean_ab_n_2020 * 1.05^3) - mean_ab_n_2020,
        hypo_05_2024 = (mean_ab_n_2020 * 1.05^4) - mean_ab_n_2020,
        hypo_10_2020 = 0,
        hypo_10_2021 = (mean_ab_n_2020 * 1.10) - mean_ab_n_2020,
        hypo_10_2022 = (mean_ab_n_2020 * 1.10^2) - mean_ab_n_2020,
        hypo_10_2023 = (mean_ab_n_2020 * 1.10^3) - mean_ab_n_2020,
        hypo_10_2024 = (mean_ab_n_2020 * 1.10^4) - mean_ab_n_2020,
        hypo_15_2020 = 0,
        hypo_15_2021 = (mean_ab_n_2020 * 1.15) - mean_ab_n_2020,
        hypo_15_2022 = (mean_ab_n_2020 * 1.15^2) - mean_ab_n_2020,
        hypo_15_2023 = (mean_ab_n_2020 * 1.15^3) - mean_ab_n_2020,
        hypo_15_2024 = (mean_ab_n_2020 * 1.15^4) - mean_ab_n_2020,
        hypo_20_2020 = 0,
        hypo_20_2021 = (mean_ab_n_2020 * 1.20) - mean_ab_n_2020,
        hypo_20_2022 = (mean_ab_n_2020 * 1.20^2) - mean_ab_n_2020,
        hypo_20_2023 = (mean_ab_n_2020 * 1.20^3) - mean_ab_n_2020,
        hypo_20_2024 = (mean_ab_n_2020 * 1.20^4) - mean_ab_n_2020,
        hypo_25_2020 = 0,
        hypo_25_2021 = (mean_ab_n_2020 * 1.25) - mean_ab_n_2020,
        hypo_25_2022 = (mean_ab_n_2020 * 1.25^2) - mean_ab_n_2020,
        hypo_25_2023 = (mean_ab_n_2020 * 1.25^3) - mean_ab_n_2020,
        hypo_25_2024 = (mean_ab_n_2020 * 1.25^4) - mean_ab_n_2020) %>% 
 pivot_longer(cols = starts_with('hypo_'),
              names_to = c('hypo', 'rate', 'yr'),
              names_sep = '_',
              values_to = 'hypo_val',
              values_drop_na = T) %>% 
 select(hypo, legal.size, rate, yr, hypo_val)

#Relative change plot
rel_change_plot <- dat_diff %>%
 ggplot()+
 geom_point(aes(x = sampyear, y = rel_diff, group = legal.size, colour = legal.size), position = position_dodge(0.05))+
 geom_line(aes(x = sampyear, y = rel_diff, group = legal.size, colour = legal.size))+
 geom_hline(yintercept = 0, linetype = 'dotted', colour = 'red', size = 0.3)+
 geom_line(data = dat_hypo_rel %>% filter(rate == '05'), aes(x = as.numeric(sampyear), y = as.numeric(hypo_val), colour = rate), linetype = 'dashed', size = 0.3)+
 geom_line(data = dat_hypo_rel %>% filter(rate == '10'), aes(x = as.numeric(sampyear), y = as.numeric(hypo_val), colour = rate), linetype = 'dashed', size = 0.3)+
 geom_line(data = dat_hypo_rel %>% filter(rate == '15'), aes(x = as.numeric(sampyear), y = as.numeric(hypo_val), colour = rate), linetype = 'dashed', size = 0.3)+
 geom_line(data = dat_hypo_rel %>% filter(rate == '20'), aes(x = as.numeric(sampyear), y = as.numeric(hypo_val), colour = rate), linetype = 'dashed', size = 0.3)+
 scale_colour_manual(values = c('red', 'blue', "#00AFBB", "#E7B800", "#FC4E07", "#52854C"),
                     labels = c('<140 mm', '>140 mm', '5%', '10%', '15%', '20%'),
                     name = '')+ 
 theme_bw()+
 ylab(bquote('Relative change in abundance'))+
 xlab('Survey Year')+
 theme(legend.position = 'bottom')+
 # theme(legend.position = c(0.9, 0.3)
 #       ,legend.background = element_rect(fill = NA, colour = NA))+
 guides(colour = guide_legend(nrow =1))+
 # ylim(-1, 1)+
 facet_wrap(. ~ blockno, ncol = 3)

#Absolute change plot
abs_change_plot <- dat_diff %>%
 ggplot()+
 geom_point(aes(x = sampyear, y = abs_diff, group = legal.size, colour = legal.size), position = position_dodge(0.05))+
 geom_line(aes(x = sampyear, y = abs_diff, group = legal.size, colour = legal.size))+
 geom_hline(yintercept = 0, linetype = 'dotted', colour = 'red', size = 0.3)+
 geom_line(data = dat_hypo_abs %>% filter(rate == '05' & legal.size == '<140 mm'), aes(x = as.numeric(yr), y = hypo_val, colour = rate), linetype = 'dashed', size = 0.3)+
 geom_line(data = dat_hypo_abs %>% filter(rate == '10' & legal.size == '<140 mm'), aes(x = as.numeric(yr), y = hypo_val, colour = rate), linetype = 'dashed', size = 0.3)+
 geom_line(data = dat_hypo_abs %>% filter(rate == '15' & legal.size == '<140 mm'), aes(x = as.numeric(yr), y = hypo_val, colour = rate), linetype = 'dashed', size = 0.3)+
 geom_line(data = dat_hypo_abs %>% filter(rate == '20' & legal.size == '<140 mm'), aes(x = as.numeric(yr), y = hypo_val, colour = rate), linetype = 'dashed', size = 0.3)+
 scale_colour_manual(values = c('red', 'blue', "#00AFBB", "#E7B800", "#FC4E07", "#52854C"),
                     labels = c('<140 mm', '>140 mm', '5%', '10%', '15%', '20%'),
                     name = '')+
 # scale_colour_manual(values = c('red', 'blue'),
 #                     labels = c('<140 mm', '>140 mm'),
 #                     name = 'Size Class')+
 theme_bw()+
 ylab(bquote('Absolute change in abundance'))+
 xlab('Survey Year')+
 theme(legend.position = 'bottom')+
 # theme(legend.position = c(0.9, 0.3)
 #       ,legend.background = element_rect(fill = NA, colour = NA))+
 guides(colour = guide_legend(nrow =1))+
 # ylim(-30, 100)+
 facet_wrap(. ~ blockno, ncol = 3)

setwd(ts.plots.folder)
ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_TenMinuteCount_RelativeChangePlot', '.pdf', sep = ''), 
       plot = rel_change_plot, units = 'mm', width = 190, height = 200)
ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_TenMinuteCount_RelativeChangePlot', '.png', sep = ''), 
       plot = rel_change_plot, units = 'mm', width = 190, height = 200)

ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_TenMinuteCount_AbsoluteChangePlot', '.pdf', sep = ''), 
       plot = abs_change_plot, units = 'mm', width = 190, height = 200)
ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_TenMinuteCount_AbsoluteChangePlot', '.png', sep = ''), 
       plot = abs_change_plot, units = 'mm', width = 190, height = 200)

##---------------------------------------------------------------------------##
## PLOT 4: Reference Abundance ####

# Reference site relative abundance to 2020 baselines

# Determine mean abalone abundance in each block, year and size class
ten.min.mean.year <- time.swim.dat.final %>% 
 filter(!subblockno %in% c('28B', '28C') & 
         !blockno %in% c('13', '14', '21', '29', '30') &
         !is.na(sizeclass_freq_10) &
         ref_site == 1) %>% 
 group_by(blockno, site, diver, sampyear, time.elapsed, legal.size) %>% 
 summarise(ab.n = sum(sizeclass_freq_10)) %>% 
 group_by(blockno, sampyear, legal.size) %>% 
 summarise(mean.ab.n = mean(ab.n),
           median.ab.n = median(ab.n),
           std_err = sd(ab.n)/sqrt(n())) 

abundance_plot <- ten.min.mean.year %>% 
 filter(blockno != 13) %>% 
 ggplot(aes(x = sampyear, y = mean.ab.n, group = legal.size, colour = legal.size))+
 geom_point(position = position_dodge(0.05))+
 geom_line()+
 geom_errorbar(aes(ymin = mean.ab.n -  std_err, ymax = mean.ab.n + std_err), width = 0.2,
               position = position_dodge(0.05))+
 scale_colour_manual(values = c('red', 'blue'))+
 theme_bw()+
 ylab(bquote('Average count (abalone.10'*~min^-1*')'))+
 xlab('Survey Year')+
 theme(legend.position = 'bottom',
       legend.background = element_rect(fill = "white", colour = NA))+
 guides(colour = guide_legend(title = "Size Class"))+
 facet_wrap(. ~ blockno, ncol = 3)

# Extract basline 2020 values
base_dat_2020 <- ten.min.mean.year %>% 
 filter(sampyear == 2020) %>% 
 dplyr::rename(mean_ab_n_2020 = 'mean.ab.n') %>% 
 ungroup() %>%
 select(blockno, legal.size, mean_ab_n_2020)

# Re-join baseline data to all data
dat_base_year <- left_join(ten.min.mean.year, base_dat_2020)

# Relative difference
dat_diff <- dat_base_year %>% 
 mutate(rel_diff = (mean.ab.n - mean_ab_n_2020) / mean_ab_n_2020,
        abs_diff = abs(mean.ab.n - mean_ab_n_2020))

#Relative plot
rel_change_plot <- dat_diff %>%
 ggplot(aes(x = sampyear, y = rel_diff, group = legal.size, colour = legal.size))+
 geom_point(position = position_dodge(0.05))+
 geom_line()+
 scale_colour_manual(values = c('red', 'blue'))+
 geom_hline(yintercept = 0, linetype = 'dashed', colour = 'red', size = 0.3)+
 theme_bw()+
 ylab(bquote('Relative change in abundance'))+
 xlab('Survey Year')+
 theme(legend.position = 'bottom')+
 # theme(legend.position = c(0.9, 0.3)
 #       ,legend.background = element_rect(fill = NA, colour = NA))+
 guides(colour = guide_legend(title = "Size Class"))+
 ylim(-1, 4)+
 facet_wrap(. ~ blockno, ncol = 3)

setwd(ts.plots.folder)
ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_TenMinuteCount_RelativeChangePlot_ReferenceSites', '.pdf', sep = ''),
       plot = rel_change_plot, units = 'mm', width = 190, height = 200)
ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_TenMinuteCount_RelativeChangePlot_ReferenceSites', '.png', sep = ''),
       plot = rel_change_plot, units = 'mm', width = 190, height = 200)

ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_TenMinuteCount_LegalSubLegal_MeanLinePlot_ReferenceSites', '.pdf', sep = ''),
       plot = abundance_plot, units = 'mm', width = 190, height = 200)
ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_TenMinuteCount_LegalSubLegal_MeanLinePlot_ReferenceSites', '.png', sep = ''),
       plot = abundance_plot, units = 'mm', width = 190, height = 200)

##---------------------------------------------------------------------------##
## PLOT 5: Reference Abundance Criteria ####

# Relative change in abundance at referenc sites between 2020 and proceeding years
# including a colour coding trend to determine if the majority (75%) of sites are
# showing signs of improvement relative to 2020 (e.g. abundance increasing)
# Criteria include:
# 1. Green = two years of consecutive increases above 2020 baseline (current + previous year)
# 2. Red   = two consecutive years of decline (current + previous year) 
#          = current year decline but previous year increase or no change
# 3. Amber = current year increase or no change but previous year decline

# Determine mean abalone abundance in each block, year and size class
site_mean_ref <- time.swim.dat.final %>% 
 filter(!subblockno %in% c('28B', '28C') & 
         !blockno %in% c('13', '14', '21', '29', '30') &
         !is.na(sizeclass_freq_10) &
         ref_site == 1) %>% 
 group_by(blockno, site, diver, sampyear, time.elapsed, legal.size) %>% 
 summarise(ab.n = sum(sizeclass_freq_10)) %>% 
 group_by(blockno, site, sampyear, legal.size) %>% 
 summarise(mean.ab.n = mean(ab.n))

# Create long dataframe
site_mean_ref_long <- site_mean_ref %>% 
 spread(key = sampyear, value = mean.ab.n)

# Classify abundance trends
site_mean_dat <- site_mean_ref_long %>% 
 mutate(rel_2021 = ((`2021` -`2020`)/ `2020`),
        rel_2022 = ((`2022` -`2020`)/ `2020`),
        rel_2023 = ((`2023` -`2020`)/ `2020`),
        rel_2024 = ((`2024` -`2020`)/ `2020`),
        plot_col = case_when(rel_2023 < 0 & rel_2024 < 0 ~ 'red',
                             rel_2023 >= 0 & rel_2024 < 0 ~ 'red',
                             rel_2023 <= 0 & rel_2024 >= 0 ~ 'orange',
                             rel_2023 >= 0 & rel_2024 >= 0 ~ 'darkgreen',
                             is.na(rel_2023) | rel_2023 == 0 & rel_2024 >= 0 ~ 'orange',
                             is.na(rel_2023) | rel_2023 == 0 & rel_2024 < 0 ~ 'red'))

# Determine overall if 75% of sites within block meet improving criteria (i.e. green)
block_crit_dat <- site_mean_dat %>% 
 group_by(blockno, legal.size, plot_col) %>%
 summarise(n = n()) %>% 
 ungroup() %>% 
 complete(blockno, legal.size, plot_col, fill = list(n = 0)) %>% 
 filter(plot_col == 'darkgreen') %>% 
 mutate(status_val = n/15,
        status_col = case_when(status_val >= 0.75 ~ 'darkgreen',
                               status_val >= 0.5 & status_val < 0.75 ~ 'orange',
                               status_val < 0.5 ~ 'red'),
        status_condition = case_when(status_val >= 0.75 ~ 'PASS',
                                     status_val >= 0.5 & status_val < 0.75 ~ 'ASSESS',
                                     status_val < 0.5 ~ 'FAIL'),
        x = 12, y = -0.8)


ref_rel_plot <- site_mean_dat %>% 
 filter(legal.size == '<140 mm' &
         !is.na(rel_2023) & !is.na(rel_2024) &
         !is.na(plot_col)) %>% 
 ggplot()+
 geom_bar(aes(x = site, y = rel_2023, group = blockno, fill = plot_col), stat='identity')+
 scale_fill_manual(values = c('red' = 'red',
                              'orange' = 'orange',
                              'darkgreen' = 'darkgreen'),
                   labels = c('2 yr Increase',
                              '1 yr Increase',
                              'Decline'))+
 # geom_rect(data = df_4 %>% filter(legal.size == '>140 mm'), aes(fill = status_col),xmin = -Inf, xmax = Inf,
 #           ymin = -Inf, ymax = Inf, alpha = 0.3) +
 facet_wrap(~blockno, scales = 'free', drop = F)+
 theme_bw()+
 coord_cartesian(ylim = c(-1, 1))+
 theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
 theme(legend.position = 'bottom',
       legend.title = element_blank())+
 xlab('Site')+
 ylab(bquote('Relative change in abundance'))
# geom_point(data = df_4 %>% filter(legal.size == '>140 mm'), aes(x = x, y = y))+
# geom_text(data = df_4 %>% filter(legal.size == '>140 mm'), aes(x = x, y = y,
# label = status_condition))

setwd(ts.plots.folder)
ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_TenMinuteCount_Sub-Legal_RelativeChangePlot_ReferenceSites_2023_2024', '.pdf', sep = ''), 
       plot = ref_rel_plot, units = 'mm', width = 190, height = 200)
ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_TenMinuteCount_Sub-Legal_RelativeChangePlot_ReferenceSites_2023_2024', '.png', sep = ''), 
       plot = ref_rel_plot, units = 'mm', width = 190, height = 200)
##---------------------------------------------------------------------------##