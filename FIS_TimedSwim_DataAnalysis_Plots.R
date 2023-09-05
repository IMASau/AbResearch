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
# 10. Data Analysis ####

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
## 11. Standardise data #### 

# Standardise counts for 10 minute swim (i.e. some swims marginally shorter or longer duration)
std.ts.dat <- time.swim.dat.final %>% 
 mutate(sizeclass_freq_10 = round((sizeclass_freq / time.elapsed) * 10))

saveRDS(std.ts.dat, paste(samp.year.folder, '/std.ts.dat.RDS', sep = ''))

# Summarise total count for blockno x site x sampyear x legal.size
ts.count.sum <- std.ts.dat %>% 
 filter(!subblockno %in% c('28B', '28C')) %>%
 group_by(blockno, site, sampyear, legal.size) %>% 
 summarise(ab.n = sum(sizeclass_freq_10)) %>%  
 group_by(blockno, site, sampyear, legal.size) %>% 
 group_by(site)

ts.av.count <- std.ts.dat %>% 
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
std.ts.dat %>% 
 filter(!subblockno %in% c('28B', '28C')) %>%
 group_by(blockno, sampyear) %>% 
 summarise(n = n_distinct(sampdate),
           sites = n_distinct(site)) %>% 
 pivot_wider(id_cols = c(blockno),
             names_from = c(sampyear),
             values_from = c('n', 'sites')) %>% 
 adorn_totals()
##---------------------------------------------------------------------------##
## PLOT 1: COUNT PER TEN MIN YEARS ####
## average count of all legal and sub-legal abalone per 10 min by year for each site within each block 
## (i.e. the average count between paired divers for each site)

# determine mean abalone abundance for block x sampyear x size class
ten.min.mean.year <- std.ts.dat %>% 
 filter(!subblockno %in% c('28B', '28C') & 
         !blockno %in% c('13', '14', '29', '30') &
         !is.na(sizeclass_freq_10)) %>%
 group_by(blockno, site, diver, sampyear, time.elapsed, legal.size) %>% 
 summarise(ab.n = sum(sizeclass_freq_10)) %>% 
 group_by(blockno, sampyear, legal.size) %>% 
 summarise(mean.ab.n = mean(ab.n),
           median.ab.n = median(ab.n)) 
 mutate(sampyear = factor(sampyear))

df.1 <- ten.min.mean.year %>% 
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
ggpubr::ggtexttable(rows = NULL, theme = ggpubr::ttheme('mOrange'))

df_2 <- std.ts.dat %>% 
 filter(!subblockno %in% c('28B', '28C') & 
         !blockno %in% c('13', '14', '29', '30') &
         !is.na(sizeclass_freq_10) &
         sampyear == samp.year) %>%
 group_by(blockno) %>% 
 summarise(Sites = n_distinct(site))

df_3 <- left_join(df_2, df.1, by = c('blockno' = 'BlockNo')) %>%
 dplyr::rename('BlockNo' = 'blockno')

df_4 <- df_3 %>% 
 ggpubr::ggtexttable(rows = NULL, theme = ggpubr::ttheme('mOrange'))

setwd(ts.plots.folder)
write.xlsx(df_3, paste('TimedSwimSurvey_', samp.year-1, 'vs', samp.year, '_PercentChange.xlsx'), sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)
ggsave(filename = paste('TimedSwimSurvey_',  samp.year-1, 'vs', samp.year, '_PercentChange', '.pdf', sep = ''), 
       plot = df_4, units = 'mm', width = 190, height = 120)
ggsave(filename = paste('TimedSwimSurvey_',  samp.year-1, 'vs', samp.year, '_PercentChange', '.png', sep = ''), 
       plot = df_4, units = 'mm', width = 190, height = 120)


time.swim.dat.n <- std.ts.dat %>% 
 filter(!subblockno %in% c('28B', '28C') & 
         !blockno %in% c('13', '14', '29', '30')) %>%
 group_by(sampyear, blockno, legal.size) %>% 
 summarise(n = n_distinct(site))

sub.legal.plot <- std.ts.dat %>% 
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
 scale_fill_manual(values = c("#77AADD", "#BBCC33", "#DDDDDD", '#EEDD88'))+
 geom_point(data = ten.min.mean.year %>% filter(legal.size == '<140 mm'), aes(group = factor(sampyear, levels = c('2020', '2021', '2022'))), shape = 19,
            size = 2, colour = 'red', fill = 'red', position = position_dodge2(0.8, preserve = 'single'))+
 theme_bw()+
 ylab(bquote('Average count (abalone.10'*~min^-1*')'))+
 xlab('Blockno')+
 ylim(0, 150)+
 geom_text(data = time.swim.dat.n %>% filter(legal.size == '<140 mm'), aes(y = 150, label = n, colour = factor(sampyear, levels = c('2020', '2021', '2022', '2023'))), size = 3, 
           position = position_dodge2(0.8))+
 scale_colour_manual(values = c("#77AADD", "#BBCC33", "#DDDDDD", '#EEDD88'))+
 guides(size = 'legend', colour = 'none',
        fill = guide_legend(title = 'Year'))+
 ggtitle('Sub-legal <140 mm')+
 theme(plot.title = element_text(vjust = 0, hjust = 0))+
 theme(legend.position = 'none')

legal.plot <- std.ts.dat %>% 
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
 scale_fill_manual(values = c("#77AADD", "#BBCC33", "#DDDDDD", '#EEDD88'))+
 geom_point(data = ten.min.mean.year %>% filter(legal.size == '>140 mm'), aes(group = factor(sampyear, levels = c('2020', '2021', '2022', '2023'))), shape = 19,
            size = 2, colour = 'red', fill = 'red', position = position_dodge2(0.8, preserve = 'single'))+
 theme_bw()+
 ylab(bquote('Average count (abalone.10'*~min^-1*')'))+
 xlab('Blockno')+
 ylim(0, 150)+
 guides(size = 'legend', colour = 'none',
        fill = guide_legend(title = 'Year'))+
 ggtitle('Legal >140 mm')+
 theme(plot.title = element_text(vjust = 0, hjust = 0))+
 theme(legend.title = element_blank(),
       legend.position = c(0.9, 0.7))

count.plot.sizeclass <- grid.arrange(sub.legal.plot, legal.plot, nrow = 2)

setwd(ts.plots.folder)
ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_TenMinuteCount_LegalSubLegal', '.pdf', sep = ''), 
       plot = count.plot.sizeclass, units = 'mm', width = 190, height = 200)
ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_TenMinuteCount_LegalSubLegal', '.png', sep = ''), 
       plot = count.plot.sizeclass, units = 'mm', width = 190, height = 200)

##---------------------------------------------------------------------------##
# Distance travelled per year 

df.1 <- std.ts.dat %>% 
 filter(!subblockno %in% c('28B', '28C') & 
         !blockno %in% c('13', '14', '29', '30')) %>%
 group_by(blockno, sampyear, legal.size) %>% 
 summarise(ab.n = sum(sizeclass_freq_10),
           mean.dist = mean(distance),
           ab.m2 = ab.n / mean.dist)

df.1 <- std.ts.dat %>% 
 filter(!subblockno %in% c('28B', '28C') & 
         !blockno %in% c('13', '14', '29', '30') &
         distance > 0) %>%
 group_by(blockno, site, diver, sampyear, time.elapsed, legal.size, distance) %>% 
 summarise(ab.n = sum(sizeclass_freq_10)) %>% 
 ungroup() %>% 
 mutate(ab.m2 = ab.n / distance) 

group_by(blockno, sampyear, legal.size) %>% 
 summarise(mean.ab.m2 = mean(ab.m2))


df.2 <- summarySE(df.1, measurevar = 'ab.m2', 
                  groupvars = c('blockno', 'sampyear', 'legal.size'),
                  na.rm = T)

pd <- position_dodge(0.1)

df.2 %>%
 ggplot(aes(x = factor(sampyear), y = ab.m2, colour = blockno, group = blockno)) + 
 geom_errorbar(aes(ymin = ab.m2 - se, ymax = ab.m2 + se), width = 0.1, position = pd) +
 geom_line(position = pd) +
 geom_point(position = pd)+
 facet_wrap(~ legal.size)

##---------------------------------------------------------------------------##
## PLOT 2: REPEAT COUNT PER TEN MIN YEARS ####
# compare counts at repeat sites between 2020 and 2021

count.plot.rep.dat <- std.ts.dat %>% 
 filter(!subblockno %in% c('28B', '28C') & 
         !blockno %in% c('13', '14', '29', '30')) %>%
 group_by(blockno, site, diver, sampyear, legal.size) %>% 
 summarise(ab.n = sum(sizeclass_freq)) %>% 
 group_by(blockno, site, sampyear, legal.size) %>% 
 summarise(mean.ab.n = mean(ab.n)) %>% 
 group_by(site) %>% 
 filter(n() > 2)

ten.min.mean.rep.year.sites <- count.plot.rep.dat %>% 
 group_by(blockno, sampyear, legal.size) %>% 
 summarise(n = n_distinct(site))

count.plot.rep.mean <- std.ts.dat %>% 
 filter(!subblockno %in% c('28B', '28C') & 
         !blockno %in% c(13, 14, 29, 30)) %>%
 group_by(blockno, site, diver, sampyear, legal.size) %>% 
 summarise(ab.n = sum(sizeclass_freq)) %>% 
 group_by(blockno, sampyear, legal.size) %>%
 filter(n() > 1) %>% 
 summarise(mean.ab.n = mean(ab.n))

sub.legal.plot.rep <- count.plot.rep.dat %>% 
 filter(legal.size == '<140 mm') %>%
 mutate(sampyear = factor(sampyear, levels = c('2020', '2021', '2022'))) %>%  
 ggplot(aes(x = blockno, y = mean.ab.n))+
 geom_boxplot(aes(fill = sampyear), position = position_dodge2(1, preserve = 'single'),
              outlier.colour = '#EE8866') +
 scale_fill_manual(values = c("#77AADD", "#BBCC33", "#DDDDDD"))+
 geom_point(data = count.plot.rep.mean %>% filter(legal.size == '<140 mm'), aes(group = factor(sampyear, levels = c('2020', '2021', '2022'))), shape = 19,
            size = 2, colour = 'red', fill = 'red', position = position_dodge2(0.8))+
 theme_bw()+
 ylab(bquote('Average count (abalone.10'*~min^-1*')'))+
 xlab('Blockno')+
 ylim(0, 100)+
 geom_text(data = ten.min.mean.rep.year.sites %>% filter(legal.size == '<140 mm'), aes(y = 100, label = n, colour = factor(sampyear, levels = c('2020', '2021', '2022'))), size = 3, 
           position = position_dodge2(0.8))+
 scale_colour_manual(values = c("#77AADD", "#BBCC33", "#DDDDDD"))+
 guides(size = 'legend', colour = 'none',
        fill = guide_legend(title = 'Year'))+
 ggtitle('Sub-legal <140 mm Repeat Sites')+
 theme(plot.title = element_text(vjust = 0, hjust = 0))+
 theme(legend.position = 'none')

legal.plot.rep <- count.plot.rep.dat %>% 
 filter(legal.size == '>140 mm') %>%
 mutate(sampyear = factor(sampyear, levels = c('2020', '2021', '2022'))) %>%  
 ggplot(aes(x = blockno, y = mean.ab.n))+
 geom_boxplot(aes(fill = sampyear), position = position_dodge2(1, preserve = 'single'),
              outlier.colour = '#EE8866') +
 scale_fill_manual(values = c("#77AADD", "#BBCC33", "#DDDDDD"))+
 geom_point(data = count.plot.rep.mean %>% filter(legal.size == '>140 mm'), 
            aes(group = factor(sampyear, levels = c('2020', '2021', '2022'))), shape = 19,
            size = 2, colour = 'red', fill = 'red', position = position_dodge2(0.8))+
 theme_bw()+
 ylab(bquote('Average count (abalone.10'*~min^-1*')'))+
 xlab('Blockno')+
 ylim(0, 100)+
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
##---------------------------------------------------------------------------##
## PLOT 3: COUNT PER TEN MIN ####
## plot showing average count of all legal and sub-legal abalone per 10 min for
## each site within each blockno (i.e. the average between paired divers for each site)

# determine number of sites sampled for each blockno
time.swim.dat.n <- time.swim.dat.final %>%
 filter(!subblockno %in% c('28B', '28C'),
        sampdate > as.Date('2022-01-01')) %>%
 group_by(blockno) %>%
 summarise(n = n_distinct(site))

## average count per 10 min for each blockno
count.plot <- std.ts.dat %>%
 filter(!subblockno %in% c('28B', '28C'),
        sampdate > as.Date('2022-01-01')) %>%
 # filter(midsize >= 150) %>%
 group_by(blockno, site, diver) %>%
 summarise(ab.n = sum(sizeclass_freq_10)) %>%
 group_by(blockno, site, diver) %>%
 summarise(mean.ab.n = mean(ab.n)) %>%
 ggplot(aes(x = blockno, y = mean.ab.n, colour = diver)) +
 geom_point(size = 3) +
 scale_colour_grey() +
 stat_summary(
  fun.y = mean,
  geom = 'point',
  shape = 19,
  size = 4,
  colour = 'red',
  fill = 'red'
 ) +
 theme_bw() +
 ylab(bquote('Average count (abalone.10' *  ~ min ^ -1 * ')')) +
 xlab('Blockno') +
 ylim(0, 250) +
 geom_text(data = time.swim.dat.n,
           aes(y = 250, label = n),
           color = 'black',
           size = 3) +
 theme(legend.position = 'none')

setwd(ts.plots.folder)

ggsave(
 filename = paste('TimedSwimSurvey_', samp.year, '_TenMinuteCountPlot', '.pdf', sep = ''),
 plot = count.plot,
 units = 'mm',
 width = 190,
 height = 120
)
ggsave(
 filename = paste('TimedSwimSurvey_', samp.year, '_TenMinuteCountPlot', '.png', sep = ''),
 plot = count.plot,
 units = 'mm',
 width = 190,
 height = 120
)

##---------------------------------------------------------------------------##
## PLOT 4: COUNT PER TEN MIN SIZECLASS ####
## average count of all legal and sub-legal abalone per 10 min by sizeclass for each site within each blockno 
## (i.e. the average between paired divers for each site)

# determine mean count per 10 min by size class for each blockno
ten.min.mean <- std.ts.dat %>% 
 filter(!subblockno %in% c('28B', '28C'),
        sampdate > as.Date('2022-01-01')) %>% 
 # filter(midsize < 150) %>% 
 group_by(blockno, site, diver, legal.size) %>% 
 summarise(ab.n = sum(sizeclass_freq)) %>% 
 group_by(blockno, legal.size) %>% 
 summarise(mean.ab.n = mean(ab.n)) %>% 
 mutate(legal.size = factor(legal.size))

ten.min.mean.site <- std.ts.dat %>% 
 filter(!subblockno %in% c('28B', '28C'),
        sampdate > as.Date('2022-01-01')) %>% 
 # filter(midsize < 150) %>% 
 group_by(site, blockno, subblockno, diver, legal.size) %>% 
 summarise(ab.n = sum(sizeclass_freq)) %>% 
 group_by(site, blockno, subblockno, legal.size) %>% 
 summarise(mean.ab.n = mean(ab.n)) %>% 
 mutate(legal.size = factor(legal.size))%>% 
 as.data.frame()

count.plot.sizeclass <- std.ts.dat %>% 
 filter(!subblockno %in% c('28B', '28C'),
        sampdate > as.Date('2022-01-01')) %>% 
 # filter(midsize < 150) %>% 
 group_by(blockno, site, diver, legal.size) %>% 
 summarise(ab.n = sum(sizeclass_freq)) %>% 
 group_by(blockno, site, legal.size) %>% 
 summarise(mean.ab.n = mean(ab.n)) %>% 
 mutate(legal.size = factor(legal.size)) %>% 
 ggplot(aes(x = blockno, y = mean.ab.n))+
 # geom_boxplot()+
 geom_boxplot(aes(fill = legal.size), position = position_dodge(0.9), outlier.colour = '#EE8866')+
 scale_fill_manual(values = c("#999999", "#56B4E9"))+
 # stat_summary(fun.y = mean, geom = 'point', shape = 19, size = 4, colour = 'red', fill = 'red')+
 geom_point(data = ten.min.mean, aes(group = legal.size), shape = 19, size = 2, colour = 'red', fill = 'red', position = position_dodge(0.9))+
 theme_bw()+
 ylab(bquote('Average count (abalone.10'*~min^-1*')'))+
 xlab('Blockno')+
 ylim(0, 125)+
 geom_text(data = time.swim.dat.n, aes(y = 125, label = n), color = 'black', size = 3)+
 theme(legend.position="none")

setwd(ts.plots.folder)
ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_TenMinuteCountSizeClassPlot', '.pdf', sep = ''), 
       plot = count.plot.sizeclass, units = 'mm', width = 190, height = 120)
ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_TenMinuteCountSizeClassPlot', '.png', sep = ''), 
       plot = count.plot.sizeclass, units = 'mm', width = 190, height = 120)
##---------------------------------------------------------------------------##
# PLOT 5: Counts x year x repeat sites ####
# Compare total counts of size class between years for repeat site

# Select repeat sites between years
ts.rep.dat <- ts.count.sum %>% 
 filter(n() > 2)

# Identify repeat blocks
ts.rep.blocks <- ts.rep.dat %>% 
 group_by(blockno) %>% 
 distinct(blockno) %>% 
 pull()

# Generate plot
for (i in ts.rep.blocks){
 
 rep.plot <- ts.rep.dat %>% 
  filter(blockno == i) %>%
  group_by(sampyear, site) %>%
  ggplot(aes(reorder_within(site, -ab.n, sampyear, fun = 'sum'), 
             y = ab.n, fill = legal.size))+
  geom_bar(stat = 'identity', position = 'stack')+
  ylim(0, 400)+
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
 ggsave(filename = paste('TimedSwimSurvey_RepeatSites_', 'BlockNo', i, 
                         '_TotalCount_', min(ts.rep.dat$sampyear), '-',
                         max(ts.rep.dat$sampyear), '.pdf', sep = ''),
        plot = rep.plot, units = 'mm', width = 190, height = 120)
 
 ggsave(filename = paste('TimedSwimSurvey_RepeatSites_', 'BlockNo', i, 
                         '_TotalCount_', min(ts.rep.dat$sampyear), '-',
                         max(ts.rep.dat$sampyear), '.png', sep = ''),
        plot = rep.plot, units = 'mm', width = 190, height = 120)
 
}

##---------------------------------------------------------------------------##
## PLOT 6: Counts x year x top ten sites ####
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
  ylim(0, 400)+
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

##---------------------------------------------------------------------------##
## PLOT 7: LF x block ####
## Length frequency plot by block (mid-points) for sampling year

# determine number of abalone recorded and number of sites sampled per block
block.ab.n <- time.swim.dat.df.final %>% 
 filter(!subblockno %in% c('28B', '28C') &
         sampyear == samp.year &
         !blockno == '13') %>% 
 # sampdate > as.Date('2021-01-01')) %>% 
 group_by(blockno) %>% 
 summarise(ab.n = paste('n = ', n()))

block.site.n <- time.swim.dat.final %>% 
 filter(!subblockno %in% c('28B', '28C') &
         sampyear == samp.year &
         !blockno == '13') %>% 
 # sampdate > as.Date('2021-01-01')) %>% 
 group_by(blockno) %>% 
 summarise(site.n = paste('(', n_distinct(site), ')', sep = ''))

block.ab.site.n <- left_join(block.ab.n, block.site.n) %>% 
 mutate(n = paste(ab.n, site.n, sep = ' '))

# create length frequency plot
lf.plot <- time.swim.dat.df.final %>% 
 filter(!subblockno %in% c('28B', '28C')&
         sampyear == samp.year &
         !blockno == '13') %>% 
 # sampdate > as.Date('2021-01-01')) %>% 
 ggplot(aes(shelllength.2021, group = blockno)) +
 geom_bar(aes(y = ..prop.., stat = 'count'), width = 20, col = 'black', fill = '#EFC000FF')+
 # scale_x_binned()+
 geom_vline(aes(xintercept = ifelse(blockno %in% c('27', '28'), 145, 138)), linetype = 'dashed', colour = 'red', size = 0.5) +
 theme_bw() +
 facet_grid(blockno ~ .) +
 scale_y_continuous(breaks = seq(0, 0.4, 0.1), labels = seq(0, 40, 10))+
 xlab("Shell Length (mm)")+
 ylab(paste("Percentage (%)"))+
 geom_text(data = block.ab.site.n, aes(x = 180, y = 0.2, label = n), color = 'black', size = 3)


setwd(ts.plots.folder)
ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_SizeFrequencyPlot', '.pdf', sep = ''),
       plot = lf.plot, units = 'mm', width = 190, height = 250)
ggsave(filename = paste('TimedSwimSurvey_', samp.year, 'SizeFrequencyPlot', '.png', sep = ''),
       plot = lf.plot, units = 'mm', width = 190, height = 250)

##---------------------------------------------------------------------------##
## PLOT 8: LF x year ####
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
 # geom_bar(data = lf.df, aes(x = sizeclass.2021, y = freq*100, fill = factor(sampyear)),
 #          stat = 'identity', position = 'dodge')+
 geom_line(data = lf.df, aes(x = sizeclass.2021, y = freq*100, 
                             group = factor(sampyear), 
                             colour = factor(sampyear)),
           stat = 'identity', position = position_dodge2(0.1),
           size = 1)+
 geom_vline(data = lf.df, aes(xintercept = ifelse(blockno %in% c('27', '28'), 3.8, 3.5)),
            linetype = 'dashed', colour = 'red', size = 0.5) +
 theme_bw()+ 
 # scale_fill_manual(values = c("#77AADD", "#BBCC33", "#EE8866"))+
 # scale_fill_manual(values = c("#7AA6DC99", "#8F770099"))+
 # guides(fill = guide_legend(title = 'Year'))+
 facet_grid(blockno ~ .)+
 scale_y_continuous(breaks = seq(0, 50, 10), labels = seq(0, 50, 10))+
 xlab("Shell Length (mm)")+
 ylab(paste("Percentage (%)"))+
 geom_text(data = block.ab.site.n, aes(x = 7, y = 10, label = n, colour = factor(sampyear, levels = c('2020', '2021', '2022', '2023'))), size = 3, 
           position = position_stack(vjust = 0.8), show.legend = F)+
 scale_colour_manual(values = c("#77AADD", "#BBCC33", "#EE8866", '#EEDD88'))+
 guides(size = 'legend', 
        colour = guide_legend(title = 'Year'))

setwd(ts.plots.folder)
ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_SizeFrequencyPlot_BLOCKS16-28', '.pdf', sep = ''),
       plot = lf.plot, units = 'mm', width = 190, height = 250)
ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_SizeFrequencyPlot_BLOCKS16-28', '.png', sep = ''),
       plot = lf.plot, units = 'mm', width = 190, height = 250)

##---------------------------------------------------------------------------##
## PLOT 9: LF x year reference sites ####

ref.sites <- time.swim.dat.final %>%
 filter(!subblockno %in% c('28B', '28C'),
        # sampdate > as.Date('2021-01-01'),
        !blockno %in% c(13, 14, 29, 30)) %>%
 group_by(sampyear, blockno, site, sizeclass.2021) %>% 
 summarise(n = sum(sizeclass_freq)) %>% 
 group_by(site) %>% 
 summarise(n = n()) %>% 
 filter(n > 20) %>% 
 mutate(ref.site = 1)


ref.site.dat <- time.swim.dat.final %>%
 filter(!subblockno %in% c('28B', '28C'),
        # sampdate > as.Date('2021-01-01'),
        !blockno %in% c(13, 14, 29, 30)) %>%
 group_by(sampyear, blockno, site, sizeclass.2021) %>% 
 left_join(., ref.sites) %>% 
 filter(ref.site == 1) %>% 
 group_by(sampyear, blockno, sizeclass.2021) %>%
 summarise(n = sum(sizeclass_freq)) %>% 
 mutate(freq = n / sum(n)) 


lf.plot.2 <- ggplot()+
 # geom_bar(data = lf.df, aes(x = sizeclass.2021, y = freq*100, fill = factor(sampyear)),
 #          stat = 'identity', position = 'dodge')+
 # geom_vline(data = lf.df, aes(xintercept = ifelse(blockno %in% c('27', '28'), 3.8, 3.5)), linetype = 'dashed', colour = 'red', size = 0.5) +
 geom_line(data = ref.site.dat, aes(x = sizeclass.2021, y = freq*100, 
                                    group = factor(sampyear), 
                                    colour = factor(sampyear)),
           stat = 'identity', position = position_dodge2(0.1),
           size = 1)+
 theme_bw()+ 
 # scale_fill_manual(values = c("#77AADD", "#BBCC33", "#EE8866"))+
 # scale_fill_manual(values = c("#7AA6DC99", "#8F770099"))+
 # guides(fill = guide_legend(title = 'Year'))+
 facet_grid(blockno ~ .)+
 scale_y_continuous(breaks = seq(0, 50, 10), labels = seq(0, 50, 10))+
 xlab("Shell Length (mm)")+
 ylab(paste("Percentage (%)"))+
 # geom_text(data = block.ab.site.n, aes(x = 7, y = 10, label = n, colour = factor(sampyear, levels = c('2020', '2021', '2022'))), size = 3, 
 #           position = position_stack(vjust = 0.8), show.legend = F)+
 scale_colour_manual(values = c("#77AADD", "#BBCC33", "#EE8866"))+
 guides(size = 'legend', 
        colour = guide_legend(title = 'Year'))

##---------------------------------------------------------------------------##
## TAB 1: SUMMARY ####
## summary table of counts and CPUE by size class and block 
## (NOTE: run script for CPUE above)

# Arrange size classes in order
sizeclasses <- c("0-20", "20-40", "40-60", "60-80", "80-100", "100-120", "120-140", "140-160", "160-180", "180-200", "200-220")

# Create summary table
time.swim.count.blockno <- std.ts.dat %>% 
 filter(!subblockno %in% c('28B', '28C'),
        sampdate > as.Date('2023-01-01')) %>% 
 group_by(blockno, site, diver, sampyear, legal.size, time.elapsed) %>% 
 summarise(ab.n = sum(sizeclass_freq)) %>% 
 group_by(blockno, legal.size) %>%
 summarise(sites = n_distinct(site),
           ab.min = round(mean(ab.n), digits = 2)) %>% 
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
# Sites completed summary table
ts.tab <- std.ts.dat %>% 
 filter(!subblockno %in% c('28B', '28C'),
        sampyear == 2022) %>% 
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
         Sites.Day = round(mean(.$Sites.Day), digits = 1))

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
## MAP 1: Site Count x size ####
## Average Count by site - possibly explore KUDs

sf.tas.map <- st_read("C:/CloudStor/DiveFisheries/GIS/SpatialLayers/TasLand.gpkg")
sf.subblock.map <- st_read("C:/CloudStor/DiveFisheries/GIS/SpatialLayers/SubBlockMaps.gpkg")

# transform maps to GDA2020

# set CRS
GDA2020 <- st_crs(7855)
GDA94 <- st_crs(28355)
WGS84 <- st_crs(4326)

sf.tas.map <- sf.tas.map %>% 
 st_set_crs(GDA2020)

sf.subblock.map <- st_transform(sf.subblock.map, GDA2020)

# join CPUE data to site location data

# identify unique sites sampled from timed swim dataframe and join to renamed site file
time.swim.sites <- std.ts.dat %>%
 # filter(sampyear == samp.year) %>%
 filter(sampdate > as.Date('2020-01-01')) %>%
 distinct(site, .keep_all = T) %>% 
 dplyr::select(c(site, blockno, subblockno, sampdate, actual.geom)) %>% 
 st_as_sf() %>% 
 st_set_crs(GDA2020)

ten.min.mean.site <- std.ts.dat %>% 
 filter(!subblockno %in% c('28B', '28C'),
        # sampyear == samp.year) %>% 
        sampdate > as.Date('2020-01-01')) %>%
 group_by(site, blockno, subblockno, diver, legal.size) %>% 
 summarise(ab.n = sum(sizeclass_freq)) %>% 
 group_by(site, blockno, subblockno, legal.size) %>% 
 summarise(mean.ab.n = mean(ab.n)) %>% 
 mutate(legal.size = factor(legal.size))%>% 
 as.data.frame()

# # transform to GDA2020
# time.swim.sites <- st_transform(time.swim.sites, GDA2020)

time.swim.count.site.loc <- left_join(ten.min.mean.site, time.swim.sites) %>% 
 st_as_sf() 

# save file for analysis
saveRDS(time.swim.count.site.loc, paste(samp.year.folder, '/time.swim.count.site.loc.RDS', sep = ''))

# identify blocks sampled
blocks.sampled <- unique(time.swim.count.site.loc$blockno)

# identify sites not surveyed but proposed
ts.proposed.geom <- readRDS(paste(samp.year.folder, '/ts.proposed.geom.RDS', sep = ''))

# identify proposed sites not surveyed
not.surveyed.df <- ts.proposed.geom %>% 
 filter(sampyear == samp.year,
        sampled == '0')

# create plot

for (i in blocks.sampled){
 
 # filter for blockno and sub-legal counts
 count.site.dat.non.legal <- time.swim.count.site.loc %>% 
  filter(blockno == i &
          legal.size == '<140 mm',
         !subblockno %in% c('28B', '28C')) %>% 
  st_as_sf
 
 count.site.dat.legal <- time.swim.count.site.loc %>% 
  filter(blockno == i &
          legal.size == '>140 mm',
         !subblockno %in% c('28B', '28C')) %>% 
  st_as_sf
 
 # filter subblock map for blockno
 sf.subblock.map.crop <- sf.subblock.map %>%
  filter(blockno == i &
          !subblockno %in% c('28B', '28C'))  %>% 
  st_as_sf() 
 
 # filter proposed sites not surveyed
 sites.not.surveyed <- not.surveyed.df %>% 
  filter(blockno == i,
         !subblockno %in% c('28B', '28C'))
 
 # crop tas map to blockno
 sf.tas.map.crop <- st_crop(sf.tas.map, sf.subblock.map.crop)
 
 count.site.map.non.legal <- ggplot(data = st_geometry(sf.tas.map.crop)) +
  geom_sf(data = sf.subblock.map.crop, aes(label = subblockno), fill = NA)+
  geom_sf_text(data = sf.subblock.map.crop, aes(label = subblockno))+
  geom_sf(fill = 'grey') +
  geom_sf(data = sites.not.surveyed, shape = 5, size = 0.8, colour = 'red')+
  geom_sf(data = count.site.dat.non.legal, aes(fill = mean.ab.n), shape = 21, size = 2)+
  scale_fill_gradientn(colours = c("navyblue", "blue", "cyan", "green", "yellow", "orange", "red"),
                       limits = c(0, 125),
                       breaks = c(0, 50, 100),
                       labels = c(0, 50, 100))+
  theme_bw() +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.05, "cm"), pad_y = unit(0.1, "cm"),
                         style = north_arrow_fancy_orienteering)+
  labs(title = '<140 mm', fill = (bquote('Average\ncount')))+
  xlab('Longitude')+
  scale_x_continuous(breaks = seq(140, 149, by = 0.1))+
  ylab('Latitude')
 
 count.site.map.legal <- ggplot(data = st_geometry(sf.tas.map.crop)) +
  geom_sf(data = sf.subblock.map.crop, aes(label = subblockno), fill = NA)+
  geom_sf_text(data = sf.subblock.map.crop, aes(label = subblockno))+
  geom_sf(fill = 'grey') +
  geom_sf(data = sites.not.surveyed, shape = 5, size = 0.8, colour = 'red')+
  geom_sf(data = count.site.dat.legal, aes(fill = mean.ab.n), shape = 21, size = 2)+
  scale_fill_gradientn(colours = c("navyblue", "blue", "cyan", "green", "yellow", "orange", "red"),
                       limits = c(0, 125),
                       breaks = c(0, 50, 100),
                       labels = c(0, 50, 100))+
  theme_bw() +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.05, "cm"), pad_y = unit(0.1, "cm"),
                         style = north_arrow_fancy_orienteering)+
  labs(title = '>140 mm', fill = (bquote('Average\ncount')))+
  xlab('Longitude')+
  scale_x_continuous(breaks = seq(140, 149, by = 0.1))+
  ylab('Latitude')
 
 # extract common legend from maps
 plot.legend <- get_legend(count.site.map.non.legal)  
 
 # remove legend from non legal map
 no.legend.non.legal <- count.site.map.non.legal +
  theme(legend.position = "none")
 
 # remove legend from legal map
 no.legend.legal <- count.site.map.legal +
  theme(legend.position = "none")
 
 # combine plots with common legend (adjusting layout for blocks)
 if(!i %in% c('16', '27')){ 
  count.site.map <- grid.arrange(arrangeGrob(no.legend.non.legal, no.legend.legal), plot.legend, 
                                 ncol = 2, widths = c(2.5, 0.8))
 } else{
  count.site.map <- grid.arrange(no.legend.non.legal, no.legend.legal, plot.legend, 
                                 ncol = 3, widths = c(2.5, 2.5, 0.8))
 }
 
 setwd(ts.plots.folder)
 ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_SiteCountMap_BlockNo_', i, '.pdf', sep = ''),
        plot = count.site.map, units = 'mm', width = 250, height = 250)
 ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_SiteCountMap_BlockNo_', i, '.png', sep = ''),
        plot = count.site.map, units = 'mm', width = 250, height = 250)
 
}

##---------------------------------------------------------------------------##
## MAP 2: Site Count x year ####
## Average Count by site between sample and previous year

sf.tas.map <- st_read("C:/CloudStor/Shared/DiveFisheries/GIS/SpatialLayers/TasLand.gpkg")
sf.subblock.map <- st_read("C:/CloudStor/Shared/DiveFisheries/GIS/SpatialLayers/SubBlockMaps.gpkg")

# transform maps to GDA2020

# set CRS
GDA2020 <- st_crs(7855)
GDA94 <- st_crs(28355)
WGS84 <- st_crs(4326)

sf.tas.map <- sf.tas.map %>% 
 st_set_crs(GDA2020)

sf.subblock.map <- st_transform(sf.subblock.map, GDA2020)

# join CPUE data to site location data

# identify unique sites sampled from timed swim dataframe and join to renamed site file
time.swim.sites <- std.ts.dat %>%
 # filter(sampdate > as.Date('2021-01-01')) %>%
 distinct(site, sampyear, .keep_all = T) %>% 
 select(c(site, blockno, subblockno, sampdate, actual.geom, sampyear)) %>% 
 st_as_sf() %>% 
 st_set_crs(GDA2020)

ten.min.mean.site <- std.ts.dat %>% 
 filter(!subblockno %in% c('28B', '28C')) %>% 
 group_by(site, blockno, subblockno, diver, sampyear) %>% 
 summarise(ab.n = sum(sizeclass_freq)) %>% 
 group_by(site, blockno, subblockno, sampyear) %>% 
 summarise(mean.ab.n = mean(ab.n)) %>% 
 # mutate(sampyear = factor(sampyear))%>% 
 as.data.frame()

time.swim.count.site.loc <- left_join(ten.min.mean.site, time.swim.sites) %>% 
 st_as_sf()

# identify blocks sampled
blocks.sampled <- time.swim.count.site.loc %>% 
 as.data.frame() %>% 
 filter(sampyear == samp.year) %>% 
 distinct(blockno) %>% 
 pull()

# identify sites not surveyed but proposed
ts.proposed.geom <- readRDS(paste(samp.year.folder, '/ts.proposed.geom.RDS', sep = ''))

# identify proposed sites not surveyed
not.surveyed.df <- ts.proposed.geom %>% 
 filter(sampled == '0')

# create plot

for (i in blocks.sampled){
 
 # filter for blockno and sub-legal counts
 count.site.dat.2020 <- time.swim.count.site.loc %>% 
  filter(blockno == i &
          sampyear == samp.year - 1) %>% 
  st_as_sf
 
 count.site.dat.2021 <- time.swim.count.site.loc %>% 
  filter(blockno == i &
          sampyear == samp.year) %>% 
  st_as_sf
 
 # filter subblock map for blockno
 sf.subblock.map.crop <- sf.subblock.map %>%
  filter(blockno == i &
          !subblockno %in% c('28B', '28C'))  %>% 
  st_as_sf() 
 
 # filter proposed sites not surveyed
 sites.not.surveyed.2020 <- not.surveyed.df %>% 
  filter(blockno == i &
          sampyear == samp.year - 1 &
          !subblockno %in% c('28B', '28C'))
 
 sites.not.surveyed.2021 <- not.surveyed.df %>% 
  filter(blockno == i &
          sampyear == samp.year &
          !subblockno %in% c('28B', '28C'))
 
 # crop tas map to blockno
 sf.tas.map.crop <- st_crop(sf.tas.map, sf.subblock.map.crop)
 
 # # crop tas and subblock map to spatial extent of all sites with a 1 km buffer
 # df.1 <- bind_rows(count.site.dat.2020, count.site.dat.2021) %>% 
 #   st_buffer(dist = 1000)
 # sf.tas.map.crop <- st_crop(sf.tas.map, df.1)
 # sf.subblock.map.crop <- st_crop(st_make_valid(sf.subblock.map), df.1)
 
 # ggplot(data = st_geometry(df.2))+
 #   geom_sf(data = df.3, aes(label = subblockno), fill = NA)+
 #   geom_sf_text(data = df.3, aes(label = subblockno))
 
 # create map for 2020
 count.site.map.2020 <- ggplot(data = st_geometry(sf.tas.map.crop)) +
  geom_sf(data = sf.subblock.map.crop, aes(label = subblockno), fill = NA)+
  geom_sf_text(data = sf.subblock.map.crop, aes(label = subblockno))+
  geom_sf(fill = 'grey') +
  geom_sf(data = sites.not.surveyed.2020, shape = 5, size = 0.8, colour = 'red')+
  geom_sf(data = count.site.dat.2020, aes(fill = mean.ab.n), shape = 21, size = 2)+
  scale_fill_gradientn(colours = c("navyblue", "blue", "cyan", "green", "yellow", "orange", "red"),
                       limits = c(0, 200),
                       breaks = c(0, 50, 100, 150),
                       labels = c(0, 50, 100, 150))+
  theme_bw() +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.05, "cm"), pad_y = unit(0.1, "cm"),
                         style = north_arrow_fancy_orienteering)+
  labs(title = samp.year - 1, fill = (bquote('Average\ncount')))+
  xlab('Longitude')+
  scale_x_continuous(breaks = seq(140, 149, by = 0.1))+
  ylab('Latitude')
 
 # create map for 2021
 count.site.map.2021 <- ggplot(data = st_geometry(sf.tas.map.crop)) +
  geom_sf(data = sf.subblock.map.crop, aes(label = subblockno), fill = NA)+
  geom_sf_text(data = sf.subblock.map.crop, aes(label = subblockno))+
  geom_sf(fill = 'grey') +
  geom_sf(data = sites.not.surveyed.2021, shape = 5, size = 0.8, colour = 'red')+
  geom_sf(data = count.site.dat.2021, aes(fill = mean.ab.n), shape = 21, size = 2)+
  scale_fill_gradientn(colours = c("navyblue", "blue", "cyan", "green", "yellow", "orange", "red"),
                       limits = c(0, 200),
                       breaks = c(0, 50, 100, 150),
                       labels = c(0, 50, 100, 150))+
  theme_bw() +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.05, "cm"), pad_y = unit(0.1, "cm"),
                         style = north_arrow_fancy_orienteering)+
  labs(title = samp.year, fill = (bquote('Average\ncount')))+
  xlab('Longitude')+
  scale_x_continuous(breaks = seq(140, 149, by = 0.1))+
  ylab('Latitude')
 
 # extract common legend from maps
 plot.legend <- get_legend(count.site.map.2020)  
 
 # remove legend from 2020 map
 no.legend.2020 <- count.site.map.2020 +
  theme(legend.position = "none")
 
 # remove legend from 2021 map
 no.legend.2021 <- count.site.map.2021 +
  theme(legend.position = "none")
 
 # combine plots with common legend (adjusting layout for blocks)
 if(!i %in% c('16', '27')){ 
  count.site.map <- grid.arrange(arrangeGrob(no.legend.2020, no.legend.2021), plot.legend, 
                                 ncol = 2, widths = c(2.5, 0.8))
 } else{
  count.site.map <- grid.arrange(no.legend.2020, no.legend.2021, plot.legend, 
                                 ncol = 3, widths = c(2.5, 2.5, 0.8))
 }
 
 # # combine plots with common legend (adjusting layout for blocks)
 # if(i != '16'){ 
 #   count.site.map <- grid.arrange(no.legend.2020, no.legend.2021, plot.legend, 
 #                                  ncol = 3, widths = c(2.5, 2.5, 0.8))
 #   # count.site.map <- grid.arrange(arrangeGrob(no.legend.2020, no.legend.2021), plot.legend, 
 #   #                              ncol = 2, widths = c(2.5, 0.8))
 # } else{
 #   count.site.map <- grid.arrange(no.legend.2020, no.legend.2021, plot.legend, 
 #                                ncol = 3, widths = c(2.5, 2.5, 0.8))
 # }
 # save plot
 setwd(ts.plots.folder)
 ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_SiteCountMap-', samp.year - 1,
                         'vs', samp.year, '_BlockNo_', i, '.pdf', sep = ''),
        plot = count.site.map, units = 'mm', width = 250, height = 250)
 ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_SiteCountMap-', samp.year - 1,
                         'vs', samp.year, '_BlockNo_', i, '.png', sep = ''),
        plot = count.site.map, units = 'mm', width = 250, height = 250)
 
}

##---------------------------------------------------------------------------##
## MAP 3: Site Count x year x size class ####
## Average Count by site between years

sf.tas.map <- st_read("C:/CloudStor/Shared/DiveFisheries/GIS/SpatialLayers/TasLand.gpkg")
sf.subblock.map <- st_read("C:/CloudStor/Shared/DiveFisheries/GIS/SpatialLayers/SubBlockMaps.gpkg")

# transform maps to GDA2020

# set CRS
GDA2020 <- st_crs(7855)
GDA94 <- st_crs(28355)
WGS84 <- st_crs(4326)

sf.tas.map <- sf.tas.map %>% 
 st_set_crs(GDA2020)

sf.subblock.map <- st_transform(sf.subblock.map, GDA2020)

# join CPUE data to site location data

# identify unique sites sampled from timed swim dataframe and join to renamed site file
time.swim.sites <- std.ts.dat %>%
 # filter(sampdate > as.Date('2021-01-01')) %>%
 distinct(site, sampyear, .keep_all = T) %>% 
 select(c(site, blockno, subblockno, sampdate, actual.geom, sampyear)) %>% 
 st_as_sf() %>% 
 st_set_crs(GDA2020)

ten.min.mean.site <- std.ts.dat %>% 
 filter(!subblockno %in% c('28B', '28C')) %>% 
 group_by(site, blockno, subblockno, diver, sampyear, legal.size) %>% 
 summarise(ab.n = sum(sizeclass_freq)) %>% 
 group_by(site, blockno, subblockno, sampyear, legal.size) %>% 
 summarise(mean.ab.n = mean(ab.n)) %>% 
 # mutate(sampyear = factor(sampyear))%>% 
 as.data.frame()


time.swim.count.site.loc <- left_join(ten.min.mean.site, time.swim.sites) %>% 
 st_as_sf()

# identify blocks sampled
blocks.sampled <- time.swim.count.site.loc %>% 
 as.data.frame() %>% 
 filter(sampyear == samp.year) %>% 
 distinct(blockno) %>% 
 pull()

# identify sites not surveyed but proposed
ts.proposed.geom <- readRDS(paste(samp.year.folder, '/ts.proposed.geom.RDS', sep = ''))

# identify proposed sites not surveyed
not.surveyed.df <- ts.proposed.geom %>% 
 filter(sampled == '0')


# create plot

for (i in blocks.sampled){
 
 # filter for blockno and sub-legal counts
 count.site.dat.2020 <- time.swim.count.site.loc %>% 
  filter(blockno == i &
          sampyear == samp.year - 1) %>% 
  st_as_sf
 
 count.site.dat.2021 <- time.swim.count.site.loc %>% 
  filter(blockno == i &
          sampyear == samp.year) %>% 
  st_as_sf
 
 # filter subblock map for blockno
 sf.subblock.map.crop <- sf.subblock.map %>%
  filter(blockno == i &
          !subblockno %in% c('28B', '28C'))  %>% 
  st_as_sf() 
 
 # filter proposed sites not surveyed
 sites.not.surveyed.2020 <- not.surveyed.df %>% 
  filter(blockno == i &
          sampyear == samp.year - 1 &
          !subblockno %in% c('28B', '28C'))
 
 sites.not.surveyed.2021 <- not.surveyed.df %>% 
  filter(blockno == i &
          sampyear == samp.year &
          !subblockno %in% c('28B', '28C'))
 
 # crop tas map to blockno
 sf.tas.map.crop <- st_crop(sf.tas.map, sf.subblock.map.crop)
 
 # # crop tas and subblock map to spatial extent of all sites with a 1 km buffer
 # df.1 <- bind_rows(count.site.dat.2020, count.site.dat.2021) %>% 
 #   st_buffer(dist = 1000)
 # sf.tas.map.crop <- st_crop(sf.tas.map, df.1)
 # sf.subblock.map.crop <- st_crop(st_make_valid(sf.subblock.map), df.1)
 
 # ggplot(data = st_geometry(df.2))+
 #   geom_sf(data = df.3, aes(label = subblockno), fill = NA)+
 #   geom_sf_text(data = df.3, aes(label = subblockno))
 
 # create map for 2020
 count.site.map.2020.sublegal <- ggplot(data = st_geometry(sf.tas.map.crop)) +
  geom_sf(data = sf.subblock.map.crop, aes(label = subblockno), fill = NA)+
  geom_sf_text(data = sf.subblock.map.crop, aes(label = subblockno))+
  geom_sf(fill = 'grey') +
  geom_sf(data = sites.not.surveyed.2020, shape = 5, size = 0.8, colour = 'red')+
  geom_sf(data = count.site.dat.2020 %>% filter(legal.size == '<140 mm'), aes(fill = mean.ab.n), shape = 21, size = 2)+
  scale_fill_gradientn(colours = c("navyblue", "blue", "cyan", "green", "yellow", "orange", "red"),
                       limits = c(0, 150),
                       breaks = c(0, 50, 100, 150),
                       labels = c(0, 50, 100, 150))+
  theme_bw() +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.05, "cm"), pad_y = unit(0.1, "cm"),
                         style = north_arrow_fancy_orienteering)+
  labs(title = paste(samp.year - 1, ' Sub-legal <140 mm'), fill = (bquote('Average\ncount')))+
  xlab('Longitude')+
  scale_x_continuous(breaks = seq(140, 149, by = 0.1))+
  ylab('Latitude')+
  theme(plot.margin=unit(c(0.1, 0.1, -1, 0.1), "cm"))
 
 count.site.map.2020.legal <- ggplot(data = st_geometry(sf.tas.map.crop)) +
  geom_sf(data = sf.subblock.map.crop, aes(label = subblockno), fill = NA)+
  geom_sf_text(data = sf.subblock.map.crop, aes(label = subblockno))+
  geom_sf(fill = 'grey') +
  geom_sf(data = sites.not.surveyed.2020, shape = 5, size = 0.8, colour = 'red')+
  geom_sf(data = count.site.dat.2020 %>% filter(legal.size == '>140 mm'), aes(fill = mean.ab.n), shape = 21, size = 2)+
  scale_fill_gradientn(colours = c("navyblue", "blue", "cyan", "green", "yellow", "orange", "red"),
                       limits = c(0, 150),
                       breaks = c(0, 50, 100, 150),
                       labels = c(0, 50, 100, 150))+
  theme_bw() +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.05, "cm"), pad_y = unit(0.1, "cm"),
                         style = north_arrow_fancy_orienteering)+
  labs(title = paste(samp.year - 1, ' Legal >140 mm'), fill = (bquote('Average\ncount')))+
  xlab('Longitude')+
  scale_x_continuous(breaks = seq(140, 149, by = 0.1))+
  ylab('Latitude')+
  theme(plot.margin=unit(c(-1, 0.1, 0.1, 0.1), "cm"))
 
 # create map for 2021
 count.site.map.2021.sublegal <- ggplot(data = st_geometry(sf.tas.map.crop)) +
  geom_sf(data = sf.subblock.map.crop, aes(label = subblockno), fill = NA)+
  geom_sf_text(data = sf.subblock.map.crop, aes(label = subblockno))+
  geom_sf(fill = 'grey') +
  geom_sf(data = sites.not.surveyed.2021, shape = 5, size = 0.8, colour = 'red')+
  geom_sf(data = count.site.dat.2021 %>% filter(legal.size == '<140 mm'), aes(fill = mean.ab.n), shape = 21, size = 2)+
  scale_fill_gradientn(colours = c("navyblue", "blue", "cyan", "green", "yellow", "orange", "red"),
                       limits = c(0, 150),
                       breaks = c(0, 50, 100, 150),
                       labels = c(0, 50, 100, 150))+
  theme_bw() +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.05, "cm"), pad_y = unit(0.1, "cm"),
                         style = north_arrow_fancy_orienteering)+
  labs(title = paste(samp.year, ' Sub-legal <140 mm'), fill = (bquote('Average\ncount')))+
  xlab('Longitude')+
  scale_x_continuous(breaks = seq(140, 149, by = 0.1))+
  ylab('Latitude')+
  theme(plot.margin=unit(c(0.1, 0.1, -1, 0.1), "cm"))
 
 count.site.map.2021.legal <- ggplot(data = st_geometry(sf.tas.map.crop)) +
  geom_sf(data = sf.subblock.map.crop, aes(label = subblockno), fill = NA)+
  geom_sf_text(data = sf.subblock.map.crop, aes(label = subblockno))+
  geom_sf(fill = 'grey') +
  geom_sf(data = sites.not.surveyed.2021, shape = 5, size = 0.8, colour = 'red')+
  geom_sf(data = count.site.dat.2021 %>% filter(legal.size == '>140 mm'), aes(fill = mean.ab.n), shape = 21, size = 2)+
  scale_fill_gradientn(colours = c("navyblue", "blue", "cyan", "green", "yellow", "orange", "red"),
                       limits = c(0, 150),
                       breaks = c(0, 50, 100, 150),
                       labels = c(0, 50, 100, 150))+
  theme_bw() +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.05, "cm"), pad_y = unit(0.1, "cm"),
                         style = north_arrow_fancy_orienteering)+
  labs(title = paste(samp.year, ' Legal >140 mm'), fill = (bquote('Average\ncount')))+
  xlab('Longitude')+
  scale_x_continuous(breaks = seq(140, 149, by = 0.1))+
  ylab('Latitude')+
  theme(plot.margin=unit(c(-1, 0.1, 0.1, 0.1), "cm"))
 
 # extract common legend from maps
 plot.legend <- get_legend(count.site.map.2020.sublegal)  
 
 # remove legend from 2020 map
 no.legend.2020 <- count.site.map.2020.sublegal +
  theme(legend.position = "none")
 
 no.legend.2020.legal <- count.site.map.2020.legal +
  theme(legend.position = "none")
 
 # remove legend from 2021 map
 no.legend.2021 <- count.site.map.2021.sublegal +
  theme(legend.position = "none")
 
 no.legend.2021.legal <- count.site.map.2021.legal +
  theme(legend.position = "none")
 
 # combine plots with common legend (adjusting layout for blocks)
 count.site.map <- grid.arrange(no.legend.2020, no.legend.2021, plot.legend,
                                no.legend.2020.legal, no.legend.2021.legal, 
                                ncol = 3, nrow = 2, widths = c(2.5, 2.5, 0.8))
 
 # save plot
 setwd(ts.plots.folder)
 ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_SiteCountMap-',
                         samp.year - 1, 'vs', samp.year, '_BlockNo_', i, '.pdf', sep = ''),
        plot = count.site.map, units = 'mm', width = 250, height = 300)
 ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_SiteCountMap-',
                         samp.year - 1, 'vs', samp.year, '_BlockNo_', i, '.png', sep = ''),
        plot = count.site.map, units = 'mm', width = 250, height = 300)
 
}

##---------------------------------------------------------------------------##
# above plot as facet wrap

sf.tas.map <- st_read("C:/CloudStor/Shared/DiveFisheries/GIS/SpatialLayers/TasLand.gpkg")
sf.subblock.map <- st_read("C:/CloudStor/Shared/DiveFisheries/GIS/SpatialLayers/SubBlockMaps.gpkg")

# transform maps to GDA2020

# set CRS
GDA2020 <- st_crs(7855)
GDA94 <- st_crs(28355)
WGS84 <- st_crs(4326)

sf.tas.map <- sf.tas.map %>% 
 st_set_crs(GDA2020)

sf.subblock.map <- st_transform(sf.subblock.map, GDA2020)

# join CPUE data to site location data

# identify unique sites sampled from timed swim dataframe and join to renamed site file
time.swim.sites <- std.ts.dat %>%
 # filter(sampdate > as.Date('2021-01-01')) %>%
 distinct(site, sampyear, .keep_all = T) %>% 
 select(c(site, blockno, subblockno, sampdate, actual.geom, sampyear)) %>% 
 st_as_sf() %>% 
 st_set_crs(GDA2020)

ten.min.mean.site <- std.ts.dat %>% 
 filter(!subblockno %in% c('28B', '28C')) %>% 
 group_by(site, blockno, subblockno, diver, sampyear, legal.size) %>% 
 summarise(ab.n = sum(sizeclass_freq)) %>% 
 group_by(site, blockno, subblockno, sampyear, legal.size) %>% 
 summarise(mean.ab.n = mean(ab.n)) %>% 
 # mutate(sampyear = factor(sampyear))%>% 
 as.data.frame()


time.swim.count.site.loc <- left_join(ten.min.mean.site, time.swim.sites) %>% 
 st_as_sf()

#create index of legal/sub-legal x sampyear

df.1 <- time.swim.count.site.loc %>% 
 unite(legal.size.year, sampyear, legal.size,  sep = '', remove = F)

# identify blocks sampled
blocks.sampled <- df.1 %>% 
 as.data.frame() %>% 
 filter(sampyear == samp.year) %>% 
 distinct(blockno) %>% 
 pull()

# identify sites not surveyed but proposed
ts.proposed.geom <- readRDS(paste(samp.year.folder, '/ts.proposed.geom.RDS', sep = ''))

# identify proposed sites not surveyed
not.surveyed.df <- ts.proposed.geom %>% 
 filter(sampled == '0')



for (i in blocks.sampled){
 
 # filter for blockno and sub-legal counts
 count.site.dat.2020 <- df.1 %>% 
  filter(blockno == i &
          between(sampyear, samp.year - 1, samp.year)) %>% 
  st_as_sf
 
 count.site.dat.2020$legal.size.year = factor(count.site.dat.2020$legal.size.year,
                                              levels = c(paste(samp.year - 1, '<140 mm', sep = ''),
                                                         paste(samp.year, '<140 mm', sep = ''),
                                                         paste(samp.year - 1, '>140 mm', sep = ''),
                                                         paste(samp.year, '>140 mm', sep = '')))
 
 # count.site.dat.2021 <- df.1 %>% 
 #  filter(blockno == i &
 #          sampyear == samp.year) %>% 
 #  st_as_sf
 
 # filter subblock map for blockno
 sf.subblock.map.crop <- sf.subblock.map %>%
  filter(blockno == i &
          !subblockno %in% c('28B', '28C'))  %>% 
  st_as_sf() 
 
 # filter proposed sites not surveyed
 sites.not.surveyed.2020 <- not.surveyed.df %>% 
  filter(blockno == i &
          between(sampyear, samp.year - 1, samp.year) &
          !subblockno %in% c('28B', '28C'))
 
 # sites.not.surveyed.2021 <- not.surveyed.df %>% 
 #  filter(blockno == i &
 #          sampyear == samp.year &
 #          !subblockno %in% c('28B', '28C'))
 
 # crop tas map to blockno
 sf.tas.map.crop <- st_crop(sf.tas.map, sf.subblock.map.crop)
 
 # # crop tas and subblock map to spatial extent of all sites with a 1 km buffer
 # df.1 <- bind_rows(count.site.dat.2020, count.site.dat.2021) %>% 
 #   st_buffer(dist = 1000)
 # sf.tas.map.crop <- st_crop(sf.tas.map, df.1)
 # sf.subblock.map.crop <- st_crop(st_make_valid(sf.subblock.map), df.1)
 
 # ggplot(data = st_geometry(df.2))+
 #   geom_sf(data = df.3, aes(label = subblockno), fill = NA)+
 #   geom_sf_text(data = df.3, aes(label = subblockno))
 
 # create map for 2020
 count.site.map.facet <- ggplot(data = st_geometry(sf.tas.map.crop)) +
  geom_sf(data = sf.subblock.map.crop, aes(label = subblockno), fill = NA)+
  geom_sf_text(data = sf.subblock.map.crop, aes(label = subblockno))+
  geom_sf(fill = 'grey') +
  geom_sf(data = sites.not.surveyed.2020, shape = 5, size = 0.8, colour = 'black')+
  geom_sf(data = count.site.dat.2020, aes(fill = mean.ab.n), shape = 21, size = 2)+
  scale_fill_gradientn(colours = c("navyblue", "blue", "cyan", "green", "yellow", "orange", "red"),
                       limits = c(0, 100),
                       breaks = c(0, 50, 100),
                       labels = c(0, 50, 100))+
  theme_bw() +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.05, "cm"), pad_y = unit(0.1, "cm"),
                         style = north_arrow_fancy_orienteering)+
  xlab('Longitude')+
  scale_x_continuous(breaks = seq(140, 149, by = 0.1))+
  ylab('Latitude')+
  labs(fill = 'Average\ncount')+
  facet_wrap(~ legal.size.year, ncol = if_else(i %in% c(16, 27), 4, 2))
 
 # save plot
 setwd(ts.plots.folder)
 ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_SiteCountMap-',
                         samp.year - 1, 'vs', samp.year, '_BlockNo_', i, '.pdf', sep = ''),
        plot = count.site.map.facet, units = 'mm', width = 250, height = 300)
 ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_SiteCountMap-',
                         samp.year - 1, 'vs', samp.year, '_BlockNo_', i, '.png', sep = ''),
        plot = count.site.map.facet, units = 'mm', width = 250, height = 300)
}

##---------------------------------------------------------------------------##
# Urchin and weed density

sf.tas.map <- st_read("C:/CloudStor/DiveFisheries/GIS/SpatialLayers/TasLand.gpkg")
sf.subblock.map <- st_read("C:/CloudStor/DiveFisheries/GIS/SpatialLayers/SubBlockMaps.gpkg")

# Import metadata frame
time.swim.meta.dat.final <- readRDS(paste(samp.year.folder, '/time.swim.meta.dat.final.RDS', sep = ''))

# transform maps to GDA2020
# set CRS
GDA2020 <- st_crs(7855)
GDA94 <- st_crs(28355)
WGS84 <- st_crs(4326)

sf.tas.map <- sf.tas.map %>% 
 st_set_crs(GDA2020)

sf.subblock.map <- st_transform(sf.subblock.map, GDA2020)

# Extract urchin data
df_1 <- time.swim.meta.dat.final %>% 
 filter(!is.na(starttime)) %>% 
 mutate(sampyear = year(starttime)) %>% 
 select(c(site, blockno, sampyear, percent.urchins, urchin.deep, start_geom))

# identify blocks sampled
blocks.sampled <- df_1 %>% 
 as.data.frame() %>% 
 filter(!is.na(blockno)) %>% 
 distinct(blockno) %>% 
 pull()

df_3 <- df_1 %>% 
 st_as_sf() %>% 
 filter(!st_is_empty(.))

i <- 22

for (i in blocks.sampled){
 
 # filter for blockno
 block_dat <- df_3 %>% 
  filter(blockno == i)
 
 # filter subblock map for blockno
 subblockmap_crop <- sf.subblock.map %>%
  filter(blockno == i) %>% 
  st_as_sf()
 
 # crop tas map to blockno
 tasmap_crop <- st_crop(sf.tas.map, subblockmap_crop)
 
 # create map
 urchin_facetmap <- ggplot(data = st_geometry(tasmap_crop)) +
  geom_sf(data = subblockmap_crop, aes(label = subblockno), fill = NA)+
  geom_sf_text(data = subblockmap_crop, aes(label = subblockno))+
  geom_sf(fill = 'grey') +
  geom_sf(data = block_dat, aes(fill = percent.urchins), shape = 21, size = 2)+
  scale_fill_gradientn(colours = c("navyblue", "blue", "cyan", "green", "yellow", "orange", "red"),
                       limits = c(0, 100),
                       breaks = c(0, 50, 100),
                       labels = c(0, 50, 100))+
  theme_bw() +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.05, "cm"), pad_y = unit(0.1, "cm"),
                         style = north_arrow_fancy_orienteering)+
  xlab('Longitude')+
  scale_x_continuous(breaks = seq(140, 149, by = 0.1))+
  ylab('Latitude')+
  labs(fill = 'Average\ncount')+
  facet_wrap(~ sampyear, ncol = if_else(i %in% c(16, 27), 4, 2))
}
##---------------------------------------------------------------------------##


# PLOT 1: Diver deviation ####

# Load timed swim diver pair data
time.swim.divers <- read.xlsx("C:/CloudStor/DiveFisheries/Abalone/FISdata/FIS_TimedSwimSurveys2022/timed_swim_diver_details_2022.xlsx",
                              detectDates = T)

# Add end date for divers still participating
time.swim.divers <- time.swim.divers %>% 
 mutate(end.date = if_else(is.na(end.date), Sys.Date(), end.date))

# Summarise total count for site x blockno x sampyear x sampdate x diver x legal.size
ts.count.sum.2 <- std.ts.dat %>% 
 filter(!subblockno %in% c('28B', '28C')) %>%
 group_by(site, blockno, sampyear, sampdate, diver, legal.size) %>% 
 summarise(ab.n = sum(sizeclass_freq_10)) %>%
 as.data.frame()

# Add diver pairing details to summary data
ts.count.divers <-  fuzzy_left_join(ts.count.sum.2, time.swim.divers, 
                                    by = c("diver" = "diver", 
                                           "sampdate" = "start.date",
                                           "sampdate" = "end.date"),
                                    match_fun = list(`==`, `>=`, `<=`))

# Identify diver pairs x year
ts.count.divers %>% 
 group_by(sampyear, diver.x, dive.pair.id) %>%
 distinct(dive.pair.id) 

# Select sample year
samp.year <- c(2022)

# Identify diver pair IDs x chosen year (remove JM)
diver.ids <- ts.count.divers %>% 
 filter(sampyear == samp.year,
        dive.pair.id != 4) %>% 
 distinct(dive.pair.id) %>% 
 pull()

q <- list()

for (i in diver.ids){
 
 
 # Select data for dive pair and sample year
 ts.diver.dev.dat <- ts.count.divers %>% 
  filter(dive.pair.id == i, sampyear == samp.year) %>% 
  select(c(site, blockno, legal.size, sampyear, sampdate, diver.id, ab.n)) %>%  
  spread(key = diver.id, value = ab.n) %>%  
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
  ylab('Diver Total Count Deviation')+
  ylim(0, 90)+
  theme_bw()+
  ggtitle(paste(samp.year, 'Diver', names(dive.dev.divers[1]), 'vs',
                'Diver', names(dive.dev.divers[2])))+
  geom_text(data = divers.site.n, aes(y = 90, label = site.n), size = 3)+
  scale_fill_manual(values = c("#999999", "#56B4E9"))+
  theme(legend.title = element_blank(),
        legend.position = c(0.15, 0.8))
 # theme(legend.title = element_blank(),
 #       legend.position = ifelse(names(dive.dev.divers) == '1' &
 #                                 names(dive.dev.divers) == '5' &
 #                                 samp.year == 2020, c(0.1, 0.8), "none"))
 
}

dive.dev.plot <- q %>% 
 discard(is.null) %>% 
 cowplot::plot_grid(plotlist = .)



# save plot
setwd(ts.plots.folder)
ggsave(filename = paste('TimedSwimSurvey_DiverDeviation_', samp.year, '.pdf', sep = ''),
       plot = dive.dev.plot, units = 'mm', width = 210, height = 200)

ggsave(filename = paste('TimedSwimSurvey_DiverDeviation_', samp.year, '.png', sep = ''),
       plot = dive.dev.plot, units = 'mm', width = 210, height =200)


##---------------------------------------------------------------------------##
# PLOT 10: Bland-Altman ####
# Bland-Altman plot of diver count differences
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4470095/
df.1 <- ts.count.divers %>% 
 filter(dive.pair.id != 4 &
         !blockno %in% c(13, 14, 29, 30) &
         dive.pair.id %in% c(1, 6)) %>%
 select(c(site, blockno, legal.size, sampyear, sampdate, dive.pair.id, ab.n)) %>%  
 group_by(site, blockno, legal.size, sampyear, sampdate, dive.pair.id) %>% 
 mutate(var = paste0('ab.n', row_number())) %>% 
 spread(var,ab.n) %>% 
 ungroup() %>% 
 mutate_at(c(7:8), ~replace(., is.na(.), 0)) %>% 
 mutate(dive.diff = .[[7]] - .[[8]],
        dive.avg = (.[[7]] + .[[8]]) / 2) %>%  
 select(c(site, blockno, legal.size, sampyear, sampdate, dive.pair.id, dive.diff, dive.avg))

df.2 <- df.1 %>% group_by(dive.pair.id, legal.size) %>% 
 summarise(avg.diff = round(mean(dive.diff), 1),
           n = n(),
           pos.ci = round(mean(dive.diff) + 1.96 * sd(dive.diff)/sqrt(n()), 1),
           neg.ci = round(mean(dive.diff) - 1.96 * sd(dive.diff)/sqrt(n()), 1),
           pos.sd = round(mean(dive.diff) + (1.96 * sd(dive.diff)), 1),
           neg.sd = round(mean(dive.diff) - (1.96 * sd(dive.diff)), 1)) %>% 
 mutate(mean.diff = paste('Mean = ', avg.diff))

diver.pairs <- c('Diver Pair 1', 'Diver Pair 6')
names(diver.pairs) <- c('1', '6')

BA.plot <- df.1 %>% ggplot(aes(x = dive.avg, y = dive.diff, group = dive.pair.id))+
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
 facet_grid(dive.pair.id ~ legal.size, 
            labeller = labeller(dive.pair.id = diver.pairs))

# save plot
setwd(ts.plots.folder)
ggsave(filename = paste('TimedSwimSurvey_Bland-Altman_DiverDifference_IAS2023_', samp.year, '.pdf', sep = ''),
       plot = BA.plot, units = 'mm', width = 210, height = 200)

ggsave(filename = paste('TimedSwimSurvey_Bland-Altman_DiverDifference_IAS2023_', samp.year, '.png', sep = ''),
       plot = BA.plot, units = 'mm', width = 210, height =200)

##---------------------------------------------------------------------------##

## NOTE FOR LACHIE - STOP HERE

## These are additional summary plots for final report examining present day with
# available historical data.


##---------------------------------------------------------------------------##
## 2021 site selection ####

# Randomly select 15 sites from each block to keep as reference sites for 2021 survey and
# beyond. Determine hexcell IDs to filter from random generation of new sites from
# GPS logger data in 2021.

set.seed(123) 
sites.2020.keep <- time.swim.dat.final %>%
 filter(sampyear == 2020,
        !subblockno %in% c('28B', '28C')) %>% 
 group_by(blockno) %>% 
 distinct(site, .keep_all = T) %>% 
 sample_n(15) %>% 
 select(-proposed.geom) %>% 
 st_as_sf()

# save GIS spatial layer
# st_write(sites.2020.keep,
#          dsn = "C:/Users/jaimem/Documents/Abalone_research/AB_TimedSwimFIS/FIS_TIMEDSWIMSITES_2020REFERENCE_2021-05-17.gpkg",
#          layer = "sites.2020.keep", driver = "GPKG", overwrite = T, delete_dsn = T)

# NOTE: set.seed wasn't used when generating the selection of reference sites from 2020 to use in 2021.
# Therefore the resulting output gpkg file above has been re-imported to identify those reference sites
# and stored as an RDS for future reference.

sites.2020.kept.sf <- st_read("C:/Users/jaimem/OneDrive - University of Tasmania/Documents/AB_TimedSwimFIS/FIS_TIMEDSWIMSITES_2020REFERENCE_2021-05-17.gpkg")

# convert to data frame
sites.2020.kept.df <- sites.2020.kept.sf %>%
 sfheaders::sf_to_df(fill = T)

# save file
saveRDS(sites.2020.kept.df, 'C:/CloudStor/Shared/DiveFisheries/Abalone/FISdata/FIS_TimedSwimSurveys2021/time.swim.2020.repeat.sites.RDS')
sites.2020.kept.df <- readRDS('C:/CloudStor/Shared/DiveFisheries/Abalone/FISdata/FIS_TimedSwimSurveys2021/time.swim.2020.repeat.sites.RDS')

# create vector of oid to be sampled in 2021 from 2020 sites sampled
# Note: 12 of the reference sites are SAM historical research sites (i.e. n = 78) 
sites.2020.keep <- sites.2020.kept.df
sites.2020.keep.oid <- sites.2020.keep %>% 
 filter(!is.na(oid)) %>% 
 ungroup() %>% 
 pull(oid)

# create vector of all oid sampled in 2020 to filter from 2021 random site selection
sites.2020.oid <- time.swim.dat.final %>%
 filter(!subblockno %in% c('28B', '28C'),
        sampdate < as_date('2021-01-01')) %>% 
 group_by(blockno) %>% 
 distinct(site, .keep_all = T) %>% 
 filter(!is.na(oid)) %>% 
 ungroup() %>% 
 pull(oid)


##---------------------------------------------------------------------------##
#Excel file of raw site data with gps positions

# read GPX file from plotter
morana.gps <- st_read('C:/Users/jaimem/Documents/Abalone_research/AB_TimedSwimFIS/MORANAII-2020-10-09_download.gpx', layer = 'waypoints')

# separate start positions
df.start <- time.swim.meta.dat %>%
 select(-c(waypoint.finish, finishtime)) %>% 
 dplyr::rename('waypoint' = waypoint.start,
               'samptime' = starttime) %>% 
 mutate(sampperiod = 'start')

# separate finish positions
df.finish <- time.swim.meta.dat %>%
 select(-c(waypoint.start, starttime)) %>% 
 dplyr::rename('waypoint' = waypoint.finish,
               'samptime' = finishtime) %>% 
 mutate(sampperiod = 'finish')

# re-join start and finish positions  and remove non-sampled sites        
df.start.finish <- bind_rows(time.swim.site.start, time.swim.site.finish) %>%
 mutate(waypoint = if_else(waypoint < 100, as.character(paste(0, waypoint, sep = '')), as.character(waypoint))) %>%  
 # select(c(sampdate, samptime, sampperiod, site, waypoint)) %>% 
 filter(site != 'Betsey')

# join geometry where start waypoint recorded as being on the original GPS position/waypoint mark
df.site.start.finish.mark <- df.start.finish %>% 
 filter(is.na(waypoint) & sampperiod == 'start') %>% 
 left_join(., morana.gps, by = c('site' = 'name')) %>% 
 dplyr::rename('gpstime' = time,
               'site' = site)
# select(c(sampdate, samptime, gpstime, sampperiod, site, waypoint, geometry))

# join geometry where waypoints were recorded 
df.site.start.finish.wp <- df.start.finish %>% 
 filter(!is.na(waypoint)) %>% 
 left_join(., morana.gps, by = c('waypoint' = 'name')) %>% 
 dplyr::rename('gpstime' = time,
               'site' = site) 
# select(c(sampdate, samptime, gpstime, sampperiod, site, waypoint, geometry))

# re-join all waypoint data
df.site.start.finish.loc <- bind_rows(df.site.start.finish.mark, df.site.start.finish.wp) %>% 
 st_as_sf() %>%
 mutate(lat = unlist(map(geometry,1)),
        long = unlist(map(geometry,2))) %>% 
 select(c("sampdate", "site", "divers", "samptime", "waypoint",
          "habitat.type", "percent.algae.cover", "percent.urchins",
          "comments", "sampperiod", "sym", 'lat', 'long')) %>% 
 as.data.frame() %>% 
 select(-c('geometry'))

# transform to GDA2020
# df.site.start.finish.loc <- st_transform(df.site.start.finish.loc, GDA2020) %>% 
#         as.data.frame()

setwd('C:/Users/jaimem/Documents/Abalone_research/AB_TimedSwimFIS')
write.xlsx(df.site.start.finish.loc,
           file = paste('TimedSwimSurveyMetaData_2020_', Sys.Date(), '.xlsx'),
           sheetName = "Sheet1",
           col.names = TRUE,
           row.names = TRUE,
           append = FALSE)
##---------------------------------------------------------------------------##
## PLOT 13: CPUE KG.HR ####
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
 filter(blockno == 21) %>% 
 select(a, b) %>% 
 mutate(join.id = 1)

# join chosen regression parameters to timed swim data
time.swim.dat.df.lw <- std.ts.dat %>% 
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
 scale_fill_manual(values = c("#77AADD", "#BBCC33", "#DDDDDD"))+
 stat_summary(aes(group = as.factor(sampyear)), fun.y = mean, 
              geom = 'point', shape = 19, size = 2, colour = 'red', fill = 'red',
              position = position_dodge2(0.8))+
 theme_bw() +
 ylim(0, 200)+
 ylab(bquote('Estimated CPUE ('*~kg.hr^-1*')'))+
 xlab('Blockno')+
 geom_text(data = time.swim.cpue.n, aes(y = 200, label = n, 
                                        colour = factor(sampyear, levels = c('2020', '2021', '2022'))), size = 3, position = position_dodge2(0.8))+
 scale_colour_manual(values = c("#77AADD", "#BBCC33", "#DDDDDD"))+
 guides(size = 'legend', colour = 'none',
        fill = guide_legend(title = 'Year'))

ggsave(filename = paste(ts.plots.folder, 'TimedSwimSurvey_', samp.year, '_CPUEPlot', '.pdf', sep = ''), 
       plot = cpue.plot, units = 'mm', width = 190, height = 120)
ggsave(filename = paste(ts.plots.folder, 'TimedSwimSurvey_', samp.year, '_CPUEPlot', '.png', sep = ''), 
       plot = cpue.plot, units = 'mm', width = 190, height = 120)


##---------------------------------------------------------------------------##
# Plot historical CPUE vs estimated timed swim CPUE

# Import data from HS_TACExtract2023.R code from line 284-286
df_1 <- readRDS('C:/cloudstor/DiveFisheries/Abalone/FISdata/FIS_TimedSwimSurveys2022/TACAdjust_E.rds')

df_1 %>% 
 filter(group %in% c('16', '22', '23', '24', '27', '28'),
        fishyear < 2020) %>%
 ggplot(aes(x = factor(fishyear), colour = factor(group)))+
 geom_point(aes(y = stndcpue), size = 3)+
 geom_line(aes(y = stndcpue, group = group))


df_2 <- df_1 %>% 
 filter(group %in% c('16', '22', '23', '24', '27', '28'),
        fishyear < 2020) %>% 
 select(c(group, fishyear, stndcpue, stndlower, stndupper)) %>% 
 dplyr::rename(blockno = 'group') %>% 
 mutate(dat_source = 'ce')

df_3 <- time.swim.cpue.blockno %>% 
 dplyr::rename(fishyear = 'sampyear',
               stndcpue = 'est_kg_hr') %>% 
 mutate(stndlower = stndcpue - kg_hr_se,
        stndupper = stndcpue + kg_hr_se,
        dat_source = 'ts',
        blockno = as.numeric(blockno)) %>% 
 select(c(blockno, fishyear, stndcpue, stndlower, stndupper, dat_source))

df_4 <- bind_rows(df_2, df_3) %>% 
 filter(stndcpue != 0)

df_5 <- df_4 %>% 
 filter(dat_source == 'ce') %>% 
 group_by(blockno) %>% 
 summarise(q_55 = quantile(stndcpue, probs =(0.55)))

# block_no <- 28
# 
# df_4 %>% 
#  # filter(blockno == 16) %>% 
#  ggplot(aes(x = factor(fishyear), colour = factor(blockno)))+
#  geom_point(data = filter(df_4, dat_source == 'ce', blockno == block_no), aes(y = stndcpue), size = 3, colour = 'blue')+
#  geom_errorbar(data = filter(df_4, dat_source == 'ce', blockno == block_no), aes(ymin = stndlower, ymax = stndupper), colour = 'blue')+
#  geom_line(data = filter(df_4, dat_source == 'ce', blockno == block_no), aes(y = stndcpue, group = blockno), colour = 'blue')+
#  geom_point(data = filter(df_4, dat_source == 'ts', blockno == block_no), aes(y = stndcpue), size = 3, colour = 'red')+
#  geom_errorbar(data = filter(df_4, dat_source == 'ts', blockno == block_no), aes(ymin = stndlower, ymax = stndupper), colour = 'red')+
#  geom_line(data = filter(df_4, dat_source == 'ts', blockno == block_no), aes(y = stndcpue, group = blockno), colour = 'red')+
#  geom_line(data = filter(df_4, fishyear %in% c(2019, 2020), blockno == block_no), aes(y = stndcpue, group = blockno), colour = 'red')+
#  theme_bw()+
#  ylim(0, 120)+
#  xlab('Year')+
#  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
#  ylab(bquote('CPUE ('*~kg.hr^-1*')'))

cpue_plot <- df_4 %>%
 ggplot(aes(x = factor(fishyear), y = stndcpue, colour = factor(blockno)))+
 geom_point(data = filter(df_4, dat_source == 'ce'), aes(y = stndcpue), size = 2, colour = 'black')+
 geom_errorbar(data = filter(df_4, dat_source == 'ce'), aes(ymin = stndlower, ymax = stndupper), colour = 'black', width = 0.5)+
 geom_line(data = filter(df_4, dat_source == 'ce'), aes(y = stndcpue, group = blockno), colour = 'black', size = 0.5)+
 geom_point(data = filter(df_4, dat_source == 'ts'), aes(y = stndcpue), size = 2, colour = 'red')+
 geom_errorbar(data = filter(df_4, dat_source == 'ts'), aes(ymin = stndlower, ymax = stndupper), colour = 'red', width = 0.5)+
 geom_line(data = filter(df_4, dat_source == 'ts'), aes(y = stndcpue, group = blockno), colour = 'red')+
 # geom_line(data = filter(df_4, fishyear %in% c(2019, 2020)), aes(y = stndcpue, group = blockno), colour = 'red')+
 geom_hline(data = df_5, aes(yintercept = q_55), linetype = 'dashed', colour = 'blue')+
 theme_bw()+
 ylim(0, 120)+
 xlab('Year')+
 theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
 ylab(bquote('CPUE ('*~kg.hr^-1*')'))+
 facet_wrap(blockno ~ ., nrow = 3)

ggsave(filename = paste(ts.plots.folder, 'TimedSwimSurvey_', '1992-2022', '_CPUEPlot', '.pdf', sep = ''), 
       plot = cpue_plot, units = 'mm', width = 190, height = 150)
ggsave(filename = paste(ts.plots.folder, 'TimedSwimSurvey_', '1992-2022', '_CPUEPlot', '.png', sep = ''), 
       plot = cpue_plot, units = 'mm', width = 190, height = 150)


##---------------------------------------------------------------------------##

## PLOT 14: CPUE vs TS ####
## plot timed swim average counts for legal and sub-leagl
## against historical fishery CPUE for each site

ts.vs.cpue.plot <- std.ts.dat %>% 
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
ts.dat <- std.ts.dat %>% 
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
time.swim.dat.vs.cpue.plot <- std.ts.dat %>% 
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
time.swim.dat.vs.sam.plot <- std.ts.dat %>%
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

df.3 <- std.ts.dat %>% 
 filter(blockno == 13) %>%
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

