# This script provides a preliminary analysis of measuring board data prior to catch dockets being processed
# by NRE and further catch details becoming available. These analysis are intended to provide processors with
# near real-time summary of catches which they have measured.

# Created by: Jaime McAllister

##----------------------------------------------------------------------------##
# clear console ####
# rm(list = ls())

# 1. load libaries ####
suppressPackageStartupMessages({
library(openxlsx)
library(fuzzyjoin)
library(lubridate)
library(tidyr)
library(tidyverse)
library(dplyr)
library(ggsci)
library(ggpubr)
library(scales)
library(gridExtra)
library(hms)        
})        
##----------------------------------------------------------------------------##
# 2. Set sample year and file paths ####

# identify target year of interest
target_year <- 2025

target_year_folder <- file.path(paste(sprintf('C:/Users/%s/Dropbox (UTAS Research)/DiveFisheries/Abalone/Assessment/Figures/MM',
                                              Sys.info()[["user"]])),
                                              paste(target_year, '/', 'MM_Plots_', target_year, 'ProcessorSummaries//', sep = ''))

# identify data folder
mm_data_folder <- paste(sprintf('C:/Users/%s/Dropbox (UTAS Research)/DiveFisheries/Abalone/MMdata/',
                                Sys.info()[["user"]]))

# target_year_folder <- paste('C:/CloudStor/DiveFisheries/Abalone/Assessment/Figures/MM/', target_year, "/",
#                             'MM_Plots_', target_year, 'ProcessorSummaries/', sep = '')
##---------------------------------------------------------------------------##
# 3. Load data ####

# load latest measuring board data that comes from MM_NextGen_4G.R script
# measure.board.next.gen.df <- readRDS('C:/CloudStor/R_Stuff/MMLF/measure.board.next.gen.df.RDS')
measure.board.next.gen.df <- readRDS(paste0(mm_data_folder, 'measure.board.next.gen.df.RDS'))

# add sample year
measure.board.next.gen.df <- measure.board.next.gen.df %>% 
        mutate(sampyear = year(plaindate))

# add last sample time for each sample.id
measure.board.next.gen.df.samptime <- measure.board.next.gen.df %>% 
        group_by(docket.index) %>% 
        summarise(samptime.max = max(local_date))

measure.board.next.gen.df <- left_join(measure.board.next.gen.df, measure.board.next.gen.df.samptime, by = 'docket.index')  

# quick summary of catches measured by processor
measure.board.next.gen.df %>% 
        filter(local_date >= as.Date('2023-12-31')) %>% 
        group_by(processor) %>% 
        summarise(catches.measured = n_distinct(docketnum),
                  n = n(),
                  last.sample = max(plaindate)) %>% 
        janitor::adorn_totals() %>% 
        as.data.frame()

# add weight grading categories used by processors
mb.next.gen.grade.df <- measure.board.next.gen.df %>% 
        mutate(grade = dplyr::if_else(wholeweight == 0, NA_character_,
                                      dplyr::if_else(processor == 'RALPHS TASMANIAN SEAFOODS PTY LTD' & between(wholeweight, 1, 400), 'xsmall', #0-400g can also be labelled xsmall requested by RTS
                                      dplyr::if_else(between(wholeweight, 1, 650), 'small', 
                                                     dplyr::if_else(between(wholeweight, 651, 850), 'medium', 'large')))),
               wholeweight = replace(wholeweight, wholeweight == 0, NA))

# list of unique processors for summary and plot loops
processors <- mb.next.gen.grade.df %>% 
 filter(!is.na(processor))
processors <- unique(processors$processor)


# determine number of abalone measured per docket
docknum.n.meas <- mb.next.gen.grade.df %>% 
        group_by(zone, docketnum, docketnum.day, processor, plaindate, samptime.max, docket.index) %>% 
        # group_by(zone, docketnum, processor, plaindate, docket.index) %>% 
        summarise(ab.meas = n()) 

docknum.n.weighed <- mb.next.gen.grade.df %>% 
        filter(!is.na(wholeweight)) %>%
        group_by(zone, docketnum, docketnum.day, processor, plaindate,  samptime.max, docket.index) %>% 
        summarise(ab.weighed = n())
        

docknum.n <- left_join(docknum.n.meas, docknum.n.weighed)

# determine number of abalone measured by grade per docket
docknum.grade.meas <- mb.next.gen.grade.df %>% 
        filter(!is.na(wholeweight)) %>% 
        group_by(docketnum, docketnum.day, grade, processor, plaindate,  samptime.max, docket.index) %>% 
        summarise(grade.meas = n())

# quick summary by zone and processor
dock.sum <- measure.board.next.gen.df %>% 
        filter(sampyear == target_year) %>% 
        group_by(processor, zone) %>% 
        summarise(n = n(),
                  catches = n_distinct(docketnum)) %>% 
        janitor::adorn_totals()

# quick summary by zone
measure.board.next.gen.df %>% 
 filter(sampyear == target_year) %>% 
 group_by(zone) %>% 
 summarise(n = n(),
           catches = n_distinct(docketnum)) %>% 
 mutate(perc = (catches/sum(catches)*100)) %>% 
 janitor::adorn_totals()

##----------------------------------------------------------------------------##
# 4. Table 1: Summary ####

# Summary length/weight and grade table for each processor

for(i in processors){

# join number of abalone measured per docket and grade, and calculate percentage measured per grade to 
# create grade summary table
grade.summary <- left_join(docknum.n, docknum.grade.meas, by = c('docket.index', 'docketnum', 'docketnum.day', 'processor', 'plaindate', 'samptime.max')) %>% 
        filter(processor == i) %>%  
        mutate(grade.perc = round((grade.meas / ab.weighed) * 100),
               sampyear = year(plaindate)) %>% 
        filter(sampyear == target_year) %>% 
        ungroup() %>% 
        # select(-c(grade.meas, processor, docket.index.x, docket.index.y, sampyear)) %>% 
        select(-c(grade.meas, processor, docket.index, sampyear)) %>% 
        spread(grade, grade.perc) %>%  
        # rename("Large\n(%)" = large, "Medium\n(%)" = medium, "Small\n(%)" = small) %>%
        {if('large' %in% names(.)) dplyr::rename(., "Large\n(%)" = large) else .} %>%
        {if('medium' %in% names(.)) dplyr::rename(., "Medium\n(%)" = medium) else .} %>%
        {if('small' %in% names(.)) dplyr::rename(., "Small\n(%)" = small) else .} %>%
        {if('xsmall' %in% names(.)) dplyr::rename(., "XSmall\n(%)" = xsmall) else .} %>% 
        arrange(desc(samptime.max)) %>%
        ungroup() %>% 
        mutate(docketnum = paste(zone, docketnum.day, sep = ''),
               samptime = format(ymd_hms(samptime.max), format = '%H:%M')) %>%
        filter(plaindate >= as.Date('2021-01-01')) %>% 
        dplyr::rename('Sample\ndate' = plaindate,
                      'Sample\ntime' = samptime,
               'Docket\nno.' = docketnum,
               'Abalone\nmeasured' = ab.meas,
               'Abalone\nweighed' = ab.weighed) %>%
        select(c("Docket\nno.", "Sample\ndate", "Sample\ntime", "Abalone\nmeasured",
                 "Abalone\nweighed", everything())) %>% 
        select(-c(zone, samptime.max, docketnum.day)) %>% 
        as.data.frame()
        
# create length and weight summary table
length.weight.summary <- mb.next.gen.grade.df %>%
        filter(wholeweight != 0 & processor == i & sampyear == target_year) %>% 
        group_by(docketnum.day, processor, plaindate, samptime.max, zone, docket.index) %>%
        summarise('Mean\nweight\n(g)' = round(mean(wholeweight), 0),
                  'Min\nweight\n(g)' = min(wholeweight),
                  'Median\nweight\n(g)' = round(median(wholeweight), 0),
                  'Max\nweight\n(g)' = max(wholeweight),
                  'Mean\nsize\n(mm)' = round(mean(shelllength), 0),
                  'Min\nsize\n(mm)' = round(min(shelllength), 0),
                  'Median\nsize\n(mm)' = round(median(shelllength), 0),
                  'Max\nsize\n(mm)' = round(max(shelllength), 0)) %>% 
        arrange(desc(samptime.max)) %>%
        ungroup() %>% 
        mutate(docketnum = paste(zone, docketnum.day, sep = ''),
               samptime = format(ymd_hms(samptime.max), format = '%H:%M')) %>% 
        ungroup() %>% 
        filter(plaindate >= as.Date('2021-01-01')) %>% 
        arrange(desc(samptime.max)) %>%
        dplyr::rename('Sample\ndate' = plaindate,
                      'Sample\ntime' = samptime,
                      'Docket\nno.' = docketnum) %>% 
        # select(-c(processor, zone, sample.id, samptime.max)) %>%
        select(c("Docket\nno.", "Sample\ndate", "Sample\ntime", "Mean\nweight\n(g)", "Min\nweight\n(g)",
                 "Median\nweight\n(g)", "Max\nweight\n(g)", "Mean\nsize\n(mm)",
                 "Min\nsize\n(mm)", "Median\nsize\n(mm)", "Max\nsize\n(mm)")) %>% 
        as.data.frame()



# # save excel summary tables to Cloudstor folder

i <- ifelse(i == 'RALPHS TASMANIAN SEAFOODS PTY LTD',
                         'TRUE SOUTH SEAFOOD', i)

# setwd(target_year_folder)
write.xlsx(grade.summary,
           file = paste(target_year_folder, i, '_GradeSummary_', target_year, '.xlsx'),
           sheetName = "Sheet1",
           col.names = TRUE,
           row.names = TRUE,
           append = FALSE)
write.xlsx(length.weight.summary,
           file = paste(target_year_folder, i, '_SizeWeightSummary_', target_year, '.xlsx'),
           sheetName = "Sheet1",
           col.names = TRUE,
           row.names = TRUE,
           append = FALSE)

# create formated summary tables for report layout
if (nrow(grade.summary) != 0) {

grade.summary.formated <- grade.summary %>% 
        ggpubr::ggtexttable(rows = NULL, theme = ggpubr::ttheme('mOrange'))

length.weight.summary.formated <- length.weight.summary %>% 
        ggpubr::ggtexttable(rows = NULL, theme = ggpubr::ttheme('mOrange'))

ggsave(
        filename = paste(target_year_folder, i, '_WEIGHTGRADESUMMARY_', target_year, '.pdf', sep = ''),
        plot = grade.summary.formated,
        width = 200,
        height = 350,
        units = 'mm'
)

ggsave(
        filename = paste(target_year_folder, i, '_LENGTHWEIGHTSUMMARY_', target_year, '.pdf', sep = ''),
        plot = length.weight.summary.formated,
        width = 250,
        height = 300,
        units = 'mm'
)
}
else{
}
}

##----------------------------------------------------------------------------##
# 5. Plot setup  ####

## Search for existing docket summaries on file and identify new dockets in the compiled
## measuring board data frame for which to create summaries

# identify local working folder containing existing docket summaries
processor_summaries_2025 <- target_year_folder
# processor.summaries.2023 <- 'C:/cloudstor/DiveFisheries/Abalone/Assessment/Figures/MM/2023/MM_Plots_2023ProcessorSummaries'
# processor.summaries.2022 <- 'C:/CloudStor/R_Stuff/MMLF/MM_Plots/MM_Plots_2022ProcessorSummaries'
# processor.summaries.2021 <- 'C:/CloudStor/R_Stuff/MMLF/MM_Plots/MM_Plots_2021ProcessorSummaries'
# processor.summaries.2020 <- 'C:/CloudStor/R_Stuff/MMLF/MM_Plots/MM_Plots_2020ProcessorSummaries'

processor.summaries.2020 <- file.path(paste(sprintf('C:/Users/%s/Dropbox (UTAS Research)/DiveFisheries/Abalone/Assessment/Figures/MM',
                                              Sys.info()[["user"]])),
                                paste(2020, '/', 'MM_Plots_', 2020, 'ProcessorSummaries//', sep = ''))
processor.summaries.2021 <- file.path(paste(sprintf('C:/Users/%s/Dropbox (UTAS Research)/DiveFisheries/Abalone/Assessment/Figures/MM',
                                                    Sys.info()[["user"]])),
                                      paste(2021, '/', 'MM_Plots_', 2021, 'ProcessorSummaries//', sep = ''))
processor.summaries.2022 <- file.path(paste(sprintf('C:/Users/%s/Dropbox (UTAS Research)/DiveFisheries/Abalone/Assessment/Figures/MM',
                                                    Sys.info()[["user"]])),
                                      paste(2022, '/', 'MM_Plots_', 2022, 'ProcessorSummaries//', sep = ''))
processor.summaries.2023 <- file.path(paste(sprintf('C:/Users/%s/Dropbox (UTAS Research)/DiveFisheries/Abalone/Assessment/Figures/MM',
                                                    Sys.info()[["user"]])),
                                      paste(2023, '/', 'MM_Plots_', 2023, 'ProcessorSummaries//', sep = ''))
processor_summaries_2024 <- file.path(paste(sprintf('C:/Users/%s/Dropbox (UTAS Research)/DiveFisheries/Abalone/Assessment/Figures/MM',
                                                    Sys.info()[["user"]])),
                                      paste(2024, '/', 'MM_Plots_', 2024, 'ProcessorSummaries//', sep = ''))

# list filenames of existing docket summaries in each folder
docket.summaries.2020 <- list.files(processor.summaries.2020,  pattern = "^AW.*pdf|^AE.*pdf|^AB.*pdf|^AN.*pdf|^AG.*pdf", full.names = F)
docket.summaries.2021 <- list.files(processor.summaries.2021,  pattern = "^AW.*pdf|^AE.*pdf|^AB.*pdf|^AN.*pdf|^AG.*pdf", full.names = F)
docket.summaries.2022 <- list.files(processor.summaries.2022,  pattern = "^AW.*pdf|^AE.*pdf|^AB.*pdf|^AN.*pdf|^AG.*pdf", full.names = F)
docket.summaries.2023 <- list.files(processor.summaries.2023,  pattern = "^AW.*pdf|^AE.*pdf|^AB.*pdf|^AN.*pdf|^AG.*pdf", full.names = F)
docket_summaries_2024 <- list.files(processor_summaries_2024,  pattern = "^AW.*pdf|^AE.*pdf|^AB.*pdf|^AN.*pdf|^AG.*pdf", full.names = F)
docket_summaries_2025 <- list.files(processor_summaries_2025,  pattern = "^AW.*pdf|^AE.*pdf|^AB.*pdf|^AN.*pdf|^AG.*pdf", full.names = F)


# combine filenames
docket.summaries <- c(docket.summaries.2020, 
                      docket.summaries.2021, 
                      docket.summaries.2022,
                      docket.summaries.2023,
                      docket_summaries_2024,
                      docket_summaries_2025)

# create a vector of existing docket numbers from filenames 
existing.dockets <- as.data.frame(docket.summaries) %>%
        mutate(docket.summaries = mgsub::mgsub(docket.summaries, c('_1_', '_2_', '_3_'), c('-1_', '-2_', '-3_'))) %>% 
        separate(docket.summaries, c('docket.number',
                                     'docket.plot',
                                     'docket.date',
                                     'docket.processor'), sep = '_') %>% 
        select(docket.number, docket.date, docket.processor) %>% 
        separate(docket.number, into = c('docket.zone', 'docket.number'), "(?<=[A-Z])(?=[0-9])") %>%
        mutate(docketnum.day = docket.number,
               docketnum.day = gsub('-', '_', docket.number)) %>% 
        mutate(docket.processor = gsub('.pdf', '', docket.processor)) %>% 
        group_by_all() %>% 
        summarise(n = n()) %>%  
        mutate(summary.plot.exists = ifelse(!is.na(n), 1, 0),
               docket.index = paste(docket.zone, docketnum.day, docket.date, docket.processor, sep = '-')) %>%         
        pull(docket.index)

# Load vector of incomplete measureboard data for existing docket numbers determined in 
# MM_NextGen4GCompile.RDS (i.e. where data upload to server was incomplete)
# docket.incomplete <- readRDS('C:/CloudStor/R_Stuff/MMLF/docket.incomplete.RDS')

docket.incomplete <- readRDS(paste(mm_data_folder, '/docket.incomplete.RDS', sep = ''))

docket.incomplete <- docket.incomplete %>% 
        mutate(docket.index = paste(zone, docketnum, plaindate, processor, sep = '-')) %>%  
        pull(docket.index)

# identify complete existing dockets
existing.dockets.complete <- setdiff(existing.dockets, docket.incomplete)

# identify all dockets in measuring board dataframe
docket.unique <- measure.board.next.gen.df %>% 
        mutate(docket.index = ifelse(processor == 'RALPHS TASMANIAN SEAFOODS PTY LTD' &
                                             sampyear == 2021, gsub('RALPHS TASMANIAN SEAFOODS PTY LTD',
                                   'TRUE SOUTH SEAFOOD', docket.index), docket.index)) %>%
        select(docketnum.day, plaindate, zone, processor, docket.index) %>%
        group_by(docketnum.day, plaindate, zone, processor, docket.index) %>% 
        summarise(n = n()) %>%    
        # mutate(docket.index = paste(zone, docketnum, plaindate, processor, sep = '-'))  
        pull(docket.index)

# identify dockets missing from summary folder
new.dockets <- setdiff(docket.unique, existing.dockets.complete)

# remove persistent docket AW-818662 from list (True South Seafoods)
new.dockets <- new.dockets[!grepl('AW-818662', new.dockets)]

# create a dataframe of weight grading threshold for plotting reference lines
grades <- data.frame(y = 0.35, x = c(300, 500, 700, 900), 
                     lab = c('XSmall', 'Small', 'Medium', 'Large'))

##---------------------------------------------------------------------------##
# 6. Plot 1: LW Summary ####
# combine weight and length summaryplot on the same page

for (i in new.dockets) {
 # create length plot
 plot.length.freq.dat <- measure.board.next.gen.df %>%
  filter(docket.index == i &
          between(shelllength, 100, 200))
 
 plot.n.measured <- measure.board.next.gen.df %>% 
  filter(!is.na(shelllength) &
          docket.index == i
         & between(shelllength, 100, 200)) %>%  
  summarise(n = paste('n =', n()))
 
 plot.zone <- unique(plot.length.freq.dat$zone)
 docketnum.day <- unique(plot.length.freq.dat$docketnum.day)
 
 length.freq.plot <- ggplot(plot.length.freq.dat, aes(shelllength)) +
  geom_histogram(
   aes(y = ..density.. * 5),
   fill = '#EFC000FF',
   col = 'black',
   binwidth = 5,
   alpha = 0.6
  ) +
  coord_cartesian(xlim = c(100, 200), ylim = c(0, 0.45)) +
  theme_bw() +
  xlab("Shell Length (mm)") +
  ylab(paste("Docket no.", plot.zone, docketnum.day, " Percentage (%)")) +
  geom_vline(aes(xintercept = case_when((zone == 'AW' & plaindate <= as.Date('2024-06-30')) ~ 145,
                                         (zone == 'AW' & plaindate >= as.Date('2024-07-01')) ~ 150,
                                         (zone == 'AE' & sampyear < 2023) ~ 138,
                                         (zone == 'AE' & sampyear == 2023) ~ 140,
                                         (zone == 'AE' & sampyear == 2024) ~ 142,
                                         (zone == 'AE' & sampyear == 2025) ~ 145,
                                         (zone == 'AG' & sampyear < 2023) ~ 145,
                                         (zone == 'AG' & sampyear >= 2023) ~ 150,
                                         (zone == 'AN') ~ 132,
                                         (zone == 'AB' ~ 114))),
             linetype = 'dashed',
             colour = 'red',
             size = 0.5)+
  # geom_vline(aes(xintercept = 138), colour = 'red',
  #            linetype = 'dashed', size = 0.5)+
  # geom_vline(
  #  aes(xintercept = ifelse(zone == 'AW', 145, 
  #                          ifelse(zone == 'AB', 114, 
  #                                 ifelse(zone == 'AN', 132, 
  #                                        ifelse(zone == 'AG', 145, 
  #                                               ifelse(zone == 'AE' & sampyear == 2023, 140, 
  #                                                      ifelse(zone == 'AE' & sampyear == 2024, 142, 
  #                                                             ifelse(zone == 'AW' & plaindate >= as.Date('2024-07-01'), 150, 138)))))))),
  #  linetype = 'dashed',
  #  colour = 'red',
  #  size = 0.5
  # ) +
  scale_y_continuous(labels = percent_format(accuracy = 1, suffix = ''))+
  geom_text(data = plot.n.measured, aes(x = 180, y = 0.3, label = n), color = 'black', size = 3)
 

 # print(length.freq.plot)
 
 xbp.len <- ggplot(plot.length.freq.dat,
                   aes(
                    x = factor(docketnum.day),
                    y = shelllength,
                    group = docketnum.day
                   )) +
  geom_boxplot(
   fill = 'lightgray',
   outlier.colour = "black",
   outlier.size = 1.5,
   position = position_dodge(0.85),
   width = 0.5
  ) +
  ggpubr::rotate() +
  theme_transparent()
 
 # print(xbp.len)
 
 xbp_grob <- ggplotGrob(xbp.len)
 xmin.len <- min(plot.length.freq.dat$shelllength)
 xmax.len <- max(plot.length.freq.dat$shelllength)
 
 length.plot <- length.freq.plot +
  annotation_custom(
   grob = xbp_grob,
   xmin = xmin.len,
   xmax = xmax.len,
   ymin = 0.3
  )
 
 # create weight plot
 
 plot.weight.freq.dat <- measure.board.next.gen.df %>%
  filter(docket.index == i &
          wholeweight != 0)
 
 plot.n.weighed <- measure.board.next.gen.df %>% 
  filter(docket.index == i &
          wholeweight != 0) %>% 
  summarise(n = paste('n =', n()))
 
 if (nrow(plot.weight.freq.dat) != 0) {
  
  weight.freq.plot <- ggplot(plot.weight.freq.dat, aes(wholeweight)) +
   geom_histogram(
    aes(y = ..density.. * 50),
    fill = '#0073C2FF',
    col = 'black',
    binwidth = 50,
    alpha = 0.6
   ) +
   coord_cartesian(xlim = c(100, 1300),
                   ylim = c(0, 0.35)) +
   theme_bw() +
   theme(panel.grid = element_blank()) +
   xlab("Whole weight (g)") +
   ylab(paste("Docket no.", plot.zone, docketnum.day, " Percentage (%)")) +
   geom_vline(aes(xintercept = 400),
              colour = 'red',
              linetype = 'dotted') +
   geom_vline(aes(xintercept = 600),
              colour = 'red',
              linetype = 'dotted') +
   geom_vline(aes(xintercept = 800),
              colour = 'red',
              linetype = 'dotted') +
   geom_text(
    data = grades,
    aes(
     x = x,
     y = y,
     label = lab
    ),
    inherit.aes = FALSE,
    vjust = 0.5,
    hjust = 0.5,
    size = 4,
    angle = 0,
    colour = c(
     'xsmall' = '#868686FF',
     'small' = '#EFC000FF',
     'medium' = '#0073C2FF',
     'large' = '#CD534CFF'
    )
   ) +
   scale_y_continuous(labels = percent_format(accuracy = 1, suffix = ''))+
   geom_text(data = plot.n.weighed, aes(x = 850, y = 0.25, label = n), color = 'black', size = 3)
  
  # print(weight.freq.plot)
  
  xbp.wt <- ggplot(
   plot.weight.freq.dat,
   aes(
    x = factor(docketnum.day),
    y = wholeweight,
    group = docketnum.day
   )
  ) +
   geom_boxplot(
    fill = 'lightgray',
    outlier.colour = "black",
    outlier.size = 1.5,
    position = position_dodge(0.85),
    width = 0.3
   ) +
   stat_summary(fun.y = mean, geom = 'point', shape = 20, size = 3, colour = 'red', fill = 'red')+
   ggpubr::rotate() +
   theme_transparent()
  
  # print(xbp.wt)
  
  xbp_grob <- ggplotGrob(xbp.wt)
  xmin.wt <- min(plot.weight.freq.dat$wholeweight)
  xmax.wt <- max(plot.weight.freq.dat$wholeweight)
  
  # weight.freq.plot +
  #         annotation_custom(grob = xbp_grob, xmin = xmin.wt, xmax = xmax.wt,  ymin = 0.17)
  
  # add pie chart to weight frequency plot
  
  docketnum.day.grade.summary <-
   left_join(docknum.n, docknum.grade.meas, by = c('docketnum.day', 'processor', 'plaindate', 'docket.index')) %>%
   mutate(grade.perc = round((grade.meas / ab.weighed) * 100))
  
  pie.plot.dat <- docketnum.day.grade.summary %>%
   filter(docket.index == i) %>%
   mutate(
    lab.position = cumsum(grade.perc),
    lab.y.position = lab.position - grade.perc / 2,
    lab = paste0(grade.perc, "%")
   )
  
  docket.pie.plot <-
   ggplot(data = pie.plot.dat, aes(
    x = "",
    y = grade.perc,
    fill = grade
   )) +
   geom_bar(stat = "identity", colour = 'white') +
   coord_polar(theta = "y") +
   geom_text(
    aes(label = lab),
    position = position_stack(vjust = 0.5),
    colour = 'white',
    size = 5
   ) +
   theme_void() +
   theme(legend.position = 'none') +
   scale_fill_manual(
    values = c(
     'xsmall' = '#868686FF',
     'small' = '#EFC000FF',
     'medium' = '#0073C2FF',
     'large' = '#CD534CFF'
    )
   )
  # labs(fill = (paste('Grade')))
  
  # print(docket.pie.plot)
  
  pp_grob <- ggplotGrob(docket.pie.plot)
  
  wt.plot <- weight.freq.plot +
   annotation_custom(
    grob = xbp_grob,
    xmin = xmin.wt,
    xmax = xmax.wt,
    ymin = 0.25
   ) +
   annotation_custom(
    grob = pp_grob,
    xmin = 900,
    xmax = 1200,
    ymax = 0.4
   )
  
  #combine length and weight plots
  
  plot.a <- grid.arrange(
   arrangeGrob(cowplot::plot_grid(wt.plot, length.plot, align = 'v', 
                                  ncol = 1), ncol = 1))
  
  #save plots
  # setwd('C:/CloudStor/R_Stuff/MMLF/MM_Plots/MM_Plots_2022ProcessorSummaries')
  file.zone <- unique(plot.length.freq.dat$zone)
  file.date <- unique(plot.length.freq.dat$plaindate)
  file.processor <- ifelse(unique(plot.length.freq.dat$processor) == 'RALPHS TASMANIAN SEAFOODS PTY LTD',
                           'TRUE SOUTH SEAFOOD', unique(plot.length.freq.dat$processor))
  
  
  
  ggsave(
   filename = paste(paste(target_year_folder, file.zone, docketnum.day, sep = ''), '_SUMMARYPLOT_', file.date, '_', file.processor, '.pdf', sep = ''),
   plot = plot.a,
   width = 200,
   height = 297,
   units = 'mm')
 }
 else{
  plot.a <- length.plot
  # plot.a <- grid.arrange(
  #         arrangeGrob(cowplot::plot_grid(length.plot, align = 'v', 
  #                                        ncol = 1), ncol = 1))
  #save plots
  # setwd('C:/CloudStor/R_Stuff/MMLF/MM_Plots/MM_Plots_2022ProcessorSummaries')
  file.zone <- unique(plot.length.freq.dat$zone)
  file.date <- unique(plot.length.freq.dat$plaindate)
  file.processor <- ifelse(unique(plot.length.freq.dat$processor) == 'RALPHS TASMANIAN SEAFOODS PTY LTD',
                           'TRUE SOUTH SEAFOOD', unique(plot.length.freq.dat$processor))
  
  
  
  ggsave(
   filename = paste(paste(target_year_folder, file.zone, docketnum.day, sep = ''), '_SUMMARYPLOT_', file.date, '_', file.processor, '.pdf', sep = ''),
   plot = plot.a,
   width = 7.4,
   height = 5.57,
   units = 'in')
 }
}

#-----------------------------------------------------------------------------##
# 7. Processor Data Export ####
## raw data Excel export for processors

df.1 <- measure.board.next.gen.df %>% 
 mutate(grade = dplyr::if_else(wholeweight == 0, NA_character_,
                               dplyr::if_else(between(wholeweight, 1, 600), 'small', #0-400g can also be labelled xsmall
                                              dplyr::if_else(between(wholeweight, 601, 800), 'medium', 'large'))),
        wholeweight = replace(wholeweight, wholeweight == 0, NA))

for(i in processors){
 df.2 <- df.1 %>%
  filter(sampyear == target_year,
         processor == i)
 
 write.xlsx(df.2,
            file = paste(target_year_folder, i, '_MeasuringBoardData_', target_year, '.xlsx'),
            sheetName = "Sheet1",
            col.names = TRUE,
            row.names = TRUE,
            append = FALSE)
}

##----------------------------------------------------------------------------##
# 8. Seafood Traders Divers ####

seafood.traders.divers.2023 <- read.xlsx("C:/cloudstor/DiveFisheries/Abalone/Assessment/Figures/MM/2023/MM_Plots_2023ProcessorSummaries/SeafoodTraders_2023_DiverDetails.xlsx",
                                         detectDates = T)

summary.month <- lubridate::month(Sys.time(), label = T, abbr = FALSE)
summary.year <- year(Sys.time())

seafood.traders.divers.2023 <- seafood.traders.divers.2023 %>% 
 distinct(docketnum, divedate, .keep_all = T) %>% 
 group_by(docketnum, diver) %>% 
 summarise(divedate = max(divedate)) %>% 
 mutate(docketnum = trimws(docketnum))

seafood.traders.grade.summary <- left_join(docknum.n, docknum.grade.meas, by = c('docketnum.day', 'processor', 'plaindate')) %>% 
 filter(processor == "SEAFOOD TRADERS PTY LTD") %>% 
 mutate(grade.perc = round((grade.meas / ab.weighed) * 100)) %>%   
 ungroup() %>% 
 select(-c(grade.meas, processor)) %>% 
 spread(grade, grade.perc) %>% 
 dplyr::rename("Large\n(%)" = large, "Medium\n(%)" = medium, "Small\n(%)" = small) %>%
 {if('xsmall' %in% names(.)) rename(., "XSmall (%)" = xsmall) else .} %>% 
 arrange(desc(plaindate)) %>%
 ungroup() %>%  
 mutate(docketnum = paste(zone, docketnum.x, sep = '')) %>%
 dplyr::rename('Docket\nno.' = docketnum,
               'Abalone\nmeasured' = ab.weighed) %>% 
 select(-zone) %>% 
 as.data.frame()

seafood.traders.grade.summary <- left_join(seafood.traders.grade.summary, seafood.traders.divers.2023, by = c('Docket\nno.' = 'docketnum')) %>%  
 select(divedate, diver, 'Docket\nno.', plaindate, 'Abalone\nmeasured', 'Large\n(%)', 'Medium\n(%)', 'Small\n(%)') %>% 
 filter(plaindate >= as.Date("2023-01-01")) %>% 
 dplyr::rename('Dive\ndate' = divedate,
               'Diver\nname' = diver,
               'Date\nsampled' = plaindate,
               'Number\nsampled' = 'Abalone\nmeasured')

seafood.traders.grade.summary.formated <- seafood.traders.grade.summary %>% 
 ggpubr::ggtexttable(rows = NULL, theme = ggpubr::ttheme('mOrange'))

# setwd('C:/CloudStor/R_Stuff/MMLF/MM_Plots/MM_Plots_2022ProcessorSummaries')
setwd('C:/cloudstor/DiveFisheries/Abalone/Assessment/Figures/MM/2023/MM_Plots_2023ProcessorSummaries')

write.xlsx(seafood.traders.grade.summary, paste('SEAFOOD TRADERS PTY LTD', 'DIVERSUMMARY', summary.month, summary.year, '.xlsx', sep = '_'), 
           sheetName = "Sheet1",
           col.names = TRUE, row.names = TRUE, append = FALSE)

ggsave(
 filename = paste('SEAFOOD TRADERS PTY LTD', 'DIVERSUMMARY', summary.month, summary.year, '.pdf', sep = '_'),
 plot = seafood.traders.grade.summary.formated,
 width = 200,
 height = 300,
 units = 'mm'
)
##----------------------------------------------------------------------------##
# # Tas Seafoods Divers
# ## Tasmanian Seafoods Diver Summary - Mark Fleming
# 
# # tas.seafoods.divers.2020 <- read.xlsx("C:/CloudStor/R_Stuff/MMLF/MM_Plots/MM_Plots_2020ProcessorSummaries/TasmaniaSeafoods_DiverDetails_2020.xlsx",
# #                        detectDates = T)
# 
# tas.seafoods.divers.2022 <- read.xlsx("C:/CloudStor/R_Stuff/MMLF/MM_Plots/MM_Plots_2022ProcessorSummaries/TasmaniaSeafoods_DiverDetails_2022.xlsx",
#                                       detectDates = T)
# 
# 
# summary.month <- lubridate::month(Sys.time(), label = T, abbr = FALSE)
# summary.year <- year(Sys.time())
# 
# tas.seafoods.divers <- tas.seafoods.divers.2022 %>% 
#         distinct(docketnum, divedate, .keep_all = T) %>% 
#         group_by(docketnum, diver) %>% 
#         summarise(divedate = max(divedate)) %>% 
#         mutate(docketnum = trimws(docketnum))
# 
# 
# tas.seafoods.grade.summary <- left_join(docknum.n, docknum.grade.meas, by = c('docketnum', 'processor', 'plaindate')) %>% 
#         filter(processor == "TASMANIAN SEAFOODS PTY LTD") %>% 
#         mutate(grade.perc = round((grade.meas / ab.weighed) * 100)) %>%   
#         ungroup() %>% 
#         select(-c(grade.meas, processor)) %>% 
#         spread(grade, grade.perc) %>% 
#         dplyr::rename("Large\n(%)" = large, "Medium\n(%)" = medium, "Small\n(%)" = small) %>%
#         {if('xsmall' %in% names(.)) rename(., "XSmall (%)" = xsmall) else .} %>% 
#         arrange(desc(plaindate)) %>%
#         ungroup() %>% 
#         mutate(docketnum = paste(zone, docketnum, sep = '')) %>%
#         dplyr::rename('Sample\ndate' = plaindate,
#                'Docket\nno.' = docketnum,
#                'Abalone\nmeasured' = ab.weighed) %>% 
#         select(-zone) %>% 
#         as.data.frame()
# 
# tas.seafoods.grade.summary <- left_join(tas.seafoods.grade.summary, tas.seafoods.divers, by = c('Docket\nno.' = 'docketnum')) %>%  
#         select(divedate, diver, 'Docket\nno.', 'Sample\ndate', 'Abalone\nmeasured', 'Large\n(%)', 'Medium\n(%)', 'Small\n(%)') %>%  
#         filter(divedate >= as.Date("2021-01-01")) %>% 
#         dplyr::rename('Dive\ndate' = divedate,
#                'Diver\nname' = diver,
#                'Date\nsampled' = 'Sample\ndate',
#                'Number\nsampled' = 'Abalone\nmeasured') 
#         
# 
# tas.seafoods.grade.summary.formated <- tas.seafoods.grade.summary %>% 
#         ggpubr::ggtexttable(rows = NULL, theme = ggpubr::ttheme('mOrange'))
# 
# # print(tas.seafoods.grade.summary.formated)
# 
# setwd('C:/CloudStor/R_Stuff/MMLF/MM_Plots/MM_Plots_2022ProcessorSummaries')
# 
# write.xlsx(tas.seafoods.grade.summary, paste('TASMANIAN SEAFOODS PTY LTD', 'DIVERSUMMARY', summary.month, summary.year, '.xlsx', sep = '_'), 
#            sheetName = "Sheet1",
#            col.names = TRUE, row.names = TRUE, append = FALSE)
# 
# ggsave(
#         filename = paste('TASMANIAN SEAFOODS PTY LTD', 'DIVERSUMMARY', summary.month, summary.year, '.pdf', sep = '_'),
#         plot = tas.seafoods.grade.summary.formated,
#         width = 200,
#         height = 400,
#         units = 'mm'
# )
# 
# 
# ##----------------------------------------------------------------------------##
# # True South Divers
# 
# ralphs.divers.2021 <- read.xlsx("C:/CloudStor/R_Stuff/MMLF/MM_Plots/MM_Plots_2021ProcessorSummaries/RalphsTasmanianSeafoods_DiverDetails_2021.xlsx",
#                                          detectDates = T)
# 
# summary.month <- month(Sys.time(), label = T, abbr = FALSE)
# summary.year <- year(Sys.time())
# 
# ralphs.divers.2021 <- ralphs.divers.2021 %>% 
#         distinct(docketnum, divedate, .keep_all = T) %>% 
#         group_by(docketnum, diver) %>% 
#         summarise(divedate = max(divedate)) %>% 
#         mutate(docketnum = trimws(docketnum))
# 
# ralphs.grade.summary <- left_join(docknum.n, docknum.grade.meas, by = c('docketnum', 'docketnum.day', 'processor', 'plaindate', 'samptime.max', 'docket.index')) %>% 
#         filter(processor == "RALPHS TASMANIAN SEAFOODS PTY LTD") %>% 
#         mutate(grade.perc = round((grade.meas / ab.weighed) * 100)) %>%   
#         ungroup() %>% 
#         select(-c(grade.meas, processor)) %>% 
#         spread(grade, grade.perc) %>% 
#         dplyr::rename("Large\n(%)" = large, "Medium\n(%)" = medium, "Small\n(%)" = small) %>%
#         {if('xsmall' %in% names(.)) rename(., "XSmall (%)" = xsmall) else .} %>% 
#         arrange(desc(plaindate)) %>%
#         ungroup() %>% 
#         mutate(docketnum = paste(zone, docketnum, sep = ''),
#                docketnum.day = paste(zone, docketnum.day, sep = '')) %>%
#         dplyr::rename('Sample\ndate' = plaindate,
#                       'Docket\nno.' = docketnum,
#                       'Abalone\nmeasured' = ab.weighed) %>% 
#         select(-zone) %>% 
#         as.data.frame()
# 
# ralphs.grade.summary <- left_join(ralphs.grade.summary, ralphs.divers.2021, by = c('Docket\nno.' = 'docketnum')) %>%  
#         select(docketnum.day, divedate, diver, 'Docket\nno.', 'Sample\ndate', 'Abalone\nmeasured', 'Large\n(%)', 'Medium\n(%)', 'Small\n(%)') %>%  
#         filter(divedate >= as.Date("2020-12-29")) %>%
#         dplyr::rename('Dive\ndate' = divedate,
#                       'Diver\nname' = diver,
#                       'Date\nsampled' = 'Sample\ndate',
#                       'Number\nsampled' = 'Abalone\nmeasured') %>% 
#         mutate('Docket\nno.' = docketnum.day) %>% 
#         select(c('Dive\ndate', 'Diver\nname', 'Docket\nno.', 'Date\nsampled', 'Number\nsampled', 'Large\n(%)', 'Medium\n(%)', 'Small\n(%)'))
# 
# ralphs.grade.summary.formated <- ralphs.grade.summary %>% 
#         ggpubr::ggtexttable(rows = NULL, theme = ggpubr::ttheme('mOrange'))
# 
# setwd('C:/CloudStor/R_Stuff/MMLF/MM_Plots/MM_Plots_2021ProcessorSummaries')
# ggsave(
#         filename = paste('TRUE SOUTH SEAFOOD', 'DIVERSUMMARY', summary.month, summary.year, '.pdf', sep = '_'),
#         plot = ralphs.grade.summary.formated,
#         width = 200,
#         height = 300,
#         units = 'mm'
# )
# 
##----------------------------------------------------------------------------##
