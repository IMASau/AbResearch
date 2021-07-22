# This script provides a preliminary analysis of measuring board data prior to catch dockets being processed
# by DPIPWE and further catch details becoming available. These analysis are intended to provide processors with
# near real-time summary of catches which they have measured.

# Created by: Jaime McAllister

##-------------------------------------------------------------------------------------------------------##
# load libaries ####
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
##-------------------------------------------------------------------------------------------------------##
# load data ####

# load latest measuring board data that comes from MM_NextGen_4G.R script
# measure.board.df <- readRDS('C:/CloudStor/R_Stuff/MMLF/measure.board.df.RDS')
measure.board.next.gen.df <- readRDS('C:/CloudStor/R_Stuff/MMLF/measure.board.next.gen.df.RDS')

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
        filter(local_date >= as.Date('2020-12-31')) %>% 
        group_by(processor) %>% 
        summarise(catches.measured = n_distinct(docketnum),
                  n = n(),
                  last.sample = max(plaindate)) %>% 
        janitor::adorn_totals() %>% 
        as.data.frame()

# add weight grading categories used by processors
# mb.next.gen.grade.df <- measure.board.next.gen.df %>%
#         filter(wholeweight != 0) %>%
#         mutate(grade = dplyr::if_else(wholeweight <= 400, 'small', #0-400g can also be labelled xsmall
#                                       dplyr::if_else(between(wholeweight, 401, 600), 'small',
#                                                      dplyr::if_else(between(wholeweight, 601, 800), 'medium', 'large'))))

mb.next.gen.grade.df <- measure.board.next.gen.df %>% 
        mutate(grade = dplyr::if_else(wholeweight == 0, NA_character_,
                                      dplyr::if_else(processor == 'RALPHS TASMANIAN SEAFOODS PTY LTD' & between(wholeweight, 1, 400), 'xsmall', #0-400g can also be labelled xsmall requested by RTS
                                      dplyr::if_else(between(wholeweight, 1, 600), 'small', 
                                                     dplyr::if_else(between(wholeweight, 601, 800), 'medium', 'large')))),
               wholeweight = replace(wholeweight, wholeweight == 0, NA))

# list of unique processors for summary and plot loops
processors <- unique(mb.next.gen.grade.df$processor)

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

##-------------------------------------------------------------------------------------------------------##
# quick summary
dock.sum <- measure.board.next.gen.df %>% 
        filter(sampyear == 2021) %>% 
        group_by(processor) %>% 
        summarise(n = n(),
                  catches = n_distinct(docketnum)) %>% 
        janitor::adorn_totals()

##-------------------------------------------------------------------------------------------------------##
## Summ Tab: length/weight and grade summary tables  ####

for(i in processors){

# join number of abalone measured per docket and grade, and calculate percentage measured per grade to 
# create grade summary table
grade.summary <- left_join(docknum.n, docknum.grade.meas, by = c('docket.index', 'docketnum', 'docketnum.day', 'processor', 'plaindate', 'samptime.max')) %>% 
        filter(processor == i) %>%  
        mutate(grade.perc = round((grade.meas / ab.weighed) * 100),
               sampyear = year(plaindate)) %>%  
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
        filter(wholeweight != 0 & processor == i) %>% 
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

setwd('C:/CloudStor/R_Stuff/MMLF/MM_Plots/MM_Plots_2021ProcessorSummaries')
write.xlsx(grade.summary,
           file = paste(i, '_GradeSummary_', Sys.Date(), '.xlsx'),
           sheetName = "Sheet1",
           col.names = TRUE,
           row.names = TRUE,
           append = FALSE)
write.xlsx(length.weight.summary,
           file = paste(i, '_SizeWeightSummary_', Sys.Date(), '.xlsx'),
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
        filename = paste(i, '_WEIGHTGRADESUMMARY_', Sys.Date(), '.pdf', sep = ''),
        plot = grade.summary.formated,
        width = 200,
        height = 350,
        units = 'mm'
)

ggsave(
        filename = paste(i, '_LENGTHWEIGHTSUMMARY_', Sys.Date(), '.pdf', sep = ''),
        plot = length.weight.summary.formated,
        width = 250,
        height = 300,
        units = 'mm'
)
}
else{
}
}

##---------------------------------------------------------------------------##
## Plot setup  ####

## search for existing docket summaries on file and identify new dockets in the compiled
## measuring board data frame for which to create summaries

# identify local working folder containing existing docket summaries
processor.summaries.2021 <- 'C:/CloudStor/R_Stuff/MMLF/MM_Plots/MM_Plots_2021ProcessorSummaries'
processor.summaries.2020 <- 'C:/CloudStor/R_Stuff/MMLF/MM_Plots/MM_Plots_2020ProcessorSummaries'



# list filenames of existing docket summaries in folder
docket.summaries.2020 <- list.files(processor.summaries.2020,  pattern = "^AW.*pdf|^AE.*pdf|^AB.*pdf|^AN.*pdf|^AG.*pdf", full.names = F)
docket.summaries.2021 <- list.files(processor.summaries.2021,  pattern = "^AW.*pdf|^AE.*pdf|^AB.*pdf|^AN.*pdf|^AG.*pdf", full.names = F)

docket.summaries <- c(docket.summaries.2020, docket.summaries.2021)


# create a vector of existing docket numbers 
existing.dockets <- as.data.frame(docket.summaries) %>%  
        separate(docket.summaries, c('docket.number',
                                     'docket.plot',
                                     'docket.date',
                                     'docket.processor'), sep = '_') %>%   
        select(docket.number, docket.date, docket.processor) %>% 
        separate(docket.number, into = c('docket.zone', 'docket.number'), "(?<=[A-Z])(?=[0-9])") %>% 
        mutate(docket.number = as.numeric(docket.number),
               docket.processor = gsub('.pdf', '', docket.processor)) %>% 
        group_by_all() %>% 
        summarise(n = n()) %>%  
        mutate(summary.plot.exists = ifelse(!is.na(n), 1, 0),
               docket.index = paste(docket.zone, docket.number, docket.date, docket.processor, sep = '-')) %>%  
        pull(docket.index)

# load vector of incomplete measureboard data for existing docket numbers determined in 
# MM_NextGen4GCompile.RDS
docket.incomplete <- readRDS('C:/CloudStor/R_Stuff/MMLF/docket.incomplete.RDS')

docket.incomplete <- docket.incomplete %>% 
        mutate(docket.index = paste(zone, docketnum, plaindate, processor, sep = '-')) %>% 
        pull(docket.index)

# identify complete existing dockets
existing.dockets.complete <- setdiff(existing.dockets, docket.incomplete)

# df.3 <- left_join(existing.dockets, docket.incomplete, by = 'docket.index')

# identify all dockets in measuring board dataframe
# docket.unique <- unique(measure.board.next.gen.df$docketnum)

docket.unique <- measure.board.next.gen.df %>% 
        select(docketnum.day, plaindate, zone, processor, docket.index) %>% 
        group_by(docketnum.day, plaindate, zone, processor, docket.index) %>% 
        summarise(n = n()) %>%   
        # mutate(docket.index = paste(zone, docketnum, plaindate, processor, sep = '-')) %>%  
        pull(docket.index)

# df.2 <- existing.dockets %>% 
#         mutate(docket.date = as.Date(docket.date)) %>% 
#         left_join(df.1, ., by = c(docketnum = 'docket.number',
#                   zone = 'docket.zone', plaindate = 'docket.date')) %>% 
#         filter(is.na(summary.plot.exists)) %>% 
#         mutate(docket.index = paste(zone, docketnum, plaindate, sep = '-'))
# 
# new.dockets <- unique(df.2$docket.index)

# identify dockets missing from summary folder
# new.dockets <- setdiff(docket.unique, existing.dockets)
new.dockets <- setdiff(docket.unique, existing.dockets.complete)

# # idenitfy processors in measuring board dataframe
# processor.unique <- unique(measure.board.next.gen.df$processor)

# create a dataframe of weight grading threshold for plotting reference lines
grades <- data.frame(y = 0.35, x = c(300, 500, 700, 900), 
                     lab = c('XSmall', 'Small', 'Medium', 'Large'))

##---------------------------------------------------------------------------##
## Plot 1: Length ####
# overlay length frequency histogram with boxplot

# measure.board.next.gen.df <- measure.board.next.gen.df %>% 
#         mutate(docket.index = paste(zone, docketnum, plaindate, sep = '-'))

for (i in new.dockets) {
        plot.length.freq.dat <- measure.board.next.gen.df %>%
                filter(docket.index == i &
                               between(shelllength, 100, 200))
        
        plot.n.measured <- measure.board.next.gen.df %>% 
                filter(!is.na(shelllength) &
                               docket.index == i
                       & between(shelllength, 100, 200)) %>%  
                summarise(n = paste('n =', n()))
        
        plot.zone <- unique(plot.length.freq.dat$zone)
        docketnum <- unique(plot.length.freq.dat$docketnum)
        
        length.freq.plot <- ggplot(plot.length.freq.dat, aes(shelllength)) +
                geom_histogram(
                        aes(y = ..density.. * 5),
                        fill = '#EFC000FF',
                        col = 'black',
                        binwidth = 5,
                        alpha = 0.6
                ) +
                coord_cartesian(xlim = c(100, 200), ylim = c(0, 0.4)) +
                theme_bw() +
                xlab("Shell Length (mm)") +
                ylab(paste("Docket no.", plot.zone, docketnum, " Percentage (%)")) +
                # geom_vline(aes(xintercept = 138), colour = 'red',
                #            linetype = 'dashed', size = 0.5)+
                geom_vline(
                        aes(xintercept = ifelse(zone == 'AW', 145, 
                                                ifelse(zone == 'AB', 114, 
                                                       ifelse(zone == 'AN', 127, 138)))),
                        linetype = 'dashed',
                        colour = 'red',
                        size = 0.5
                ) +
                scale_y_continuous(labels = percent_format(accuracy = 1, suffix = ''))+
                geom_text(data = plot.n.measured, aes(x = 180, y = 0.3, label = n), color = 'black', size = 3)
        
        # print(length.freq.plot)
        
        xbp.len <- ggplot(plot.length.freq.dat,
                          aes(
                                  x = factor(docketnum),
                                  y = shelllength,
                                  group = docketnum
                          )) +
                geom_boxplot(
                        fill = 'lightgray',
                        outlier.colour = "black",
                        outlier.size = 1.5,
                        position = position_dodge(0.85),
                        width = 0.5
                ) +
                rotate() +
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
        
        setwd('C:/CloudStor/R_Stuff/MMLF/MM_Plots/MM_Plots_2021ProcessorSummaries')
        file.zone <- unique(plot.length.freq.dat$zone)
        file.date <- unique(plot.length.freq.dat$plaindate)
        file.processor <- unique(plot.length.freq.dat$processor)
        ggsave(
                filename = paste(paste(file.zone, docketnum, sep = ''), '_LENGTHSUMMARYPLOT_', file.date, '_', file.processor, '.pdf', sep = ''),
                plot = length.plot,
                width = 7.4,
                height = 5.57,
                units = 'in'
        )
        
}

## Plot 2: Weight####
# overlay weight frequency with boxplot and pie chart of grading

for (j in new.dockets) {
                plot.weight.freq.dat <- measure.board.next.gen.df %>%
                        filter(docket.index == j &
                                       wholeweight != 0)
                
                plot.n.weighed <- measure.board.next.gen.df %>% 
                        filter(docket.index == j &
                               wholeweight != 0) %>% 
                        summarise(n = paste('n =', n()))
                
                plot.zone <- unique(plot.weight.freq.dat$zone)
                docketnum <- unique(plot.weight.freq.dat$docketnum)
                
                weight.freq.plot <- ggplot(plot.weight.freq.dat, aes(wholeweight)) +
                        geom_histogram(
                                aes(y = ..density.. * 50),
                                fill = '#0073C2FF',
                                col = 'black',
                                binwidth = 50,
                                alpha = 0.6
                        ) +
                        coord_cartesian(xlim = c(100, 1200),
                                        ylim = c(0, 0.35)) +
                        theme_bw() +
                        theme(panel.grid = element_blank()) +
                        xlab("Whole weight (g)") +
                        ylab(paste("Docket no.", plot.zone, docketnum, " Percentage (%)")) +
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
                                x = factor(docketnum),
                                y = wholeweight,
                                group = docketnum
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
                        rotate() +
                        theme_transparent()
                
                # print(xbp.wt)
                
                xbp_grob <- ggplotGrob(xbp.wt)
                xmin.wt <- min(plot.weight.freq.dat$wholeweight)
                xmax.wt <- max(plot.weight.freq.dat$wholeweight)
                
                # weight.freq.plot +
                #         annotation_custom(grob = xbp_grob, xmin = xmin.wt, xmax = xmax.wt,  ymin = 0.17)
                
                # add pie chart to weight frequency plot
                docketnum.grade.summary <-
                        left_join(docknum.n, docknum.grade.meas, by = c('docketnum', 'processor', 'plaindate', 'docket.index')) %>%
                        mutate(grade.perc = round((grade.meas / ab.weighed) * 100))
                
                pie.plot.dat <- docketnum.grade.summary %>%
                        filter(docket.index == j) %>%
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
                
                setwd('C:/CloudStor/R_Stuff/MMLF/MM_Plots/MM_Plots_2021ProcessorSummaries')
                file.zone <- unique(plot.weight.freq.dat$zone)
                file.date <- unique(plot.weight.freq.dat$plaindate)
                file.processor <- unique(plot.weight.freq.dat$processor)
                ggsave(
                        filename = paste(paste(file.zone, docketnum, sep = ''), '_WEIGHTSUMMARYPLOT_', file.date, '_', file.processor, '.pdf', sep = ''),
                        plot = wt.plot,
                        width = 7.4,
                        height = 5.57,
                        units = 'in'
                )
        }

#---------------------------------------------------------------------------##
## Plot 2b: LT + WT combined ####
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
                # geom_vline(aes(xintercept = 138), colour = 'red',
                #            linetype = 'dashed', size = 0.5)+
                geom_vline(
                        aes(xintercept = ifelse(zone == 'AW', 145, 
                                                ifelse(zone == 'AB', 114, 
                                                       ifelse(zone == 'AN', 127, 
                                                              ifelse(zone == 'AG', 145, 138))))),
                        linetype = 'dashed',
                        colour = 'red',
                        size = 0.5
                ) +
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
                rotate() +
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
                rotate() +
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
        setwd('C:/CloudStor/R_Stuff/MMLF/MM_Plots/MM_Plots_2021ProcessorSummaries')
        file.zone <- unique(plot.length.freq.dat$zone)
        file.date <- unique(plot.length.freq.dat$plaindate)
        file.processor <- ifelse(unique(plot.length.freq.dat$processor) == 'RALPHS TASMANIAN SEAFOODS PTY LTD',
                                 'TRUE SOUTH SEAFOOD', unique(plot.length.freq.dat$processor))
        
        ggsave(
                filename = paste(paste(file.zone, docketnum.day, sep = ''), '_SUMMARYPLOT_', file.date, '_', file.processor, '.pdf', sep = ''),
                plot = plot.a,
                width = 200,
                height = 297,
                units = 'mm')
        
}

#---------------------------------------------------------------------------##

## Plot 3: Grades ####
## pie chart of weight grades by docketnum

docketnum.grade.summary <- left_join(docknum.n.meas, docknum.grade.meas, by = 'docketnum') %>% 
        mutate(grade.perc = round((grade.meas / ab.meas) * 100))

docket.nums <- unique(docketnum.grade.summary$docketnum)

for(i in docket.nums){
        
        pie.plot.dat <- docketnum.grade.summary %>%
                filter(docketnum == i) %>%
                mutate(lab.position = cumsum(grade.perc),
                       lab.y.position = lab.position - grade.perc / 2,
                       lab = paste0(grade.perc, "%"))
        
        docket.pie.plot <- ggplot(data = pie.plot.dat, aes(x = "", y = grade.perc, fill = grade)) +
                # ggplot(pie.plot.dat, aes(x = '', weight = grade.perc, fill = grade)) +
                # geom_bar(width = 1, position = "stack", colour = 'white') +
                geom_bar(stat = "identity", colour = 'white') +
                coord_polar(theta = "y") +
                # geom_text(aes(x = 1, y = lab.position, label = lab), colour = 'white', size = 5)+
                # geom_text(aes(x = 1, y = lab.y.position, label = lab), colour = 'white', size = 5)+
                geom_text(aes(label = lab), position = position_stack(vjust = 0.5), colour = 'white', size = 5) +
                theme(axis.ticks = element_blank(), axis.title = element_blank(), 
                      axis.text.y = , panel.grid  = element_blank(),
                      axis.text.x = element_blank(), panel.background = element_blank())+
                # scale_fill_brewer(palette = 'Set1')+
                # scale_fill_jco()+
                scale_fill_manual(values = c('xsmall' = '#868686FF',
                                             'small' = '#EFC000FF',
                                             'medium' = '#0073C2FF',
                                             'large' = '#CD534CFF'))+
                # scale_fill_manual(values = c('xsmall' = 'orange', 
                #                              'small' = 'green', 
                #                              'medium' = 'blue', 
                #                              'large' = 'red'))+
                labs(fill = (paste('Docket no:', i, '\n\nGrade')))
        
        print(docket.pie.plot)
        
        setwd('C:/CloudStor/R_Stuff/MMLF/MM_Plots/MM_Plots_2021ProcessorSummaries')
        ggsave(
                filename = paste('Docket no. ', i, '_weightgrade_piechart_pre-docket_2020', '.pdf', sep = ''),
                plot = docket.pie.plot,
                width = 7.4,
                height = 5.57,
                units = 'in'
        )
        ggsave(
                filename = paste('Docket no. ', i, '_weightgrade_piechart_pre-docket_2020', '.wmf', sep = ''),
                plot = docket.pie.plot,
                width = 7.4,
                height = 5.57,
                units = 'in'
        )
        ggsave(
                filename = paste('Docket no. ', i, '_weightgrade_piechart_pre-docket_2020', '.png', sep = ''),
                plot = docket.pie.plot,
                width = 7.4,
                height = 5.57,
                units = 'in'
        )
        
}

##---------------------------------------------------------------------------##
## Plot 4: Grade multi ####
## faceted pie chart of weight grades by all docketnum

multi.pie.plot.dat <- docketnum.grade.summary %>%
        group_by(docketnum, grade) %>% 
        mutate(lab.position = cumsum(grade.perc),
               lab.y.position = lab.position - grade.perc / 2,
               lab = paste0(grade.perc, "%"))

ggplot(data = multi.pie.plot.dat, aes(x = "", y = grade.perc, fill = grade)) + 
        geom_bar(stat = "identity", colour = 'white') +
        geom_text(aes(label = lab), position = position_stack(vjust = 0.5), colour = 'white') +
        coord_polar(theta = "y") +
        facet_wrap(.~docketnum, nrow = 3)+
        theme(axis.ticks = element_blank(), axis.title = element_blank(), 
              axis.text.y = , panel.grid  = element_blank(),
              axis.text.x = element_blank(), panel.background = element_blank(), 
              panel.border = element_blank(), strip.background = element_blank())+
        scale_fill_manual(values = c('xsmall' = '#868686FF',
                                     'small' = '#EFC000FF',
                                     'medium' = '#0073C2FF',
                                     'large' = '#CD534CFF'))+
        labs(fill = 'Weight grade')

##---------------------------------------------------------------------------##
## Plot 5: WT BP ####
## box plot of weights by docketnum per processor (inc. grade intervals)

for (i in processors) {
        
        weight.plot.dat <- mb.next.gen.grade.df %>%
                filter(!is.na(wholeweight)
                       & between(wholeweight, 1, 2000)
                       & processor == i)
        
        plotdat.n <- weight.plot.dat %>%
                group_by(docketnum) %>%
                summarize(n = n())
        
        dockets <- length(unique(weight.plot.dat$docketnum))
        
        grades <- data.frame(x = dockets + 0.5, y = c(500, 700, 900), lab = c('small', 'medium', 'large'))
        
        weight.boxplot <-
                ggplot(weight.plot.dat, aes(
                        x = factor(docketnum),
                        y = wholeweight,
                        group = docketnum
                )) +
                # geom_boxplot(outlier.colour = "black", outlier.size = 1.5, position = position_dodge(preserve = 'single')) +
                geom_boxplot(
                        outlier.colour = "black",
                        outlier.size = 1.5,
                        position = position_dodge(0.85)
                ) +
                geom_text(
                        data = plotdat.n,
                        aes(y = 1000, label = n),
                        size = 3,
                        angle = 0,
                        position = position_dodge(width = 0.85)
                ) +
                # geom_hline(aes(yintercept = 132), colour = 'red', linetype = 'dotted')+
                xlab('Docket number') +
                ylab(paste('Whole weight (g)')) +
                coord_cartesian(ylim = c(300, 1000)) +
                theme_bw() +
                theme(
                        plot.title = element_text(hjust = 0.5),
                        axis.line = element_line(colour = "black"),
                        axis.text.x = element_text(angle = 0, vjust = 0.5),
                        legend.position = 'top'
                )+
                geom_hline(aes(yintercept = 400), colour = 'red', linetype = 'dotted')+
                geom_hline(aes(yintercept = 600), colour = 'red', linetype = 'dotted')+
                geom_hline(aes(yintercept = 800), colour = 'red', linetype = 'dotted')+
                geom_text(data = grades, aes(x = x, y = y, label = lab), inherit.aes = FALSE, vjust = 0.5, hjust = 0.5, size = 4, angle = 270)
        
        print(weight.boxplot)
        
        setwd('C:/CloudStor/R_Stuff/MMLF/MM_Plots/MM_Plots_2021ProcessorSummaries')
        ggsave(
                filename = paste(i, '_weight_pre-docket_boxplot_2020', '.pdf', sep = ''),
                plot = weight.boxplot,
                width = 7.4,
                height = 5.57,
                units = 'in'
        )
        ggsave(
                filename = paste(i, '_weight_pre-docket_boxplot_2020', '.wmf', sep = ''),
                plot = weight.boxplot,
                width = 7.4,
                height = 5.57,
                units = 'in'
        )
        ggsave(
                filename = paste(i, '_weight_pre-docket_boxplot_2020', '.png', sep = ''),
                plot = weight.boxplot,
                width = 7.4,
                height = 5.57,
                units = 'in'
        )
}

##---------------------------------------------------------------------------##
## Plot 6: LF Histo ####
## length frequency of catch per docket

for (i in docket.nums){
        
        plot.length.freq.dat <- mb.next.gen.grade.df %>% 
                filter(docketnum == i &
                               between(shelllength, 120, 200))
        
        length.freq.plot <- ggplot(plot.length.freq.dat, aes(shelllength)) +
                geom_histogram(aes(y = ..density.. * 5),
                               fill = 'white',
                               col = 'black',
                               binwidth = 5) +
                coord_cartesian(xlim = c(130, 200), ylim = c(0, 0.4)) +
                theme_bw() +
                xlab("Shell Length (mm)")+
                ylab(paste("Docket no.", i, " Percentage (%)")) +
                # geom_vline(aes(xintercept = 138), colour = 'red', 
                #            linetype = 'dashed', size = 0.5)+
                geom_vline(aes(xintercept = ifelse(zone == 'AW', 145, 138)),
                           linetype = 'dashed', colour = 'red', size = 0.5)
        
        print(length.freq.plot)
        
        setwd('C:/CloudStor/R_Stuff/MMLF/MM_Plots/MM_Plots_2021ProcessorSummaries')
        
        ggsave(
                filename = paste('Docket no. ', i, '_lengthfrequency_pre-docket_2020', '.pdf', sep = ''),
                plot = length.freq.plot,
                width = 7.4,
                height = 5.57,
                units = 'in'
        )
        ggsave(
                filename = paste('Docket no. ', i, '_lengthfrequency_pre-docket_2020', '.wmf', sep = ''),
                plot = length.freq.plot,
                width = 7.4,
                height = 5.57,
                units = 'in'
        )
        ggsave(
                filename = paste('Docket no. ', i, '_lengthfrequency_pre-docket_2020', '.png', sep = ''),
                plot = length.freq.plot,
                width = 7.4,
                height = 5.57,
                units = 'in'
        )
        
}

##---------------------------------------------------------------------------##
## Plot 5: LF BP ####
## box plot of lengths by docketnum per processor

for (i in processors) {
        
        length.plot.dat <- mb.next.gen.grade.df %>%
                filter(!is.na(shelllength)
                       & between(shelllength, 120, 200)
                       & processor == i)
        
        plotdat.n <- length.plot.dat %>%
                group_by(docketnum) %>%
                summarize(n = n())
        
        dockets <- length(unique(length.plot.dat$docketnum))
        
        length.boxplot <-
                ggplot(length.plot.dat, aes(
                        x = factor(docketnum),
                        y = shelllength,
                        group = docketnum
                )) +
                # geom_boxplot(outlier.colour = "black", outlier.size = 1.5, position = position_dodge(preserve = 'single')) +
                geom_boxplot(
                        outlier.colour = "black",
                        outlier.size = 1.5,
                        position = position_dodge(0.85)
                ) +
                geom_text(
                        data = plotdat.n,
                        aes(y = 200, label = n),
                        size = 3,
                        angle = 0,
                        position = position_dodge(width = 0.85)
                ) +
                # geom_hline(aes(yintercept = 132), colour = 'red', linetype = 'dotted')+
                xlab('Docket number') +
                ylab(paste('Shell length (mm)')) +
                coord_cartesian(ylim = c(130, 200)) +
                theme_bw() +
                theme(
                        plot.title = element_text(hjust = 0.5),
                        axis.line = element_line(colour = "black"),
                        axis.text.x = element_text(angle = 0, vjust = 0.5),
                        legend.position = 'top'
                )+
                geom_hline(aes(yintercept = ifelse(zone == 'AW', 145, 138)),
                           linetype = 'dashed', colour = 'red', size = 0.5)
        
        print(length.boxplot)
        
        setwd('C:/CloudStor/R_Stuff/MMLF/MM_Plots/MM_Plots_2021ProcessorSummaries')
        ggsave(
                filename = paste(i, '_length_pre-docket_boxplot_2020', '.pdf', sep = ''),
                plot = length.boxplot,
                width = 7.4,
                height = 5.57,
                units = 'in'
        )
        ggsave(
                filename = paste(i, '_length_pre-docket_boxplot_2020', '.wmf', sep = ''),
                plot = length.boxplot,
                width = 7.4,
                height = 5.57,
                units = 'in'
        )
        ggsave(
                filename = paste(i, '_length_pre-docket_boxplot_2020', '.png', sep = ''),
                plot = length.boxplot,
                width = 7.4,
                height = 5.57,
                units = 'in'
        )
}

##---------------------------------------------------------------------------##
## Tas Seafoods ####
## Tasmanian Seafoods Diver Summary - Mark Fleming

# tas.seafoods.divers.2020 <- read.xlsx("C:/CloudStor/R_Stuff/MMLF/MM_Plots/MM_Plots_2020ProcessorSummaries/TasmaniaSeafoods_DiverDetails_2020.xlsx",
#                        detectDates = T)

tas.seafoods.divers.2021 <- read.xlsx("C:/CloudStor/R_Stuff/MMLF/MM_Plots/MM_Plots_2021ProcessorSummaries/TasmaniaSeafoods_DiverDetails_2021.xlsx",
                                      detectDates = T)


summary.month <- lubridate::month(Sys.time(), label = T, abbr = FALSE)
summary.year <- year(Sys.time())

tas.seafoods.divers <- tas.seafoods.divers.2021 %>% 
        distinct(docketnum, divedate, .keep_all = T) %>% 
        group_by(docketnum, diver) %>% 
        summarise(divedate = max(divedate)) %>% 
        mutate(docketnum = trimws(docketnum))


tas.seafoods.grade.summary <- left_join(docknum.n, docknum.grade.meas, by = c('docketnum', 'processor', 'plaindate')) %>% 
        filter(processor == "TASMANIAN SEAFOODS PTY LTD") %>% 
        mutate(grade.perc = round((grade.meas / ab.weighed) * 100)) %>%   
        ungroup() %>% 
        select(-c(grade.meas, processor)) %>% 
        spread(grade, grade.perc) %>% 
        dplyr::rename("Large\n(%)" = large, "Medium\n(%)" = medium, "Small\n(%)" = small) %>%
        {if('xsmall' %in% names(.)) rename(., "XSmall (%)" = xsmall) else .} %>% 
        arrange(desc(plaindate)) %>%
        ungroup() %>% 
        mutate(docketnum = paste(zone, docketnum, sep = '')) %>%
        dplyr::rename('Sample\ndate' = plaindate,
               'Docket\nno.' = docketnum,
               'Abalone\nmeasured' = ab.weighed) %>% 
        select(-zone) %>% 
        as.data.frame()

tas.seafoods.grade.summary <- left_join(tas.seafoods.grade.summary, tas.seafoods.divers, by = c('Docket\nno.' = 'docketnum')) %>%  
        select(divedate, diver, 'Docket\nno.', 'Sample\ndate', 'Abalone\nmeasured', 'Large\n(%)', 'Medium\n(%)', 'Small\n(%)') %>%  
        filter(divedate >= as.Date("2021-01-01")) %>% 
        dplyr::rename('Dive\ndate' = divedate,
               'Diver\nname' = diver,
               'Date\nsampled' = 'Sample\ndate',
               'Number\nsampled' = 'Abalone\nmeasured') 
        

tas.seafoods.grade.summary.formated <- tas.seafoods.grade.summary %>% 
        ggpubr::ggtexttable(rows = NULL, theme = ggpubr::ttheme('mOrange'))

# print(tas.seafoods.grade.summary.formated)

setwd('C:/CloudStor/R_Stuff/MMLF/MM_Plots/MM_Plots_2021ProcessorSummaries')

write.xlsx(tas.seafoods.grade.summary, paste('TASMANIAN SEAFOODS PTY LTD', 'DIVERSUMMARY', summary.month, summary.year, '.xlsx', sep = '_'), 
           sheetName = "Sheet1",
           col.names = TRUE, row.names = TRUE, append = FALSE)

ggsave(
        filename = paste('TASMANIAN SEAFOODS PTY LTD', 'DIVERSUMMARY', summary.month, summary.year, '.pdf', sep = '_'),
        plot = tas.seafoods.grade.summary.formated,
        width = 200,
        height = 400,
        units = 'mm'
)
##---------------------------------------------------------------------------##
## Seafood Traders ####

seafood.traders.divers.2021 <- read.xlsx("C:/CloudStor/R_Stuff/MMLF/MM_Plots/MM_Plots_2021ProcessorSummaries/SeafoodTraders_2021_DiverDetails.xlsx",
                                      detectDates = T)

summary.month <- lubridate::month(Sys.time(), label = T, abbr = FALSE)
summary.year <- year(Sys.time())

seafood.traders.divers.2021 <- seafood.traders.divers.2021 %>% 
        distinct(docketnum, divedate, .keep_all = T) %>% 
        group_by(docketnum, diver) %>% 
        summarise(divedate = max(divedate)) %>% 
        mutate(docketnum = trimws(docketnum))

seafood.traders.grade.summary <- left_join(docknum.n, docknum.grade.meas, by = c('docketnum', 'processor', 'plaindate')) %>% 
        filter(processor == "SEAFOOD TRADERS PTY LTD") %>% 
        mutate(grade.perc = round((grade.meas / ab.weighed) * 100)) %>%   
        ungroup() %>% 
        select(-c(grade.meas, processor)) %>% 
        spread(grade, grade.perc) %>% 
        dplyr::rename("Large\n(%)" = large, "Medium\n(%)" = medium, "Small\n(%)" = small) %>%
        {if('xsmall' %in% names(.)) rename(., "XSmall (%)" = xsmall) else .} %>% 
        arrange(desc(plaindate)) %>%
        ungroup() %>% 
        mutate(docketnum = paste(zone, docketnum, sep = '')) %>%
        dplyr::rename('Docket\nno.' = docketnum,
                      'Abalone\nmeasured' = ab.weighed) %>% 
        select(-zone) %>% 
        as.data.frame()

seafood.traders.grade.summary <- left_join(seafood.traders.grade.summary, seafood.traders.divers.2021, by = c('Docket\nno.' = 'docketnum')) %>%  
        select(divedate, diver, 'Docket\nno.', plaindate, 'Abalone\nmeasured', 'Large\n(%)', 'Medium\n(%)', 'Small\n(%)') %>% 
        filter(plaindate >= as.Date("2021-01-01")) %>% 
        dplyr::rename('Dive\ndate' = divedate,
                      'Diver\nname' = diver,
                      'Date\nsampled' = plaindate,
                      'Number\nsampled' = 'Abalone\nmeasured')

seafood.traders.grade.summary.formated <- seafood.traders.grade.summary %>% 
        ggpubr::ggtexttable(rows = NULL, theme = ggpubr::ttheme('mOrange'))

setwd('C:/CloudStor/R_Stuff/MMLF/MM_Plots/MM_Plots_2021ProcessorSummaries')

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

##---------------------------------------------------------------------------##
## Ralphs Tasmanian Seafoods ####

ralphs.divers.2021 <- read.xlsx("C:/CloudStor/R_Stuff/MMLF/MM_Plots/MM_Plots_2021ProcessorSummaries/RalphsTasmanianSeafoods_DiverDetails_2021.xlsx",
                                         detectDates = T)

summary.month <- month(Sys.time(), label = T, abbr = FALSE)
summary.year <- year(Sys.time())

ralphs.divers.2021 <- ralphs.divers.2021 %>% 
        distinct(docketnum, divedate, .keep_all = T) %>% 
        group_by(docketnum, diver) %>% 
        summarise(divedate = max(divedate)) %>% 
        mutate(docketnum = trimws(docketnum))

ralphs.grade.summary <- left_join(docknum.n, docknum.grade.meas, by = c('docketnum', 'docketnum.day', 'processor', 'plaindate', 'samptime.max', 'docket.index')) %>% 
        filter(processor == "RALPHS TASMANIAN SEAFOODS PTY LTD") %>% 
        mutate(grade.perc = round((grade.meas / ab.weighed) * 100)) %>%   
        ungroup() %>% 
        select(-c(grade.meas, processor)) %>% 
        spread(grade, grade.perc) %>% 
        dplyr::rename("Large\n(%)" = large, "Medium\n(%)" = medium, "Small\n(%)" = small) %>%
        {if('xsmall' %in% names(.)) rename(., "XSmall (%)" = xsmall) else .} %>% 
        arrange(desc(plaindate)) %>%
        ungroup() %>% 
        mutate(docketnum = paste(zone, docketnum, sep = ''),
               docketnum.day = paste(zone, docketnum.day, sep = '')) %>%
        dplyr::rename('Sample\ndate' = plaindate,
                      'Docket\nno.' = docketnum,
                      'Abalone\nmeasured' = ab.weighed) %>% 
        select(-zone) %>% 
        as.data.frame()

ralphs.grade.summary <- left_join(ralphs.grade.summary, ralphs.divers.2021, by = c('Docket\nno.' = 'docketnum')) %>%  
        select(docketnum.day, divedate, diver, 'Docket\nno.', 'Sample\ndate', 'Abalone\nmeasured', 'Large\n(%)', 'Medium\n(%)', 'Small\n(%)') %>%  
        filter(divedate >= as.Date("2020-12-29")) %>%
        dplyr::rename('Dive\ndate' = divedate,
                      'Diver\nname' = diver,
                      'Date\nsampled' = 'Sample\ndate',
                      'Number\nsampled' = 'Abalone\nmeasured') %>% 
        mutate('Docket\nno.' = docketnum.day) %>% 
        select(c('Dive\ndate', 'Diver\nname', 'Docket\nno.', 'Date\nsampled', 'Number\nsampled', 'Large\n(%)', 'Medium\n(%)', 'Small\n(%)'))

ralphs.grade.summary.formated <- ralphs.grade.summary %>% 
        ggpubr::ggtexttable(rows = NULL, theme = ggpubr::ttheme('mOrange'))

setwd('C:/CloudStor/R_Stuff/MMLF/MM_Plots/MM_Plots_2021ProcessorSummaries')
ggsave(
        filename = paste('TRUE SOUTH SEAFOOD', 'DIVERSUMMARY', summary.month, summary.year, '.pdf', sep = '_'),
        plot = ralphs.grade.summary.formated,
        width = 200,
        height = 300,
        units = 'mm'
)

#---------------------------------------------------------------------------##
## Excel Export ####
## raw data Excel export for processors

df.1 <- measure.board.next.gen.df %>% 
        mutate(grade = dplyr::if_else(wholeweight == 0, NA_character_,
                                      dplyr::if_else(between(wholeweight, 1, 600), 'small', #0-400g can also be labelled xsmall
                                                     dplyr::if_else(between(wholeweight, 601, 800), 'medium', 'large'))),
               wholeweight = replace(wholeweight, wholeweight == 0, NA))

for(i in processors){
        df.2 <- df.1 %>%
                filter(processor == i)
        
        setwd('C:/CloudStor/R_Stuff/MMLF/MM_Plots/MM_Plots_2020ProcessorSummaries')
        write.xlsx(df.2,
                   file = paste(i, '_MeasuringBoardData_', Sys.Date(), '.xlsx'),
                   sheetName = "Sheet1",
                   col.names = TRUE,
                   row.names = TRUE,
                   append = FALSE)
}
##---------------------------------------------------------------------------##