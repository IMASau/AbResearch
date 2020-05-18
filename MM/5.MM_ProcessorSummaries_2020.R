# This script provides a preliminary analysis of measuring board data prior to catch dockets being processed
# by DPIPWE and further catch details becoming available. These analysis are intended to provide processors with
# near real-time summary of catches which they have measured.

# Created by: Jaime McAllister

##-------------------------------------------------------------------------------------------------------##
# load libaries ####
library(openxlsx)
library(fuzzyjoin)
library(lubridate)
library(tidyr)
library(tidyverse)
library(dplyr)
library(ggsci)
library(ggpubr)
library(scales)
##-------------------------------------------------------------------------------------------------------##
# load data ####

# load latest measuring board data that comes from MM_NextGen_4G.R script
# measure.board.df <- readRDS('C:/CloudStor/R_Stuff/MMLF/measure.board.df.RDS')
measure.board.next.gen.df <- readRDS('C:/CloudStor/R_Stuff/MMLF/measure.board.next.gen.df.RDS')

# quick summary of catches measured by processor
measure.board.next.gen.df %>% 
        group_by(processor) %>% 
        summarise(catches.measured = n_distinct(docketnum),
                  n = n()) %>% 
        as.data.frame()

# add weight grading categories used by processors
mb.next.gen.grade.df <- measure.board.next.gen.df %>%
        filter(wholeweight != 0) %>%
        mutate(grade = dplyr::if_else(wholeweight <= 400, 'small', #0-400g can also be labelled xsmall
                                      dplyr::if_else(between(wholeweight, 401, 600), 'small',
                                                     dplyr::if_else(between(wholeweight, 601, 800), 'medium', 'large'))))

# list of unique processors for summary and plot loops
processors <- unique(mb.next.gen.grade.df$processor)

# determine number of abalone measured per docket
docknum.n.meas <- mb.next.gen.grade.df %>% 
        group_by(docketnum, processor, plaindate) %>% 
        summarise(ab.meas = n())

# determine number of abalone measured by grade per docket
docknum.grade.meas <- mb.next.gen.grade.df %>% 
        group_by(docketnum, grade) %>% 
        summarise(grade.meas = n())

##-------------------------------------------------------------------------------------------------------##
## Sum 1: length/weight and grade summary tables  ####

for(i in processors){

# join number of abalone measured per docket and grade, and calculate percentage measured per grade to 
# create grade summary table
grade.summary <- left_join(docknum.n.meas, docknum.grade.meas, by = 'docketnum') %>% 
        filter(processor == i) %>% 
        mutate(grade.perc = round((grade.meas / ab.meas) * 100)) %>% 
        ungroup() %>% 
        select(-c(grade.meas, processor)) %>% 
        spread(grade, grade.perc) %>% 
        rename("Large (%)" = large, "Medium (%)" = medium, "Small (%)" = small) %>%
        {if('xsmall' %in% names(.)) rename(., "XSmall (%)" = xsmall) else .} %>%
        rename('sampdate' = plaindate) %>% 
        arrange(desc(sampdate)) %>% 
        as.data.frame() 
        
# create length and weight summary table
length.weight.summary <- mb.next.gen.grade.df %>%
        filter(wholeweight != 0 & processor == i) %>% 
        group_by(docketnum, processor, plaindate) %>% 
        summarise('mean.weight (g)' = round(mean(wholeweight), 0),
                  'min.weight (g)' = min(wholeweight),
                  'max.weight (g)' = max(wholeweight),
                  'mean.size (mm)' = round(mean(shelllength), 0),
                  'min.size (mm)' = round(min(shelllength), 0),
                  'max.size (mm)' = round(max(shelllength), 0)) %>% 
        rename('sampdate' = plaindate) %>% 
        arrange(desc(sampdate)) %>%
        ungroup() %>% 
        select(-processor) %>% 
        as.data.frame()

# save summary tables to Cloudstor folder
setwd('C:/CloudStor/R_Stuff/MMLF/MM_Plots/MM_Plots_2020ProcessorSummaries')
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


# create formated summary tablesfor report layout
grade.summary.formated <- grade.summary %>% 
        ggpubr::ggtexttable(rows = NULL, theme = ggpubr::ttheme('mOrange'))

length.weight.summary.formated <- length.weight.summary %>% 
        ggpubr::ggtexttable(rows = NULL, theme = ggpubr::ttheme('mOrange'))

ggsave(
        filename = paste(i, '_GradeSummary_Formatted_', Sys.Date(), '.pdf', sep = ''),
        plot = grade.summary.formated,
        width = 200,
        height = 297,
        units = 'mm'
)

ggsave(
        filename = paste(i, '_SizeWeightSummary_Formatted_', Sys.Date(), '.pdf', sep = ''),
        plot = length.weight.summary.formated,
        width = 200,
        height = 297,
        units = 'mm'
)

}

#---------------------------------------------------------------------------##
## Plot 1: pie chart of weight grades by docketnum  ####

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

setwd('C:/CloudStor/R_Stuff/MMLF/MM_Plots/MM_Plots_2020ProcessorSummaries')
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
## Plot 2: faceted pie chart of weight grades by all docketnum  ####
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
## Plot 3: box plot of weights by docketnum per processor (inc. grade intervals)  ####

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

setwd('C:/CloudStor/R_Stuff/MMLF/MM_Plots/MM_Plots_2020ProcessorSummaries')
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
## Plot 4: length frequency of catch per docket  ####

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

setwd('C:/CloudStor/R_Stuff/MMLF/MM_Plots/MM_Plots_2020ProcessorSummaries')

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
## Plot 5: box plot of lengths by docketnum per processor  ####

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
        
        setwd('C:/CloudStor/R_Stuff/MMLF/MM_Plots/MM_Plots_2020ProcessorSummaries')
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
## Docket report: compiled summary of docketnum per processor  ####

docket.unique <- unique(measure.board.next.gen.df$docketnum)
processor.unique <- unique(measure.board.next.gen.df$processor)

grades <- data.frame(y = 0.35, x = c(500, 700, 900), 
                     lab = c('Small', 'Medium', 'Large'))

## Plot 6: ltfrq vs bp ####
# overlay length frequency with boxplot
for (i in docket.unique) {
        plot.length.freq.dat <- measure.board.next.gen.df %>%
                filter(docketnum == i &
                               between(shelllength, 120, 200))
        
        length.freq.plot <- ggplot(plot.length.freq.dat, aes(shelllength)) +
                geom_histogram(
                        aes(y = ..density.. * 5),
                        fill = '#EFC000FF',
                        col = 'black',
                        binwidth = 5,
                        alpha = 0.6
                ) +
                coord_cartesian(xlim = c(130, 200), ylim = c(0, 0.4)) +
                theme_bw() +
                xlab("Shell Length (mm)") +
                ylab(paste("Docket no.", i, " Percentage (%)")) +
                # geom_vline(aes(xintercept = 138), colour = 'red',
                #            linetype = 'dashed', size = 0.5)+
                geom_vline(
                        aes(xintercept = ifelse(zone == 'AW', 145, 138)),
                        linetype = 'dashed',
                        colour = 'red',
                        size = 0.5
                ) +
                scale_y_continuous(labels = percent_format(accuracy = 1, suffix = ''))
        
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
        
        setwd('C:/CloudStor/R_Stuff/MMLF/MM_Plots/MM_Plots_2020ProcessorSummaries')
        ggsave(
                filename = paste(i, 'length.summary.plot', '.pdf', sep = ''),
                plot = length.plot,
                width = 7.4,
                height = 5.57,
                units = 'in'
        )
        
}

## Plot 7: wtfrq vs bp vs pie ####
# overlay weight frequency with boxplot

for (j in docket.unique) {
                plot.weight.freq.dat <- measure.board.next.gen.df %>%
                        filter(docketnum == j &
                                       wholeweight != 0)
                
                weight.freq.plot <- ggplot(plot.weight.freq.dat, aes(wholeweight)) +
                        geom_histogram(
                                aes(y = ..density.. * 50),
                                fill = '#0073C2FF',
                                col = 'black',
                                binwidth = 50,
                                alpha = 0.6
                        ) +
                        coord_cartesian(xlim = c(300, 1200),
                                        ylim = c(0, 0.35)) +
                        theme_bw() +
                        theme(panel.grid = element_blank()) +
                        xlab("Whole weight (g)") +
                        ylab(paste("Docket no.", j, " Percentage (%)")) +
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
                        scale_y_continuous(labels = percent_format(accuracy = 1, suffix = ''))
                
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
                        left_join(docknum.n.meas, docknum.grade.meas, by = 'docketnum') %>%
                        mutate(grade.perc = round((grade.meas / ab.meas) * 100))
                
                pie.plot.dat <- docketnum.grade.summary %>%
                        filter(docketnum == j) %>%
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
                
                setwd('C:/CloudStor/R_Stuff/MMLF/MM_Plots/MM_Plots_2020ProcessorSummaries')
                ggsave(
                        filename = paste(j, 'wt.summary.plot', '.pdf', sep = ''),
                        plot = wt.plot,
                        width = 7.4,
                        height = 5.57,
                        units = 'in'
                )
        }
