# The following script is used to produce various catch sampling plots for the Tasmanian Abalone Stock 
# Assessment Report and miscellaneous plots using data comipled in MM_HistoricalDataCompile.R.

# Created by: Jaime McAllister

# Load libaries ####
# load required libaries and custom functions for plotting
library(RODBC)
library(R.utils)
library(lubridate)
library(rgdal)
library(sp)
library(maptools)
library(tools)
library(openxlsx)
library(lubridate)
library(gdata)
library(dplyr)
library(doBy)
library(tidyr)
library(tidyverse)
library(splitstackshape)
library(readxl)
library(data.table)
library(scales)
library(gridExtra)
library(ggspatial)
library(tmap)
library(sf)
library(janitor)
library(xtable)
library(gsubfn)
library(ggplot2)
source("C:/GitCode/AbResearch/codeBLnewzone.r")
##-------------------------------------------------------------------------------------------------------##
# Load data ####

# load most recent compiled MM dataframe
compiledMM.df.final <-
  readRDS('C:/CloudStor/R_Stuff/MMLF/compiledMM.df.final.RDS')

##-------------------------------------------------------------------------------------------------------##
# Data additions ####

# add quarter variable
compiledMM.df.final <- compiledMM.df.final %>% 
  mutate(fishquarter = quarter(msr.date))

##-------------------------------------------------------------------------------------------------------##
# Identify fish year ####

# identify stock assessment year of interest
stock.assessment.year <- 2020

##-------------------------------------------------------------------------------------------------------##
# Identify zone/blocks/processors ####

# # identify stock assessment zone of interest
# stock.assessment.zone <- 'E'

# identify unique zones and blocks sampled in stock assessment year
df.2019.blocks <- compiledMM.df.final %>%
  filter(fishyear == stock.assessment.year
         # & newzone == stock.assessment.zone
         & numblocks <= 1)

df.2019.unique.blocks <- sort(unique(df.2019.blocks$blocklist))

df.2019.zones <- compiledMM.df.final %>%
  filter(fishyear == stock.assessment.year
         & numblocks <= 1)

df.2019.unique.zones <- sort(unique(df.2019.zones$newzone))

# identify unique processors for stock assessment year
processors.2019 <- compiledMM.df.final %>%
  filter(fishyear == stock.assessment.year) %>%
  distinct(processorname) %>% 
  pull()

##-------------------------------------------------------------------------------------------------------##
# Size limits data ####
# Add size limit data to filter out measurement errors and for reference points on plots.

# load legal minimum length data
size.limits <- read.csv("C:/CloudStor/R_Stuff/MMLF/AbaloneSizeLimits2.csv", fileEncoding="UTF-8-BOM")

# clean lml data
colnames(size.limits) <- tolower(colnames(size.limits))
names(size.limits) <- gsub('.', '-', names(size.limits), fixed = T)

# convert lml data to long format and create lml index variable
size.limits.tab <- size.limits %>%
  gather(monthyear, sizelimit, `jan-1962`:`dec-2020`) %>% 
  mutate(monthyear = gsub('jan', 1, monthyear)) %>% 
  mutate(monthyear = gsub('feb', 2, monthyear)) %>% 
  mutate(monthyear = gsub('mar', 3, monthyear)) %>% 
  mutate(monthyear = gsub('apr', 4, monthyear)) %>% 
  mutate(monthyear = gsub('may', 5, monthyear)) %>% 
  mutate(monthyear = gsub('jun', 6, monthyear)) %>% 
  mutate(monthyear = gsub('jul', 7, monthyear)) %>% 
  mutate(monthyear = gsub('aug', 8, monthyear)) %>% 
  mutate(monthyear = gsub('sep', 9, monthyear)) %>% 
  mutate(monthyear = gsub('oct', 10, monthyear)) %>% 
  mutate(monthyear = gsub('nov', 11, monthyear)) %>% 
  mutate(monthyear = gsub('dec', 12, monthyear)) %>% 
  mutate(sizelimit.index = paste(abzone, subblockno, monthyear, sep = '-')) %>% 
  select(sizelimit.index, sizelimit)

# add columns that count number of blocks and subblocks in compiledMM.df
compiledMM.df.final <- compiledMM.df.final %>%
  mutate(subblocklist = ifelse(is.na(subblocklist), subblockno, subblocklist)) %>%
  mutate(blocklist = ifelse(is.na(blocklist), as.numeric(gsub("([0-9]+).*$", "\\1", subblocklist)), blocklist)) %>%
  mutate(numblocks = count.fields(textConnection(blocklist), sep = ',')) %>%
  mutate(numsubblocks = count.fields(textConnection(subblocklist), sep = ','))

# add column to determine if the same block was fished where multiple sub-blocks are listed in compiledMM.df
compiledMM.df.final <- compiledMM.df.final %>%
  mutate(same.block = if_else(!is.na(blocklist_1) & is.na(blocklist_2), 1,
                              if_else(is.na(blocklist_5) & is.na(blocklist_4) & is.na(blocklist_3) & blocklist_1 == blocklist_2, 1,
                                      if_else(is.na(blocklist_5) & is.na(blocklist_4) & blocklist_1 == blocklist_2 & blocklist_2 == blocklist_3, 1,
                                              if_else(is.na(blocklist_5) & blocklist_1 == blocklist_2 & blocklist_2 == blocklist_3 & blocklist_3 == blocklist_4, 1,
                                                      if_else(!is.na(blocklist_5) & blocklist_5 == blocklist_1, 1, 0))))))

# create lml index variable in compiledMM.df
compiledMM.df.final <- compiledMM.df.final %>%
  mutate(sizelimit.index = if_else(
    numsubblocks == 1 & subblockno %in% c('A', 'B', 'C', 'D', 'E'),
    paste(
      newzone,
      blockno,
      lubridate::month(daylist_max),
      fishyear,
      sep = '-'
    ),
    if_else(
      numsubblocks == 1 & is.na(subblockno),
      paste(
        newzone,
        blockno,
        lubridate::month(daylist_max),
        fishyear,
        sep = '-'
      ),
      if_else(
        numsubblocks == 1,
        paste(
          newzone,
          subblockno,
          lubridate::month(daylist_max),
          fishyear,
          sep = '-'
        ),
        if_else(
          numsubblocks > 1 & same.block == 1,
          paste(
            newzone,
            blockno,
            lubridate::month(daylist_max),
            fishyear,
            sep = '-'
          ),        
          NA_character_
          
        )
      ))))

# join size limit data and compiledMM.df to include size limit for each observation
compiledMM.df.final <- left_join(compiledMM.df.final, size.limits.tab, "sizelimit.index")
##-------------------------------------------------------------------------------------------------------##
# Quick summaries ####

# number of catches measured where a single block was reported
proc.block <- compiledMM.df.final %>% 
  filter(fishyear == stock.assessment.year & 
           numblocks <= 1 &
         between(shell.length, sizelimit - 5, 220)) %>% 
  group_by(processorname) %>% 
  summarise(catches.single.block = n_distinct(docket.number),
            single.block.n = n()) %>% 
  as.data.frame() %>%
  arrange(desc(processorname)) %>% 
  adorn_totals(fill = '')

# number of catches measured where multiple blocks were reported and measurements can't be allocated to a
# specific block
proc.multiblock <- compiledMM.df.final %>% 
  filter(fishyear == stock.assessment.year &
         between(shell.length, sizelimit - 5, 220)) %>% 
  group_by(processorname) %>% 
  summarise(catches.measured = n_distinct(docket.number),
            multi.block.n = n()) %>% 
  as.data.frame() %>%
  arrange(desc(processorname)) %>% 
  adorn_totals(fill = '')

df.1 <- left_join(proc.block, proc.multiblock, by = c('processorname'))

##-------------------------------------------------------------------------------------------------------##
# Fish year summary ####

# create summary table of most recent year catch sampling data to determine zones (and blocks) sampled
fishyear.summary.df <- compiledMM.df.final %>%
  dplyr::filter(fishyear == stock.assessment.year
         & numblocks <= 1
         & between(shell.length, sizelimit - 5, 220)) %>%
  group_by(newzone, blocklist) %>%
  summarise(catches = n_distinct(docket.number),
            n = n(),
            med.sl = round(median(shell.length), 1),
            max.sl = round(max(shell.length), 1)) %>%
  rename(Zone = newzone,
         BlockNo = blocklist) %>% 
  mutate(med.sl = as.character(med.sl),
         max.sl = as.character(max.sl)) %>% 
  as.data.frame()

# rename column headings
colnames(fishyear.summary.df) <- c('Zone', 'Block\nNo', 'Catches\nmeasured', 'Abalone\nmeasured', 'Median\nSL', 'Max\nSL')

# add totals for numeric columns (Note: median and max have been converted to characters)
fishyear.summary.df <- adorn_totals(fishyear.summary.df, fill = '')

# create formated summary tables for report layout
fishyear.summary.df.formated <- fishyear.summary.df %>% 
  ggpubr::ggtexttable(rows = NULL, theme = ggpubr::ttheme('mOrange'))

# save Excel and Latex tables
setwd('C:/CloudStor/R_Stuff/MMLF/MM_Plots')

ggsave(
  filename = paste('MM_Sampling_Summary_2020.pdf', sep = ''),
  plot = fishyear.summary.df.formated,
  width = 200,
  height = 297,
  units = 'mm'
)

ggsave(
  filename = paste('MM_Sampling_Summary_2020.png', sep = ''),
  plot = fishyear.summary.df.formated,
  width = 200,
  height = 297,
  units = 'mm'
)

write.xlsx(fishyear.summary.df, 'MM_Sampling_Summary_2020.xlsx', sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)
print(xtable(fishyear.summary.df, type = 'latex'), file = 'MM_Sampling_Summary_2020.tex')

##-------------------------------------------------------------------------------------------------------##
# Processor x fish year summary ####

# create summary table of most recent year catch sampling data to determine zones (and blocks) sampled

for(i in processors.2019) {
  processor.fishyear.summary.df <- compiledMM.df.final %>%
    dplyr::filter(fishyear == stock.assessment.year
                  & numblocks <= 1
                  & processorname == i
                  & between(shell.length, sizelimit - 5, 220)) %>%
    group_by(newzone, blocklist) %>%
    summarise(
      catches = n_distinct(docket.number),
      n = n(),
      med.sl = as.character(median(shell.length)),
      max.sl = as.character(max(shell.length))
    ) %>%
    rename(Zone = newzone,
           BlockNo = blocklist) %>%
    as.data.frame()
  
  # rename column headings
  colnames(processor.fishyear.summary.df) <-
    c(
      'Zone',
      'Block\nNo',
      'Catches\nMeasured',
      'Abalone\nMeasured',
      'Median\nSL',
      'Max\nSL'
    )
  
  # add totals for numeric columns (Note: median and max have been converted to characters)
  processor.fishyear.summary.df <-
    adorn_totals(processor.fishyear.summary.df, fill = '')
  
  # save Excel and Latex tables
  setwd('C:/CloudStor/R_Stuff/MMLF/MM_Plots')
  write.xlsx(
    processor.fishyear.summary.df,
    file = paste(i, '_Sampling_Summary_', stock.assessment.year, '.xlsx'),
    sheetName = "Sheet1",
    col.names = TRUE,
    row.names = TRUE,
    append = FALSE
  )
  print(
    xtable(processor.fishyear.summary.df, type = 'latex'),
    file = paste(i, '_Sampling_Summary_', stock.assessment.year, '.tex')
  )
  
}

##-------------------------------------------------------------------------------------------------------##
# Processor summary ####

# quick summary of processor details
processor.summary <- compiledMM.df.final %>%
  dplyr::filter(fishyear == stock.assessment.year
                & numblocks <= 1) %>%
  group_by(processorname) %>%
  summarise(catches = n_distinct(docket.number),
            n = n()) %>%
  as.data.frame()

colnames(processor.summary) <- c('Processor', 'Catches\nMeasured', 'Abalone\nMeasured')

processor.summary$Processor <- tolower(processor.summary$Processor)
processor.summary$Processor <- str_to_title(processor.summary$Processor)

processor.summary <- adorn_totals(processor.summary, fill = '')

# save Excel and Latex tables
setwd('C:/CloudStor/R_Stuff/MMLF/MM_Plots')
write.xlsx(processor.summary, 'MM_Processor_Summary_2019.xlsx', sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)
print(xtable(processor.summary, type = 'latex'), file = 'MM_Processor_Summary_2019.tex')

##-------------------------------------------------------------------------------------------------------##
# PLOT 1: LF zone x block x year ####

# create length frequency plots for each block sampled in the stock assessment year and zone of interest

for (i in df.2019.unique.blocks) {
  # select data for zone
  plotdat.zone <- compiledMM.df.final %>%
    filter(
      newzone == stock.assessment.zone
      & fishyear %in% c(2000, 2005, 2010, 2015, 2019)
      & between(shell.length, 100, 220)
    ) %>%
    group_by(fishyear) %>%
    mutate(n = n())
  
  # select data for zone and count the number of measurments
  plotdat.zone.n <- compiledMM.df.final %>%
    filter(
      newzone %in% stock.assessment.zone
      & fishyear %in% c(2000, 2005, 2010, 2015, 2019)
      & between(shell.length, 100, 220)
    ) %>%
    group_by(fishyear) %>%
    summarise(zone.n = n(),
              n = paste('n =', n()))
  
  # select data for block
  plotdat.block <- compiledMM.df.final %>%
    filter(
      newzone == stock.assessment.zone
      & blocklist == i
      & fishyear %in% c(2000, 2005, 2010, 2015, 2019)
      & between(shell.length, 100, 220)
    )
  
  # select data for block and count the number of measurments
  plotdat.block.n <- compiledMM.df.final %>%
    filter(
      newzone == stock.assessment.zone
      & blocklist == i
      & fishyear %in% c(2000, 2005, 2010, 2015, 2019)
      & between(shell.length, 100, 220)
    ) %>%
    group_by(fishyear) %>%
    summarise(block.n = n(),
              n = paste('n =', n()))
  
  # join count of measurments for zone, and for chosen block, and create plot label
  plotdat.zone.block.n.join <-
    left_join(plotdat.zone.n, plotdat.block.n, by = "fishyear") %>%
    mutate(percent.block = round((block.n / zone.n) * 100), 0) %>%
    mutate(n = ifelse(
      is.na(block.n),
      'NO DATA',
      paste0('n = ', block.n, '\n', '(', percent.block, '%', ')')
    ))
  
  # create dataframes for block/zone size limits
  size.limits <- data.frame(
    fishyear = c(2000, 2005, 2010, 2015, 2019),
    size.limit = c(145, 145, 145, 145, 145)
  )
  
  size.limits.145 <- data.frame(
    fishyear = c(2000, 2005, 2010, 2015, 2019),
    size.limit = c(132, 136, 138, 138, 145)
  )
  
  # generate plot (add or remove size limits where data is absent)
  
  mm.block.plot <- ggplot(plotdat.block, aes(shell.length)) +
    geom_histogram(
      data = transform(subset(
        plotdat.zone, newzone %in% stock.assessment.zone
      )),
      aes(y = -..density.. * 5),
      fill = 'white',
      col = 'black',
      binwidth = 5
    ) +
    geom_histogram(
      data = subset(plotdat.block, blockno %in% df.2019.unique.blocks),
      aes(y = ..density.. * 5),
      fill = 'black',
      colour = 'black',
      binwidth = 5
    ) +
    theme_bw() +
    ylab(paste("BlockNo", i, " Percentage (%)")) +
    xlab("Shell Length (mm)") +
    coord_flip(xlim = c(100, 220), ylim = c(-0.4, 0.4)) +
    facet_grid(. ~ fishyear) +
    scale_y_continuous(labels = percent_format(accuracy = 1, suffix = '')) +
    geom_text(
      data = plotdat.zone.block.n.join,
      aes(x = 200, y = 0.2, label = n),
      colour = 'black',
      inherit.aes = F,
      parse = F,
      size = 3.5
    ) +
    #add size limits for each time period
    # geom_vline(data = size.limits, aes(xintercept = as.numeric(size.limit)),
    #            colour = 'red', linetype = 'dashed', size = 0.5)
    {
      if (i %in% c(27, 28))
      {
        geom_vline(
          data = size.limits.145,
          aes(xintercept = as.numeric(size.limit)),
          colour = 'red',
          linetype = 'dashed',
          size = 0.5
        )
      } else
      {
        geom_vline(
          data = size.limits,
          aes(xintercept = as.numeric(size.limit)),
          colour = 'red',
          linetype = 'dashed',
          size = 0.5
        )
      }
    }
  # geom_vline(data = filter(plotdat.block, fishyear == 2000), aes(xintercept = 132),colour = 'red', linetype = 'dashed', size = 0.5)+
  # geom_vline(data = filter(plotdat.block, fishyear == 2005), aes(xintercept = 136),colour = 'red', linetype = 'dashed', size = 0.5)+
  # # geom_vline(data = filter(plotdat.block, fishyear == 2010), aes(xintercept = 138),colour = 'red', linetype = 'dashed', size = 0.5)+
  # # geom_vline(data = filter(plotdat.block, fishyear == 2015), aes(xintercept = 138),colour = 'red', linetype = 'dashed', size = 0.5)+
  # geom_vline(data = filter(plotdat.block, fishyear == 2019), aes(xintercept = 138),colour = 'red', linetype = 'dashed', size = 0.5)
  print(mm.block.plot)
  
  # save plots to file
  
  setwd('C:/CloudStor/R_Stuff/MMLF/MM_Plots')
  ggsave(
    filename = paste('BlockNo', i, '_MM_FiveYrLF_2019', '.pdf', sep = ''),
    plot = mm.block.plot,
    width = 7.4,
    height = 5.57,
    units = 'in'
  )
  ggsave(
    filename = paste('BlockNo', i, '_MM_FiveYrLF_2019', '.wmf', sep = ''),
    plot = mm.block.plot,
    width = 7.4,
    height = 5.57,
    units = 'in'
  )
  ggsave(
    filename = paste('BlockNo', i, '_MM_FiveYrLF_2019', '.png', sep = ''),
    plot = mm.block.plot,
    width = 7.4,
    height = 5.57,
    units = 'in'
  )
}

##-------------------------------------------------------------------------------------------------------##
# PLOT 2: Boxplot block x year ####

## create box plots for each block sampled in the stock assessment year and compare with historical records
for (i in df.2019.unique.zones) {
  for (j in df.2019.unique.blocks) {
    # select data for block
    plotdat.block <- compiledMM.df.final %>%
      filter(
        newzone == i
        & blocklist == j
        & fishyear == stock.assessment.year
        & between(shell.length, sizelimit - 5, 220) 
      )
    
    plotdat.block.historic <- compiledMM.df.final %>%
      filter(
        newzone == i
        & blocklist == j
        & between(fishyear, 1980, stock.assessment.year - 1)
        & between(shell.length, sizelimit - 5, 220)
      )
    
    plotdat.block <- bind_rows(plotdat.block, plotdat.block.historic)
    
    # generate plot for zone and block combination only if data is present
    
    if (nrow(plotdat.block) != 0) {
      # convert required grouping variable to factor for boxplot
      
      plotdat.block$fishyear <- as.factor(plotdat.block$fishyear)
      
      # generate a count of records for each year to add to boxplot
      
      plotdat.n <- plotdat.block %>%
        group_by(fishyear, blockno) %>%
        summarize(n = n())
      
      # generate table of size limits for each year to add to boxplot
      
      size.limit <- plotdat.block %>% 
        group_by(fishyear, blockno) %>% 
        summarise(legal.min.length = max(sizelimit))
      
      # generate boxplot of shell lengths for chosen grouping variable
      
      mm.zone.boxplot <-
        ggplot(plotdat.block, aes(x = fishyear, y = shell.length)) +
        geom_boxplot(outlier.colour = "orange", outlier.size = 1.5) +
        geom_text(
          data = plotdat.n,
          aes(y = 220, label = n),
          size = 3,
          angle = 90
        ) +
        # geom_hline(aes(yintercept = 132), colour = 'red', linetype = 'dotted')+
        geom_point(data = size.limit, aes(x = fishyear, y = legal.min.length), 
                   shape = 95, size = 7, colour = "red")+
        xlab('Year') +
        ylab(paste('BlockNo', j, 'Shell Length (mm)')) +
        coord_cartesian(ylim = c(100, 225)) +
        theme_bw() +
        theme(
          plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.text.x = element_text(angle = 45, vjust = 0.5)
        )
      
      # print(mm.zone.boxplot)
      
      setwd('C:/CloudStor/R_Stuff/MMLF/MM_Plots')
      ggsave(
        filename = paste(i, '_BlockNo', j, '_MM_Boxplot_2020', '.pdf', sep = ''),
        plot = mm.zone.boxplot,
        width = 7.4,
        height = 5.57,
        units = 'in'
      )
      ggsave(
        filename = paste(i, '_BlockNo', j, '_MM_Boxplot_2020', '.wmf', sep = ''),
        plot = mm.zone.boxplot,
        width = 7.4,
        height = 5.57,
        units = 'in'
      )
      ggsave(
        filename = paste(i, '_BlockNo', j, '_MM_Boxplot_2020', '.png', sep = ''),
        plot = mm.zone.boxplot,
        width = 7.4,
        height = 5.57,
        units = 'in'
      )
      
    }
    else{
    }
    
  }
}

##-------------------------------------------------------------------------------------------------------##
# PLOT 3: Boxplot block x quarter x processor ####

# create boxplots for each block and quarter fished in the stock assessment year (for processor)

for (i in processors.2019) {
  # select data for stock assessment year
  df.1 <- compiledMM.df.final %>%
    filter(processorname == i
           & fishyear == stock.assessment.year
           & numblocks <= 1
           & between(shell.length, sizelimit - 5, 220)) %>% 
    mutate(zone.blockno = paste(newzone, blockno, sep = '-'))
  
  # generate a count of records for each year to add label to boxplot
  plotdat.n <- df.1 %>%
    group_by(zone.blockno, fishquarter) %>%
    summarize(n = n())
  
  # generate table of size limits for each year to add to boxplot
  
  size.limit <- df.1 %>% 
    group_by(zone.blockno, fishquarter) %>% 
    summarise(legal.min.length = max(sizelimit))
  
  # create plot
  quarter.boxplot <-
    ggplot(df.1, aes(
      x = zone.blockno,
      y = shell.length,
      fill = factor(fishquarter)
    )) +
    # geom_boxplot(outlier.colour = "black", outlier.size = 1.5, position = position_dodge(preserve = 'single')) +
    geom_boxplot(
      outlier.colour = "black",
      outlier.size = 1.5,
      position = position_dodge(0.85)
    ) +
    geom_text(
      data = plotdat.n,
      aes(y = 220, label = n),
      size = 3,
      angle = 90,
      position = position_dodge(width = 0.85)
    ) +
    geom_point(data = size.limit, aes(x = zone.blockno, y = legal.min.length), 
               shape = 95, size = 7, colour = "red")+
    # geom_hline(aes(yintercept = 132), colour = 'red', linetype = 'dotted')+
    xlab('Zone-BlockNo') +
    ylab(paste('Shell Length (mm)')) +
    coord_cartesian(ylim = c(100, 225)) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.line = element_line(colour = "black"),
      axis.text.x = element_text(angle = 0, vjust = 0.5),
      legend.position = 'top'
    ) +
    # scale_fill_grey(start = 0.8, end = 0.3,
    #   name = 'Quarter',
    #   breaks = c('1', '2', '3', '4'),
    #   labels = c('Q1', 'Q2', 'Q3', 'Q4')
      scale_fill_manual(values = c('1' = "#4D4D4D", 
                                   '2' = "#CCCCCC", 
                                   '3' = "#AEAEAE", 
                                   '4' = "#888888"),
                      name = 'Quarter',
                      breaks = c('1', '2', '3', '4'),
                      labels = c('Q1', 'Q2', 'Q3', 'Q4')
    )+
    guides(fill = guide_legend(override.aes = list(shape = NA)))
  
  # print(quarter.boxplot)
  
  setwd('C:/CloudStor/R_Stuff/MMLF/MM_Plots')
  ggsave(
    filename = paste(i, '_Quarter_MM_Boxplot_2020', '.pdf', sep = ''),
    plot = quarter.boxplot,
    width = 7.4,
    height = 5.57,
    units = 'in'
  )
  ggsave(
    filename = paste(i, '_Quarter_MM_Boxplot_2020', '.wmf', sep = ''),
    plot = quarter.boxplot,
    width = 7.4,
    height = 5.57,
    units = 'in'
  )
  ggsave(
    filename = paste(i, '_Quarter_MM_Boxplot_2020', '.png', sep = ''),
    plot = quarter.boxplot,
    width = 7.4,
    height = 5.57,
    units = 'in'
  )
  
}  

##-------------------------------------------------------------------------------------------------------##
# PLOT 4: Map catches x processor ####

# create map indicating blocks sampled in stock assessment year (for processor)

# load map of Tasmania and abalone blocks
tas.sp <- readOGR(dsn = 'C:/Users/jaimem/Documents/ArcGIS/GIS_files/Coastlines_GIS/Tasmania_ll_(WGS 84)/Tas_25k_land_ll_wgs84.shp'
                  , layer = 'Tas_25k_land_ll_wgs84', verbose = F)

# ab.blocks.sp <- readOGR(dsn = 'C:/CloudStor/R_Stuff/BlockSHapefile/Ab_Blocks_MGAZ55.shp'
#                         , layer = 'Ab_Blocks_MGAZ55', verbose = F)

new.ab.blocks.sp <- readOGR(dsn = 'C:/GitCode/r-AbSpatialAnalyses/Tas_Ab_Polyg_SubBlocks.shp'
                            , layer = 'Tas_Ab_Polyg_SubBlocks', verbose = F)

# set projection
projstring.mga55 <- CRS("+proj=utm +zone=55 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
projstring.wgs84 <-  CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# convert maps to projection
tas.sp <- spTransform(tas.sp, projstring.mga55)
# ab.blocks.sp <- spTransform(ab.blocks.sp, projstring.mga55)
# new.ab.blocks.sp <- spTransform(new.ab.blocks.sp, projstring.mga55)
# df <- st_as_sf(new.ab.blocks.sp)

# generate maps

for(i in processors.2019) {
  # summarise dataframe to determine number of catches measured per block by processor
  df.1 <- compiledMM.df.final %>%
    filter(fishyear == stock.assessment.year
           & processorname == i
           & numsubblocks <= 1)
  
  df.1 <- df.1 %>%
    group_by(subblockno) %>%
    summarise(n = n_distinct(docket.number))
  
  # ab.blocks.sp.2 <- ab.blocks.sp
  ab.blocks.sp.2 <- new.ab.blocks.sp
  
  # add data to attribute table of spatial dataframe (e.g. number of catches per block in fishyear xxxx)
  ab.blocks.poly <-
    merge(ab.blocks.sp.2, df.1, by.x = 'SubBlockNo', by.y = 'subblockno')
  
  # https://atlan.com/courses/introduction-to-gis-r/lesson3-static-maps/
  
  # convert spatial polygon to special feature dataframe
  df.3 <- st_as_sf(ab.blocks.poly)
  
  # # convert non-sampled blocks (i.e. NA) to zero
  # df.3$n[is.na(df.3$n)] <- 0
  
  # need to fix this code to maintain colour catergories across all processors
  
  df.3 <- df.3 %>% 
    mutate(subblock.sampled = if_else(n > 0, as.character(SubBlockNo), ''))
  
  catch.plot <- df.3 %>%
    # filter(n >= 0) %>%
    tm_shape() +
    tm_fill(col = 'n',
            title = 'Catches \nmeasured',
            breaks = c(0, 2, 5, 10, 15),
            style = 'fixed',
            textNA = 'No data',
            colorNA = 'white',
            palette = '-Spectral') +
    tm_borders(lwd = 0.5) +
    tm_text('n', size = 0.75)+
    tm_shape(df.3)+
    tm_text('subblock.sampled', size = 0.25, just = 'bottom', ymod = -0.4)
  
  # print(catch.plot)
  
  setwd('C:/CloudStor/R_Stuff/MMLF/MM_Plots')
  
  tmap_save(
    tm = catch.plot,
    filename = paste(i, '_', stock.assessment.year, '_BlockSampled.pdf'),
    height = 5.57,
    units = 'in'
  )
  tmap_save(
    tm = catch.plot,
    filename = paste(i, '_', stock.assessment.year, '_BlockSampled.png'),
    height = 5.57,
    units = 'in'
  )
  
}

##-------------------------------------------------------------------------------------------------------##
# PLOT 5: Map catches in fish year ####

# create map indicating blocks sampled in stock assessment year (for processor)

# load map of Tasmania and abalone blocks
tas.sp <- readOGR(dsn = 'C:/Users/jaimem/Documents/ArcGIS/GIS_files/Coastlines_GIS/Tasmania_ll_(WGS 84)/Tas_25k_land_ll_wgs84.shp'
                  , layer = 'Tas_25k_land_ll_wgs84', verbose = F)

# ab.blocks.sp <- readOGR(dsn = 'C:/CloudStor/R_Stuff/BlockSHapefile/Ab_Blocks_MGAZ55.shp'
#                         , layer = 'Ab_Blocks_MGAZ55', verbose = F)

new.ab.blocks.sp <- readOGR(dsn = 'C:/GitCode/r-AbSpatialAnalyses/Tas_Ab_Polyg_SubBlocks.shp'
                            , layer = 'Tas_Ab_Polyg_SubBlocks', verbose = F)

# set projection
projstring.mga55 <- CRS("+proj=utm +zone=55 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
projstring.wgs84 <-  CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# convert maps to projection
tas.sp <- spTransform(tas.sp, projstring.mga55)
# ab.blocks.sp <- spTransform(ab.blocks.sp, projstring.mga55)

# summarise dataframe to determine number of catches measured per block by processor
df.1 <- compiledMM.df.final %>%
  dplyr::filter(fishyear == stock.assessment.year
         & numsubblocks <= 1) %>%
  group_by(subblockno) %>%
  summarise(n = n_distinct(docket.number))

ab.blocks.sp.2 <- new.ab.blocks.sp

# add data to attribute table of spatial dataframe (e.g. number of catches per block in fishyear xxxx)
ab.blocks.poly <-
  merge(ab.blocks.sp.2, df.1, by.x = 'SubBlockNo', by.y = 'subblockno')

# https://atlan.com/courses/introduction-to-gis-r/lesson3-static-maps/

# convert spatial polygon to special feature dataframe
df.3 <- st_as_sf(ab.blocks.poly)

# # convert non-sampled blocks (i.e. NA) to zero
# df.3$n[is.na(df.3$n)] <- 0

df.3 <- df.3 %>% 
  mutate(subblock.sampled = if_else(n > 0, as.character(SubBlockNo), ''))

# need to fix this code to maintain colour catergories across all processors
catch.plot <- df.3 %>%
  # filter(n >= 0) %>%
  tm_shape() +
  tm_fill(
    col = 'n',
    title = 'Catches \nmeasured',
    breaks = c(0, 2, 5, 10, 15, 20),
    labels = c('0 to 2', '2 to 5', '5 to 10', '10 to 15', '>15'),
    style = 'fixed',
    textNA = 'No data',
    colorNA = 'white',
    palette = '-Spectral'
  ) +
  tm_borders(lwd = 0.5) +
  tm_text('n', size = 0.75)+
  tm_shape(df.3)+
  tm_text('subblock.sampled', size = 0.25, just = 'bottom', ymod = -0.4)

print(catch.plot)

setwd('C:/CloudStor/R_Stuff/MMLF/MM_Plots')

tmap_save(
  tm = catch.plot,
  filename = paste('MM_SamplingMap', '_', stock.assessment.year, '_BlockSampled.pdf'),
  height = 5.57,
  units = 'in'
)
tmap_save(
  tm = catch.plot,
  filename = paste('MM_SamplingMap', '_', stock.assessment.year, '_BlockSampled.png'),
  height = 5.57,
  units = 'in'
)

##-------------------------------------------------------------------------------------------------------##
## PLOT 6: Boxplot weight ####

df.1 <- compiledMM.df.final %>% 
  filter(fishyear == stock.assessment.year,
         !is.na(whole.weight)
         & between(whole.weight, 1, 2000),
         numsubblocks <= 1)

plotdat.n <- df.1 %>%
  group_by(subblockno, fishquarter) %>%
  summarize(n = n())

grades <- data.frame(x = 1, y = c(500, 700, 900), lab = c('small', 'medium', 'large'))

quarter.whole.weight.boxplot <-
  ggplot(df.1, aes(
    x = subblockno,
    y = whole.weight,
    fill = factor(fishquarter)
  )) +
  # geom_boxplot(outlier.colour = "black", outlier.size = 1.5, position = position_dodge(preserve = 'single')) +
  geom_boxplot(
    outlier.colour = "black",
    outlier.size = 1.5,
    position = position_dodge(0.85)
  ) +
  geom_text(
    data = plotdat.n,
    aes(y = 1600, label = n),
    size = 3,
    angle = 0,
    position = position_dodge(width = 0.85)
  ) +
  # geom_hline(aes(yintercept = 132), colour = 'red', linetype = 'dotted')+
  xlab('SubBlockNo') +
  ylab(paste('Whole weight (g)')) +
  coord_cartesian(ylim = c(100, 1600)) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(angle = 0, vjust = 0.5),
    legend.position = 'top'
  ) +
  scale_fill_grey(start = 0.8, end = 0.3,
    name = 'Quarter',
    breaks = c('1', '2', '3', '4'),
    labels = c('Q1', 'Q2', 'Q3', 'Q4')
  ) +
  geom_hline(aes(yintercept = 400), colour = 'red', linetype = 'dotted')+
  geom_hline(aes(yintercept = 600), colour = 'red', linetype = 'dotted')+
  geom_hline(aes(yintercept = 800), colour = 'red', linetype = 'dotted')+
  geom_text(data = grades, aes(x = x-0.5, y = y, label = lab), inherit.aes = FALSE, vjust = 0.5, hjust = 0.5, size = 4, angle = 90)

print(quarter.whole.weight.boxplot)

setwd('C:/CloudStor/R_Stuff/MMLF/MM_Plots')

ggsave(
  filename = paste('Quarter_MM_Weight_Boxplot', '_', stock.assessment.year, '.pdf', sep = ''),
  plot = quarter.whole.weight.boxplot,
  width = 7.4,
  height = 5.57,
  units = 'in'
)
ggsave(
  filename = paste('Quarter_MM_Weight_Boxplot', '_', stock.assessment.year, '.wmf', sep = ''),
  plot = quarter.whole.weight.boxplot,
  width = 7.4,
  height = 5.57,
  units = 'in'
)
ggsave(
  filename = paste('Quarter_MM_Weight_Boxplot', '_', stock.assessment.year, '.png', sep = ''),
  plot = quarter.whole.weight.boxplot,
  width = 7.4,
  height = 5.57,
  units = 'in'
)

##-------------------------------------------------------------------------------------------------------##
# PLOT 7: Boxplot block x quarter ####

# create boxplots for each block and quarter fished in the stock assessment year (for processor)

# select data for stock assessment year
df.1 <- compiledMM.df.final %>%
  filter(fishyear == stock.assessment.year
         & numblocks <= 1 &
           shell.length >= sizelimit - 5)

# generate a count of records for each year to add label to boxplot
plotdat.n <- df.1 %>%
  group_by(blockno, fishquarter) %>%
  summarize(n = n())

sl <- df.1 %>% 
  group_by(fishyear, blockno, fishquarter) %>% 
  summarise(legal.min.length = max(sizelimit))

# create plot
quarter.boxplot <-
  ggplot(df.1, aes(
    x = blockno,
    y = shell.length,
    fill = factor(fishquarter)
  )) +
  # geom_boxplot(outlier.colour = "black", outlier.size = 1.5, position = position_dodge(preserve = 'single')) +
  geom_boxplot(
    outlier.colour = "black",
    outlier.size = 1.5,
    position = position_dodge(0.85)
  ) +
  geom_text(
    data = plotdat.n,
    aes(y = 220, label = n),
    size = 3,
    angle = 0,
    position = position_dodge(width = 0.85)
  ) +
  geom_point(
    data = sl,
    aes(x = factor(blockno), y = legal.min.length),
    shape = 95,
    size = 7,
    colour = "red",
    position = position_dodge(width = 0.85)
  ) +
  xlab('BlockNo') +
  ylab(paste('Shell Length (mm)')) +
  coord_cartesian(ylim = c(100, 225)) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(angle = 0, vjust = 0.5),
    legend.position = 'top'
  ) +
  scale_fill_grey(start = 0.8, end = 0.3,
    name = 'Quarter',
    breaks = c('1', '2', '3', '4'),
    labels = c('Q1', 'Q2', 'Q3', 'Q4')
  )+
  guides(fill = guide_legend(override.aes = list(shape = NA)))

print(quarter.boxplot)

setwd('C:/CloudStor/R_Stuff/MMLF/MM_Plots')
ggsave(
  filename = paste('Quarter_MM_Size_Boxplot_', stock.assessment.year, '.pdf', sep = ''),
  plot = quarter.boxplot,
  width = 7.4,
  height = 5.57,
  units = 'in'
)
ggsave(
  filename = paste('Quarter_MM_Size_Boxplot_', stock.assessment.year, '.wmf', sep = ''),
  plot = quarter.boxplot,
  width = 7.4,
  height = 5.57,
  units = 'in'
)
ggsave(
  filename = paste('Quarter_MM_Boxplot_', stock.assessment.year, '.png', sep = ''),
  plot = quarter.boxplot,
  width = 7.4,
  height = 5.57,
  units = 'in'
)

##-------------------------------------------------------------------------------------------------------##
# Tasmanian Seafoods grading post 2019

tas.seafoods.df <- compiledMM.df.final %>% 
  filter(processorname == 'TASMANIAN SEAFOODS PTY LTD' 
         & !is.na(whole.weight)
         & between(whole.weight, 1, 2000))
  
tas.seafoods.df <- tas.seafoods.df %>% 
  mutate(grade = dplyr::if_else(whole.weight <= 400, 'xsmall',
                         dplyr::if_else(between(whole.weight, 401, 600), 'small',
                                 dplyr::if_else(between(whole.weight, 601, 800), 'medium', 'large'))))


df.1 <- tas.seafoods.df %>% 
  group_by(docket.number) %>% 
  summarise(ab.meas = n())

df.2 <- tas.seafoods.df %>% 
  group_by(docket.number, grade) %>% 
  summarise(grade.meas = n())

df.3 <- left_join(df.2, df.1, by = 'docket.number') %>% 
  mutate(grade.perc = (grade.meas / ab.meas) * 100)

df.4 <- tas.seafoods.df %>% 
  select(c(docket.number, msr.date, daylist_max, blockno, subblockno, fishyear)) %>% 
  distinct()

df.5 <- left_join(df.3, df.4, by = 'docket.number')

df.6 <- df.5 %>% 
  group_by(blockno, grade) %>% 
  mutate(position = cumsum(grade.perc - 0.5 * grade.perc))

grade.plot <- ggplot(df.6, aes(x = '', y = grade.perc, fill = grade))+
  geom_bar(width = 1, stat = 'identity', position = position_fill())+
  coord_polar(theta = 'y', start = 0)+
  theme(axis.ticks = element_blank(), axis.title = element_blank(), 
        axis.text.y = , panel.grid  = element_blank(),
        axis.text.x = element_blank())+
  # geom_text(aes(label = sprintf("%1.2f%%", 100 * grade.perc), y = position))+
  facet_grid(. ~ blockno)

setwd('C:/CloudStor/R_Stuff/MMLF/MM_Plots')
ggsave(
  filename = paste('TASMANIAN SEAFOODS PTY LTD', '_', stock.assessment.year, '_WholeWeight_Grading.pdf', sep = ''),
  plot = grade.plot,
  width = 7.4,
  height = 5.57,
  units = 'in'
)
ggsave(
  filename = paste('TASMANIAN SEAFOODS PTY LTD', '_', stock.assessment.year, '_WholeWeight_Grading.wmf', sep = ''),
  plot = grade.plot,
  width = 7.4,
  height = 5.57,
  units = 'in'
)
ggsave(
  filename = paste('TASMANIAN SEAFOODS PTY LTD', '_', stock.assessment.year, '_WholeWeight_Grading.png', sep = ''),
  plot = grade.plot,
  width = 7.4,
  height = 5.57,
  units = 'in'
)

##-------------------------------------------------------------------------------------------------------##
# Decadal boxplot vs stock assessment year

# add decade to df
df.7 <- compiledMM.df.final %>% 
  mutate(decade = if_else(between(fishyear, 1980, 1989), '1980s',
                          if_else(between(fishyear, 1990, 1999), '1990s',
                                  if_else(between(fishyear, 2000, 2009), '2000s',
                                          if_else(between(fishyear, 2010, 2019), '2010s', NA_character_))))) 

# select data for block
df.7.dat.2019 <- df.7 %>%
  filter(
    newzone == 'E'
    & blocklist == 13
    & fishyear == 2019
    & between(shell.length, 100, 220)
  )

df.7.dat.histo <- df.7 %>%
  filter(
    newzone == 'E'
    & blocklist == 13
    & between(fishyear, 1980, stock.assessment.year - 1)
    & between(shell.length, 100, 220)
  )

df.7.block <- bind_rows(df.7.dat.2019, df.7.dat.histo)

# generate plot for zone and block combination only if data is present

  # convert required grouping variable to factor for boxplot
  
  df.7.block$fishyear <- as.factor(df.7.block$fishyear)
  df.7.block$decade <- as.factor(df.7.block$decade)
  
  # # generate a count of records for each year to add to boxplot
  # 
  # plotdat.n <- plotdat.block %>%
  #   group_by(fishyear, blockno) %>%
  #   summarize(n = n())
  
  # # generate table of size limits for each year to add to boxplot
  # 
  # size.limit <- plotdat.block %>% 
  #   group_by(fishyear, blockno) %>% 
  #   summarise(legal.min.length = max(sizelimit))
  
  # generate boxplot of shell lengths for chosen grouping variable
  
  mm.zone.boxplot <-
    ggplot(df.7.block, aes(x = fishyear, y = shell.length)) +
    geom_boxplot(outlier.colour = "orange", outlier.size = 1.5) +
    # # geom_text(
    #   data = plotdat.n,
    #   aes(y = 220, label = n),
    #   size = 3,
    #   angle = 90
    # ) +
    # geom_hline(aes(yintercept = 132), colour = 'red', linetype = 'dotted')+
    # geom_point(data = size.limit, aes(x = fishyear, y = legal.min.length), 
               # shape = 95, size = 7, colour = "red")+
    xlab('Year') +
    # ylab(paste('BlockNo', j, 'Shell Length (mm)')) +
    coord_cartesian(ylim = c(100, 225)) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black"),
      axis.text.x = element_text(angle = 0, vjust = 0.5)
    )+
    facet_wrap(.~decade, scales = 'free_x', nrow = 2)
  
  print(mm.zone.boxplot)
