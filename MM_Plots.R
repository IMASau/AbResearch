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
stock.assessment.year <- 2019

##-------------------------------------------------------------------------------------------------------##
# Identify zone/blocks/processors ####

# identify stock assessment zone of interest
stock.assessment.zone <- 'E'

# identify unique blocks sampled in year and zone of interest
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
# Fish year summary ####

# create summary table of most recent year catch sampling data to determine zones (and blocks) sampled
fishyear.summary.df <- compiledMM.df.final %>%
  dplyr::filter(fishyear == stock.assessment.year
         & numblocks <= 1) %>%
  group_by(newzone, blocklist) %>%
  summarise(catches = n_distinct(docket.number),
            n = n(),
            med.sl = as.character(median(shell.length)),
            max.sl = as.character(max(shell.length))) %>%
  rename(Zone = newzone,
         BlockNo = blocklist) %>% 
  as.data.frame()

# rename column headings
colnames(fishyear.summary.df) <- c('Zone', 'Block\nNo', 'Catches\nMeasured', 'Abalone\nMeasured', 'Median\nSL', 'Max\nSL')

# add totals for numeric columns (Note: median and max have been converted to characters)
fishyear.summary.df <- adorn_totals(fishyear.summary.df, fill = '')

# save Excel and Latex tables
setwd('C:/CloudStor/R_Stuff/MMLF/MM_Plots')
write.xlsx(fishyear.summary.df, 'MM_Sampling_Summary_2019.xlsx', sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)
print(xtable(fishyear.summary.df, type = 'latex'), file = 'MM_Sampling_Summary_2019.tex')

##-------------------------------------------------------------------------------------------------------##
# Processor x fish year summary ####

# create summary table of most recent year catch sampling data to determine zones (and blocks) sampled

for(i in processors.2019) {
  processor.fishyear.summary.df <- compiledMM.df.final %>%
    dplyr::filter(fishyear == stock.assessment.year
                  & numblocks <= 1
                  & processorname == i) %>%
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
        & between(shell.length, 100, 220)
      )
    
    plotdat.block.historic <- compiledMM.df.final %>%
      filter(
        newzone == i
        & blocklist == j
        & between(fishyear, 1981, 1996)
        & between(shell.length, 100, 220)
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
      
      # generate boxplot of shell lengths for chosen grouping variable
      
      mm.zone.boxplot <-
        ggplot(plotdat.block, aes(x = fishyear, y = shell.length)) +
        geom_boxplot(outlier.colour = "orange", outlier.size = 1.5) +
        geom_text(
          data = plotdat.n,
          aes(y = 220, label = n, ),
          size = 3,
          angle = 90
        ) +
        # geom_hline(aes(yintercept = 132), colour = 'red', linetype = 'dotted')+
        xlab('Year') +
        ylab(paste('BlockNo', j, 'Shell Length (mm)')) +
        coord_cartesian(ylim = c(100, 225)) +
        theme_bw() +
        theme(
          plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.text.x = element_text(angle = 0, vjust = 0.5)
        )
      
      # print(mm.zone.boxplot)
      
      setwd('C:/CloudStor/R_Stuff/MMLF/MM_Plots')
      ggsave(
        filename = paste(i, '_BlockNo', j, '_MM_Boxplot_2019', '.pdf', sep = ''),
        plot = mm.zone.boxplot,
        width = 7.4,
        height = 5.57,
        units = 'in'
      )
      ggsave(
        filename = paste(i, '_BlockNo', j, '_MM_Boxplot_2019', '.wmf', sep = ''),
        plot = mm.zone.boxplot,
        width = 7.4,
        height = 5.57,
        units = 'in'
      )
      ggsave(
        filename = paste(i, '_BlockNo', j, '_MM_Boxplot_2019', '.png', sep = ''),
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
           & numblocks <= 1)
  
  # generate a count of records for each year to add label to boxplot
  plotdat.n <- df.1 %>%
    group_by(blockno, fishquarter) %>%
    summarize(n = n())
  
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
      angle = 90,
      position = position_dodge(width = 0.85)
    ) +
    # geom_hline(aes(yintercept = 132), colour = 'red', linetype = 'dotted')+
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
    scale_fill_grey(
      name = 'Quarter',
      breaks = c('1', '2', '3', '4'),
      labels = c('Q1', 'Q2', 'Q3', 'Q4')
    )
  
  print(quarter.boxplot)
  
  setwd('C:/CloudStor/R_Stuff/MMLF/MM_Plots')
  ggsave(
    filename = paste(i, '_Quarter_MM_Boxplot_2019', '.pdf', sep = ''),
    plot = quarter.boxplot,
    width = 7.4,
    height = 5.57,
    units = 'in'
  )
  ggsave(
    filename = paste(i, '_Quarter_MM_Boxplot_2019', '.wmf', sep = ''),
    plot = quarter.boxplot,
    width = 7.4,
    height = 5.57,
    units = 'in'
  )
  ggsave(
    filename = paste(i, '_Quarter_MM_Boxplot_2019', '.png', sep = ''),
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

ab.blocks.sp <- readOGR(dsn = 'C:/CloudStor/R_Stuff/BlockSHapefile/Ab_Blocks_MGAZ55.shp'
                        , layer = 'Ab_Blocks_MGAZ55', verbose = F)

# set projection
projstring.mga55 <- CRS("+proj=utm +zone=55 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
projstring.wgs84 <-  CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# convert maps to projection
tas.sp <- spTransform(tas.sp, projstring.mga55)
ab.blocks.sp <- spTransform(ab.blocks.sp, projstring.mga55)

# generate maps

for(i in processors.2019) {
  # summarise dataframe to determine number of catches measured per block by processor
  df.1 <- compiledMM.df.final %>%
    filter(fishyear == stock.assessment.year
           & processorname == i
           & numblocks <= 1)
  
  df.1 <- df.1 %>%
    group_by(blockno) %>%
    summarise(n = n_distinct(docket.number))
  
  ab.blocks.sp.2 <- ab.blocks.sp
  
  # add data to attribute table of spatial dataframe (e.g. number of catches per block in fishyear xxxx)
  ab.blocks.poly <-
    merge(ab.blocks.sp.2, df.1, by.x = 'BLOCK_NO', by.y = 'blockno')
  
  # https://atlan.com/courses/introduction-to-gis-r/lesson3-static-maps/
  
  # convert spatial polygon to special feature dataframe
  df.3 <- st_as_sf(ab.blocks.poly)
  
  # # convert non-sampled blocks (i.e. NA) to zero
  # df.3$n[is.na(df.3$n)] <- 0
  
  # need to fix this code to maintain colour catergories across all processors
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
    tm_text('n', size = 0.75)
  
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

# PLOT 5: Map catches in fish year ####

# create map indicating blocks sampled in stock assessment year (for processor)

# load map of Tasmania and abalone blocks
tas.sp <- readOGR(dsn = 'C:/Users/jaimem/Documents/ArcGIS/GIS_files/Coastlines_GIS/Tasmania_ll_(WGS 84)/Tas_25k_land_ll_wgs84.shp'
                  , layer = 'Tas_25k_land_ll_wgs84', verbose = F)

ab.blocks.sp <- readOGR(dsn = 'C:/CloudStor/R_Stuff/BlockSHapefile/Ab_Blocks_MGAZ55.shp'
                        , layer = 'Ab_Blocks_MGAZ55', verbose = F)

# set projection
projstring.mga55 <- CRS("+proj=utm +zone=55 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
projstring.wgs84 <-  CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# convert maps to projection
tas.sp <- spTransform(tas.sp, projstring.mga55)
ab.blocks.sp <- spTransform(ab.blocks.sp, projstring.mga55)

# summarise dataframe to determine number of catches measured per block by processor
df.1 <- compiledMM.df.final %>%
  dplyr::filter(fishyear == stock.assessment.year
         & numblocks <= 1)%>%
  group_by(blockno) %>%
  summarise(n = n_distinct(docket.number))

ab.blocks.sp.2 <- ab.blocks.sp

# add data to attribute table of spatial dataframe (e.g. number of catches per block in fishyear xxxx)
ab.blocks.poly <-
  merge(ab.blocks.sp.2, df.1, by.x = 'BLOCK_NO', by.y = 'blockno')

# https://atlan.com/courses/introduction-to-gis-r/lesson3-static-maps/

# convert spatial polygon to special feature dataframe
df.3 <- st_as_sf(ab.blocks.poly)

# # convert non-sampled blocks (i.e. NA) to zero
# df.3$n[is.na(df.3$n)] <- 0

# need to fix this code to maintain colour catergories across all processors
catch.plot <- df.3 %>%
  # filter(n >= 0) %>%
  tm_shape() +
  tm_fill(
    col = 'n',
    title = 'Catches \nmeasured',
    breaks = c(0, 2, 5, 10, 15),
    style = 'fixed',
    textNA = 'No data',
    colorNA = 'white',
    palette = '-Spectral'
  ) +
  tm_borders(lwd = 0.5) +
  tm_text('n', size = 0.75)

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
