# The following scripts produce summary plots for processors for the current stock
# assessment year.

# Created by: Jaime McAllister

##----------------------------------------------------------------------------##
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
target_year <- 2023

target_year_folder <- paste('C:/CloudStor/DiveFisheries/Abalone/Assessment/Figures/MM/', target_year, "/",
                            'MM_Plots_2023ProcessorSummaries/', sep = '')
##---------------------------------------------------------------------------##
# 3. Load data ####

# load most recent compiled MM dataframe
compiledMM.df.final <- readRDS('C:/CloudStor/R_Stuff/MMLF/compiledMM.df.final.RDS')

##----------------------------------------------------------------------------##
# Plot 1: YTD LW Status ####

# Length and weight frequency density plot last five years overlaid with processor 
# measuring board data for the present year

# Set zone and block
i <- 'E'
j <- '21'

# Filter length data from compiled MM dataframe
plot.length.freq.dat <- compiledMM.df.final %>%
 filter(
  newzone == i
  & blocklist == j
  & between(fishyear, target_year - 5, target_year)
  & between(shell.length, sizelimit - 5, 220))

# Filter YTD length data for processor
plot.length.freq.dat.2 <- compiledMM.df.final %>%
 filter(
  processorname == 'TASMANIAN SEAFOODS PTY LTD'
  & newzone == i
  & blocklist == j
  & fishyear == target_year
  & between(shell.length, sizelimit - 5, 220))

# Create length plot
length_plot <- ggplot(plot.length.freq.dat, 
                      aes(shell.length, 
                          colour = as.factor(fishyear),
                          fill = as.factor(fishyear))) +
 geom_density(aes(y = ..density.. * 5), lwd = 1, linetype = 1, alpha = 0.3, show_guide = F)+
 stat_density(aes(y = ..density.. * 5), lwd = 1, linetype = 1, geom = "line", position = "identity")+
 geom_density(data = plot.length.freq.dat.2, aes(shell.length, y = ..density.. * 5), 
              lwd = 1, linetype = 1, colour = 'black', fill = 'grey', alpha = 0.3)+
 scale_y_continuous(labels = percent_format(accuracy = 1, suffix = ''))+
 xlab("Shell Length (mm)") +
 ylab(paste(i, "BlockNo", j, " Percentage (%)")) +
 coord_cartesian(xlim = c(125, 200), ylim = c(0, 0.3)) +
 scale_colour_discrete(name = 'FishYear')+
 scale_fill_discrete(guide = 'none')+
 theme_bw()+
 geom_vline(
  aes(xintercept = ifelse(newzone == 'W', 145, 
                          ifelse(newzone == 'BS', 114, 
                                 ifelse(newzone == 'N', 127, 140)))),
  linetype = 'dotted',
  colour = 'red')

# Filter weight data from compiled MM dataframe
plot.weight.freq.dat <- compiledMM.df.final %>%
 filter(
  newzone == i
  & blocklist == j
  & between(fishyear, target_year - 5, target_year)
  & between(shell.length, sizelimit - 5, 220))

# Filter YTD weight data for processor
plot.weight.freq.dat_2 <- compiledMM.df.final %>%
 filter(
  processorname == 'TASMANIAN SEAFOODS PTY LTD'
  & newzone == i
  & blocklist == j
  & fishyear == target_year
  & between(shell.length, sizelimit - 5, 220))

weight_plot <- ggplot(plot.weight.freq.dat, 
                      aes(whole.weight, 
                          colour = as.factor(fishyear),
                          fill = as.factor(fishyear))) +
 geom_density(aes(y = ..density.. * 50), lwd = 1, linetype = 1, alpha = 0.3, show_guide = F)+
 stat_density(aes(y = ..density.. * 50), lwd = 1, linetype = 1, geom = "line", position = "identity")+
 geom_density(data = plot.weight.freq.dat_2, aes(whole.weight, y = ..density.. * 50), 
              lwd = 1, linetype = 1, colour = 'black', fill = 'grey', alpha = 0.3)+
 scale_y_continuous(labels = percent_format(accuracy = 1, suffix = ''))+
 coord_cartesian(xlim = c(100, 1300), ylim = c(0, 0.3)) +
 xlab("Weight (g)") +
 ylab(paste(i, "BlockNo", j, " Percentage (%)")) +
 scale_colour_discrete(name = 'FishYear')+
 scale_fill_discrete(guide = 'none')+
 theme_bw()+
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
   y = 0.3,
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
 )

# extract the legend from one of the plots
plot_legend <- get_legend(
 length_plot + 
  guides(color = guide_legend(nrow = 1)) +
  theme(legend.position = "bottom")
)

# combine length and weight plots
length_weight_plot <- grid.arrange(
 arrangeGrob(cowplot::plot_grid(weight_plot + theme(legend.position = 'none'), 
                                length_plot + theme(legend.position = 'none'), align = 'v', 
                                ncol = 1), ncol = 1))

cowplot::plot_grid(length_weight_plot, plot_legend, ncol = 1, rel_heights = c(1, 0.1))
