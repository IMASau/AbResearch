library(remotes)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

# read raw data
overstory.dat <- read.csv("C:/Users/jaimem/OneDrive - University of Tasmania/Documents/AB_proteomics/density_overstory_pooled.csv",
                     header = T)

df.1 <- overstory.dat %>% 
 group_by(Site) %>% 
 mutate(per.dec = round(mean_freq/sum(mean_freq)*100, 2),
        per = paste0(round(mean_freq/sum(mean_freq)*100, 0), "%"),
        mean_freq = round(mean_freq, 1),
        se_freq = round(se_freq, 1)) %>% 
 ungroup() %>%
 arrange(desc(per.dec)) %>% 
 group_by(Site) %>%
 slice(1:2)

understory.dat <- read.csv("C:/Users/jaimem/OneDrive - University of Tasmania/Documents/AB_proteomics/mean_biomass_group_year_group.csv",
                          header = T)

df.2 <- understory.dat %>% 
 group_by(Site) %>% 
 mutate(per.dec = round(Biomass/sum(Biomass)*100, 2),
        per = paste0(round(Biomass/sum(Biomass)*100, 0), "%"),
        bio.tot = round(Biomass, 1),
        bio.se = round(Biomass_se, 1)) %>% 
 ungroup()
