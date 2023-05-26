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

mycols <- c('Brown' = 'chocolate4', 
            'Green' = 'darkgreen', 
            'Red' = 'red')

unique(df.2$Site)

c("Black Reef", "Gardens", "Mouldy Hole", "Seymour", "Sisters", "Thumbs")

plot_site <- 'Mouldy Hole'

pie_plot <- df.2 %>% 
 filter(Site == plot_site) %>% 
 ggplot(aes(x = "", y = per.dec, fill = Group))+
 geom_bar(width = 1, stat = 'identity', colour = 'white')+
 coord_polar("y", start = 0)+
 scale_fill_manual(values = mycols)+
 theme_void()+
 theme(legend.position = 'none')

ggsave(filename = paste('C:/cloudstor/R_Stuff/WEI/Results/PhysiologySeasonalSite_UnderstoryPiePlot_', plot_site, '.png', sep = ''), 
       plot = pie_plot)
 