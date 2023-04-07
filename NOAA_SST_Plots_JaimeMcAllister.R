library(dplyr)
library(tidyr)
library(sf)
library(lubridate)
library(ggplot2)
library(openxlsx)

noaa_aus <- readRDS('C:/cloudstor/R_Stuff/WEI/Results/noaa_Aus_2023_03_07.RDS')

unique(noaa_aus$sau_name)

##---------------------------------------------------------------------------##
# SST plot for FAO paper - for John Keane 2023-03-10

df.1 <- noaa_aus %>% 
 mutate(samp_year = year(timestamp),
        samp_month = month(timestamp),
        month_year = paste(samp_year, samp_month, sep = '_')) %>% 
 filter(sau_name %in% c('AB22', 'AB30'),
        between(samp_year, 1982, 2022),
 samp_month %in% c(8)) %>%
 group_by(sau_name, samp_year) %>% 
 summarise(mean_sst = mean(C),
           q10_sst = quantile(C, probs = c(0.1)),
           q90_sst = quantile(C, probs = c(0.9)))

sst_plot <- df.1 %>% ggplot(aes(x = samp_year, y = mean_sst, group = sau_name, colour = sau_name))+
 geom_line()+
 geom_smooth(method = lm, se = F)+
 theme_bw()+
 geom_hline(yintercept = 12, linetype = 'dashed', colour = 'red')+
 xlab('Year')+
 ylab(expression("Temperature " ( degree~C)))+
 guides(colour = guide_legend(reverse = T))+
 scale_colour_discrete(name = '', labels = c('Southeast', 'Northeast'))+
 scale_y_continuous(breaks = seq(10, 15, by = 0.5))+
 theme(legend.position = c(0.15, 0.9),
       legend.background = element_blank(),
       text = element_text(size = 20))

ggsave(filename = paste('C:/cloudstor/R_Stuff/WEI/Results/EastcoastMeanAnnual_SST_NEvsSE_1982-2022', '.pdf', sep = ''), 
       plot = sst_plot, units = 'mm', width = 190, height = 200)
ggsave(filename = paste('C:/cloudstor/R_Stuff/WEI/Results/EastcoastMeanAnnual_SST_NEvsSE_1982-2022', '.png', sep = ''), 
       plot = sst_plot, units = 'mm', width = 190, height = 200)

##---------------------------------------------------------------------------##
# SST plot for physiology sites in 2021-2022

df.2 <- noaa_aus %>% 
 mutate(samp_year = year(timestamp),
        samp_month = month(timestamp),
        month_year = paste(samp_year, samp_month, sep = '_')) %>% 
 filter(sau_name %in% c('AB13', 'AB22', 'AB23', 'AB28', 'AB30'),
        between(samp_year, 2021, 2022))

df.2 %>% ggplot(aes(x = timestamp, y = C, group = sau_name, colour = sau_name))+
 geom_line()+
 theme_bw()+
 xlab('Year')+
 ylab(expression("Temperature " ( degree~C)))+
 scale_colour_discrete(name = '', labels = c('ACT', 'THU', 'SIS', 'SEY', 'GAR'))

##---------------------------------------------------------------------------##
# WEI for physiology sites

wei_2010 <- readRDS('C:/Users/jaimem/Dropbox/AbaloneData/WEI_2010.rds')

wei_2010 <- wei_2010 %>% 
 st_as_sf() %>% 
 st_transform(st_crs(7855))

physio_sites <- read.xlsx("C:/Users/jaimem/OneDrive - University of Tasmania/Documents/AB_proteomics/Abalone_ProteomicCollection_Sites_July2021.xlsx",
                       detectDates = T)

physio_sites_sf <- physio_sites %>% 
 filter(label %in% c('MOL', 'ACT-2', 'THU', 'SIS', 'SEY', 'GAR')) %>%  
 mutate(label = gsub('ACT-2', 'ACT', label)) %>% 
 st_as_sf(coords = c('longitude', 'latitude'), crs = 4326) %>% 
 st_transform(st_crs(7855))
  
physio_df <- st_join(physio_sites_sf, wei_2010, join = st_nearest_feature)

physio_df$label <- factor(physio_df$label, levels = c('MOL', 'ACT', 'THU', 'SIS', 'SEY', 'GAR'))

physio_df %>% ggplot(aes(x = label, y = waveyear))+
 geom_bar(stat = 'identity')+
 theme_bw()

df.2 <- physio_df %>% 
 gather(key = 'wei_month', value = 'wei', month_1:month_12)

levels(df.2$wei_month) <- list(
 Jan = 'month_1',
 Feb = 'month_2',
 Mar = 'month_3',
 Apr = 'month_4',
 May = 'month_5',
 Jun = 'month_6',
 Jul = 'month_7',
 Aug = 'month_8',
 Sep = 'month_9',
 Oct = 'month_10',
 Nov = 'month_11',
 Dec = 'month_12'
)

df.2 %>% ggplot(aes(x = wei_month, y = wei, group = label, colour = label))+
 geom_line()


