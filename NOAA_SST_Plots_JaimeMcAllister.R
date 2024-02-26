library(dplyr)
library(tidyr)
library(sf)
library(lubridate)
library(ggplot2)
library(ggfortify)
library(openxlsx)
library(RColorBrewer)
library(tidyverse)
library(oce)
library(ocedata)
library(astsa)
library(ggfortify)
library(ggpmisc)
library(strucchange)
library(changepoint)

source("C:/GitCode/AbResearch/getSeason.r")

# Import latest noaa data 
noaa_data_folder <- file.path(paste(sprintf('C:/Users/%s/Dropbox (UTAS Research)/DiveFisheries/Abalone/Climatologydata/WEI/Results//',
                                              Sys.info()[["user"]])))

# noaa_aus <- readRDS('C:/cloudstor/R_Stuff/WEI/Results/noaa_Aus_2023_03_07.RDS')

noaa_aus_pre2024 <- readRDS(paste(noaa_data_folder, 'noaa_archive_Aus_1981_2023.RDS', sep = ''))

noaa_aus_post2024 <- readRDS(paste(noaa_data_folder, 'noaa_Aus_2024_01_19.RDS', sep = ''))

noaa_aus <- bind_rows(noaa_aus_pre2024, noaa_aus_post2024)

# Import physiology seasonal sampling data
physio_dat <- read.xlsx("C:/Users/jaimem/OneDrive - University of Tasmania/Documents/AB_proteomics/Abalone_Proteomics_Data_SeasonalSampling_July2021.xlsx",
                        sheet = 'SeasonalData', detectDates = T)

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
 summarise(mean_sst = mean(sst),
           q10_sst = quantile(sst, probs = c(0.1)),
           q90_sst = quantile(sst, probs = c(0.9)))

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

write.xlsx(df.1,
           file = paste(noaa_data_folder, 'McAllister_FAO_Report_Figure_2_data', '.xlsx'),
           sheetName = "Sheet1",
           col.names = TRUE,
           row.names = TRUE,
           append = FALSE)

##---------------------------------------------------------------------------##
# Physiology marine heat wave event data

df.1 <- physio_dat %>%
 mutate(samp_season = getSeason(date),
        samp_season = if_else(site %in% c('SIS', 'THU') & date %in% c(as.Date('2022-06-16')), 'Autumn',
                              if_else(site %in% c('GAR') & date %in% c(as.Date('2022-06-21')), 'Autumn', samp_season))) %>% 
 filter(site == 'ACT' & 
         samp_season == 'Summer'|
         grepl('Additional', comments))

##---------------------------------------------------------------------------##
# SST plot for physiology sites in 2021-2022
# Plot of daily water temperature of each site in sampling period 2021-2022 and sampling periods

# Select NOAA data for physiology site Blocks/SAUs for sampling period 2021-2022
noaa_physio_site_dat <- noaa_aus %>% 
 mutate(samp_year = year(timestamp),
        samp_month = month(timestamp),
        month_year = paste(samp_year, samp_month, sep = '_')) %>% 
 filter(sau_name %in% c('AB13', 'AB14', 'AB22', 'AB23', 'AB28', 'AB30'),
        between(samp_year, 2021, 2022))

# Add site names to match sau_name
noaa_physio_site_dat <- noaa_physio_site_dat %>% 
 mutate(site = case_when(sau_name == 'AB13' ~ 'MOL',
                         sau_name == 'AB14' ~ 'ACT',
                         sau_name == 'AB22' ~ 'THU',
                         sau_name == 'AB23' ~ 'SIS',
                         sau_name == 'AB28' ~ 'SEY',
                         sau_name == 'AB30' ~ 'GAR'))
 
noaa_physio_site_dat$site <- factor(noaa_physio_site_dat$site, levels = c('GAR', 'SEY', 'SIS', 'THU', 'ACT', 'MOL'))

df_1 <- noaa_site_dat %>% 
 group_by(site) %>% 
 summarise(min_temp = min(C),
           max_temp = max(C),
           mean_temp = mean(C),
           sd_temp = sd(C),
           n_temp = n(),
           se_temp = sd_temp/sqrt(n_temp))


# Add seasonal variable noting adjustment for late Autumn sampling at some sites
physio_season_dat <- physio_dat %>% 
 mutate(samp_season = getSeason(date),
        samp_season = if_else(site %in% c('SIS', 'THU') & date %in% c(as.Date('2022-06-16')), 'Autumn',
                              if_else(site %in% c('GAR') & date %in% c(as.Date('2022-06-21')), 'Autumn', samp_season)))

# Summarise min and max sampling dates for each season
physio_season_dates <- physio_season_dat %>% 
 filter(!grepl('Additional', comments)) %>% 
 group_by(samp_season) %>% 
 summarise(min_date = min(date),
           max_date = max(date)) %>% 
 mutate(samp_season = factor(samp_season, levels = c('Winter', 'Spring', 'Summer', 'Autumn')),
        cols = c('green', 'purple', 'red', 'blue'))

# Create vector of plot colours for sites to match metabolomics analysis
plot_cols <- c('red', 'purple', 'orange', 'yellow3', 'green3', 'blue')
plot_labs <- c('GAR', 'SEY', 'SIS', 'THU', 'ACT', 'MOL')

physio_site_plot <- noaa_physio_site_dat %>% 
 ggplot(aes(x = as.Date(timestamp), y = C, group = site, colour = site))+
 geom_line()+
 theme_bw()+
 xlab('Year')+
 ylab(expression("Temperature " ( degree~C)))+
 geom_vline(data = physio_season_dates, aes(xintercept = as.Date(min_date)), linetype = 'dashed', alpha = 0.2)+
 geom_vline(data = physio_season_dates, aes(xintercept = as.Date(max_date)), linetype = 'dashed', alpha = 0.2)+
 scale_colour_manual(name = '', values = plot_cols, labels = plot_labs)+
 scale_x_date(date_labels = '%b-%Y')+
 geom_rect(data= physio_season_dates, inherit.aes = FALSE,
           aes(xmin = min_date, xmax = max_date, ymin = -Inf, ymax = Inf, group = samp_season, fill = samp_season), 
           alpha = 0.2)+
 scale_fill_manual(values = c('blue', 'purple', 'red', 'green3'), name = '')
 # theme(legend.title = element_blank(),
 #       legend.position = c(0, 1),
 #       legend.justification = c(0, 1),
 #       legend.background = element_rect(fill = "white", color = "black", linewidth = 0.2))

ggsave(filename = paste('C:/cloudstor/R_Stuff/WEI/Results/PhysiologySeasonalSiteTemperature_2021-2022', '.pdf', sep = ''), 
       plot = physio_site_plot, units = 'mm', width = 190, height = 200)

ggsave(filename = paste('C:/cloudstor/R_Stuff/WEI/Results/PhysiologySeasonalSiteTemperature_2021-2022', '.png', sep = ''), 
       plot = physio_site_plot, units = 'mm', width = 190, height = 200)

##---------------------------------------------------------------------------##
# SST plot for physiology sites historical
# Plot of daily water temperature of each site using all historical data

# Select NOAA data for physiology site Blocks/SAUs
noaa_site_dat <- noaa_aus %>% 
 mutate(samp_year = year(timestamp),
        samp_month = month(timestamp),
        month_year = paste(samp_year, samp_month, sep = '_')) %>% 
 filter(sau_name %in% c('AB13', 'AB14', 'AB22', 'AB23', 'AB28', 'AB30'))

# Add site names to match sau_name
noaa_site_dat <- noaa_site_dat %>% 
 mutate(site = case_when(sau_name == 'AB13' ~ 'MOL',
                         sau_name == 'AB14' ~ 'ACT',
                         sau_name == 'AB22' ~ 'THU',
                         sau_name == 'AB23' ~ 'SIS',
                         sau_name == 'AB28' ~ 'SEY',
                         sau_name == 'AB30' ~ 'GAR'))

noaa_site_dat$site <- factor(noaa_site_dat$site, levels = c('GAR', 'SEY', 'SIS', 'THU', 'ACT', 'MOL'))

# Create vector of plot colours for sites to match metabolomics analysis
plot_cols <- c('red', 'purple', 'orange', 'yellow3', 'green3', 'blue')
plot_labs <- c('GAR', 'SEY', 'SIS', 'THU', 'ACT', 'MOL')

physio_site_temperature_plot <- noaa_site_dat %>% 
 ggplot(aes(x = as.Date(timestamp), y = C, group = site, colour = site))+
 # geom_line()+
 geom_smooth(method = lm, se = F)+
 scale_y_continuous(breaks = seq(13, 16, 0.5))+
 theme_bw()+
 xlab('Year')+
 ylab(expression("Temperature " ( degree~C)))+
 scale_colour_manual(name = '', values = plot_cols, labels = plot_labs)+
 scale_x_date(date_labels = '%b-%Y')+
 theme(legend.title = element_blank(),
       legend.position = c(0.1, 0.85),
       # legend.justification = c(0, 1),
       legend.background = element_rect(fill = "white", color = "black", linewidth = 0.2))

ggsave(filename = paste('C:/cloudstor/R_Stuff/WEI/Results/PhysiologySeasonalSiteTemperature_1981-2022', '.pdf', sep = ''), 
       plot = physio_site_temperature_plot, units = 'mm', width = 190, height = 200)

ggsave(filename = paste('C:/cloudstor/R_Stuff/WEI/Results/PhysiologySeasonalSiteTemperature_1981-2022', '.png', sep = ''), 
       plot = physio_site_temperature_plot, units = 'mm', width = 190, height = 200)

##---------------------------------------------------------------------------##
# WEI for physiology sites

# Load wave exposure index for points every 100 m along Tasmanian coastline
wei_2010 <- readRDS('C:/Users/jaimem/Dropbox/AbaloneData/WEI_2010.rds')

wei_2010 <- wei_2010 %>% 
 st_as_sf() %>% 
 st_transform(st_crs(7855))

# Load physiology sampling sites
physio_sites <- read.xlsx("C:/Users/jaimem/OneDrive - University of Tasmania/Documents/AB_proteomics/Abalone_ProteomicCollection_Sites_July2021.xlsx",
                       detectDates = T)

physio_sites_sf <- physio_sites %>% 
 filter(label %in% c('MOL', 'ACT-2', 'THU', 'SIS', 'SEY', 'GAR')) %>%  
 mutate(label = gsub('ACT-2', 'ACT', label)) %>% 
 st_as_sf(coords = c('longitude', 'latitude'), crs = 4326) %>% 
 st_transform(st_crs(7855))
  
# Join physiology sites to nearest WEI point along coastline
physio_df <- st_join(physio_sites_sf, wei_2010, join = st_nearest_feature)

plot_cols <- c('GAR' = 'red', 
               'SEY' = 'purple',
               'SIS' = 'orange', 
               'THU' = 'yellow3',
               'ACT' = 'green3', 
               'MOL' = 'blue')
plot_labs <- c('GAR', 'SEY', 'SIS', 'THU', 'ACT', 'MOL')

physio_df$label <- factor(physio_df$label, levels = c('GAR', 'SEY', 'SIS', 'THU', 'ACT', 'MOL'))

# Quick plot of annual WEI for each site
physio_wei_plot_annual <- physio_df %>% ggplot(aes(x = label, y = waveyear, fill = label))+
 geom_bar(stat = 'identity')+
 scale_fill_manual(values = plot_cols)+
 theme_bw()+
 ylab('WEI')+
 xlab('Site')+
 theme(legend.position = 'none')

# Re-format monthly WEI variable names and values for plotting
physio_wei_df <- physio_df %>%
 gather(key = 'wei_month', value = 'wei', month_1:month_12) %>%
 mutate(wei_month = month.abb[as.numeric(gsub('month_', '', wei_month))],
        wei_month = factor(wei_month, levels = month.abb))
 

physio_wei_plot <- physio_wei_df %>% 
 ggplot(aes(x = wei_month, y = wei, group = label, colour = label))+
 geom_line()+
 geom_point()+
 theme_bw()+
 ylab('WEI')+
 xlab('Month')+
 scale_colour_manual(name = '', values = plot_cols, labels = plot_labs)+
 theme(legend.title = element_blank(),
  legend.position = c(0.1, 0.85),
       # legend.justification = c(0, 1),
       legend.background = element_rect(fill = "white", color = "black", linewidth = 0.2))

ggsave(filename = paste('C:/cloudstor/R_Stuff/WEI/Results/PhysiologySeasonalSiteWEI', '.pdf', sep = ''), 
       plot = physio_wei_plot, units = 'mm', width = 190, height = 200)

ggsave(filename = paste('C:/cloudstor/R_Stuff/WEI/Results/PhysiologySeasonalSiteWEI', '.png', sep = ''), 
       plot = physio_wei_plot, units = 'mm', width = 190, height = 200)

ggsave(filename = paste('C:/cloudstor/R_Stuff/WEI/Results/PhysiologyAnnualSiteWEI', '.pdf', sep = ''), 
       plot = physio_wei_plot_annual, units = 'mm', width = 190, height = 200)

ggsave(filename = paste('C:/cloudstor/R_Stuff/WEI/Results/PhysiologyAnnualSiteWEI', '.png', sep = ''), 
       plot = physio_wei_plot_annual, units = 'mm', width = 190, height = 200)

##---------------------------------------------------------------------------##
## Non-linear seasonal trend - Craig's

noaa <- noaa_aus %>% within({
 year <- year(timestamp)
 yday <- yday(timestamp)
 satdate <- as.Date(timestamp)
 geom <- NULL
})

# Select NOAA data for physiology site Blocks/SAUs for sampling period 2021-2022
noaa_physio_site_dat <- noaa %>% 
 mutate(samp_year = year(timestamp),
        samp_month = month(timestamp),
        month_year = paste(samp_year, samp_month, sep = '_')) %>% 
 filter(sau_name %in% c('AB13', 'AB14', 'AB22', 'AB23', 'AB28', 'AB30'))

# Add site names to match sau_name
noaa_physio_site_dat <- noaa_physio_site_dat %>% 
 mutate(site = case_when(sau_name == 'AB13' ~ 'MOL',
                         sau_name == 'AB14' ~ 'ACT',
                         sau_name == 'AB22' ~ 'THU',
                         sau_name == 'AB23' ~ 'SIS',
                         sau_name == 'AB28' ~ 'SEY',
                         sau_name == 'AB30' ~ 'GAR'))

noaa_physio_site_dat$site <- factor(noaa_physio_site_dat$site, levels = c('GAR', 'SEY', 'SIS', 'THU', 'ACT', 'MOL'))

# # unique site names
# noaa_sites <- unique(noaa_physio_site_dat$site)
# 
# # create empty list
# trend_list <- list()
# 
# # loop through dataframe to generate trends for each site
# for(i in noaa_sites){
#  noaa.day <- noaa_physio_site_dat %>%
#   filter(site == i)
# 
# ## time-series object
# noaa.ts <- ts(noaa.day$C, start = c(1989,9) ,frequency = 365.24, class = "ts")
# decomposition <- stl(noaa.ts, s.window = 365, t.window = 7001)
# trend_list[[i]] <- decomposition$time.series[,2]
# }
# 
# # create dataframe from list
# # need to code to convert time-series list to single time series

# df_2 <- ts(bind_rows(trend_list))
# plot(df_2, col = 1:6)


unique(noaa_physio_site_dat$site)
# GAR SEY SIS THU ACT MOL
i <- 'GAR'

noaa.day <- noaa_physio_site_dat %>% 
 filter(site == i)

## time-series object
noaa.ts <- ts(noaa.day$C, start = c(1981,9) ,frequency = 365.24, class = "ts")
decomposition <- stl(noaa.ts, s.window = 365, t.window = 7001)
trend_mol <- decomposition$time.series[,2]

trend <- ts.union(trend_gar, trend_sey, trend_sis, trend_thu, trend_act, trend_mol)

saveRDS(trend, file = 'C:/Users/jaimem/Dropbox/AbaloneData/SeasonalPhysiologySites_TemperatureTrends_1981-2023.rds')
##---------------------------------------------------------------------------##
## Identify change point of daily sea surface temperature
daily.changepoints <- noaa.ts %>% 
 changepoint::cpt.meanvar(penalty="AIC") 

daily.changepoints %>% summary()

daily.changepoints %>% 
 changepoint::plot(xlab = "", ylab = expression(Temperature~(degree*C)), las = 1)

# manually find temperature at cp (e.g. for GAR)for creating geom_point
# window(trend, start = 1998.296)

cp_summary <- data.frame(site = c('GAR', 'SEY', 'SIS', 'THU', 'ACT', 'MOL'),
                         cp = c(6310, 6310, 6313, 6331, 10330, 10330),
                         C = c(14.86379, 14.76829, 14.27378, 14.23792, 13.94630, 13.94391),
                         plot_colour <- c('red','purple','orange','yellow3','green3','blue')) %>%
 mutate(cp_dec_date = decimal_date(ymd("1981-01-08") + cp))

##---------------------------------------------------------------------------##
trend <- readRDS('C:/Users/jaimem/Dropbox/AbaloneData/SeasonalPhysiologySites_TemperatureTrends_1981-2023.rds')


# Create vector of plot colours for sites to match metabolomics analysis
plot_cols <- c('trend_gar' = 'red', 
               'trend_sey' = 'purple',
               'trend_sis' = 'orange', 
               'trend_thu' = 'yellow3',
               'trend_act' = 'green3', 
               'trend_mol' = 'blue')

plot_labs <- c('trend_gar' = 'GAR', 
               'trend_sey' = 'SEY',
               'trend_sis' = 'SIS', 
               'trend_thu' = 'THU',
               'trend_act' = 'ACT', 
               'trend_mol' = 'MOL')

physio_trend_plot <- autoplot(trend, facets = F)+
 # labs(x = 'Year', y = 'Degrees')+
 theme_bw()+
 # scale_y_continuous(breaks = seq(13, 16, 0.5))+
 theme_bw()+
 xlab('Year')+
 ylab(expression("Temperature " ( degree~C)))+
 scale_colour_manual(breaks = c('trend_gar','trend_sey', 'trend_sis', 'trend_thu', 'trend_act', 'trend_mol'),
                     name = '', values = plot_cols, labels = plot_labs)+
 theme(legend.title = element_blank(),
       legend.position = c(0.1, 0.85),
       # legend.justification = c(0, 1),
       legend.background = element_rect(fill = "white", color = "black", linewidth = 0.25))+
 # geom_vline(xintercept = 2022, linetype = 'dashed', colour = 'red', linewidth = 0.5)+
 geom_point(data = cp_summary, aes(x = cp_dec_date, y = C), colour = plot_colour)
 # geom_vline(data = cp_summary, aes(xintercept = cp_dec_date), color = plot_colour, 
 #            linewidth = 0.5, linetype = 'dashed', position = position_jitter(w = 0.02, h = 0)) 

ggsave(filename = paste('C:/cloudstor/R_Stuff/WEI/Results/PhysiologySeasonalSiteTemperature_trend', '.pdf', sep = ''), 
       plot = physio_trend_plot, units = 'mm', width = 190, height = 200)

ggsave(filename = paste('C:/cloudstor/R_Stuff/WEI/Results/PhysiologySeasonalSiteTemperature_trend', '.png', sep = ''), 
       plot = physio_trend_plot, units = 'mm', width = 190, height = 200)
