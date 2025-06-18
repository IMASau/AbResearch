library(remotes)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

# read raw data
logger.a <- read.csv("C:/Users/jaimem/UTAS Research Dropbox/Jaime McAllister/DiveFisheries/Projects/AIRF_2020_43/2020_43_Data/HeatStress_Data/1_abalone_physiology_Control_13degrees.csv",
                           header = T)

logger.a <- logger.a %>%
 dplyr::rename(date.time = 2, temperature = 3) %>%  
 mutate(date.time = as.POSIXct(date.time, format="%y-%m-%d %H:%M:%S", tz = 'Australia/Tasmania'),
        treatment = 'control') %>% 
 select(date.time, temperature, treatment)

logger.b <- read.csv("C:/Users/jaimem/UTAS Research Dropbox/Jaime McAllister/DiveFisheries/Projects/AIRF_2020_43/2020_43_Data/HeatStress_Data/1_abalone_physiology_TreatmentA_16degrees.csv",
                     header = T)

logger.b <- logger.b %>%
 dplyr::rename(date.time = 2, temperature = 3) %>%  
 mutate(date.time = as.POSIXct(date.time, format="%y-%m-%d %H:%M:%S", tz = 'Australia/Tasmania'),
        treatment = 'medium') %>% 
 select(date.time, temperature, treatment)
        
logger.c <- read.csv("C:/Users/jaimem/UTAS Research Dropbox/Jaime McAllister/DiveFisheries/Projects/AIRF_2020_43/2020_43_Data/HeatStress_Data/1_abalone_physiology_TreatmentB_19degrees.csv",
                     header = T)

logger.c <- logger.c %>%
 dplyr::rename(date.time = 2, temperature = 3) %>%  
 mutate(date.time = as.POSIXct(date.time, format="%y-%m-%d %H:%M:%S", tz = 'Australia/Tasmania'),
        treatment = 'high') %>% 
 select(date.time, temperature, treatment)

logger.dat <- bind_rows(logger.a, logger.b, logger.c) %>% 
 mutate(treatment = factor(treatment, levels = c('high', 'medium', 'control')))

logger_plot <- logger.dat %>% 
 filter(date.time > ymd_hms("2022-12-12 12:40:00") & date.time < ymd_hms("2022-12-19 05:00:00") & treatment == 'high'|
         date.time > ymd_hms("2022-12-12 12:40:00") & date.time < ymd_hms("2022-12-19 14:00:00") & treatment %in% c('control', 'medium')) %>%  
 ggplot(aes(x = date.time, y = temperature))+
 geom_line(aes(color = treatment))+
 geom_vline(aes(xintercept = ymd_hms("2022-12-14 00:00:00")), colour = 'red', linetype = 'dashed')+
 geom_vline(aes(xintercept = ymd_hms("2022-12-16 00:00:00")), colour = 'blue', linetype = 'dashed')+
 theme_bw()+
 xlab('Date.Time')+
 ylab('Temperature (\u00B0C)')+
 scale_color_discrete(name = "Treatment", labels = c("High", "Medium", "Control"))+
 scale_y_continuous(breaks = seq(12,20,1))+
 # geom_hline(aes(yintercept = 13.8), linetype = 'dashed', colour = '#F8766D')+
 # geom_hline(aes(yintercept = 15.4), linetype = 'dashed', colour = '#00BA38')+
 # geom_hline(aes(yintercept = 18.2), linetype = 'dashed', colour = '#619CFF')
 geom_segment(aes(x = ymd_hms("2022-12-16 00:00:00"), xend = ymd_hms("2022-12-19 05:00:00"), y = 13.8, yend = 13.8), linetype = 'dashed', colour = '#619CFF')+
 geom_segment(aes(x = ymd_hms("2022-12-16 00:00:00"), xend = ymd_hms("2022-12-19 05:00:00"), y = 15.4, yend = 15.4), linetype = 'dashed', colour = '#00BA38')+
 geom_segment(aes(x = ymd_hms("2022-12-16 00:00:00"), xend = ymd_hms("2022-12-19 05:00:00"), y = 18.2, yend = 18.2), linetype = 'dashed', colour = '#F8766D')+
 geom_text(label = '13.8 \u00B0C', x = ymd_hms("2022-12-19 15:00:00"), y = 13.8, colour = '#619CFF', size = 3, stat = 'identity')+
 geom_text(label = '15.4 \u00B0C', x = ymd_hms("2022-12-19 15:00:00"), y = 15.4, colour = '#00BA38', size = 3, stat = 'identity')+
 geom_text(label = '18.2 \u00B0C', x = ymd_hms("2022-12-19 15:00:00"), y = 18.2, colour = '#F8766D', size = 3, stat = 'identity')+
 geom_text(label = 'Aclimatisation', x = ymd_hms("2022-12-13 00:00:00"), y = 12.5, size = 3, stat = 'identity')+
 geom_text(label = 'Manipulation', x = ymd_hms("2022-12-15 00:00:00"), y = 12.5, size = 3, stat = 'identity')+
 geom_text(label = 'Heat Stress', x = ymd_hms("2022-12-18 00:00:00"), y = 12.5, size = 3, stat = 'identity')+
 theme(legend.position = c(0, 1),
       legend.justification = c(0, 1),
       legend.background = element_rect(fill = "white", color = "black", linewidth = 0.2))


logger.dat %>% 
 filter(date.time > ymd_hms("2022-12-16 00:00:00") & date.time < ymd_hms("2022-12-19 05:00:00")) %>% 
 group_by(treatment) %>% 
 summarise(average.temp = mean(temperature),
           max.temp = max(temperature),
           min.temp = min(temperature))

ggsave(filename = paste('C:/Users/jaimem/OneDrive - University of Tasmania/Documents/AB_proteomics/HeatStressExperiment_LoggerTemperature', '.pdf', sep = ''), 
       plot = logger_plot, units = 'mm', width = 190, height = 200)

ggsave(filename = paste('C:/Users/jaimem/OneDrive - University of Tasmania/Documents/AB_proteomics/HeatStressExperiment_LoggerTemperature', '.png', sep = ''), 
       plot = logger_plot, units = 'mm', width = 190, height = 200)

##---------------------------------------------------------------------------##
# Formulated feed experiment temperature data

# read raw data
logger_dat <- read.csv("C:/Users/jaimem/UTAS Research Dropbox/Jaime McAllister/DiveFisheries/Projects/AIRF_2020_43/2020_43_Data/HeatStress_Data/AbStressTest_Capture-Harvest_Temperature_2021-12-10_clean.csv",
                     header = T)

logger_dat <- logger_dat %>%
 dplyr::rename(temperature = 6) %>%   
 mutate(date_time = as.POSIXct(date_time, format="%d-%m-%y %H:%M:%S", tz = 'Australia/Tasmania')) %>% 
 select(date_time, temperature)

logger_dat %>% 
 filter(date_time > ymd_hms("2021-10-28 11:18:00") & date_time < ymd_hms("2021-12-08 12:00:00")) %>%    
 ggplot(aes(x = date_time, y = temperature))+
 geom_line()

logger_dat %>% 
 filter(date_time > ymd_hms("2021-10-28 11:18:00") & date_time < ymd_hms("2021-12-08 12:00:00")) %>%
 summarise(across(where(is.numeric), .fns = 
                   list(Median = median,
                        Mean = mean,
                        n = sum,
                        SD = sd,
                        SE = ~sd(.)/sqrt(n()),
                        Min = min,
                        Max = max,
                        q25 = ~quantile(., 0.25), 
                        q75 = ~quantile(., 0.75)
                   ))) %>% 
 pivot_longer(everything(), names_sep = "_", names_to = c( "variable", ".value"))

##---------------------------------------------------------------------------##
# Live transport experiment

live_temp_dat <- read.csv("C:/Users/jaimem/UTAS Research Dropbox/Jaime McAllister/DiveFisheries/Projects/AIRF_2020_43/2020_43_Data/HeatStress_Data/AbStressTest_TemperatureRH_2021-12-08_clean.csv",
                       header = T)

live_temp_dat_clean <- live_temp_dat %>%
 dplyr::rename(temperature = 3,
               humidity = 4,
               date_time = 2) %>% 
 mutate(date_time = as.POSIXct(date_time, format="%m-%d-%Y %H:%M:%S", tz = 'Australia/Tasmania')) %>% 
 select(date_time, temperature, humidity)

trans_exp_dat <- live_temp_dat_clean %>% 
 filter(date_time > ymd_hms("2021-12-07 12:15:00", tz = 'Australia/Tasmania') & 
         date_time < ymd_hms("2021-12-08 12:15:00", tz = 'Australia/Tasmania'))

min_date <- min(trans_exp_dat$date_time)
transport_start_date <- as.numeric(difftime((ymd_hms("2021-12-07 13:30:00", tz = 'Australia/Tasmania')), min_date, units = 'hours'))
transport_end_date <- as.numeric(difftime((ymd_hms("2021-12-08 11:30:00", tz = 'Australia/Tasmania')), min_date, units = 'hours'))
packing_date_lab <- as.numeric(difftime((ymd_hms("2021-12-07 12:15:00", tz = 'Australia/Tasmania')), min_date, units = 'hours'))
transport_date_lab <- as.numeric(difftime((ymd_hms("2021-12-08 00:00:00", tz = 'Australia/Tasmania')), min_date, units = 'hours'))
processing_date_lab <- as.numeric(difftime((ymd_hms("2021-12-08 12:25:00", tz = 'Australia/Tasmania')), min_date, units = 'hours'))  

trans_exp_dat <- trans_exp_dat %>% 
 mutate(hours = as.numeric(difftime(date_time, min_date, units = 'hours')))

trans_exp_plot <- trans_exp_dat %>% 
 ggplot(aes(x = hours))+
 geom_line(aes(y = temperature), colour = 'red')+
 geom_line(aes(y = humidity / 6), colour = 'blue')+
 scale_y_continuous(name = "Temperature (\u00B0C)", sec.axis = sec_axis(~.*6, name = "Humidity (rH%)"))+
 geom_vline(aes(xintercept = transport_start_date), colour = 'black', linetype = 'dashed')+
 geom_vline(aes(xintercept = transport_end_date), colour = 'black', linetype = 'dashed')+
 theme_bw()+
 xlab('Hours')+
 # ylab('Temperature (\u00B0C)')+
 geom_text(label = 'Packing', x = packing_date_lab, y = 12.5, size = 3, stat = 'identity')+
 geom_text(label = 'Live Transport', x = transport_date_lab, y = 12.5, size = 3, stat = 'identity')+
 geom_text(label = 'Processing', x = processing_date_lab, y = 12.5, size = 3, stat = 'identity', angle = 90)

ggsave(filename = paste('C:/Users/jaimem/UTAS Research Dropbox/Jaime McAllister/DiveFisheries/Projects/AIRF_2020_43/2020_43_Figures/Transport/TransportExperiment_LoggerTemperature', '.pdf', sep = ''), 
       plot = trans_exp_plot, units = 'mm', width = 190, height = 150)

ggsave(filename = paste('C:/Users/jaimem/UTAS Research Dropbox/Jaime McAllister/DiveFisheries/Projects/AIRF_2020_43/2020_43_Figures/Transport/TransportExperiment_LoggerTemperature', '.png', sep = ''), 
       plot = trans_exp_plot, units = 'mm', width = 190, height = 150)

trans_exp_dat %>% 
 filter(date_time > ymd_hms("2021-12-07 13:30:00", tz = 'Australia/Tasmania') & 
         date_time < ymd_hms("2021-12-08 11:30:00", tz = 'Australia/Tasmania')) %>% 
 summarise(across(where(is.numeric), .fns = 
                   list(Median = median,
                        Mean = mean,
                        n = sum,
                        SD = sd,
                        SE = ~sd(.)/sqrt(n()),
                        Min = min,
                        Max = max,
                        q25 = ~quantile(., 0.25), 
                        q75 = ~quantile(., 0.75)
                   ))) %>% 
 pivot_longer(everything(), names_sep = "_", names_to = c( "variable", ".value"))
