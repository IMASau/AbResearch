library(remotes)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

# read raw data
logger.a <- read.csv("C:/Users/jaimem/OneDrive - University of Tasmania/Documents/AB_proteomics/1_abalone_physiology_Control_13degrees.csv",
                           header = T)

logger.a <- logger.a %>%
 dplyr::rename(date.time = 2, temperature = 3) %>%  
 mutate(date.time = as.POSIXct(date.time, format="%y-%m-%d %H:%M:%S", tz = 'Australia/Tasmania'),
        treatment = 'control') %>% 
 select(date.time, temperature, treatment)

logger.b <- read.csv("C:/Users/jaimem/OneDrive - University of Tasmania/Documents/AB_proteomics/1_abalone_physiology_TreatmentA_16degrees.csv",
                     header = T)

logger.b <- logger.b %>%
 dplyr::rename(date.time = 2, temperature = 3) %>%  
 mutate(date.time = as.POSIXct(date.time, format="%y-%m-%d %H:%M:%S", tz = 'Australia/Tasmania'),
        treatment = 'mid') %>% 
 select(date.time, temperature, treatment)
        
logger.c <- read.csv("C:/Users/jaimem/OneDrive - University of Tasmania/Documents/AB_proteomics/1_abalone_physiology_TreatmentB_19degrees.csv",
                     header = T)

logger.c <- logger.c %>%
 dplyr::rename(date.time = 2, temperature = 3) %>%  
 mutate(date.time = as.POSIXct(date.time, format="%y-%m-%d %H:%M:%S", tz = 'Australia/Tasmania'),
        treatment = 'high') %>% 
 select(date.time, temperature, treatment)

logger.dat <- bind_rows(logger.a, logger.b, logger.c) %>% 
 mutate(treatment = factor(treatment, levels = c('control', 'mid', 'high')))

logger.dat %>% 
 filter(date.time > ymd_hms("2022-12-12 12:40:00") & date.time < ymd_hms("2022-12-19 05:00:00") & treatment == 'high'|
         date.time > ymd_hms("2022-12-12 12:40:00") & date.time < ymd_hms("2022-12-19 14:00:00") & treatment %in% c('control', 'mid')) %>%  
 ggplot(aes(x = date.time, y = temperature))+
 geom_line(aes(color = treatment))+
 geom_vline(aes(xintercept = ymd_hms("2022-12-14 00:00:00")), colour = 'red', linetype = 'dashed')+
 geom_vline(aes(xintercept = ymd_hms("2022-12-16 00:00:00")), colour = 'blue', linetype = 'dashed')+
 theme_bw()+
 xlab('Date.Time')+
 ylab('Temperature (\u00B0C)')+
 scale_color_discrete(name = "Treatment", labels = c("Control", "Mid", "High"))+
 scale_y_continuous(breaks = seq(12,20,1))


logger.dat %>% 
 filter(date.time > ymd_hms("2022-12-16 00:00:00") & date.time < ymd_hms("2022-12-19 05:00:00")) %>% 
 group_by(treatment) %>% 
 summarise(average.temp = mean(temperature),
           max.temp = max(temperature),
           min.temp = min(temperature))
