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
        treatment = 'medium') %>% 
 select(date.time, temperature, treatment)
        
logger.c <- read.csv("C:/Users/jaimem/OneDrive - University of Tasmania/Documents/AB_proteomics/1_abalone_physiology_TreatmentB_19degrees.csv",
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
