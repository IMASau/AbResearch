suppressPackageStartupMessages({
        library(dplyr)
        library(ggplot2)
        library(scales)
        library(tidyr)
        library(gdata)
        library(openxlsx)
        library(lubridate)
        library(reshape)
        library(gridExtra)
        library(ggpubr)
        library(readxl)
        library(tibble)
        library(data.table)
        library(janitor)
        library(anytime)
        library(stringr)
})

time.swim.dat <- read.xlsx("C:/Users/jaimem/Documents/Abalone_research/AB_TimedSwimFIS/FIS_TimedSwim_RawData_2020.xlsx",
                       detectDates = T)

# convert Excel time
time.swim.dat$starttime <- convertToDateTime(time.swim.dat$starttime, origin = "1970-01-01", tz = "Australia/HOBART")
time.swim.dat$firstabtime <- convertToDateTime(time.swim.dat$firstabtime, origin = "1970-01-01", tz = "Australia/HOBART")
time.swim.dat$finishtime <- convertToDateTime(time.swim.dat$finishtime, origin = "1970-01-01", tz = "Australia/HOBART")

time.swim.dat <- time.swim.dat %>% 
        mutate(starttime = strftime(starttime, format="%H:%M:%S"),
               firstabtime = strftime(firstabtime, format="%H:%M:%S"),
               finishtime = strftime(finishtime, format="%H:%M:%S"))

time.swim.dat$starttime <- as.POSIXct(paste(time.swim.dat$sampdate, time.swim.dat$starttime), format = "%Y-%m-%d %H:%M:%S")
time.swim.dat$firstabtime <- as.POSIXct(paste(time.swim.dat$sampdate, time.swim.dat$firstabtime), format = "%Y-%m-%d %H:%M:%S")
time.swim.dat$finishtime <- as.POSIXct(paste(time.swim.dat$sampdate, time.swim.dat$finishtime), format = "%Y-%m-%d %H:%M:%S")

# calculate elapsed dive time
time.swim.dat <- time.swim.dat %>% 
        mutate(time.elapsed = ifelse(is.na(firstabtime), finishtime - starttime, finishtime - firstabtime))

# add blockno including randomly sampled sites in block 22
time.swim.dat <- time.swim.dat %>% 
        mutate(site2 = site) %>% 
        separate(col = site2, into = c('ab', 'blockno'), sep = '-') %>% 
                select(-ab) %>% 
        mutate(blockno = ifelse(blockno %in% c(72, 74, 'LEG'), 22, blockno))

# quick summary of sites sampled by block
time.swim.dat %>% 
        group_by(blockno) %>% 
        summarise(sites.sampled = n_distinct(site)) %>%  
        adorn_totals("row") %>% 
        as.data.frame()

# arrange sizeclasses in order
sizeclasses <- c("0-20", "20-40", "40-60", "60-80", "80-100", "100-120", "120-140", "140-160", "160-180", "180-200", "200-220")

# determine legal and sub-legal abalone

time.swim.dat <- time.swim.dat %>% 
        mutate(legal.size = ifelse(sizeclass %in% c("0-20", "20-40", "40-60", "60-80", "80-100", "100-120", "120-140"), '<140 mm', '>140 mm'))

# summary table of counts by size class and block
time.swim.dat %>%
        mutate(sizeclass = factor(sizeclass, levels = sizeclasses)) %>% 
        group_by(sizeclass, blockno) %>%
        summarise(n = sum(sizeclass_freq)) %>%
        spread(blockno, n) %>% 
        as.data.frame()

# summary table of abalone/minute by block and size class
time.swim.dat %>% 
        group_by(blockno, site, diver) %>% 
        summarise(ab.measured = sum(sizeclass_freq),
                  swim.time = max(time.elapsed)) %>% 
        mutate(ab.cpue = ab.measured / swim.time) %>% 
        group_by(blockno) %>% 
        summarise(sites = n_distinct(site),
                  ab.min = mean(ab.cpue),) %>% 
        as.data.frame()

time.swim.dat %>% 
        group_by(blockno, site, diver, legal.size) %>% 
        summarise(ab.measured = sum(sizeclass_freq),
                  swim.time = max(time.elapsed)) %>%  
        mutate(ab.cpue = ab.measured / swim.time) %>%  
        group_by(blockno, legal.size) %>% 
        summarise(sites = n_distinct(site),
                  ab.min = round(mean(ab.cpue), digits = 2)) %>% 
        spread(legal.size, ab.min) %>% 
        as.data.frame()

time.swim.dat %>% 
        group_by(blockno, site, diver) %>% 
        summarise(ab.measured = sum(sizeclass_freq),
                  swim.time = max(time.elapsed)) %>% 
        mutate(ab.cpue = ab.measured / swim.time) %>% 
        group_by(diver) %>% 
        summarise(ab.min = mean(ab.cpue))

## Diver A vs Diver B ####

diver.a <- 'LT'
diver.b <- 'BD'

df.1 <- time.swim.dat %>% 
        mutate(sizeclass = factor(sizeclass, levels = sizeclasses)) %>%
        filter(diver %in% c(diver.a)) %>% 
        select(c(site, sizeclass, sizeclass_freq)) %>%
        dplyr::rename('DIVER A' = sizeclass_freq)

df.2 <- time.swim.dat %>% 
        mutate(sizeclass = factor(sizeclass, levels = sizeclasses)) %>%
        filter(diver %in% c(diver.b)) %>% 
        select(c(site, sizeclass, sizeclass_freq)) %>%
        dplyr::rename('DIVER B' = sizeclass_freq)

df.3 <- left_join(df.1, df.2)

LT_BD <- df.3 %>% 
        mutate(sizeclass = factor(sizeclass, levels = sizeclasses)) %>%
        ggplot(aes(`DIVER A`, `DIVER B`))+
        geom_point(aes(colour = factor(sizeclass)))+
        geom_smooth(method='lm')+
        coord_cartesian(xlim = c(0, 40), ylim = c(0, 40))+
        xlab(paste(diver.a, 'abalone_count'))+
        ylab(paste(diver.b, 'abalone_count'))+
        labs(colour = 'Size class (mm)')+
        theme_bw()

grid.arrange(LT_BD, NW_SI, SL_GP, ncol = 1)

## Length frequency plot by block ####

lf.plot<- time.swim.dat %>%
        mutate(sizeclass = factor(sizeclass, levels = sizeclasses)) %>% 
        group_by(sizeclass, blockno) %>%
        summarise(n = sum(sizeclass_freq)) %>%
        # spread(blockno, n) %>% 
        as.data.frame() %>% 
        ggplot(aes(x = sizeclass, y = n)) +
                       geom_bar(stat = 'identity') +
        facet_grid(blockno ~ .)+
        theme_bw()

setwd('C:/CloudStor/R_Stuff/FIS/FIS_2020')
ggsave(filename = paste('TIME_SWIM_LF', '.pdf', sep = ''),
       plot = lf.plot, units = 'mm', width = 190, height = 250)
ggsave(filename = paste('TIME_SWIM_LF', '.png', sep = ''),
       plot = lf.plot, units = 'mm', width = 190, height = 250)

## Length frequency plot by block (mid-points) ####

time.swim.mid.dat <- separate(time.swim.dat, sizeclass, c('minsize', 'maxsize'), sep = '-', convert = TRUE) %>% 
        mutate(midsize = (minsize + maxsize)/2)

time.swim.mid.dat %>% ggplot(aes(x = midsize, y = sizeclass_freq)) +
        geom_bar(stat = 'identity', width = 20) +
        geom_vline(xintercept = 138, linetype = 'dashed', colour = 'red', size = 0.5) +
        theme_bw() +
        facet_grid(blockno ~ .) +
        xlab("Shell Length (mm)")

## re-label sites with corrected site names (i.e. site numbers in sequence from south to north)

fis.sites.final <- read.xlsx("C:/Users/jaimem/Documents/Abalone_research/AB_TimedSwimFIS/timed swim sites_final.xlsx",
                  detectDates = T)

fis.site.cpue <- read.xlsx("C:/Users/jaimem/Documents/Abalone_research/AB_TimedSwimFIS/timed swim sites_CPUE_original.xlsx",
                               detectDates = T)

fis.site.sam <- read.xlsx("C:/Users/jaimem/Documents/Abalone_research/AB_TimedSwimFIS/timed swim sites_SAM_original.xlsx",
                               detectDates = T)

fis.site.sam <- fis.site.sam %>% 
        mutate(str_trim(name)) %>% 
        # mutate(name2 = paste(name,'-B', sep = '')) %>% 
        dplyr::rename('lat' = Latitude,
                      'long' = Longitude) %>% 
        # mutate(site = name) %>% 
        select(c(name, lat, long))


fis.sites.join <- left_join(fis.sites.final, fis.site.cpue, by = c('lat', 'long')) %>% 
        left_join(fis.site.sam, by = c('lat', 'long')) %>% 
        mutate(name.y = ifelse(is.na(name.y), name, name.y)) %>% 
        select(-c(desc.y, name, desc.x))

df.1 <- time.swim.dat %>% 
        distinct(site, .keep_all = T) %>% 
        select(c(site, sampdate))

df.2 <- left_join(fis.sites.join, df.1, by = c('name.y' = 'site')) %>% 
        dplyr::rename(c('site.new' = name.x,
                        'site.old' = name.y)) 
        

df.3 <- df.2 %>% 
        mutate(name = site.new,
               color = ifelse(is.na(sampdate), 'red', 'green'),
               symbol = 'circle',
               latitude = lat,
               longitude = long,
               desc = paste('oldsite', site.old, sep = '-')) %>% 
        select(c(name, desc, color, symbol, latitude, longitude))

write.csv(df.3,"C:/Users/jaimem/Documents/Abalone_research/AB_TimedSwimFIS/FIS_TIMEDSWIM_24082020.csv", row.names = FALSE)        
