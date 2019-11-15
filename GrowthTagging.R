library(dplyr)
library(tidyverse)
library(tidyr)
arms.sl <- readRDS('C:/CloudStor/R_Stuff/FIS/arms.sl.RDS')

# extract Betsey Island tagging data for 2019 and add columns to seperate tag and recapture data

tag.dat <- arms.sl %>% 
 filter(site == 'BET' & !is.na(tag.id) & sampyear == 2019) %>% 
 mutate(tagdate = if_else(tag.recap == 'T', survdate, as.POSIXct(NA)),
        recapdate = if_else(tag.recap == 'R', survdate, as.POSIXct(NA)),
        tagstring = if_else(tag.recap == 'T', string, NA_character_),
        tagplate = if_else(tag.recap == 'T', plate, NA_character_),
        recapstring = if_else(tag.recap == 'R', string, NA_character_),
        recapplate = if_else(tag.recap == 'R', plate, NA_character_)) %>% 
 spread(tag.recap, sllength) %>% 
 group_by(tag.id) %>% 
 select(c(tag.id, tag.col, tag.id.col, tagdate, tagstring, tagplate, recapdate, recapstring, recapplate, T, R)) %>% 
 as.data.frame()

# remove blank space from tag.id
tag.dat$tag.id <- gsub(' ', '', tag.dat$tag.id)
tag.dat$tag.col <- tolower(tag.dat$tag.col)
tag.dat$tag.id.col <- tolower(tag.dat$tag.id.col)

# seperate data into tagging and recapture
tag.dat.tag <- tag.dat %>% 
 filter(is.na(R))

tag.dat.recap <- tag.dat %>% 
 filter(!is.na(R))

# rejoin data using tag.id
tag.dat.df <- left_join(tag.dat.tag, tag.dat.recap, by = c("tag.id")) %>% 
        mutate(tagdate = tagdate.x,
               tagstring = tagstring.x,
               tagplate = tagplate.x,
               recapdate = recapdate.y,
               recapstring = recapstring.y,
               recapplate = recapplate.y,
               taglength = T.x,
               recaplength = R.y) %>% 
        select(c(tag.id, tagdate, tagstring, tagplate, recapdate, recapstring, recapplate, taglength, recaplength)) %>% 
        mutate(daysliberty = as.POSIXct(recapdate) - as.POSIXct(tagdate),
               growthincrement = recaplength - taglength,
               growth.yr = (growthincrement/as.numeric(daysliberty, units = 'days'))*365)

# plot mean growth.yr (i.e. multiple recapture dates)
growth.yr.plot <- tag.dat.df %>% 
        filter(!is.na(recapdate)) %>% 
        group_by(tag.id) %>% 
        summarise(mean.growth.yr = mean(growth.yr)) %>% 
        filter(tag.id != 'A339') %>% 
        ggplot(aes(x = tag.id, y = mean.growth.yr))+
        geom_bar(stat = 'identity')+
        theme_bw()+
        xlab('TagID')+
        ylab(bquote('Growth.yr'^-1*' (mm)'))

# save plot to file 
setwd('C:/CloudStor/R_Stuff/FIS')
ggsave(filename = paste('Gowth.yr_', 'BET', '.pdf', sep = ''), 
       plot = growth.yr.plot)
ggsave(filename = paste('Gowth.yr_', 'BET', '.wmf', sep = ''), 
       plot = growth.yr.plot)
ggsave(filename = paste('Gowth.yr_', 'BET', '.png', sep = ''), 
       plot = growth.yr.plot)

# plot mean growth.yr based on intial length at tagging
tag.dat.df.summ <- tag.dat.df %>% 
        filter(!is.na(recapdate)) %>% 
        group_by(tag.id) %>% 
        summarise(mean.growth.yr = mean(growth.yr))

growth.yr.size.plot <- left_join(tag.dat.df, tag.dat.df.summ, by = c('tag.id')) %>% 
        select(c(tag.id, taglength, tagdate, mean.growth.yr)) %>% 
        mutate(tagdate = as.factor(tagdate)) %>% 
        filter(!is.na(mean.growth.yr) & tag.id != 'A339') %>% 
        distinct() %>% 
        ggplot(aes(x = taglength, y = mean.growth.yr))+
        geom_point(aes(colour = tagdate), size = 5)+
        theme_bw()+
        # labs(x = expression(Length['t'] (mm)), y = bquote('Growth.yr'^-1*' (mm)'))+
        labs(x = expression(paste('Length'[t], ' (mm)')), y = bquote('Growth.yr'^-1*' (mm)'))+
        coord_cartesian(ylim = c(0, 40))+
        scale_color_discrete(name = 'Tagging date')

# save plot to file 
#setwd('C:/CloudStor/R_Stuff/FIS')
ggsave(filename = paste('Gowth.yr.size_', 'BET', '.pdf', sep = ''), 
       plot = growth.yr.size.plot)
ggsave(filename = paste('Gowth.yr.size_', 'BET', '.wmf', sep = ''), 
       plot = growth.yr.size.plot)
ggsave(filename = paste('Gowth.yr.size_', 'BET', '.png', sep = ''), 
       plot = growth.yr.size.plot)
 



             