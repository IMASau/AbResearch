arm.sl <- readRDS('C:/CloudStor/R_Stuff/FIS/arms.sl.RDS')

# extract Betsey Island tagging data for 2019 and clean data where recaptures have been denoted in rock.plate rather than
# tag.recapture column, add columns to seperate tag and recapture data

df.2 <- arm.sl %>% 
 filter(site == 'BET' & !is.na(tag.id) & sampyear == 2019) %>% 
 mutate(tag.recep = if_else(is.na(rock.plate), tag.recep, 'R'),
        rock.plate = NA,
        tagdate = if_else(tag.recep == 'T', survdate, as.POSIXct(NA)),
        recapdate = if_else(tag.recep == 'R', survdate, as.POSIXct(NA)),
        tagstring = if_else(tag.recep == 'T', string, NA_character_),
        tagplate = if_else(tag.recep == 'T', plate, NA_character_),
        recapstring = if_else(tag.recep == 'R', string, NA_character_),
        recapplate = if_else(tag.recep == 'R', plate, NA_character_)) %>% 
 spread(tag.recep, sllength) %>% 
 group_by(tag.id) %>% 
 select(c(tag.id, tagdate, tagstring, tagplate, recapdate, recapstring, recapplate, T, R)) %>% 
 as.data.frame()

# seperate data into tagging and recapture
df.3 <- df.2 %>% 
 filter(is.na(R))

df.4 <- df.2 %>% 
 filter(!is.na(R))

# rejoin data using tag.id
df.5 <- left_join(df.3, df.4, by = c("tag.id")) %>% 
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

# plot shell growth between tagging and recapture
df.5 %>% 
        filter(!is.na(growthincrement) & tag.id != 'A339') %>% 
        ggplot(aes(x = tag.id, y = growthincrement)) +
        geom_bar(stat = 'identity') +
        theme_bw() +
        xlab('TagID') +
        ylab(bquote('Growth (mm)'))

# plot shell growth for one year based on recapture data from period above
df.5 %>% 
        filter(!is.na(growth.yr) & tag.id != 'A339') %>% 
        ggplot(aes(x = tag.id, y = growth.yr)) +
        geom_bar(stat = 'identity') +
        theme_bw() +
        xlab('TagID') +
        ylab(bquote('Growth.yr'^-1*' (mm)'))
             