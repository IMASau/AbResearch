

sf.hex <- st_read("C:/Users/jaimem/Dropbox/AbaloneData/SpatialLayers/Hex1Ha.gpkg")
colnames(sf.hex) <- tolower(colnames(sf.hex))
sf.hex$zone <- str_trim(sf.hex$zone)

sf.hex.east <- filter(sf.hex, zone == "E")

sf.hex.east.closed <- filter(sf.hex, blockno == 16)

## Transform from GDA94 to GDA2020
GDA2020 <- st_crs(7855)
sf.hex.east.closed <- st_transform(sf.hex.east.closed, GDA2020)

# Import final dataframes 
time.swim.dat.final <-
 readRDS(paste(samp.year.folder, '/time.swim.dat.final.RDS', sep = ''))

time.swim.dat.df.final <-
 readRDS(paste(samp.year.folder, '/time.swim.dat.df.final.RDS', sep = ''))

# Standardise counts for 10 minute swim (i.e. some swims marginally shorter or longer duration)
std.ts.dat <- time.swim.dat.final %>% 
 mutate(sizeclass_freq_10 = round((sizeclass_freq / time.elapsed) * 10))

ts.count.sum <- std.ts.dat %>% 
 filter(!subblockno %in% c('28B', '28C')) %>%
 group_by(blockno, site, sampyear, legal.size) %>% 
 summarise(ab.n = sum(sizeclass_freq_10)) %>%  
 group_by(blockno, site, sampyear, legal.size) %>% 
 group_by(site)

df.1 <- std.ts.dat %>% 
 select(site, sampdate, sampyear, actual.geom) %>% 
 filter(sampdate != as.Date('2021-10-08')) %>% 
 distinct()

df.2 <- left_join(ts.count.sum, df.1) %>% 
 filter(legal.size == '<140 mm' &
         blockno == 16 &
         sampdate != as.Date('2022-08-04')) %>% 
 st_as_sf()

ggplot(data = st_geometry(sf.hex.east.closed)) +
 geom_sf()+
 geom_sf(data = st_geometry(df.2), shape = 5, size = 0.8, colour = 'red')

library(gstat)

g <- gstat(formula = ab.n ~ 1, data = df.2)

z <- predict(g, sf.hex.east.closed)


