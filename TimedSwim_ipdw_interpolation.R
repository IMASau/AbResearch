##---------------------------------------------------------------------------##
#https://cran.r-project.org/web/packages/ipdw/vignettes/ipdw2.html
#https://eriqande.github.io/2014/12/24/plotting-sst-with-ggplot.html

suppressPackageStartupMessages({
library(sf)
library(dplyr)
library(ipdw)
library(spatstat)
library(ggplot2)
library(lubridate)
})

# import tas coastal polygon data 
sf.tas.map <- st_read("C:/CloudStor/DiveFisheries/GIS/SpatialLayers/TasLand.gpkg")
sf.subblock.map <- st_read("C:/CloudStor/DiveFisheries/GIS/SpatialLayers/SubBlockMaps.gpkg")

# transform coordinates to GDA2020
sf.tas.map <- st_transform(sf.tas.map, st_crs(7855))
sf.subblock.map <- st_transform(sf.subblock.map, st_crs(7855))

# timed swim point data 
time.swim.count.site.loc <- readRDS(paste(samp.year.folder, '/time.swim.count.site.loc.RDS', sep = ''))
##---------------------------------------------------------------------------##
## Set sample year and file paths ####

# identify sampling year of interest
samp.year <- 2022

# identify associated sampling year folder path to save dataframes
samp.year.folder <- file.path('C:', 'CloudStor', 'DiveFisheries', 
                              'Abalone', 'FISdata',
                              paste('FIS_TimedSwimSurveys', samp.year, sep = ''))

# identify associated sampling year folder path to save plots
ts.plots.folder <- file.path('C:', 'CloudStor', 'DiveFisheries', 
                             'Abalone', 'Assessment', 'Figures', 'FIS',
                             paste('FIS_TimedSwimSurvey', samp.year, '_Plots', sep = ''))
##---------------------------------------------------------------------------##
## Create function to interpolate abundance by each block, year and size class ####

# Create parameter dataframe of block, year and size class combinations
blocknos <- c(16, 22, 23, 24, 27, 28)
sampyears <- seq(2020, 2022, 1)
legalsizes <- c('<140 mm', '>140 mm')

parameters_data <- expand.grid(blockno = blocknos,
                               sampyear = sampyears,
                               legal.size = legalsizes) %>% 
 as_tibble()

# Create function
ts_ipdw <- function(i){
 
 # extract variables to add to ipdw dataframe at end
 ts.pnts.blockno <- parameters_data[i, 'blockno'] %>% 
  pull()
 ts.pnts.sampyear <- parameters_data[i, 'sampyear'] %>% 
  pull()
 ts.pnts.legal.size <- parameters_data[i, 'legal.size'] %>% 
  pull()

# filter data for block, year and sizeclass 
ts.pnts <- time.swim.count.site.loc %>% 
 dplyr::rename(geom = actual.geom) %>%
 mutate(sampyear = year(sampdate)) %>% 
 filter(!st_is_empty(.) &
         blockno == ts.pnts.blockno &
         sampyear == ts.pnts.sampyear &
         legal.size == ts.pnts.legal.size) %>%
 mutate(id = row_number())

# transform coordinates to GDA2020
ts.pnts <-  ts.pnts %>% 
 select(c(id, mean.ab.n, geom)) %>%
 st_transform(st_crs(7855))

# crop subblockmap to block
ts.tas.subblockmap.crop <- sf.subblock.map %>%  
 filter(blockno == ts.pnts.blockno)

# crop coastal map to subblock
ts.tas.coast.crop <- st_crop(sf.tas.map, ts.tas.subblockmap.crop)

# add 500 m buffer around coastal map 
ts.tas.coast.crop.buffer <- ts.tas.coast.crop %>% 
 st_buffer(dist = units::set_units(0.5, km)) %>% 
 st_cast()

# convert buffer to single outer polygon of coastal buffer
ts.tas.coast.crop.buffer <- ts.tas.coast.crop.buffer %>% 
 summarise()

# re-crop outer coastal buffer to subblock extents
ts.tas.coast.crop.buffer <- st_crop(ts.tas.coast.crop.buffer, ts.tas.subblockmap.crop)

# create coastal buffer polygon for sea
buffer.coast <- ts.tas.coast.crop.buffer %>% 
 st_difference(ts.tas.coast.crop) %>% 
 st_cast()

# create bounding box extent of area
# https://statnmap.com/2020-07-31-buffer-area-for-nearest-neighbour/
buffer.extent <- as.polygons(ext(buffer.coast)) %>% 
 st_as_sf()

buffer.coast <- st_transform(buffer.coast, st_crs(7855))
buffer.extent <- buffer.extent %>% 
 st_set_crs(st_crs(buffer.coast))

# create polygon of land area
buffer.land <- buffer.extent %>% 
 st_difference(buffer.coast) %>% 
 st_cast()

# extract tas map polygon geometry
ts.pols <- buffer.land %>% 
 dplyr::select(geometry)

# create cost raster using tas coast line
costras <- costrasterGen(ts.pnts, ts.pols,
                         projstr = projection(ts.pols), resolution = 300)

# # adjust resolution of costraster
# costras.aggregate <- aggregate(costras, factor = 50)

# find average nearest neighbor
W <- owin(range(c(st_bbox(ts.pnts)["xmin"], st_bbox(ts.pnts)["xmax"])),
                       range(c(st_bbox(ts.pnts)["ymin"], st_bbox(ts.pnts)["ymax"])))
kat.pp <- ppp(st_coordinates(ts.pnts)[,1], st_coordinates(ts.pnts)[,2], window = W)
mean.neighdist <- mean(nndist(kat.pp))

# grid building
gridsize <- mean.neighdist * 2
grainscale.fac <- gridsize / res(costras)[1]
gridras <- aggregate(costras, fact = grainscale.fac)
gridpol <- rasterToPolygons(gridras)
gridpol$value <- row.names(gridpol)

gridpol <- gridpol %>% 
 st_as_sf()

gridpol <- st_transform(gridpol, st_crs(7855))

# spatial join
fulldataset.over <- sf::st_join(ts.pnts, gridpol)

# grid selection
set.seed(2)
gridlev <- unique(fulldataset.over$value)
for (i in seq_along(gridlev)) {
 activesub <- subset(fulldataset.over, fulldataset.over$value == gridlev[i])
 selectnum <- gdata::resample(seq_len(nrow(activesub)), 1)
 if (i == 1) {
  training <- activesub[selectnum, ]
 } else {
  training <- rbind(training, activesub[selectnum, ])
 }
}

validate <- fulldataset.over[!(row.names(fulldataset.over) %in%
                                            row.names(training)), ]

paramlist <- c("mean.ab.n")

final.ipdw <- ipdw(training, costras, range = mean.neighdist * 10, paramlist,
                   overlapped = TRUE)

# convert ipdw results to a dataframe for plotting in two steps

# first, to a SpatialPointsDataFrame
ipdw_pts <- rasterToPoints(final.ipdw, spatial = TRUE)

# then to a 'conventional' dataframe
ipdw_pts_df  <- data.frame(ipdw_pts)

# add variable names to dataframe for plotting
ipdw_pts_df <- ipdw_pts_df %>% 
 mutate(blockno = ts.pnts.blockno,
        sampyear = ts.pnts.sampyear,
        legal.size = ts.pnts.legal.size)

return(ipdw_pts_df)

}
##---------------------------------------------------------------------------##
## Run ipdw function and generate a dataframe with results from each combination ####
# of block, year and size class for ggplot facet plotting

# run function across all block, year and size class combinations
ts_ipdw_df <- lapply(1:nrow(parameters_data), ts_ipdw)

# combine list created above into dataframe
ts_ipdw_df_all <- bind_rows(ts_ipdw_df)

# save dataframe
saveRDS(ts_ipdw_df_all, paste(samp.year.folder, '/ts_ipdw_df_all.RDS', sep = ''))
##---------------------------------------------------------------------------##
## Plot interpolation comparing years ####

# load ipdw dataframe 
ts_ipdw_df_all <- readRDS(paste(samp.year.folder, '/ts_ipdw_df_all.RDS', sep = ''))

# load tas land and coastal polygon maps 
sf.tas.map <- st_read("C:/CloudStor/DiveFisheries/GIS/SpatialLayers/TasLand.gpkg")
sf.subblock.map <- st_read("C:/CloudStor/DiveFisheries/GIS/SpatialLayers/SubBlockMaps.gpkg")

# transform map coordinates to GDA2020
sf.tas.map <- st_transform(sf.tas.map, st_crs(7855))
sf.subblock.map <- st_transform(sf.subblock.map, st_crs(7855))

# create custom gradient colour scheme
mycolor = c("#7f007f", "#0000ff",  "#007fff", "#00ffff", "#00bf00", "#7fdf00",
                     "#ffff00", "#ff7f00", "#ff3f00", "#ff0000", "#bf0000")


# # select block and size
# plot.blockno <- 22
# plot.legal.size <- '<140 mm'

# Create parameter dataframe of block and size class combinations
blocknos <- c(16, 22, 23, 24, 27, 28)
legalsizes <- c('<140 mm', '>140 mm')

plot_parameters_data <- expand.grid(blockno = blocknos,
                               legal.size = legalsizes) %>% 
 as_tibble()

ts_ipdw_plot <- function(i){
 
 # extract variables to add to ipdw dataframe at end
 ts.blockno.plot <- plot_parameters_data[i, 'blockno'] %>% 
  pull()
 
 ts.legal.size.plot <- plot_parameters_data[i, 'legal.size'] %>% 
  pull()
 
 size.class <- ifelse(grepl('<140 mm',ts.legal.size.plot), 'SUB-LEGAL', 'LEGAL')
 
 # crop subblock map to block
 ts.tas.subblockmap.crop <- sf.subblock.map %>%  
  filter(blockno == ts.blockno.plot)
 
 # crop tas land map to subblock
 ts.tas.coast.crop <- st_crop(sf.tas.map, ts.tas.subblockmap.crop)

 # create plot
ipdw.plot <- ggplot() +
 geom_tile(data = ts_ipdw_df_all %>% 
            filter(legal.size == ts.legal.size.plot &
                    blockno == ts.blockno.plot), aes(x = x, y = y, fill = layer)) +
 coord_fixed(1.1)+
 scale_fill_gradientn(colours = mycolor, breaks = seq(0,160,20), limits = c(0, 160), labels = seq(0, 160, 20), name = "Abalone \ncount",
                      guide = guide_colorbar(reverse = TRUE, nbin = 10, raster = FALSE,barheight = unit(6,"cm")))+
 geom_sf(data = ts.tas.coast.crop)+
 theme_bw()+
 theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
       axis.title.x = element_blank(),
       axis.title.y = element_blank())+
 facet_wrap(~ sampyear, ncol = 3)

 # save plot
setwd(ts.plots.folder)
ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_IPDW_BlockNo_', 
                        ts.blockno.plot, '_', size.class, '.pdf', sep = ''),
       plot = ipdw.plot, units = 'mm', width = 250, height = 250)
ggsave(filename = paste('TimedSwimSurvey_', samp.year, '_IDPW_BlockNo_', 
                        ts.blockno.plot, '_', size.class, '.png', sep = ''),
       plot = ipdw.plot, units = 'mm', width = 250, height = 250)

}

# run function across all block, year and size class combinations
ts_ipdw_plots <- lapply(1:nrow(plot_parameters_data), ts_ipdw_plot)
       