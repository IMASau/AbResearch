library(gstat) # Use gstat's idw routine
library(sp)    # Used for the spsample function

z <- gzcon(url("https://github.com/mgimond/Spatial/raw/main/Data/precip.rds"))
P <- readRDS(z)
z <- gzcon(url("https://github.com/mgimond/Spatial/raw/main/Data/texas.rds"))
W <- readRDS(z)


# Create an empty grid where n is the total number of cells
grd              <- as.data.frame(spsample(P, "regular", n=50000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

# Add P's projection information to the empty grid
proj4string(P) <- proj4string(P) # Temp fix until new proj env is adopted
proj4string(grd) <- proj4string(P)

# Interpolate the grid cells using a power value of 2 (idp=2.0)
P.idw <- gstat::idw(Precip_in ~ 1, P, newdata=grd, idp=2.0)

# Convert to raster object then clip to Texas
r       <- raster::raster(P.idw)
r.m     <- raster::mask(r, W)

# Plot
tm_shape(r.m) + 
 tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
           title="Predicted precipitation \n(in inches)") + 
 tm_shape(P) + tm_dots(size=0.2) +
 tm_legend(legend.outside=TRUE)


# load 1 ha hex cell layer
sf.hex <- st_read("C:/Users/jaimem/Dropbox/AbaloneData/SpatialLayers/Hex1Ha.gpkg")

# filter hexcell to closed blocks
sf.hex.closed <- sf.hex %>% 
 filter(BlockNo %in% c(16, 22, 23, 24, 27, 28))

grid.block <- sf.hex.closed %>% 
 filter(BlockNo == 16) %>% 
 dplyr::rename(geometry = geom) %>% 
 select(geometry)

# hex.block.vert <- spatialEco::extract.vertices(sf.hex.block, as.sp = T, rm.duplicates = T)
# hex.block.grid <- hex.block.vert

gridded(hex.block.grid) <- TRUE  # Create SpatialPixel object
fullgrid(hex.block.grid) <- TRUE  # Create SpatialGrid object



# Add P's projection information to the empty grid
proj4string(df.1) <- proj4string(df.1) # Temp fix until new proj env is adopted
proj4string(hex.block.grid) <- proj4string(df.1)

df.idw <- gstat::idw(mean.ab.n ~ 1, df.1, newdata = hex.block.grid, idp = 2.0)
df.rast <- raster::raster(df.idw)

tm_shape(df.rast)+ 
 tm_raster(n = 10, palette = "RdBu", drop.levels = T)+ 
 tm_shape(df.1) + tm_dots(size = 0.2)+
 tm_legend(legend.outside = TRUE)

##---------------------------------------------------------------------------##
# https://cran.r-project.org/web/packages/SpatialKDE/vignettes/SpatialKDE.html
# https://semba-blog.netlify.app/06/30/2020/kernel-smoothin-of-spatial-data/
library(SpatialKDE)
library(sp)
library(sf)
library(dplyr)
library(tmap)

# select timed swim data
df.1 <- time.swim.count.site.loc %>% 
 dplyr::rename(geometry = actual.geom) %>%
 mutate(sampyear = year(sampdate)) %>% 
 filter(!st_is_empty(.) &
         blockno == 22 &
         sampyear == 2022 &
         legal.size == '<140 mm') %>% 
 as.data.frame()

# urchin data
df.1 <- meta.dat.start %>% 
 filter(!st_is_empty(.) &
         blockno == 24 &
         sampyear == 2020 &
         sampperiod == 'start' &
         !is.na(percent.urchins)) %>% 
 as.data.frame()

# extract timed swim count data as vector
weight.ab.n <- df.1 %>% 
 select(mean.ab.n) %>% 
 pull()

#extract urchin data
weight.ab.n <- df.1 %>% 
 select(percent.urchins) %>% 
 pull()

# extract timed swim site point geometry data
points.sites <- df.1 %>% 
 select(geometry) %>% 
 st_as_sf() %>% 
 st_set_crs(GDA2020)

# set band width and cell size of resulting grid
cell_size <- 400
band_width <- 1000

# create hexagonal grid covering sites

grid_block <- points.sites %>%
 create_grid_hexagonal(cell_size = cell_size, side_offset = band_width, only_inside = T) %>%
 st_set_crs(GDA2020)

tm_shape(grid_block)+
 tm_polygons()

sf.subblock.map.crop <- sf.subblock.map %>%
 filter(blockno == 24 &
         !subblockno %in% c('28B', '28C'))  %>% 
 st_as_sf()

df.2 <- sf::st_intersection(grid_block, sf.subblock.map.crop)

tm_shape(df.2)+
 tm_polygons()

kde <- points.sites %>% 
 kde(band_width = band_width, kernel = "quartic", grid = df.2, weights = weight.ab.n) %>% 
 filter(kde_value > 0.1)

tm_shape(sf.subblock.map.crop) +
 tm_polygons()+
tm_shape(kde) +
 tm_polygons(col = "kde_value", palette = "viridis", title = "KDE Estimate")
 # tm_shape(points.sites) +
 # tm_bubbles(size = 0.1, col = "red")

raster_block <- points.sites %>% 
 create_raster(cell_size = cell_size, side_offset = band_width)

kde.2 <- points.sites %>%
 kde(band_width = band_width, kernel = "triweight", grid = raster_block)

## convert sf into stars object
block.raster <- kde %>%
 stars::st_rasterize(nx = 100, ny = 100)

## convert to data table with cubelyr 
block.tb <- block.raster %>% 
 cubelyr::as.tbl_cube()

## oobtain individual compponent
lon <- block.tb$dims$x
lat <- block.tb$dims$y
obs <- block.tb$mets$kde_value

class(lon); class(lat); class(obs)

blocks <- expand.grid(lon,lat) %>% 
 bind_cols(expand.grid(obs))  %>% 
 dplyr::rename(lon = 1, lat = 2)

blocks %>% glimpse()

plot.2 <- ggplot() +
 metR::geom_contour_fill(data = blocks, aes(x = lon, y = lat, z = nbObs), bins = 120, na.fill = TRUE)+
 geom_sf(data = tz.ke, fill = "grey80") +
 coord_sf(xlim = c(38.6, 39.8), ylim = c(-6,-4.7))+
 scale_fill_gradientn(colours = mycolor, breaks = seq(100,1400,120), name = "Number\nof PFZs",
                      guide = guide_colorbar(reverse = TRUE, nbin = 11, raster = TRUE, barheight = unit(6,"cm")))

kde %>% plot()

raster_meuse <- meuse %>%
 create_raster(cell_size = cell_size, side_offset = band_width)

kde <- meuse %>%
 kde(band_width = band_width, kernel = "triweight", grid = raster_meuse)

tm_shape(kde) +
 tm_raster(palette = "viridis", title = "KDE Estimate") +
 tm_shape(points.sites) +
 tm_bubbles(size = 0.1, col = "red") +
 tm_layout(legend.outside = TRUE)

