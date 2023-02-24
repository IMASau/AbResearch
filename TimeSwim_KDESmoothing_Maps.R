# https://semba-blog.netlify.app/06/30/2020/kernel-smoothin-of-spatial-data/

library(sf)
library(btb)
library(tidyverse)

fronts.polygons <- st_read("C:/Users/jaimem/OneDrive - University of Tasmania/Documents/Abalone_r-scripts/pfz/pfz.shp", quiet = TRUE)

fronts.polygons.area <- fronts.polygons %>%
 mutate(area_km2 = as.numeric(st_area(geometry)/1000000),
        month = lubridate::month(date, label = TRUE))%>% filter(area_km2 < 200000)

# fronts.polygons.area <- fronts.polygons

fronts.points.wgs <- fronts.polygons.area %>%
 # st_crop(xmin = 38, ymin = -6, xmax = 40, ymax = -4)%>%
 # st_transform(4326)  %>%
 st_cast("POINT") %>%
 mutate(month = lubridate::month(date, label = TRUE))

fronts.points.tb <- fronts.points.wgs %>% 
 st_coordinates() %>%
 as_tibble() %>%
 dplyr::rename(x = X, y = Y) %>%
 mutate(area = fronts.points.wgs$area_km2)

# df.1 <- time.swim.count.site.loc
 

# df.2 <- df.1 %>%
#  filter(!st_is_empty(.)) %>%  
#  dplyr::rename(geometry = actual.geom) %>% 
#  as_tibble() %>% 
#  btb::kernelSmoothing(sEPSG = "7855",
#                  iCellSize = 10000L,
#                  iBandwidth = 20000L,
#                  vQuantiles = c(0.1, 0.5, 0.9)) %>% 
#  st_transform(4326) %>%
#  st_as_sf()

# fronts.points.tb = fronts.points.wgs %>% 
#  st_coordinates() %>%
#  as_tibble() %>% 
#  dplyr::rename(x = X, y = Y) %>%
#  mutate(area = fronts.points.wgs$area_km2)

fronts.smoothed <- fronts.points.tb %>%
 # kernelSmoothing(sEPSG = "32737",
 #                 iCellSize = 10000L,
 #                 iBandwidth = 20000L,
 #                 vQuantiles = c(0.1, 0.5, 0.9)) %>%
 btb_smooth(sEPSG = "32737",
                 iCellSize = 10000L,
                 iBandwidth = 20000L,
                 vQuantiles = c(0.1, 0.5, 0.9)) %>%
 st_transform(4326) %>%
 st_as_sf()

fronts.smoothed %>%
 st_crop(xmin = 38, ymin = -6.5, xmax = 41, ymax = -3) %>%
 ggplot() +
 geom_sf(aes(fill = nbObs), col = NA)+
 # geom_sf(data = tz.ke, fill = "grey80") +
 coord_sf(xlim = c(38.6, 39.8), ylim = c(-6,-4.6))+
 # scale_fill_gradientn(colours = mycolor, breaks = seq(0,110,10), name = "Number\nof PFZs",
 #                      guide = guide_colorbar(reverse = TRUE, nbin = 11, raster = FALSE,barheight = unit(6,"cm")))+
 scale_fill_viridis_c() +
 metR::scale_x_longitude(ticks = 0.4)+
 metR::scale_y_latitude(breaks = seq(-5.8,-4.8, length.out = 4) %>% round(1))+
 theme_bw() +
 theme(legend.position = c(0.13,.45),
       axis.text = element_text(size = 11, colour = "black"),
       legend.background = element_rect(fill = "grey83")) +
 ggspatial::annotation_north_arrow(location = "tl", width = unit(.75, "cm"), height = unit(.75, "cm"))+
 ggspatial::annotation_scale(location = "bl", text_cex = .9)

mycolor = c("#7f007f", "#0000ff",  "#007fff", "#00ffff", "#00bf00", "#7fdf00",
            "#ffff00", "#ff7f00", "#ff3f00", "#ff0000", "#bf0000")

ke.pfz <- fronts.smoothed %>%
 st_crop(xmin = 38, ymin = -6.5, xmax = 41, ymax = -3) %>%
 ggplot() +
 geom_sf(aes(fill = nbObs), col = NA)+
 # geom_sf(data = tz.ke, fill = "grey80") +
 coord_sf(xlim = c(38.6, 39.8), ylim = c(-6,-4.6))+
 scale_fill_gradientn(colours = mycolor, breaks = seq(100,1400,120), name = "Number\nof PFZs",
                      guide = guide_colorbar(reverse = TRUE, nbin = 11, raster = FALSE,barheight = unit(6,"cm")))+
 metR::scale_x_longitude(ticks = 0.4)+
 metR::scale_y_latitude(breaks = seq(-5.8,-4.8, length.out = 4) %>% round(1))+
 theme_bw() +
 theme(legend.position = c(0.13,.45),
       axis.text = element_text(size = 11, colour = "black"),
       legend.background = element_rect(fill = "grey83"),
       plot.caption = ggtext::element_markdown()) +
 ggspatial::annotation_north_arrow(location = "tl", width = unit(.75, "cm"), height = unit(.75, "cm"))+
 ggspatial::annotation_scale(location = "bl", text_cex = .9)+
 labs(caption = "<span style = 'font-size:8pt;color:#888888'> Data Source: Tanzania Fisheries Research Insitute <br> Moderate Resolution Imaging Spectroradiometer, <br> Plymouth Marine Laboratory </span>")

fronts.pemba <- fronts.smoothed %>%
 st_crop(xmin = 38, ymin = -7, xmax = 41, ymax = -3.5) %>%
 select(nbObs)

## convert sf into stars object
fronts.pemba.raster <- fronts.pemba %>%
 stars::st_rasterize(nx = 30, ny = 30)

# fronts.pemba.raster %>% plot()

## convert to data table with cubelyr 
fronts.pemba.tb <- fronts.pemba.raster %>% cubelyr::as.tbl_cube()

## obtain individual component
lon <- fronts.pemba.tb$dims$x
lat <- fronts.pemba.tb$dims$y
obs <- fronts.pemba.tb$mets

class(lon); class(lat); class(obs)

fronts.pemba <- expand.grid(lon,lat) %>% 
 bind_cols(expand.grid(obs))  %>% 
 dplyr::rename(lon = 1, lat = 2)

fronts.pemba %>% glimpse()

met.pfz <- ggplot() +
 # geom_contour_filled(data = fronts.pemba, aes(x = lon, y = lat, z = nbObs), bins = 12)+
 metR::geom_contour_fill(data = fronts.pemba, aes(x = lon, y = lat, z = nbObs), bins = 120, na.fill = TRUE)+
 # geom_sf(data = tz.ke, fill = "grey80") +
 coord_sf(xlim = c(38.6, 39.8), ylim = c(-6,-4.7))+
 scale_fill_gradientn(colours = mycolor, breaks = seq(100,1400,120), name = "Number\nof PFZs",
                      guide = guide_colorbar(reverse = TRUE, nbin = 11, raster = TRUE, barheight = unit(6,"cm")))+
 metR::scale_x_longitude(ticks = 0.4)+
 metR::scale_y_latitude(breaks = seq(-5.8,-4.8, length.out = 4) %>% round(1))+
 theme_bw() +
 theme(legend.position = c(0.13,.45),
       axis.text = element_text(size = 11, colour = "black"),
       legend.background = element_rect(fill = "grey83"),
       plot.caption = ggtext::element_markdown()) +
 ggspatial::annotation_north_arrow(location = "tl", width = unit(.75, "cm"), height = unit(.75, "cm"))+
 ggspatial::annotation_scale(location = "bl", text_cex = .9)+
 labs(caption = "<span style = 'font-size:8pt;color:#888888'> Data Source: Tanzania Fisheries Research Insitute <br> Moderate Resolution Imaging Spectroradiometer, <br> Plymouth Marine Laboratory </span>")

met.pfz
