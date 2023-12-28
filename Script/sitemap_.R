#### packages ####

library(readxl)
library(sf)
library(raster)
library(dplyr)
library(spData)
library(spDataLarge)
library(tmap)    # for static and interactive maps
library(tmaptools)
library(leaflet) # for interactive maps
library(ggplot2) # tidyverse data visualization package
library(ggspatial)

# Import shapefile 
province_shp <- here::here("~/R/data/southafrica/guysutton/ZAF_adm/ZAF_adm1.shp") %>%
  st_read()

# Subset the shapefile to only Western cape
province_shp <- province_shp %>%
  dplyr::filter(NAME_1 == "Western Cape") 

# Have to do some cleaning
# Make the shapefile valid, and check crs (projection)
province_shp <- st_as_sf(province_shp) %>%
  sf::st_make_valid()
st_crs(province_shp) = 4326

#### *Create map of site area* ----
# Import data (enter your file path)
mres_gps <- read_excel("~/path/acacia_siteinfo.xlsx")
mres_towns <- read_excel("~/path/wc_towns.xlsx")
mres_gps %>% distinct(site.type)
mres_gps %>% distinct(veg.type)

ggplot(data = province_shp) + geom_sf(fill="beige") + 
  coord_sf(xlim = c(19.35, 19.78), ylim = c(-34.47, -34.65), expand = FALSE) +
  labs(x = "", y = "") +
  annotation_scale(location = "br",  
                   style = "ticks", 
                   pad_x = unit(0.175, "in"), 
                   pad_y = unit(1.45, "in"),
                   width_hint = 0.145) +
  annotation_north_arrow(location = "br", 
                         which_north = "true", 
                         pad_x = unit(0.18, "in"), 
                         pad_y = unit(1.55, "in"),
                         style = north_arrow_fancy_orienteering) +
  # Add the points onto the map
  geom_point(data = mres_gps, aes(x = Longitude, 
                                    y = Latitude,
                                    colour = site.type, shape = veg.type), size = 1.5) +
  # Colour the points manually according to site type: Invaded = red, S. Cleared = blue and Pristine = green
  scale_colour_manual (name = 'Site Type', 
                     values = c("blue", "red", "darkgreen"),
                     breaks=c("Flower-removed","Invaded","Pristine"),
                     labels=c("Flower-removed","Invaded","Pristine")) +
  scale_shape_manual (name = 'Vegetation Type', 
                      values = c('circle','triangle','square'), 
                      breaks=c("OS","EF","AS"),
                      labels=c("Overberg Sandstone","Elim Ferricrete","Agulhus Sand")) +
  theme(legend.position = "right", axis.text=element_text(size=8),legend.title = element_text(face = "bold", size=9), panel.background = element_rect(fill = 'aliceblue')) +
  # Add town names
  #geom_point(data = mres_towns, aes(x = Longitude, 
  #  y = Latitude), size = 1) +
  geom_text(data= mres_towns,aes(x=Longitude+0.02, y=Latitude+0.015, label=name),
          color = "black", fontface = "bold", check_overlap = TRUE, size=2)

??annotation_north_arrow
