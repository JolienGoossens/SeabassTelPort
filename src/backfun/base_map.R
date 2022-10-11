#### Load packages ####
library(tidyverse)
library(sf)
library(ggrepel)

flag_plot = F # set to TRUE to see what's happening


#### Colour settings ####
colland =  "#c8ccc0" #"#e2d7ca"# "honeydew"
colsea = "white" # "#00949e"

#### Read data ####
eez <- st_read("data/external/mapping/eez", layer = "eez") # EEZ Belgium
shp_zeeb <- st_read("data/external/mapping/GRB_watergang_sel/Shapefile", layer = "Wtze618071") # Port
contour_zeeb = st_read("data/external/mapping/belgium_municipalities", layer ="belgium_municipalities") # Zeebrugge contour

# Transform sail
contour_zeeb = contour_zeeb %>% st_transform(crs = 4326)
if(flag_plot == T) {contour_zeeb %>% ggplot() + geom_sf()}

# Filter Zeebrugge port and transform
shp_zeeb = shp_zeeb %>% 
  filter(OIDN == 275904) %>%  
  st_transform(crs = 4326) 
if(flag_plot == T) {shp_zeeb %>% ggplot() + geom_sf()}

#### Get land and sea based on eez ####
# Make a rectangle
coords = matrix(c(3.1, 51.18,
                  3.1, 51.4,
                  3.3, 51.4,
                  3.3, 51.18,
                  3.1, 51.18), 
                ncol = 2, byrow = TRUE)

rect_st = tibble(lon = c(3.1, 3.3), lat = c(51.18,51.4)) %>% 
  st_as_sf(coords= c("lon", "lat"), crs = 4326) %>% 
  st_bbox() %>% 
  st_as_sfc()

# Intersect and erase with eez
eez_sea <- st_intersection(rect_st, eez)
if(flag_plot == T) {eez_sea %>% ggplot() + geom_sf()}

# Cut port out of sea
eez_sea_cut = sf::st_difference(eez_sea, contour_zeeb)
if(flag_plot == T) {eez_sea_cut %>% ggplot() + geom_sf()}

# Join port water with sea water
shp_zeeb_sea = st_union(eez_sea_cut,shp_zeeb)
if(flag_plot == T) {shp_zeeb_sea %>% ggplot() + geom_sf()}

rm(shp_zeeb, rect_st, eez_sea_cut, eez_sea, eez, contour_zeeb, coords)

#### Make map ####
base_map = shp_zeeb_sea %>% 
  ggplot() +
  theme_minimal() +
  theme(panel.background = element_rect(fill = colland)) +
  geom_sf(size = 0.8, fill = colsea) +
  coord_sf(xlim = c(3.15, 3.26), ylim = c(51.23, 51.37))
