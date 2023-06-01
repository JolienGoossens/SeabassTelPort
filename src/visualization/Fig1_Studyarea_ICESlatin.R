######################
# Map of study area  #
######################

# Jolien Goossens - Marine Biology Research Group, Ghent University (Marbiol) / Flanders Marine Institute (VLIZ)
# R version 3.6.2

#### Read data and packages ####
library(sf)
library(tidyverse)

#### Colour settings ####
colland = "gray90"  #"#c8ccc0" #"#e2d7ca"# "honeydew"
colsea = "white" # "#00949e"

#### Get ICES areas ####
ices = st_read("Data/external/ICES_areas", layer = "ICES_Areas_20160601_cut_dense_3857")
ices_latin = read_delim("data/raw/ices_latin.txt")
ices_latin = ices_latin %>% filter(latin != "VIIId")

ices = ices %>% 
  mutate(stock = case_when(
    Area_27 %in% c("4.b", "4.c", "7.a", "7.d", "7.e", "7.f", "7.g", "7.h") ~ "Northern stock",
    Area_27 %in% c("6.a", "7.b", "7.j.1", "7.j.2") ~ "West coast Scotland and Ireland",
    Area_27 %in% c("8.a", "8.b") ~ "Bay of Biscay",
    Area_27 %in% c("8.c", "9.a") ~ "North Spain & Portugal"
  ))  %>% 
  filter(!is.na(stock)) %>% 
  mutate(stock = factor(stock, levels = c(
    "Northern stock",
    "West coast Scotland and Ireland",
    "Bay of Biscay",
    "North Spain & Portugal"
  )))

colstocks = c(
  "Northern stock" = "#A9E0D9",
  "West coast Scotland and Ireland" = "#D2EBF6",
  "Bay of Biscay" = "#386FA4",
  "North Spain & Portugal" = "#133C55"
)




#### Get Europe ####
europe = st_read("Data/external/worldcontinents", layer = "worldcontinents")
africa = st_read("Data/external/worldcontinents_africa", layer = "worldcontinents")

ices = ices %>% st_transform(crs = st_crs(europe))

map_stocks  = ggplot() +
  theme_bw() +
  theme(panel.background = element_rect(fill = colsea),
        legend.title = element_blank(),
        legend.position = c(0.71, 0.47),
        legend.background = element_rect(fill = NA),
        legend.key = element_rect(fill = NA),
        legend.text = element_text(size = 11)) +
  geom_sf(data = ices, aes(fill = stock))+
  geom_sf(data = europe, fill = colland) +
  geom_sf(data = africa, fill = colland) +
  geom_text(data = ices_latin, aes(x = lon, y = lat, label = latin), 
            size = 3, colour = "maroon1", fontface = 'bold') +
  scale_fill_manual(values = colstocks) +
  # coord_sf(xlim = c(-2,3 ), ylim = c(51, 52))
  coord_sf(xlim = c(-12, 11), ylim = c(37, 60)) +
  guides(fill = guide_legend(label.position = "left", label.hjust = 1)) +
  labs(x ="", y= "")



#### Save ####
ggsave(filename = "reports/figures/Fig_extramanagement_ICESmap.jpg", 
       plot = map_stocks,   
       scale = 1, dpi = 600, width = 14, height = 19, units = "cm")

