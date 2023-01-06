######################
# Map of study area  #
######################

# Jolien Goossens - Marine Biology Research Group, Ghent University (Marbiol) / Flanders Marine Institute (VLIZ)
# R version 3.6.2

#### Read data and packages ####
source("src/backfun/base_map.R")
source("src/backfun/read_and_format.R")
# devtools::install_github('oswaldosantos/ggsn')
library(ggsn)
library(patchwork)
deploy_zeeb <- read_delim("data/raw/deploy_zeebrugge.csv")

#### Get ICES areas ####
ices = st_read("Data/external/ICES_areas", layer = "ICES_Areas_20160601_cut_dense_3857")

ices = ices %>% 
  mutate(stock = case_when(
    Area_27 %in% c("4.b", "4.c", "7.a", "7.d", "7.e", "7.f", "7.g", "7.h") ~ "Northern stock",
    Area_27 %in% c("6.a", "7.b", "7.j.1", "7.j.2") ~ "West coast Scotland and Ireland",
    Area_27 %in% c("8.a", "8.b") ~ "Bay of Biscay",
    Area_27 %in% c("8.c", "9.a") ~ "North Spain & Portugal"
  ))  %>% 
  filter(!is.na(stock))

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
  geom_point(data = filter(deploy_zeeb, label_name == 'Vandamme'), 
             aes(deploy_longitude, deploy_latitude), 
             shape = 19, colour= "#c75133", size = 3) +
  scale_fill_manual(values = colstocks) +
  # coord_sf(xlim = c(-2,3 ), ylim = c(51, 52))
  coord_sf(xlim = c(-12, 11), ylim = c(37, 60)) +
  guides(fill = guide_legend(label.position = "left", label.hjust = 1)) +
  labs(x ="", y= "")



#### Make plot ####
map_zeebrugge = base_map +
  coord_sf(xlim = c(3.15, 3.26), ylim = c(51.22, 51.37)) +
  theme(legend.position = "none",
        axis.title = element_blank()) +
  geom_point(data = filter(deploy_zeeb, label_name != 'ZOKN'), 
             aes(deploy_longitude-0.001, deploy_latitude -0.0005, shape = deploy_type, fill = station_group), size = 4) +
  scale_shape_manual(values = c(21, 22, 24)) +
  scale_fill_manual(values = cols) +
  scale_x_continuous(breaks = c(3.16, 3.2, 3.24)) +
  scale_y_continuous(breaks = c(51.24, 51.28, 51.32))+
  ggsn::scalebar(location =  "bottomleft", 
                 x.min = 3.15, x.max =3.26, y.min = 51.22, y.max = 51.37, 
                 dist= 1, dist_unit = "km", model = "WGS84",  transform= T, 
                 st.color = "gray40", box.fill = c("gray40", "white"), box.color = "gray40") +
  
  geom_text(data = filter(deploy_zeeb, label_name %in% c("West1", "West2", "West3")),
            aes(deploy_longitude, deploy_latitude, label = label_name),
            nudge_x = -0.01, nudge_y = 0.002) +
  geom_text(data = filter(deploy_zeeb, label_name %in% c("East1", "East2", "East3")),
            aes(deploy_longitude, deploy_latitude, label = label_name),
            nudge_x = 0.005, nudge_y = 0.005, angle = 30) +
  geom_text(data = filter(deploy_zeeb, label_name %in% c("Brugge", "Herder", "Boudewijn", "Visart-inner", "Visart-port")),
            aes(deploy_longitude, deploy_latitude, label = label_name),
            nudge_x = -0.018, nudge_y = 0)+
  geom_text(data = filter(deploy_zeeb, label_name %in% c("Vandamme")),
            aes(deploy_longitude, deploy_latitude, label = label_name),
            nudge_x = 0.02, nudge_y = 0.001)+
  geom_text(data = filter(deploy_zeeb, label_name %in% c("ZAND4")),
            aes(deploy_longitude, deploy_latitude, label = label_name),
            nudge_x = 0.01, nudge_y = -0.002)+
  geom_text(data = filter(deploy_zeeb, label_name %in% c("LNG")),
            aes(deploy_longitude, deploy_latitude, label = label_name),
            nudge_x = -0.008, nudge_y = -0.001)+
  geom_text(data = filter(deploy_zeeb, label_name %in% c("ZA2")),
            aes(deploy_longitude, deploy_latitude, label = label_name),
            nudge_x = 0.008, nudge_y = -0.001)+
  geom_text(data = filter(deploy_zeeb, label_name %in% c("ZW1")),
            aes(deploy_longitude, deploy_latitude, label = label_name),
            nudge_x = 0, nudge_y = -0.003)

map_study = map_stocks + map_zeebrugge


#### Save ####
# ggsave(filename = "reports/figures/Fig1_Studyarea.jpg", 
#        plot = map_zeebrugge,   
#        scale = 1, dpi = 600, width = 15, height = 20, units = "cm")
ggsave(filename = "reports/figures/Fig1_Studyarea.jpg", 
       plot = map_study,   
       scale = 1, dpi = 600, width = 24, height = 20, units = "cm")

