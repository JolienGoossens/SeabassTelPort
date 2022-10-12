######################
# Map of study area  #
######################

# Jolien Goossens - Marine Biology Research Group, Ghent University (Marbiol) / Flanders Marine Institute (VLIZ)
# R version 3.6.2

#### Read data and packages ####
source("src/backfun/base_map.R")
# devtools::install_github('oswaldosantos/ggsn')
library(ggsn)
deploy_zeeb <- read_delim("data/raw/deploy_zeebrugge.csv")


#### Make plot ####
map_zeebrugge =  base_map + 
  coord_sf(xlim = c(3.15, 3.26), ylim = c(51.22, 51.37)) +
  theme(legend.position = "none",
        axis.title = element_blank()) +
  geom_point(data = filter(deploy_zeeb, label_name != 'ZOKN'), 
             aes(deploy_longitude, deploy_latitude, shape = deploy_type, fill = station_group), size = 4) +
  scale_shape_manual(values = c(21, 22, 24)) +
  scale_fill_manual(values = cols) +
  scale_x_continuous(breaks = c(3.16, 3.2, 3.24)) +
  scale_y_continuous(breaks = c(51.24, 51.28, 51.32))+
  ggsn::scalebar(location =  "bottomleft", 
                 x.min = 3.15, x.max =3.26, y.min = 51.22, y.max = 51.37, 
                 dist= 1, dist_unit = "km", model = "WGS84",  transform= T) +
  
  geom_text(data = filter(deploy_zeeb, label_name %in% c("West1", "West2", "West3")),
            aes(deploy_longitude, deploy_latitude, label = label_name),
            nudge_x = -0.01, nudge_y = 0.002) +
  geom_text(data = filter(deploy_zeeb, label_name %in% c("East1", "East2", "East3")),
            aes(deploy_longitude, deploy_latitude, label = label_name),
            nudge_x = 0.005, nudge_y = 0.005, angle = 30) +
  geom_text(data = filter(deploy_zeeb, label_name %in% c("Brugge", "Herdersbrug", "Boudewijn", "Visart-inner", "Visart-port")),
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

#### Save ####
ggsave(filename = "reports/figures/Fig1_Studyarea.jpg", 
       plot = map_zeebrugge,   
       scale = 1, dpi = 600, width = 15, height = 20, units = "cm")
