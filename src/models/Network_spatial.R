##################################
# Network analysis: spatial map  #
##################################

# Jolien Goossens - Marine Biology Research Group, Ghent University (Marbiol) / Flanders Marine Institute (VLIZ)
# R version 3.6.2


#### Run read_and_format.R ####
source("src/backfun/read_and_format.R")
source("src/backfun/base_map.R")
source("src/backfun/sharkov_stehfest2015.R")
library(patchwork)

#### Plot for all stations concatenating port wall stations ####
#### Filter Zeebrugge ####
df = df %>% 
  filter(acoustic_project_code == "bpns") %>%  # only consider Zeebrugge detections (not release)
  filter(station_group != 'Other')  # only consider Zeebrugge detections

#### Get edges ####
deploy_zeeb2 <- deploy_zeeb %>%
  mutate(
    deploy_latitude = case_when(
      station_name %in% c("bpns-zbw1", "bpns-zbw2", "bpns-zbw3") ~ 51.36291,
      station_name %in% c("bpns-zbe1", "bpns-zbe2", "bpns-zbe3") ~ 51.3643,
      station_name %in% c("bpns-ZW1", "bpns-ZOKN") ~ 51.35821,
      TRUE ~ deploy_latitude),
    deploy_longitude = case_when(
      station_name %in% c("bpns-zbw1", "bpns-zbw2", "bpns-zbw3") ~ 3.183634,
      station_name %in% c("bpns-zbe1", "bpns-zbe2", "bpns-zbe3") ~ 3.200701,
      station_name %in% c("bpns-ZW1", "bpns-ZOKN") ~ 3.189648,
      TRUE ~ deploy_longitude),
    station_name = case_when(
      station_name %in% c("bpns-zbw1", "bpns-zbw2", "bpns-zbw3") ~ "bpns-zbw",
      station_name %in% c("bpns-zbe1", "bpns-zbe2", "bpns-zbe3") ~ "bpns-zbe",
      station_name %in% c("bpns-ZW1", "bpns-ZOKN") ~ "bpns-ZW1",
      TRUE ~ station_name))

df_zeeb <- df %>% 
  select(animal_id, station_name, date_time) %>% 
  mutate(
    station_name = case_when(
      station_name %in% c("bpns-zbw1", "bpns-zbw2", "bpns-zbw3") ~ "bpns-zbw",
      station_name %in% c("bpns-zbe1", "bpns-zbe2", "bpns-zbe3") ~ "bpns-zbe",
      station_name %in% c("bpns-ZW1", "bpns-ZOKN") ~ "bpns-ZW1",
      TRUE ~ station_name))  %>% 
  left_join(deploy_zeeb2)


df_short <- df_zeeb %>% 
  arrange(date_time) %>% 
  group_by(animal_id) %>% 
  mutate(llon=lead(deploy_longitude), llat=lead(deploy_latitude), lstation = lead(station_name), lstation_group = lead(station_group)) %>% 
  filter(llon != deploy_longitude) %>% 
  group_by(animal_id, 
           station_name, station_group, deploy_longitude, deploy_latitude, 
           lstation, lstation_group, llon, llat) %>% 
  summarise(n=n()) %>% 
  group_by(station_name, station_group, deploy_longitude, deploy_latitude, 
           lstation, lstation_group, llon, llat) %>% 
  summarise(n = sum(n))

#### Format for sharkov ####
df_sharkov = df %>% 
  mutate(station_name = case_when(
    station_name %in% c("bpns-zbw1", "bpns-zbw2", "bpns-zbw3") ~ "bpns-zbw",
    station_name %in% c("bpns-zbe1", "bpns-zbe2", "bpns-zbe3") ~ "bpns-zbe",
    station_name %in% c("bpns-ZW1", "bpns-ZOKN") ~ "bpns-ZW1",
    TRUE ~ station_name)) %>% 
  select(
    id = animal_id,
    time = date_hour,
    state = station_name) %>% 
  group_by(id, time, state) %>% 
  dplyr::summarise() %>% 
  as.data.frame()

#### Calculate centrality ####
sharkov_result_eigen <- sharkov(df_sharkov, return.matrix = FALSE, time_res = "1 hour", include_absence = FALSE)
sharkov_result_df = data.frame(
  station_name = rownames(sharkov_result_eigen),
  sharkov_result_eigen = round(sharkov_result_eigen, digits = 2)
)
sharkov_result_df = sharkov_result_df %>% left_join(deploy_zeeb2)

#### Plot map ####
network_map = base_map +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm")) +
  theme(legend.position=c(0.15, 0.15),
        legend.title = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA)) +
  geom_curve(data = df_short, 
             aes(x=deploy_longitude, xend=llon, y=deploy_latitude, yend=llat, 
                 size = n,  colour= station_group),
             alpha = 0.5) +
  geom_point(data = sharkov_result_df, 
             aes(deploy_longitude, deploy_latitude, 
                 size = sharkov_result_eigen*1000, fill = station_group),
             shape = 21, show.legend = F) +
  scale_fill_manual(values = cols, guide = "none") +
  scale_colour_manual(values = cols, guide = "none") +
  scale_size_continuous(breaks = c(2, 20, 200), limits = c(1,350), range = c(0.8,8)) +
  labs(x = "", y = "")



#### Subplot of outer ####
#### Filter Zeebrugge ####
df_zeeb <- df %>% 
  select(animal_id, station_name, date_time) %>% 
  left_join(deploy_zeeb)

#### Get edges ####
df_short <- df_zeeb %>% 
  arrange(date_time) %>% 
  group_by(animal_id) %>% 
  mutate(llon=lead(deploy_longitude), llat=lead(deploy_latitude), lstation = lead(station_name), lstation_group = lead(station_group)) %>% 
  filter(llon != deploy_longitude) %>% 
  # filter(!is.na(llon))  %>% 
  group_by(animal_id, station_name, station_group, deploy_longitude, deploy_latitude, lstation, lstation_group, llon, llat) %>% 
  summarise(n=n()) %>% 
  group_by(station_name, station_group, deploy_longitude, deploy_latitude, lstation, lstation_group, llon, llat) %>% 
  summarise(n = sum(n))

#### Format for sharkov ####
df_sharkov = df %>% 
  select(
    id = animal_id,
    time = date_hour,
    state = station_name) %>% 
  group_by(id, time, state) %>% 
  dplyr::summarise() %>% 
  as.data.frame()

#### Calculate centrality ####
sharkov_result_eigen <- sharkov(df_sharkov, return.matrix = FALSE, time_res = "1 hour", include_absence = FALSE)
sharkov_result_df = data.frame(
  station_name = rownames(sharkov_result_eigen),
  sharkov_result_eigen = round(sharkov_result_eigen, digits = 2)
)
sharkov_result_df = sharkov_result_df %>% left_join(deploy_zeeb)

#### Plot map ####
network_map_out = base_map +
  coord_sf(xlim = c(3.16, 3.23), ylim = c(51.35, 51.37)) +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm")) +
  theme(legend.position=c(0.15, 0.1),
        legend.title = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA)) +
  geom_curve(data = filter(df_short, 
                           station_group == "Zeebrugge_out" & 
                             lstation_group == "Zeebrugge_out"), 
             aes(x=deploy_longitude, xend=llon, y=deploy_latitude, yend=llat, 
                 size = n,  colour= station_group),
             curvature = -0.5, alpha = 0.5, show.legend = F) +
  geom_point(data = filter(sharkov_result_df, station_group == "Zeebrugge_out"), 
             aes(deploy_longitude, deploy_latitude, 
                 size = sharkov_result_eigen*1000, fill = station_group),
             shape = 21, show.legend = F) +
  scale_fill_manual(values = cols, guide = "none") +
  scale_colour_manual(values = cols, guide = "none") +
  scale_size_continuous(breaks = c(2, 20, 200), limits = c(1,350), range = c(0.8,8)) +
  labs(x = "", y = "")


network_map_combi = network_map_out /network_map

#### Save ####
ggsave(filename = "reports/figures/Fig6_Networkmap.jpg", 
       plot = network_map_combi,   
       scale = 1, dpi = 600, width = 13, height = 16, units = "cm")

