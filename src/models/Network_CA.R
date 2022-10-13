##############################################
# Network analysis: Correspondence Analysis  #
##############################################

# Jolien Goossens - Marine Biology Research Group, Ghent University (Marbiol) / Flanders Marine Institute (VLIZ)
# R version 3.6.2


#### Read data and load packages ####
source("src/backfun/read_and_format.R")
library(FactoMineR)

df_pres = read_csv("data/interim/df_pres.csv")

#### Format ####
# Receiver at station ZOKN was moved to ZW1 (very close): consider as 1 station for this analysis
df_pres = df_pres %>% mutate(station_name = ifelse(station_name == "bpns-ZOKN", "bpns-ZW1", station_name))


# Bring to daily level
df_pres_an = df_pres %>% 
  group_by(station_name, animal_id, date) %>% 
  summarise(det_bin = ifelse(sum(det_bin) == 0, 0, 1))

# Calculate period at large and RI (= res)
df_pres_an = df_pres_an %>% 
  group_by(animal_id, station_name) %>% 
  summarise(n_date = length(unique(date)),
            n_det = sum(det_bin)) %>% 
  mutate(res = n_det/n_date)

# Make matrix
df_pres_an = df_pres_an %>% 
  select(animal_id, station_name, res) %>% 
  spread(animal_id, res, fill = 0) 


#### CA animal - station ####
clust = df_pres_an %>% 
  data.matrix()
row.names(clust) = df_pres_an$station_name
clust = clust[,-1]
cari = CA(clust)

#### Plot output CA  ####
coor_an = as.data.frame(cari$col$coord[,1:2])
colnames(coor_an) = c("dim1", "dim2")
coor_an$animal_id = as.numeric(row.names(coor_an))
coor_an = an %>% select(animal_id, station_group) %>% right_join(coor_an)

coor_stat = as.data.frame(cari$row$coord[,1:2])
colnames(coor_stat) = c("dim1", "dim2")
coor_stat$station_name = row.names(coor_stat)
coor_stat = deploy_zeeb %>% select(station_name, label_name, station_group) %>% right_join(coor_stat)

plot_ca = ggplot() +
  theme_bw() +
  theme(legend.position = "none") +
  coord_fixed(ratio = 1/2) +
  geom_point(data = coor_stat, shape = 8, size = 5, alpha = 0.8, aes(dim1, dim2, colour = station_group)) +
  geom_point(data = coor_an, shape = 21, size = 2, aes(dim1, dim2, fill = station_group)) +
  geom_text(data = filter(coor_stat, label_name %in% c("LNG", "ZA2")), 
            aes(dim1, dim2, label = label_name, colour = station_group),
            position = position_nudge(x = -0.1)) +
  geom_text(data = filter(coor_an, animal_id %in% c(7179, 9089, 3511)), 
            aes(dim1, dim2, label = animal_id, colour = station_group),
            position = position_nudge(y = 0.2, x = -0.1)) +
  scale_fill_manual(values = cols, name = "Location") +
  scale_colour_manual(values = cols, name = "Location") +
  labs(x = paste0("Dim 1 - ", round(cari$eig[1,2], 2), "%"),
       y = paste0("Dim 2 - ", round(cari$eig[2,2], 2), "%"))


#### Save plot #### 
ggsave(filename = "reports/figures/Fig5_CA.jpg", 
       plot = plot_ca,   
       scale = 1, dpi = 600, width = 8, height = 14, units = "cm")
