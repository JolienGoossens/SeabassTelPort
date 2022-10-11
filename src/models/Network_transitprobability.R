#############################################
# Network analysis: transition probability  #
#############################################

# Jolien Goossens - Marine Biology Research Group, Ghent University (Marbiol) / Flanders Marine Institute (VLIZ)
# R version 3.6.2


#### Run read_and_format.R ####
source("src/backfun/read_and_format.R")
source("src/backfun/sharkov_stehfest2015.R")


#### Filter Zeebrugge ####
df = df %>% 
  filter(acoustic_project_code == "bpns") %>%  # only consider Zeebrugge detections (not release)
  filter(station_group != 'Other')  # only consider Zeebrugge detections

df = df %>% mutate(station_name = ifelse(station_name == "bpns-ZOKN", "bpns-ZW1", station_name))

#### Set levels for plotting later ####
level_setting = c("Absent", "bpns-zbe3", "bpns-zbe2", "bpns-zbe1", 
                  "bpns-zbw3", "bpns-zbw2", "bpns-zbw1",
                  "bpns-ZW1", "bpns-ZAND4", "bpns-ZA2", "bpns-LNG",
                  "bpns-vandamme", "bpns-boudewijn", 
                  "bpns-visartboudewijn",  "bpns-herdersbrug", "bpns-brugge")

level_setting_label = c("Absent", "East3", "East2", "East1", 
                        "West3", "West2", "West1",
                        "ZW1", "ZAND4", "ZA2", "LNG",
                        "Vandamme", "Boudewijn", 
                        "Visart-inner", "Herder", "Brugge")

# Set colours
colred = "#c75133"
colorange = "#ffc05f"
colpurple = "#5f2f47"

cols = c("black", rep(colpurple, 6), rep(colorange, 4), rep(colred, 5))
names(cols) = level_setting_label


#### Format for sharkov ####
df_sharkov = df %>% 
  left_join(select(deploy_zeeb, station_name, label_name)) %>% 
  filter(station_group != 'Other') %>% 
  select(
    id = animal_id,
    time = date_hour,
    state = label_name) %>% 
  group_by(id, time, state) %>% 
  dplyr::summarise() %>% 
  as.data.frame()

#### Compute matrix ####
# Situation: fish is detected in station A on hour 1, not detected on hour 2, detected on station B on hour 3
# Absence is included: stations A and B are not connected
# sharkov_result <- sharkov(df_sharkov, return.matrix = TRUE, time_res = "1 hour", include_absence = TRUE)
# Absence is not included: stations A and B are connected
sharkov_result <- sharkov(df_sharkov, return.matrix = TRUE, time_res = "1 hour", include_absence = FALSE)
sharkov_result_eigen <- sharkov(df_sharkov, return.matrix = FALSE, time_res = "1 hour", include_absence = FALSE)

#### Format for plot  ####
# Transition probabilities
sharkov_result_df = as.data.frame(sharkov_result)

# Centrality
sharkov_result_eigen = t(sharkov_result_eigen)
rownames(sharkov_result_eigen) = 'Centrality'

sharkovcentral = sharkov_result_eigen %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "station_name") %>%  # make a column with station name
  
  gather("station_name2", "eigen", -station_name) %>%  # make a column with station name 2 (not sure if this is destination, think so)
  mutate(eigenlabel = ifelse(round(eigen, 2) == 0, "< 0.01", as.character(round(eigen, 2)))) %>% 
  # Edit values for colouration
  mutate(eigen = eigen*3) %>% 
  # Change levels for plotting purpose
  mutate(station_name = factor(station_name, levels = rev(level_setting_label))) %>%
  mutate(check = "central")

#### Plot matrix ####
network_matrix = sharkov_result_df %>% 
  # Get results in long data format to facilitate plotting
  rownames_to_column(var = "station_name") %>%  # make a column with station name
  gather("station_name2", "eigen", -station_name) %>%  # make a column with station name 2 (not sure if this is destination, think so)
  # Change 0 to NA for plotting purpose
  mutate(eigen = ifelse(eigen == 0, NA, eigen)) %>%
  mutate(eigenlabel = ifelse(round(eigen, 2) == 0, "< 0.01", as.character(round(eigen, 2)))) %>% 
  # Change levels for plotting purpose
  mutate(station_name2 = factor(station_name2, levels = rev(level_setting_label))) %>% 
  mutate(station_name = factor(station_name, levels = levels(station_name2))) %>%
  mutate(check = "noncentral") %>% 
  rbind(sharkovcentral) %>% 
  
  # Plot
  ggplot(aes(station_name2, station_name, fill = station_name, 
             colour = station_name2, alpha = eigen)) +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust=0),
        strip.background = element_blank(), 
        strip.text.x = element_blank()) +
  geom_tile(size = 0.5) +
  geom_text(aes(label = eigenlabel), colour = "black", alpha = 0.5, size = 2.5) +
  scale_fill_manual(values = cols) +
  scale_colour_manual(values = cols) +
  scale_alpha_continuous(range = c(0.3,1), na.value = 0) +
  scale_x_discrete(position = "top") +
  scale_y_discrete(position = "right") +
  labs(x="", y ="") +
  facet_grid(cols = vars(check), scales = "free", space = "free")

ggsave(filename = "reports/figures/Fig5_Transitprobmatrix.jpg", 
       plot = network_matrix,   
       scale = 1, dpi = 600, width = 17, height = 15, units = "cm")
