###################################################
# Calculate residence index (RI) & site fidelity  #
###################################################

# Jolien Goossens - Marine Biology Research Group, Ghent University (Marbiol) / Flanders Marine Institute (VLIZ)
# R version 3.6.2


#### Run read_and_format.R ####
source("src/backfun/read_and_format.R")

# Number of detections
df %>% 
  filter(!acoustic_project_code %in% c("release", "recapture")) %>%
  nrow()
df %>% 
  filter(!acoustic_project_code %in% c("release", "recapture")) %>%
  filter(station_group != "Other") %>% 
  nrow()

# Number of animals
df %>% 
  filter(!acoustic_project_code %in% c("release", "recapture")) %>%
  filter(station_group != "Other") %>% 
  distinct(tag_serial_number) %>% 
  nrow()

df %>% 
  filter(!acoustic_project_code %in% c("release", "recapture")) %>%
  distinct(tag_serial_number) %>% 
  nrow()

#### Filter for port data ####
df <- df %>% filter(station_group != "Other") %>% filter(acoustic_project_code == "bpns")
df <- df %>% mutate(station_group2 = ifelse(station_group == "Zeebrugge_inner", "Inner", "Outer"))

unique(df$station_name) # No detections at bpns-visart

#### Get minimum temperature ####
df %>% filter(sensor_type == "T") %>% summarise(min_T= min(sensor_value_calc))
# 2.85 °C

df %>% filter(sensor_type == "T") %>% 
  filter(season == 1) %>% 
  select(sensor_value_calc) %>% 
  summary()
# median temperature in winter: 6.9 °C


#### Calculate for every individual for entire study period ####
res <- df %>% 
  group_by(animal_id) %>% 
  summarise(
    n_det = n(),
    n_days = length(unique(date(date_time))),
    n_hours = length(unique(date_hour)),
    min_date = min(date_time),
    max_date = max(date_time))

res <- an %>% 
  mutate(battery_estimated_life2 = ifelse(
    recapture_date_time > battery_estimated_end_date | is.na(recapture_date_time),
    battery_estimated_life2,
    ceiling(as.numeric(difftime(recapture_date_time, release_date_time, units = "days"))))) %>% 
  
  select(animal_id, 
         length = length1, 
         weight, release_date_time, 
         station_group, 
         capture_location, capture_latitude, capture_longitude, 
         battery_estimated_life2) %>% 
  left_join(res) %>% 
  
  mutate(residency_day_battery = n_days/battery_estimated_life2,
         residency_hr_battery = n_hours/(battery_estimated_life2*24)) %>% 
  mutate(site_fidelity = ifelse(difftime(max_date, release_date_time, units = "days") > 180, 
                                "YES", "NO")) %>% 
  mutate(site_fidelity = ifelse(is.na(site_fidelity), 
                                "NO", site_fidelity)) %>% 
  mutate(site_fidelity = ifelse(battery_estimated_life2 < 6*30, 
                                NA, site_fidelity))

#### Site fidelity ####
table(res$site_fidelity)

#### RI ####
res %>% select(residency_day_battery, residency_hr_battery) %>% summary()

#### RI and site fidelity table ####
res_sum = res %>% 
  group_by(station_group) %>% 
  summarise(length_median = median(length),
            length_min = min(length),
            length_max = max(length),
            n_sitfid = sum(site_fidelity == "YES", na.rm = T),
            n_nositfid = sum(site_fidelity == "NO", na.rm = T),
            residency_day_median = median(residency_day_battery, na.rm = T),
            residency_day_min = min(residency_day_battery, na.rm = T),
            residency_day_max = max(residency_day_battery, na.rm = T),
            residency_hr_median = median(residency_hr_battery, na.rm = T),
            residency_hr_min = min(residency_hr_battery, na.rm = T),
            residency_hr_max = max(residency_hr_battery, na.rm = T))


res_taggingloc = df %>%
  group_by(animal_id, station_group2) %>%
  summarise(
    max_date = max(date_time))

res_taggingloc =res %>%
  filter(site_fidelity == "YES") %>%
  left_join(res_taggingloc) %>%
  mutate(station_group2 = ifelse(station_group2 =="Inner", "Zeebrugge_inner", "Zeebrugge_out")) %>%
  filter(station_group == station_group2) %>%
  mutate(site_fidelity = ifelse(difftime(max_date, release_date_time, units = "days") > 180, "YES", "NO"))

res_sum = res_taggingloc %>% 
  group_by(station_group) %>% 
  summarise(n_sitfidtagging = sum(site_fidelity == "YES", na.rm = T)) %>% 
  right_join(res_sum)

res_sum_nice = res_sum %>% 
  mutate(length = paste0(length_median, " [", length_min, "-", length_max, "]"),
         sitfid = n_sitfid / (n_sitfid + n_nositfid),
         sitfidtagging = n_sitfidtagging / (n_sitfid + n_nositfid),
         residency_day = paste0(round(residency_day_median, 2), 
                                " [", round(residency_day_min, 2), "-", 
                                round(residency_day_max, 2), "]"),
         residency_hr = paste0(round(residency_hr_median, 2), 
                               " [", round(residency_hr_min, 2), "-", 
                               round(residency_hr_max, 2), "]")) %>% 
  mutate(sitfid = round(sitfid, 2)*100,
         sitfidtagging = round(sitfidtagging, 2)*100) %>% 
  select(station_group, length, sitfid, sitfidtagging, residency_day, residency_hr)

