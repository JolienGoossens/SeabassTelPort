#####################
# Organize ETN data #
#####################

# Jolien Goossens - Marine Biology Research Group, Ghent University (Marbiol) / Flanders Marine Institute (VLIZ)
# R version 3.6.2

#### Load packages ####
library(tidyverse)
library(lubridate)


#### Get data ####
df <- read_csv("data/raw/df_raw.csv")
an <- read_csv("data/raw/animals_raw.csv")
tags <- read_csv("data/raw/tags_raw.csv")

#### Clean columns ####
df <- df %>% 
  dplyr::arrange(date_time) %>% 
  dplyr::select(-signal_to_noise_ratio, -source_file, - qc_flag, -deployment_id)

an <- an %>% 
  dplyr::select(
    animal_id,
    tag_serial_number,
    acoustic_tag_id,
    animal_label,
    capture_date_time,
    capture_location,
    capture_latitude,
    capture_longitude,
    release_date_time,
    release_location,
    release_latitude,
    release_longitude,
    recapture_date_time,
    length1_type,
    length1,
    weight
  )

tags <- tags %>% 
  dplyr::select(
    acoustic_tag_id,
    tag_serial_number,
    battery_estimated_life,
    battery_estimated_end_date,
    tag_type,
    sensor_type,
    sensor_slope,
    sensor_intercept,
    step1_min_delay,
    step1_max_delay,
    step1_power,
    step1_duration,
    step2_min_delay,
    step2_max_delay,
    step2_power,
    step2_duration,
    step3_min_delay,
    step3_max_delay,
    step3_power,
    step3_duration,
    step4_min_delay,
    step4_max_delay,
    step4_power,
    step4_duration
  )

#### Add station_group ####
df <- df %>% 
  mutate(station_group = case_when(
    
    station_name %in% c(
      "bpns-zbw3",
      "bpns-zbw2",
      "bpns-zbw1",
      "bpns-zbe1",
      "bpns-zbe2",
      "bpns-zbe3"
    ) ~ "Zeebrugge_out",
    station_name %in% c(
      "bpns-ZAND4",
      "bpns-ZOKN",
      "bpns-ZW1",
      "bpns-ZA2",
      "bpns-LNG"
    ) ~ "Zeebrugge_port",
    station_name %in% c(
      "bpns-boudewijn" , 
      "bpns-vandamme",
      "bpns-herdersbrug",
      "bpns-visartboudewijn",
      "bpns-brugge"
    ) ~ "Zeebrugge_inner",
    TRUE ~ "Other"
  ))

an <- an %>% 
  mutate(station_group = case_when(
    
    capture_location %in% c(
      "Zeebrugge westerdam"
    ) ~ "Zeebrugge_out",
    capture_location %in% c(
      "Boudewijnkanaal",
      "Herdersbrug",
      "Lissewege",
      "Harbour Zeebrugge",
      "Zeebrugge_innerport_Insteekdok",
      "Zeebrugge_innerport_Verbindingsdok",
      "Zeebrugge_innerport"
    ) ~ "Zeebrugge_inner",
    
  ))

#### Formatting of ETN extracted data: tag data ####
# Recalculate tag lifetime
tags <- tags %>% 
  rowwise() %>% 
  mutate(battery_estimated_life2 = case_when(
    is.na(step2_min_delay) & is.na(step3_min_delay) ~ step1_duration,
    !is.na(step2_min_delay) & is.na(step3_min_delay) ~  sum(step1_duration, step2_duration, na.rm = T),
    !is.na(step2_min_delay) & !is.na(step3_min_delay) ~  sum(step1_duration, step2_duration, step3_duration, na.rm = T)))

# Calculate sensor measurements
tags = tags %>% 
  mutate(sensor_slope = case_when(
    sensor_type == 'temperature' & sensor_slope == 0 ~ 0.1569,
    sensor_type == 'pressure' & sensor_slope == 0 ~ 0.30088,
    TRUE ~ sensor_slope)) %>% 
  mutate(sensor_intercept = case_when(
    sensor_type == 'pressure' & sensor_intercept == -1 ~ -1.20354,
    TRUE ~ sensor_intercept
  )) %>% 
  mutate(sensor_type = case_when(
    sensor_type == 'pressure' ~ 'P',
    sensor_type == 'temperature' ~ 'T',
    TRUE ~ sensor_type)) %>% 
  mutate(tag_serial_number = as.character(tag_serial_number))


#### Formatting of ETN extracted data: animal data ####
# Fix classes and a spelling issue
an <- an %>% 
  mutate(station_group = as.factor(station_group)) %>% 
  mutate(tag_serial_number = str_sub(tag_serial_number, 1, 7))

# Link animal data to relevant tag data
an <- tags %>% 
  mutate(
    step1 = paste0(step1_duration, " ", step1_power, " (", step1_min_delay, "-", step1_max_delay, ")"),
    step2 = paste0(step2_duration, " ", step2_power, " (", step2_min_delay, "-", step2_max_delay, ")"),
    step3 = paste0(step3_duration, " ", step3_power, " (", step3_min_delay, "-", step3_max_delay, ")")) %>% 
  mutate(tag_serial_number = as.character(tag_serial_number)) %>% 
  arrange(tag_serial_number) %>% 
  group_by(tag_serial_number, tag_type, battery_estimated_life2, step1, step2, step3) %>% 
  summarise(
    acoustic_tag_id = paste(acoustic_tag_id, collapse = ','),
    sensor_type = paste(sensor_type, collapse = ',')) %>% 
  select(-acoustic_tag_id) %>% 
  as_tibble() %>% 
  right_join(an, by = "tag_serial_number")

an <- an %>% 
  mutate(battery_estimated_end_date = date(release_date_time) + days(battery_estimated_life2))


#### Formatting of ETN extracted data: detection data ####
# Fix classes
df <- df %>% 
  mutate(tag_serial_number= as.character(tag_serial_number))

# Link detecion data to animal metadata
df <- an %>% 
  mutate(capture_date = date(capture_date_time)) %>% 
  select(animal_id, capture_date, battery_estimated_end_date) %>% 
  right_join(df)

# Remove improbable detections (100 days after battery estimated end date)
df <- df %>% 
  mutate(improbable_date = battery_estimated_end_date + lubridate::days(100)) %>% # 100 days after supposed end date
  filter(lubridate::parse_date_time(date_time, orders = "ymd HMS") < improbable_date) %>% 
  select(-improbable_date)

# Add sensor values
df = tags %>%
  select(tag_serial_number, acoustic_tag_id, sensor_type, sensor_slope, sensor_intercept) %>% 
  right_join(df) %>% 
  mutate(sensor_value_calc = sensor_intercept + (sensor_slope * sensor_value)) %>% 
  select(-sensor_slope, sensor_intercept)

df = df %>% 
  mutate(sensor_value_calc = ifelse(sensor_type == "P" & 
                                      sensor_value_calc < 0, 0, sensor_value_calc))

#### Add animal capture and recapture ####
an_release = an %>% select(
  tag_serial_number, animal_id, 
  date_time = release_date_time, 
  station_name = release_location, 
  deploy_latitude = release_latitude, 
  deploy_longitude = release_longitude, 
  station_group) %>% 
  mutate(acoustic_project_code = "release")

an_recapture = an %>% 
  filter(!is.na(recapture_date_time)) %>% 
  select(
    tag_serial_number, animal_id, 
    date_time = recapture_date_time) %>% 
  mutate(
    station_group = "Other",
    acoustic_project_code = "recapture")


df = full_join(df, an_release)
df = full_join(df, an_recapture)


#### Add time variables ####
df <- df %>% 
  as_tibble() %>% 
  mutate(date = parse_date_time(date(date_time), orders = "ymd")) %>% 
  mutate(date_hour = date_time) %>% 
  mutate(season = quarter(date_time))

minute(df$date_hour) = 0
second(df$date_hour) = 0

# Add day night
sunset = suncalc::getSunlightTimes(date = as.Date(unique(df$date)), 
                                   lat = df$deploy_latitude[1], lon = df$deploy_longitude[1], 
                                   keep = c("sunrise", "sunset"))
sunset <- sunset %>% 
  mutate(date = parse_date_time(date, orders = "ymd")) %>% 
  select(-lat,-lon)

df <- df %>% 
  left_join(sunset) %>% 
  mutate(dn = ifelse(sunrise <= date_time & date_time < sunset, "Day", "Night")) %>% 
  select(-sunrise, -sunset)



#### Save data ####
write_csv(df, "data/interim/df.csv")
write_csv(an, "data/interim/animals.csv")
write_csv(tags, "data/interim/tags.csv")
