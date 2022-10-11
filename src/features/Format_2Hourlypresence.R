#########################
# Format presence data  #
#########################

# Jolien Goossens - Marine Biology Research Group, Ghent University (Marbiol) / Flanders Marine Institute (VLIZ)
# R version 3.6.2


#### Run read_and_format.R ####
source("src/backfun/read_and_format.R")

# Filter for only Zeebrugge
df = df %>% filter(station_group != "Other")


#### Summarise per hour ####
df_hour = df %>% 
  mutate(date_month = month(date_time)) %>% 
  group_by(date_hour, date, season, dn,
           tag_serial_number, animal_id, 
           acoustic_project_code, station_name, deploy_latitude, deploy_longitude) %>% 
  summarise()


#### Full presence table ####
# Potential deployments
deploy_zeeb = deploy_zeeb %>% 
  mutate(start_date = parse_date_time(paste0(start_date, " 00:00:00"), orders = "dmy HMS"),
         end_date = parse_date_time(paste0(end_date, " 23:00:00"), orders = "dmy HMS"))

list_pot_hour = lapply(deploy_zeeb$station_name, function(station_i){
  deploy_stat = deploy_zeeb %>% filter(station_name == station_i)
  pot_hour = data.frame(
    station_name = station_i,
    date_hour = seq.POSIXt(deploy_stat$start_date, deploy_stat$end_date, "hour")
  )
  return(pot_hour)
})
pot_hour = do.call(rbind, list_pot_hour)

# Potential detections
an = an %>% 
  mutate(end_date = case_when(
    !is.na(recapture_date_time) & recapture_date_time < battery_estimated_end_date ~ date(recapture_date_time),
    TRUE ~ battery_estimated_end_date
  )) %>% 
  mutate(end_date = parse_date_time(paste0(end_date, " 00:00:00"), orders = "ymd HMS"))
minute(an$capture_date_time) = 0

list_an_hour = lapply(an$animal_id, function(animal_i){
  an_sub = an %>% filter(animal_id == animal_i)
  pot_an = data.frame(
    animal_id = animal_i,
    date_hour = seq.POSIXt(an_sub$capture_date_time, an_sub$end_date, "hour")
  )
  return(pot_an)
})

pot_an = do.call(rbind, list_an_hour)

# Join the pots!
pot = left_join(pot_an, pot_hour)

# Join with actual data
df_pres = left_join(pot, df_hour)

df_pres = df_pres %>% mutate(date = date(date_hour))

# Add presence var
df_pres = df_pres %>% mutate(det_bin = ifelse(is.na(tag_serial_number), 0, 1))

#### Add day/ night per hour ####
sunset = suncalc::getSunlightTimes(date = as.Date(unique(df_pres$date)), 
                                   lat = df$deploy_latitude[1], lon = df$deploy_longitude[1], 
                                   keep = c("sunrise", "sunset"))
sunset <- sunset %>% 
  mutate(date = lubridate::parse_date_time(date, orders = "ymd")) %>% 
  dplyr::select(-lat,-lon)

df_pres <- df_pres %>% 
  left_join(sunset) %>% 
  mutate(dn = ifelse(sunrise <= date_hour & date_hour < sunset, "Day", "Night")) %>% 
  dplyr::select(-sunrise, -sunset)


#### Add time variables ####
df_pres = df_pres %>% 
  mutate(time_hour = hour(date_hour),
         time_month = month(date_hour))


#### Save ####
write_csv(df_pres, "data/interim/df_pres.csv")
