#################################
# Format hourly epistemic data  #
#################################

# Jolien Goossens - Marine Biology Research Group, Ghent University (Marbiol) / Flanders Marine Institute (VLIZ)
# R version 3.6.2


#### Run read_and_format.R ####
source("src/backfun/read_and_format.R")


#### Make data frame with all possible dates for each fish ####
list_pot <- lapply(unique(an$animal_id), function(animal_id_i) {
  df_temp <- df %>% filter(animal_id == animal_id_i)
  an_temp <- an %>% filter(animal_id == animal_id_i)
  
  # # Get potential dates
  # if (is.na(an_temp$recapture_date_time) | 
  #     an_temp$recapture_date_time > an_temp$battery_estimated_end_date) {
  #   pot_dates = seq.Date(from = date(an_temp$capture_date_time), 
  #                        to = date(an_temp$battery_estimated_end_date), by = "day")
  # } else {
  #   pot_dates = seq.Date(from = date(an_temp$capture_date_time), 
  #                        to = date(an_temp$recapture_date_time), by = "day")
  # }
  
  
  # Get potential dates
  if (is.na(an_temp$recapture_date_time) | 
      an_temp$recapture_date_time > an_temp$battery_estimated_end_date) {
    df_temp = df_temp %>% filter(acoustic_project_code != "recapture")
    pot_dates = seq.Date(from = date(an_temp$capture_date_time), 
                         to = date(max(df_temp$date)), by = "day")
  } else {
    pot_dates = seq.Date(from = date(an_temp$capture_date_time), 
                         to = date(an_temp$recapture_date_time), by = "day")
  }
  
  
  # Put potential dates in a dataframe
  df_pot = expand.grid(
    animal_id = animal_id_i,
    date = parse_date_time(pot_dates, orders = "ymd"),
    hour = 1:24)
  
  df_pot = df_pot %>% 
    mutate(date_hour = parse_date_time(paste0(date, " ", hour, ":00:00"), 
                                       orders = "ymd HMS")) %>% 
    select(-hour)
  
  # Link potential dates dataframe with known dates
  df_pot = df_temp %>% 
    arrange(date_time) %>% 
    group_by(date_hour) %>% 
    summarise(station_name_first = first(station_name),
              station_group_first = first(station_group),
              station_name_last = last(station_name),
              station_group_last = last(station_group)) %>% 
    ungroup() %>% 
    mutate(station_group = "Known") %>% 
    right_join(df_pot) %>% 
    arrange(date_hour)
  
  # If not detected first day: change station_group_last to tagging station group
  df_pot = df_pot %>% mutate(
    station_group_last = ifelse(row_number() == 1 & is.na(station_group_last),
                                an_temp$station_group, station_group_last),
    station_name_last = ifelse(row_number() == 1 & is.na(station_name_last),
                               an_temp$release_location, station_name_last),
    station_group = ifelse(row_number() == 1 & is.na(station_group), 
                           "Known", station_group))
  
  # Fill first and last with relevant values
  df_pot = df_pot %>% 
    mutate(
      station_group_first = zoo::na.locf(station_group_first, fromLast = T, na.rm = F),
      station_name_first = zoo::na.locf(station_name_first, fromLast = T, na.rm = F),
      station_group_last = zoo::na.locf(station_group_last, na.rm = F),
      station_name_last = zoo::na.locf(station_name_last, na.rm = F)
    ) %>% 
    mutate(
      station_group = case_when(
        is.na(station_group) & 
          station_group_last == "Zeebrugge_inner" & 
          station_group_first == "Zeebrugge_inner" ~ "Zeebrugge_inner",
        is.na(station_group) & 
          station_group_last %in% c("Zeebrugge_out", "Other", "Zeebrugge_port") ~ "Other",
        is.na(station_group) & 
          is.na(station_group_first) & 
          station_group_last == "Zeebrugge_inner" ~ "Zeebrugge_inner",
        is.na(station_group) & 
          is.na(station_group_first) & 
          station_group_last %in% c("Zeebrugge_out", "Other", "Zeebrugge_port") ~ "Other",
        TRUE ~ station_group))
  return(df_pot)
})

# Combine in data table
df_pot = plyr::ldply(list_pot)
df_pot = df_pot %>% as_tibble()

# Add epistemic quality
df_pot = df_pot %>% mutate(qual = ifelse(station_group == "Known", "det", "presumed"))
df_pot = df_pot %>% mutate(station_group = ifelse(station_group == "Known",
                                                  station_group_first,
                                                  station_group))
# Limit column numbers for saving
df_pot_save = df_pot %>%
  select(date_hour, animal_id, station_group_epist = station_group, qual)


#### Save ####
write_csv(df_pot_save, "data/interim/df_pot_hour.csv")
