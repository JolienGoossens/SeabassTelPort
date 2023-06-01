####################################
# Network analysis: co-occurrence  #
####################################

# Jolien Goossens - Marine Biology Research Group, Ghent University (Marbiol) / Flanders Marine Institute (VLIZ)
# R version 3.6.2


#### Run read_and_format.R ####
source("src/backfun/read_and_format.R")

#### Format data ####
df = df %>% filter(!acoustic_project_code %in% c("release", "recapture"))

# Filter for at least 5 hours with detections
tag_vect = df %>% 
  group_by(tag_serial_number) %>% 
  summarise(n_hour = length(unique(date_hour))) %>% 
  filter(n_hour >= 5) %>% 
  distinct(tag_serial_number) %>% 
  pull()

an = an %>% filter(tag_serial_number %in% tag_vect)
table(an$station_group) # Number of animals for each group that the analysis can be done on (filter of at least 5 hours)

an = an %>% mutate(tag_serial_number = as.character(tag_serial_number)) %>% 
  arrange(station_group, battery_estimated_end_date)
tag_vect = an %>% distinct(tag_serial_number) %>% pull() 

df = df %>% 
  mutate(tag_serial_number = as.character(tag_serial_number)) %>% 
  filter(tag_serial_number %in% tag_vect) 


#### Combine dyads ####
list_result = lapply(1:length(combn(unique(tag_vect), m=2)[1,]), function(combo){
  tagA = combn(unique(tag_vect), m=2)[1,combo]
  tagB = combn(unique(tag_vect), m=2)[2,combo]
  an_A = an %>% filter(tag_serial_number == tagA)
  an_B = an %>% filter(tag_serial_number == tagB)
  
  df_combo = df %>% 
    filter(tag_serial_number %in% c(tagA, tagB)) %>%
    filter(date_time >= an_A$release_date_time & 
             date_time >= an_B$release_date_time &
             date_time <= an_A$battery_estimated_end_date & 
             date_time <= an_B$battery_estimated_end_date)
  
  df_combo_sum = df_combo %>% 
    group_by(tag_serial_number) %>% 
    summarise(n_hours = length(unique(date_hour)))
  
  
  if(an_A$battery_estimated_end_date < an_B$release_date_time | 
     an_B$battery_estimated_end_date < an_A$release_date_time |
     any(df_combo_sum$n_hours < 10) |
     nrow(df_combo) == 0) {
    
    df_result = tibble(
      tagA = tagA,
      tagB = tagB,
      SRI = NA,
      cooc_hours = NA,
      total_hours = NA,
      hours_at_large = NA
    )
  } else {

    total_hours = length(unique(df_combo$date_hour))
    
    cooc_hours = df_combo %>% 
      group_by(date_hour, station_name) %>% 
      summarise(n_ind = length(unique(tag_serial_number))) %>% 
      filter(n_ind > 1) %>% 
      nrow() %>% 
      as.numeric()
    
    SRI = cooc_hours/total_hours
    
    hours_at_large = round(as.numeric(difftime(time1 = min(an_A$battery_estimated_end_date,
                                                           an_B$battery_estimated_end_date),
                                               time2 = max(an_A$release_date_time, 
                                                           an_B$release_date_time),
                                               units = "hours" )), 0)
    
    df_result = tibble(
      tagA = tagA,
      tagB = tagB,
      SRI = SRI,
      cooc_hours = cooc_hours,
      total_hours = total_hours,
      hours_at_large = hours_at_large
    )
  }
  
  return(df_result)
})

df_result = plyr::ldply(list_result)
df_result = df_result %>% as_tibble()

# Retain dyads that had at least 28 combined days at large
df_result = df_result %>% filter(hours_at_large >= 28*24)

#### Organize data with combination type ####
# Add release location
an_A = an  %>% select(tagA = tag_serial_number, releaseA = station_group)
an_B = an %>% select(tagB = tag_serial_number, releaseB = station_group)

df_result = df_result %>% 
  left_join(an_A) %>% 
  left_join(an_B) 

levels_tags = an$tag_serial_number
df_result = df_result %>% 
  mutate(tagA = factor(tagA, levels = levels_tags)) %>% 
  mutate(tagB = factor(tagB, levels = rev(levels_tags)))%>% 
  mutate(combo_type = ifelse(releaseA != releaseB, "Different", unique(releaseA,releaseB)))


#### Summarise ####
df_result_sum = df_result  %>% 
  group_by(combo_type) %>% 
  summarise(SRI_median = median(SRI, na.rm = T),
            SRI_min = min(SRI, na.rm = T),
            SRI_max = max(SRI, na.rm = T),
            n_dyads = sum(SRI>0, na.rm = T),
            n_total = n()) %>% 
  mutate(dyad_perc = n_dyads/n_total) %>% 
  mutate(SRI = paste0(round(SRI_median,2) , " [", 
                      round(SRI_min,2), "-",
                      round(SRI_max,2), "]"),
         dyad_perc = paste0(n_dyads, "/", n_total, " (", round(dyad_perc,2)*100, "%)")) %>% 
  select(combo_type, dyad_perc, SRI)


#### Plot co-occurrence matrix? ####
df_result %>% 
  # ggplot(aes(tagA,tagB,fill = releaseB, colour = releaseA, alpha = SRI)) +
  ggplot(aes(tagA,tagB,fill = stratB, colour = stratA, alpha = SRI)) +
  geom_tile() +
  geom_text(aes(label = round(SRI,2)), colour = "black") +
  scale_alpha_continuous(range = c(0.7, 0.8), na.value = 0)
