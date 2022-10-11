##########################
# Figure 2 - Abacus plot #
##########################

# Jolien Goossens - Marine Biology Research Group, Ghent University (Marbiol) / Flanders Marine Institute (VLIZ)
# R version 3.6.2


#### Run read_and_format.R ####
source("src/backfun/read_and_format.R")
Sys.setlocale("LC_ALL", "English")

#### Make data frame with all possible dates for each fish ####
list_pot <- lapply(unique(an$animal_id), function(animal_id_i) {
  df_temp <- df %>% filter(animal_id == animal_id_i)
  an_temp <- an %>% filter(animal_id == animal_id_i)
  
  # Get potential dates
  if (is.na(an_temp$recapture_date_time) | 
      an_temp$recapture_date_time > an_temp$battery_estimated_end_date) {
    pot_dates = seq.Date(from = date(an_temp$capture_date_time), 
                         to = date(an_temp$battery_estimated_end_date), by = "day")
  } else {
    pot_dates = seq.Date(from = date(an_temp$capture_date_time), 
                         to = date(an_temp$recapture_date_time), by = "day")
  }
  
  
  # Put potential dates in a dataframe
  df_pot = data.frame(
    animal_id = animal_id_i,
    date = parse_date_time(pot_dates, orders = "ymd"))
  
  # Link potential dates dataframe with known dates
  df_pot = df_temp %>% 
    arrange(date_time) %>% 
    group_by(date) %>% 
    summarise(station_name_first = first(station_name),
              station_group_first = first(station_group),
              station_name_last = last(station_name),
              station_group_last = last(station_group)) %>% 
    ungroup() %>% 
    mutate(station_group = "Known") %>% 
    right_join(df_pot) %>% 
    arrange(date)
  
  # If not detected first day: change station_group_last to tagging station group
  df_pot = df_pot %>% mutate(
    station_group_last = ifelse(row_number() == 1 & is.na(station_group_last), an_temp$station_group, station_group_last),
    station_name_last = ifelse(row_number() == 1 & is.na(station_name_last), an_temp$release_location, station_name_last),
    station_group = ifelse(row_number() == 1 & is.na(station_group), "Known", station_group))
  
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
        # is.na(station_group) & station_group_last == "Zeebrugge_inner" & station_name_first == "bpns-boudewijn" ~ "Zeebrugge_inner",
        is.na(station_group) & station_group_last == "Zeebrugge_inner" & station_group_first == "Zeebrugge_inner" ~ "Zeebrugge_inner",
        is.na(station_group) & station_group_last %in% c("Zeebrugge_out", "Other", "Zeebrugge_port") ~ "Other",
        is.na(station_group) & is.na(station_group_first) & station_group_last == "Zeebrugge_inner" ~ "Zeebrugge_inner",
        is.na(station_group) & is.na(station_group_first) & station_group_last %in% c("Zeebrugge_out", "Other", "Zeebrugge_port") ~ "Other",
        TRUE ~ station_group))
  return(df_pot)
})

df_pot = plyr::ldply(list_pot)
df_pot = df_pot %>% 
  mutate(qual = ifelse(station_group == "Known", "det", "presumed")) %>% 
  as_tibble()


#### Format for plot ####
# Set levels for order in plot
levels_animal_id = df_pot %>% 
  group_by(animal_id) %>% 
  summarise(early = min(date), late = max(date)) %>% 
  left_join(select(an, animal_id, station_group)) %>% 
  arrange(station_group, early, late) %>% 
  pull(animal_id) %>% 
  rev() %>% 
  as.factor()

# Rename
data_det = df
data_an = an

# Add animal id for CA outliers
data_an <- data_an %>% 
  mutate(label_ca = ifelse(animal_id %in% c(9089, 7179, 3511),
                           animal_id, ""))%>%
  mutate(animal_id = factor(animal_id, levels = levels_animal_id)) %>% 
  arrange(animal_id)  

# Set factor levels
data_det <- data_det %>% 
  mutate(animal_id = factor(animal_id, levels = levels_animal_id)) %>% 
  arrange(animal_id)

df_pot = df_pot %>% 
  mutate(animal_id = factor(animal_id, levels = levels_animal_id)) %>% 
  arrange(animal_id)

# Add release group
data_an = data_an %>% 
  mutate(station_group_release = station_group) %>% 
  mutate(station_group_release = factor(station_group_release, 
                                        levels = c("Zeebrugge_out", "Zeebrugge_inner")))
data_det = data_det %>% left_join(select(data_an, animal_id, station_group_release))
df_pot = df_pot %>% left_join(select(data_an, animal_id, station_group_release))

# Add final date to an_data
data_an = df_pot %>% 
  group_by(animal_id) %>% 
  summarise(end_date = max(date)) %>% 
  left_join(data_an)

data_an = data_det%>% 
  filter(acoustic_project_code != "recapture") %>% 
  group_by(animal_id) %>% 
  summarise(end_date_det = max(date)) %>% 
  left_join(data_an) 

data_an = data_an %>% 
  mutate(end_date = data.table::fifelse(end_date_det > end_date, end_date_det, end_date))

#### Plot ####
# Set new year
newyear = lubridate::parse_date_time(str_c(unique(lubridate::year(data_det$date_time)), "-1-1"),
                                     orders = "ymd")

# Settings
shape_det = 15
size_det = 2
shape_release = 23
size_release = 3


# Plot
abacus_plot = ggplot() +
  theme_bw() +
  theme(panel.background = element_rect(fill = 'gray98'),
        axis.ticks.y=element_blank(),
        axis.text.x=element_text(hjust=0, size = 12),
        axis.title = element_blank(),
        legend.position = 'none',
        axis.text.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank()) +
  geom_point(data = data_an, 
             aes(release_date_time, animal_id, fill = station_group), size = 0.1) +
  geom_vline(xintercept = newyear, size = 0.1, colour = "gray50") +
  geom_point(data = data_det, 
             aes(date_time, animal_id, colour = station_group), 
             shape = shape_det, size = size_det) +
  geom_point(data = data_an, 
             aes(release_date_time, animal_id, fill = station_group),
             shape = shape_release, size = size_release) +
  geom_point(data = filter(data_an, !is.na(recapture_date_time)), 
             aes(recapture_date_time, animal_id),
             shape = 9, size = size_release) +
  scale_fill_manual(values = cols) +
  scale_colour_manual(values = cols) +
  geom_point(data = filter(df_pot, station_group != "Known"), 
             aes(date, as.factor(animal_id), colour = station_group), alpha = 0.1, size = 1) +
  geom_point(data = data_an, 
             aes(release_date_time, animal_id, fill = station_group),
             shape = shape_release, size = size_release) +
  geom_text(data = data_an,
            aes(release_date_time - days(10),
                animal_id, label = label_ca,
                colour = station_group),
            hjust = 1) +
  # geom_text(data = data_an,
  #           aes(end_date + days(10), animal_id,
  #               label = format(length1, nsmall = 1),
  #               colour = station_group),
  #           hjust = 0) +
  theme(panel.grid.minor = element_blank()) +
  scale_x_datetime(limits = c(parse_date_time("2018-07-01", "ymd"),
                              parse_date_time("2022-03-01", "ymd")),expand = c(0.05,0),
                   date_breaks = "3 month", date_labels = "%b", position = "top",
                   sec.axis = sec_axis(~.)) +
  facet_grid(station_group_release~., scales = "free_y", space = "free")
abacus_plot


#### Save plot ####
ggsave(filename = "reports/figures/Fig2_Abacus.jpg", 
       plot = abacus_plot,   
       scale = 1, dpi = 600, width = 25, height = 15, units = "cm")
