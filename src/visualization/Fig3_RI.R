###############################
# Figure 3 - Residence Index  #
###############################

# Jolien Goossens - Marine Biology Research Group, Ghent University (Marbiol) / Flanders Marine Institute (VLIZ)
# R version 3.6.2


#### Run read_and_format.R ####
source("src/backfun/read_and_format.R")

#### Filter for port data ####
df <- df %>% filter(station_group != "Other") %>% filter(acoustic_project_code == "bpns")
df <- df %>% mutate(station_group2 = ifelse(station_group == "Zeebrugge_inner", "Inner", "Outer"))


#### Prepare monthly residency: potential days within month for each animal #### 
an_pot = an %>% 
  mutate(end_date = data.table::fifelse(is.na(recapture_date_time) |
                                          recapture_date_time > battery_estimated_end_date,
                                        battery_estimated_end_date, 
                                        date(recapture_date_time))) %>% 
  mutate(release_date = parse_date_time(date(release_date_time), orders = "ymd")) %>% 
  select(animal_id, release_date, end_date)

list_month_seq = lapply(unique(an_pot$animal_id), function(animal_id_i){
  an_pot = an_pot %>% filter(animal_id == animal_id_i)
  month_seq = seq(
    from = parse_date_time(paste(year(an_pot$release_date), 
                                 month(an_pot$release_date), "01", sep = "-"), 
                           orders = "ymd"),
    to = parse_date_time(paste(year(an_pot$end_date), 
                               month(an_pot$end_date), "01", sep = "-"), 
                         orders = "ymd"),
    by = "month")
  
  an_pot = data.frame(
    an_pot,
    month_seq = month_seq)
  return(an_pot) 
})

an_pot = plyr::ldply(list_month_seq)

an_pot = an_pot %>% 
  mutate(
    date_month = month(month_seq),
    date_year = year(month_seq),
    daysinmonth = lubridate::days_in_month(month_seq)
  ) %>% 
  mutate(
    daysinmonth = ifelse(
      month(release_date) == date_month & year(release_date) == date_year,
      daysinmonth - difftime(release_date, month_seq, units = "days"),
      ifelse(
        month(end_date) == date_month & year(end_date) == date_year,
        difftime(end_date, month_seq, units = "days") + 1,
        daysinmonth
      ))) %>% 
  mutate(hoursinmonth = daysinmonth *24)

an_pot = an_pot %>% select(animal_id, date_month, date_year, daysinmonth, hoursinmonth)


#### Monthly residency per station_group ####
res_month_ind_stat <- df %>% 
  mutate(
    date_month = month(date),
    date_year = year(date)) %>% 
  group_by(animal_id, station_group, date_month, date_year) %>% 
  summarise(
    n_detday = length(unique(date)),
    n_dethour = length(unique(date_hour)))

res_month_ind_stat = expand_grid(an_pot, station_group = unique(df$station_group)) %>%
  left_join(res_month_ind_stat)
res_month_ind_stat[is.na(res_month_ind_stat)] = 0

# Per year - month per individual
res_month_ind_stat = res_month_ind_stat %>% 
  arrange(animal_id, station_group, date_year, date_month) %>% 
  mutate(
    residency_day = n_detday / daysinmonth,
    residency_hour = n_dethour / hoursinmonth
  )

#### Calculate values to show on plot ####
res_month_stat <- res_month_ind_stat %>% 
  ungroup() %>% as.data.frame() %>% 
  group_by(station_group,date_month) %>% 
  summarise(
    residency_day_mean = mean(residency_day),
    residency_hr_mean= mean(residency_hour),
    residency_day_0.05 = quantile(residency_day, 0.025),
    residency_hour_0.05 = quantile(residency_hour, 0.025),
    residency_day_0.95 = quantile(residency_day, 0.975),
    residency_hour_0.95 = quantile(residency_hour, 0.975))


#### Make plot ####
levels_stat_group = c("Zeebrugge_out", "Zeebrugge_port", "Zeebrugge_inner")
res_plot = res_month_stat %>% 
  gather(residency, value, residency_day_mean:residency_hr_mean) %>% 
  mutate(station_group = factor(station_group, levels = levels_stat_group)) %>% 
  ggplot(aes(y = value, shape = residency, colour = station_group)) +
  theme_classic() +
  theme(legend.position = "none",
        strip.background = element_blank(),
        # axis.text= element_text(size = 7),
        # axis.title = element_text(size = 8),
        strip.text = element_blank()) +
  geom_point(aes(x = as.factor(date_month)), size = 2) +
  geom_ribbon(aes(x = as.integer(date_month),
                  ymin = residency_day_0.05, ymax = residency_day_0.95, fill = station_group), 
              alpha = 0.2)+geom_line(aes(x = as.integer(date_month))) +
  scale_colour_manual(values = cols) +
  scale_fill_manual(values = cols) +
  facet_grid(~station_group) +
  labs(x ="Month", y= "Daily / hourly residency")
res_plot

#### Save plot ####
ggsave(filename = "reports/figures/Fig3_RI.jpg", 
       plot = res_plot,   
       scale = 1.9, dpi = 600, width = 8.5, height = 4, units = "cm")
