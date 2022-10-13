##############################
# Investigate diel patterns  #
##############################

# Jolien Goossens - Marine Biology Research Group, Ghent University (Marbiol) / Flanders Marine Institute (VLIZ)
# R version 3.6.2


#### Run read_and_format.R ####
source("src/backfun/read_and_format.R")
df = df %>% filter(acoustic_project_code == "bpns") %>% filter(station_group != "Other")

#### Get sunset and sunrise ####
pot_dates = seq.Date(from = min(date(df$date)), 
                     to = max(date(df$date)), by = "day")
sunset = suncalc::getSunlightTimes(date = pot_dates, 
                                   lat = df$deploy_latitude[1], lon = df$deploy_longitude[1], 
                                   keep = c("sunrise", "sunset"))
sunset = sunset %>% mutate(date = parse_date_time(date, orders ="ymd"))

# Set sunrise and sunset date to fixed date (of your choosing: e.g. bread and roses) for plotting purposes
date(sunset$sunrise) = "1908-03-08"
date(sunset$sunset) = "1908-03-08"

#### Summarise dataset for tiling ####
df_tile <- df %>% 
  group_by(station_group, station_name, date, date_hour) %>% 
  summarise(n_an = length(unique(tag_serial_number))) 
date(df_tile$date_hour) = "1908-03-08"

df_tile = deploy_zeeb %>% 
  select(station_name, label_name) %>% 
  right_join(df_tile)

#### Make plot per area ####
lapply(unique(df_tile$station_group), function(stat_i){
  p_dn = df_tile %>% 
    mutate(n_an = ifelse(n_an >6, 6, n_an)) %>% 
    filter(station_group == stat_i) %>% 
    ggplot() +
    theme_bw() +
    theme(legend.direction = "horizontal",
          legend.position = "top",
          legend.title = element_blank()) +
    geom_tile(aes(x = date_hour, y = date, fill = label_name, alpha = n_an)) +
    scale_alpha(range = c(0.4,1), limits = c(1,6)) +
    geom_path(data = sunset, aes(x = sunrise, y = date)) +
    geom_path(data = sunset, aes(x = sunset, y = date)) +
    scale_y_datetime(expand = c(0,0)) +
    scale_x_datetime(expand = c(-0.01,-0.01),
                     date_breaks = '6 hours', date_labels = '%H:%M',
                     limits = as.POSIXct(c("1908-03-08 00:00:00", "1908-03-09 00:00:00")))+
    guides(alpha = "none") +
    labs(x ="", y = "")
  ggsave(filename = paste0("reports/figures/Fig_Annex_daynight", stat_i, ".jpg"), 
         plot = p_dn,   
         scale = 1, dpi = 600, width = 14, height = 14, units = "cm")
})
