#########################
# Format presence data  #
#########################

# Jolien Goossens - Marine Biology Research Group, Ghent University (Marbiol) / Flanders Marine Institute (VLIZ)
# R version 3.6.2


#### Read data and packages ####
source("src/backfun/read_and_format.R")
library(lme4)
library(patchwork)

df_pres = read_csv("data/interim/df_pres.csv")
df_pot = read_csv("data/interim/df_pot_hour.csv")

#### Formatting ####
# Animal data
an_release = an %>% 
  mutate(tagging_group = ifelse(station_group == "Zeebrugge_inner","inner", "outer")) %>% 
  select(animal_id, tagging_group) 

# Join detection data with hourly epistemic data
df_pres = df_pres %>% 
  left_join(df_pot)

# Join with deployment metadata
df_pres = deploy_zeeb %>% 
  select(station_name, station_group) %>% 
  right_join(df_pres)


#### Create protection scenarios ####
df_scenarios = tibble(
  scenario = c(1:7),
  scenario_name = c(
    "Maritime security measures",
    "Current regulation",
    "Current regulation without seasonal closure EU",
    "Current regulation without seasonal closure rivers",
    "Current regulation without spatial closure sluice",
    "Current regulation without spatial closure sluice, without fishing at night",
    "Current regulation, without fishing at night"),
  prot_EU = c(         FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE),
  prot_ANB_janjune = c(FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE),
  prot_ANB_vandamme = c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE),
  prot_night = c(     FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE)
)


#### Set up data frame with only the maritime access & port authorities measures ####
# Define stations where no fishing is allowed
prot_stations_security = c("bpns-ZAND4", "bpns-ZW1", "bpns-ZOKN",
                           "bpns-LNG", "bpns-ZA2",   
                           "bpns-zbw1", "bpns-zbw2", "bpns-zbw3", 
                           "bpns-zbe1", "bpns-zbe2", "bpns-zbe3",
                           "bpns-visart" , "bpns-visartboudewijn")

# Define for each hour whether the animal was protected or not
df_pres = df_pres %>%
  mutate(prot = ifelse(test = (det_bin == 1 & 
                                 station_name %in% prot_stations_security),
                       yes = "Not exposed", 
                       no = "Exposed"))

# Save this data frame as baseline  
df_maritime = df_pres

#### Model selection and validation (including epistemic locations) ####
df_scenarios_output_model = lapply(unique(df_scenarios$scenario), function(scenario_id) {
  df_scenario = df_scenarios %>% filter(scenario == scenario_id)
  
  df_pres = df_maritime
  
  if (df_scenario$prot_EU) {
    df_pres = df_pres %>%
      mutate(prot = ifelse(test = ((station_group_epist == "Other" & 
                                      time_month %in% c(2,3) & year(date) %in% c(2018, 2019)) |
                                     (station_group_epist == "Other" & time_month == 2 & 
                                        year(date) %in% c(2020, 2021, 2022))),
                           yes = "Not exposed", 
                           no = prot))
    
    # df_pres = df_pres %>%
    #   mutate(prot = ifelse(test = (station_group_epist == "Other" & 
    #                                   time_month %in% c(2,3)),
    #                        yes = "Not exposed", 
    #                        no = prot))
  }         
  
  if (df_scenario$prot_ANB_janjune) {
    df_pres = df_pres %>%
      mutate(prot = ifelse(test = (det_bin == 1 & 
                                     station_group == "Zeebrugge_inner" & 
                                     time_month %in% c(1:6)) |
                             (station_group_epist == "Zeebrugge_inner" & 
                                time_month %in% c(1:6)),
                           yes = "Not exposed", 
                           no = prot))
  }
  
  
  if (df_scenario$prot_ANB_vandamme) {
    df_pres = df_pres %>%
      mutate(prot = ifelse(test = (det_bin == 1 & 
                                     station_name == "bpns-vandamme"),
                           yes = "Not exposed", 
                           no = prot))
  } 
  
  if (df_scenario$prot_night) {
    df_pres = df_pres %>%
      mutate(prot = ifelse(test = (det_bin == 1 & 
                                     station_group == "Zeebrugge_inner" &
                                     dn == "Night") |
                             (station_group_epist == "Zeebrugge_inner" & 
                                dn == "Night"),
                           yes = "Not exposed", 
                           no = prot))
  }
  
  # Format for modelling
  df_prot = df_pres %>% 
    group_by(animal_id, time_month, date_hour) %>% 
    summarise(prot = ifelse("Not exposed" %in% prot, "Not exposed", "Exposed"))
  
  df_prot = df_prot %>% 
    mutate(prot_bin = ifelse(prot == "Not exposed", 1, 0)) %>% 
    group_by(animal_id, time_month) %>% 
    summarise(prot_bin = sum(prot_bin),
              n = n()) %>% 
    mutate(prot_perc = prot_bin / n)
  
  df_prot = df_prot %>% 
    left_join(an_release)
  
  df_protmodel = df_prot %>% 
    mutate(fid = factor(animal_id),
           fmonth = factor(time_month),
           fgroup = factor(tagging_group)) %>% 
    ungroup() %>% 
    select(fid, fmonth,fgroup, prot_bin, n)
  
  # Model
  M1 = glmer(cbind(prot_bin, n - prot_bin) ~  fgroup+ (1|fgroup:fid) + (1|fmonth), 
                   family =binomial(link = "logit"), data = df_protmodel)
  M2 = glmer(cbind(prot_bin, n - prot_bin) ~  fgroup + (1|fmonth), 
             family =binomial(link = "logit"), data = df_protmodel)
  M3 = glmer(cbind(prot_bin, n - prot_bin) ~  fgroup + (1|fgroup:fid), 
             family =binomial(link = "logit"), data = df_protmodel)
  M4 = glmer(cbind(prot_bin, n - prot_bin) ~  (1|fgroup:fid) + (1|fmonth), 
             family =binomial(link = "logit"), data = df_protmodel)
  
  anova(M1, M2)$'Pr(>Chisq)'[2]
  anova(M1, M3)$'Pr(>Chisq)'[2]
  anova(M1, M4)$'Pr(>Chisq)'[2]
  
  df_model_selection = tibble(
    model = c("M1", "M2", "M3", "M4"),
    AIC = c(summary(M1)$AIC[1], summary(M2)$AIC[1], summary(M3)$AIC[1], summary(M4)$AIC[1]),
    M1_p = c(NA, anova(M1, M2)$'Pr(>Chisq)'[2], 
             anova(M1, M3)$'Pr(>Chisq)'[2], 
             anova(M1, M4)$'Pr(>Chisq)'[2])
  )
  df_model_selection$scenario = df_scenario$scenario
  df_model_selection$scenario_name = df_scenario$scenario_name
  
  df_protmodel = df_protmodel %>% 
    mutate(pred = predict(M1, newdata = df_protmodel, type = "response"),
           pred_fit = fitted(M1),
           resid_pear = resid(M1, type = "pearson")) %>% 
    as_tibble()
  
  p1 = df_protmodel %>% 
    ggplot() +
    theme_bw() +
    geom_point(aes(pred_fit, resid_pear), size = 1) +
    ggtitle(df_scenario$scenario_name)
  p2 = df_protmodel %>% 
    ggplot() +
    theme_bw() +
    geom_boxplot(aes(fgroup, resid_pear))
  pcombo = p1 / p2 
  
  ggsave(filename = paste0("reports/figures/Validation/M1_scenario", 
                           scenario_id, ".jpg"), 
         plot = pcombo,   
         scale = 1, dpi = 600, width = 10, height = 10, units = "cm")
  
  return(df_model_selection)
})

df_scenarios_output_modeldf = plyr::ldply(df_scenarios_output_model)
# All models have M1 as best model

write_csv(df_scenarios_output_modeldf, "data/processed/df_scenarios_output_modeldf.csv")

#### Predict protection (including epistemic locations) ####
df_scenarios_output = lapply(unique(df_scenarios$scenario), function(scenario_id) {
  df_scenario = df_scenarios %>% filter(scenario == scenario_id)
  
  df_pres = df_maritime
  
  if (df_scenario$prot_EU) {
    df_pres = df_pres %>%
      mutate(prot = ifelse(test = ((station_group_epist == "Other" & 
                                      time_month %in% c(2,3) & year(date) %in% c(2018, 2019)) |
                                     (station_group_epist == "Other" & time_month == 2 & 
                                        year(date) %in% c(2020, 2021, 2022))),
                           yes = "Not exposed", 
                           no = prot))
    
    # df_pres = df_pres %>%
    #   mutate(prot = ifelse(test = (station_group_epist == "Other" & 
    #                                   time_month %in% c(2,3)),
    #                        yes = "Not exposed", 
    #                        no = prot))
  }         
  
  if (df_scenario$prot_ANB_janjune) {
    df_pres = df_pres %>%
      mutate(prot = ifelse(test = (det_bin == 1 & 
                                     station_group == "Zeebrugge_inner" & 
                                     time_month %in% c(1:6)) |
                             (station_group_epist == "Zeebrugge_inner" & 
                                time_month %in% c(1:6)),
                           yes = "Not exposed", 
                           no = prot))
  }
  
  
  if (df_scenario$prot_ANB_vandamme) {
    df_pres = df_pres %>%
      mutate(prot = ifelse(test = (det_bin == 1 & 
                                     station_name == "bpns-vandamme"),
                           yes = "Not exposed", 
                           no = prot))
  } 
  
  if (df_scenario$prot_night) {
    df_pres = df_pres %>%
      mutate(prot = ifelse(test = (det_bin == 1 & 
                                     station_group == "Zeebrugge_inner" &
                                     dn == "Night") |
                             (station_group_epist == "Zeebrugge_inner" & 
                                dn == "Night"),
                           yes = "Not exposed", 
                           no = prot))
  }
  
  # Format for modelling
  df_prot = df_pres %>% 
    group_by(animal_id, time_month, date_hour) %>% 
    summarise(prot = ifelse("Not exposed" %in% prot, "Not exposed", "Exposed"))
  
  df_prot = df_prot %>% 
    mutate(prot_bin = ifelse(prot == "Not exposed", 1, 0)) %>% 
    group_by(animal_id, time_month) %>% 
    summarise(prot_bin = sum(prot_bin),
              n = n()) %>% 
    mutate(prot_perc = prot_bin / n)
  
  df_prot = df_prot %>% 
    left_join(an_release)
  
  df_protmodel = df_prot %>% 
    mutate(fid = factor(animal_id),
           fmonth = factor(time_month),
           fgroup = factor(tagging_group)) %>% 
    ungroup() %>% 
    select(fid, fmonth,fgroup, prot_bin, n)
  
  # Model
  M1 = glmer(cbind(prot_bin, n - prot_bin) ~  fgroup+ (1|fgroup:fid) + (1|fmonth), 
             family =binomial(link = "logit"), data = df_protmodel)  
  
  p_value = summary(M1)$coefficients[2,4]
 
  df_protmodel = df_protmodel %>% 
    mutate(pred = predict(M1, newdata = df_protmodel, type = "response"))
  
  plot_scenario = df_protmodel %>% 
    ggplot(aes(x = fmonth, y = pred, fill = fgroup)) +
    theme_bw() +
    theme(legend.position = "none") +
    geom_boxplot(alpha= 0.8) +
    geom_point(shape = 21, position=position_jitterdodge()) + 
    scale_fill_manual(values = col_tagging) +
    scale_y_continuous(limits = c(0,1)) +
    labs(x = "Month", y = expression(pi)) +
    ggtitle(df_scenario$scenario_name)
  
  ggsave(filename = paste0("reports/figures/Fig6_protmodel_scenario", scenario_id, ".jpg"), 
         plot = plot_scenario,   
         scale = 1, dpi = 600, width = 18, height = 11, units = "cm")
  
  pred_output = df_protmodel %>% group_by(fgroup, fid) %>% 
    summarise(pred = mean(pred)) %>% 
    group_by(fgroup) %>% 
    summarise(med = round(median(pred), 3), 
              q5 = round(quantile(pred, 0.025), 3),
              q95 = round(quantile(pred, 0.975), 3)) %>% 
    mutate(pred = paste0(med, " [", q5, "-", q95, "]")) %>% 
    select(fgroup, pred) %>% 
    spread(fgroup, pred)
  
  df_scenario = cbind(df_scenario, pred_output)
  df_scenario$p_value = p_value
  return(df_scenario)
})

df_scenarios_outputdf = plyr::ldply(df_scenarios_output)

#### Save results df ####
write_csv(df_scenarios_outputdf, "data/processed/model_output.csv")

