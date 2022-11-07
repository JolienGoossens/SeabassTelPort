#######################################
# Investigate diel patterns Vandamme  #
#######################################

# Jolien Goossens - Marine Biology Research Group, Ghent University (Marbiol) / Flanders Marine Institute (VLIZ)
# R version 3.6.2


#### Run read_and_format.R ####
source("src/backfun/read_and_format.R")
library(lme4)
library(afex)

#### Make subset ####
# Get animals detected in Vandamme
tag_vect = df %>% 
  filter(station_name  == "bpns-vandamme") %>% 
  distinct(tag_serial_number) %>% 
  pull()

df %>% 
  filter(tag_serial_number %in% tag_vect) %>% 
  filter(acoustic_project_code == "bpns" & station_group == "Zeebrugge_inner") %>% 
  group_by(station_group, station_name) %>% 
  summarise(n_an = length(unique(tag_serial_number)),
            n = n())
# Only use Vandamme and Boudewijn

df %>% 
  filter(tag_serial_number %in% tag_vect) %>% 
  filter(station_name %in% c("bpns-vandamme", "bpns-boudewijn")) %>% 
  group_by(sensor_type) %>% 
  summarise(n = n(),
            n_an = length(unique(tag_serial_number)),
            n_hour = length(unique(date_hour)))

df %>% 
  filter(tag_serial_number %in% tag_vect) %>% 
  filter(station_name %in% c("bpns-vandamme", "bpns-boudewijn")) %>% 
  filter(sensor_type == "P") %>% 
  ggplot(aes(station_name, -sensor_value_calc)) +
  geom_boxplot()

df %>% 
  filter(tag_serial_number %in% tag_vect) %>% 
  filter(station_name %in% c("bpns-vandamme", "bpns-boudewijn")) %>% 
  filter(sensor_type == "A") %>% 
  ggplot(aes(station_name, sensor_value_calc)) +
  geom_boxplot()

  
# Make tibble depth 
dfdn = df %>% 
  filter(tag_serial_number %in% tag_vect) %>% 
  filter(station_name %in% c("bpns-vandamme", "bpns-boudewijn")) %>% 
  filter(sensor_type == "P") %>% 
  group_by(station_name, tag_serial_number, dn, date_hour) %>% 
  arrange(date_time) %>% 
  mutate(depth_diff = sensor_value_calc - lag(sensor_value_calc)) %>% 
  summarise(n= n(),
            depth_diff = sum(abs(depth_diff), na.rm = T),
            depth_diff_n = depth_diff/(n-1))  %>% 
  filter(n > 3) %>% 
  ungroup()

dfdn = dfdn %>% 
  group_by(station_name, tag_serial_number, dn) %>% 
  summarise(depth_diff_mean = mean(depth_diff_n))
  
# Add label_name for plot
dfdn = deploy_zeeb %>% 
  select(station_name, label_name) %>% 
  right_join(dfdn)


#### Boxplot ####
p_vandamme_depth = dfdn %>% 
  ggplot(aes(dn, depth_diff_mean)) +
  theme_classic() +
  theme(axis.line.x = element_blank(),
        axis.ticks.x = element_blank()) +
  geom_boxplot(aes(fill = dn), colour = "#c75133", lwd = 0.8) +
  geom_jitter(aes(fill = dn), colour = "#c75133", shape = 21) +
  scale_fill_manual(values = c("#94d7f1", "#1d2855")) +
  scale_y_continuous(expand = c(0.01,0)) +
  labs(x="", y = "Standardized distance travelled per fish (m)") +
  guides(fill = "none") +
  facet_wrap(~label_name)

ggsave(filename = "reports/figures/Fig4_Vandamme_dn_depth.jpg", 
       plot = p_vandamme_depth,   
       scale = 1, dpi = 600, width = 10, height = 10, units = "cm")

#### Model ####
# Plot distribution
dfdn %>% 
  ggplot(aes(depth_diff_mean)) +
  theme_bw() +
  geom_density() +
  facet_wrap(label_name~dn, scales ="free")
dfdn %>% 
  ggplot(aes(log(depth_diff_mean))) +
  theme_bw() +
  geom_density() +
  facet_wrap(label_name~dn, scales ="free")
# Close to normal distribution

# Format
dfdn = dfdn %>% 
  mutate(fid = as.factor(tag_serial_number),
         fdn = as.factor(dn))

# Model
model_dn = lmer(log(depth_diff_mean) ~ fdn*station_name + (1|fid), data = dfdn)
model_dn = lmer(log(depth_diff_mean) ~ fdn+station_name + (1|fid), data = dfdn)
model_dn = lmer(depth_diff_mean ~ fdn*station_name + (1|fid), data = dfdn)
summary(model_dn)
model_dn = lmer(depth_diff_mean ~ fdn+station_name + (1|fid), data = dfdn)
summary(model_dn)


# Model selection
drop1(model_dn, test = "Chisq")
M2 = glm(n_hour ~ fdn*station_name, data = dfdn, family = poisson)
anova(model_dn, M2)


# Plot residuals
p_pearson = dfdn %>% 
  mutate(pred = fitted(model_dn),
         pear = residuals(model_dn, "pearson")) %>% 
  ggplot(aes(pred, pear))+
  theme_bw()+ 
  geom_point(aes(fill = dn), shape = 21) +
  scale_x_continuous(expand = c(0.01,0.01)) +
  scale_fill_manual(values = c("#94d7f1", "#1d2855")) +
  stat_smooth(method = "lm", colour = "black") +
  # facet_wrap(~label_name, scales = "free_x") +
  guides(fill = "none") +
  labs(x = "Fitted values", 
       y = "Pearson residuals")
ggsave(filename = "reports/figures/Fig_Annex_ModelVandamme_depth_pearson.jpg", 
       plot = p_pearson,   
       scale = 1, dpi = 600, width = 10, height = 10, units = "cm")
# Potential issue with larger values: just to check: remove animals with outliers






#### Make subset ####

# Make tibble acceleration 
dfdn = df %>% 
  filter(tag_serial_number %in% tag_vect) %>% 
  filter(station_name %in% c("bpns-vandamme", "bpns-boudewijn")) %>% 
  filter(sensor_type == "A") %>% 
  group_by(station_name, tag_serial_number, dn) %>% 
  summarise(n= n(),
            acc_mean = mean(sensor_value_calc, na.rm = T),
            acc_median = median(sensor_value_calc, na.rm = T))  %>% 
  filter(n > 3) %>% 
  ungroup()

# Add label_name for plot
dfdn = deploy_zeeb %>% 
  select(station_name, label_name) %>% 
  right_join(dfdn)


#### Boxplot ####
p_vandamme_acc = dfdn %>% 
  ggplot(aes(dn, acc_mean)) +
  theme_classic() +
  theme(axis.line.x = element_blank(),
        axis.ticks.x = element_blank()) +
  geom_boxplot(aes(fill = dn), colour = "#c75133", lwd = 0.8) +
  geom_jitter(aes(fill = dn), colour = "#c75133", shape = 21) +
  scale_fill_manual(values = c("#94d7f1", "#1d2855")) +
  scale_y_continuous(expand = c(0.01,0)) +
  labs(x="", y = "Standardized distance travelled per fish (m)") +
  guides(fill = "none") +
  facet_wrap(~label_name)

library(patchwork)
p_vandamme_depth / p_vandamme_acc

ggsave(filename = "reports/figures/Fig4_Vandamme_dn_depth.jpg", 
       plot = p_vandamme_depth,   
       scale = 1, dpi = 600, width = 10, height = 10, units = "cm")

#### Model ####
# Plot distribution
dfdn %>% 
  ggplot(aes(acc_mean)) +
  theme_bw() +
  geom_density() +
  facet_wrap(label_name~dn, scales ="free")
dfdn %>% 
  ggplot(aes(log(acc_mean))) +
  theme_bw() +
  geom_density() +
  facet_wrap(label_name~dn, scales ="free")
# Close to normal distribution

# Format
dfdn = dfdn %>% 
  mutate(fid = as.factor(tag_serial_number),
         fdn = as.factor(dn))

# Model
model_dn = lmer(log(depth_diff_mean) ~ fdn*station_name + (1|fid), data = dfdn)
model_dn = lmer(log(depth_diff_mean) ~ fdn+station_name + (1|fid), data = dfdn)
model_dn = lmer(acc_mean ~ fdn*station_name + (1|fid), data = dfdn)
summary(model_dn)
model_dn = lmer(acc_mean ~ fdn+station_name + (1|fid), data = dfdn)
summary(model_dn)


# Model selection
drop1(model_dn, test = "Chisq")
M2 = glm(n_hour ~ fdn*station_name, data = dfdn, family = poisson)
anova(model_dn, M2)


# Plot residuals
p_pearson = dfdn %>% 
  mutate(pred = fitted(model_dn),
         pear = residuals(model_dn, "pearson")) %>% 
  ggplot(aes(pred, pear))+
  theme_bw()+ 
  geom_point(aes(fill = dn), shape = 21) +
  scale_x_continuous(expand = c(0.01,0.01)) +
  scale_fill_manual(values = c("#94d7f1", "#1d2855")) +
  stat_smooth(method = "lm", colour = "black") +
  # facet_wrap(~label_name, scales = "free_x") +
  guides(fill = "none") +
  labs(x = "Fitted values", 
       y = "Pearson residuals")
ggsave(filename = "reports/figures/Fig_Annex_ModelVandamme_depth_pearson.jpg", 
       plot = p_pearson,   
       scale = 1, dpi = 600, width = 10, height = 10, units = "cm")
# Potential issue with larger values: just to check: remove animals with outliers

