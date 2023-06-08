#######################################
# Investigate diel patterns Vandamme  #
#######################################

# Jolien Goossens - Marine Biology Research Group, Ghent University (Marbiol) / Flanders Marine Institute (VLIZ)
# R version 3.6.2


#### Run read_and_format.R ####
source("src/backfun/read_and_format.R")
library(lme4)
library(afex)
library(patchwork)

#### Vandamme animals ####
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
            n_hour = length(unique(date_hour)),
            n = n())
# Only use Vandamme and Boudewijn
tag_vect_final = df %>% 
  filter(tag_serial_number %in% tag_vect) %>% 
  filter(station_name  == "bpns-boudewijn") %>% 
  distinct(tag_serial_number) %>% 
  pull()

tag_vect = tag_vect_final


#### Presence: make subset ####
# Presence
dfdn = df %>% 
  filter(tag_serial_number %in% tag_vect) %>% 
  filter(station_name %in% c("bpns-vandamme", "bpns-boudewijn"))   %>% 
  group_by(station_name, tag_serial_number, dn) %>% 
  summarise(n_hour = length(unique(date_hour))) %>% 
  # Filter for animals that were seen at least 10 hours in these stations
  group_by(tag_serial_number) %>% 
  filter(sum(n_hour) > 10) %>% 
  ungroup()

# Get zeros for fish that weren't seen at a station during day or night
dfdn = expand.grid(
  station_name = unique(dfdn$station_name),
  tag_serial_number = unique(dfdn$tag_serial_number),
  dn = unique(dfdn$dn)) %>% 
  left_join(dfdn) %>% 
  mutate(n_hour = replace_na(n_hour, 0))

# Add label_name for plot
dfdn = deploy_zeeb %>% 
  select(station_name, label_name) %>% 
  right_join(dfdn)

#### Presence: Boxplot ####
length(unique(dfdn$tag_serial_number))

dfdn %>%
  ggplot(aes(dn, n_hour)) +
  theme_classic() +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  geom_boxplot(aes(fill = dn), outlier.shape = NA, colour = "#c75133", lwd = 0.8) +
  geom_jitter(aes(fill = dn), colour = "#c75133", shape = 21) +
  scale_fill_manual(values = c("#94d7f1", "#1d2855")) +
  scale_y_continuous(expand = c(0.01,0)) +
  labs(x="", y = "Detected hour bins") +
  guides(fill = "none") +
  facet_wrap(~label_name)

#### Presence: Model ####
# Plot distribution
dfdn %>% 
  ggplot() +
  theme_bw() +
  geom_density(aes(n_hour)) +
  facet_wrap(label_name~dn, scales ="free")
# Seems to be Poisson distribution

# Format
dfdn = dfdn %>% 
  mutate(fid = as.factor(tag_serial_number),
         fdn = as.factor(dn))

# Model
M1 = glmer(n_hour ~ fdn*station_name + (1|fid), data = dfdn, family = poisson)
summary(M1) # All effects significant

# Model selection
drop1(M1, test = "Chisq")
M2 = glm(n_hour ~ fdn*station_name, data = dfdn, family = poisson)
anova(M1, M2)

# Final model
model_dn = M1

newdata = with(model_dn,
               expand.grid(fdn = unique(dfdn$fdn),
                           station_name = unique(dfdn$station_name)))
Xmat <- model.matrix(~fdn*station_name, newdata)
fixest <- fixef(model_dn)
fit <- as.vector(fixest %*% t(Xmat))
SE <- sqrt(diag(Xmat %*% vcov(model_dn) %*% t(Xmat)))
q <- qt(0.975, df=df.residual(model_dn))
linkinv <- family(model_dn)$linkinv
newdata <- cbind(newdata, fit=linkinv(fit),
                 lower=linkinv(fit-q*SE),
                 upper=linkinv(fit+q*SE))
newdata <- deploy_zeeb %>% 
  select(station_name, label_name) %>% 
  right_join(newdata)

p_pres_box = newdata %>% 
  ggplot(aes(fdn, fit)) +
  geom_jitter(data = dfdn, 
              aes(x = dn, y=  n_hour, fill = dn), 
              colour = "#c75133", shape = 21, alpha = 0.7) +
  geom_linerange(aes(ymin= lower, ymax = upper), colour = "#c75133", size = 2) + 
  geom_point(aes(fill= fdn), colour = "#c75133", shape = 22, size = 3) +
  theme_classic() +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  # geom_boxplot(aes(fill = dn), outlier.shape = NA, colour = "#c75133", lwd = 0.8) +
  
  scale_fill_manual(values = c("#94d7f1", "#1d2855")) +
  scale_y_continuous(expand = c(0.01,0)) +
  labs(x="", y = "Detected hour bins") +
  guides(fill = "none") +
  facet_wrap(~label_name)

# Plot residuals
p_pres_pearson = dfdn %>% 
  mutate(pred = fitted(model_dn),
         pear = residuals(model_dn, "pearson")) %>% 
  ggplot(aes(pred, pear))+
  theme_bw()+
  theme(legend.position = "top", legend.direction = "horizontal", 
        legend.title = element_blank()) +
  geom_point(aes(fill = dn, shape = label_name)) +
  scale_x_continuous(expand = c(0.01,0.01)) +
  scale_shape_manual(values = c(22, 23)) +
  scale_fill_manual(values = c("#94d7f1", "#1d2855")) +
  stat_smooth(method = "lm", colour = "black") +
  guides(fill = "none") +
  labs(x = "", 
       y = "Pearson residuals (Presence)")

# Potential issue with larger values: just to check: remove animals with outliers




#### Depth: make subset #### 
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

#### Depth: Boxplot ####
length(unique(dfdn$tag_serial_number))
dfdn %>% 
  ggplot(aes(dn, depth_diff_mean)) +
  theme_classic() +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        strip.text.x = element_blank()) +
  geom_boxplot(aes(fill = dn), outlier.shape = NA, colour = "#c75133", lwd = 0.8) +
  geom_jitter(aes(fill = dn), colour = "#c75133", shape = 21) +
  scale_fill_manual(values = c("#94d7f1", "#1d2855")) +
  scale_y_continuous(expand = c(0.01,0), limits = c(0, 4.5)) +
  labs(x="", y = "Vertical distance (m)") +
  guides(fill = "none") +
  facet_wrap(~label_name)

#### Depth: Model ####
# Plot distribution
dfdn %>% 
  ggplot(aes(depth_diff_mean)) +
  theme_bw() +
  geom_density() +
  facet_wrap(label_name~dn, scales ="free")
# Close to normal distribution

# Format
dfdn = dfdn %>% 
  mutate(fid = as.factor(tag_serial_number),
         fdn = as.factor(dn))

# Model
# M1 = lmer(depth_diff_mean ~ fdn*station_name + (1|fid), data = dfdn)
M1 = glmer(depth_diff_mean ~ fdn*station_name + (1|fid), data = dfdn, family = Gamma(link = "log"))
summary(M1)
# M2 = lmer(depth_diff_mean ~ fdn+station_name + (1|fid), data = dfdn)
M2 = glmer(depth_diff_mean ~ fdn+station_name + (1|fid), data = dfdn, family = Gamma(link = "log"))
summary(M2)

# Model selection
drop1(M1, test = "Chisq")
anova(M1,M2)

# Final model
model_dn = M2

newdata = with(model_dn,
               expand.grid(fdn = unique(dfdn$fdn),
                           station_name = unique(dfdn$station_name)))
Xmat <- model.matrix(~fdn+station_name, newdata)
fixest <- fixef(model_dn)
fit <- as.vector(fixest %*% t(Xmat))
SE <- sqrt(diag(Xmat %*% vcov(model_dn) %*% t(Xmat)))
q <- qt(0.975, df=df.residual(model_dn))
linkinv <- family(model_dn)$linkinv
newdata <- cbind(newdata, fit=linkinv(fit),
                 lower=linkinv(fit-q*SE),
                 upper=linkinv(fit+q*SE))
newdata <- deploy_zeeb %>% 
  select(station_name, label_name) %>% 
  right_join(newdata)

p_depth_box = newdata %>% 
  ggplot(aes(fdn, fit)) +
  geom_jitter(data = dfdn, 
              aes(x = dn, y=  depth_diff_mean, fill = dn), 
              colour = "#c75133", shape = 21, alpha = 0.7) +
  geom_linerange(aes(ymin= lower, ymax = upper), colour = "#c75133", size = 2) + 
  geom_point(aes(fill= fdn), colour = "#c75133", shape = 22, size = 3) +
  theme_classic() +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        strip.text.x = element_blank()) +
  scale_fill_manual(values = c("#94d7f1", "#1d2855")) +
  scale_y_continuous() +
  labs(x="", y = "Vertical distance (m)") +
  guides(fill = "none") +
  facet_wrap(~label_name)


# Plot residuals
p_depth_pearson = dfdn %>% 
  mutate(pred = fitted(model_dn),
         pear = residuals(model_dn, "pearson")) %>% 
  ggplot(aes(pred, pear))+
  theme_bw()+ 
  theme(legend.position = "none") +
  geom_point(aes(fill = dn, shape = label_name)) +
  scale_shape_manual(values = c(22, 23)) +
  scale_x_continuous(expand = c(0.01,0.01)) +
  scale_fill_manual(values = c("#94d7f1", "#1d2855")) +
  stat_smooth(method = "lm", colour = "black") +
  guides(fill = "none") +
  labs(x = "", 
       y = "Pearson residuals (Depth)")

# Potential issue with larger values: just to check: remove animals with outliers




#### Acceleration: make subset ####
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

#### Acceleration: Boxplot ####
length(unique(dfdn$tag_serial_number))

dfdn %>% 
  ggplot(aes(dn, acc_mean)) +
  theme_classic() +
  theme(strip.text.x = element_blank()) +
  geom_boxplot(aes(fill = dn), outlier.shape = NA, colour = "#c75133", lwd = 0.8) +
  geom_jitter(aes(fill = dn), colour = "#c75133", shape = 21) +
  scale_fill_manual(values = c("#94d7f1", "#1d2855")) +
  scale_y_continuous(expand = c(0.01,0), limits = c(0, 1.9)) +
  labs(x="", y = expression(paste("Acceleration (m/s"^"2",")"))) +
  guides(fill = "none") +
  facet_wrap(~label_name)


#### Acceleration: Model ####
# Plot distribution
dfdn %>% 
  ggplot(aes(acc_mean)) +
  theme_bw() +
  geom_density() +
  facet_wrap(label_name~dn, scales ="free")

# Format
dfdn = dfdn %>% 
  mutate(fid = as.factor(tag_serial_number),
         fdn = as.factor(dn))

# Model
# M1 = lmer(acc_mean ~ fdn*station_name + (1|fid), data = dfdn)
M1 = glmer(acc_mean ~ fdn*station_name + (1|fid), data = dfdn, family = Gamma(link = "log"))
summary(M1)
# M2 = lmer(acc_mean ~ fdn+station_name + (1|fid), data = dfdn)
M2 = glmer(acc_mean ~ fdn+station_name + (1|fid), data = dfdn, family = Gamma(link = "log"))
summary(M2)

# Model selection
drop1(M1, test = "Chisq")
anova(M1, M2)

# Final model
model_dn = M2


newdata = with(model_dn,
               expand.grid(fdn = unique(dfdn$fdn),
                           station_name = unique(dfdn$station_name)))
Xmat <- model.matrix(~fdn+station_name, newdata)
fixest <- fixef(model_dn)
fit <- as.vector(fixest %*% t(Xmat))
SE <- sqrt(diag(Xmat %*% vcov(model_dn) %*% t(Xmat)))
q <- qt(0.975, df=df.residual(model_dn))
linkinv <- family(model_dn)$linkinv
newdata <- cbind(newdata, fit=linkinv(fit),
                 lower=linkinv(fit-q*SE),
                 upper=linkinv(fit+q*SE))
newdata <- deploy_zeeb %>% 
  select(station_name, label_name) %>% 
  right_join(newdata)


p_acc_box = newdata %>% 
  ggplot(aes(fdn, fit)) +
  geom_jitter(data = dfdn, 
              aes(x = dn, y=  acc_mean, fill = dn), 
              colour = "#c75133", shape = 21, alpha = 0.7) +
  geom_linerange(aes(ymin= lower, ymax = upper), colour = "#c75133", size = 2) + 
  geom_point(aes(fill= fdn), colour = "#c75133", shape = 22, size = 3) +
  theme_classic() +
  theme(strip.text.x = element_blank()) +
  # geom_boxplot(aes(fill = dn), outlier.shape = NA, colour = "#c75133", lwd = 0.8) +
  scale_fill_manual(values = c("#94d7f1", "#1d2855")) +
  scale_y_continuous(expand = c(0.01,0), limits = c(0, 1.9)) +
  labs(x="", y = expression(paste("Acceleration (m/s"^"2",")"))) +
  guides(fill = "none") +
  facet_wrap(~label_name)

# Plot residuals
p_acc_pearson = dfdn %>% 
  mutate(pred = fitted(model_dn),
         pear = residuals(model_dn, "pearson")) %>% 
  ggplot(aes(pred, pear))+
  theme_bw()+ 
  theme(legend.position = "none") +
  geom_point(aes(fill = dn, shape = label_name)) +
  scale_shape_manual(values = c(22, 23)) +
  scale_x_continuous(expand = c(0.01,0.01)) +
  scale_fill_manual(values = c("#94d7f1", "#1d2855")) +
  stat_smooth(method = "lm", colour = "black") +
  # facet_wrap(~label_name, scales = "free_x") +
  guides(fill = "none") +
  labs(x = "Fitted values", 
       y = "Pearson residuals (Acceleration)")



#### Combine all plots ####
p_box = p_pres_box + p_depth_box + p_acc_box + plot_layout(nrow = 3, heights = c(1,0.5,0.5))
p_pearson = p_pres_pearson / p_depth_pearson / p_acc_pearson

#### Save plots ####
ggsave(filename = "reports/figures/Fig6_Vandamme_box.jpg", 
       plot = p_box,   
       scale = 1, dpi = 600, width = 8.5, height = 18, units = "cm")
ggsave(filename = "reports/figures/Fig_Annex_Vandamme_pearson.jpg", 
       plot = p_pearson,   
       scale = 1, dpi = 600, width = 10, height = 20, units = "cm")
