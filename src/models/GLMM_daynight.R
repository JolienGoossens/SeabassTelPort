#######################################
# Investigate diel patterns Vandamme  #
#######################################

# Jolien Goossens - Marine Biology Research Group, Ghent University (Marbiol) / Flanders Marine Institute (VLIZ)
# R version 3.6.2


#### Run read_and_format.R ####
source("src/backfun/read_and_format.R")
library(lme4)

#### Make subset ####
df %>% 
  filter(acoustic_project_code == "bpns" & station_group == "Zeebrugge_inner") %>% 
  group_by(station_group, station_name) %>% 
  summarise(n_an = length(unique(tag_serial_number)),
            n = n())
# Only use Vandamme and Boudewijn


dfdn = df %>% 
  filter(station_name %in% c("bpns-vandamme", "bpns-boudewijn")) %>% 
  group_by(station_name, tag_serial_number, dn) %>% 
  summarise(n_hour = length(unique(date_hour))) %>% 
  # Filter for animals that were seen at least 10 hours in these stations
  group_by(tag_serial_number) %>% 
  filter(sum(n_hour) > 10) %>% 
  ungroup()

dfdn = expand.grid(
  station_name = unique(dfdn$station_name),
  tag_serial_number = unique(dfdn$tag_serial_number),
  dn = unique(dfdn$dn)) %>% 
  left_join(dfdn) %>% 
  mutate(n_hour = replace_na(n_hour, 0))


dfdn = deploy_zeeb %>% 
  select(station_name, label_name) %>% 
  right_join(dfdn)

#### Boxplot ####
p_vandamme = dfdn %>% 
  ggplot(aes(dn, n_hour)) +
  theme_classic() +
  theme(axis.line.x = element_blank(),
        axis.ticks.x = element_blank()) +
  geom_boxplot(aes(fill = dn), colour = "#c75133", lwd = 0.8) +
  geom_jitter(aes(fill = dn), colour = "#c75133", shape = 21) +
  scale_fill_manual(values = c("#94d7f1", "#1d2855")) +
  scale_y_continuous(expand = c(0.01,0)) +
  labs(x="", y = "Detected hour bin per fish") +
  guides(fill = "none") +
  facet_wrap(~label_name)

ggsave(filename = "reports/figures/Fig4_Vandamme_dn.jpg", 
       plot = p_vandamme,   
       scale = 1, dpi = 600, width = 10, height = 10, units = "cm")

#### Model ####
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
model_dn = glmer(n_hour ~ fdn*station_name + (1|fid), data = dfdn, family = poisson)
summary(model_dn) # All effects significant

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
  facet_wrap(~label_name, scales = "free_x") +
  guides(fill = "none") +
  labs(x = "Fitted values", 
       y = "Pearson residuals")
ggsave(filename = "reports/figures/Fig_Annex_ModelVandamme_pearson.jpg", 
       plot = p_pearson,   
       scale = 1, dpi = 600, width = 10, height = 10, units = "cm")
# Potential issue with larger values: just to check: remove animals with outliers

#### Run model on subset to check ####
dfdn_sub = dfdn %>% 
  group_by(tag_serial_number) %>% 
  summarise(max_hour = max(n_hour)) %>% 
  filter(max_hour < 1000) %>% 
  left_join(dfdn)

# Model
model_dfdn_sub = lme4::glmer(n_hour ~ fdn*station_name + (1|fid), data = dfdn_sub, family = poisson)
summary(model_dfdn_sub) # All effects significant

p_pearsonsub = dfdn_sub %>% 
  mutate(pred = fitted(model_dfdn_sub),
         pear = residuals(model_dfdn_sub, "pearson")) %>% 
  ggplot(aes(pred, pear))+
  theme_bw()+ 
  geom_point(aes(fill = dn), shape = 21) +
  scale_x_continuous(expand = c(0.01,0.01)) +
  scale_fill_manual(values = c("#94d7f1", "#1d2855")) +
  stat_smooth(method = "lm", colour = "black") +
  facet_wrap(~label_name, scales = "free_x")+
  guides(fill = "none") +
  labs(x = "Fitted values", 
       y = "Pearson residuals")
# Seems okay!

ggsave(filename = "reports/figures/Fig_Annex_ModelVandamme_pearsonsub.jpg", 
       plot = p_pearsonsub,   
       scale = 1, dpi = 600, width = 10, height = 10, units = "cm")
