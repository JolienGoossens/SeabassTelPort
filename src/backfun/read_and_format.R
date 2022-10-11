######################
# Read + format data #
######################

# Jolien Goossens - Marine Biology Research Group, Ghent University (Marbiol) / Flanders Marine Institute (VLIZ)
# R version 3.6.2


#### Load packages ####
library(tidyverse)
library(lubridate)

#### Get data ####
df <- read_csv("data/interim/df.csv")
an <- read_csv("data/interim/animals.csv")
deploy_zeeb <- read_delim("data/raw/deploy_zeebrugge.csv")

#### Format variables ####
deploy_zeeb <- deploy_zeeb %>% mutate(deploy_type = as.factor(deploy_type))

df = df %>% mutate(tag_serial_number = as.character(tag_serial_number))

an = an %>% mutate(tag_serial_number = as.character(tag_serial_number))


#### Set colours ####
cols <- c("Zeebrugge_port" = "#ffc05f",
          "Zeebrugge_out" = "#5f2f47",
          "Zeebrugge_inner" = "#c75133",
          "Other" = "#79b5ad")
