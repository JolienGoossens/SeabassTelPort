#############################
# Get data and make folders #
#############################

# Jolien Goosens - Marine Biology Research Group, Ghent University (Marbiol)
# R version 3.6.2

#### Load packages ####
#devtools::install_github("inbo/etn")
library(etn)
library(tidyverse)

#### Function to create directory ####
mkdirs <- function(fp) {
  if(!dir.exists(fp)) {
    dir.create(fp)
  }
} 

#### Create directories ####
mkdirs("src")
mkdirs("src/backfun")
mkdirs("src/data")
mkdirs("src/features")
mkdirs("src/models")
mkdirs("src/visualization")
mkdirs("data")
mkdirs("data/external")
mkdirs("data/interim")
mkdirs("data/processed")
mkdirs("data/raw")
mkdirs("reports")
mkdirs("reports/figures")

#### Get detection data #####
# Needs to be changed
my_con <- connect_to_etn(Sys.getenv("username"),
                         Sys.getenv("password"))



df <- get_detections(connection = my_con, animal_project = "PhD_Goossens")
animals <- get_animals(connection = my_con, scientific_name = "Dicentrarchus labrax")
project_list <- c("bpns","pc4c","testvr2ar","thornton", "ws1", "ws2", "ws3", "lifewatch","JJ_Belwind","rt2020_zeeschelde")
deploy <- get_deployments(my_con, open_only = F, network_project_code = project_list)

# Save data
write.csv(df, "data/raw/df_raw.csv", row.names = F)
write.csv(animals, "data/raw/animals_raw.csv", row.names = F)

#### Mapping data ####
# Shape files originate from MarineRegions.org and EMODnet.