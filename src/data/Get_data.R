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
# Set up connection
my_con <- connect_to_etn(Sys.getenv("username"),
                         Sys.getenv("password"))

# Define tag serial numbers
tag_vect = c(1271063, 1271064, 1271067, 1271068, 1271069, 1271073, 1271075, 1271077,
             1271096, 1292640, 1292641, 1292642, 1292643, 1292644, 1292647, 1292648,
             1292649, 1292650, 1292651, 1292653, 1292654, 1292655, 1292656, 1292657,
             1292658, 1292659, 1292660, 1292661, 1292662, 1292663, 1292664, 1292665,
             1292666, 1292667, 1292668, 1292669, 1292670, 1292671, 1292672, 1293271,
             1293272, 1293273, 1293274, 1293275, 1293276, 1293277, 1293278, 1293279,
             1293280, 1350779, 1350780, 1350782, 1350783, 1350784, 1350785, 1350786,
             1350787, 1350788, 1400185, 1400186, 1400187, 1400192, 1400193)

# Get animal metadata
animals <- get_animals(connection = my_con, tag_serial_number = tag_vect)

# Get detection data
df <- get_detections(connection = my_con, tag_id = unlist(str_split(animals$acoustic_tag_id, ",")))

# Save data
write_csv(df, "data/raw/df_raw.csv")
write_csv(animals, "data/raw/animals_raw.csv")

#### Mapping data ####
# Shape files originate from MarineRegions.org and EMODnet.