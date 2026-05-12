#### Preamble ####
# Purpose: Downloads and saves the data "Motor Vehicle Collisions Involving 
# Killed or Seriously Injured Persons" from Open Data Toronto
# Author: Arusan Surendiran
# Date: 11 May 2026
# Contact: arusan.surendiran@utoronto.ca
# License: MIT


#### Workspace setup ####
library(opendatatoronto)
library(tidyverse)
library(arrow)
library(here)

#### Download data ####
# Extract resources from the package and download the primary CSV file
resources <- list_package_resources("73a8e475-9683-42e1-ac06-b8690dcba062")

raw_collision_data <- resources |>
  filter(format == "CSV", row_number() == 1) |>
  get_resource()

#### Save data ####
write_csv(raw_collision_data, here("data/01-raw_data/raw_collision_data.csv"))
write_parquet(raw_collision_data, here("data/01-raw_data/raw_collision_data.parquet"))

