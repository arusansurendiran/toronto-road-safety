#### Preamble ####
# Purpose: Cleans the raw collision data to handle missing values and correct data types.
# Author: Arusan Surendiran
# Date: 11 May 2026
# Contact: arusan.surendiran@utoronto.ca

#### Workspace setup ####
library(tidyverse)
library(lubridate)
library(here)

#### Clean data ####
raw_data <- read_csv(here("data/01-raw_data/raw_collision_data.parquet"))

cleaned_ksi_data <- raw_data |>
  # Fix outliers where the longitude sign was likely flipped during data entry.
  mutate(longitude = if_else(longitude > 0, -longitude, longitude)) |>
  
  # Remove rows without coordinates ()
  filter(!is.na(longitude), !is.na(latitude)) |>
  
  # Clean age data and collision classifications
  mutate(
    invage = if_else(invage > 110, NA_real_, invage),
    acclass = if_else(acclass == "Property Damage Only", "Non-Fatal Injury", acclass)) |>
  
  # Create temporal variables
  mutate(
    accdate = ymd_hms(accdate),
    year = year(accdate),
    month = month(accdate, label = TRUE, abbr = TRUE),
    day_of_week = wday(accdate, label = TRUE, abbr = TRUE),
    hour = hour(accdate)
  ) |>
  
  # Standardize empty strings and missing flags to 'No' for logical consistency
  mutate(across(where(is.character), ~ if_else(.x == "" | .x == " ", NA_character_, .x))) |>
  mutate(across(c(aggressive, distracted, red_light, pedestrian, cyclist, motorcyclist, heavy_truck), 
                ~ replace_na(., "No")))

#### Save data ####
write_csv(cleaned_ksi_data, "data/analysis_data/analysis_data.csv")
