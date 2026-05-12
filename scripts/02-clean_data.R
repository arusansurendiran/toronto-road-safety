#### Preamble ####
# Purpose: Cleans the raw collision data to handle missing values and correct data types.
# Author: Arusan Surendiran
# Date: 11 May 2026
# Contact: arusan.surendiran@utoronto.ca

#### Workspace setup ####
library(tidyverse)
library(lubridate)
library(here)

#### Load data ####
raw_collision_data <- read_parquet(here("data/01-raw_data/raw_collision_data.parquet"))

#### Data overview ####

dropped_counts <- raw_collision_data |> 
  summarise(
    total_rows = n(),
    missing_coords = sum(is.na(latitude) | is.na(longitude)),
    wrong_coords = sum(longitude > 0 | latitude < 0, na.rm = TRUE),
    impossible_ages = sum(invage > 110, na.rm = TRUE),
    prop_damage_cases = sum(acclass == "Property Damage Only", na.rm = TRUE))

print(dropped_counts)

## Most columns of interest have little NA values

na_counts <- (colMeans(is.na(raw_collision_data)) * 100) |> sort(decreasing = TRUE)
print(na_counts)

## Many columns have empty strings

empty_props <- (colMeans(raw_collision_data == "", na.rm = TRUE) * 100) |> sort(decreasing = TRUE)
print(empty_props)

empty_counts <- (colSums(raw_collision_data == "", na.rm = TRUE)) |> sort(decreasing = TRUE)
print(empty_counts)

#### Clean data ####

empty_to_na_data <- raw_collision_data |>
  # Convert empty strings to NA across all character columns
  mutate(across(where(is.character), ~na_if(.x, "")))

clean_collision_data <- empty_to_na_data |>
  
  filter(
    # Remove rows without coordinates (Only 3 ROWS)
    !is.na(longitude), !is.na(latitude),
    # Remove unclassified accidents (Only 1 ROW)
    !is.na(acclass),
    # Remove observations without a ward (Only 126 ROWS (0.61%))
    !is.na(wardname)) |>
  
  mutate(
    # Fix outliers where the longitude sign was likely flipped during data entry (Only 2 ROWS)
    longitude = if_else(longitude > 0, -longitude, longitude),
    
    # Remove implausible age values (Only 2 ROWS)
    invage = if_else(invage > 110, NA_real_, invage),
    
    # Simplify accident classification by combining Property Only with Non-Fatal (Only 18 ROWS) 
    acclass = if_else(acclass == "Property Damage Only", "Non-Fatal Injury", acclass),
    
    # Create temporal variables
    accdate = ymd_hms(accdate),
    year = year(accdate),
    month = month(accdate, label = TRUE, abbr = TRUE),
    day_of_week = wday(accdate, label = TRUE, abbr = TRUE),
    hour = hour(accdate),

    # Fill specific NAs with "No"
    across(c(aggressive, distracted, red_light, pedestrian, cyclist, motorcyclist, heavy_truck), 
           ~replace_na(.x, "No")))

#### Save data ####
write_csv(clean_collision_data, here("data/02-analysis_data/clean_collision_data.csv"))
write_parquet(clean_collision_data, here("data/02-analysis_data/clean_collision_data.parquet"))
cat("Successfully saved data files to 02-analysis_data folder")

