#### Preamble ####
# Purpose: 
# Author: Arusan Surendiran
# Date: 12 May 2026
# Contact: arusan.surendiran@utoronto.ca


#### Workspace setup ####
library(tidyverse)
library(ggplot2)
library(patchwork)
library(here)
library(arrow)
library(kableExtra)
library(opendatatoronto)
library(sf)
library(osmdata)

#### Read data ####
collisions <- read_parquet(here("data/02-analysis_data/clean_collision_data.parquet"))

### MAP: Collisions by Ward ####
collisions_by_ward <- collisions |>
  #filter(acclass == "Fatal Injury") |>
  count(wardname, name = "collisions")

ward_map_data <- list_package_resources("city-wards") |>
  filter(name == "City Wards Data") |>
  get_resource() |>
  left_join(collisions_by_ward, by = c("AREA_DESC" = "wardname"))

# Toronto Map
collisions_map_wards <- ggplot(data = ward_map_data) +
  geom_sf(aes(fill = collisions), color = "white", size = 0.2) +
  scale_fill_viridis_c(option = "mako", direction = -1) +
  theme_void() +
  labs(
    title = "Concentration of Traffic Collisions by Toronto Ward",
    subtitle = "Total Collisions Count from 2006 to 2026",
    fill = "Collisions")


### MAP: Collisions by Neighbourhood ####


## PREPARE Neighbourhood Data

# Aggregate collisions by neighbourhood
collisions_by_nbhd <- collisions |>
  count(neighbourhood, name = "collisions")

nbhd_resources <- list_package_resources("6e19a90f-971c-46b3-852c-0c48c436d1fc") |>
  filter(name == "neighbourhood-profiles-2021-158-model") |>
  get_resource()

nbhd_profile_raw <- nbhd_resources$hd2021_census_profile

nbhd_population <- nbhd_profile_raw |>
  rename(characteristic = `Neighbourhood Name`) |>
  
  # Extract row containing total population
  filter(str_detect(characteristic, "Total - Age groups")) |>
  
  pivot_longer(
    cols = -characteristic, 
    names_to = "neighbourhood_name", 
    values_to = "population"
  ) |>
  
  mutate(
    population = as.numeric(population),
    neighbourhood_name = trimws(neighbourhood_name)
  ) |>
  
  select(neighbourhood_name, population) |>
  filter(neighbourhood_name != "City of Toronto")

# Get Neighbourhood geography
nbhd_data <- list_package_resources("neighbourhoods") |>
  filter(name == "Neighbourhoods") |>
  get_resource() |>
  left_join(collisions_by_nbhd, by = c("AREA_NAME" = "neighbourhood")) |>
  left_join(nbhd_population, by = c("AREA_NAME" = "neighbourhood_name")) |>
  mutate(
    total_collisions = replace_na(collisions, 0),
    collision_rate = (total_collisions / population) * 10000)



## Plot Side-by-Side Maps

p1 <- ggplot(data = nbhd_data) +
  geom_sf(aes(fill = total_collisions), color = "white", size = 0.05) +
  scale_fill_viridis_c(option = "rocket", direction = -1) +
  theme_void() +
  labs(
    subtitle = "Absolute Collision Counts",
    fill = "Total Deaths"
  ) +
  theme(legend.position = "bottom")

p2 <- ggplot(data = nbhd_data) +
  geom_sf(aes(fill = collision_rate), color = "white", size = 0.05) +
  scale_fill_viridis_c(option = "rocket", direction = -1) +
  theme_void() +
  labs(
    subtitle = "Annual Rate per 10,000 Residents",
    fill = "Rate"
  ) +
  theme(legend.position = "bottom")

combined_maps <- p1 + p2 + 
  plot_annotation(
    title = "Comparison of Collision Frequency and Risk by Population Size",
    subtitle = "Toronto Neighbourhoods",
    caption = "Comparison between raw incident counts and annualized population-adjusted rates."
  )


#### LINE PLOT: Trends by Hour of Day ####

hourly_data <- collisions |>
  # Drop NA hours to keep the plot clean
  filter(!is.na(hour)) |>
  group_by(hour) |>
  summarise(incident_count = n(), .groups = "drop")

# Plot the temporal trend
hourly_collisions <- ggplot(hourly_data, aes(x = hour, y = incident_count)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(0, 23, by = 2)) +
  theme_minimal() +
  theme(legend.position = "none") + # Hide legend since facets act as labels
  labs(
    title = "Hourly Distribution of Traffic Collisions",
    x = "Hour of Day (0 = Midnight)",
    y = "Total Incidents")








