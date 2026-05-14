#### Preamble ####
# Purpose: Generate map visualizations for Toronto collision data.
# Author: Arusan Surendiran
# Date: 12 May 2026
# Contact: arusan.surendiran@utoronto.ca

#### Workspace setup ####
library(tidyverse)
library(here)
library(arrow)
library(sf)
library(opendatatoronto)
library(patchwork)

# Set global theme for maps
map_theme <- theme_void() + 
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10),)

#### Read and Prepare Data ####

collisions <- read_parquet(here("data/02-analysis_data/clean_collision_data.parquet"))

#### Ward-Level Analysis ####

# Aggregate collisions by ward
collisions_by_ward <- collisions |>
  count(wardname, name = "collisions")

# Fetch Ward boundaries
ward_map_data <- list_package_resources("city-wards") |>
  filter(name == "City Wards Data") |>
  get_resource() |>
  left_join(collisions_by_ward, by = c("AREA_DESC" = "wardname"))

# Plot: Ward Map
collisions_map_wards <- ggplot(data = ward_map_data) +
  geom_sf(aes(fill = collisions), color = "white", size = 0.2) +
  scale_fill_viridis_c(option = "mako", direction = -1, na.value = "grey90") +
  map_theme +
  labs(
    title = "Vehicle Collisions by Toronto Ward",
    subtitle = "Total Collision Counts (2006 - 2026)",
    fill = "Total Collisions")

#### Neighbourhood-Level Analysis ####

## Prepare Population Data (2021 Census)
nbhd_resources <- list_package_resources("6e19a90f-971c-46b3-852c-0c48c436d1fc") |>
  filter(name == "neighbourhood-profiles-2021-158-model") |>
  get_resource()

nbhd_population <- nbhd_resources$hd2021_census_profile |>
  rename(characteristic = `Neighbourhood Name`) |>
  filter(str_detect(characteristic, "Total - Age groups")) |>
  pivot_longer(
    cols = -characteristic, 
    names_to = "neighbourhood_name", 
    values_to = "population"
  ) |>
  mutate(
    population = as.numeric(gsub(",", "", population)), # Ensure numeric
    neighbourhood_name = trimws(neighbourhood_name)
  ) |>
  filter(neighbourhood_name != "City of Toronto") |>
  select(neighbourhood_name, population)

## Prepare Geographic Data
collisions_by_nbhd <- collisions |>
  count(neighbourhood, name = "total_collisions")

nbhd_data <- list_package_resources("neighbourhoods") |>
  filter(name == "Neighbourhoods") |>
  get_resource() |>
  # Join collision counts and population data
  left_join(collisions_by_nbhd, by = c("AREA_NAME" = "neighbourhood")) |>
  left_join(nbhd_population, by = c("AREA_NAME" = "neighbourhood_name")) |>
  mutate(
    total_collisions = replace_na(total_collisions, 0),
    # Annualized rate per 10,000 residents (assuming 20-year span: 2006-2026)
    collision_rate = (total_collisions / population / 20) * 10000 
  )

#### Visualization: Comparison Maps ####

plot_absolute <- ggplot(data = nbhd_data) +
  geom_sf(aes(fill = total_collisions), color = "white", size = 0.05) +
  scale_fill_viridis_c(option = "rocket", direction = -1) +
  map_theme +
  labs(
    subtitle = "Absolute Collision Counts",
    fill = "Total Collisions"
  )

plot_rate <- ggplot(data = nbhd_data) +
  geom_sf(aes(fill = collision_rate), color = "white", size = 0.05) +
  scale_fill_viridis_c(option = "rocket", direction = -1) +
  map_theme +
  labs(
    subtitle = "Average Collisions per year per 10,000 Residents",
    fill = "Annual Rate"
  )

# Combine using patchwork
combined_maps <- plot_absolute + plot_rate + 
  plot_annotation(
    title = "Comparison of Collision Frequency and Risk by Population Size",
    subtitle = "Analysis across Toronto Neighbourhoods (2006-2026)")
