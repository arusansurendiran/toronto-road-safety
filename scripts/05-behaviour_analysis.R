#### Preamble ####
# Purpose: Analyze risk factors (behavior, vehicle type, and road user) in collisions.
# Author: Arusan Surendiran
# Date: 14 May 2026
# Contact: arusan.surendiran@utoronto.ca

#### Workspace setup ####
library(tidyverse)
library(here)
library(arrow)
library(patchwork)

#### Data Preparation ####
collisions <- read_parquet(here("data/02-analysis_data/clean_collision_data.parquet"))

# Behavioral Risk Factors
behavioral_data <- collisions |>
  
  filter(acclass == "Fatal Injury") |>
  
  select(aggressive, distracted, red_light) |>
  # True/False check: 
  mutate(across(everything(), ~ ifelse(. == "true", 1, 0))) |>
  
  # Sum each column to get total fatal counts per behavior
  summarise(across(everything(), sum, na.rm = TRUE)) |>
  
  # Pivot from wide to long format for plotting
  pivot_longer(cols = everything(), names_to = "behavior", values_to = "fatal_count") |>
  
  # Clean up the labels for the chart
  mutate(behavior = case_when(
    behavior == "aggressive" ~ "Aggressive Driving\n(Inc. Speeding)",
    behavior == "distracted" ~ "Distracted / Inattentive",
    behavior == "red_light" ~ "Red Light Running"
  )) |>
  arrange(desc(fatal_count))

# Heavy Truck Involvement
truck_danger <- collisions |>
  mutate(truck_involved = ifelse(heavy_truck == "true", "Heavy Truck Involved", "No Heavy Truck")) |>
  group_by(truck_involved) |>
  summarise(
    total_incidents = n(),
    fatalities = sum(acclass == "Fatal Injury", na.rm = TRUE),
    fatality_rate = (fatalities / total_incidents) * 100)

# Road User Vulnerability
vru_data <- collisions |>
  filter(road_user != "other") |>
  group_by(road_user) |>
  summarise(
    total = n(),
    fatality_rate = (sum(acclass == "Fatal Injury") / total) * 100,
    .groups = "drop") |>
  mutate(road_user = str_to_title(str_replace_all(road_user, "_", " ")))

#### Visual Analysis ####

# Behavioral Data
plot_behavioral <- ggplot(behavioral_data, aes(x = reorder(behavior, fatal_count), y = fatal_count, fill = fatal_count)) +
  geom_col(color = "white", width = 0.7) +
  scale_fill_viridis_c(option = "viridis", direction = -1) + 
  coord_flip() + 
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(face = "bold", size = 12)) +
  labs(title = "Total Fatalities by Behavioral Factor", x = NULL, y = "Count")

# Truck Involvement
plot_truck <- ggplot(truck_danger, aes(x = reorder(truck_involved, fatality_rate), y = fatality_rate, fill = fatality_rate)) +
  geom_col(color = "white", width = 0.6) +
  scale_fill_viridis_c(option = "viridis", direction = -1) + 
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(face = "bold", size = 12)) +
  labs(title = "Fatality Rate by Heavy Vehicle Involvement", x = NULL, y = "Fatality Rate (%)")

# Road User Type
plot_vru <- ggplot(vru_data, aes(x = reorder(road_user, fatality_rate), y = fatality_rate, fill = fatality_rate)) +
  geom_col(color = "white", width = 0.7) +
  scale_fill_viridis_b(option = "viridis", direction = -1) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(face = "bold", size = 12)) +
  labs(title = "Fatality Rate by Road User Classification", x = NULL, y = "Fatality Rate (%)")

# Combine using patchwork
risk_factor_stack <- (plot_behavioral / plot_truck / plot_vru) + 
  plot_annotation(
    title = "Analysis of High-Risk Factors in Toronto Collisions",
    subtitle = "Comparing driver behaviors, vehicle types, and user vulnerability (2006-2026)",
    theme = theme(plot.title = element_text(size = 14, face = "bold")))