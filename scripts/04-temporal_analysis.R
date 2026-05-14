#### Preamble ####
# Purpose: Analyze and visualize temporal trends in Toronto collision data.
# Author: Arusan Surendiran
# Date: 14 May 2026
# Contact: arusan.surendiran@utoronto.ca

#### Workspace setup ####
library(tidyverse)
library(here)
library(arrow)

# Set a consistent theme for report consistency
map_theme <- theme_minimal() + 
  theme(
    text = element_text(size = 12),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    legend.position = "bottom")

#### Load Data ####
collisions <- read_parquet(here("data/02-analysis_data/clean_collision_data.parquet"))

#### Hourly Distribution ####
hourly_data <- collisions |>
  filter(!is.na(hour)) |>
  group_by(hour) |>
  summarise(incident_count = n(), .groups = "drop")

hourly_collisions <- ggplot(hourly_data, aes(x = hour, y = incident_count)) +
  geom_line(linewidth = 1, color = "#2c3e50") +
  geom_point(size = 2, color = "#2c3e50") +
  scale_x_continuous(breaks = seq(0, 23, by = 2)) +
  map_theme +
  labs(
    x = "Hour of Day (0 = Midnight)",
    y = "Total Incidents"
  )

#### Temporal Heatmap ####
heatmap_data <- collisions |>
  filter(!is.na(hour), !is.na(day_of_week)) |>
  group_by(day_of_week, hour) |>
  summarise(count = n(), .groups = "drop_last") |>
  mutate(day_of_week = factor(day_of_week, 
                              levels = c("Sat", "Fri", "Thu", 
                                         "Wed", "Tue", "Mon", "Sun")))

heatmap_day_hour <- ggplot(heatmap_data, aes(x = hour, y = day_of_week, fill = count)) +
  geom_tile(color = "white", linewidth = 0.2) +
  scale_fill_viridis_c(option = "magma", direction = -1) +
  scale_x_continuous(breaks = seq(0, 23, by = 2)) +
  map_theme +
  labs(
    x = "Hour of Day",
    y = NULL,
    fill = "Incident Count"
  ) +
  theme(panel.grid = element_blank())
