#### Preamble ####
# Purpose: Evaluate the impact of Vision Zero policy on collision trends.
# Author: Arusan Surendiran
# Date: 14 May 2026
# Contact: arusan.surendiran@utoronto.ca

#### Workspace setup ####
library(tidyverse)
library(here)
library(arrow)

#### Data Preparation ####
# Load cleaned analysis data
collisions <- read_parquet(here("data/02-analysis_data/clean_collision_data.parquet"))

vision_zero_analysis <- collisions |>
  group_by(year, acclass) |> 
  summarise(total_incidents = n(), .groups = "drop")

#### Visual Analysis ####

collision_legend <- c(
  "Vision Zero (2016)" = "darkorange",
  "Vision Zero 2.0 (2019)" = "darkgreen"
)

vision_zero_chart <- ggplot(vision_zero_analysis, aes(x = year, y = total_incidents)) +
  # Trend Lines and Points
  geom_line(aes(color = acclass), linewidth = 1) +
  geom_point(aes(color = acclass), size = 2) +
  
  # Policy Times mapped to aesthetic for legend inclusion
  geom_vline(aes(xintercept = 2016, color = "Vision Zero (2016)"), 
             linetype = "longdash", linewidth = 0.8) +
  geom_vline(aes(xintercept = 2019, color = "Vision Zero 2.0 (2019)"), 
             linetype = "longdash", linewidth = 0.8) +
  facet_wrap(~ acclass, scales = "free_y") + 
  expand_limits(y = 0) +
  scale_color_manual(values = collision_legend) +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = 11)) +
  labs(
    x = "Year",
    y = "Total Incidents",
    color = "Legend")