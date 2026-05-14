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

#### BAR CHART: Collisions over Time ####

# Milestone years for Toronto, including the pre- and post-years of Vision Zero
milestone_years <- c(2006, 2015, 2019, 2025)

vision_zero_analysis <- collisions |>
  # Filter only for the years we care about
  filter(year %in% milestone_years) |>
  # Group by year and accident class (Fatal vs Non-Fatal)
  group_by(year, acclass) |>
  summarise(total_incidents = n(), .groups = "drop")

# Grouped Bar Chart
vision_zero_chart <- ggplot(vision_zero_analysis, aes(x = factor(year), y = total_incidents, fill = acclass)) +
  geom_col(position = "dodge", color = "black", width = 0.7) +
  scale_fill_manual(values = c("Fatal Injury" = "darkred", "Non-Fatal Injury" = "steelblue")) +
  theme_minimal() +
  labs(
    title = "Toronto Collisions: Vision Zero Impact Analysis",
    subtitle = "Comparing baseline (2006), pre-policy (2015), pre-pandemic (2019), and current (2025)",
    x = "Year",
    y = "Total Incidents",
    fill = "Severity",
    caption = "Evaluating collision volume before and after the 2016 implementation of Vision Zero."
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )


#### HEATMAP: Day and Hour of Week Trends ####

# Aggregate data for the heatmap
heatmap_data <- collisions |>
  filter(!is.na(hour), !is.na(day_of_week)) |>
  group_by(day_of_week, hour) |>
  summarise(count = n(), .groups = "drop")

# Plot the Heatmap
heatmap_day_hour <- ggplot(heatmap_data, aes(x = hour, y = day_of_week, fill = count)) +
  geom_tile(color = "white", size = 0.5) +
  scale_fill_viridis_c(option = "magma", direction = -1) +
  scale_x_continuous(breaks = seq(0, 23, by = 2)) +
  theme_minimal() +
  labs(
    title = "Temporal Heatmap of Fatal Traffic Incidents",
    subtitle = "Concentration of fatalities by day of week and hour",
    x = "Hour of Day",
    y = "",
    fill = "Fatalities",
    caption = "Figure 14: Heatmap visualization identifying critical risk windows."
  )

####
### BEHAVIOURS


behavioral_data <- collisions |>
  # Isolate only fatal incidents (checking both common formats just in case)
  #filter(acclass == "Fatal Injury") |>
  
  select(aggressive, distracted, red_light) |>
  
  # Robust True/False check: 
  # If it is TRUE (logical) or "True" (string), make it 1. Everything else (including NA) is 0.
  mutate(across(everything(), ~ ifelse(. %in% c(TRUE, "True", "TRUE", "true"), 1, 0))) |>
  
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

# Plot the horizontal bar chart
plot_behavioral_data <- ggplot(behavioral_data, aes(x = reorder(behavior, fatal_count), y = fatal_count, fill = behavior)) +
  geom_col(color = "black") +
  scale_fill_viridis_d(option = "mako", direction = -1) +
  coord_flip() + 
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    title = "Fatalities by Behavioral Risk Factor",
    subtitle = "Toronto KSI Data (2006-2026)",
    x = "",
    y = "Total Fatalities")


truck_danger <- collisions |>
  # Ensure the flag is treated as True/False or 1/0
  mutate(truck_involved = ifelse(heavy_truck == "true", "Heavy Truck Involved", "No Heavy Truck")) |>
  group_by(truck_involved) |>
  summarise(
    total_incidents = n(),
    fatalities = sum(acclass %in% c("Fatal", "Fatal Injury"), na.rm = TRUE),
    lethality_rate = (fatalities / total_incidents) * 100
  )

# Plot the comparison
plot_truck_danger <- ggplot(truck_danger, aes(x = truck_involved, y = lethality_rate, fill = truck_involved)) +
  geom_col(color = "black", width = 0.5) +
  scale_fill_manual(values = c("Heavy Truck Involved" = "darkred", "No Heavy Truck" = "steelblue")) +
  theme_minimal() +
  labs(
    title = "Lethality Rate: Heavy Trucks vs. Standard Traffic",
    y = "Fatality Rate per Incident (%)",
    x = "") +
  theme(legend.position = "none")

vru_data <- collisions |>
  # Filter out property owners and undefined others
  filter(!road_user %in% c("owner", "other", NA)) |>
  group_by(road_user) |>
  summarise(
    total_involved = n(),
    fatalities = sum(acclass %in% c("Fatal", "Fatal Injury"), na.rm = TRUE),
    lethality_rate = (fatalities / total_involved) * 100
  ) |>
  arrange(desc(lethality_rate))

# Plot the Lethality Rate
plot_road_user <- ggplot(vru_data, aes(x = reorder(road_user, lethality_rate), y = lethality_rate, fill = road_user)) +
  geom_col(color = "black", width = 0.6) +
  scale_fill_viridis_d(option = "rocket", direction = -1) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    title = "Lethality Rate by Road User Type",
    subtitle = "Percentage of involvements that result in a fatality",
    x = "Road User Category",
    y = "Fatality Rate (%)"
  )

