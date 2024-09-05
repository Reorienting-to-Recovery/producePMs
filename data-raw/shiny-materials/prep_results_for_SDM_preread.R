library(tidyverse)
colors_small <-  c("#9A8822", "#899DA4", "#C93312", "#DC863B" # royal 1 (- 3)
)
# Load in model resutls data ---------------------------------------------------
model_results <- read_csv("data-raw/shiny-materials/fall_blended_results_2024-09-04.csv")

# Average Annual central valley wide abundance ---------------------------------
annual_spawners <- model_results |>
  filter(performance_metric == "1 All Spawners") |>
  group_by(year, scenario) |>
  summarize(total_spawners = sum(value, na.rm = T)) |>
  ungroup() |>
  group_by(scenario) |>
  summarize(avg_annual_spawners = mean(total_spawners, na.rm = T),
            min_spawners = min(total_spawners, na.rm = T),
            max_spawners = max(total_spawners, na.rm = T)) |> glimpse()

# Plot for spawners ------------------------------------------------------------
plot <- model_results |>
  filter(performance_metric == "1 All Spawners",
         scenario %in% c("Baseline", "Platypus")) |>
  group_by(year, scenario) |>
  summarize(total_spawners = sum(value, na.rm = T)) |>
  ggplot(aes(year, total_spawners, color = scenario)) +
  geom_line() +
  labs(title = "Total Spawners Summed Across Watersheds Over 20 Year Simulation",
       y = "Total Spawners",
       x = "Simulation Year") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA), labels = scales::comma) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, NA), breaks = 1:20) +
  scale_color_manual(values = colors_small) +
  theme_minimal() +
  theme(text = element_text(size = 12),
        legend.title = element_blank(),
        legend.position = c(.85, .9))

plotly::ggplotly(plot)

# Average central valley wide CRR ----------------------------------------------
# TODO issue here
model_results |>
  filter(performance_metric == "CRR: Total Adult to Returning Natural Adult") |>
  group_by(year, scenario) |>
  summarize(average_crr = mean(value, na.rm = T)) |>
  ungroup() |>
  group_by(scenario) |>
  summarize(avg_annual_crr = mean(average_crr, na.rm = T),
            min_annual_crr = min(average_crr, na.rm = T),
            max_annual_crr = max(average_crr, na.rm = T)) |> glimpse()

# Independent populations ------------------------------------------------------
# For now just listing potential in the DSM preread
# Growth rate of prior two years must be positive
# Natural spawners must be greater than 500
# Then we can indicate independent population
# TODO update logic to incorporate PHOS value - will need more natural fish when high phos
model_results |>
  filter(performance_metric %in% c("Natural Spawners", "2.2 Growth Rate Spawners")) |>
  mutate(above_500_spawners = if_else(value > 500, TRUE, FALSE)) |>
  pivot_wider(names_from = performance_metric, values_from = value) |> glimpse()

# size distribution & month of juveniles ---------------------------------------
# shannon diversity index
monthly_total <- juv_results |>
  group_by(year, scenario, month) |>
  summarize(total_juveniles = sum(value, na.rm = T)) |> glimpse()

juv_results |>
  group_by(year, size, scenario, month) |>
  summarize(frequency = sum(value, na.rm = T)) |>
  ungroup() |>
  left_join(monthly_total) |>
  mutate(pi = frequency / total_juveniles,
         ln_pi = log(pi),
         pi_ln_pi = pi * ln_pi) |>
  group_by(year, scenario) |>
  summarize(shannon_index = -1 * sum(pi_ln_pi, na.rm = T)) |>
  ungroup() |>
  group_by(scenario) |>
  summarise(avg_annual_shannon_di = mean(shannon_index)) |>
  glimpse()

# just size - no timing
annual_total <- juv_results |>
group_by(year, scenario) |>
  summarize(total_juveniles = sum(value, na.rm = T)) |> glimpse()

juv_results |>
  group_by(year, size, scenario) |>
  summarize(frequency = sum(value, na.rm = T)) |>
  ungroup() |>
  left_join(annual_total) |>
  mutate(pi = frequency / total_juveniles,
         ln_pi = log(pi),
         pi_ln_pi = pi * ln_pi) |>
  group_by(year, scenario) |>
  summarize(shannon_index = -1 * sum(pi_ln_pi, na.rm = T)) |>
  ungroup() |>
  group_by(scenario) |>
  summarise(avg_annual_shannon_di = mean(shannon_index)) |>
  glimpse()

# marine derived nutrient ------------------------------------------------------
# prep chanel area (sqmt)
chanel_area <- read_csv("channel_areas.csv") |>
  mutate(sit_width = `width - sit` * 0.3048,
         sit_length = `length - sit` * 0.3048,
         add_max_hab_width = `width - add tmh`* 0.3048,
         add_max_hab_length = `length - add tmh` * 0.3048,
         sit_area = sit_width * sit_length,
         add_tmh_area = add_max_hab_width * add_max_hab_length) |> glimpse()

sit_total_area_sqmt <- chanel_area |>
  pull(sit_area) |>
  sum(na.rm = T)

max_hab_total_area_sqmt <- chanel_area |>
  pull(add_tmh_area) |>
  sum(na.rm = T) + sit_total_area_sqmt

annual_spawners |>
  mutate(area_sqmt = ifelse(scenario %in% c("Baseline", "New sit results 2022 params"), sit_total_area_sqmt, max_hab_total_area_sqmt),
         fish_total_pounds = avg_annual_spawners * 21, # TODO assuming ~21 pounds
         marine_derived_nutrient_pounds_per_sqmt = fish_total_pounds / area_sqmt) |> glimpse()

# Plots for SDM presentation ---------------------------------------------------
plot_watershed_specific <- model_results |>
  filter(location %in% c("Calaveras River",
                         "Stony Creek"),
         performance_metric == "All Spawners",
         scenario %in% c("Baseline", "Theoretical Max Habitat")) |>
  ggplot(aes(year, value, linetype = scenario, color = location)) +
  geom_line(color = "#9A8822") +
  labs(title = "Total Spawners Over 20 Year Simulation",
       y = "Total Spawners",
       x = "Simulation Year") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA), labels = scales::comma) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, NA), breaks = 1:20) +
  scale_color_manual(values = colors_small) +
  theme_minimal() +
  theme(text = element_text(size = 12),
        legend.title = element_blank(),
        legend.position = "bottom") +
  facet_wrap(~location, dir = "v")
plot_watershed_specific

# Baseline total spawners
model_results |>
filter(performance_metric == "All Spawners",
       scenario %in% c("Baseline")) |>
  group_by(year, scenario) |>
  summarize(total_spawners = sum(value, na.rm = T)) |>
  ggplot(aes(year, total_spawners, color = scenario)) +
  geom_line() +
  labs(title = "Fall Run Total Spawners",
       y = "Total Spawners",
       x = "Simulation Year") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA), labels = scales::comma) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, NA), breaks = 1:20) +
  scale_color_manual(values = colors_small) +
  theme_minimal() +
  theme(text = element_text(size = 12),
        legend.title = element_blank(),
        legend.position = "none")



