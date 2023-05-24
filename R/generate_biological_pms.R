library(tidyverse)

# Load in model resutls data ---------------------------------------------------
# source("R/run_model_scenarios.R")
# model_results_df <-process_model_results(baseline_model_results, "Baseline", "Fall Run")

### 1 ###
# Average Annual central valley wide abundance ---------------------------------
produce_spawner_abundance_pm <- function(model_results_df){
  annual_spawners <- model_results_df |>
    filter(performance_metric == "All Spawners") |>
    group_by(year, scenario) |>
    summarize(total_spawners = sum(value, na.rm = T)) |>
    ungroup() |>
    group_by(scenario) |>
    summarize(avg_annual_spawners = mean(total_spawners, na.rm = T),
              min_spawners = min(total_spawners, na.rm = T),
              max_spawners = max(total_spawners, na.rm = T))
  return(annual_spawners)
}

### 2.1 ###
# Average central valley wide CRR ----------------------------------------------
produce_crr_pm <- function(model_results_df){
  crr <- model_results_df |>
    filter(performance_metric == "CRR: Total Adult to Returning Natural Adult") |>
    group_by(year, scenario) |>
    summarize(average_crr = mean(value, na.rm = T)) |>
    ungroup() |>
    group_by(scenario) |>
    summarize(avg_annual_crr = mean(average_crr, na.rm = T),
              min_annual_crr = min(average_crr, na.rm = T),
              max_annual_crr = max(average_crr, na.rm = T))
  return(crr)
}

### 2.2 ###
# Average growth rate central valley wide --------------------------------------
# TODO check do we want trend in growth rate or last 3 years
produce_growth_rate_pm <- function(model_results_df){
  growth_rates <- model_results_df |>
    filter(performance_metric == "Growth Rate Natural Spawners") |>
    group_by(year, scenario) |>
    summarize(average_crr = mean(value, na.rm = T)) |>
    ungroup() |>
    group_by(scenario) |>
    summarize(avg_annual_crr = mean(average_crr, na.rm = T),
              min_annual_crr = min(average_crr, na.rm = T),
              max_annual_crr = max(average_crr, na.rm = T))
  return(growth_rates)
}

### 3.1 ###
# Number of Independent populations ------------------------------------------------------
# For now just listing potential in the DSM preread
# Growth rate of prior two years must be positive
# Natural spawners must be greater than 500
# Then we can indicate independent population
# TODO update logic to incorporate PHOS value - will need more natural fish when high phos
model_results_df |>
  filter(performance_metric %in% c("Natural Spawners", "Growth Rate Natural Spawners")) |>
  pivot_wider(names_from = performance_metric, values_from = value) |>
  mutate(above_500_spawners = if_else(value > 500, TRUE, FALSE)) |> glimpse()

### 3.2 ###
### TODO
# % of potential independent viable populations in each diversity group per ESU/run

### 3.2 ###
### TODO
# Number of dependent populations in each diversity group per ESU/run


### 4 ###
# PHOS - Salmon genetic diversity ----------------------------------------------
# TODO check max phos seems low
produce_phos_pm <- function(model_results_df){
  phos <- model_results_df |>
    filter(performance_metric == "PHOS") |>
    group_by(year, scenario) |>
    summarize(average_crr = mean(value, na.rm = T)) |>
    ungroup() |>
    group_by(scenario) |>
    summarize(avg_annual_crr = mean(average_crr, na.rm = T),
              min_annual_crr = min(average_crr, na.rm = T),
              max_annual_crr = max(average_crr, na.rm = T))
  return(phos)
}

### 5.1 ###
# Age distribution of spawning adults;
# Min % of each age class of adults, Age 4 >35%, Age 5+ >20%

### 5.2 ###
# Size distribution of juveniles; Variation in juvenile abundance of each life stage
# (fry, parr, yearling): variation across years --------------------------------
#



### 5.3 ###
# Diversity of size and timing of juveniles at ocean entry ---------------------
# size distribution & month of juveniles ---------------------------------------
# shannon diversity index
# TODO figure out why month is not showing up
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

### 5.4 ###
# Amount and relative % of available habitat of different types (measured in area and days)
# TODO check in on this

### 6 ###
# marine derived nutrient ------------------------------------------------------

produce_marine_nutrient_pm <- function(model_results, scenario_name){
# prep chanel area (sqmt)
  chanel_area <- read_csv("data-raw/channel_areas.csv") |>
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

  produce_spawner_abundance_pm(model_results) |>
    mutate(area_sqmt = ifelse(scenario == "Max Habitat",
                              max_hab_total_area_sqmt, sit_total_area_sqmt),
           fish_total_pounds = avg_annual_spawners * 21, # TODO assuming ~21 pounds
           marine_derived_nutrient_pounds_per_sqmt = fish_total_pounds / area_sqmt) |> glimpse()
}


# TODO figure out what to do with these...
juv_size_dist <- function(model_results, scenario_name) {
  juveniles <- model_results$juveniles_at_chipps |>
    as_tibble() |>
    mutate(year = as.numeric(year)) |>
    rename(location = watershed) |>
    group_by(year, month, location, size) |>
    summarise(value = sum(juveniles_at_chipps, na.rm = TRUE)) |>
    mutate(performance_metric = "Juveniles at Ocean Entry",
           scenario = scenario_name) |> glimpse()

  return(juveniles)
}

adult_return_age <- function(model_results, scenario_name) {
  adults <- model_results$returning_adults |>
    mutate(performance_metric = "Adult Age of Return",
           scenario = scenario_name) |>
    rename(location = watershed) |> glimpse()

  return(adults)
}




