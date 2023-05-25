library(tidyverse)

# Load in model resutls data ---------------------------------------------------
# source("R/run_model_scenarios.R")
# model_results_df <- create_model_results_dataframe(baseline_model_results, "Baseline", "Fall Run")

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
produce_independent_pops_pm <- function(model_results_df) {
  ind_pops <- model_results_df |>
    filter(performance_metric %in% c("Natural Spawners", "Growth Rate Natural Spawners", "PHOS")) |>
    pivot_wider(names_from = performance_metric, values_from = value) |>
    mutate(above_500_spawners = if_else(`Natural Spawners` > 500, TRUE, FALSE),
           phos_less_than_5_percent = ifelse(`PHOS` < .05, TRUE, FALSE),
           growth_rate_above_1 = ifelse(`Growth Rate Natural Spawners` > 1, TRUE, FALSE),
           independent_population = ifelse(above_500_spawners & phos_less_than_5_percent & growth_rate_above_1, TRUE, FALSE)) |>
    group_by(scenario) |>
    summarize(number_independent_populations = sum(independent_population))
  return(ind_pops)
}

### 3.2 ###
### TODO
# % of potential independent viable populations in each diversity group per ESU/run
produce_independent_pops_per_diversity_group_pm <- function(model_results_df, selected_run) {
  # diversity groups same for all runs
  diversity_group <- tibble(location = fallRunDSM::watershed_labels,
                            diversity_group = fallRunDSM::diversity_group)
  ind_pops <- model_results_df |>
    left_join(diversity_group) |>
    filter(performance_metric %in% c("Natural Spawners", "Growth Rate Natural Spawners", "PHOS")) |>
    pivot_wider(names_from = performance_metric, values_from = value) |>
    mutate(above_500_spawners = if_else(`Natural Spawners` > 500, TRUE, FALSE),
           phos_less_than_5_percent = ifelse(`PHOS` < .05, TRUE, FALSE),
           growth_rate_above_1 = ifelse(`Growth Rate Natural Spawners` > 1, TRUE, FALSE),
           independent_population = ifelse(above_500_spawners & phos_less_than_5_percent & growth_rate_above_1, TRUE, FALSE)) |>
    group_by(scenario, diversity_group) |>
    summarize(number_independent_populations = sum(independent_population))
  return(ind_pops)
}
### 3.2 ###
### TODO
# Number of dependent populations in each diversity group per ESU/run
produce_dependent_pops_per_diversity_group_pm <- function(model_results_df, selected_run) {
  # diversity groups same for all runs
  diversity_group <- tibble(location = fallRunDSM::watershed_labels,
                            diversity_group = fallRunDSM::diversity_group)
  # adult pop exists not the same, use adult seeds to determine
  adult_pop_exists <- switch(selected_run,
                             "fall" = tibble(location = fallRunDSM::watershed_labels,
                                             seeds = ifelse(fallRunDSM::adult_seeds[, 1] == 0, FALSE, TRUE)),
                             "spring" = tibble(location = springRunDSM::watershed_labels,
                                               seeds = ifelse(springRunDSM::adult_seeds[, 1] == 0, FALSE, TRUE)),
                             "winter" = tibble(location = winterRunDSM::watershed_labels,
                                               seeds = ifelse(winterRunDSM::adult_seeds[, 1] == 0, FALSE, TRUE)), )
  ind_pops <- model_results_df |>
    left_join(diversity_group) |>
    left_join(adult_pop_exists) |>
    filter(performance_metric %in% c("Natural Spawners", "Growth Rate Natural Spawners", "PHOS")) |>
    pivot_wider(names_from = performance_metric, values_from = value) |>
    mutate(above_500_spawners = if_else(`Natural Spawners` > 500, TRUE, FALSE),
           phos_less_than_5_percent = ifelse(`PHOS` < .05, TRUE, FALSE),
           growth_rate_above_1 = ifelse(`Growth Rate Natural Spawners` > 1, TRUE, FALSE),
           independent_population = ifelse(above_500_spawners & phos_less_than_5_percent & growth_rate_above_1, TRUE, FALSE),
           dependent_population = ifelse(!independent_population & seeds, TRUE, FALSE)) |>
    group_by(scenario, diversity_group) |>
    summarize(number_dependent_populations = sum(dependent_population))
  return(ind_pops)
}

### 4 ###
# PHOS - Salmon genetic diversity ----------------------------------------------
# TODO check max phos seems low
produce_phos_pm <- function(model_results_df){
  phos <- model_results_df |>
    filter(performance_metric == "PHOS") |>
    group_by(year, scenario) |>
    summarize(average_phos = mean(value, na.rm = T)) |>
    ungroup() |>
    group_by(scenario, run) |>
    summarize(avg_annual_phos = mean(average_crr, na.rm = T),
              min_annual_phos = min(average_crr, na.rm = T),
              max_annual_phos = max(average_crr, na.rm = T))
  return(phos)
}

### 5.1 ###
# Age distribution of spawning adults;
# Min % of each age class of adults, Age 4 >35%, Age 5+ >20%
# TODO figure this one out shouldn't be this hard...
# probably can just do based on return ratios
produce_categorical_return_age_pm <- function(model_results_df) {
  adults <- model_results_df |>
    select(-size) |>
    filter(performance_metric == "Adult Age of Return") |>
    glimpse()

  return(adults)
}
### 5.2 ###
# Size distribution of juveniles; Variation in juvenile abundance of each life stage
# (fry, parr, yearling): variation across years --------------------------------
# shannon diversity index
produce_shannon_div_ind_size_pm <- function(model_results_df){
  annual_total <- model_results_df |>
    filter(performance_metric == "Juveniles Size at Ocean Entry") |>
    group_by(year, scenario, run) |>
    summarize(total_juveniles = sum(value, na.rm = T)) |> glimpse()

  shannon_di <- model_results_df |>
    filter(performance_metric == "Juveniles Size at Ocean Entry") |>
    group_by(year, size, scenario, run) |>
    summarize(frequency = sum(value, na.rm = T)) |>
    ungroup() |>
    left_join(annual_total) |>
    mutate(pi = frequency / total_juveniles,
           ln_pi = log(pi),
           pi_ln_pi = pi * ln_pi) |>
    group_by(year, scenario, run) |>
    summarize(shannon_index = -1 * sum(pi_ln_pi, na.rm = T)) |>
    ungroup() |>
    group_by(scenario, run) |>
    summarise(avg_annual_shannon_di = mean(shannon_index)) |>
    glimpse()
  return(shannon_di)
}

### 5.3 ###
# Diversity of size and timing of juveniles at ocean entry ---------------------
# size distribution & month of juveniles ---------------------------------------
# TODO check if we want div index here
produce_shannon_div_ind_size_and_timing_pm <- function(model_results_df){
  monthly_total <- model_results_df |>
    filter(performance_metric == "Juveniles Size at Ocean Entry") |>
    group_by(year, scenario, month, run) |>
    summarize(total_juveniles = sum(value, na.rm = T)) |> glimpse()

  shannon_di <- model_results_df |>
    filter(performance_metric == "Juveniles Size at Ocean Entry") |>
    group_by(year, size, scenario, month, run) |>
    summarize(frequency = sum(value, na.rm = T)) |>
    ungroup() |>
    left_join(monthly_total) |>
    mutate(pi = frequency / total_juveniles,
           ln_pi = log(pi),
           pi_ln_pi = pi * ln_pi) |>
    group_by(year, scenario, run) |>
    summarize(shannon_index = -1 * sum(pi_ln_pi, na.rm = T)) |>
    ungroup() |>
    group_by(scenario, run) |>
    summarise(avg_annual_shannon_di = mean(shannon_index)) |>
    glimpse()
  return(shannon_di)
}

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


