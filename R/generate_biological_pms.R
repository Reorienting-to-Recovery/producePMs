library(tidyverse)

# Load in model resutls data ---------------------------------------------------
# source("R/run_model_scenarios.R")
# model_results_df <- create_model_results_dataframe(baseline_model_results, "Baseline", "Fall Run")

### 1 ###
# Average Annual central valley wide abundance ---------------------------------
#' Calculate Annual Spawner Abundance
#'
#' This function calculates the average, minimum, and maximum annual spawner abundance based on model results.
#'
#' @param model_results_df A data frame containing model results.
#'
#' @return A data frame with the calculated average, minimum, and maximum annual spawner abundance per scenario.
#'
#' @import dplyr
#'
#' @examples
#' produce_spawner_abundance_pm(model_results)
#'
#' @export
produce_spawner_abundance_pm <- function(model_results_df){
  annual_spawners <- model_results_df |>
    filter(performance_metric == "1 All Spawners") |>
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
#' Calculate CRR (Total Adult to Returning Natural Adult)
#'
#' This function calculates the average, minimum, and maximum annual CRR (Total Adult to Returning Natural Adult) based on model results.
#'
#' @param model_results_df A data frame containing model results.
#'
#' @return A data frame with the calculated average, minimum, and maximum annual CRR per scenario.
#'
#' @import dplyr
#'
#' @examples
#' produce_crr_pm(model_results)
#'
#' @export
produce_crr_pm <- function(model_results_df){
  crr <- model_results_df |>
    filter(performance_metric == "2.1 CRR: Total Adult to Returning Natural Adult") |>
    group_by(location, scenario) |>
    summarize(average_crr = mean(value, na.rm = TRUE)) |>
    ungroup() |>
    mutate(average_crr = ifelse(average_crr == Inf, 0, average_crr)) |>
    group_by(scenario) |>
    summarize(avg_annual_crr = mean(average_crr, na.rm = T),
              min_annual_crr = min(average_crr, na.rm = T),
              max_annual_crr = max(average_crr, na.rm = T))
  return(crr)
}

produce_crr_geometric_mean_pm <- function(model_results_df){
  geom_mean_calc <- function(watershed, scenario) {
    data <- model_results_df |>
      filter(performance_metric == "2.1 CRR: Total Adult to Returning Natural Adult",
             location == watershed) |>
      select(year, location, scenario, run, performance_metric, value) |>
      mutate(geometric_mean = zoo::rollapply(value, 3, psych::geometric.mean, fill = NA)) |>
      filter(!is.na(geometric_mean))
    return(data)
  }
  watersheds <- rep(fallRunDSM::watershed_labels, 7)
  scenarios_lists <- c(rep("Baseline", 31),
                       rep("Theoretical Max Habitat", 31),
                       rep("No Harvest", 31),
                       rep("No Hatchery", 31),
                       rep("Max Flow", 31),
                       rep("Max Flow & Max Habitat", 31),
                       rep("Max Hatchery", 31))

  res <- purrr::map2(watersheds,scenarios_lists, geom_mean_calc) |> reduce(bind_rows)
  goem_mean_crr <- res |>
    group_by(location, scenario) |>
    summarize(average_crr = mean(geometric_mean, na.rm = TRUE)) |>
    ungroup() |>
    mutate(average_crr = ifelse(average_crr == Inf, 0, average_crr)) |>
    group_by(scenario) |>
    summarize(avg_annual_crr = mean(average_crr, na.rm = T),
              min_annual_crr = min(average_crr, na.rm = T),
              max_annual_crr = max(average_crr, na.rm = T))
 return(goem_mean_crr)
}

### 2.2 ###
# Average growth rate central valley wide --------------------------------------
# TODO check do we want trend in growth rate or last 3 years
#' Calculate Growth Rates of Natural Spawners
#'
#' This function calculates the average, minimum, and maximum annual growth rates of natural spawners based on model results.
#'
#' @param model_results_df A data frame containing model results.
#'
#' @return A data frame with the calculated average, minimum, and maximum annual growth rates per scenario.
#'
#' @import dplyr
#'
#' @examples
#' produce_growth_rate_pm(model_results)
#'
#' @export
produce_growth_rate_pm <- function(model_results_df){
  growth_rates <- model_results_df |>
    filter(performance_metric == "2.2 Growth Rate Spawners",
           value != Inf,
           !is.na(value)) |>
    group_by(year, scenario) |>
    summarize(average_growth_rate = mean(value, na.rm = T)) |>
    ungroup() |>
    group_by(scenario) |>
    summarize(avg_annual_growth_rate = mean(average_growth_rate, na.rm = T),
              min_annual_growth_rate = min(average_growth_rate, na.rm = T),
              max_annual_growth_rate = max(average_growth_rate, na.rm = T))
  return(growth_rates)
}

### 3.1 ###
# Number of Independent populations ------------------------------------------------------
# For now just listing potential in the DSM preread
# Growth rate of prior two years must be positive
# Natural spawners must be greater than 500
# Then we can indicate independent population
#' Calculate Number of Independent Populations
#'
#' This function calculates the number of independent populations based on model results.
#'
#' @param model_results_df A data frame containing model results.
#'
#' @return A data frame with the calculated number of independent populations per scenario.
#'
#' @import dplyr
#' @importFrom tidyr pivot_wider
#'
#' @examples
#' produce_independent_pops_pm(model_results)
#'
#' @export
produce_independent_pops_pm <- function(model_results_df) {
  potential_dependent_pops <- c("Bear River", "Big Chico Creek", "Elder River", "Paynes Creek",  "Stoney Creek", "Thomes Creek")
  ind_pops <- model_results_df |>
    filter(performance_metric %in% c("Natural Spawners", "2.2 Growth Rate Spawners",
                                     "4 PHOS", "2.1 CRR: Total Adult to Returning Natural Adult"),
           !location %in% potential_dependent_pops) |>
    pivot_wider(names_from = performance_metric, values_from = value) |>
    mutate(above_500_spawners = if_else(`Natural Spawners` > 500, TRUE, FALSE),
           phos_less_than_5_percent = ifelse(`4 PHOS` < .05, TRUE, FALSE),
           crr_above_1 = ifelse(`2.1 CRR: Total Adult to Returning Natural Adult` >= 1, TRUE, FALSE),
           growth_rate_above_1 = ifelse(`2.2 Growth Rate Spawners` > 0, TRUE, FALSE),
           independent_population = ifelse(above_500_spawners & phos_less_than_5_percent & growth_rate_above_1 & crr_above_1, TRUE, FALSE)) |>
    group_by(scenario) |>
    summarize(number_independent_populations = sum(independent_population, na.rm = TRUE))
  return(ind_pops)
}

### 3.1.1 ###
# Number of Independent populations ------------------------------------------------------
# Just looks at if fish are present or not
#'
#' This function calculates if populations are present on a trib based on model results.
#'
#' @param model_results_df A data frame containing model results.
#'
#' @return A data frame with the calculated populations per scenario.
#'
#' @examples
#' produce_populations_present_pm(model_results)
#'
#' @export
#'
# TODO check only for ind pops
produce_populations_present_pm <- function(model_results_df) {
  potential_dependent_pops <- c("Bear River", "Big Chico Creek", "Elder River", "Paynes Creek",  "Stoney Creek", "Thomes Creek")

  pops <- model_results_df |>
    filter(performance_metric %in% c("1 All Spawners"),
           !location %in% potential_dependent_pops) |>
    pivot_wider(names_from = performance_metric, values_from = value) |>
    mutate(population = ifelse(`1 All Spawners` > 1, TRUE, FALSE)) |>
    group_by(scenario, year) |>
    summarize(yearly_pops = sum(population, na.rm = TRUE)) |>
    group_by(scenario) |>
    summarise(annual_avg_pops = mean(yearly_pops, na.rm = TRUE)) |> glimpse()
  return(pops)
}


### 3.2 ###
# % of potential independent viable populations in each diversity group per ESU/run
#' Calculate Number of Independent Populations per Diversity Group
#'
#' This function calculates the number of independent populations per diversity group based on model results and a selected run.
#'
#' @param model_results_df A data frame containing model results.
#' @param selected_run A character string specifying the selected run ("fall", "spring", or "winter").
#'
#' @return A data frame with the calculated number of independent populations per diversity group.
#'
#' @import tidyverse
#'
#' @examples
#' produce_independent_pops_per_diversity_group_pm(model_results, "fall")
#'
#' @export
produce_independent_pops_per_diversity_group_pm <- function(model_results_df, selected_run) {
  potential_dependent_pops <- c("Bear River", "Big Chico Creek", "Elder River", "Paynes Creek",  "Stoney Creek", "Thomes Creek")

  # diversity groups same for all runs
  diversity_group <- tibble(location = fallRunDSM::watershed_labels,
                            diversity_group = fallRunDSM::diversity_group)
  ind_pops <- model_results_df |>
    left_join(diversity_group) |>
    filter(performance_metric %in% c("Natural Spawners", "2.2 Growth Rate Spawners",
                                     "4 PHOS", "2.1 CRR: Total Adult to Returning Natural Adult"),
           !location %in% potential_dependent_pops) |>
    pivot_wider(names_from = performance_metric, values_from = value) |>
    mutate(above_500_spawners = if_else(`Natural Spawners` > 500, TRUE, FALSE),
           phos_less_than_5_percent = ifelse(`4 PHOS` < .05, TRUE, FALSE),
           crr_above_1 = ifelse(`2.1 CRR: Total Adult to Returning Natural Adult` >= 1, TRUE, FALSE),
           growth_rate_above_1 = ifelse(`2.2 Growth Rate Spawners` > 0, TRUE, FALSE),
           independent_population = ifelse(above_500_spawners & phos_less_than_5_percent & growth_rate_above_1 & crr_above_1, TRUE, FALSE)) |>
    group_by(scenario, diversity_group) |>
    summarize(number_independent_populations = sum(independent_population, na.rm = TRUE))
  return(ind_pops)
}
### 3.2 ###
### TODO
# Number of dependent populations in each diversity group per ESU/run
#' Calculate Number of Dependent Populations per Diversity Group
#'
#' This function calculates the number of dependent populations per diversity group based on model results and a selected run.
#'
#' @param model_results_df A data frame containing model results.
#' @param selected_run A character string specifying the selected run ("fall", "spring", or "winter").
#'
#' @return A data frame with the calculated number of dependent populations per diversity group.
#'
#' @import tidyverse
#'
#' @examples
#' produce_dependent_pops_per_diversity_group_pm(model_results, "fall")
#' @export
produce_dependent_pops_per_diversity_group_pm <- function(model_results_df, selected_run) {
  # diversity groups same for all runs

  diversity_group <- tibble(location = fallRunDSM::watershed_labels,
                            diversity_group = fallRunDSM::diversity_group)
  # adult pop exists not the same, use adult seeds to determine
  potential_dependent_pops <- c("Bear River", "Big Chico Creek", "Elder River", "Paynes Creek",  "Stoney Creek", "Thomes Creek")
  dep_pops <- model_results_df |>
    left_join(diversity_group) |>
    filter(location %in% potential_dependent_pops) |>
    filter(performance_metric %in% c("1 All Spawners")) |>
    pivot_wider(names_from = performance_metric, values_from = value) |>
    mutate(dependent_population = ifelse(`1 All Spawners` >= 1, TRUE, FALSE)) |>
    group_by(scenario) |>
    summarize(number_dependent_populations = sum(dependent_population, na.rm = TRUE))
  return(dep_pops)
}

### 4 ###
# PHOS - Salmon genetic diversity ----------------------------------------------
# TODO check max phos seems low
#' Calculate PHOS Levels
#'
#' This function calculates the average, minimum, and maximum annual Proportion hatchery on spawning grounds (PHOS) levels based on model results.
#'
#' @param model_results_df A data frame containing model results.
#'
#' @return A data frame with the calculated average, minimum, and maximum annual PHOS levels.
#'
#' @import dplyr
#'
#' @examples
#' produce_phos_pm(model_results)
#'
#' @export
produce_phos_pm <- function(model_results_df){
  phos <- model_results_df |>
    filter(performance_metric %in% c("4 PHOS", "1 All Spawners")) |>
    pivot_wider(names_from = performance_metric, values_from = value) |>
    group_by(year, scenario, run) |>
    summarize(weighted_average_phos = weighted.mean(`4 PHOS`, `1 All Spawners`, na.rm = T)) |>
    ungroup() |>
    group_by(scenario, run) |>
    summarize(avg_annual_phos = mean(weighted_average_phos, na.rm = T),
              min_annual_phos = min(weighted_average_phos, na.rm = T),
              max_annual_phos = max(weighted_average_phos, na.rm = T))
  return(phos)
}

### 5.1 ###
# Age distribution of spawning adults;
# Min % of each age class of adults, Age 4 >35%, Age 5+ >20%
# probably can just do based on return ratios
# Calculate Total Years/Locations Meeting Categorical Return Age Criteria
#' This function calculates the number of locations meeting the categorical return age criteria based on model results.
#'
#' @param model_results_df A data frame containing model results.
#'
#' @return A data frame with the count of locations meeting the categorical return age criteria per scenario and run.
#'
#' @import dplyr
#'
#' @examples
#' produce_categorical_return_age_pm(model_results)
#'
#' @export
produce_categorical_return_age_pm <- function(model_results_df) {
  adults <- model_results_df |>
    filter(year > 5) |>
    # select(-size_or_age) |>
    filter(performance_metric == "Adult Age of Return") |>
    select(-month) |>
    group_by(year, location, size_or_age, scenario, run) |>
    summarise(total_spawners = round(sum(value, na.rm = TRUE))) |>
    ungroup() |>
    pivot_wider(names_from = size_or_age, values_from = total_spawners) |>
    mutate(total_spawners = round(`2` + `3` + `4` + `5`),
           perc_age_2 = round(`2`/total_spawners * 100),
           perc_age_3 = round(`3`/total_spawners * 100),
           perc_age_4 = round(`4`/total_spawners * 100),
           perc_age_5 = round(`5`/total_spawners * 100),
           meets_age_criteria = ifelse(perc_age_4 > 35 & perc_age_5 > 20, TRUE, FALSE)) |>
    group_by(scenario, run) |>
    summarize(year_locations_meeting_age_criteria = sum(meets_age_criteria, na.rm = TRUE))

  return(adults)
}
### 5.2 ###
# Size distribution of juveniles; Variation in juvenile abundance of each life stage
# (fry, parr, yearling): variation across years --------------------------------
# shannon diversity index
#' Calculate Shannon Diversity Index for Juvenile Size
#'
#' This function calculates the Shannon Diversity Index (DI) for juvenile size based on model results.
#'
#' @param model_results_df A data frame containing model results.
#'
#' @return A data frame with the calculated Shannon Diversity Index (DI) for juvenile size.
#'
#' @import dplyr
#'
#' @examples
#' produce_shannon_div_ind_size_pm(model_results)
#'
#' @export

produce_shannon_div_ind_size_pm <- function(model_results_df){
  annual_total <- model_results_df |>
    filter(performance_metric == "Juveniles Size at Ocean Entry") |>
    group_by(year, scenario, run) |>
    summarize(total_juveniles = sum(value, na.rm = T))

  shannon_di <- model_results_df |>
    filter(performance_metric == "Juveniles Size at Ocean Entry") |>
    group_by(year, size_or_age, scenario, run) |>
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
    summarise(avg_annual_shannon_di = mean(shannon_index))
  return(shannon_di)
}

### 5.3 ###
# Diversity of size and timing of juveniles at ocean entry ---------------------
# size distribution & month of juveniles ---------------------------------------
#' Calculate Shannon Diversity Index for Juvenile Size and Timing
#'
#' This function calculates the Shannon Diversity Index (DI) for juvenile size and timing based on model results.
#'
#' @param model_results_df A data frame containing model results.
#'
#' @return A data frame with the calculated Shannon Diversity Index (DI) for juvenile size and timing.
#'
#' @import dplyr
#' @import tidyverse
#' @examples
#' produce_shannon_div_ind_size_and_timing_pm(model_results)
#'
#' @export
# TODO check in on the timing
produce_shannon_div_ind_size_and_timing_pm <- function(model_results_df){
  annual_total <- model_results_df |>
    filter(performance_metric == "Juveniles Size at Ocean Entry") |>
    group_by(year, scenario, run) |>
    summarize(total_juveniles = sum(value, na.rm = T))

  shannon_di <- model_results_df |>
    filter(performance_metric == "Juveniles Size at Ocean Entry") |>
    group_by(year, size_or_age, scenario, month, run) |>
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
    summarise(avg_annual_shannon_di = mean(shannon_index))
  return(shannon_di)
}

### 5.4 ###
#' Calculate Carrying Capacity vs. Abundance
#'
#' This function calculates the carrying capacity vs. abundance based on the provided model
#' results dataframe, model parameters, and run. Only spawning habitat is considered, not holding.
#'
#' @param model_results_df The model results dataframe.
#' @param model_parameters The model parameters.
#' @param run The selected run.
#'
#' @return A tibble with carrying capacity vs. abundance values.
#' @export
#'
#' @examples
#' model_results <- data.frame(
#'   spawning_habitat = c(100, 200, 150, 250),
#'   location = c("Location A", "Location B", "Location A", "Location B"),
#' )
#' model_parameters <- list(spawning_habitat = data.frame(location = c("Location A", "Location B"), value = 300))
#'
#' produce_carrying_capacity_vs_abundance(model_results, model_parameters, "fall")
produce_floodplain_over_inchannel_habitat <- function(model_results_df, model_parameters, selected_run, scenario){
  rearing_months <- switch(selected_run,
                           "fall" = c(1:8),
                           "spring" = c(1:5),
                           "winter" = c(1:5),
                           "late fall" = c(4:11))
  month_lookup <- tibble(month_num = c(1:12), month = month.abb)
  inchannel_habitat <- model_parameters$inchannel_habitat_fry |>
    as_tibble() |>
    mutate(location = fallRunDSM::watershed_labels) |>
    pivot_longer(cols = -location, names_to = "year_date", values_to = "inchannel_habitat") |>
    separate(year_date, into = c("month", "year")) |>
    left_join(month_lookup) |>
    filter(month_num  %in% rearing_months) |> glimpse()

  floodplain_habitat <- model_parameters$floodplain_habitat |>
    as_tibble() |>
    mutate(location = fallRunDSM::watershed_labels) |>
    pivot_longer(cols = -location, names_to = "year_date", values_to = "floodplain_habitat") |>
    separate(year_date, into = c("month", "year")) |>
    left_join(month_lookup) |>
    filter(month_num  %in% rearing_months) |> glimpse()

  joined_habitat <- left_join(inchannel_habitat, floodplain_habitat) |>
    transmute(year = as.numeric(year),
              scenario = scenario,
              floodplain_over_inchannel = floodplain_habitat/inchannel_habitat) |>
    filter(!is.na(scenario)) |>
    group_by(scenario) |>
    summarize(avg_annual_floodplain_over_inchannel = mean(floodplain_over_inchannel, na.rm = T),
              min_annual_floodplain_over_inchannel = min(floodplain_over_inchannel, na.rm = T),
              max_annual_floodplain_over_inchannel = max(floodplain_over_inchannel, na.rm = T)) |> glimpse()

}


### 6 ###
# marine derived nutrient ------------------------------------------------------
##' Calculate marine nutrient production per square meter
#'
#' This function calculates the marine nutrient production per square meter based on model results and scenario information.
#'
#' @param model_results_df A data frame containing model results.
#' @param scenario_name A character string specifying the scenario name.
#'
#' @return A modified data frame with additional columns representing marine nutrient production per square meter.
#'
#' @import dplyr
#' @importFrom readr read_csv
#'
#' @examples
#' produce_marine_nutrient_pm(model_results, "Max Habitat")
#'
#' @export
produce_marine_nutrient_pm <- function(model_results_df, scenario_name){
# prep chanel area (sqmt)
  chanel_area <- read_csv("data-raw/channel_areas.csv") |>
    mutate(sit_width = `width - sit` * 0.3048,
           sit_length = `length - sit` * 0.3048,
           add_max_hab_width = `width - add tmh`* 0.3048,
           add_max_hab_length = `length - add tmh` * 0.3048,
           sit_area = sit_width * sit_length,
           add_tmh_area = add_max_hab_width * add_max_hab_length)

  sit_total_area_sqmt <- chanel_area |>
    pull(sit_area) |>
    sum(na.rm = T)

  max_hab_total_area_sqmt <- chanel_area |>
    pull(add_tmh_area) |>
    sum(na.rm = T) + sit_total_area_sqmt

  produce_spawner_abundance_pm(model_results_df) |>
    mutate(area_sqmt = ifelse(scenario == "Max Habitat",
                              max_hab_total_area_sqmt, sit_total_area_sqmt),
           fish_total_pounds = avg_annual_spawners * 21, # TODO assuming ~21 pounds
           marine_derived_nutrient_pounds_per_sqmt = fish_total_pounds / area_sqmt)
}

### 7 ###
# time to recovery -------------------------------------------------------------
# TODO add this
# "Notes from 5/30: Calculate for each year if CRR > 1, phos < 5, if min 20% in
# each juv size class, and if adults are Age 4 >35%, Age 5+ >20% for each individual
# population - if these are both true check for abundance > 500 (independent population).
# Then by diversity groups check that all independent populations are fullfilling
# above criteria. (state when we meet recovery objectives within each diversity group)
# SAT meeting check on logic for natural and hatchery adults return proportions"
#
#' Calculate Time to Recovery for Two Diversity Groups
#'
#' This function calculates the time to recovery for two diversity groups based on the provided model results dataframe and selected run.
#'
#' @param model_results_df The model results dataframe.
#' @param selected_run The selected run.
#'
#' @return The time to recovery for two diversity groups, or a message indicating that no two diversity groups showed recovery throughout the simulation.
#' @export
#'
#' @examples
#' model_results <- data.frame(
#'   performance_metric = c("Natural Spawners", "Growth Rate Natural Spawners", "PHOS", "CRR: Total Adult to Returning Natural Adult"),
#'   value = c(100, 1.5, 0.03, 0.8),
#'   location = c("Location A", "Location B", "Location C", "Location D"),
#'   year = c(2021, 2022, 2023, 2024),
#'   scenario = c("Scenario 1", "Scenario 2", "Scenario 1", "Scenario 2"),
#'   run = c(1, 1, 2, 2)
#' )
#'
#' produce_time_to_recovery_pm(model_results, 2)
produce_time_to_recovery_pm <- function(model_results_df, selected_run) {
  # diversity groups same for all runs
  diversity_group <- tibble(location = fallRunDSM::watershed_labels,
                            diversity_group = fallRunDSM::diversity_group)
  ind_pops <- model_results_df |>
    left_join(diversity_group) |>
    filter(performance_metric %in% c("Natural Spawners", "2.2 Growth Rate Spawners",
                                     "4 PHOS", "2.1 CRR: Total Adult to Returning Natural Adult")) |>
    pivot_wider(names_from = performance_metric, values_from = value) |>
    mutate(above_500_spawners = if_else(`Natural Spawners` > 500, TRUE, FALSE),
           phos_less_than_5_percent = ifelse(`4 PHOS` < .05, TRUE, FALSE),
           growth_rate_above_1 = ifelse(`2.2 Growth Rate Spawners` > 0, TRUE, FALSE),
           crr_above_1 = ifelse(`2.1 CRR: Total Adult to Returning Natural Adult` > 1, TRUE, FALSE),
           meet_req = ifelse(above_500_spawners & phos_less_than_5_percent & growth_rate_above_1 & crr_above_1 ,TRUE , FALSE)) |>
    group_by(diversity_group, year, scenario, run) |>
    summarise(diversity_group_meets_req = ifelse(all(meet_req) == TRUE, TRUE, FALSE)) |>
    filter(diversity_group_meets_req == TRUE)

  if (nrow(ind_pops) > 0){
    years <- ind_pops |>
      group_by(year) |>
      summarise(num_dg_meeting_reqs = sum(diversity_group_meets_req)) |>
      filter(num_dg_meeting_reqs > 2) |>
      pull(year)
    two_dg_recovered <- min(years)
    return(two_dg_recovered)
  } else return(print("No 2 diversity groups showed recovery throuout the simulation"))
}




