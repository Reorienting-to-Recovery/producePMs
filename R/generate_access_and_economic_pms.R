library(tidyverse)

### 10	###
# Land/water access – Indigenous/cultural --------------------------------------
# Value from tribes, flowwest does not need to do

### 11 ###
# Managed wetlands -------------------------------------------------------------
# Value from elsewhere, flowwest does not need to do

### 12.1 ###
# Annual number of adults in rivers
# (above abundance numbers required to meet biological objectives) -------------
produce_spawner_abundance_above_biological_objective_pm <- function(model_results_df, scenario, ocean_harvest_seperate = FALSE, ocean_harvest){
  annual_spawners <- model_results_df |>
    filter(performance_metric == "All Spawners") |>
    mutate(remaining_spawners = ifelse(ocean_harvest_seperate, (value - 500 - ocean_harvest), value - 500)) |>
    group_by(year, scenario) |>
    summarize(total_remaining_spawners = sum(remaining_spawners, na.rm = T)) |>
    ungroup() |>
    group_by(scenario) |>
    summarize(avg_remaining_annual_spawners = mean(total_remaining_spawners, na.rm = T),
              min_remaining_spawners = min(total_remaining_spawners, na.rm = T),
              max_remaining_spawners = max(total_remaining_spawners, na.rm = T))
  return(annual_spawners)
}

### 12.2 ###
### TODO need clarification on how this is different from the one above (added in conditional ocean harvest statement)

### 12.3 & 12.4 ###
# % of years where annual number of adults in rivers and oceans (above abundance numbers required to meet biological objectives)
# is >= 200K (minimum annual number of harvestable fish to support Indigenous, recreational, and commercial uses)
# How many consecutive
produce_percent_harvestable_abv_threshold_pm <- function(model_results_df, scenario) {
  annual_spawners <- model_results_df |>
    filter(performance_metric == "All Spawners") |>
    mutate(remaining_spawners = value - 500) |>
    group_by(year, scenario) |>
    summarize(total_remaining_spawners = sum(remaining_spawners, na.rm = T)) |>
    ungroup() |>
    mutate(abv_threshold = ifelse(total_remaining_spawners > 200000, TRUE, FALSE))

  percent_harvestable_years <- (sum(annual_spawners$abv_threshold)/ 20) * 100
  print(paste("Havest allocation is met in", percent_harvestable_years, "% of the 20 model simulation years under the", scenario, "scenario."))

  grouped_tf <- rle(annual_spawners$abv_threshold == TRUE)
  trues <- which(grouped_tf$values == TRUE)
  max_trues <- max(grouped_tf$lengths[c(trues)])
  print(paste("The maximum number of consecutive years that numbers are at a harvestable level throughout the 20 year", scenario, "scenario is:", max_trues))
}

### 13.1 ###
# Water supply and delivery	Annual acre ft of water divertible water for agriculture (average for wetter years and drier years)
# Get from Trend Report

### 13.2 ###
# Annual acre ft of water divertible water for municipalities (average for wetter years and drier years)
# Get from Trend Report

### 14 ###
# Acres in ag production
# Maddee and Alison are working on this
