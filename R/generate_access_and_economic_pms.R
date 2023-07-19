library(tidyverse)

### 10	###
# Land/water access – Indigenous/cultural --------------------------------------
# Value from tribes, flowwest does not need to do

### 11 ###
# Managed wetlands -------------------------------------------------------------
# Value from elsewhere, flowwest does not need to do

### 12.1 ###
# harvest Annual number of adults in rivers
# (above abundance numbers required to meet biological objectives) -------------
#
#' Produce Spawner Abundance Above Biological Objective Performance Metrics
#'
#' Calculate the spawner abundance above the biological objective performance metrics based on model results.
#'
#' @param model_results_df A data frame containing the model results.
#' @param scenario The scenario name for which the spawner abundance is calculated.
#' @param ocean_harvest_seperate Boolean indicating whether ocean harvest is separated from remaining spawners.
#' @param ocean_harvest The value of ocean harvest if ocean_harvest_seperate is TRUE.
#'
#' @return A data frame containing the spawner abundance above biological objective performance metrics.
#'   - \code{avg_remaining_annual_spawners}: The average remaining annual spawners above the biological objective.
#'   - \code{min_remaining_spawners}: The minimum remaining spawners above the biological objective.
#'   - \code{max_remaining_spawners}: The maximum remaining spawners above the biological objective.
#'
#' @examples
#' model_results <- read.csv("model_results.csv")
#' spawner_metrics <- produce_spawner_abundance_above_biological_objective_pm(model_results, "Scenario1", TRUE, 100)
#' print(spawner_metrics)
#'
#' @export
produce_spawner_abundance_above_biological_objective_river_pm <- function(model_results_df){
  annual_spawners <- model_results_df |>
    filter(performance_metric == "Adult Age of Return") |>
    filter(year > 5) |>
    group_by(location, year, scenario) |>
    summarise(river_toals = sum(value, na.rm = TRUE),
              remaining_spawners = (river_toals - 500) * .5) |> # *.5 for ocean harvest
    ungroup() |>
    group_by(year, scenario) |>
    summarize(total_remaining_spawners = sum(remaining_spawners, na.rm = T)) |>
    ungroup() |>
    group_by(scenario) |>
    summarize(avg_remaining_annual_spawners = mean(total_remaining_spawners, na.rm = T),
              min_remaining_spawners = min(total_remaining_spawners, na.rm = T),
              max_remaining_spawners = max(total_remaining_spawners, na.rm = T)) |> glimpse()
  return(annual_spawners)
}

### 12.2 ###
### harvest TODO need clarification on how this is different from the one above (added in conditional ocean harvest statement)
produce_spawner_abundance_above_biological_objective_ocean_pm <- function(model_results_df){
  annual_spawners <- model_results_df |>
    filter(performance_metric == "Adult Age of Return") |>
    filter(year > 5) |>
    group_by(location, year, scenario) |>
    summarise(river_toals = sum(value, na.rm = TRUE),
              remaining_spawners = river_toals - 500) |>
    ungroup() |>
    group_by(year, scenario) |>
    summarize(total_remaining_spawners = sum(remaining_spawners, na.rm = T)) |>
    ungroup() |>
    group_by(scenario) |>
    summarize(avg_remaining_annual_spawners = mean(total_remaining_spawners, na.rm = T),
              min_remaining_spawners = min(total_remaining_spawners, na.rm = T),
              max_remaining_spawners = max(total_remaining_spawners, na.rm = T)) |> glimpse()
  return(annual_spawners)
}

### 12.3 & 12.4 ###
# harvest % of years where annual number of adults in rivers and oceans (above abundance numbers required to meet biological objectives)
# is >= 200K (minimum annual number of harvestable fish to support Indigenous, recreational, and commercial uses)
# How many consecutive
#' Produce Percent Harvestable Above Threshold Performance Metrics
#'
#' Calculate the percentage of harvestable years above a threshold performance metric based on model results.
#'
#' @param model_results_df A data frame containing the model results.
#' @param scenario The scenario name for which the percentage of harvestable years is calculated.
#'
#' @return The percentage of harvestable years above the threshold performance metric and the maximum number of consecutive harvestable years.
#'
#' @examples
#' model_results <- read.csv("model_results.csv")
#' percent_harvestable <- produce_percent_harvestable_abv_threshold_pm(model_results, "Scenario1")
#' print(percent_harvestable)
#'
#' @export
produce_percent_harvestable_abv_threshold_pm <- function(model_results_df, selected_scenario) {
  annual_spawners <- model_results_df |>
    filter(performance_metric == "Adult Age of Return", scenario == selected_scenario) |>
    filter(year > 5) |>
    group_by(location, year) |>
    summarise(river_toals = sum(value, na.rm = TRUE),
              remaining_spawners = river_toals - 500) |>
    ungroup() |>
    group_by(year) |>
    summarize(total_remaining_spawners = sum(remaining_spawners, na.rm = T)) |>
    ungroup() |>
    mutate(abv_threshold = ifelse(total_remaining_spawners > 200000, TRUE, FALSE))

  percent_harvestable_years <- (sum(annual_spawners$abv_threshold)/ 20) * 100
  print(paste("Havest allocation is met in", percent_harvestable_years, "% of the 20 model simulation years under the", selected_scenario, "scenario."))

  grouped_tf <- rle(annual_spawners$abv_threshold == TRUE)
  trues <- which(grouped_tf$values == TRUE)
  max_trues <- max(grouped_tf$lengths[c(trues)])
  print(paste("The maximum number of consecutive years that numbers are at a harvestable level throughout the 20 year", scenario, "scenario is:", max_trues))
  returnselected_scenario
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
