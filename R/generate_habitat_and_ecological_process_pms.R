library(tidyverse)

### 8.1 ###
# Wetted acre days of suitable juvenile rearing habitat ------------------------
# Present mean wetted acre days (mulitply total inchannel rearing & floodplain
# habitat values by day in month, sum across all months, take annual average) report mean and range of values
#
# # Describe in docs Only spawning habitat is considered, not holding.
#
#' Calculate Juvenile Wetted Acre-Day
#'
#' Calculate the juvenile wetted acre-day based on model parameters, scenario, and selected run.
#'
#' @param model_parameters A list containing the model parameters.
#' @param scenario The scenario name for which the juvenile wetted acre-day is calculated.
#' @param selected_run The selected run for which the juvenile wetted acre-day is calculated.
#'
#' @return A data frame containing the juvenile wetted acre-day.
#'
#' @examples
#' model_params <- fallRunDSM::r_to_r_baseline_params
#' juv_hab <- produce_juvenile_wetted_acre_day_pm(model_params, "Baseline", "fall")
#' print(juv_hab)
#'
#' @export

produce_juvenile_wetted_acre_day_pm <- function(model_parameters, scenario, selected_run){
  rearing_months <- switch(selected_run,
                            "fall" = c(1:8),
                            "spring" = c(1:5),
                            "winter" = c(1:5),
                            "late fall" = c(4:11))

  length_rm <- length(rearing_months)
  fry_juv_cutoff <- length_rm/2
  ic_habitat_fry <- model_parameters$inchannel_habitat_fry[ , rearing_months[1:fry_juv_cutoff], ] |>
    as_tibble() |>
    mutate(watershed = fallRunDSM::watershed_labels) |>
    pivot_longer(cols = -watershed, names_to = "year_date", values_to = "rearing_hab") |>
    separate(year_date, into = c("month", "year"))
  ic_habitat_juv <- model_parameters$inchannel_habitat_juvenile[ , fry_juv_cutoff:length_rm, ] |>
    as_tibble() |>
    mutate(watershed = fallRunDSM::watershed_labels) |>
    pivot_longer(cols = -watershed, names_to = "year_date", values_to = "rearing_hab") |>
    separate(year_date, into = c("month", "year"))
  floodplain_habitat <- model_parameters$floodplain_habitat[ , rearing_months, ] |>
    as_tibble() |>
    mutate(watershed = fallRunDSM::watershed_labels) |>
    pivot_longer(cols = -watershed, names_to = "year_date", values_to = "rearing_hab") |>
    separate(year_date, into = c("month", "year"))

  rearing_habitat <- bind_rows(ic_habitat_fry, ic_habitat_juv, floodplain_habitat) |>
    mutate(rearing_hab = DSMhabitat::square_meters_to_acres(rearing_hab),
           wetted_acre_day = rearing_hab * 30) |> # assume 30 days in month
    group_by(year) |>
    summarize(annual_wetted_t_acre_day = sum(wetted_acre_day, na.rm = TRUE)/1000) |>
    ungroup() |>
    mutate(scenario = scenario,
           run = selected_run) |>
    group_by(scenario, run) |>
    summarize(avg_wetted_t_acre_day = mean(annual_wetted_t_acre_day, na.rm = T),
              min_wetted_t_acre_day = min(annual_wetted_t_acre_day, na.rm = T),
              max_wetted_t_acre_day = max(annual_wetted_t_acre_day, na.rm = T))
  return(rearing_habitat)
}

### 8.2 ###
# Wetted acre days of suitable spawning habitat --------------------------------
# Present mean wetted acre days (mulitply total inchannel rearing & floodplain
# habitat values by day in month, sum across all months, take annual average) report mean and range of values
# # TODO just do spawning for now - check on what our holding habitat actually is and maybe capture holding wetted acre days seperatly
#
# Describe in docs Only spawning habitat is considered, not holding.
#
#' Calculate Spawning Wetted Acre-Day
#'
#' Calculate the spawning wetted acre-day based on model parameters, scenario, and selected run.
#'
#' @param model_parameters A list containing the model parameters.
#' @param scenario The scenario name for which the spawning wetted acre-day is calculated.
#' @param selected_run The selected run for which the spawning wetted acre-day is calculated.
#'
#' @return A data frame containing the spawning wetted acre-day.
#'
#' @examples
#' model_params <- fallRunDSM::r_to_r_baseline_params
#' spawn_hab <- produce_spawning_wetted_acre_day_pm(model_params, "Baseline", "fall")
#' print(spawn_hab)
#'
#' @export
produce_spawning_wetted_acre_day_pm <- function(model_parameters, scenario, selected_run){
  spawning_months <- switch(selected_run,
                            "fall" = c(10:12),
                            "spring" = c(7:10),
                            "winter" = c(5:7),
                            "late fall" = c(10:12, 1:2))
  month_lookup <- tibble(month_num = c(1:12), month = month.abb)
  spawn_hab <- model_parameters$spawning_habitat |>
    as_tibble() |>
    mutate(watershed = fallRunDSM::watershed_labels) |>
    pivot_longer(cols = -watershed, names_to = "year_date", values_to = "spawning_habitat") |>
    separate(year_date, into = c("month", "year")) |>
    left_join(month_lookup) |>
    filter(month_num  %in% spawning_months) |>
    mutate(spawning_habitat = DSMhabitat::square_meters_to_acres(spawning_habitat),
           wetted_acre_day = spawning_habitat * 30) |> # assume 30 days in month
    group_by(year) |>
    summarize(annual_wetted_t_acre_day = sum(wetted_acre_day, na.rm = TRUE)/1000) |>
    ungroup() |>
    mutate(scenario = scenario,
           run = selected_run) |>
    group_by(scenario, run) |>
    summarize(avg_wetted_t_acre_day = mean(annual_wetted_t_acre_day, na.rm = T),
              min_wetted_t_acre_day = min(annual_wetted_t_acre_day, na.rm = T),
              max_wetted_t_acre_day = max(annual_wetted_t_acre_day, na.rm = T))
    return(spawn_hab)
}

### 8.3 ###
# Spawning habitat decay rate (as a proxy for riverine condition) --------------
# convert this to habitat loss. yearly loss in capacity relative to the maximum
# over the 20 year period - sum over 20 year period
#' Calculate Total Habitat Decay
#'
#' Calculate the total habitat decay based on model parameters and scenario.
#'
#' @param model_parameters A list containing the model parameters.
#' @param scenario The scenario name for which the total habitat decay is calculated.
#'
#' @return A data frame containing the total habitat decay.
#'
#' @examples
#' model_params <- fallRunDSM::r_to_r_baseline_params
#' total_decay <- total_habitat_decay(model_params, "Baseline")
#' print(total_decay)
#'
#' @export
total_habitat_decay <- function(model_params, scenario, run){
  decay_multiplier <- model_params$spawn_decay_multiplier
  # TODO confused about this - what is going on with decay - I thought it was driven by flow
  proportion_decay <- tibble(scenario = scenario,
                             prop_decay = 1 - mean(decay_multiplier),
                             run = run)

  return(proportion_decay)
}


### 9.1
# Wetted acre days - total days floodplain activation occurs -------------------
#' Produce Wetted Acre-Day Floodplain Activation Performance Metrics
#'
#' Calculate the wetted acre-day floodplain based on model parameters and scenario.
#'
#' @param model_parameters A data frame containing the model parameters.
#' @param scenario The scenario name for which the wetted acre-day floodplain activation metrics are calculated.
#' @param selected_run The selected run for which the wetted acre-day floodplain activation metrics are calculated.
#'
#' @return A data frame containing the wetted acre-day floodplain activation metrics.
#'
#' @examples
#' model_params <- read.csv("model_parameters.csv")
#' floodplain_activation <- produce_wetted_acre_day_floodplain_activation_pm(model_params, "Scenario1", "Fall")
#' print(floodplain_activation)
#'
#' @export

produce_wetted_acre_day_floodplain_activation_pm <- function(model_parameters, scenario, selected_run){
  rearing_months <- switch(selected_run,
                           "fall" = c(1:8),
                           "spring" = c(1:5),
                           "winter" = c(1:5),
                           "late fall" = c(4:11))

  floodplain_habitat <- model_parameters$floodplain_habitat[ , rearing_months, ] |>
    as_tibble() |>
    mutate(watershed = fallRunDSM::watershed_labels) |>
    pivot_longer(cols = -watershed, names_to = "year_date", values_to = "rearing_hab") |>
    separate(year_date, into = c("month", "year"))

  fp_habitat <- floodplain_habitat |>
    mutate(rearing_hab = DSMhabitat::square_meters_to_acres(rearing_hab),
           wetted_acre_day = rearing_hab * 30) |> # assume 30 days in month
    group_by(year) |>
    summarize(annual_wetted_t_acre_day = sum(wetted_acre_day, na.rm = TRUE)/1000) |>
    ungroup() |>
    mutate(scenario = scenario,
           run = selected_run) |>
    group_by(scenario, run) |>
    summarize(avg_wetted_t_acre_day = mean(annual_wetted_t_acre_day, na.rm = T),
              min_wetted_t_acre_day = min(annual_wetted_t_acre_day, na.rm = T),
              max_wetted_t_acre_day = max(annual_wetted_t_acre_day, na.rm = T))
  return(fp_habitat)
}

### 9.1 & 9.3
### TODO update to seperate - one wetted acre days floodplain and one floodplain habitat associated with 2 year 30 day floods
# Wetted acre days - total days floodplain activation occurs -------------------
#' Produce Wetted Acre-Day Floodplain Activation Performance Metrics
#'
#' Calculate the wetted acre-day floodplain activation above 2 - year 30 day flood performance metrics based on model parameters and scenario.
#'
#' @param model_parameters A data frame containing the model parameters.
#' @param scenario The scenario name for which the wetted acre-day floodplain activation metrics are calculated.
#' @param selected_run The selected run for which the wetted acre-day floodplain activation metrics are calculated.
#'
#' @return A data frame containing the wetted acre-day floodplain activation metrics.
#'
#' @examples
#' model_params <- read.csv("model_parameters.csv")
#' floodplain_activation <- produce_wetted_acre_day_floodplain_activation_pm(model_params, "Scenario1", "Fall")
#' print(floodplain_activation)
#'
#' @export
produce_2yr_30d_floodplain_acres_pm <- function(model_parameters, scenario, selected_run){
  year_types <- waterYearType::water_year_indices |>
    filter(location == "Sacramento Valley") |>
    rename(water_year = WY) |>
    mutate(year_type = ifelse(Yr_type %in% c("Wet", "Above Normal"), "wet", "dry")) |>
    select(water_year, year_type)

  dry_years <- year_types |> filter(year_type == "dry") |> pull(water_year)
  wet_years <- year_types |> filter(year_type == "wet") |> pull(water_year)

  dry_year_scenario_flow <- bind_rows(DSMflow::flows_cfs$eff_sac |>
                                        filter(year(date) %in% as.numeric(dry_years)), DSMflow::flows_cfs$biop_itp_2018_2019 |>
                                        filter(year(date) %in% as.numeric(wet_years)))

  flow_data <- switch(scenario,
                      "Baseline" = DSMflow::flows_cfs$biop_itp_2018_2019,
                      "Theoretical Max Habitat" = DSMflow::flows_cfs$biop_itp_2018_2019,
                      "No Harvest" = DSMflow::flows_cfs$biop_itp_2018_2019,
                      "No Hatchery" = DSMflow::flows_cfs$biop_itp_2018_2019,
                      "Max Flow" = DSMflow::flows_cfs$run_of_river,
                      "Max Flow & Max Habitat" = DSMflow::flows_cfs$run_of_river,
                      "Dry Years" = dry_year_scenario_flow, # fix this...
                      "Habitat and Hatchery" = DSMflow::flows_cfs$biop_itp_2018_2019,
                      "Kitchen Sink" = DSMflow::flows_cfs$eff_sac,
                      "Max Hatchery" = DSMflow::flows_cfs$biop_itp_2018_2019,
                      "Planned Plus" = DSMflow::flows_cfs$biop_itp_2018_2019) |> # TODO update
    mutate("Lower-mid Sacramento River" = 35.6/58 * `Lower-mid Sacramento River1` + 22.4/58 * `Lower-mid Sacramento River2`) |>
    select(-`Lower-mid Sacramento River1`, -`Lower-mid Sacramento River2`)
  create_threshold_df <- function(watershed) {
    data <- flow_data |>
      filter(year(date) > 1979, year(date) < 2000) |>
      select(watershed, date) |>
      rename(flow_cfs = watershed)

    dur_30 <-  data |>
      mutate(water_year = ifelse(lubridate::month(date) %in% 10:12, lubridate::year(date) + 1, lubridate::year(date))) |>
      group_by(water_year) |>
      mutate(roll_mean = zoo::rollapply(flow_cfs, FUN = min,
                                        width = lubridate::month(date), fill = NA, align = "left")) |>
      summarise(stat_in_duration = mean(roll_mean, na.rm = TRUE)) |>
      mutate(dist = round(cume_dist(-stat_in_duration), 3)) |>
      arrange(dist)

    interpolate_probs_30 <- approxfun(x = dur_30$dist, y = dur_30$stat_in_duration)
    d30 <- interpolate_probs_30(0.5)
    threshold_df <- tibble(watershed = watershed,
                           threshold = d30)
    return(threshold_df)
  }
  watersheds <- fallRunDSM::watershed_labels[-c(17, 22)] # removes bypasses
  thresholds <- purrr::map(watersheds, create_threshold_df) |> reduce(bind_rows)

  flood_summary <- flow_data |>
    filter(year(date) > 1979, year(date) < 2000) |>
    pivot_longer(cols = -date, names_to = "watershed", values_to = "flow_cfs") |>
    left_join(thresholds) |>
    mutate(scenario = scenario,
           run = selected_run,
           month = month.abb[lubridate::month(date)],
           year = as.character(lubridate::year(date))) |>
    mutate(abv_threshold = ifelse(flow_cfs > threshold, TRUE, FALSE)) |>
    filter(abv_threshold) |>
    group_by(watershed) |>
    summarise(min_flow_above_threshold = min(flow_cfs, na.rm = TRUE)) |> glimpse()


  floodplain_habitat <- model_parameters$floodplain_habitat |>
    as_tibble() |>
    mutate(watershed = fallRunDSM::watershed_labels) |>
    pivot_longer(cols = -watershed, names_to = "year_date", values_to = "flooded_hab") |>
    separate(year_date, into = c("month", "year"))

  dates_to_pull_hab <- flow_data |>
    filter(year(date) > 1979, year(date) < 2000) |>
    pivot_longer(cols = -date, names_to = "watershed", values_to = "flow_cfs") |>
    left_join(flood_summary) |>
    mutate(flows_equal = ifelse(flow_cfs == min_flow_above_threshold, TRUE, FALSE)) |>
    filter(flows_equal) |>
    group_by(watershed) |>
    summarize(date = min(date, na.rm = TRUE)) |>
    mutate(year = as.character(year(date)),
           month = month.abb[month(date)]) |> glimpse()

  flood_hab_abv30 <- dates_to_pull_hab |>
    left_join(floodplain_habitat) |>
    mutate(scenario = scenario,
           run = selected_run,
           acres_flooded_hab = square_meters_to_acres(flooded_hab)) |>
    group_by(scenario, run) |>
    summarize(average_acres_at_2yr_30day = sum(acres_flooded_hab, na.rm = TRUE))

  return(flood_hab_abv30)
}


### 9.2 ###
# Functional Flow Metric -------------------------------------------------------
# TODO Not sure on this one - hopefully we will know after Julie




