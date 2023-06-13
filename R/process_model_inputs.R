#' Create Model Inputs Tidy Data Frame
#'
#' This function generates a tidy data frame containing various performance metrics related to habitat and flow for a given set of model parameters, scenario name, and selected run.
#'
#' @param model_parameters A list containing the model parameters, including spawning habitat, in-channel habitat for fry, in-channel habitat for juvenile, floodplain habitat, and other parameters.
#' @param scenario_name A character string specifying the name of the scenario.
#' @param selected_run A character string specifying the selected run.
#'
#' @return A tidy data frame containing the performance metrics.
#'
#' @examples
#' model_params <- fallRunDSM::r_to_r_baseline_params
#' scenario <- "Baseline"
#' run <- "fall"
#' create_model_inputs_tidy_df(model_params, scenario, run)
#'
#' @export
create_model_inputs_tidy_df <- function(model_parameters, scenario_name, selected_run) {
  # set up hab inputs
  rearing_months <- switch(selected_run,
                           "fall" = c(1:8),
                           "spring" = c(1:5),
                           "winter" = c(1:5),
                           "late fall" = c(4:11))
  length_rm <- length(rearing_months)
  fry_juv_cutoff <- length_rm/2
  spawning_months <- switch(selected_run,
                            "fall" = c(10:12),
                            "spring" = c(7:10),
                            "winter" = c(5:7),
                            "late fall" = c(10:12, 1:2))
  month_lookup <- tibble(month_num = c(1:12), month = month.abb)
  # spawn
  spawn_hab <- model_parameters$spawning_habitat[ ,spawning_months, ] |>
    as_tibble() |>
    mutate(watershed = fallRunDSM::watershed_labels,
           habitat_type = "spawning") |>
    pivot_longer(cols = -c(watershed, habitat_type), names_to = "year_date", values_to = "habitat_quantity") |>
    separate(year_date, into = c("month", "year"))
  # rear
  ic_habitat_fry <- model_parameters$inchannel_habitat_fry[ , rearing_months[1:fry_juv_cutoff], ] |>
    as_tibble() |>
    mutate(watershed = fallRunDSM::watershed_labels,
           habitat_type = "fry rearing") |>
    pivot_longer(cols = -c(watershed, habitat_type), names_to = "year_date", values_to = "habitat_quantity") |>
    separate(year_date, into = c("month", "year"))
  ic_habitat_juv <- model_parameters$inchannel_habitat_juvenile[ , fry_juv_cutoff:length_rm, ] |>
    as_tibble() |>
    mutate(watershed = fallRunDSM::watershed_labels,
           habitat_type = "juv rearing") |>
    pivot_longer(cols = -c(watershed, habitat_type), names_to = "year_date", values_to = "habitat_quantity") |>
    separate(year_date, into = c("month", "year"))
  floodplain_habitat <- model_parameters$floodplain_habitat[ , rearing_months, ] |>
    as_tibble() |>
    mutate(watershed = fallRunDSM::watershed_labels,
           habitat_type = "flood") |>
    pivot_longer(cols = -c(watershed, habitat_type), names_to = "year_date", values_to = "habitat_quantity") |>
    separate(year_date, into = c("month", "year"))

 all_wetted_acre_days <- bind_rows(spawn_hab, ic_habitat_fry, ic_habitat_juv, floodplain_habitat) |>
    mutate(habitat_quantity = DSMhabitat::square_meters_to_acres(habitat_quantity),
           wetted_acre_day = habitat_quantity * 30) |> # assume 30 days in month
    group_by(year, location = watershed, habitat_type) |>
    summarize(value = sum(wetted_acre_day, na.rm = TRUE)) |>
    ungroup() |>
    mutate(scenario = scenario_name,
           run = selected_run,
           performance_metric = "Annual Wetted Acre Days")

 # Habitat Decay --------------------------------------------------------------
   habitats <- list(
     spawning_habitat = model_parameters$spawning_habitat,
     inchannel_habitat_fry = model_parameters$inchannel_habitat_fry,
     inchannel_habitat_juvenile = model_parameters$inchannel_habitat_juvenile,
     floodplain_habitat = model_parameters$floodplain_habitat,
     weeks_flooded = model_parameters$weeks_flooded
   )
   # TODO confused about this - what is going on with decay - I thought it was driven by flow
   decayed_habitat <- DSMscenario::load_scenario(scenario = DSMscenario::scenarios$NO_ACTION,
                                                 habitat_inputs = habitats,
                                                 species = DSMscenario::species$FALL_RUN,
                                                 spawn_decay_rate = model_parameters$spawn_decay_rate,
                                                 rear_decay_rate = model_parameters$rear_decay_rate,
                                                 spawn_decay_multiplier = model_parameters$spawn_decay_multiplier,
                                                 stochastic = FALSE)

   spawning_decay <- DSMhabitat::square_meters_to_acres(decayed_habitat$spawning_habitat -
                                                              habitats$spawning_habitat) |>
    as_tibble() |>
    mutate(watershed = fallRunDSM::watershed_labels,
           habitat_type = "spawning") |>
    pivot_longer(cols = -c(watershed, habitat_type), names_to = "year_date", values_to = "decay_acres") |>
    separate(year_date, into = c("month", "year"))

   fp_rearing_decay <- DSMhabitat::square_meters_to_acres(decayed_habitat$floodplain_habitat -
                                                                habitats$floodplain_habitat) |>
     as_tibble() |>
     mutate(watershed = fallRunDSM::watershed_labels,
            habitat_type = "flood") |>
     pivot_longer(cols = -c(watershed, habitat_type), names_to = "year_date", values_to = "decay_acres") |>
     separate(year_date, into = c("month", "year"))

   fry_rearing_decay <- DSMhabitat::square_meters_to_acres(decayed_habitat$inchannel_habitat_fry -
                                                                 habitats$inchannel_habitat_fry) |>
     as_tibble() |>
     mutate(watershed = fallRunDSM::watershed_labels,
            habitat_type = "fry rearing") |>
     pivot_longer(cols = -c(watershed, habitat_type), names_to = "year_date", values_to = "decay_acres") |>
     separate(year_date, into = c("month", "year"))

   juv_rearing_decay <- DSMhabitat::square_meters_to_acres(decayed_habitat$inchannel_habitat_juv -
                                                                 habitats$inchannel_habitat_juv) |>
     as_tibble() |>
     mutate(watershed = fallRunDSM::watershed_labels,
            habitat_type = "juv rearing") |>
     pivot_longer(cols = -c(watershed, habitat_type), names_to = "year_date", values_to = "decay_acres") |>
     separate(year_date, into = c("month", "year"))

   all_decay <- bind_rows(spawning_decay, fp_rearing_decay, fry_rearing_decay, juv_rearing_decay) |>
     group_by(year, location = watershed, habitat_type) |>
     summarize(value = sum(decay_acres, na.rm = TRUE)) |>
     ungroup() |>
     mutate(scenario = scenario_name,
            run = selected_run,
            performance_metric = "Habitat Decay Acres")

   # floodplain above threshold ------------------------------------------------
   flow_data <- switch(scenario_name,
                       "Baseline" = DSMflow::flows_cfs$biop_itp_2018_2019,
                       "Theoretical Max Habitat" = DSMflow::flows_cfs$biop_itp_2018_2019,
                       "No Harvest" = DSMflow::flows_cfs$biop_itp_2018_2019,
                       "No Hatchery" = DSMflow::flows_cfs$biop_itp_2018_2019,
                       "Max Flow and Habitat" = DSMflow::flows_cfs$biop_itp_2018_2019,
                       "Max Flow" = DSMflow::flows_cfs$run_of_river) |> # TODO update
     mutate("Lower-mid Sacramento River" = 35.6/58 * `Lower-mid Sacramento River1` + 22.4/58 * `Lower-mid Sacramento River2`) |>
     select(-`Lower-mid Sacramento River1`, -`Lower-mid Sacramento River2`)
   create_threshold_df <- function(watershed) {
     data <- flow_data |>
       filter(date >= lubridate::as_date("1979-01-01")) |>
       select(watershed, date) |>
       rename(flow_cfs = watershed)

     dur_30 <-  data |>
       mutate(water_year = ifelse(lubridate::month(date) %in% 10:12, lubridate::year(date) + 1, lubridate::year(date))) |>
       group_by(water_year) |>
       mutate(roll_mean = zoo::rollapply(flow_cfs, FUN = min,
                                         width = lubridate::month(date), fill = NA, align = "left")) |>
       summarise(stat_in_duration = mean(roll_mean, na.rm = TRUE)) |>
       ungroup()
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
     pivot_longer(cols = -date, names_to = "watershed", values_to = "flow_cfs") |>
     left_join(thresholds) |>
     mutate(flooded = ifelse(flow_cfs > threshold, TRUE, FALSE),
            scenario = scenario_name,
            run = selected_run,
            days_flooded = ifelse(flooded, 30, 0),
            month = month.abb[lubridate::month(date)],
            year = as.character(lubridate::year(date)))

   rearing_months <- switch(selected_run,
                            "fall" = c(1:8),
                            "spring" = c(1:5),
                            "winter" = c(1:5),
                            "late fall" = c(4:11))

   floodplain_habitat <- model_parameters$floodplain_habitat[ , rearing_months, ] |>
     as_tibble() |>
     mutate(location = fallRunDSM::watershed_labels) |>
     pivot_longer(cols = -location, names_to = "year_date", values_to = "flooded_hab") |>
     separate(year_date, into = c("month", "year")) |>
     left_join(flood_summary) |>
     mutate(rearing_hab = DSMhabitat::square_meters_to_acres(flooded_hab),
            flooded_acres = rearing_hab * days_flooded) |> # assume 30 days in month
     group_by(year, location) |>
     summarize(value = sum(flooded_acres, na.rm = TRUE)) |>
     ungroup() |>
     mutate(scenario = scenario_name,
            run = selected_run,
            performance_metric = "Flooded Acre Days Above 2 year Exceedance")

   # flow frequency and stage
   generate_lag_flows <- function(selected_watershed) {
     flood_summary <- flow_data |>
       pivot_longer(cols = -date, names_to = "watershed", values_to = "flow_cfs") |>
       filter(watershed == selected_watershed) |>
       mutate(lag_flow_cfs = lag(flow_cfs),
              monthly_flow_difference = abs(flow_cfs - lag_flow_cfs))
   }
   watersheds <- fallRunDSM::watershed_labels[-c(17, 22)] # removes bypasses
   max_yearly_flow_diff_table <- purrr::map(watersheds, generate_lag_flows) |>
     reduce(bind_rows) |>
     mutate(year = lubridate::year(date),
            location = watershed
            ) |>
     group_by(year, location) |>
     summarize(value = max(monthly_flow_difference, na.rm = TRUE)) |>
     ungroup() |>
     mutate(scenario = scenario_name,
            run = selected_run,
            performance_metric = "Flood Frequecy and Stage: Monthly Flow Differencial")



   prepped_inputs <- bind_rows(all_wetted_acre_days, all_decay, floodplain_habitat, max_yearly_flow_diff_table)

 return(prepped_inputs)

}
