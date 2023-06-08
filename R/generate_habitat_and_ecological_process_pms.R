library(tidyverse)

### 8.1 ###
# Wetted acre days of suitable juvenile rearing habitat ------------------------
# Present mean wetted acre days (mulitply total inchannel rearing & floodplain
# habitat values by day in month, sum across all months, take annual average) report mean and range of values
#
# # Describe in docs Only spawning habitat is considered, not holding.

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
    summarize(annual_wetted_acre_day = sum(wetted_acre_day, na.rm = TRUE)) |>
    ungroup() |>
    mutate(scenario = scenario,
           run = selected_run) |>
    group_by(scenario, run) |>
    summarize(avg_wetted_acre_day = mean(annual_wetted_acre_day, na.rm = T),
              min_wetted_acre_day = min(annual_wetted_acre_day, na.rm = T),
              max_wetted_acre_day = max(annual_wetted_acre_day, na.rm = T))
  return(rearing_habitat)
}

### 8.2 ###
# Wetted acre days of suitable spawning habitat --------------------------------
# Present mean wetted acre days (mulitply total inchannel rearing & floodplain
# habitat values by day in month, sum across all months, take annual average) report mean and range of values
# # TODO just do spawning for now - check on what our holding habitat actually is and maybe capture holding wetted acre days seperatly
#
# Describe in docs Only spawning habitat is considered, not holding.
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
    summarize(annual_wetted_acre_day = sum(wetted_acre_day, na.rm = TRUE)) |>
    ungroup() |>
    mutate(scenario = scenario,
           run = selected_run) |>
    group_by(scenario, run) |>
    summarize(avg_wetted_acre_day = mean(annual_wetted_acre_day, na.rm = T),
              min_wetted_acre_day = min(annual_wetted_acre_day, na.rm = T),
              max_wetted_acre_day = max(annual_wetted_acre_day, na.rm = T))
    return(spawn_hab)
}

### 8.3 ###
# Spawning habitat decay rate (as a proxy for riverine condition) --------------
# convert this to habitat loss. yearly loss in capacity relative to the maximum
# over the 20 year period - sum over 20 year period
total_habitat_decay <- function(model_parameters, scenario){
  habitats <- list(
    spawning_habitat = model_params$spawning_habitat,
    inchannel_habitat_fry = model_params$inchannel_habitat_fry,
    inchannel_habitat_juvenile = model_params$inchannel_habitat_juvenile,
    floodplain_habitat = model_params$floodplain_habitat,
    weeks_flooded = model_params$weeks_flooded
  )
  # TODO confused about this - what is going on with decay - I thought it was driven by flow
  decayed_habitat <- DSMscenario::load_scenario(scenario = DSMscenario::scenarios$NO_ACTION,
                                              habitat_inputs = habitats,
                                              species = DSMscenario::species$FALL_RUN,
                                              spawn_decay_rate = model_params$spawn_decay_rate,
                                              rear_decay_rate = model_params$rear_decay_rate,
                                              spawn_decay_multiplier = model_params$spawn_decay_multiplier,
                                              stochastic = FALSE)

  spawning_decay <- DSMhabitat::square_meters_to_acres(sum(decayed_habitat$spawning_habitat -
                                                             habitats$spawning_habitat))
  fp_rearing_decay <- DSMhabitat::square_meters_to_acres(sum(decayed_habitat$floodplain_habitat -
                                                         habitats$floodplain_habitat))
  fry_rearing_decay <- DSMhabitat::square_meters_to_acres(sum(decayed_habitat$inchannel_habitat_fry -
                                                               habitats$inchannel_habitat_fry))
  juv_rearing_decay <- DSMhabitat::square_meters_to_acres(sum(decayed_habitat$inchannel_habitat_juv -
                                                                habitats$inchannel_habitat_juv))
  all <- sum(spawning_decay, fp_rearing_decay, fry_rearing_decay, juv_rearing_decay)
  total_decay <- tibble(habitat_type = c("Spawn", "Floodplain", "Inchannel Fry", "Inchannel Juv", "All"),
                        scenario = c(rep(scenario, 5)),
                        habitat_decay = c(spawning_decay, fp_rearing_decay, fry_rearing_decay, juv_rearing_decay, all))
  return(total_decay)
}


### 9.1 & 9.3
# Wetted acre days - total days floodplain activation occurs -------------------

produce_wetted_acre_day_floodplain_activation_pm <- function(model_parameters, scenario, selected_run){
    flow_data <- switch(scenario,
                        "Baseline" = DSMflow::flows_cfs$biop_itp_2018_2019,
                        "Max Flow" = DSMflow::flows_cfs$biop_itp_2018_2019) |> # TODO update
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
           scenario = scenario,
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
    mutate(watershed = fallRunDSM::watershed_labels) |>
    pivot_longer(cols = -watershed, names_to = "year_date", values_to = "flooded_hab") |>
    separate(year_date, into = c("month", "year")) |>
    left_join(flood_summary) |>
    mutate(rearing_hab = DSMhabitat::square_meters_to_acres(flooded_hab),
           wetted_acre_day = rearing_hab * days_flooded) |> # assume 30 days in month
    group_by(year) |>
    summarize(annual_wetted_acre_day = sum(wetted_acre_day, na.rm = TRUE)) |>
    ungroup() |>
    mutate(scenario = scenario,
           run = selected_run) |>
    group_by(scenario, run) |>
    summarize(avg_wetted_acre_day = mean(annual_wetted_acre_day, na.rm = T),
              min_wetted_acre_day = min(annual_wetted_acre_day, na.rm = T),
              max_wetted_acre_day = max(annual_wetted_acre_day, na.rm = T))

  return(floodplain_habitat)
}

### 9.2 ###
# Functional Flow Metric -------------------------------------------------------
# TODO Not sure on this one - hopefully we will know after Julie




