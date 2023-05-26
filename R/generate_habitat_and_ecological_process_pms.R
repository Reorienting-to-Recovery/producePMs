library(tidyverse)

### 8.1 ###
# Wetted acre days of suitable juvenile rearing habitat ------------------------
# Present mean wetted acre days (mulitply total inchannel rearing & floodplain
# habitat values by day in month, sum across all months, take annual average) report mean and range of values
produce_juvenile_wetted_acre_day_pm <- function(model_parameters, scenario, run){
  rearing_months <- switch(run,
                            "fall" = c(1:8),
                            "spring" = c(11, 12, 1:5), #TODO do we want to include holding and spawning months
                            "winter" = c(9:12, 1:5), #TODO do we want to include holding and spawning months
                            "late fall" = c(4:11))
  length_rm <- length(rearing_months)
  fry_juv_cutoff <- length_rm/2
  ic_habitat_fry <- model_parameters$inchannel_habitat_fry[ , rearing_months[1:fry_juv_cutoff], ] |>
    as_tibble() |>
    pivot_longer(cols = everything(), names_to = "year_date", values_to = "rearing_hab") |>
    separate(year_date, into = c("month", "year"))
  ic_habitat_juv <- model_parameters$inchannel_habitat_juvenile[ , fry_juv_cutoff:length_rm, ] |>
    as_tibble() |>
    pivot_longer(cols = everything(), names_to = "year_date", values_to = "rearing_hab") |>
    separate(year_date, into = c("month", "year"))
  floodplain_habitat <- model_parameters$floodplain_habitat[ , rearing_months, ] |>
    as_tibble() |>
    pivot_longer(cols = everything(), names_to = "year_date", values_to = "rearing_hab") |>
    separate(year_date, into = c("month", "year"))

  rearing_habitat <- bind_rows(ic_habitat_fry, ic_habitat_juv, floodplain_habitat) |>
    mutate(rearing_hab = DSMhabitat::square_meters_to_acres(rearing_hab),
           wetted_acre_day = rearing_hab * 30) |> # assume 30 days in month
    group_by(year) |>
    summarize(annual_wetted_acre_day = sum(wetted_acre_day, na.rm = TRUE)) |>
    ungroup() |>
    mutate(scenario = scenario,
           run = run) |>
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
produce_spawning_wetted_acre_day_pm <- function(model_parameters, scenario, selected_run){
  spawning_months <- switch(run,
                            "fall" = c(10:12),
                            "spring" = c(3:6, 7:10), #TODO do we want to include holding and spawning months
                            "winter" = c(1:4, 5:7), #TODO do we want to include holding and spawning months
                            "late fall" = c(10:12, 1:2))
  month_lookup <- tibble(month_num = c(1:12), month = month.abb)
  spawn_hab <- model_parameters$spawning_habitat |>
    as_tibble() |>
    pivot_longer(cols = everything(), names_to = "year_date", values_to = "spawning_habitat") |>
    separate(year_date, into = c("month", "year")) |>
    left_join(month_lookup) |>
    filter(month_num  %in% spawning_months) |>
    mutate(spawning_habitat = DSMhabitat::square_meters_to_acres(spawning_habitat),
           wetted_acre_day = spawning_habitat * 30) |> # assume 30 days in month
    group_by(year) |>
    summarize(annual_wetted_acre_day = sum(wetted_acre_day, na.rm = TRUE)) |>
    ungroup() |>
    mutate(scenario = scenario,
           run = run) |>
    group_by(scenario, run) |>
    summarize(avg_wetted_acre_day = mean(annual_wetted_acre_day, na.rm = T),
              min_wetted_acre_day = min(annual_wetted_acre_day, na.rm = T),
              max_wetted_acre_day = max(annual_wetted_acre_day, na.rm = T))
    return(spawn_hab)
}

### 8.3 ###
# Spawning habitat decay rate (as a proxy for riverine condition) --------------


### 9.1 ###
# Wetted acre days - total days floodplain activation occurs -------------------
produce_wetted_acre_day_floodplain_activation_pm <- function(model_parameters, scenario, selected_run){
  rearing_months <- switch(run,
                           "fall" = c(1:8),
                           "spring" = c(11, 12, 1:5), #TODO do we want to include holding and spawning months
                           "winter" = c(9:12, 1:5), #TODO do we want to include holding and spawning months
                           "late fall" = c(4:11))

}

### 9.2 ###
# Functional Flow Metric -------------------------------------------------------
# TODO Not sure on this one - hopefully we will know after Julie

### 9.3 ###
# TODO replace 9.1 with this one
# TODO figure out flow threshold we want to use


