library(tidyverse)
#' Create model results dataframe
#'
#' @param model_results A model output from the Recovery life cycle models (fall, spring, winter or late fall)
#' @param scenario_name The name of the scenario that produced the model results.
#' @return A dataframe containing the following columns(location, year, scenario, performance_metric, value)
#' @examples
#' baseline_seeds <- fallRunDSM::fall_run_model(mode = "seed", ..params = fallRunDSM::r_to_r_baseline_params)
#' baseline_model_results <- fallRunDSM::fall_run_model(mode = "simulate", ..params = fallRunDSM::r_to_r_baseline_params,
#'                                                      seeds = baseline_seeds)
#' process_model_results(baseline_model_results, "Baseline")
#' @export
create_model_results_dataframe <- function(model_results, model_parameters, scenario_name, selected_run) {
  nat_spawn <- dplyr::as_tibble(model_results$spawners * model_results$proportion_natural_at_spawning) |>
    dplyr::mutate(location = fallRunDSM::watershed_labels) |>
    pivot_longer(cols = c(`1`:`20`), values_to = "value", names_to = "year") |>
    mutate(preformance_metric = "Natural Spawners",
           year = as.numeric(year)) |>
    glimpse()

  nat_juves <- dplyr::as_tibble(model_results$proportion_natural_juves_in_tribs) |>
    dplyr::mutate(location = fallRunDSM::watershed_labels) |>
    pivot_longer(cols = c(`1`:`20`), values_to = "value", names_to = "year") |>
    mutate(preformance_metric = "Natural Juveniles in River",
           year = as.numeric(year)) |>
    glimpse()

  phos <- dplyr::as_tibble(model_results$phos) |>
    dplyr::mutate(location = fallRunDSM::watershed_labels) |>
    pivot_longer(cols = c(`1`:`20`), values_to = "value", names_to = "year") |>
    mutate(preformance_metric = "PHOS",
           year = as.numeric(year)) |>
    glimpse()

  all_spawn <- dplyr::as_tibble(model_results$spawners) |>
    dplyr::mutate(location = fallRunDSM::watershed_labels) |>
    pivot_longer(cols = c(`1`:`20`), values_to = "value", names_to = "year") |>
    mutate(preformance_metric = "All Spawners",
           year = as.numeric(year)) |>
    glimpse()

  juveniles <- model_results$juveniles |>
    as_tibble() |>
    mutate(year = as.numeric(year)) |>
    rename(location = watershed) |>
    group_by(year, location) |>
    summarise(value = sum(juveniles)) |>
    mutate(preformance_metric = "Juveniles") |> glimpse()

  names_spawning_watersheds <- switch(selected_run,
                                       "fall" = names(which(ifelse(fallRunDSM::adult_seeds[, 1] == 0, FALSE, TRUE))),
                                       "spring" = names(which(ifelse(springRunDSM::adult_seeds[, 1] == 0, FALSE, TRUE))),
                                       "winter" = names(which(ifelse(winterRunDSM::adult_seeds[, 1] == 0, FALSE, TRUE))))

  # add marine derived nutrient stuff in
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
  # prep hab capacity stuff
  spawning_months <- switch(selected_run,
                            "fall" = c(10:12),
                            "spring" = c(7:10),
                            "winter" = c(5:7),
                            "late fall" = c(10:12, 1:2))
  month_lookup <- tibble(month_num = c(1:12), month = month.abb)
  year_lookup <- tibble(year = as.character(1979:2000), model_year = 1:22)
  spawn_capacity <- model_parameters$spawning_habitat |>
    as_tibble() |>
    mutate(location = fallRunDSM::watershed_labels) |>
    pivot_longer(cols = -location, names_to = "year_date", values_to = "spawning_habitat") |>
    separate(year_date, into = c("month", "year")) |>
    left_join(month_lookup) |>
    left_join(year_lookup) |>
    mutate(year = model_year) |>
    select(-model_year) |>
    filter(month_num  %in% spawning_months) |>
    group_by(year, month, location) |> # can add location in here too if we want
    summarise(total_monthly_spawning_habitat = sum(spawning_habitat, na.rm = TRUE)) |>
    ungroup() |>
    group_by(year, location) |>
    summarise(average_monthly_annual_spawning_habitat = mean(total_monthly_spawning_habitat, na.rm = TRUE)) |>
    ungroup() |>
    mutate(spawner_capacity = round(average_monthly_annual_spawning_habitat/
                                      fallRunDSM::r_to_r_baseline_params$spawn_success_redd_size),
           year = as.numeric(year)) |>
    select(year, location, spawner_capacity)

  result_dataframe <- bind_rows(nat_spawn, all_spawn, juveniles, phos) |>
    mutate(scenario = scenario_name,
           run = selected_run) |>
    pivot_wider(values_from = value, names_from = preformance_metric) |>
    left_join(spawn_capacity) |>
    arrange(location, year) |>
    group_by(location) |>
    mutate(nat_spawners_lead = lead(`Natural Spawners`, 3),
           PHOS = ifelse(PHOS == 1, NA, PHOS),
           "CRR: Juvenile to Natural Adult" =  nat_spawners_lead / `Juveniles`,
           "CRR: Total Adult to Returning Natural Adult" = nat_spawners_lead / `All Spawners`,
           "Growth Rate Natural Spawners" = (`Natural Spawners` - lag(`Natural Spawners`, 1) ) / lag(`Natural Spawners`, 1),
           "Independent Population" = ifelse(`Natural Spawners` > 500 & `PHOS` < .05 & `Growth Rate Natural Spawners` > 1 & `CRR: Total Adult to Returning Natural Adult` > 1, TRUE, FALSE),
           "Dependent Populations" = ifelse(location %in% names_spawning_watersheds & !`Independent Population`, TRUE, FALSE),
           area_sqmt = ifelse(scenario == "Max Habitat",
                               max_hab_total_area_sqmt, sit_total_area_sqmt),
           "Marine Derived Nutrient (pounds per sq meter)" = (`All Spawners` * 21) / area_sqmt,
           "Carrying Capacity vs Abundance" = `All Spawners` / spawner_capacity
    ) |>
    ungroup() |>
    select(-nat_spawners_lead, -area_sqmt, -spawner_capacity) |>
    pivot_longer(cols = 5:15, names_to = "performance_metric", values_to = "value") |> glimpse()

  # add juv and adult df
  adults_age <- model_results$returning_adults |>
    transmute(performance_metric = "Adult Age of Return",
           scenario = scenario_name,
           value = return_total,
           location = watershed,
           origin = origin,
           year = return_sim_year,
           size_or_age = as.character(return_sim_year - sim_year),
           run = selected_run) |> glimpse()

  juveniles <- model_results$juveniles_at_chipps |>
    as_tibble() |>
    mutate(year = as.numeric(year)) |>
    rename(location = watershed) |>
    group_by(year, month, location, size_or_age = size) |>
    summarise(value = sum(juveniles_at_chipps, na.rm = TRUE)) |>
    mutate(performance_metric = "Juveniles Size at Ocean Entry",
           scenario = scenario_name,
           run = selected_run) |> glimpse()

  full_results <- bind_rows(result_dataframe, adults_age, juveniles)

  return(full_results)
}

