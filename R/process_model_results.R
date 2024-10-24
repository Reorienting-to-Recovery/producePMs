#' Create Model Results Dataframe
#'
#' This function takes model results and parameters, along with scenario information, and creates a dataframe containing performance metrics for different locations and years.
#'
#' @param model_results The model results object containing the necessary data.
#' @param model_parameters The model parameters object containing the necessary data.
#' @param scenario_name The name of the scenario.
#' @param selected_run The selected chinook run.
#'
#' @return A dataframe containing performance metrics for different locations and years.
#' @examples
#' create_model_results_dataframe(model_results, model_parameters, "Scenario 1", "fall")
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
    mutate(preformance_metric = "4 PHOS",
           year = as.numeric(year)) |>
    glimpse()

  all_spawn <- dplyr::as_tibble(model_results$spawners) |>
    dplyr::mutate(location = fallRunDSM::watershed_labels) |>
    pivot_longer(cols = c(`1`:`20`), values_to = "value", names_to = "year") |>
    mutate(preformance_metric = "1 All Spawners",
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
           PHOS = ifelse(`4 PHOS` == 1, NA, `4 PHOS`),
           "CRR: Juvenile to Natural Adult" =  nat_spawners_lead / `Juveniles`,
           "2.1 CRR: Total Adult to Returning Natural Adult" = nat_spawners_lead / `1 All Spawners`,
           "2.2 Growth Rate Spawners" = (`1 All Spawners` - lag(`1 All Spawners`, 1) ) / lag(`1 All Spawners`, 1),
           "3.1 & 3.2 Independent Population" = ifelse(!location %in% c("Bear River", "Big Chico Creek", "Elder River", "Paynes Creek",  "Stoney Creek", "Thomes Creek") &
                                                       `Natural Spawners` > 500 & `4 PHOS` < .05 & `2.2 Growth Rate Spawners` > 0 & `2.1 CRR: Total Adult to Returning Natural Adult` > 1, TRUE, FALSE),
           "3.3 Dependent Populations" = ifelse(location %in% c("Bear River", "Big Chico Creek", "Elder River", "Paynes Creek",  "Stoney Creek", "Thomes Creek") & `1 All Spawners` > 1, TRUE, FALSE),
           area_sqmt = ifelse(scenario == "Max Habitat",
                               max_hab_total_area_sqmt, sit_total_area_sqmt),
           "7.2 Marine Derived Nutrient (pounds per sq meter)" = (`1 All Spawners` * 21) / area_sqmt,
           "7.1 Carrying Capacity vs Abundance" = `1 All Spawners` / spawner_capacity,
           "4 PHOS" = ifelse(`1 All Spawners` > 0, `4 PHOS`, NA)
    ) |>
    ungroup() |>
    select(-nat_spawners_lead, -area_sqmt, -spawner_capacity) |>
    pivot_longer(cols = 5:16, names_to = "performance_metric", values_to = "value") |> glimpse()

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

  # Harvest
  total_released <- fallRunDSM::fall_hatchery_release |> rowSums() |> sum()
  total_released
  # assume only 1% of net pen fish are harvested throughout lifetime
  # (consistent with/very low end of CFM_CWT_report studies, year 3 captures much higher)
  # https://www.calfish.org/Portals/2/Programs/CentralValley/CFM/docs/2019_CFM_CWT_Report.pdfv
  harvestable_ocean_terminal_hatcheries <- total_released * .01
  harvestable_ocean_terminal_hatcheries
  # how much harvest is in river vs ocean in the model
  # Total harvest
  total_harvest <- fallRunDSM::r2r_adult_harvest_rate |> sum()

  # 17 tribs allow in river harvest each of these allows it at .8 percent
  in_river_harvest_percentace <- (.08 * 17) / total_harvest

  ocean_harvest <- model_results$harvested_adults |>
    rowwise() |>
    mutate(scenario = scenario_name,
           run = selected_run,
           location = "Ocean",
           performance_metric = "12.2: Total ocean harvest",
           harvest = total_harvest * (1 - in_river_harvest_percentace),
           value = ifelse(model_parameters$terminal_hatchery_logic,
                          harvest + harvestable_ocean_terminal_hatcheries, harvest)) |>
    select(-hatchery_harvest, -natural_harvest, -total_harvest, -harvest) |> glimpse()

  river_harvest <- model_results$harvested_adults |>
    rowwise() |>
    mutate(scenario = scenario_name,
           run = selected_run,
           location = "River",
           performance_metric = "12.1: Total river harvest",
           value = total_harvest * in_river_harvest_percentace) |>
    select(-hatchery_harvest, -natural_harvest, -total_harvest) |> glimpse()

  total_harvest_df <- bind_rows(ocean_harvest, river_harvest)


  juveniles <- model_results$juveniles_at_chipps |>
    as_tibble() |>
    mutate(year = as.numeric(year)) |>
    rename(location = watershed) |>
    group_by(year, month, location, size_or_age = size) |>
    summarise(value = sum(juveniles_at_chipps, na.rm = TRUE)) |>
    mutate(performance_metric = "Juveniles Size at Ocean Entry",
           scenario = scenario_name,
           run = selected_run) |> glimpse()

  full_results <- bind_rows(result_dataframe, adults_age, juveniles, total_harvest_df)

  return(full_results)
}

