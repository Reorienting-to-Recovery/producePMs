library(tidyverse)
#' Process Model Results
#'
#' @param model_results A model output from the Recovery life cycle models (fall, spring, winter or late fall)
#' @param scenario_name The name of the scenario that produced the model results.
#' @return A dataframe containing the following columns(location, year, scenario, performance_metric, value)
#' @examples
#' baseline_seeds <- fallRunDSM::fall_run_model(mode = "seed", ..params = fallRunDSM::r_to_r_baseline_params)
#' baseline_model_results <- fallRunDSM::fall_run_model(mode = "simulate", ..params = fallRunDSM::r_to_r_baseline_params,
#'                                                      seeds = baseline_seeds)
#' process_model_results(baseline_model_results, "Baseline")

process_model_results <- function(model_results, scenario_name) {
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

  result_dataframe <- bind_rows(nat_spawn, all_spawn, juveniles, phos) |>
    mutate(scenario = scenario_name) |>
    pivot_wider(values_from = value, names_from = preformance_metric) |>
    arrange(location, year) |>
    group_by(location) |>
    mutate(nat_spawners_lead = lead(`Natural Spawners`, 3),
           "CRR: Juvenile to Natural Adult" =  nat_spawners_lead / `Juveniles`,
           "CRR: Total Adult to Returning Natural Adult" = nat_spawners_lead / `All Spawners`,
           "Growth Rate Natural Spawners" = (`Natural Spawners` - lag(`Natural Spawners`, 1) ) / lag(`Natural Spawners`, 1)
    ) |>
    ungroup() |>
    select(-nat_spawners_lead) |>
    pivot_longer(cols = 4:9, names_to = "performance_metric", values_to = "value") |> glimpse()

  return(result_dataframe)
}
