library(fallRunDSM)
library(tidyverse)
library(plotly)
# The following script contains helper functions for producing visualizations to
# describe performance metrics for the R2R project. Each function should take in
# either model parameters associated with a modeling run or the model outputs

# MODEL INPUT PERFORMANCE METRICS ----------------------------------------------
# Performance Metric: Habitat Ratios -------------------------------------------
produce_habitat_ratios <- function(model_parameters, watershed) {
  # pull base habitat from model parameters
  spawn_hab <- model_parameters$spawning_habitat
  rearing_habitat_juvenile <- model_parameters$inchannel_habitat_juvenile
  rearing_habitat_fry <- model_parameters$inchannel_habitat_fry

  # Generate floodplain scaler to multiply floodplain habitat by
  floodplain_scaler <-  model_parameters$weeks_flooded
  floodplain_scaler[floodplain_scaler == 2] <- .5
  floodplain_scaler[floodplain_scaler == 1] <- .25
  floodplain_scaler[floodplain_scaler == 3] <- .75
  floodplain_scaler[floodplain_scaler == 4] <- 1

  floodplain_habitat <- model_parameters$floodplain_habitat * floodplain_scaler

  total_habitat <- spawn_hab[,,2:22] + rearing_habitat_juvenile +
    rearing_habitat_fry + floodplain_habitat

  perc_spawn <- (spawn_hab[,, 2:22]/total_habitat) * 100
  perc_rear_fry <- (rearing_habitat_fry/total_habitat) * 100
  perc_rear_juv <- (rearing_habitat_juvenile/total_habitat) * 100
  perc_fp <- (floodplain_habitat/total_habitat) * 100

  spawners <- as_tibble(perc_spawn) |>
    dplyr::mutate(location = fallRunDSM::watershed_labels) |>
    pivot_longer(cols = c(1:252), values_to = 'percent_spawning_habitat', names_to = "month_year") |>
    mutate(month = match(gsub("\\..*","", month_year), month.abb),
           year = gsub("^.*\\.","", month_year),
           date = as.Date(paste0(year, "-", month, "-1")),
           habitat_type = "spawning") |>
    select(location, date, habitat_type, percent_habitat = percent_spawning_habitat) |> glimpse()

  fry_rearing <- as_tibble(perc_rear_fry) |>
    dplyr::mutate(location = fallRunDSM::watershed_labels) |>
    pivot_longer(cols = c(1:252), values_to = 'percent_fry_rearing_habitat', names_to = "month_year") |>
    mutate(month = match(gsub("\\..*","", month_year), month.abb),
           year = gsub("^.*\\.","", month_year),
           date = as.Date(paste0(year, "-", month, "-1")),
           habitat_type = "fry rearing") |>
    select(location, date, habitat_type, percent_habitat = percent_fry_rearing_habitat) |> glimpse()

  juv_rearing <- as_tibble(perc_rear_juv) |>
    dplyr::mutate(location = fallRunDSM::watershed_labels) |>
    pivot_longer(cols = c(1:252), values_to = 'percent_juv_rearing_habitat', names_to = "month_year") |>
    mutate(month = match(gsub("\\..*","", month_year), month.abb),
           year = gsub("^.*\\.","", month_year),
           date = as.Date(paste0(year, "-", month, "-1")),
           habitat_type = "juv rearing") |>
    select(location, date, habitat_type, percent_habitat = percent_juv_rearing_habitat) |> glimpse()

  floodplain <- as_tibble(perc_fp) |>
    dplyr::mutate(location = fallRunDSM::watershed_labels) |>
    pivot_longer(cols = c(1:252), values_to = 'percent_floodplain_habitat', names_to = "month_year") |>
    mutate(month = match(gsub("\\..*","", month_year), month.abb),
           year = gsub("^.*\\.","", month_year),
           date = as.Date(paste0(year, "-", month, "-1")),
           habitat_type = "floodplain rearing") |>
    select(location, date, habitat_type, percent_habitat = percent_floodplain_habitat) |> glimpse()

  all_hab_percents <- bind_rows(spawners, fry_rearing, juv_rearing, floodplain) |>
    mutate(month_length = lubridate::days_in_month(lubridate::month(date)))

  all_hab_percents |>
    filter(location == watershed) |>
    ggplot(aes(x = date, y = percent_habitat, fill = habitat_type)) +
    geom_col(position = "stack", width = 31) +
    scale_fill_manual(values = wesanderson::wes_palette("Royal1")) +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.title = element_blank()) +
    labs(x = "Simulation Year",
         y = "Percentage",
         title = paste0(watershed, ": Ratio of Habitat Types for Simulation Period")) +
    theme(text = element_text(size = 15))
}

hab_ratio_summary_table <- function(model_parameters) {
  # pull base habitat from model parameters
  spawn_hab <- model_parameters$spawning_habitat
  rearing_habitat_juvenile <- model_parameters$inchannel_habitat_juvenile
  rearing_habitat_fry <- model_parameters$inchannel_habitat_fry

  # Generate floodplain scaler to multiply floodplain habitat by
  floodplain_scaler <-  model_parameters$weeks_flooded
  floodplain_scaler[floodplain_scaler == 2] <- .5
  floodplain_scaler[floodplain_scaler == 1] <- .25
  floodplain_scaler[floodplain_scaler == 3] <- .75
  floodplain_scaler[floodplain_scaler == 4] <- 1

  floodplain_habitat <- model_parameters$floodplain_habitat * floodplain_scaler

  total_habitat <- spawn_hab[,,2:22] + rearing_habitat_juvenile +
    rearing_habitat_fry + floodplain_habitat

  perc_spawn <- (spawn_hab[,, 2:22]/total_habitat) * 100
  perc_rear_fry <- (rearing_habitat_fry/total_habitat) * 100
  perc_rear_juv <- (rearing_habitat_juvenile/total_habitat) * 100
  perc_fp <- (floodplain_habitat/total_habitat) * 100

  spawners <- as_tibble(perc_spawn) |>
    dplyr::mutate(location = fallRunDSM::watershed_labels) |>
    pivot_longer(cols = c(1:252), values_to = 'percent_spawning_habitat', names_to = "month_year") |>
    mutate(month = match(gsub("\\..*","", month_year), month.abb),
           year = gsub("^.*\\.","", month_year),
           date = as.Date(paste0(year, "-", month, "-1")),
           habitat_type = "spawning") |>
    select(location, date, habitat_type, percent_habitat = percent_spawning_habitat) |> glimpse()

  fry_rearing <- as_tibble(perc_rear_fry) |>
    dplyr::mutate(location = fallRunDSM::watershed_labels) |>
    pivot_longer(cols = c(1:252), values_to = 'percent_fry_rearing_habitat', names_to = "month_year") |>
    mutate(month = match(gsub("\\..*","", month_year), month.abb),
           year = gsub("^.*\\.","", month_year),
           date = as.Date(paste0(year, "-", month, "-1")),
           habitat_type = "fry rearing") |>
    select(location, date, habitat_type, percent_habitat = percent_fry_rearing_habitat) |> glimpse()

  juv_rearing <- as_tibble(perc_rear_juv) |>
    dplyr::mutate(location = fallRunDSM::watershed_labels) |>
    pivot_longer(cols = c(1:252), values_to = 'percent_juv_rearing_habitat', names_to = "month_year") |>
    mutate(month = match(gsub("\\..*","", month_year), month.abb),
           year = gsub("^.*\\.","", month_year),
           date = as.Date(paste0(year, "-", month, "-1")),
           habitat_type = "juv rearing") |>
    select(location, date, habitat_type, percent_habitat = percent_juv_rearing_habitat) |> glimpse()

  floodplain <- as_tibble(perc_fp) |>
    dplyr::mutate(location = fallRunDSM::watershed_labels) |>
    pivot_longer(cols = c(1:252), values_to = 'percent_floodplain_habitat', names_to = "month_year") |>
    mutate(month = match(gsub("\\..*","", month_year), month.abb),
           year = gsub("^.*\\.","", month_year),
           date = as.Date(paste0(year, "-", month, "-1")),
           habitat_type = "floodplain rearing") |>
    select(location, date, habitat_type, percent_habitat = percent_floodplain_habitat) |> glimpse()

  all_hab_percents <- bind_rows(spawners, fry_rearing, juv_rearing, floodplain) |>
    pivot_wider(names_from = habitat_type, values_from = percent_habitat) |>
    group_by(location) |>
    summarize("Average Percent Spawning" = round(mean(spawning, na.rm = T), 0),
              "Average Percent Fry Rearing" = round(mean(`fry rearing`, na.rm = T), 0),
              "Average Percent Juv Rearing" = round(mean(`juv rearing`, na.rm = T),0),
              "Average Percent Floodplain Rearing" = round(mean(`floodplain rearing`, na.rm = T),0))
  return(all_hab_percents)

}

# TODO refine floodplain stuff, seems very high
floodplain_activation <- function(model_parameters, watershed) {
  # Generate floodplain scaler to multiply floodplain habitat by
  weeks_flooded <-  model_parameters$weeks_flooded
  as_tibble(weeks_flooded) |>
    dplyr::mutate(location = fallRunDSM::watershed_labels) |>
    pivot_longer(cols = c(1:252), values_to = 'weeks_flooded', names_to = "month_year") |>
    mutate(month = match(gsub("\\..*","", month_year), month.abb),
           year = gsub("^.*\\.","", month_year),
           date = as.Date(paste0(year, "-", month, "-1"))) |>
    group_by(year, location) |>
    summarize(total_weeks_flooded = sum(weeks_flooded),
              total_days_flooded = total_weeks_flooded * 7) |> glimpse()
}
