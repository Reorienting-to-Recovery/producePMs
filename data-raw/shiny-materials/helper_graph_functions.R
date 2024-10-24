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
    filter(if(watershed != "All") location == watershed else TRUE) |> 
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
# MODEL OUTPUT PERFORMANCE METRICS ---------------------------------------------
# Performance Metric: Abundance - total spawners & natural spawners ------------
# totals
plot_total_spawners <- function(model_results,
                                result_type = c("Total Spawners", "Total Natural Spawners")) {
  if (result_type == "Total Natural Spawners") {
    spawn <- dplyr::as_tibble(model_results$spawners * model_results$proportion_natural) |>
      dplyr::mutate(location = fallRunDSM::watershed_labels)
  }
  else {
    spawn <- dplyr::as_tibble(model_results$spawners) |>
      dplyr::mutate(location = fallRunDSM::watershed_labels)
  }
  
  spawn %>%
    pivot_longer(cols = c(`1`:`20`), values_to = 'spawners', names_to = "year") %>%
    group_by(year) |>
    summarize(total_spawners = sum(spawners)) |>
    mutate(year = as.numeric(year)) %>%
    ggplot(aes(year, total_spawners)) +
    geom_line() +
    theme_minimal() +
    labs(y = result_type,
         x = "Year") +
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(breaks = 1:20) +
    theme(text = element_text(size = 20))
}

# all watershed specific plot
# TODO figure out best way to display legend
plot_all_watersheds_spawners <- function(model_results,
                                         result_type = c("Total Spawners", "Total Natural Spawners")) {
  if (result_type == "Total Natural Spawners")
    spawn <- dplyr::as_tibble(model_results$spawners * model_results$proportion_natural) |>
      dplyr::mutate(location = fallRunDSM::watershed_labels)
  else {
    spawn <- dplyr::as_tibble(model_results$spawners) |>
      dplyr::mutate(location = fallRunDSM::watershed_labels)
  }
  plot <- spawn %>%
    pivot_longer(cols = c(`1`:`20`), values_to = 'spawners', names_to = "year") %>%
    group_by(year) |>
    mutate(year = as.numeric(year)) %>%
    ggplot(aes(year, spawners, color = location)) +
    geom_line() +
    theme_minimal() +
    labs(y = result_type,
         x = "Simulation Year",
         # title = paste0("All Watersheds: ", result_type)
         ) +
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(breaks = 1:20) +
    theme(text = element_text(size = 12),
          legend.position = "none",
          legend.title = element_blank())
  
  ggplotly(plot)
}
# watershed specific plot
plot_single_watershed_natural_spawners <- function(model_results,
                                                   watershed,
                                                   result_type = c("Total Spawners", "Total Natural Spawners")) {
  if (result_type == "Total Natural Spawners")
    spawn <- dplyr::as_tibble(model_results$spawners * model_results$proportion_natural) |>
      dplyr::mutate(location = fallRunDSM::watershed_labels)
  else {
    spawn <- dplyr::as_tibble(model_results$spawners) |>
      dplyr::mutate(location = fallRunDSM::watershed_labels)
  }
  plot <- spawn %>%
    pivot_longer(cols = c(`1`:`20`), values_to = 'spawners', names_to = "year") %>%
    group_by(year) |>
    filter(location %in% watershed) |>
    mutate(year = as.numeric(year)) %>%
    ggplot(aes(year, spawners)) +
    geom_line() +
    theme_minimal() +
    labs(y = result_type,
         x = "Year") +
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(breaks = 1:20) +
    theme(text = element_text(size = 20)) 
  
  # TODO: this could also be by color instead of faceting. 
  if(length(watershed > 1)) {
    plot <- plot + 
      facet_wrap(~ location) 
  } else {
    plot <- plot +
      labs(title = paste0(watershed, ": ", result_type))
  }
  
  return(plot)
}

# Performance Metric: Cohort Replacement Rate ----------------------------------
# Juvenile
plot_juv_crr <- function(model_results,
                         watershed = fallRunDSM::watershed_labels,
                         result_type = c("Total Spawners", "Total Natural Spawners")){
  
  if (result_type == "Total Natural Spawners")
    spawn <- dplyr::as_tibble(model_results$spawners * model_results$proportion_natural) |>
      dplyr::mutate(location = fallRunDSM::watershed_labels) |>
      pivot_longer(names_to = "year", values_to = "spawners", -location)
  else {
    spawn <- dplyr::as_tibble(model_results$spawners) |>
      dplyr::mutate(location = fallRunDSM::watershed_labels) |>
      pivot_longer(names_to = "year", values_to = "spawners", -location)
  }
  
  juveniles <- model_results$juveniles |>
    as_tibble() |>
    mutate(year = as.numeric(year)) |>
    rename(location = watershed) |>
    filter(location %in% watershed) |> 
    group_by(year, location) |>
    summarise(total_juveniles = sum(juveniles)) |>
    ungroup() |>
    left_join(spawn |>
                mutate(year = as.numeric(year))) |>
    arrange(location, year) |>
    group_by(location) |>
    mutate(spawners_lag = lead(spawners, 3),
           metric = total_juveniles / spawners_lag,
           metric_rev = spawners_lag / total_juveniles) |>
    ungroup()
  
  # plot
  plot <- juveniles |>
    group_by(year, location) |>
    summarize(mean_crr = mean(metric_rev, na.rm = T)) |>
    ggplot(aes( year, mean_crr)) +
    geom_line() +
    scale_x_continuous(breaks = 1:20) +
    labs(x = "Year",
         y = "Adult Returns Per Juvenile",
         title = paste0("CRR: ", result_type, " Returns for Each Juvenile")) +
    theme_minimal() +
    theme(text = element_text(size = 20))
  
  return(plot)
  
  #ggplotly(plot)
  
  return(plot)
}

# Adult
plot_adult_crr <- function(model_results, 
                           watershed = fallRunDSM::watershed_labels,
                           result_type = c("Total Spawners", "Total Natural Spawners")) {
  if (result_type == "Total Natural Spawners")
    spawn <- dplyr::as_tibble(model_results$spawners * model_results$proportion_natural) |>
      dplyr::mutate(location = fallRunDSM::watershed_labels) |>
      pivot_longer(names_to = "year", values_to = "spawners", -location)
  else {
    spawn <- dplyr::as_tibble(model_results$spawners) |>
      dplyr::mutate(location = fallRunDSM::watershed_labels) |>
      pivot_longer(names_to = "year", values_to = "spawners", -location)
  }
  
  sim_spawners <- spawn |>
    group_by(location) |>
    mutate(
      year = as.numeric(year),
      lag_total = lag(spawners, 3),
      origin_year = lag(year, 3),
      metric = spawners / lag_total) |> 
    filter(location %in% watershed)
  
  # Plot
  plot <- sim_spawners |>
    ggplot(aes(origin_year, metric)) +
    geom_line() +
    scale_x_continuous(breaks = 1:20) +
    labs(x = "Year",
         y = "Adult Returns Per Spawner (3 yrs prior)",
         title = paste0("CRR: ", result_type, " Returns for Each Spawner")) +
    theme_minimal() +
    theme(text = element_text(size = 20))
  
  # TODO: this could also be by color instead of faceting. 
  if(length(watershed > 1)) {
    plot <- plot +
      facet_wrap(~ location)
  } else {
    plot <- plot +
      labs(title = paste0(watershed, ": ", result_type))
  }
  
  return(plot)
  
  #ggplotly(adult_plot)
}

# Performance Metric: Population Growth ----------------------------------------

# Performance Metric: Juvenile Size Distribition -------------------------------
#
# TODO need model refactoring and enhancement
# Performance Metric: Proportion Hatchery on Spawning Ground (PHOS)-------------
# Performance Metric: Age Distribution of Spawning Adults ----------------------
#