library(tidyverse)

### 15 ###
# Non-salmon-oriented recreation -----------------------------------------------
# Total weeks flooded for each year - take mean of annual totals, report mean and range of annual totals
produce_weeks_flooded_pm <- function(model_parameters, scenario, selected_run){
  weeks_flooded <- model_parameters$weeks_flooded |>
    as_tibble() |>
    pivot_longer(cols = everything(), names_to = "year_date", values_to = "weeks_flooded") |>
    separate(year_date, into = c("month", "year")) |>
    mutate(scenario = scenario,
           run = selected_run) |>
    group_by(scenario, run, year) |>
    summarize(annual_weeks_flooded = sum(weeks_flooded, na.rm = TRUE)) |>
    ungroup() |>
    group_by(scenario, run) |>
    summarize(mean_weeks_flooded = mean(annual_weeks_flooded, na.rm = TRUE),
              max_weeks_flooded = max(annual_weeks_flooded, na.rm = TRUE),
              min_weeks_flooded = min(annual_weeks_flooded, na.rm = TRUE)) |>
    glimpse()
  return(weeks_flooded)
}

### 16.1 ###
# Pull From Trend Report -------------------------------------------------------

### 16.2 ###
# Proportion Unimparied --------------------------------------------------------
#TODO do not know how we would classify baseline unimparied


### 17 ###
# Flood frequency and stage for each watershed ---------------------------------
# Mark/Erin develop simple calculation based on change in peak flow from one month to next.
# TODO check below logic with Mark
produce_flood_frequency_and_stage_pm <- function(model_params, scenario, selected_run){
  flow_data <- switch(scenario,
                      "Baseline" = DSMflow::flows_cfs$biop_itp_2018_2019,
                      "Max Flow" = DSMflow::flows_cfs$biop_itp_2018_2019) |> # TODO update
    mutate("Lower-mid Sacramento River" = 35.6/58 * `Lower-mid Sacramento River1` + 22.4/58 * `Lower-mid Sacramento River2`) |>
    select(-`Lower-mid Sacramento River1`, -`Lower-mid Sacramento River2`)
  generate_lag_flows <- function(selected_watershed) {
    flood_summary <- flow_data |>
      pivot_longer(cols = -date, names_to = "watershed", values_to = "flow_cfs") |>
      filter(watershed == selected_watershed) |>
      mutate(lag_flow_cfs = lag(flow_cfs),
             monthly_flow_difference = abs(flow_cfs - lag_flow_cfs))
  }
  watersheds <- fallRunDSM::watershed_labels[-c(17, 22)] # removes bypasses
  monthly_flow_diff_table <- purrr::map(watersheds, generate_lag_flows) |>
    reduce(bind_rows) |>
    mutate(scenario = scenario,
           run = selected_run) |>
    group_by(scenario, run) |>
    summarize(max_flow_differencial = max(monthly_flow_difference, na.rm = TRUE),
              avg_flow_differencial = mean(monthly_flow_difference, na.rm = TRUE),
              min_flow_differencial = min(monthly_flow_difference, na.rm = TRUE))

  return(monthly_flow_diff_table)
}
### 18 ###
# Hydropower generation ability ------------------------------------------------
# TODO use these power mods to determine total cost associated with storage in each scenario
# need storage values to finish this calculation
lost_gen <- tibble(volume_af = c(49000,44000,42000,74000,
                                 23000,7000,10000,120000,137000,
                                 21000,18000,12000,70000,83000),
                   lost_gen = c(106656,91038,99412,165255,
                                48634,16578,22076,250968,316259,
                                42377,37589,24761,150909,193031))
ggplot(lost_gen, aes(x = volume_af, y = lost_gen)) +
  geom_point()

power_mod <- lm(lost_gen ~ volume_af, data = lost_gen)
summary(power_mod)
predict(power_mod, tibble(volume_af = 5000))
