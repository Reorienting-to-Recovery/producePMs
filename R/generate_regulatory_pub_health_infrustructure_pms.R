library(tidyverse)

### 15 ###
# Non-salmon-oriented recreation -----------------------------------------------
# Total weeks flooded for each year - take mean of annual totals, report mean and range of annual totals
produce_weeks_flooded_pm <- function(model_parameters, scenario, selected_run){
  weeks_flooded <- model_parameters$weeks_flooded |>
    as_tibble() |>
    mutate(location = fallRunDSM::watershed_labels) |>
    pivot_longer(-location, names_to = "year_date", values_to = "weeks_flooded") |>
    separate(year_date, into = c("month", "year")) |>
    mutate(scenario = scenario,
           run = selected_run) |>
    group_by(scenario, run, year, location) |>
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
## Calculated as proportion total annual volume on the Sacramento, Yuba, Tuolumne. Summarized as average unimpaired flow over 20 years and all locations.
produce_proportion_unimpaired_flow <- function(model_params, scenario, selected_run){
  if (scenario %in% c("Max Flow", "Max Flow & Max Habitat")) {
    print("Assuming 100% unimpaired flows for Run of River")
  } else {
  run_of_river_flows <- DSMflow::flows_cfs$run_of_river |>
    mutate("Lower-mid Sacramento River" = 35.6/58 * `Lower-mid Sacramento River1` + 22.4/58 * `Lower-mid Sacramento River2`) |>
    select(-`Lower-mid Sacramento River1`, -`Lower-mid Sacramento River2`) |>
    filter(year(date) > 1979 & year(date) < 2000) |>
    # filter(month(date) > 3 & month(date) <= 9) |>
    pivot_longer(cols = -date, names_to = "watershed", values_to = "flow_cfs") |>
    # filter(watershed %in% c("Yuba River", "Upper Sacramento River", "American River")) |>
    mutate(monthly_acre_feet = flow_cfs * 59.5) |>
    group_by(year = year(date), watershed) |>
    summarize(total_acre_feet_run_of_river = sum(monthly_acre_feet, na.rm = TRUE)) |>
    glimpse()

  other_flows <- DSMflow::flows_cfs$biop_itp_2018_2019 |>
    mutate("Lower-mid Sacramento River" = 35.6/58 * `Lower-mid Sacramento River1` + 22.4/58 * `Lower-mid Sacramento River2`) |>
    select(-`Lower-mid Sacramento River1`, -`Lower-mid Sacramento River2`) |>
    filter(year(date) > 1979 & year(date) < 2000) |>
    # filter(month(date) > 3 & month(date) <= 9) |>
    pivot_longer(cols = -date, names_to = "watershed", values_to = "flow_cfs") |>
    # filter(watershed %in% c("Yuba River", "Upper Sacramento River", "American River")) |>
    mutate(monthly_acre_feet = flow_cfs * 59.5) |>
    group_by(year = year(date), watershed) |>
    summarize(total_acre_feet_biop = sum(monthly_acre_feet, na.rm = TRUE)) |>
    glimpse()

  produce_unimpaired <- left_join(run_of_river_flows, other_flows) |>
    mutate(prop_unimpaired = total_acre_feet_biop/total_acre_feet_run_of_river) |>
    # filter(watershed == "Lower Sacramento River") |> glimpse()
    summarize(avg_prop_unimpaired = mean(prop_unimpaired)) |>
    glimpse()

  }
}

### 17 ###
# Flood frequency and stage for each watershed ---------------------------------
# Mark/Erin develop simple calculation based on change in peak flow from one month to next.
# TODO check below logic with Mark
produce_flood_frequency_and_stage_pm <- function(model_params, scenario, selected_run){
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
                      "Planned Plus" = DSMflow::flows_cfs$biop_itp_2018_2019,
                      "Platypus" = DSMflow::flows_cfs$eff_sac,
                      "Tortoise" = dry_year_scenario_flow,
                      "Elephant" = DSMflow::flows_cfs$LTO_12a,
                      "Elephant Plus" = DSMflow::flows_cfs$LTO_12a_eff_dy) |> # TODO update
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
prepare_hydropower_pm <- function(){
map_nodes_to_storage_facilities <- tibble(storage_facilities = c("Wiskeytown Lake",
                                                                 "Shasta Lake",
                                                                 "Keswich Reservoir",
                                                                 'Thermalito Complex',
                                                                 "Lake Oroville",
                                                                 "Stony Gorge",
                                                                 "Engilbright",
                                                                 "Folsom",
                                                                 "New Hogan",
                                                                 "New Melones",
                                                                 "New Don Padro",
                                                                 "Lake McClure",
                                                                 "Friant"),
                                          nodes = c("S3", "S4", "S5", "S7", "S6",
                                                    "S41", "S37", "S8", "S92", "S10",
                                                    "S81", "S20", "S18"))

storage_nodes <- map_nodes_to_storage_facilities$nodes

  pick_columns <- function(file, nodes) {
    col_nm <- readxl::read_excel(file, skip = 1) %>% names()
    temp <- readxl::read_excel(file, skip = 7, col_names = col_nm)
    desired_nodes <- col_nm %in% nodes
    filter <- temp |>
      rename(date = `...2`) |>
      select(date, col_nm[desired_nodes]) |>
      mutate(date = as.Date(date)) |>
      filter(year(date) <= 2003)
    cleaned <- temp |>
      rename(date = `...2`) |>
      select(date, col_nm[desired_nodes]) |>
      filter(year(date) > 2003) |>
      mutate(date = as.Date(date)-lubridate::years(100)) |>
      bind_rows(filter) |>
      filter(year(date) >= 1979, year(date) <= 2000)
    return(cleaned)
  }
  run_of_river_storage_data <- pick_columns("data-raw/run_of_river_storage.xlsx", storage_nodes) |>
    pivot_longer(-date, names_to = "nodes", values_to = "TAF") |>
    left_join(map_nodes_to_storage_facilities) |>
    mutate(scenario = "Max Flow") |> glimpse()
  baseline_storage_data <- pick_columns("data-raw/baseline_storage.xlsx", storage_nodes) |>
    pivot_longer(-date, names_to = "nodes", values_to = "TAF") |>
    left_join(map_nodes_to_storage_facilities) |>
    mutate(scenario = "Baseline") |> glimpse()

  all_storage <- bind_rows(run_of_river_storage_data, baseline_storage_data) |>
    mutate(af = TAF * 1000) |>
    group_by(storage_facilities, year = lubridate::year(date), scenario, nodes) |>
    summarize(volume_af = sum(af)) |>
    ungroup() |>
    glimpse()

  # convert to power gen
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
  power_gen_potential <- predict(power_mod, all_storage)

  all_storage$power_gen_potential <- ifelse(power_gen_potential < 0, 0, power_gen_potential)

  diff_in_power_production <- all_storage  |>
    select(-volume_af) |>
    pivot_wider(names_from = scenario, values_from = power_gen_potential) |>
    mutate(`Difference in Potential Power Production From Baseline` =  `Max Flow` - Baseline) |>
    group_by(year) |>
    summarize(`Annual Difference in Potential Power Production From Baseline` = sum(`Difference in Potential Power Production From Baseline`, na.rm = TRUE)) |>
    ungroup() |>
    mutate(scenario = "Summarized diffs for Max Flow Scenrios") |>
    group_by(scenario) |>
    summarise(`Average Annual Difference in Potential Power Production From Baseline` = mean(`Annual Difference in Potential Power Production From Baseline`),
              `Min Difference in Potential Power Production From Baseline` = min(`Annual Difference in Potential Power Production From Baseline`),
              `Max Difference in Potential Power Production From Baseline` = max(`Annual Difference in Potential Power Production From Baseline`)) |> glimpse()
}

