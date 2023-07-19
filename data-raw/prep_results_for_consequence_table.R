library(tidyverse)
library(producePMs)
source("data-raw/shiny-materials/process_model_results.R")
library(fallRunDSM)
# For now just running baseline through - could map through all and reduce(bind_rows())
# model_results <- create_model_results_dataframe(baseline_model_results, model_parameters = fallRunDSM::r_to_r_baseline_params, "Baseline Scenario", selected_run = "fall")
# model_params <- fallRunDSM::r_to_r_baseline_params
#
params_list <- c(fall_baseline_results,
                 fall_run_tmh_results,
                 fall_run_no_harvest,
                 fall_run_no_hatchery,
                 fall_max_flow,
                 fall_max_flow_max_hab,
                 fall_max_hatcheries)

scenarios_lists <- c("Baseline",
                     "Theoretical Max Habitat",
                     "No Harvest",
                     "No Hatchery",
                     "Max Flow",
                     "Max Flow & Max Habitat",
                     "Max Hatchery"
)
run_list <- c("fall", "fall", "fall", "fall", "fall", "fall", "fall")
### Biological Objectives ### --------------------------------------------------
# 1 #
#
produce_spawner_abundance_pm(all_res) |>
  arrange(factor(scenario, levels = c("Baseline", "Theoretical Max Habitat", "Max Flow", "No Harvest", "No Hatchery", "Max Hatchery", "Max Flow & Max Habitat"))) |>
  pivot_longer(2:4, names_to = "type", values_to = "value") |>
  pivot_wider(names_from = scenario, values_from = value) |> View()

# 2.1 #
# Average central valley wide CRR-
# produce_crr_pm(all_res) |>
#   arrange(factor(scenario, levels = c("Baseline", "Theoretical Max Habitat", "Max Flow", "No Harvest", "No Hatchery", "Max Hatchery", "Max Flow & Max Habitat"))) |>
#   pivot_longer(2:4, names_to = "type", values_to = "value") |>
#   pivot_wider(names_from = scenario, values_from = value) |> View()

# redo with geometric mean
produce_crr_geometric_mean_pm(all_res) |>
  arrange(factor(scenario, levels = c("Baseline", "Theoretical Max Habitat", "Max Flow", "No Harvest", "No Hatchery", "Max Hatchery", "Max Flow & Max Habitat"))) |>
  pivot_longer(2:4, names_to = "type", values_to = "value") |>
  pivot_wider(names_from = scenario, values_from = value) |> View()

# 2.2 #
# Average growth rate central valley wide
produce_growth_rate_pm(all_res) |>
  arrange(factor(scenario, levels = c("Baseline", "Theoretical Max Habitat", "Max Flow", "No Harvest", "No Hatchery", "Max Hatchery", "Max Flow & Max Habitat"))) |>
  pivot_longer(2:4, names_to = "type", values_to = "value") |>
  pivot_wider(names_from = scenario, values_from = value) |> View()

# 3.1 #
# Number of independent pops
produce_independent_pops_pm(all_res) |>
  arrange(factor(scenario, levels = c("Baseline", "Theoretical Max Habitat", "Max Flow", "No Harvest", "No Hatchery", "Max Hatchery", "Max Flow & Max Habitat"))) |> View()

# 3.1.1
produce_populations_present_pm(all_res) |>
  arrange(factor(scenario, levels = c("Baseline", "Theoretical Max Habitat", "Max Flow", "No Harvest", "No Hatchery", "Max Hatchery", "Max Flow & Max Habitat"))) |> View()

# 3.2 #
# of potential independent viable populations in each diversity group per ESU/run
produce_independent_pops_per_diversity_group_pm(all_res) |>
  arrange(factor(scenario, levels = c("Baseline", "Theoretical Max Habitat", "Max Flow", "No Harvest", "No Hatchery", "Max Hatchery", "Max Flow & Max Habitat"))) |> View()

# 3.3 #
# number dependent pops
produce_dependent_pops_per_diversity_group_pm(all_res, selected_run = "fall") |>
  arrange(factor(scenario, levels = c("Baseline", "Theoretical Max Habitat", "Max Flow", "No Harvest", "No Hatchery", "Max Hatchery", "Max Flow & Max Habitat"))) |> View()

# 4 #
# PHOS
produce_phos_pm(all_res) |>
  arrange(factor(scenario, levels = c("Baseline", "Theoretical Max Habitat", "Max Flow", "No Harvest", "No Hatchery", "Max Hatchery", "Max Flow & Max Habitat"))) |>
  pivot_longer(3:5, names_to = "type", values_to = "value") |>
  pivot_wider(names_from = scenario, values_from = value) |> View()

# 5.1 #
# Age distribution of spawning adults
produce_categorical_return_age_pm(all_res) |>
  arrange(factor(scenario, levels = c("Baseline", "Theoretical Max Habitat", "Max Flow", "No Harvest", "No Hatchery", "Max Hatchery", "Max Flow & Max Habitat"))) |> View()

# 5.2 #
# shannon diversity index
produce_shannon_div_ind_size_pm(all_res) |>
  arrange(factor(scenario, levels = c("Baseline", "Theoretical Max Habitat", "Max Flow", "No Harvest", "No Hatchery", "Max Hatchery", "Max Flow & Max Habitat"))) |> View()

# 5.3 #
# size distribution & month of juveniles
produce_shannon_div_ind_size_and_timing_pm(all_res) |>
  arrange(factor(scenario, levels = c("Baseline", "Theoretical Max Habitat", "Max Flow", "No Harvest", "No Hatchery", "Max Hatchery", "Max Flow & Max Habitat"))) |> View()

# 5.4 #
# TODO deal with this one

produce_carrying_capacity_vs_abundance(fall_baseline_results, r_to_r_baseline_params, "fall")
produce_carrying_capacity_vs_abundance(fall_run_tmh_results, r_to_r_tmh_params, "fall")
produce_carrying_capacity_vs_abundance(fall_run_no_harvest, r_to_r_no_harvest_params, "fall")
produce_carrying_capacity_vs_abundance(fall_run_no_hatchery, r_to_r_no_hatchery_params, "fall")
produce_carrying_capacity_vs_abundance(fall_max_flow, r_to_r_max_flow_params, "fall")
produce_carrying_capacity_vs_abundance(fall_max_flow_max_hab, r_to_r_max_flow_max_hab_params, "fall")
produce_carrying_capacity_vs_abundance(fall_max_hatcheries, r_to_r_max_hatchery_params, "fall")

# 6 #
# marine derived nutrient
produce_marine_nutrient_pm(all_res) |>
  arrange(factor(scenario, levels = c("Baseline", "Theoretical Max Habitat", "Max Flow", "No Harvest", "No Hatchery", "Max Hatchery", "Max Flow & Max Habitat"))) |> View()

# 7 #
# # time to recovery
produce_time_to_recovery_pm(all_res, selected_run = "fall")

# lump all together for quick results

### Habitat and Ecological Objectives ### --------------------------------------
# TODO get purrr logic working here to simplify
inputs <- list(params_list, scenarios_lists, run_list)
purrr::map(params_list, produce_juvenile_wetted_acre_day_pm, scenarios_lists, run_list)
# 8.1 #
# Wetted acre days of suitable juvenile rearing habitat
bind_rows(produce_juvenile_wetted_acre_day_pm(r_to_r_baseline_params, scenario = "Baseline", selected_run = "fall"),
produce_juvenile_wetted_acre_day_pm(r_to_r_tmh_params, scenario = "Theoretical Max Habitat", selected_run = "fall"),
produce_juvenile_wetted_acre_day_pm(r_to_r_no_harvest_params, scenario = "No Harvest", selected_run = "fall"),
produce_juvenile_wetted_acre_day_pm(r_to_r_no_hatchery_params, scenario = "No Hatchery", selected_run = "fall"),
produce_juvenile_wetted_acre_day_pm(r_to_r_max_flow_params, scenario = "Max Flow", selected_run = "fall"),
produce_juvenile_wetted_acre_day_pm(r_to_r_max_flow_max_hab_params, scenario = "Max Flow & Max Habitat", selected_run = "fall"),
produce_juvenile_wetted_acre_day_pm(r_to_r_max_hatchery_params, scenario = "Max Hatchery", selected_run = "fall")) |>
  arrange(factor(scenario, levels = c("Baseline", "Theoretical Max Habitat", "Max Flow", "No Harvest", "No Hatchery", "Max Hatchery", "Max Flow & Max Habitat"))) |>
  pivot_longer(3:5, names_to = "type", values_to = "value") |>
  pivot_wider(names_from = scenario, values_from = value) |> View()


# 8.2 #
# Wetted acre days of suitable spawning habitat
produce_spawning_wetted_acre_day_pm(model_params, scenario = "Baseline", selected_run = "fall")
bind_rows(produce_spawning_wetted_acre_day_pm(r_to_r_baseline_params, scenario = "Baseline", selected_run = "fall"),
          produce_spawning_wetted_acre_day_pm(r_to_r_tmh_params, scenario = "Theoretical Max Habitat", selected_run = "fall"),
          produce_spawning_wetted_acre_day_pm(r_to_r_no_harvest_params, scenario = "No Harvest", selected_run = "fall"),
          produce_spawning_wetted_acre_day_pm(r_to_r_no_hatchery_params, scenario = "No Hatchery", selected_run = "fall"),
          produce_spawning_wetted_acre_day_pm(r_to_r_max_flow_params, scenario = "Max Flow", selected_run = "fall"),
          produce_spawning_wetted_acre_day_pm(r_to_r_max_flow_max_hab_params, scenario = "Max Flow & Max Habitat", selected_run = "fall"),
          produce_spawning_wetted_acre_day_pm(r_to_r_max_hatchery_params, scenario = "Max Hatchery", selected_run = "fall")) |>
  arrange(factor(scenario, levels = c("Baseline", "Theoretical Max Habitat", "Max Flow", "No Harvest", "No Hatchery", "Max Hatchery", "Max Flow & Max Habitat"))) |>
  pivot_longer(3:5, names_to = "type", values_to = "value") |>
  pivot_wider(names_from = scenario, values_from = value) |> View()
# 8.3 #
# Spawning habitat decay rate (as a proxy for riverine condition)
# Note: should update to change between scenarios when Emanuel updates spawn_decay_multiplier
# STILL NOT WORKING - get values from EManuel and try and understand why it is so high here

total_habitat_decay(model_params, scenario = "Baseline")
bind_rows(total_habitat_decay(r_to_r_baseline_params, scenario = "Baseline", "fall"),
          total_habitat_decay(r_to_r_tmh_params, scenario = "Theoretical Max Habitat", "fall"),
          total_habitat_decay(r_to_r_no_harvest_params, scenario = "No Harvest", "fall"),
          total_habitat_decay(r_to_r_no_hatchery_params, scenario = "No Hatchery", "fall"),
          total_habitat_decay(r_to_r_max_flow_params, scenario = "Max Flow", "fall"),
          total_habitat_decay(r_to_r_max_flow_max_hab_params, scenario = "Max Flow & Max Habitat", "fall"),
          total_habitat_decay(r_to_r_max_hatchery_params, scenario = "Max Hatchery", "fall")) |>
  arrange(factor(scenario, levels = c("Baseline", "Theoretical Max Habitat", "Max Flow", "No Harvest", "No Hatchery", "Max Hatchery", "Max Flow & Max Habitat"))) |> View()
# 9.1
# Wetted acre days - total days floodplain activation occurs
produce_wetted_acre_day_floodplain_activation_pm(r_to_r_baseline_params, scenario = "Baseline", selected_run = "fall")
bind_rows(produce_wetted_acre_day_floodplain_activation_pm(r_to_r_baseline_params, scenario = "Baseline", selected_run = "fall"),
          produce_wetted_acre_day_floodplain_activation_pm(r_to_r_tmh_params, scenario = "Theoretical Max Habitat", selected_run = "fall"),
          produce_wetted_acre_day_floodplain_activation_pm(r_to_r_no_harvest_params, scenario = "No Harvest", selected_run = "fall"),
          produce_wetted_acre_day_floodplain_activation_pm(r_to_r_no_hatchery_params, scenario = "No Hatchery", selected_run = "fall"),
          produce_wetted_acre_day_floodplain_activation_pm(r_to_r_max_flow_params, scenario = "Max Flow", selected_run = "fall"),
          produce_wetted_acre_day_floodplain_activation_pm(r_to_r_max_flow_max_hab_params, scenario = "Max Flow & Max Habitat", selected_run = "fall"),
          produce_wetted_acre_day_floodplain_activation_pm(r_to_r_max_hatchery_params, scenario = "Max Hatchery", selected_run = "fall")) |>
  arrange(factor(scenario, levels = c("Baseline", "Theoretical Max Habitat", "Max Flow", "No Harvest", "No Hatchery", "Max Hatchery", "Max Flow & Max Habitat"))) |>
  pivot_longer(3:5, names_to = "type", values_to = "value") |>
  pivot_wider(names_from = scenario, values_from = value) |> View()
# 9.2 #
# Functional Flow Metric
# TODO

# 9.3 #
# TODO figure out logic for wetted acre days of floodplain
produce_2yr_30d_floodplain_acres_pm
bind_rows(produce_2yr_30d_floodplain_acres_pm(r_to_r_baseline_params, scenario = "Baseline", selected_run = "fall"),
          produce_2yr_30d_floodplain_acres_pm(r_to_r_tmh_params, scenario = "Theoretical Max Habitat", selected_run = "fall"),
          produce_2yr_30d_floodplain_acres_pm(r_to_r_no_harvest_params, scenario = "No Harvest", selected_run = "fall"),
          produce_2yr_30d_floodplain_acres_pm(r_to_r_no_hatchery_params, scenario = "No Hatchery", selected_run = "fall"),
          produce_2yr_30d_floodplain_acres_pm(r_to_r_max_flow_params, scenario = "Max Flow", selected_run = "fall"),
          produce_2yr_30d_floodplain_acres_pm(r_to_r_max_flow_max_hab_params, scenario = "Max Flow & Max Habitat", selected_run = "fall"),
          produce_2yr_30d_floodplain_acres_pm(r_to_r_max_hatchery_params, scenario = "Max Hatchery", selected_run = "fall")) |>
  arrange(factor(scenario, levels = c("Baseline", "Theoretical Max Habitat", "Max Flow", "No Harvest", "No Hatchery", "Max Hatchery", "Max Flow & Max Habitat"))) |>
  pivot_longer(3, names_to = "type", values_to = "value") |>
  pivot_wider(names_from = scenario, values_from = value) |> View()
### Access and Economics ### ---------------------------------------------------
# 10#
# Land/water access – Indigenous/cultural
# Value from tribes, flowwest does not need to do

# 11 #
# Managed wetlands
all_inputs |>
  filter(performance_metric == "11 Managed Wetlands: Refuge Water Supply and Delivery") |>
  group_by(scenario) |>
  summarise(total_deliveries_to_wetlands_TAF = sum(value, na.rm = TRUE)/1000) # convert to TAF

# 12.1 #
# Annual number of adults in rivers
# (above abundance numbers required to meet biological objectives)
produce_spawner_abundance_above_biological_objective_river_pm(all_res) |>
  arrange(factor(scenario, levels = c("Baseline", "Theoretical Max Habitat", "Max Flow", "No Harvest", "No Hatchery", "Max Hatchery", "Max Flow & Max Habitat"))) |>
  pivot_longer(2:4, names_to = "type", values_to = "value") |>
  pivot_wider(names_from = scenario, values_from = value) |> View()



produce_spawner_abundance_above_biological_objective_ocean_pm(all_res) |>
  arrange(factor(scenario, levels = c("Baseline", "Theoretical Max Habitat", "Max Flow", "No Harvest", "No Hatchery", "Max Hatchery", "Max Flow & Max Habitat"))) |>
  pivot_longer(2:4, names_to = "type", values_to = "value") |>
  pivot_wider(names_from = scenario, values_from = value) |> View()


# 12.3 #
# % of years where annual number of adults in rivers and oceans (above abundance numbers required to meet biological objectives)
# is >= 200K (minimum annual number of harvestable fish to support Indigenous, recreational, and commercial uses)
produce_percent_harvestable_abv_threshold_pm(fall_baseline_results, selected_scenario = "Baseline")
produce_percent_harvestable_abv_threshold_pm(fall_run_tmh_results, selected_scenario = "Theoretical Max Hab")
produce_percent_harvestable_abv_threshold_pm(fall_run_no_harvest, selected_scenario = "No Harvest")
produce_percent_harvestable_abv_threshold_pm(fall_run_no_hatchery, selected_scenario = "No Hatchery")
produce_percent_harvestable_abv_threshold_pm(fall_max_flow, selected_scenario = "Max Flow")
produce_percent_harvestable_abv_threshold_pm(fall_max_flow_max_hab, selected_scenario = "Max Flow and Hab")
produce_percent_harvestable_abv_threshold_pm(fall_max_hatchery, selected_scenario = "Max Hatchery")



# 13.1 #
# Water supply and delivery	Annual acre ft of water divertible water for agriculture (average for wetter years and drier years)
all_inputs |>
  filter(performance_metric == "13.1 Agricultural Water Supply and Delivery") |>
  group_by(scenario, year) |>
  summarise(total_deliveries_to_ag = sum(value, na.rm = TRUE)) |>
  group_by(scenario) |>
  summarise(avg_annual_del = mean(total_deliveries_to_ag, na.rm = TRUE)/1000) # convert to TAF

# 13.2 #
# Annual acre ft of water divertible water for municipalities (average for wetter years and drier years)
all_inputs |>
  filter(performance_metric == "13.2 Municipal Water Supply and Delivery") |>
  group_by(scenario, year) |>
  summarise(total_deliveries_to_mni = sum(value, na.rm = TRUE)) |>
  group_by(scenario) |>
  summarise(avg_annual_del = mean(total_deliveries_to_mni, na.rm = TRUE)/1000) # convert to TAF
# 14 #
# Acres in ag production
# Maddee and Alison are working on this

### Regulatory and Public Health ### -------------------------------------------

# 15 #
# Non-salmon-oriented recreation
# Total weeks flooded for each year - take mean of annual totals, report mean and range of annual totals
produce_weeks_flooded_pm(r_to_r_baseline_params, scenario = "Baseline", selected_run = "fall")
bind_rows(produce_weeks_flooded_pm(r_to_r_baseline_params, scenario = "Baseline", selected_run = "fall"),
          produce_weeks_flooded_pm(r_to_r_tmh_params, scenario = "Theoretical Max Habitat", selected_run = "fall"),
          produce_weeks_flooded_pm(r_to_r_no_harvest_params, scenario = "No Harvest", selected_run = "fall"),
          produce_weeks_flooded_pm(r_to_r_no_hatchery_params, scenario = "No Hatchery", selected_run = "fall"),
          produce_weeks_flooded_pm(r_to_r_max_flow_params, scenario = "Max Flow", selected_run = "fall"),
          produce_weeks_flooded_pm(r_to_r_max_flow_max_hab_params, scenario = "Max Flow & Max Habitat", selected_run = "fall"),
          produce_weeks_flooded_pm(r_to_r_max_hatchery_params, scenario = "Max Hatchery", selected_run = "fall")) |>
  arrange(factor(scenario, levels = c("Baseline", "Theoretical Max Habitat", "Max Flow", "No Harvest", "No Hatchery", "Max Hatchery", "Max Flow & Max Habitat"))) |>
  pivot_longer(3:5, names_to = "type", values_to = "value") |>
  pivot_wider(names_from = scenario, values_from = value) |> View()
# 16.1 #
all_inputs |>
  filter(performance_metric == "13.2 Municipal Water Supply and Delivery") |>
  group_by(scenario, year) |>
  summarise(total_deliveries_to_mni = sum(value, na.rm = TRUE)) |>
  group_by(scenario) |>
  summarise(avg_annual_del = mean(total_deliveries_to_mni, na.rm = TRUE))

# 16.2 #
# Proportion Unimparied - compare total acre feet per year to
#TODO do not know how we would classify baseline unimparied
# Calculated as proportion total annual volume on the Sacramento, Yuba, Tuolumne. Summarized as average unimpaired flow over 20 years and all locations.
# Calculation is not workinng saying Run of River is more impaired than Baseline - need new approach


# 17 #
# Flood frequency and stage for each watershed
# TODO check assumptions with Mark
produce_flood_frequency_and_stage_pm(model_params, scenario = "Baseline", selected_run = "fall")
bind_rows(produce_flood_frequency_and_stage_pm(r_to_r_baseline_params, scenario = "Baseline", selected_run = "fall"),
          produce_flood_frequency_and_stage_pm(r_to_r_tmh_params, scenario = "Theoretical Max Habitat", selected_run = "fall"),
          produce_flood_frequency_and_stage_pm(r_to_r_no_harvest_params, scenario = "No Harvest", selected_run = "fall"),
          produce_flood_frequency_and_stage_pm(r_to_r_no_hatchery_params, scenario = "No Hatchery", selected_run = "fall"),
          produce_flood_frequency_and_stage_pm(r_to_r_max_flow_params, scenario = "Max Flow", selected_run = "fall"),
          produce_flood_frequency_and_stage_pm(r_to_r_max_flow_max_hab_params, scenario = "Max Flow & Max Habitat", selected_run = "fall"),
          produce_flood_frequency_and_stage_pm(r_to_r_max_hatchery_params, scenario = "Max Hatchery", selected_run = "fall")) |>
  arrange(factor(scenario, levels = c("Baseline", "Theoretical Max Habitat", "Max Flow", "No Harvest", "No Hatchery", "Max Hatchery", "Max Flow & Max Habitat"))) |>
  pivot_longer(3:5, names_to = "type", values_to = "value") |>
  pivot_wider(names_from = scenario, values_from = value) |> View()
# 18 #
# Hydropower generation ability
all_inputs |>
  filter(performance_metric == "18 Hydropower Generation: Difference in Potential Power Produnction From Baseline") |>
  group_by(scenario, year) |>
  summarise(total_deliveries_to_mni = sum(value, na.rm = TRUE)) |>
  group_by(scenario) |>
  summarise(avg_annual_del = mean(total_deliveries_to_mni, na.rm = TRUE))
