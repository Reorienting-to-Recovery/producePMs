library(tidyverse)
library(producePMs)
source("data-raw/run_model_scenarios.R")
# For now just running baseline through - could map through all and reduce(bind_rows())
model_results <- create_model_results_dataframe(baseline_model_results, "Baseline Scenario", "fall")
model_params <- fallRunDSM::r_to_r_baseline_params
### Biological Objectives ### --------------------------------------------------
# 1 #
produce_spawner_abundance_pm(model_results)

# 2.1 #
# Average central valley wide CRR-
produce_crr_pm(model_results)

# 2.2 #
# Average growth rate central valley wide
produce_growth_rate_pm(model_results)

# 3.1 #
# Number of independent pops
produce_independent_pops_pm(model_results)

# 3.2 #
# of potential independent viable populations in each diversity group per ESU/run
produce_independent_pops_per_diversity_group_pm(model_results)

# 3.3 #
# number dependent pops
produce_dependent_pops_per_diversity_group_pm(model_results, selected_run = "fall")

# 4 #
# PHOS
produce_phos_pm(model_results)

# 5.1 #
# Age distribution of spawning adults
produce_categorical_return_age_pm(model_results)

# 5.2 #
# shannon diversity index
produce_shannon_div_ind_size_pm(model_results)

# 5.3 #
# size distribution & month of juveniles
produce_shannon_div_ind_size_and_timing_pm(model_results)

# 5.4 #
produce_carrying_capacity_vs_abundance(model_results, model_params, "fall")

# 6 #
# marine derived nutrient
produce_marine_nutrient_pm(model_results)

# 7 #
# # time to recovery
produce_time_to_recovery_pm(model_results, selected_run = "fall")



### Habitat and Ecological Objectives ### --------------------------------------

# 8.1 #
# Wetted acre days of suitable juvenile rearing habitat
produce_juvenile_wetted_acre_day_pm(model_params, scenario = "Baseline", selected_run = "fall")

# 8.2 #
# Wetted acre days of suitable spawning habitat
produce_spawning_wetted_acre_day_pm(model_params, scenario = "Baseline", selected_run = "fall")

# 8.3 #
# Spawning habitat decay rate (as a proxy for riverine condition)
# Note: should update to change between scenarios when Emanuel updates spawn_decay_multiplier
total_habitat_decay(model_params, scenario = "Baseline")

# 9.1 & 9.3 #
# Wetted acre days - total days floodplain activation occurs
produce_wetted_acre_day_floodplain_activation_pm(model_params, scenario = "Baseline", selected_run = "fall")

# 9.2 #
# Functional Flow Metric
# TODO


### Access and Economics ### ---------------------------------------------------
# 10#
# Land/water access – Indigenous/cultural
# Value from tribes, flowwest does not need to do

# 11 #
# Managed wetlands
# Value from elsewhere, flowwest does not need to do

# 12.1 #
# Annual number of adults in rivers
# (above abundance numbers required to meet biological objectives)
produce_spawner_abundance_above_biological_objective_pm(model_results, scenario = "Baseline")

# 12.3 #
# % of years where annual number of adults in rivers and oceans (above abundance numbers required to meet biological objectives)
# is >= 200K (minimum annual number of harvestable fish to support Indigenous, recreational, and commercial uses)
produce_percent_harvestable_abv_threshold_pm(model_results_df = model_results, scenario = "Baseline")

# 12.3 #
# % of years where annual number of adults in rivers and oceans (above abundance numbers required to meet biological objectives)
# is >= 200K (minimum annual number of harvestable fish to support Indigenous, recreational, and commercial uses)
# 12.4 consecuative years #
produce_percent_harvestable_abv_threshold_pm(model_results_df = model_results, scenario = "Baseline")

# 13.1 #
# Water supply and delivery	Annual acre ft of water divertible water for agriculture (average for wetter years and drier years)
# Get from Trend Report

# 13.2 #
# Annual acre ft of water divertible water for municipalities (average for wetter years and drier years)
# Get from Trend Report

# 14 #
# Acres in ag production
# Maddee and Alison are working on this

### Regulatory and Public Health ### -------------------------------------------

# 15 #
# Non-salmon-oriented recreation
# Total weeks flooded for each year - take mean of annual totals, report mean and range of annual totals
produce_weeks_flooded_pm(model_params, scenario = "Baseline", selected_run = "fall")

# 16.1 #
# Pull From Trend Report

# 16.2 #
# Proportion Unimparied
#TODO do not know how we would classify baseline unimparied

# 17 #
# Flood frequency and stage for each watershed
# TODO check assumptions with Mark
produce_flood_frequency_and_stage_pm(model_params, scenario = "Baseline", selected_run = "fall")

# 18 #
# Hydropower generation ability
# Still need to pull storage levels from CalSim Runs

