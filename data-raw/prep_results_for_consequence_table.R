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
# TODO

# 5.2 #
# shannon diversity index
produce_shannon_div_ind_size_pm(model_results)

# 5.3 #
# size distribution & month of juveniles
produce_shannon_div_ind_size_and_timing_pm(model_results)

# 5.4 #
# TODO

# 6 #
# marine derived nutrient
produce_marine_nutrient_pm(model_results)

# 7 #
# # time to recovery
# TODO add this
