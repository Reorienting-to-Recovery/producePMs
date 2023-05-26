# make suure most recent R2R fall run DSM
# remotes::install_github("Reorienting-to-Recovery/fallRunDSM", force = TRUE)
remotes::install_github("Reorienting-to-Recovery/fallRunDSM@phos-refactor", force = TRUE)
library(fallRunDSM) # make sure R2R org
library(tidyverse)
# remotes::install_github("Reorienting-to-Recovery/DSMhabitat")
library(DSMhabitat)

# 0 - BASELINE ---------------------------------------------------------------------
library(fallRunDSM)
baseline_seeds <- fallRunDSM::fall_run_model(mode = "seed", ..params = fallRunDSM::r_to_r_baseline_params)

baseline_model_results <- fallRunDSM::fall_run_model(mode = "simulate", ..params = fallRunDSM::r_to_r_baseline_params,
                                                     seeds = baseline_seeds)
# 1 - TMH --------------------------------------------------------------------------
# TODO add hatchery_release to max habitat params
# tmh_seeds <- fallRunDSM::fall_run_model(mode = "seed", ..params = fallRunDSM::r_to_r_tmh_params)
#
# tmh_model_results <- fallRunDSM::fall_run_model(mode = "simulate", ..params = fallRunDSM::r_to_r_tmh_params,
#                                                 seeds = tmh_seeds)
# 2 - Run of River Flows  ----------------------------------------------------------
# Maddee working on incorporating now to create data objects, we will need to create max_flow params

# 3 - No harvest -------------------------------------------------------------------
# update adult_harvest_rate to 0 - update params

# 4 - No hatchery  -----------------------------------------------------------------
# update hatchery release to 0

# 5 - Max hatchery  ----------------------------------------------------------------
# Update hatchery release to max numbers - need to get these from Rene
#
# 6 - TMH & Max Flows --------------------------------------------------------------
# We will need to rerun max hab with max flows and create new params list with Max hab & max flows
#
# TODO WE WILL NEED TO RUN THESE 6 SCENARIOS FOR ALL RUNS (FALL SPRING WINTER)
