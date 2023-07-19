# make suure most recent R2R fall run DSM
# remotes::install_github("Reorienting-to-Recovery/fallRunDSM", force = TRUE)
remotes::install_github("Reorienting-to-Recovery/fallRunDSM@phos-refactor", force = TRUE)
library(fallRunDSM) # make sure R2R org
# library(tidyverse)

# # remotes::install_github("Reorienting-to-Recovery/DSMhabitat")
# remotes::install_github("Reorienting-to-Recovery/DSMhabitat")
library(DSMhabitat)
library(producePMs)
library(tidyverse)

# FALL BOOKEND RUNS ------------------------------------------------------------
baseline_seeds <- fallRunDSM::fall_run_model(mode = "seed", ..params = fallRunDSM::r_to_r_baseline_params)
baseline_model_results <- fallRunDSM::fall_run_model(mode = "simulate", ..params = fallRunDSM::r_to_r_baseline_params,
                                                seeds = baseline_seeds)

tmh_seeds <- fallRunDSM::fall_run_model(mode = "seed", ..params = fallRunDSM::r_to_r_tmh_params)
tmh_model_results <- fallRunDSM::fall_run_model(mode = "simulate", ..params = fallRunDSM::r_to_r_tmh_params,
                                                     seeds = tmh_seeds)

no_harvest_seeds <- fallRunDSM::fall_run_model(mode = "seed", ..params = fallRunDSM::r_to_r_no_harvest_params)
no_harvest_model_results <- fallRunDSM::fall_run_model(mode = "simulate", ..params = fallRunDSM::r_to_r_no_harvest_params,
                                                seeds = no_harvest_seeds)

no_hatchery_seeds <- fallRunDSM::fall_run_model(mode = "seed", ..params = fallRunDSM::r_to_r_no_hatchery_params)
no_hatchery_model_results <- fallRunDSM::fall_run_model(mode = "simulate", ..params = fallRunDSM::r_to_r_no_hatchery_params,
                                                seeds = no_hatchery_seeds)

max_flow_seeds <- fallRunDSM::fall_run_model(mode = "seed", ..params = fallRunDSM::r_to_r_max_flow_params)
max_flow_model_results <- fallRunDSM::fall_run_model(mode = "simulate", ..params = fallRunDSM::r_to_r_max_flow_params,
                                                seeds = max_flow_seeds)

max_flow_max_hab_seeds <- fallRunDSM::fall_run_model(mode = "seed", ..params = fallRunDSM::r_to_r_max_flow_max_hab_params)
max_flow_max_hab_results <- fallRunDSM::fall_run_model(mode = "simulate", ..params = fallRunDSM::r_to_r_max_flow_max_hab_params,
                                                     seeds = max_flow_max_hab_seeds)

max_hatchery_seeds <- fallRunDSM::fall_run_model(mode = "seed", ..params = fallRunDSM::r_to_r_max_hatchery_params)
max_hatchery_results <- fallRunDSM::fall_run_model(mode = "simulate", ..params = fallRunDSM::r_to_r_max_hatchery_params,
                                                       seeds = max_hatchery_seeds)

# sit_seeds <- fallRunDSM::fall_run_model(mode = "seed", ..params = fallRunDSM::params_2022[1:119])
#
# sit_model_results <- fallRunDSM::fall_run_model(mode = "simulate", ..params = fallRunDSM::params_2022[1:119],
#                                                 seeds = sit_seeds)



fall_baseline_results <- create_model_results_dataframe(baseline_model_results,
                                                                    model_parameters = fallRunDSM::r_to_r_baseline_params,
                                                                    "Baseline", selected_run = "fall")
fall_run_tmh_results <- create_model_results_dataframe(tmh_model_results,
                                                                   model_parameters = fallRunDSM::r_to_r_tmh_params,
                                                                   "Theoretical Max Habitat", selected_run = "fall")
fall_run_no_harvest <- create_model_results_dataframe(no_harvest_model_results,
                                                                   model_parameters = fallRunDSM::r_to_r_no_harvest_params,
                                                                   "No Harvest", selected_run = "fall")
fall_run_no_hatchery <- create_model_results_dataframe(no_hatchery_model_results,
                                                                   model_parameters = fallRunDSM::r_to_r_no_hatchery_params,
                                                                   "No Hatchery", selected_run = "fall")
fall_max_flow <- create_model_results_dataframe(max_flow_model_results,
                                                                   model_parameters = fallRunDSM::r_to_r_max_flow_params,
                                                                   "Max Flow", selected_run = "fall")
fall_max_flow <- create_model_results_dataframe(max_flow_model_results,
                                                model_parameters = fallRunDSM::r_to_r_max_flow_params,
                                                "Max Flow", selected_run = "fall")
fall_max_flow_max_hab <- create_model_results_dataframe(max_flow_max_hab_results,
                                                model_parameters = fallRunDSM::r_to_r_max_flow_max_hab_params,
                                                "Max Flow & Max Habitat", selected_run = "fall")
fall_max_hatcheries <- create_model_results_dataframe(max_hatchery_results,
                                                        model_parameters = fallRunDSM::r_to_r_max_hatchery_params,
                                                        "Max Hatchery", selected_run = "fall")

all_res <- bind_rows(fall_baseline_results, fall_run_tmh_results, fall_run_no_harvest, fall_run_no_hatchery,
                     fall_max_flow, fall_max_flow_max_hab, fall_max_hatcheries)

write_csv(all_res, "data-raw/shiny-materials/fall_model_results_7_19.csv")


# PRocess inputs
# library(producePMs)
fall_baseline_inputs <- create_model_inputs_tidy_df(model_parameters = fallRunDSM::r_to_r_baseline_params,
                                                                 "Baseline", selected_run = "fall")
fall_run_tmh_inputs <- create_model_inputs_tidy_df(model_parameters = fallRunDSM::r_to_r_tmh_params,
                                                                "Theoretical Max Habitat", selected_run = "fall")
fall_run_no_harvest_inputs <- create_model_inputs_tidy_df(model_parameters = fallRunDSM::r_to_r_no_harvest_params,
                                                               "No Harvest", selected_run = "fall")
fall_run_no_hatchery_inputs <- create_model_inputs_tidy_df(model_parameters = fallRunDSM::r_to_r_no_hatchery_params,
                                                                "No Hatchery", selected_run = "fall")
fall_max_flow_inputs <- create_model_inputs_tidy_df(model_parameters = fallRunDSM::r_to_r_max_flow_params,
                                                         "Max Flow", selected_run = "fall")
fall_max_flow_max_hab_inputs <- create_model_inputs_tidy_df(model_parameters = fallRunDSM::r_to_r_max_flow_max_hab_params,
                                                           "Max Flow & Max Habitat", selected_run = "fall")
fall_max_hatchery <- create_model_inputs_tidy_df(model_parameters = fallRunDSM::r_to_r_max_hatchery_params,
                                                            "Max Hatchery", selected_run = "fall")
# Add inputs for storage and deliveries from calsim nodes not within model parameters
calsim_inputs <- create_calsim_non_cvpia_nodes_tidy() |>
  mutate(year = as.character(year))


all_inputs <- bind_rows(fall_baseline_inputs, fall_run_tmh_inputs, fall_run_no_harvest_inputs,
                     fall_run_no_hatchery_inputs, fall_max_flow_inputs, fall_max_flow_max_hab_inputs,
                     fall_max_hatchery,
                     calsim_inputs)

write_csv(all_inputs, "data-raw/shiny-materials/fall_model_inputs_7_19.csv")

