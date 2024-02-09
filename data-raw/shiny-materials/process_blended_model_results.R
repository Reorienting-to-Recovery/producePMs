# make suure most recent R2R fall run DSM
# remotes::install_github("Reorienting-to-Recovery/fallRunDSM", force = TRUE)
# Rerun next line if any updates are made to fallRunDSM
# remotes::install_github("Reorienting-to-Recovery/fallRunDSM@phos-refactor", force = TRUE)
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

kitchen_sink_results <- fallRunDSM::fall_run_model(mode = "simulate", ..params = fallRunDSM::r_to_r_kitchen_sink_params,
                                                     seeds = baseline_seeds)

# CREATE RESULT DFS ------------------------------------------------------------
fall_baseline_results <- create_model_results_dataframe(baseline_model_results,
                                                                    model_parameters = fallRunDSM::r_to_r_baseline_params,
                                                                    "Baseline", selected_run = "fall")
fall_run_ks_results <- create_model_results_dataframe(kitchen_sink_results,
                                                                   model_parameters = fallRunDSM::r_to_r_kitchen_sink_params,
                                                                   "Kitchen Sink", selected_run = "fall")

all_res <- bind_rows(fall_baseline_results, fall_run_ks_results)

write_csv(all_res, "data-raw/shiny-materials/fall_blended_results_feb_2024.csv")


# PROCESS INPUTS ---------------------------------------------------------------
# library(producePMs)
fall_baseline_inputs <- create_model_inputs_tidy_df(model_parameters = fallRunDSM::r_to_r_baseline_params,
                                                                 "Baseline", selected_run = "fall")
fall_run_kitchen_sink_inputs <- create_model_inputs_tidy_df(model_parameters = fallRunDSM::r_to_r_kitchen_sink_params,
                                                            "Kitchen Sink", selected_run = "fall")

# Add inputs for storage and deliveries from calsim nodes not within model parameters
calsim_inputs <- create_calsim_non_cvpia_nodes_tidy() |>
  mutate(year = as.character(year))


all_inputs <- bind_rows(fall_baseline_inputs, fall_run_kitchen_sink_inputs, calsim_inputs)

write_csv(all_inputs, "data-raw/shiny-materials/fall_blended_inputs_feb_2024.csv")

