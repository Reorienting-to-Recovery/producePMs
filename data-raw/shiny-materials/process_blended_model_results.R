# make suure most recent R2R fall run DSM
# Rerun next line if any updates are made to fallRunDSM
# remotes::install_github("Reorienting-to-Recovery/fallRunDSM@wip", force = TRUE)
library(fallRunDSM) # make sure R2R org
# # remotes::install_github("Reorienting-to-Recovery/DSMhabitat")
# remotes::install_github("Reorienting-to-Recovery/DSMhabitat")
library(DSMhabitat)
library(producePMs)
library(tidyverse)

# Run all with updated movement hypo weights
new_params_baseline <- fallRunDSM::r_to_r_baseline_params
new_params_baseline$movement_hypo_weights <- c(1, rep(0, 7))
new_params_baseline$san_joaquin_flows <- matrix(0, nrow = 12, ncol = 21,
                                                dimnames = list(month.abb, 1980:2000))

# FALL BLENDED RUNS ------------------------------------------------------------
baseline_seeds <- fallRunDSM::fall_run_model(mode = "seed",
                                             ..params = new_params_baseline,
                                             seeds = fallRunDSM::adult_seeds)

baseline_model_results <- fallRunDSM::fall_run_model(mode = "simulate",
                                                     ..params = new_params_baseline,
                                                     seeds = baseline_seeds,
                                                     delta_surv_inflation = FALSE)

kitchen_sink_results <- fallRunDSM::fall_run_model(mode = "simulate",
                                               scenario = "kitchen_sink",
                                               ..params = new_params_baseline,
                                               seeds = baseline_seeds,
                                               delta_surv_inflation = TRUE)

dry_year_results <- fallRunDSM::fall_run_model(mode = "simulate",
                                               scenario = "dry_year",
                                               ..params = new_params_baseline,
                                               seeds = baseline_seeds,
                                               delta_surv_inflation = TRUE)

hab_and_hatchery_results <- fallRunDSM::fall_run_model(mode = "simulate",
                                                    scenario = "habitat_and_hatcheries",
                                                    ..params = new_params_baseline,
                                                    seeds = baseline_seeds,
                                                    delta_surv_inflation = TRUE)

# CREATE RESULT DFS ------------------------------------------------------------
fall_baseline_results <- create_model_results_dataframe(baseline_model_results,
                                                        model_parameters = fallRunDSM::r_to_r_baseline_params,
                                                                    "Baseline", selected_run = "fall")
fall_run_ks_results <- create_model_results_dataframe(kitchen_sink_results,
                                                      model_parameters = load_scenario(R2Rscenario::scenarios$blended_scenarios$kitchen_sink,
                                                                                       species = "fr"),
                                                                   "Kitchen Sink", selected_run = "fall")

fall_run_dy_results <- create_model_results_dataframe(dry_year_results,
                                                      model_parameters = load_scenario(R2Rscenario::scenarios$blended_scenarios$dry_year,
                                                                                       species = "fr"),
                                                      "Dry Year", selected_run = "fall")

fall_run_hh_results <- create_model_results_dataframe(hab_and_hatchery_results,
                                                      model_parameters = load_scenario(R2Rscenario::scenarios$blended_scenarios$habitat_and_hatcheries,
                                                                                       species = "fr"),
                                                      "Habitat and Hatchery", selected_run = "fall")


all_res <- bind_rows(fall_baseline_results, fall_run_ks_results, fall_run_dy_results, fall_run_hh_results)

write_csv(all_res, paste0("data-raw/shiny-materials/fall_blended_results_", Sys.Date(), ".csv"))


# PROCESS INPUTS ---------------------------------------------------------------
# library(producePMs)
fall_baseline_inputs <- create_model_inputs_tidy_df(model_parameters = fallRunDSM::r_to_r_baseline_params,
                                                                 "Baseline", selected_run = "fall")
fall_run_kitchen_sink_inputs <- create_model_inputs_tidy_df(model_parameters = load_scenario(R2Rscenario::scenarios$blended_scenarios$kitchen_sink,
                                                                                             species = "fr"),
                                                            "Kitchen Sink", selected_run = "fall")
fall_run_dry_year_inputs <- create_model_inputs_tidy_df(model_parameters = load_scenario(R2Rscenario::scenarios$blended_scenarios$dry_year,
                                                                                         species = "fr"),
                                                            "Dry Year", selected_run = "fall")
fall_run_hab_and_hatchery_inputs <- create_model_inputs_tidy_df(model_parameters = load_scenario(R2Rscenario::scenarios$blended_scenarios$habitat_and_hatcheries,
                                                                                                 species = "fr"),
                                                            "Habitat and Hatchery", selected_run = "fall")

# Add inputs for storage and deliveries from calsim nodes not within model parameters
# Files for generating the create calsim non cvia are in data raw
# Function will need to be updates if new CalSim data becomes avaliable
calsim_inputs <- create_calsim_non_cvpia_nodes_tidy() |>
  mutate(year = as.character(year))


all_inputs <- bind_rows(fall_baseline_inputs, fall_run_kitchen_sink_inputs, fall_run_dry_year_inputs,
                        fall_run_hab_and_hatchery_inputs, calsim_inputs)

write_csv(all_inputs, paste0("data-raw/shiny-materials/fall_blended_inputs_", Sys.Date(), ".csv"))

