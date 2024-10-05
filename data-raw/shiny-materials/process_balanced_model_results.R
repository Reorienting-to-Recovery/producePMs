# make suure most recent R2R fall run DSM
# Rerun next line if any updates are made to fallRunDSM
# remotes::install_github("Reorienting-to-Recovery/fallRunDSM@wip", force = TRUE)
library(fallRunDSM) # make sure R2R org
# # remotes::install_github("Reorienting-to-Recovery/DSMhabitat")
# remotes::install_github("Reorienting-to-Recovery/R2Rscenarios")
# remotes::install_github("Reorienting-to-Recovery/DSMflow")
library(DSMhabitat)
library(producePMs)
library(tidyverse)
library(R2Rscenario)

# remotes::install_github("Reorienting-to-Recovery/fallRunDSM@wip", force = TRUE)
#remotes::install_github("Reorienting-to-Recovery/R2Rscenarios", force = TRUE)

# Run all with updated movement hypo weights
new_params_baseline <- fallRunDSM::r_to_r_baseline_params
new_params_baseline$movement_hypo_weights <- c(1, rep(0, 7))
new_params_baseline$san_joaquin_flows <- matrix(0, nrow = 12, ncol = 21,
                                                dimnames = list(month.abb, 1980:2000))

# FALL BALANCED RUNS ------------------------------------------------------------
baseline_seeds <- fallRunDSM::fall_run_model(mode = "seed",
                                             ..params = new_params_baseline,
                                             seeds = fallRunDSM::adult_seeds)

baseline_model_results <- fallRunDSM::fall_run_model(mode = "simulate",
                                                     ..params = new_params_baseline,
                                                     seeds = baseline_seeds,
                                                     delta_surv_inflation = FALSE)

platypus_results <- fallRunDSM::fall_run_model(mode = "simulate",
                                               scenario = "platypus",
                                               ..params = new_params_baseline,
                                               seeds = baseline_seeds,
                                               delta_surv_inflation = TRUE)

elephant_results <- fallRunDSM::fall_run_model(mode = "simulate",
                                               scenario = "elephant",
                                               ..params = new_params_baseline,
                                               seeds = baseline_seeds,
                                               delta_surv_inflation = TRUE)

elephant_plus_results <- fallRunDSM::fall_run_model(mode = "simulate",
                                                   scenario = "elephant_plus",
                                                   ..params = new_params_baseline,
                                                   seeds = baseline_seeds,
                                                   delta_surv_inflation = TRUE)

tortoise_results <- fallRunDSM::fall_run_model(mode = "simulate",
                                               scenario = "tortoise",
                                               ..params = new_params_baseline,
                                               seeds = baseline_seeds,
                                               delta_surv_inflation = TRUE)
# CREATE RESULT DFS ------------------------------------------------------------
fall_baseline_results <- create_model_results_dataframe(baseline_model_results,
                                                        model_parameters = fallRunDSM::r_to_r_baseline_params,
                                                        "Baseline",
                                                        selected_run = "fall")

fall_run_platypus_results <- create_model_results_dataframe(platypus_results,
                                                            model_parameters = load_scenario(R2Rscenario::scenarios$balanced_scenarios$platypus,
                                                                                             species = "fr"),
                                                            "Platypus", selected_run = "fall")

fall_run_tortoise_results <- create_model_results_dataframe(tortoise_results,
                                                            model_parameters = load_scenario(R2Rscenario::scenarios$balanced_scenarios$tortoise,
                                                                                             species = "fr"),
                                                            "Tortoise", selected_run = "fall")
fall_run_elephant_results <- create_model_results_dataframe(elephant_results,
                                                            model_parameters = load_scenario(R2Rscenario::scenarios$balanced_scenarios$elephant,
                                                                                             species = "fr"),
                                                            "Elephant", selected_run = "fall")

fall_run_elephant_plus_results <- create_model_results_dataframe(elephant_results,
                                                            model_parameters = load_scenario(R2Rscenario::scenarios$balanced_scenarios$elephant_plus,
                                                                                             species = "fr"),
                                                            "Elephant Plus", selected_run = "fall")

all_res <- bind_rows(fall_baseline_results, fall_run_platypus_results, fall_run_tortoise_results,
                     fall_run_elephant_results, fall_run_elephant_plus_results)

write_csv(all_res, paste0("data-raw/shiny-materials/fall_balanced_results_", Sys.Date(), ".csv"))


# PROCESS INPUTS ---------------------------------------------------------------
# library(producePMs)
fall_baseline_inputs <- create_model_inputs_tidy_df(model_parameters = fallRunDSM::r_to_r_baseline_params,
                                                    "Baseline", selected_run = "fall")

fall_run_platypus_inputs <- create_model_inputs_tidy_df(model_parameters = load_scenario(R2Rscenario::scenarios$balanced_scenarios$platypus,
                                                                                             species = "fr"),
                                                            "Platypus", selected_run = "fall")

fall_run_tortoise_inputs <- create_model_inputs_tidy_df(model_parameters = load_scenario(R2Rscenario::scenarios$balanced_scenarios$tortoise,
                                                                                         species = "fr"),
                                                        "Tortoise", selected_run = "fall")
fall_run_elephant_inputs <- create_model_inputs_tidy_df(model_parameters = load_scenario(R2Rscenario::scenarios$balanced_scenarios$elephant,
                                                                                         species = "fr"),
                                                        "Elephant", selected_run = "fall")

fall_run_elephant_plus_inputs <- create_model_inputs_tidy_df(model_parameters = load_scenario(R2Rscenario::scenarios$balanced_scenarios$elephant_plus,
                                                                                         species = "fr"),
                                                        "Elephant Plus", selected_run = "fall")

# Add inputs for storage and deliveries from calsim nodes not within model parameters
# Files for generating the create calsim non cvia are in data raw
# Function will need to be updates if new CalSim data becomes avaliable
calsim_inputs <- create_calsim_non_cvpia_nodes_tidy() |>
  mutate(year = as.character(year))


all_inputs <- bind_rows(fall_baseline_inputs, fall_run_platypus_inputs, fall_run_tortoise_inputs, calsim_inputs,
                        fall_run_elephant_inputs, fall_run_elephant_plus_inputs)

write_csv(all_inputs, paste0("data-raw/shiny-materials/fall_balanced_inputs_", Sys.Date(), ".csv"))

