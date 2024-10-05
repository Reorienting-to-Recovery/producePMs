library(tidyverse)
library(producePMs)
source("data-raw/shiny-materials/process_balanced_model_results.R")
library(fallRunDSM)
# For now just running baseline through - could map through all and reduce(bind_rows())
# model_results <- create_model_results_dataframe(baseline_model_results, model_parameters = fallRunDSM::r_to_r_baseline_params, "Baseline Scenario", selected_run = "fall")
# model_params <- fallRunDSM::r_to_r_baseline_params
#
params_list <- c(fall_baseline_results,
                 fall_run_platypus_results,
                 fall_run_tortoise_results,
                 fall_run_elephant_results)

scenarios_lists <- c("Baseline",
                     "Platypus",
                     "Tortoise",
                     "Elephant",
                     "Elephant Plus"
)
run_list <- c("fall", "fall", "fall", "fall", "fall")

r_to_r_platypus_params <- load_scenario(R2Rscenario::scenarios$balanced_scenarios$platypus,
                                        species = "fr")
r_to_r_tortoise_params <- load_scenario(R2Rscenario::scenarios$balanced_scenarios$tortoise,
                                        species = "fr")
r_to_r_elephant_params <- load_scenario(R2Rscenario::scenarios$balanced_scenarios$elephant,
                                        species = "fr")
r_to_r_elephant_plus_params <- load_scenario(R2Rscenario::scenarios$balanced_scenarios$elephant_plus,
                                             species = "fr")
### Biological Objectives ### --------------------------------------------------
# 1 #
#
produce_spawner_abundance_pm(all_res) |>
  arrange(factor(scenario, levels = c("Baseline","Tortoise", "Platypus",  "Elephant", "Elephant Plus"))) |>
  pivot_longer(2:4, names_to = "type", values_to = "value") |>
  pivot_wider(names_from = scenario, values_from = value) |>
  mutate_if(is.numeric, pretty_num) |> View()

# 2.1 #
# Average central valley wide CRR-
# produce_crr_pm(all_res) |>
#   arrange(factor(scenario, levels = c("Baseline", "Theoretical Max Habitat", "Max Flow", "No Harvest", "No Hatchery", "Max Hatchery", "Max Flow & Max Habitat"))) |>
#   pivot_longer(2:4, names_to = "type", values_to = "value") |>
#   pivot_wider(names_from = scenario, values_from = value) |> View()

# redo with geometric mean
produce_crr_geometric_mean_pm(all_res) |>
  arrange(factor(scenario, levels = c("Baseline","Tortoise", "Platypus",  "Elephant", "Elephant Plus"))) |>
  pivot_longer(2:4, names_to = "type", values_to = "value") |>
  mutate_if(is.numeric, pretty_num) |>
  pivot_wider(names_from = scenario, values_from = value) |> View()

# 2.2 #
# Average growth rate central valley wide
produce_growth_rate_pm(all_res) |>
  arrange(factor(scenario, levels = c("Baseline","Tortoise", "Platypus",  "Elephant", "Elephant Plus"))) |>
  pivot_longer(2:4, names_to = "type", values_to = "value") |>
  mutate_if(is.numeric, pretty_num) |>
  pivot_wider(names_from = scenario, values_from = value) |> View()

# 3.1 #
# Number of independent pops
produce_independent_pops_pm(all_res) |>
  arrange(factor(scenario, levels = c("Baseline","Tortoise", "Platypus",  "Elephant", "Elephant Plus"))) |> View()

# 3.1.1
produce_populations_present_pm(all_res)
# 3.2 #
# of potential independent viable populations in each diversity group per ESU/run
produce_independent_pops_per_diversity_group_pm(all_res) |>
  arrange(factor(scenario, levels = c("Baseline","Tortoise", "Platypus",  "Elephant", "Elephant Plus"))) |> View()

# LOoks like now this is total independent / possible ind (looks like possible ind 20 tribs * 16 years (only possible to meet criteria in 16 years)) so 320

# 3.3 #
# number dependent pops
produce_dependent_pops_per_diversity_group_pm(all_res, selected_run = "fall") |>
  arrange(factor(scenario, levels = c("Baseline","Tortoise", "Platypus",  "Elephant", "Elephant Plus"))) |> View()

# 4 #
# PHOS
produce_phos_pm(all_res) |>
  arrange(factor(scenario, levels = c("Baseline","Tortoise", "Platypus",  "Elephant", "Elephant Plus"))) |>
  pivot_longer(3:5, names_to = "type", values_to = "value") |>
  mutate_if(is.numeric, pretty_num) |>
  pivot_wider(names_from = scenario, values_from = value) |> View()

# 5.1 #
# Age distribution of spawning adults
produce_categorical_return_age_pm(all_res) |>
  arrange(factor(scenario, levels = c("Baseline","Tortoise", "Platypus",  "Elephant", "Elephant Plus"))) |> View()

# 5.2 #
# shannon diversity index
produce_shannon_div_ind_size_pm(all_res) |>
  mutate_if(is.numeric, pretty_num) |>
  arrange(factor(scenario, levels = c("Baseline","Tortoise", "Platypus",  "Elephant", "Elephant Plus"))) |> View()

# 5.3 #
# size distribution & month of juveniles
produce_shannon_div_ind_size_and_timing_pm(all_res) |>
  mutate_if(is.numeric, pretty_num) |>
  arrange(factor(scenario, levels = c("Baseline","Tortoise", "Platypus",  "Elephant", "Elephant Plus"))) |> View()

# 5.4 #
produce_floodplain_over_inchannel_habitat(fall_baseline_results, r_to_r_baseline_params, "fall", "Baseline") |>
  bind_rows(produce_floodplain_over_inchannel_habitat(fall_run_platypus_inputs, r_to_r_platypus_params, "fall", "Platypus"),
            produce_floodplain_over_inchannel_habitat(fall_run_tortoise_results, r_to_r_tortoise_params, "fall", "Tortoise"),
            produce_floodplain_over_inchannel_habitat(fall_run_elephant_results, r_to_r_elephant_params, "fall", "Elephant"),
            produce_floodplain_over_inchannel_habitat(fall_run_elephant_results, r_to_r_elephant_params, "fall", "Elephant Plus")) |>
  arrange(factor(scenario, levels = c("Baseline", "Tortoise", "Platypus", "Elephant", "Elephant Plus"))) |>
  pivot_longer(2:4, names_to = "type", values_to = "value") |>
  pivot_wider(names_from = scenario, values_from = value) |> View()
produce_floodplain_over_inchannel_habitat(fall_run_dy_results, r_to_r_dry_years_params, "fall", "Dry Year")
produce_floodplain_over_inchannel_habitat(fall_run_hh_results, r_to_r_habitat_and_hatchery_params, "fall", "Habitat and Hatchery")
produce_floodplain_over_inchannel_habitat(fall_run_ks_results, r_to_r_kitchen_sink_params, "fall", "Kitchen Sink")
produce_floodplain_over_inchannel_habitat(fall_run_pc_results, r_to_r_planned_and_current, "fall", "Planned Plus")

# 6 #
# marine derived nutrient
produce_marine_nutrient_pm(all_res) |>
  mutate_if(is.numeric, pretty_num) |>
  arrange(factor(scenario, levels = c("Baseline","Tortoise", "Platypus",  "Elephant", "Elephant Plus"))) |> View()

# 7 #
# # time to recovery
produce_time_to_recovery_pm(fall_baseline_results, selected_run = "fr")
produce_time_to_recovery_pm(fall_run_platypus_results, selected_run = "fr")
produce_time_to_recovery_pm(fall_run_tortoise_results, selected_run = "fr")
produce_time_to_recovery_pm(fall_run_elephant_results, selected_run = "fr")
produce_time_to_recovery_pm(fall_run_dy_results)
produce_time_to_recovery_pm(fall_run_hh_results)
produce_time_to_recovery_pm(fall_run_ks_results)
produce_time_to_recovery_pm(fall_run_pc_results)

# lump all together for quick results

### Habitat and Ecological Objectives ### --------------------------------------
# TODO get purrr logic working here to simplify
inputs <- list(params_list, scenarios_lists, run_list)
purrr::map(params_list, produce_juvenile_wetted_acre_day_pm, scenarios_lists, run_list)

# 8.1 #
# Wetted acre days of suitable juvenile rearing habitat
bind_rows(produce_juvenile_wetted_acre_day_pm(r_to_r_baseline_params, scenario = "Baseline", selected_run = "fall"),
          produce_juvenile_wetted_acre_day_pm(r_to_r_platypus_params,
                                              scenario = "Platypus", selected_run = "fall"),
          produce_juvenile_wetted_acre_day_pm(r_to_r_tortoise_params,
                                              scenario = "Tortoise", selected_run = "fall"),
          produce_juvenile_wetted_acre_day_pm(r_to_r_elephant_params,
                                              scenario = "Elephant", selected_run = "fall"),
          produce_juvenile_wetted_acre_day_pm(r_to_r_elephant_plus_params,
                                              scenario = "Elephant Plus", selected_run = "fall")) |>
  arrange(factor(scenario, levels = c("Baseline", "Tortoise", "Platypus", "Elephant", "Elephant Plus"))) |>
  pivot_longer(3:5, names_to = "type", values_to = "value") |>
  mutate_if(is.numeric, pretty_num) |>
  pivot_wider(names_from = scenario, values_from = value) |> View()


# 8.2 #
# Wetted acre days of suitable spawning habitat
produce_spawning_wetted_acre_day_pm(model_params, scenario = "Baseline", selected_run = "fall")
bind_rows(produce_spawning_wetted_acre_day_pm(r_to_r_baseline_params, scenario = "Baseline", selected_run = "fall"),
          produce_spawning_wetted_acre_day_pm(r_to_r_platypus_params, scenario = "Platypus", selected_run = "fall"),
          produce_spawning_wetted_acre_day_pm(r_to_r_tortoise_params, scenario = "Tortoise", selected_run = "fall"),
          produce_spawning_wetted_acre_day_pm(r_to_r_elephant_params, scenario = "Elephant", selected_run = "fall"),
          produce_spawning_wetted_acre_day_pm(r_to_r_elephant_plus_params, scenario = "Elephant Plus", selected_run = "fall")) |>
  arrange(factor(scenario, levels = c("Baseline","Tortoise", "Platypus",  "Elephant", "Elephant Plus"))) |>
  pivot_longer(3:5, names_to = "type", values_to = "value") |>
  mutate_if(is.numeric, pretty_num) |>
  pivot_wider(names_from = scenario, values_from = value) |> View()
# 8.3 #
# Spawning habitat decay rate (as a proxy for riverine condition)
# Note: should update to change between scenarios when Emanuel updates spawn_decay_multiplier
# STILL NOT WORKING - get values from EManuel and try and understand why it is so high here

bind_rows(total_habitat_decay(r_to_r_baseline_params, scenario = "Baseline", "fall"),
          total_habitat_decay(r_to_r_platypus_params, scenario = "Platypus", "fall"),
          total_habitat_decay(r_to_r_tortoise_params, scenario = "Tortoise", "fall"),
          total_habitat_decay(r_to_r_elephant_params, scenario = "Elephant", "fall"),
          total_habitat_decay(r_to_r_elephant_plus_params, scenario = "Elephant Plus", "fall")) |>
  arrange(factor(scenario, levels = c("Baseline","Tortoise", "Platypus",  "Elephant", "Elephant Plus"))) |> View()
# 9.1
# Wetted acre days - total days floodplain activation occurs
produce_wetted_acre_day_floodplain_activation_pm(r_to_r_baseline_params, scenario = "Baseline", selected_run = "fall")
bind_rows(produce_wetted_acre_day_floodplain_activation_pm(r_to_r_baseline_params, scenario = "Baseline", selected_run = "fall"),
          produce_wetted_acre_day_floodplain_activation_pm(r_to_r_platypus_params, scenario = "Platypus", selected_run = "fall"),
          produce_wetted_acre_day_floodplain_activation_pm(r_to_r_tortoise_params, scenario = "Tortoise", selected_run = "fall"),
          produce_wetted_acre_day_floodplain_activation_pm(r_to_r_elephant_params, scenario = "Elephant", selected_run = "fall"),
          produce_wetted_acre_day_floodplain_activation_pm(r_to_r_elephant_plus_params, scenario = "Elephant Plus", selected_run = "fall")) |>
  arrange(factor(scenario, levels = c("Baseline","Tortoise", "Platypus",  "Elephant", "Elephant Plus"))) |>
  pivot_longer(3:5, names_to = "type", values_to = "value") |>
  mutate_if(is.numeric, pretty_num) |>
  pivot_wider(names_from = scenario, values_from = value) |> View()
# 9.2 #

# Functional Flow Metric
# TODO

# 9.3 #
# TODO figure out logic for wetted acre days of floodplain
produce_2yr_30d_floodplain_acres_pm
bind_rows(produce_2yr_30d_floodplain_acres_pm(r_to_r_baseline_params, scenario = "Baseline", selected_run = "fall"),
          produce_2yr_30d_floodplain_acres_pm(r_to_r_platypus_params, scenario = "Platypus", selected_run = "fall"),
          produce_2yr_30d_floodplain_acres_pm(r_to_r_tortoise_params, scenario = "Tortoise", selected_run = "fall"),
          produce_2yr_30d_floodplain_acres_pm(r_to_r_elephant_params, scenario = "Elephant", selected_run = "fall"),
          produce_2yr_30d_floodplain_acres_pm(r_to_r_elephant_plus_params, scenario = "Elephant Plus", selected_run = "fall")) |>
  arrange(factor(scenario, levels = c("Baseline","Tortoise", "Platypus",  "Elephant", "Elephant Plus"))) |>
  pivot_longer(3, names_to = "type", values_to = "value") |>
  mutate_if(is.numeric, pretty_num) |>
  pivot_wider(names_from = scenario, values_from = value) |> View()
### Access and Economics ### ---------------------------------------------------

upper_sac_flow_eff <- DSMflow::flows_cfs$eff_sac |>
  filter(year(date) > 1979, year(date) < 2000) |>
  select(date, us_eff = `Upper Sacramento River`,
         sj_eff = `San Joaquin River`)

upper_sac_flow_biop <- DSMflow::flows_cfs$biop_itp_2018_2019 |>
  filter(year(date) > 1979, year(date) < 2000) |>
  select(date, us = `Upper Sacramento River`,
         sj = `San Joaquin River`)

# for LTO we will pull in actual numbers

acre_feet <- left_join(upper_sac_flow_eff, upper_sac_flow_biop) |>
  mutate(us_af = us * 60.370,
         us_eff_af = us_eff * 60.370,
         sj_af = sj * 60.370,
         sj_eff_af = sj_eff * 60.370) |>
  group_by(year = year(date)) |>
  summarise(us = sum(us_af),
            us_eff = sum(us_eff_af),
            sj = sum(sj_af),
            sj_eff = sum(sj_eff_af)) |>
  mutate(us_acre_ft_change = us_eff - us,
         sj_acre_ft_change = sj_eff - sj)

# result platypus
us_mean_af_change <-  acre_feet |>
  pull(us_acre_ft_change) |> mean()
us_mean_af_change/1000

sj_mean_af_change <-  acre_feet |>
  pull(sj_acre_ft_change) |> mean()
sj_mean_af_change/1000

total_mean_af_change_eff <- us_mean_af_change + sj_mean_af_change
total_mean_af_change_eff/1000

water_year_types <- waterYearType::water_year_indices |>
  filter(location == "Sacramento Valley", WY %in% c(1922:2003)) |>
  select(year = WY, index = Index, year_type = Yr_type)

new_year_types <- water_year_types |>
  mutate(year_type = ifelse(year_type %in% c("Dry", "Critical", "Below Normal"),
                            "Dry", "Wet")) |>
  select(year, year_type)

# ks tortoise (dry year)
dry_year_acre_feet <- acre_feet |>
  left_join(new_year_types) |>
  mutate(us_acre_ft_change = ifelse(year_type == "Dry", us_acre_ft_change, 0),
         sj_acre_ft_change = ifelse(year_type == "Dry", sj_acre_ft_change, 0))

us_mean_af_change_dry <- dry_year_acre_feet |>
  pull(us_acre_ft_change) |> mean() / 1000
sj_mean_af_change_dry <- dry_year_acre_feet |>
  pull(sj_acre_ft_change) |> mean() / 1000

total_mean_af_change_eff_dry <- us_mean_af_change_dry + sj_mean_af_change_dry
total_mean_af_change_eff_dry

# to get municipal vs ag vs wetlands, use proportion of total acre feet
# TODO

# 10#
# Land/water access – Indigenous/cultural
# Value from tribes, flowwest does not need to do

# 11 #
# Managed wetlands
# TODO
all_inputs |>
  filter(performance_metric == "11 Managed Wetlands: Refuge Water Supply and Delivery") |>
  group_by(scenario) |>
  summarise(total_deliveries_to_wetlands_TAF = sum(value, na.rm = TRUE)/1000) |>  # convert to TAF
  mutate(mean_annual_deliveries_to_wetlands_TAF = total_deliveries_to_wetlands_TAF/20) |>
  select(-total_deliveries_to_wetlands_TAF)

# 12.1 #
# SEE new R script data-raw/terminal_hatchery_harvest.R for new harvest metrics

# Annual number of adults in rivers
# (above abundance numbers required to meet biological objectives)
produce_spawner_abundance_above_biological_objective_river_pm(all_res) |>
  arrange(factor(scenario, levels = c("Baseline","Tortoise", "Platypus",  "Elephant", "Elephant Plus"))) |>
  pivot_longer(2:4, names_to = "type", values_to = "value") |>
  mutate_if(is.numeric, pretty_num) |>
  pivot_wider(names_from = scenario, values_from = value) |> View()

produce_spawner_abundance_above_biological_objective_ocean_pm(all_res) |>
  arrange(factor(scenario, levels = c("Baseline","Tortoise", "Platypus",  "Elephant", "Elephant Plus"))) |>
  pivot_longer(2:4, names_to = "type", values_to = "value") |>
  mutate_if(is.numeric, pretty_num) |>
  pivot_wider(names_from = scenario, values_from = value) |> View()


# 12.3 #
# TODO check on this one
# % of years where annual number of adults in rivers and oceans (above abundance numbers required to meet biological objectives)
# is >= 200K (minimum annual number of harvestable fish to support Indigenous, recreational, and commercial uses)
produce_percent_harvestable_abv_threshold_pm(fall_baseline_results, selected_scenario = "Baseline")
produce_percent_harvestable_abv_threshold_pm(fall_run_dy_results, selected_scenario = "Dry Year")
produce_percent_harvestable_abv_threshold_pm(fall_run_hh_results, selected_scenario = "Habitat and Hatchery")
produce_percent_harvestable_abv_threshold_pm(fall_run_ks_results, selected_scenario = "Kitchen Sink")




# 13.1 #
# Water supply and delivery	Annual acre ft of water divertible water for agriculture (average for wetter years and drier years)
# # Only works for ones we have calsim runs for

all_inputs |>
  filter(performance_metric == "13.1 Agricultural Water Supply and Delivery") |>
  group_by(scenario, year) |>
  summarise(total_deliveries_to_ag = sum(value, na.rm = TRUE)) |>
  group_by(scenario) |>
  summarise(avg_annual_del = mean(total_deliveries_to_ag, na.rm = TRUE)/1000) # convert to TAF

# 13.2 #
# Annual acre ft of water divertible water for municipalities (average for wetter years and drier years)
# # Only works for ones we have calsim runs for

all_inputs |>
  filter(performance_metric == "13.2 Municipal Water Supply and Delivery") |>
  group_by(scenario, year) |>
  summarise(total_deliveries_to_mni = sum(value, na.rm = TRUE)) |>
  group_by(scenario) |>
  summarise(avg_annual_del = mean(total_deliveries_to_mni, na.rm = TRUE)/1000) # convert to TAF
# 14 #
# Acres in ag production
# constructed scale to do manually

### Regulatory and Public Health ### -------------------------------------------

# 15 #
# Non-salmon-oriented recreation
# Total weeks flooded for each year - take mean of annual totals, report mean and range of annual totals
produce_weeks_flooded_pm(r_to_r_baseline_params, scenario = "Baseline", selected_run = "fall")
bind_rows(produce_weeks_flooded_pm(r_to_r_baseline_params, scenario = "Baseline", selected_run = "fall"),
          produce_weeks_flooded_pm(r_to_r_platypus_params, scenario = "Platypus", selected_run = "fall"),
          produce_weeks_flooded_pm(r_to_r_elephant_params, scenario = "Elephant", selected_run = "fall"),
          produce_weeks_flooded_pm(r_to_r_tortoise_params, scenario = "Tortoise", selected_run = "fall")
          ) |>
  arrange(factor(scenario, levels = c("Baseline", "Tortoise", "Platypus", "Elephant"))) |>
  pivot_longer(3:5, names_to = "type", values_to = "value") |>
  pivot_wider(names_from = scenario, values_from = value) |> View()
# 16.1 #
all_inputs |>
  filter(performance_metric == "16.1 Delta Outflow (cfs)" ) |>
  group_by(scenario, year) |>
  summarise(total_deliveries_to_mni = sum(value, na.rm = TRUE)) |>
  group_by(scenario) |>
  summarise(avg_annual_del = mean(total_deliveries_to_mni, na.rm = TRUE))

# 16.2 #
# Proportion Unimparied - compare total acre feet per year to
#TODO do not know how we would classify baseline unimparied
# Calculated as proportion total annual volume on the Sacramento, Yuba, Tuolumne. Summarized as average unimpaired flow over 20 years and all locations.
# Calculation is not workinng saying Run of River is more impaired than Baseline - need new approach
natural_flows <- read_csv("data-raw/natural_flows_sac.csv") |>
  filter(comid == 2851441) |>
  mutate(watershed = "Upper Sacramento River") |>
  glimpse()

# COMID 2821742
natural_flows_sj <- read_csv("data-raw/flow_2821742_mean_estimated_1980_2000.csv") |>
  filter(comid == 2821742) |>
  mutate(watershed = "San Joaquin River") |>
  glimpse()

nat <- natural_flows |>
  bind_rows(natural_flows_sj) |>
  filter(year > 1979, year < 2000) |>
  rename(flow_cfs = value) |>
  mutate(scenario = "natural flow") |>
  select(year, month, scenario, flow_cfs, watershed) |>
  glimpse()

model_flows <- left_join(upper_sac_flow_eff, upper_sac_flow_biop) |>
  left_join(upper_sac_flow_LTO_12a) |>
  pivot_longer(-date, names_to = "type", values_to = "flow_cfs") |>
  mutate(watershed = ifelse(str_detect(type, "sj"),
                            "San Joaquin River", "Upper Sacramento River"),
         scenario = case_when(str_detect(type, "eff") ~ "EFF",
                              str_detect(type, "lto") ~ "Elephant",
                              TRUE ~ "Baseline")) |>
  mutate(month = month(date), year = year(date)) |>
  select(-date, -type) |>
  glimpse()

data <- bind_rows(model_flows, nat) |>
  group_by(year, scenario, watershed) |>
  summarise(total_flows = sum(flow_cfs, na.rm = TRUE)) |>
  ungroup() |>
  left_join(new_year_types) |>
  pivot_wider(names_from = scenario, values_from = total_flows) |>
  mutate(baseline_percent_of_nat_flows = Baseline / `natural flow`,
            full_eff_percent_of_nat_flows = `EFF` / `natural flow`,
            dry_eff_percent_of_nat_flows = ifelse(year_type == "Dry",
                            `EFF` / `natural flow`, Baseline / `natural flow`),
            elephant_percent_of_nat_flows = Elephant / `natural flow`) |>
  group_by(watershed) |>
  summarize(baseline_percent_of_nat_flows = mean(baseline_percent_of_nat_flows),
         full_eff_percent_of_nat_flows = mean(full_eff_percent_of_nat_flows),
         dry_eff_percent_of_nat_flows = mean(dry_eff_percent_of_nat_flows),
         elephant_percent_of_nat_flows = mean(elephant_percent_of_nat_flows))
data |>
  glimpse()

ifelse(data$baseline_percent_of_nat_flows > 1, 1, data$baseline_percent_of_nat_flows) |> mean()
ifelse(data$full_eff_percent_of_nat_flows > 1, 1, data$full_eff_percent_of_nat_flows) |> mean()
ifelse(data$dry_eff_percent_of_nat_flows > 1, 1, data$dry_eff_percent_of_nat_flows) |> mean()



# 17 #
# Flood frequency and stage for each watershed
produce_flood_frequency_and_stage_pm(r_to_r_baseline_params, scenario = "Baseline", selected_run = "fall")
bind_rows(produce_flood_frequency_and_stage_pm(r_to_r_baseline_params, scenario = "Baseline", selected_run = "fall"),
          produce_flood_frequency_and_stage_pm(r_to_r_platypus_params, scenario = "Platypus", selected_run = "fall"),
          produce_flood_frequency_and_stage_pm(r_to_r_elephant_params, scenario = "Elephant", selected_run = "fall"),
          produce_flood_frequency_and_stage_pm(r_to_r_tortoise_params, scenario = "Tortoise", selected_run = "fall")
) |>
  arrange(factor(scenario, levels = c("Baseline", "Tortoise", "Platypus", "Elephant"))) |>
  pivot_longer(3:5, names_to = "type", values_to = "value") |>
  pivot_wider(names_from = scenario, values_from = value) |> View()

# 18 #
# Hydropower generation ability
# Only works for ones we have calsim runs for
all_inputs |>
  filter(performance_metric == "18 Hydropower Generation: Difference in Potential Power Produnction From Baseline") |>
  group_by(scenario, year) |>
  summarise(total_deliveries_to_mni = sum(value, na.rm = TRUE)) |>
  group_by(scenario) |>
  summarise(avg_annual_del = mean(total_deliveries_to_mni, na.rm = TRUE))

# Update for blended scenarios
lost_gen <- tibble(volume_af = c(49000,44000,42000,74000,
                                 23000,7000,10000,120000,137000,
                                 21000,18000,12000,70000,83000),
                   lost_gen = c(106656,91038,99412,165255,
                                48634,16578,22076,250968,316259,
                                42377,37589,24761,150909,193031))

# TODO might be better to use CalSim3 storage outputs here
upper_sac_flow_LTO_12a <- DSMflow::flows_cfs$LTO_12a |>
  filter(year(date) > 1979, year(date) <= 2000) |>
  select(date, us_lto_12a = `Upper Sacramento River`,
         sj_lto_12a = `San Joaquin River`)

# assume flow change means loss of flow at reservoir
flow_change <- left_join(upper_sac_flow_eff, upper_sac_flow_biop) |>
  left_join(upper_sac_flow_LTO_12a) |>
  mutate(us_af = us * 60.370,
         us_eff_af = us_eff * 60.370,
         us_lto_af = us_lto_12a * 60.370,
         sj_af = sj * 60.370,
         sj_eff_af = sj_eff * 60.370,
         sj_lto_af = sj_lto_12a * 60.370,
         # TODO eff sj relationship is based on the feather
         eff_volume_af =  ifelse(us_eff_af - us_af > 0, us_eff_af - us_af, 0),
         lto_volume_af = ifelse(us_lto_af - us_af > 0, us_lto_af - us_af, 0),
         eff_volume_af_sj =  ifelse(sj_eff_af - sj_af > 0, sj_eff_af - sj_af, 0),
         lto_volume_af_sj = ifelse(sj_lto_af - sj_af > 0, sj_lto_af - sj_af, 0)) |>
  select(date, eff_volume_af, lto_volume_af, eff_volume_af_sj, lto_volume_af_sj) |> glimpse()

power_mod <- lm(lost_gen ~ volume_af, data = lost_gen)
# summary(power_mod)
power_gen_potential_eff <- predict(power_mod, flow_change |>
                                     select(date, volume_af = eff_volume_af))
power_gen_potential_lto <- predict(power_mod, flow_change |>
                                     select(date, volume_af = lto_volume_af))
power_gen_potential_eff_sj <- predict(power_mod, flow_change |>
                                     select(date, volume_af = eff_volume_af_sj))
power_gen_potential_lto_sj <- predict(power_mod, flow_change |>
                                     select(date, volume_af = lto_volume_af_sj))

flow_change$power_gen_potential_eff <- ifelse(power_gen_potential_eff < 0, 0, power_gen_potential_eff)
flow_change$power_gen_potential_eff_sj <- ifelse(power_gen_potential_eff_sj < 0, 0, power_gen_potential_eff_sj)
sum(flow_change$power_gen_potential_eff, flow_change$power_gen_potential_eff_sj)/1e6

flow_change$power_gen_potential_lto <- ifelse(power_gen_potential_lto < 0, 0, power_gen_potential_lto)
flow_change$power_gen_potential_lto_sj <- ifelse(power_gen_potential_lto_sj < 0, 0, power_gen_potential_lto_sj)
sum(flow_change$power_gen_potential_lto, flow_change$power_gen_potential_lto_sj)/1e6


# DRY YEAR ONE
flow_change_dy <- left_join(upper_sac_flow_eff, upper_sac_flow_biop) |>
  left_join(upper_sac_flow_LTO_12a) |>
  mutate(year = year(date)) |>
  left_join(new_year_types) |>
  mutate(us_af = us * 60.370,
         us_eff_af = us_eff * 60.370,
         sj_af = sj * 60.370,
         sj_eff_af = sj_eff * 60.370,
         volume_af =  ifelse(us_eff_af - us_af > 0, us_eff_af - us_af, 0),
         volume_af_sj =  ifelse(sj_eff_af - sj_af > 0, sj_eff_af - sj_af, 0)) |>
  mutate(volume_af = ifelse(year_type == "Dry", volume_af, 0),
         volume_af_sj = ifelse(year_type == "Dry", volume_af_sj, 0)) |>
  select(date, volume_af, volume_af_sj) |> glimpse()

us_flow_change <- flow_change_dy |> select(date, volume_af)
sj_flow_change <- flow_change_dy |> select(date, volume_af = volume_af_sj)

power_gen_potential_us <- predict(power_mod, us_flow_change)
power_gen_potential_sj <- predict(power_mod, sj_flow_change)

us_flow_change$power_gen_potential <- ifelse(power_gen_potential_us < 0, 0, power_gen_potential_us)
sj_flow_change$power_gen_potential <- ifelse(power_gen_potential_sj < 0, 0, power_gen_potential_sj)
sum(us_flow_change$power_gen_potential, sj_flow_change$power_gen_potential) / 1e6
