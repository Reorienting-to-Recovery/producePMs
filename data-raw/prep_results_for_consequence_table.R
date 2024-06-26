library(tidyverse)
library(producePMs)
source("data-raw/shiny-materials/process_blended_model_results.R")
library(fallRunDSM)
# For now just running baseline through - could map through all and reduce(bind_rows())
# model_results <- create_model_results_dataframe(baseline_model_results, model_parameters = fallRunDSM::r_to_r_baseline_params, "Baseline Scenario", selected_run = "fall")
# model_params <- fallRunDSM::r_to_r_baseline_params
#
params_list <- c(fall_baseline_results,
                 fall_run_dy_results,
                 fall_run_ks_results,
                 fall_run_hh_results,
                 fall_run_pc_results)

scenarios_lists <- c("Baseline",
                     "Dry Year",
                     "Kitchen Sink",
                     "Habitat and Hatchery",
                     "Planned Plus"
)
run_list <- c("fall", "fall", "fall", "fall", "fall")
### Biological Objectives ### --------------------------------------------------
# 1 #
#
produce_spawner_abundance_pm(all_res) |>
  arrange(factor(scenario, levels = c("Baseline", "Theoretical Max Habitat", "Max Flow", "No Harvest", "No Hatchery", "Max Hatchery", "Max Flow & Max Habitat"))) |>
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
  arrange(factor(scenario, levels = c("Baseline", "Theoretical Max Habitat", "Max Flow", "No Harvest", "No Hatchery", "Max Hatchery", "Max Flow & Max Habitat"))) |>
  pivot_longer(2:4, names_to = "type", values_to = "value") |>
  mutate_if(is.numeric, pretty_num) |>
  pivot_wider(names_from = scenario, values_from = value) |> View()

# 2.2 #
# Average growth rate central valley wide
produce_growth_rate_pm(all_res) |>
  arrange(factor(scenario, levels = c("Baseline", "Theoretical Max Habitat", "Max Flow", "No Harvest", "No Hatchery", "Max Hatchery", "Max Flow & Max Habitat"))) |>
  pivot_longer(2:4, names_to = "type", values_to = "value") |>
  mutate_if(is.numeric, pretty_num) |>
  pivot_wider(names_from = scenario, values_from = value) |> View()

# 3.1 #
# Number of independent pops
produce_independent_pops_pm(all_res) |>
  arrange(factor(scenario, levels = c("Baseline", "Theoretical Max Habitat", "Max Flow", "No Harvest", "No Hatchery", "Max Hatchery", "Max Flow & Max Habitat"))) |> View()

# 3.1.1
produce_populations_present_pm(all_res)
# 3.2 #
# of potential independent viable populations in each diversity group per ESU/run
produce_independent_pops_per_diversity_group_pm(all_res) |>
  arrange(factor(scenario, levels = c("Baseline", "Theoretical Max Habitat", "Max Flow", "No Harvest", "No Hatchery", "Max Hatchery", "Max Flow & Max Habitat"))) |> View()

# LOoks like now this is total independent / possible ind (looks like possible ind 20 tribs * 16 years (only possible to meet criteria in 16 years)) so 320

# 3.3 #
# number dependent pops
produce_dependent_pops_per_diversity_group_pm(all_res, selected_run = "fall") |>
  arrange(factor(scenario, levels = c("Baseline", "Theoretical Max Habitat", "Max Flow", "No Harvest", "No Hatchery", "Max Hatchery", "Max Flow & Max Habitat"))) |> View()

# 4 #
# PHOS
produce_phos_pm(all_res) |>
  arrange(factor(scenario, levels = c("Baseline", "Theoretical Max Habitat", "Max Flow", "No Harvest", "No Hatchery", "Max Hatchery", "Max Flow & Max Habitat"))) |>
  pivot_longer(3:5, names_to = "type", values_to = "value") |>
  mutate_if(is.numeric, pretty_num) |>
  pivot_wider(names_from = scenario, values_from = value) |> View()

# 5.1 #
# Age distribution of spawning adults
produce_categorical_return_age_pm(all_res) |>
  arrange(factor(scenario, levels = c("Baseline", "Theoretical Max Habitat", "Max Flow", "No Harvest", "No Hatchery", "Max Hatchery", "Max Flow & Max Habitat"))) |> View()

# 5.2 #
# shannon diversity index
produce_shannon_div_ind_size_pm(all_res) |>
  mutate_if(is.numeric, pretty_num) |>
  arrange(factor(scenario, levels = c("Baseline", "Theoretical Max Habitat", "Max Flow", "No Harvest", "No Hatchery", "Max Hatchery", "Max Flow & Max Habitat"))) |> View()

# 5.3 #
# size distribution & month of juveniles
produce_shannon_div_ind_size_and_timing_pm(all_res) |>
  mutate_if(is.numeric, pretty_num) |>
  arrange(factor(scenario, levels = c("Baseline", "Theoretical Max Habitat", "Max Flow", "No Harvest", "No Hatchery", "Max Hatchery", "Max Flow & Max Habitat"))) |> View()

# 5.4 #
produce_floodplain_over_inchannel_habitat(fall_baseline_results, r_to_r_baseline_params, "fall", "Baseline")
produce_floodplain_over_inchannel_habitat(fall_run_dy_results, r_to_r_dry_years_params, "fall", "Dry Year")
produce_floodplain_over_inchannel_habitat(fall_run_hh_results, r_to_r_habitat_and_hatchery_params, "fall", "Habitat and Hatchery")
produce_floodplain_over_inchannel_habitat(fall_run_ks_results, r_to_r_kitchen_sink_params, "fall", "Kitchen Sink")
produce_floodplain_over_inchannel_habitat(fall_run_pc_results, r_to_r_planned_and_current, "fall", "Planned Plus")

# 6 #
# marine derived nutrient
produce_marine_nutrient_pm(all_res) |>
  mutate_if(is.numeric, pretty_num) |>
  arrange(factor(scenario, levels = c("Baseline", "Theoretical Max Habitat", "Max Flow", "No Harvest", "No Hatchery", "Max Hatchery", "Max Flow & Max Habitat"))) |> View()

# 7 #
# # time to recovery
produce_time_to_recovery_pm(fall_baseline_results, selected_run = "fr")
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
produce_juvenile_wetted_acre_day_pm(r_to_r_dry_years_params, scenario = "Dry Years", selected_run = "fall"),
produce_juvenile_wetted_acre_day_pm(r_to_r_habitat_and_hatchery_params, scenario = "Habitat and Hatchery", selected_run = "fall"),
produce_juvenile_wetted_acre_day_pm(r_to_r_kitchen_sink_params, scenario = "Kitchen Sink", selected_run = "fall"),
produce_juvenile_wetted_acre_day_pm(r_to_r_planned_and_current, scenario = "Planned Plus", selected_run = "fall")) |>
  arrange(factor(scenario, levels = c("Baseline", "Dry Years", "Habitat and Hatchery", "Kitchen Sink"))) |>
  pivot_longer(3:5, names_to = "type", values_to = "value") |>
  mutate_if(is.numeric, pretty_num) |>
  pivot_wider(names_from = scenario, values_from = value) |> View()


# 8.2 #
# Wetted acre days of suitable spawning habitat
produce_spawning_wetted_acre_day_pm(model_params, scenario = "Baseline", selected_run = "fall")
bind_rows(produce_spawning_wetted_acre_day_pm(r_to_r_baseline_params, scenario = "Baseline", selected_run = "fall"),
          produce_spawning_wetted_acre_day_pm(r_to_r_dry_years_params, scenario = "Dry Years", selected_run = "fall"),
          produce_spawning_wetted_acre_day_pm(r_to_r_habitat_and_hatchery_params, scenario = "Habitat and Hatchery", selected_run = "fall"),
          produce_spawning_wetted_acre_day_pm(r_to_r_kitchen_sink_params, scenario = "Kitchen Sink", selected_run = "fall"),
          produce_spawning_wetted_acre_day_pm(r_to_r_planned_and_current, scenario = "Planned Plus", selected_run = "fall")) |>
  arrange(factor(scenario, levels = c("Baseline", "Dry Years", "Habitat and Hatchery", "Kitchen Sink"))) |>
  pivot_longer(3:5, names_to = "type", values_to = "value") |>
  mutate_if(is.numeric, pretty_num) |>
  pivot_wider(names_from = scenario, values_from = value) |> View()
# 8.3 #
# Spawning habitat decay rate (as a proxy for riverine condition)
# Note: should update to change between scenarios when Emanuel updates spawn_decay_multiplier
# STILL NOT WORKING - get values from EManuel and try and understand why it is so high here

bind_rows(total_habitat_decay(r_to_r_baseline_params, scenario = "Baseline", "fall"),
          total_habitat_decay(r_to_r_dry_years_params, scenario = "Dry Years", "fall"),
          total_habitat_decay(r_to_r_habitat_and_hatchery_params, scenario = "Habitat and Hatchery", "fall"),
          total_habitat_decay(r_to_r_kitchen_sink_params, scenario = "Kitchen Sink", "fall"),
          total_habitat_decay(r_to_r_planned_and_current, scenario = "Planned Plus", "fall"),) |>
  arrange(factor(scenario, levels = c("Baseline", "Dry Years", "Habitat and Hatchery", "Kitchen Sink"))) |> View()
# 9.1
# Wetted acre days - total days floodplain activation occurs
produce_wetted_acre_day_floodplain_activation_pm(r_to_r_baseline_params, scenario = "Baseline", selected_run = "fall")
bind_rows(produce_wetted_acre_day_floodplain_activation_pm(r_to_r_baseline_params, scenario = "Baseline", selected_run = "fall"),
          produce_wetted_acre_day_floodplain_activation_pm(r_to_r_dry_years_params, scenario = "Dry Years", selected_run = "fall"),
          produce_wetted_acre_day_floodplain_activation_pm(r_to_r_habitat_and_hatchery_params, scenario =  "Habitat and Hatchery", selected_run = "fall"),
          produce_wetted_acre_day_floodplain_activation_pm(r_to_r_kitchen_sink_params, scenario =  "Kitchen Sink", selected_run = "fall"),
            produce_wetted_acre_day_floodplain_activation_pm(r_to_r_planned_and_current, scenario =  "Planned Plus", selected_run = "fall")) |>
  arrange(factor(scenario, levels = c("Baseline", "Dry Years", "Habitat and Hatchery", "Kitchen Sink"))) |>
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
          produce_2yr_30d_floodplain_acres_pm(r_to_r_dry_years_params, scenario = "Dry Years", selected_run = "fall"),
          produce_2yr_30d_floodplain_acres_pm(r_to_r_habitat_and_hatchery_params, scenario = "Habitat and Hatchery", selected_run = "fall"),
          produce_2yr_30d_floodplain_acres_pm(r_to_r_kitchen_sink_params, scenario = "Kitchen Sink", selected_run = "fall"),
          produce_2yr_30d_floodplain_acres_pm(r_to_r_planned_and_current, scenario = "Planned Plus", selected_run = "fall")) |>
  arrange(factor(scenario, levels = c("Baseline", "Dry Years", "Habitat and Hatchery", "Kitchen Sink", "Planned Plus"))) |>
  pivot_longer(3, names_to = "type", values_to = "value") |>
  mutate_if(is.numeric, pretty_num) |>
  pivot_wider(names_from = scenario, values_from = value) |> View()
### Access and Economics ### ---------------------------------------------------

upper_sac_flow_eff <- DSMflow::flows_cfs$eff_sac |>
  filter(year(date) > 1979, year(date) < 2000) |>
  select(date, us_eff = `Upper Sacramento River`)

upper_sac_flow_biop <- DSMflow::flows_cfs$biop_itp_2018_2019 |>
  filter(year(date) > 1979, year(date) < 2000) |>
  select(date, us = `Upper Sacramento River`)

acre_feet <- left_join(upper_sac_flow_eff, upper_sac_flow_biop) |>
  mutate(us_af = us * 60.370,
         us_eff_af = us_eff * 60.370) |>
  group_by(year = year(date)) |>
  summarise(us = sum(us_af),
            us_eff = sum(us_eff_af)) |>
  mutate(acre_ft_change = us_eff - us)

mean_af_change <-  acre_feet |>
  pull(acre_ft_change) |> mean()
mean_af_change/1000

water_year_types <- waterYearType::water_year_indices |>
  filter(location == "Sacramento Valley", WY %in% c(1922:2003)) |>
  select(year = WY, index = Index, year_type = Yr_type)

new_year_types <- water_year_types |>
  mutate(year_type = ifelse(year_type %in% c("Dry", "Critical", "Below Normal"),
                            "Dry", "Wet")) |>
  select(year, year_type)

acre_feet |>
  left_join(new_year_types) |>
  mutate(acre_ft_change = ifelse(year_type == "Dry", acre_ft_change, 0)) |>
  pull(acre_ft_change) |> mean() / 1000

flow_change <- left_join(upper_sac_flow_eff, upper_sac_flow_biop) |>
  mutate(us_af = us,
         us_eff_af = us_eff) |>
  group_by(year = year(date)) |>
  summarise(us = sum(us_af),
            us_eff = sum(us_eff_af)) |>
  mutate(flow_change = us_eff - us) |> glimpse()

mean(flow_change$us)
mean(flow_change$us_eff)

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
# SEE new R script data-raw/terminal_hatchery_harvest.R for new harvest metrics

# Annual number of adults in rivers
# (above abundance numbers required to meet biological objectives)
produce_spawner_abundance_above_biological_objective_river_pm(all_res) |>
  arrange(factor(scenario, levels = c("Baseline", "Dry Year", "Habitat and Hatchery", "Kitchen Sink"))) |>
  pivot_longer(2:4, names_to = "type", values_to = "value") |>
  mutate_if(is.numeric, pretty_num) |>
  pivot_wider(names_from = scenario, values_from = value) |> View()

produce_spawner_abundance_above_biological_objective_ocean_pm(all_res) |>
  arrange(factor(scenario, levels = c("Baseline", "Dry Year", "Habitat and Hatchery", "Kitchen Sink"))) |>
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
          produce_weeks_flooded_pm(r_to_r_kitchen_sink_params, scenario = "Kitchen Sink", selected_run = "fall"),
          produce_weeks_flooded_pm(r_to_r_dry_years_params, scenario = "Dry Years", selected_run = "fall"),
          produce_weeks_flooded_pm(r_to_r_habitat_and_hatchery_params, scenario = "Habitat and Hatchery", selected_run = "fall"),
          produce_weeks_flooded_pm(r_to_r_planned_and_current, scenario = "Planned Plus", selected_run = "fall"),
          ) |>
  arrange(factor(scenario, levels = c("Baseline", "Dry Years", "Habitat and Hatchery", "Kitchen Sink", "Planned Plus"))) |>
  pivot_longer(3:5, names_to = "type", values_to = "value") |>
  pivot_wider(names_from = scenario, values_from = value) |> View()
# 16.1 #
all_inputs |>
  filter(performance_metric == "16.1 Delta Outflow (cfs)" ) |> glimpse()
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
  filter(comid == 2851441) |> glimpse()

nat <- natural_flows |>
  filter(year > 1979, year < 2000) |>
  rename(flow_cfs = value) |>
  mutate(type = "natural flow") |>
  select(year, month, type, flow_cfs) |> glimpse()

model_flows <- left_join(upper_sac_flow_eff, upper_sac_flow_biop) |>
  rename("eff flow" = us_eff,
        "baseline" = us) |>
  pivot_longer(cols = 2:3, names_to = "type", values_to = "flow_cfs") |>
  mutate(month = month(date), year = year(date)) |>
  select(-date) |>
  glimpse()

data <- bind_rows(model_flows, nat) |>
  group_by(year, type) |>
  summarise(total_flows = sum(flow_cfs, na.rm = TRUE)) |>
  ungroup() |>
  left_join(new_year_types) |>
  pivot_wider(names_from = type, values_from = total_flows) |>
  mutate(baseline_percent_of_nat_flows = baseline / `natural flow`,
         full_eff_percent_of_nat_flows = `eff flow` / `natural flow`,
         dry_eff_percent_of_nat_flows = ifelse(year_type == "Dry",
                            `eff flow` / `natural flow`,
                           baseline / `natural flow`)) |> glimpse()

ifelse(data$baseline_percent_of_nat_flows > 1, 1, data$baseline_percent_of_nat_flows) |> mean()
ifelse(data$full_eff_percent_of_nat_flows > 1, 1, data$full_eff_percent_of_nat_flows) |> mean()
ifelse(data$dry_eff_percent_of_nat_flows > 1, 1, data$dry_eff_percent_of_nat_flows) |> mean()



# 17 #
# Flood frequency and stage for each watershed
produce_flood_frequency_and_stage_pm(r_to_r_baseline_params, scenario = "Baseline", selected_run = "fall")
bind_rows(produce_flood_frequency_and_stage_pm(r_to_r_baseline_params, scenario = "Baseline", selected_run = "fall"),
          produce_flood_frequency_and_stage_pm(r_to_r_kitchen_sink_params, scenario = "Kitchen Sink", selected_run = "fall"),
          produce_flood_frequency_and_stage_pm(r_to_r_dry_years_params, scenario = "Dry Years", selected_run = "fall"),
          produce_flood_frequency_and_stage_pm(r_to_r_habitat_and_hatchery_params, scenario = "Habitat and Hatchery", selected_run = "fall"),
          produce_flood_frequency_and_stage_pm(r_to_r_planned_and_current, scenario = "Planned Plus", selected_run = "fall"),
) |>
  arrange(factor(scenario, levels = c("Baseline", "Dry Years", "Habitat and Hatchery", "Kitchen Sink", "Planned Plus"))) |>
  pivot_longer(3:5, names_to = "type", values_to = "value") |>
  pivot_wider(names_from = scenario, values_from = value) |> View()

# 18 #
# Hydropower generation ability
# Only works for ones we have calsim runs for
all_inputs |>
  filter(performance_metric == "18 Hydropower Generation: Difference in Potential Power Produnction From Baseline") |> glimpse()
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
# assume flow change means loss of flow at reservoir
flow_change <- left_join(upper_sac_flow_eff, upper_sac_flow_biop) |>
  mutate(us_af = us * 60.370,
         us_eff_af = us_eff * 60.370,
         volume_af =  ifelse(us_eff_af - us_af > 0, us_eff_af - us_af, 0)) |>
  select(date, volume_af) |> glimpse()

power_mod <- lm(lost_gen ~ volume_af, data = lost_gen)
# summary(power_mod)
power_gen_potential <- predict(power_mod, flow_change)

flow_change$power_gen_potential <- ifelse(power_gen_potential < 0, 0, power_gen_potential)
sum(flow_change$power_gen_potential)

# DRY YEAR ONE
flow_change_dy <- left_join(upper_sac_flow_eff, upper_sac_flow_biop) |>
  mutate(year = year(date)) |>
  left_join(new_year_types) |>
  mutate(us_af = us * 60.370,
         us_eff_af = us_eff * 60.370,
         volume_af =  ifelse(us_eff_af - us_af > 0, us_eff_af - us_af, 0)) |>
  mutate(volume_af = ifelse(year_type == "Dry", volume_af, 0)) |>
  select(date, volume_af) |> glimpse()

power_gen_potential <- predict(power_mod, flow_change_dy)

flow_change_dy$power_gen_potential <- ifelse(power_gen_potential < 0, 0, power_gen_potential)
sum(flow_change_dy$power_gen_potential)
