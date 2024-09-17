# Terminal Hatchery Analysis
total_released <- fallRunDSM::fall_hatchery_release |> rowSums() |> sum()
total_released

# assume only 1% of net pen fish are harvested throughout lifetime
# (consistent with/very low end of CFM_CWT_report studies, year 3 captures much higher)
# https://www.calfish.org/Portals/2/Programs/CentralValley/CFM/docs/2019_CFM_CWT_Report.pdfv
harvestable_ocean_terminal_hatcheries <- total_released * .01
harvestable_ocean_terminal_hatcheries

# how much harvest is in river vs ocean in the model
# Total harvest
total_harvest <- fallRunDSM::r2r_adult_harvest_rate |> sum()

# 17 tribs allow in river harvest each of these allows it at .8 percent
in_river_harvest_percentace <- (.08 * 17) / total_harvest


# harvest scenarios
# need to source file before running
source("data-raw/shiny-materials/process_balanced_model_results.R")
# BASELINE
baseline_harvest <- baseline_model_results$harvested_adults
baseline_harvest_river <- mean(baseline_harvest$total_harvest * in_river_harvest_percentace)
baseline_harvest_ocean <- mean(baseline_harvest$total_harvest * (1 - in_river_harvest_percentace))
number_years_dont_meet <- (baseline_harvest$total_harvest * in_river_harvest_percentace) + (baseline_harvest$total_harvest * (1 - in_river_harvest_percentace))
sum(number_years_dont_meet[6:20] > 250000)

harvest_pm <- function() {
  # Terminal Hatchery Analysis
  total_released <- fallRunDSM::fall_hatchery_release |> rowSums() |> sum()
  total_released
  # assume only 1% of net pen fish are harvested throughout lifetime
  # (consistent with/very low end of CFM_CWT_report studies, year 3 captures much higher)
  # https://www.calfish.org/Portals/2/Programs/CentralValley/CFM/docs/2019_CFM_CWT_Report.pdfv
  harvestable_ocean_terminal_hatcheries <- total_released * .01
  harvestable_ocean_terminal_hatcheries
  # how much harvest is in river vs ocean in the model
  # Total harvest
  total_harvest_rate <- fallRunDSM::r2r_adult_harvest_rate |> sum()

  # 17 tribs allow in river harvest each of these allows it at .8 percent
  in_river_harvest_percentace <- (.08 * 17) / total_harvest_rate
  model_results <- baseline_model_results
  selected_scenario <- "Baseline"
  selected_run <- "fall"
  model_parameters <- r_to_r_baseline_params

  ocean_harvest <- model_results$harvested_adults |>
    rowwise() |>
    mutate(scenario = selected_scenario,
           run = selected_run,
           location = "Ocean",
           performance_metric = "12.2: Total ocean harvest",
           harvest = total_harvest * (1 - in_river_harvest_percentace),
           value = ifelse(model_parameters$terminal_hatchery_logic,
                          harvest + harvestable_ocean_terminal_hatcheries, harvest)) |>
    select(-hatchery_harvest, -natural_harvest, -total_harvest, -harvest) |> glimpse()

  river_harvest <- model_results$harvested_adults |>
    rowwise() |>
    mutate(scenario = selected_scenario,
           run = selected_run,
           location = "River",
           performance_metric = "12.2: Total river harvest",
           value = total_harvest * in_river_harvest_percentace) |>
    select(-hatchery_harvest, -natural_harvest, -total_harvest) |> glimpse()

  total_harvest_df <- bind_rows(ocean_harvest, river_harvest)

  return(total_harvest_df)
}
# PLATYPUS
platypus_harvest <- platypus_results$harvested_adults
# All natural harvest is in river so that would all be sport harvest
# All terminal hatchery harvest is in ocean
platypus_harvest_river <- mean(platypus_harvest$natural_harvest)
platypus_harvest_ocean <- mean(harvestable_ocean_terminal_hatcheries + platypus_harvest$hatchery_harvest)
platypus_harvest_river
platypus_harvest_ocean

number_years_dont_meet <- (platypus_harvest$natural_harvest + (harvestable_ocean_terminal_hatcheries + platypus_harvest$hatchery_harvest))
sum(number_years_dont_meet[6:20] > 250000)

# ELEPHANT
elephant_harvest <- elephant_results$harvested_adults
# All natural harvest is in river so that would all be sport harvest
# All terminal hatchery harvest is in ocean
elephant_harvest_river <- mean(elephant_harvest$natural_harvest)
elephant_harvest_ocean <- mean(harvestable_ocean_terminal_hatcheries + elephant_harvest$hatchery_harvest)
elephant_harvest_river
elephant_harvest_ocean

number_years_dont_meet <- (elephant_harvest$natural_harvest +
                             (harvestable_ocean_terminal_hatcheries + elephant_harvest$hatchery_harvest))
sum(number_years_dont_meet[6:20] > 250000)

# TORTOISE
tortoise_harvest <- tortoise_results$harvested_adults
# All natural harvest is in river so that would all be sport harvest
# All terminal hatchery harvest is in ocean
tortoise_harvest_river <- mean(tortoise_harvest$natural_harvest)
tortoise_harvest_ocean <- mean(harvestable_ocean_terminal_hatcheries + tortoise_harvest$hatchery_harvest)
tortoise_harvest_river
tortoise_harvest_ocean

number_years_dont_meet <- (tortoise_harvest$natural_harvest + (harvestable_ocean_terminal_hatcheries + tortoise_harvest$hatchery_harvest))
sum(number_years_dont_meet[6:20] > 250000)

# TESTS
colSums(platypus_results$spawners * .08, na.rm = TRUE)
colSums(elephant_results$spawners * .08, na.rm = TRUE)
colSums(tortoise_results$spawners * .08, na.rm = TRUE)

river_harvest <- tibble(years = 1:20,
                        Baseline = (baseline_harvest$total_harvest * in_river_harvest_percentace),
                        "Platypus" = (platypus_harvest$natural_harvest),
                        "Elephant" = (elephant_results$total_harvest * in_river_harvest_percentace),
                        "Tortoise" = (tortoise_results$total_harvest * in_river_harvest_percentace)) |>
  pivot_longer(Baseline:`Tortoise`, names_to = "Scenario", values_to = "river_harvest") |> glimpse()

ocean_harvest <- tibble(years = 1:20,
                        Baseline = (baseline_harvest$total_harvest * (1 - in_river_harvest_percentace)),
                        "Platypus" = (harvestable_ocean_terminal_hatcheries + platypus_harvest$hatchery_harvest),
                        "Elephant" = (harvestable_ocean_terminal_hatcheries + (elephant_harvest$total_harvest * (1 - in_river_harvest_percentace))),
                        "Tortoise" = (harvestable_ocean_terminal_hatcheries + (tortoise_harvest$total_harvest * (1 - in_river_harvest_percentace)))) |>
  pivot_longer(Baseline:`Tortoise`, names_to = "Scenario", values_to = "ocean_harvest") |> glimpse()
scenario_six_colors <- c("#02401B", "#9A8822", "#798E87", "#5B1A18","#972D15", "#DC863B", "#AA9486")

river_harvest |>
  ggplot(aes(x = years, y = river_harvest, color = Scenario)) +
  geom_line() +
  scale_color_manual(values = scenario_six_colors) +
  # geom_line(data = flow_spawn_plot_data |> filter(scenario == "Baseline"), aes(x = date, y = value), color = "black", linewidth = .5, alpha = 1) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Total River Harvest Over Time",
       y = "Harvest Totals",
       x = "Year",
       color = "Balanced Scenario",
       #  caption = "Note: These numbers only reflect Upper Sacramento River Fall Run Chinook Spawners. Baseline and No Hatchery perform very simmilarly in the Upper Sacramento River."
  ) +
  theme_minimal() +
  theme(
    plot.caption = element_text(hjust = 0, face = "italic"),# move caption to the left
    legend.position = "bottom",
    # legend.position = "none",
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 20),
    plot.title = element_text(size = 20)
  ) +
  geom_hline(yintercept = 50000, linetype = "dashed")
ggsave("data-raw/figures/ocean_harvest_plot_balanced.png")

ocean_harvest |>
  ggplot(aes(x = years, y = ocean_harvest, color = Scenario)) +
  geom_line() +
  scale_color_manual(values = scenario_six_colors) +
  # geom_line(data = flow_spawn_plot_data |> filter(scenario == "Baseline"), aes(x = date, y = value), color = "black", linewidth = .5, alpha = 1) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Total Ocean Harvest Over Time",
       y = "Harvest Totals",
       x = "Year",
       color = "Balanced Scenario",
       #  caption = "Note: These numbers only reflect Upper Sacramento River Fall Run Chinook Spawners. Baseline and No Hatchery perform very simmilarly in the Upper Sacramento River."
  ) +
  theme_minimal() +
  theme(
    plot.caption = element_text(hjust = 0, face = "italic"),# move caption to the left
    legend.position = "bottom",
    # legend.position = "none",
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 20),
    plot.title = element_text(size = 20)
  ) +
  geom_hline(yintercept = 200000, linetype = "dashed")
ggsave("data-raw/figures/river_harvest_plot_balanced.png")

