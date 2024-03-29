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
source("data-raw/shiny-materials/process_blended_model_results.R")
# BASELINE
baseline_harvest <- baseline_model_results$harvested_adults
baseline_harvest_river <- mean(baseline_harvest$total_harvest * in_river_harvest_percentace)
baseline_harvest_ocean <- mean(baseline_harvest$total_harvest * (1 - in_river_harvest_percentace))
number_years_dont_meet <- (baseline_harvest$total_harvest * in_river_harvest_percentace) + (baseline_harvest$total_harvest * (1 - in_river_harvest_percentace))
sum(number_years_dont_meet[6:20] > 250000)
# KITCHEN SINK
kitchen_sink_harvest <- kitchen_sink_results$harvested_adults
# All natural harvest is in river so that would all be sport harvest
# All terminal hatchery harvest is in ocean
ks_harvest_river <- mean(kitchen_sink_harvest$natural_harvest)
ks_harvest_ocean <- mean(harvestable_ocean_terminal_hatcheries + kitchen_sink_harvest$hatchery_harvest)
ks_harvest_river
ks_harvest_ocean

number_years_dont_meet <- (kitchen_sink_harvest$natural_harvest + (harvestable_ocean_terminal_hatcheries + kitchen_sink_harvest$hatchery_harvest))
sum(number_years_dont_meet[6:20] > 250000)

# TESTS
colSums(kitchen_sink_results$spawners * .08, na.rm = TRUE)

# Dry Years
dry_year_harvest <- dry_year_results$harvested_adults
# All natural harvest is in river so that would all be sport harvest
# All terminal hatchery harvest is in ocean
dry_year_harvest_river <- mean(dry_year_harvest$total_harvest * in_river_harvest_percentace)
dry_year_harvest_ocean <- mean(harvestable_ocean_terminal_hatcheries + (dry_year_harvest$total_harvest * (1 - in_river_harvest_percentace)))
dry_year_harvest_river
dry_year_harvest_ocean

number_years_dont_meet <- (dry_year_harvest$total_harvest * in_river_harvest_percentace) + (harvestable_ocean_terminal_hatcheries + (dry_year_harvest$total_harvest * (1 - in_river_harvest_percentace)))
dry_year_harvest$total_harvest * in_river_harvest_percentace
sum(number_years_dont_meet[6:20] > 250000)
# Dry Years
hab_hatch_harvest <- hab_and_hatchery_results$harvested_adults
hab_and_hatchery_results$spawners

# All natural harvest is in river so that would all be sport harvest
# All terminal hatchery harvest is in ocean
hh_harvest_river <- mean(hab_hatch_harvest$total_harvest * in_river_harvest_percentace)
hh_harvest_ocean <- mean(harvestable_ocean_terminal_hatcheries + (hab_hatch_harvest$total_harvest * (1 - in_river_harvest_percentace)))
hh_harvest_river
hh_harvest_ocean

number_years_dont_meet <- (hab_hatch_harvest$total_harvest * in_river_harvest_percentace) + (harvestable_ocean_terminal_hatcheries + (hab_hatch_harvest$total_harvest * (1 - in_river_harvest_percentace)))
sum(number_years_dont_meet[6:20] > 250000)

river_harvest <- tibble(years = 1:20,
                        Baseline = (baseline_harvest$total_harvest * in_river_harvest_percentace),
                        "Kitchen Sink" = (kitchen_sink_harvest$natural_harvest),
                        "Dry Year" = (dry_year_harvest$total_harvest * in_river_harvest_percentace),
                        "Habitat and Hatchery" = (hab_hatch_harvest$total_harvest * in_river_harvest_percentace)) |>
  pivot_longer(Baseline:`Habitat and Hatchery`, names_to = "Scenario", values_to = "river_harvest") |> glimpse()

ocean_harvest <- tibble(years = 1:20,
                        Baseline = (baseline_harvest$total_harvest * (1 - in_river_harvest_percentace)),
                        "Kitchen Sink" = (harvestable_ocean_terminal_hatcheries + kitchen_sink_harvest$hatchery_harvest),
                        "Dry Year" = (harvestable_ocean_terminal_hatcheries + (dry_year_harvest$total_harvest * (1 - in_river_harvest_percentace))),
                        "Habitat and Hatchery" = (harvestable_ocean_terminal_hatcheries + (hab_hatch_harvest$total_harvest * (1 - in_river_harvest_percentace)))) |>
  pivot_longer(Baseline:`Habitat and Hatchery`, names_to = "Scenario", values_to = "ocean_harvest") |> glimpse()
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
       color = "Blended Scenario",
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
ggsave("data-raw/figures/ocean_harvest_plot.png")

ocean_harvest |>
  ggplot(aes(x = years, y = ocean_harvest, color = Scenario)) +
  geom_line() +
  scale_color_manual(values = scenario_six_colors) +
  # geom_line(data = flow_spawn_plot_data |> filter(scenario == "Baseline"), aes(x = date, y = value), color = "black", linewidth = .5, alpha = 1) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Total Ocean Harvest Over Time",
       y = "Harvest Totals",
       x = "Year",
       color = "Blended Scenario",
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
ggsave("data-raw/figures/river_harvest_plot.png")

