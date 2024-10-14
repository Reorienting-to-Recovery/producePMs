doubling_goal <- sum(3300, 9300, 2200, 38000, 22000, 18000, 170000, 66000, 450,
                     160000, 258700, 720, 4200, 1500, 1500, 800)

source("data-raw/shiny-materials/process_balanced_model_results.R")
all_res_balanced <- all_res
#source("data-raw/shiny-materials/process_blended_model_results.R")
#all_res_blended <- all_res
all_res_blended <- read_csv("data-raw/shiny-materials/fall_blended_results_april_2024.csv")

scenario_six_colors <- c("#02401B", "#9A8822", "#798E87", "#5B1A18","#972D15", "#DC863B", "#AA9486")
scenario_eight_colors <- c("#02401B", "#9A8822", "#798E87", "#5B1A18","#972D15", "darkgray", "darkgray", "darkgray")
year_lookup <- tibble(year = 1:21,
                      actual_year = 1980:2000)

all_res_balanced |>
  filter(scenario != "Elephant") |> # following planning team meeting, not presenting Elephant in plots with others
  bind_rows(all_res_blended |>
              filter(scenario != "Baseline")) |>
  filter(performance_metric == "1 All Spawners",
         !scenario %in% c("Planned Plus"),
         !location %in% c("Stoney Creek", "Thomes Creek")) |>
  group_by(year, scenario, run) |>
  summarize(value = sum(value, na.rm = TRUE)) |>
  left_join(year_lookup) |>
  mutate(date = as.Date(paste0(actual_year, "-12-31"))) |>
  ggplot() +
  geom_line(aes(x = date, y = value, color = scenario),linewidth = 1, alpha = 1) +
  scale_color_manual(values = c("Baseline" = "#02401B",
                                "Elephant" = "#9A8822",
                                #"Elephant Plus" = "#972D15",
                                "Platypus" = "#DC863B",
                                "Tortoise" = "#5B1A18",
                                "Dry Year" = "darkgrey",
                                "Kitchen Sink" = "darkgrey",
                                "Habitat and Hatchery" = "darkgrey")) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Total Spawner Abundance over Time",
       y = "Spawner Abundance",
       x = "Year",
       color = "Scenario") +
  geom_hline(yintercept = doubling_goal, linetype = "dashed") +
  theme_minimal() +
  theme(
    plot.caption = element_text(hjust = 0, face = "italic"),# move caption to the left
    legend.position = "bottom",
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 20),
    plot.title = element_text(size = 20)
  )
ggsave("data-raw/figures/doubling_goal_balanced_and_blended.png")
