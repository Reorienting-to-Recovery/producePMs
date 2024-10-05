doubling_goal <- sum(3300, 9300, 2200, 38000, 22000, 18000, 170000, 66000, 450,
                     160000, 258700, 720, 4200, 1500, 1500, 800)

source("data-raw/shiny-materials/process_balanced_model_results.R")
all_res_balanced <- all_res
source("data-raw/shiny-materials/process_blended_model_results.R")
all_res_blended <- all_res

all_res_balanced |>
  bind_rows(all_res_blended |>
              filter(scenario != "Baseline")) |>
  filter(performance_metric == "1 All Spawners",
         !scenario %in% c("Planned Plus", "Elephant Plus"),
         !location %in% c("Stoney Creek", "Thomes Creek")) |>
  group_by(year, scenario, run) |>
  summarize(value = sum(value, na.rm = TRUE)) |>
  left_join(year_lookup) |>
  mutate(date = as.Date(paste0(actual_year, "-12-31"))) |>
  ggplot() +
  geom_line(aes(x = date, y = value, color = scenario),linewidth = .5, alpha = 1) +
  scale_color_manual(values = scenario_six_colors) +
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
