# script for plots here
# TODO functionalize
colors_small <-  c("#9A8822", "#899DA4", "#C93312", "#DC863B" # royal 1 (- 3)
)
# Plot for spawners ------------------------------------------------------------
plot <- model_results |>
  filter(performance_metric == "All Spawners",
         scenario %in% c("Baseline", "Theoretical Max Habitat")) |>
  group_by(year, scenario) |>
  summarize(total_spawners = sum(value, na.rm = T)) |>
  ggplot(aes(year, total_spawners, color = scenario)) +
  geom_line() +
  labs(title = "Total Spawners Summed Across Watersheds Over 20 Year Simulation",
       y = "Total Spawners",
       x = "Simulation Year") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA), labels = scales::comma) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, NA), breaks = 1:20) +
  scale_color_manual(values = colors_small) +
  theme_minimal() +
  theme(text = element_text(size = 12),
        legend.title = element_blank(),
        legend.position = c(.85, .9))

plotly::ggplotly(plot)

# Plots for SDM presentation ---------------------------------------------------
plot_watershed_specific <- model_results |>
  filter(location %in% c("Calaveras River",
                         "Stony Creek"),
         performance_metric == "All Spawners",
         scenario %in% c("Baseline", "Theoretical Max Habitat")) |>
  ggplot(aes(year, value, linetype = scenario, color = location)) +
  geom_line(color = "#9A8822") +
  labs(title = "Total Spawners Over 20 Year Simulation",
       y = "Total Spawners",
       x = "Simulation Year") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA), labels = scales::comma) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, NA), breaks = 1:20) +
  scale_color_manual(values = colors_small) +
  theme_minimal() +
  theme(text = element_text(size = 12),
        legend.title = element_blank(),
        legend.position = "bottom") +
  facet_wrap(~location, dir = "v")
plot_watershed_specific

# Baseline total spawners
model_results |>
  filter(performance_metric == "All Spawners",
         scenario %in% c("Baseline")) |>
  group_by(year, scenario) |>
  summarize(total_spawners = sum(value, na.rm = T)) |>
  ggplot(aes(year, total_spawners, color = scenario)) +
  geom_line() +
  labs(title = "Fall Run Total Spawners",
       y = "Total Spawners",
       x = "Simulation Year") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA), labels = scales::comma) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, NA), breaks = 1:20) +
  scale_color_manual(values = colors_small) +
  theme_minimal() +
  theme(text = element_text(size = 12),
        legend.title = element_blank(),
        legend.position = "none")



