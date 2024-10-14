library(tidyverse)
library(producePMs)
source("data-raw/shiny-materials/process_balanced_model_results.R")
# remotes::install_github("Reorienting-to-Recovery/DSMflow", force = TRUE)
library(DSMflow)
library(fallRunDSM)
library(waterYearType)
library(ggnewscale)

colors_full <-  c("#9A8822", "#F5CDB4",
                  "#899DA4", "#C93312", "#DC863B", # royal 1 (- 3)
                  "#E6A0C4", "#C6CDF7", "#D8A499", "#7294D4", #Grand Budapest 2
                  "#F8AFA8", "#FDDDA0", "#74A089", #Royal 2
                  "#F1BB7B", "#FD6467", "#5B1A18", # Grand Budapest 1 (-4)
                  "#9986A5", "#02401B", "#A2A475", # Cavalcanti 1
                  "#D8B70A", "#EAD3BF", "#AA9486", "#B6854D", "#798E87", # Isle of dogs 2 altered slightly
                  "gray", "lightblue4"
)

colors_small <-  c("#9A8822", "#F5CDB4", "#F8AFA8", "#FDDDA0", "#74A089", #Royal 2
                   "#899DA4", "#C93312", "#DC863B" # royal 1 (- 3)
)
scenario_six_colors <- c("#02401B", "#9A8822", "#798E87", "#5B1A18","#972D15", "#DC863B", "#AA9486")

muted = c("Critical" = "#972D15",
          "Dry" = "#D8B70A",
          "Below Normal" = "#A2A475",
          "Above Normal" ="#81A88D",
          "Wet" ="#02401B",
          "Run of River" = "#899DA4",
          "Biop (Baseline)" = "#7294D4",
          "LTO_12a" = "#08519C",
          "FF" = "#9ECAE1")

blues <- c( "Run of River" = "#899DA4", "Biop (Baseline)" = "#7294D4")
# GENERAL PLOTS
# - Figure that shows what has happened since the last workshop (conceptual model with updates)\

# CRR -------------------------------------------------------------------------
# - CRR - show some more hatchery dominated tribs, show a few less dominated tribs
#
# -   Upper Sacramento River
#
# -   American River (?)
#
# -   Stanislaus River
#
# -   Tuolumne River
plot_data <- all_res |>
  filter(performance_metric == "2.1 CRR: Total Adult to Returning Natural Adult",
         location %in% c("Upper Sacramento River", "Stanislaus River", "Tuolumne River", "American River"),
         scenario %in% c("Baseline", "Platypus", "Tortoise")) |> #, "Elephant", "Elephant Plus"))  |>
  glimpse()

plot_data |>
  ggplot(aes(x = year, y = value, color = location)) +
  geom_line(linewidth = 1) +
  theme_minimal() +
  scale_color_manual(values = colors_full) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  facet_wrap(~scenario, nrow = 3) +
  labs(x = "Simulation Year",
       y = "Cohort Replacement Rate",
       title = "Total Adult to Returning Natural Adult over Simulation Period") +
  theme(
    plot.caption = element_text(hjust = 0, face = "italic"),# move caption to the left
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 20),
    plot.title = element_text(size = 20),
    legend.position = "bottom"
  )

ggsave("data-raw/figures/crr_plot_balanced.png")

# GROWTH RATE -------------------------------------------------------------------------
# - CRR - show some more hatchery dominated tribs, show a few less dominated tribs
#
# -   Upper Sacramento River
#
# -   American River (?)
#
# -   Stanislaus River
#
# -   Tuolumne River
plot_data <- all_res |>
  filter(performance_metric == "2.2 Growth Rate Spawners",
         location %in% c("Upper Sacramento River", "Stanislaus River", "Tuolumne River", "American River"),
         scenario %in% c("Baseline", "Platypus", "Tortoise", "Elephant", "Elephant Plus"))  |>
  glimpse()

plot_data |>
  ggplot(aes(x = year, y = value, color = location)) +
  geom_line() +
  theme_minimal() +
  scale_color_manual(values = colors_full) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  facet_wrap(~scenario, scales = "free") +
  labs(x = "Simulation Year",
       y = "Growth Rate",
       title = "Growth Rate over Simulation Period") +
  theme(
    plot.caption = element_text(hjust = 0, face = "italic"),# move caption to the left
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 20),
    plot.title = element_text(size = 20),
    legend.position = "bottom"
  )

ggsave("data-raw/figures/growth_rate_plot_balanced.png")
### All Spawn ------------------------------------------------------------------
# - Spawners - overlay flows & All spawners number(Upper Sac)
water_yt_lookup <- waterYearType::water_year_indices |>
  filter(WY > 1977, WY < 2001, location == "Sacramento Valley") |>
  mutate(water_year = WY,
         year_type = Yr_type) |> glimpse()

flow_data_biop <- DSMflow::flows_cfs$biop_itp_2018_2019 |>
  pivot_longer(cols = -date, names_to = "watershed", values_to = "flow_cfs") |>
  filter(date >= lubridate::as_date("1979-01-01"),
         watershed %in% c("Upper Sacramento River", "San Joaquin River")) |>
  mutate(water_year = ifelse(month(date) %in% 10:12, year(date) + 1, year(date)),
         Hydrology = "Biop (Baseline)") |>
  filter(water_year < 2001) |>
  left_join(water_yt_lookup) |>
  glimpse()

flow_data_eff <- DSMflow::flows_cfs$eff_sac |>
  pivot_longer(cols = -date, names_to = "watershed", values_to = "flow_cfs") |>
  filter(date >= lubridate::as_date("1979-01-01"),
         watershed %in% c("Upper Sacramento River", "San Joaquin River")) |>
  mutate(water_year = ifelse(month(date) %in% 10:12, year(date) + 1, year(date)),
         Hydrology = "EFF") |>
  filter(water_year < 2001) |>
  left_join(water_yt_lookup) |> glimpse()

flow_data_lto_12a <- DSMflow::flows_cfs$LTO_12a |>
  pivot_longer(cols = -date, names_to = "watershed", values_to = "flow_cfs") |>
  filter(date >= lubridate::as_date("1979-01-01"),
         watershed %in% c("Upper Sacramento River", "San Joaquin River")) |>
  mutate(water_year = ifelse(month(date) %in% 10:12, year(date) + 1, year(date)),
         Hydrology = "LTO_12a") |>
  filter(water_year < 2001) |>
  left_join(water_yt_lookup) |> glimpse()

flow_data <-bind_rows(flow_data_biop, flow_data_eff, flow_data_lto_12a)

year_lookup <- tibble(year = 1:21,
                      actual_year = 1980:2000)
flow_spawn_plot_data <- all_res |>
  filter(performance_metric == "1 All Spawners",
         scenario %in% c("Baseline", "Platypus", "Tortoise", "Elephant", "Elephant Plus")) |>
  group_by(year, scenario, run) |>
  summarize(value = sum(value, na.rm = TRUE)) |>
  left_join(year_lookup) |>
  mutate(date = as.Date(paste0(actual_year, "-12-31"))) |> glimpse()

# Spawn plot with 20 years of data
ggplot() +
  geom_area(data = flow_data, aes(x = date, y = flow_cfs, fill = Hydrology), alpha = .5, position = "identity") +
  geom_col(data = flow_data, aes(x = date, y = -4000, fill = year_type), alpha = 1, width = 31) +
  scale_fill_manual(values = muted,
                    # limits = c("Run of River", "Biop (Baseline)")
                   #  limits = c('Critical', 'Dry', 'Below Normal', 'Above Normal', 'Wet')
                    ) +
  geom_line(data = flow_data, aes(x = date, y = flow_cfs), color = "black", linewidth = .1) +
  geom_line(data = flow_spawn_plot_data, aes(x = date, y = value, color = scenario),linewidth = .5, alpha = 1) +
  scale_color_manual(values = c(rep("gray", 7))) +
  geom_line(data = flow_spawn_plot_data |> filter(scenario == "Baseline"), aes(x = date, y = value), color = "black", linewidth = .5, alpha = 1) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Historical Water Year Types, and Total Spawner Abundance",
       y = "Spawner Abundance",
       x = "Year",
       linetype = "Blended Scenario",
       fill = "Hydrology",
       caption = "Note: These numbers only reflect Upper Sacramento River and San Joaquin River Fall Run Chinook Spawners."
       ) +
  theme_minimal() +
  theme(
    # plot.caption = element_text(hjust = 0, face = "italic"),# move caption to the left
    # legend.position = c(0.85, 0.7),
    legend.position = "none",
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 20),
    plot.title = element_text(size = 20)
  )


# JUST HYDROLOGY
ggplot() +
  geom_area(data = flow_data, aes(x = date, y = flow_cfs, color = Hydrology), alpha = .3,
            position = "identity", fill = "lightgray") +
  # geom_col(data = flow_data, aes(x = date, y = -4000, fill = year_type), alpha = 1, width = 31) +
  scale_color_manual(values = muted,
                    limits = c("EFF", "Biop (Baseline)", "LTO_12a")
                    ) +
  # geom_line(data = flow_data, aes(x = date, y = flow_cfs), color = "black", linewidth = .1) +
  # geom_line(data = flow_spawn_plot_data, aes(x = date, y = value, color = scenario),linewidth = .5, alpha = 1) +
  # scale_color_manual(values = c(rep("gray", 7))) +
  # geom_line(data = flow_spawn_plot_data |> filter(scenario == "Baseline"), aes(x = date, y = value), color = "black", linewidth = .5, alpha = 1) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Sacramento and San Joaquin River Flows",
       subtitle = "Baseline, FF, and LTO 12a",
       y = "Flow CFS",
       x = "Year",
       # linetype = "Bookend Scenario",
       fill = "Hydrology") +
  facet_wrap(~watershed, nrow = 2) +
  theme_minimal() +
  theme(
    plot.caption = element_text(hjust = 0, face = "italic"),# move caption to the left
    # legend.position = c(0.85, 0.7),
    # legend.position = "none",
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 20),
    plot.title = element_text(size = 20)
  )
ggsave("data-raw/figures/hydrology_balanced.png")


# doubling goal plot
doubling_goal <- sum(3300, 9300, 2200, 38000, 22000, 18000, 170000, 66000, 450,
                     160000, 258700, 720, 4200, 1500, 1500, 800)
all_res |>
  filter(performance_metric == "1 All Spawners",
         !scenario %in% c("Elephant"), # do not include Elephant for SDM working group call
         !scenario %in% c("Planned Plus", "Dry Year with Projects"),
         !location %in% c("Stoney Creek", "Thomes Creek")) |>
  group_by(year, scenario, run) |>
  summarize(value = sum(value, na.rm = TRUE)) |>
  left_join(year_lookup) |>
  mutate(date = as.Date(paste0(actual_year, "-12-31"))) |>
  ggplot() +
  geom_line(aes(x = date, y = value, color = scenario),linewidth = 1, alpha = 1) +
  scale_color_manual(values = scenario_six_colors) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Total Spawner Abundance over Time",
       y = "Spawner Abundance",
       x = "Year",
       color = "Blended Scenario" ) +
  geom_hline(yintercept = doubling_goal, linetype = "dashed") +
  theme_minimal() +
  theme(
    plot.caption = element_text(hjust = 0, face = "italic"),# move caption to the left
    legend.position = "bottom",
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 20),
    plot.title = element_text(size = 20)
  )
ggsave("data-raw/figures/doubling_goal_balanced.png")

# plot elephant by itself
all_res |>
  filter(performance_metric == "1 All Spawners",
         scenario %in% c("Baseline", "Elephant"),
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
       color = "Blended Scenario" ) +
  geom_hline(yintercept = doubling_goal, linetype = "dashed") +
  theme_minimal() +
  theme(
    plot.caption = element_text(hjust = 0, face = "italic"),# move caption to the left
    legend.position = "bottom",
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 20),
    plot.title = element_text(size = 20)
  )

ggsave("data-raw/figures/doubling_goal_balanced_elephant.png")

# flow_spawn_plot_data <- all_res |>
#   filter(performance_metric == "1 All Spawners",
#          scenario != "Planned Plus") |>
#   group_by(year, scenario, run, location) |>
#   summarize(value = sum(value, na.rm = TRUE)) |>
#   left_join(year_lookup) |>
#   mutate(date = as.Date(paste0(actual_year, "-12-31"))) |>
#   filter(scenario == "Habitat and Hatchery",
#          location == "Feather River") |> glimpse()
#
# ggplot() +
#   geom_line(data = flow_spawn_plot_data, aes(x = date, y = value, color = location),linewidth = .5, alpha = 1) +
#   scale_color_manual(values = scenario_six_colors) +
#   # geom_line(data = flow_spawn_plot_data |> filter(scenario == "Baseline"), aes(x = date, y = value), color = "black", linewidth = .5, alpha = 1) +
#   scale_y_continuous(labels = scales::comma) +
#   labs(title = "Total Spawner Abundance over Time",
#        y = "Spawner Abundance",
#        x = "Year",
#        color = "Blended Scenario",
#        fill = "Hydrology",
#        #  caption = "Note: These numbers only reflect Upper Sacramento River Fall Run Chinook Spawners. Baseline and No Hatchery perform very simmilarly in the Upper Sacramento River."
#   ) +
#   theme_minimal() +
#   theme(
#     plot.caption = element_text(hjust = 0, face = "italic"),# move caption to the left
#     legend.position = "bottom",
#     # legend.position = "none",
#     axis.text = element_text(size = 15),
#     axis.title = element_text(size = 20),
#     plot.title = element_text(size = 20)
#   ) +
#   geom_hline(yintercept = 500)

## Percent Difference ----------------------------------------------------------
# create percent diff formula
percent_diff <- function(old_value, new_value) {
  ((new_value - old_value) / old_value) * 100
}

# create spawner
spawners_per_dif <- all_res |>
  filter(performance_metric == "1 All Spawners",
         scenario %in% c("Baseline", "Platypus", "Tortoise")) |>  # "Elephant")) |> "Elephant Plus"))  |>
  pivot_wider(names_from = scenario, values_from = value) |>
  group_by(year) |>
  summarize(`Baseline` = sum(`Baseline`, na.rm = TRUE),
            `Platypus` = sum(`Platypus`, na.rm = TRUE),
            `Tortoise` = sum(`Tortoise`, na.rm = TRUE)
            #`Elephant` = sum(`Elephant`, na.rm = TRUE)
            #`Elephant Plus` = sum(`Elephant Plus`, na.rm = TRUE)
            ) |>
  mutate(`Tortoise` = percent_diff(`Baseline`, `Tortoise`),
         `Platypus` = percent_diff(`Baseline`, `Platypus`)
         #`Elephant Plus` = percent_diff(`Baseline`, `Elephant Plus`),
         #`Elephant` = percent_diff(`Baseline`, `Elephant`)
         ) |>
  pivot_longer(`Tortoise`:`Platypus`, names_to = "scenario", values_to = "value") |> glimpse()

ggplot() +
  geom_line(data = spawners_per_dif, aes(x = year, y = value, color = scenario),linewidth = .5, alpha = 1) +
  scale_color_manual(values = scenario_six_colors) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Percent Change from Baseline",
       y = "Percent Change from Baseline",
       x = "Year",
       color = "Balanced Scenario",
  ) +
  theme_minimal() +
  theme(
    plot.caption = element_text(hjust = 0, face = "italic"),# move caption to the left
    legend.position = "bottom",
    # legend.position = "none",
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 20),
    plot.title = element_text(size = 20),
    legend.text = element_text(size=15),
    legend.title = element_text(size=15)
  ) +
  scale_y_continuous(labels = scales::unit_format(unit = "T", scale = 1e-3))

ggsave("data-raw/figures/percent_diff_plot_balanced.png")

## Fish Present Map ------------------------------------------------------------
# - potential map through the 20 year simulation indicating where fish are present
# - helpful to have some representation for the max habitat of which 7 populations are succeeding
library(leaflet.minicharts)
library(leaflet)
library(leaflet)
library(rgdal)
library(tidyverse)
library(readxl)
library(sp)


salmonid_extents <- readOGR("../../CVPIA/DSM/DSMhabitat/data-raw/rearing_spatial_data/salmonid_habitat_extents/salmonid_habitat_extents.shp",
                            stringsAsFactors = FALSE, verbose = FALSE) %>%
  spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs"))

fs <- subset(salmonid_extents, Species == 'Fall Run Chinook' & Habitat == 'spawning')
fr <- subset(salmonid_extents, Species == 'Fall Run Chinook' & Habitat == 'rearing')

make_label <- function(data) {
  labels <- sprintf("<strong>%s</strong> <br/> %s: %s miles",
                    data$River,
                    data$Habitat,
                    round(data$miles, 1)) %>% lapply(htmltools::HTML)
}

leaflet() %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) |>
  # addProviderTiles(providers$Stamen.Terrain,
  #                  options = providerTileOptions(noWrap = TRUE,
  #                                                opacity = 0.5,
  #                                                maxNativeZoom = 14,
  #                                                maxZoom = 14
  #                  )) %>%
  addPolylines(data = fs, group = 'Fall Run Spawning', label = make_label(fs),
               color = "#99B3CC", opacity = .8, weight = 3)


diversity_groups <- fallRunDSM::diversity_group
fish_present_data <- all_res |>
  filter(performance_metric == "1 All Spawners",
         scenario %in% c("Baseline", "Platypus", "Tortoise", "Elephant", "Elephant Plus")) |>
  mutate(scenario = case_when(scenario == "Max Flow" ~ "Run of River",
                              scenario == "Max Flow & Max Habitat" ~ "Run of River & Max Habitat",
                              scenario == "Theoretical Max Habitat" ~ "Max Habitat",
                              T ~ scenario)) |>
  mutate(`Spawners Present` = ifelse(value > 1, 1, NA),
         scenario = ifelse(scenario == "No Harvest", "Max Habitat, No Harvest", scenario),
         diversity_group = as.factor(diversity_group[location]),
         div_group_with_watersheds = case_when(diversity_group == 1 ~ paste(names(which(diversity_group == 1)), collapse = ", "),
                                               diversity_group == 2 ~ paste0("2:", paste(names(which(diversity_group == 2)), collapse = ", ")),
                                               diversity_group == 3 ~ paste0("3:", paste(names(which(diversity_group == 3)), collapse = ", ")),
                                               diversity_group == 4 ~ paste0("4:", paste(names(which(diversity_group == 4)), collapse = ", ")),
                                               diversity_group == 5 ~ paste0("5:", paste(names(which(diversity_group == 5)), collapse = ", ")))) |> glimpse()

fish_present <- fish_present_data |>
  ggplot(aes(x = year, y = `Spawners Present`, fill = diversity_group)) +
  geom_col() +
  theme_minimal() +
  scale_fill_manual(values = colors_small
                    # name = diversity_group,
                    # labels = c(paste(names(which(diversity_group == 1)), collapse = ", "),
                    #             paste(names(which(diversity_group == 2)), collapse = ", "),
                    #             paste(names(which(diversity_group == 3)), collapse = ", "),
                    #             paste(names(which(diversity_group == 4)), collapse = ", "),
                    #             paste(names(which(diversity_group == 5)), collapse = ", "))
                    ) +
  facet_wrap(~scenario, ncol = 2) +
  labs(y = "Number of Tribs with Spawners Present",
       x = "Simulation Year",
       fill = "Diversity Group") +
  theme(
    plot.caption = element_text(hjust = 0, face = "italic"),# move caption to the left
    # legend.text = "None",
    legend.position = "bottom",
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 18),
    plot.title = element_text(size = 18)
  )

# ggplotly(fish_present)
fish_present
diverity_group_lookup <- tibble("Diversity Group" = c(1, 2, 3, 4, 5),
                                "Tributaries Included" = c(paste(names(which(diversity_group == 1)), collapse = ", "),
                                                                        paste(names(which(diversity_group == 2)), collapse = ", "),
                                                                        paste(names(which(diversity_group == 3)), collapse = ", "),
                                                                        paste(names(which(diversity_group == 4)), collapse = ", "),
                                                                        paste(names(which(diversity_group == 5)), collapse = ", ")))
View(diverity_group_lookup)

# # - Wetted acre day plot
plot_data <- all_inputs |>
 filter(performance_metric == "8.1 & 8.2 Annual Wetted Acre Days")
#
# plot_data |>
#   ggplot(aes(x = year, y = value, color = scenario)) +
#   geom_line() +
#   theme_minimal() +
#   scale_color_manual(values = colors_full) +
#   geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
#   labs(x = "Simulation Year",
#        y = "Cohort Replacement Rate",
#        title = "Total Adult to Returning Natural Adult over Simulation Period")

# Harvest Plot
#
#
# SOME MAXHAB VISUALS
# - max hab map
# - max hab vs baseline proportion each habitat type (look at
#
# Max Hab - PHOS ---
phos_plot_data <- all_res |>
  filter(performance_metric %in% c("4 PHOS", "1 All Spawners"),
         scenario %in% c("Baseline", "Platypus", "Tortoise")) |> #, "Elephant", "Elephant Plus")) |>
         # location %in% c("Clear Creek", "Yuba River", "Feather River", "American River")
  pivot_wider(names_from = performance_metric, values_from = value) |>
  group_by(scenario, year) |>
  summarize(value = weighted.mean(`4 PHOS`, `1 All Spawners`, na.rm = TRUE)) |>
  ungroup() |>
  mutate(scenario = case_when(scenario == "Max Flow" ~ "Run of River",
                              scenario == "Theoretical Max Habitat" ~ "Max Habitat",
                              T ~ scenario)) |>
  glimpse()

phos_plot_data |>
  ggplot(aes(x = year, y = value, color = scenario)) +
  geom_line(linewidth = 1) +
  theme_minimal() +
  scale_color_manual(values = colors_full) +
  geom_hline(yintercept = .05, linetype = "dashed", color = "black") +
  # facet_wrap(~scenario) +
  labs(x = "Simulation Year",
       y = "PHOS",
       title = "PHOS for Select Tributaries Over Simulation Period") +
  theme(
    plot.caption = element_text(hjust = 0, face = "italic"),# move caption to the left
    # legend.position = c(0.85, 0.7),
    # legend.position = "none",
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 20),
    plot.title = element_text(size = 20)
  )
ggsave("data-raw/figures/phos_plot_balanced.png")


# OTHER PLOT IDEAS
# - think about plot to convey how each bookend compares to the baseline - got the max hab covered above


scenarios = "Baseline"

plot_data_juvs <- all_res |>
  filter(performance_metric == "Juveniles Size at Ocean Entry",
         location == "Upper Sacramento River",
         scenario %in% c("Baseline", "Platypus", "Tortoise", "Elephant", "Elephant Plus")) |>
  group_by(month, scenario, size_or_age) |>
  summarize(total_count = round(sum(value, na.rm = TRUE))) |>
  ungroup() |>
  mutate(scenario = ifelse(scenario == "Max Flow", "Run of River", scenario),
         month = month.abb[month],
         month = factor(month, levels = month.abb),
         size_class = case_when(size_or_age == "s" ~ "small",
                                size_or_age == "m" ~ "medium",
                                size_or_age == "l" ~ "large",
                                size_or_age == "vl" ~ "very large"),
         size_class = factor(size_class,
                             levels = c("small", 'medium', "large", "very large"))) |>
  glimpse()

plot_data_juvs |>
  ggplot(aes(x = month, y = total_count, fill = size_class)) +
  geom_col(alpha = .8) +
  # geom_density() +
  # facet_wrap(~year) +
  theme_minimal() +
  scale_fill_manual(values = colors_full[16:20]) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Size Distribution of Upper Sacramento River Juvenile Outmigrants",
       subtitle = "Average Juveniles at Chipps over 20 year simulation",
       y = "Total Count",
       x = "",
       fill = "Size Class") +
  theme(
    plot.caption = element_text(hjust = 0, face = "italic"),# move caption to the left
    # legend.position = c(0.85, 0.7),
    # legend.position = "none",
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 20),
    plot.title = element_text(size = 20),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
  ) +
  facet_grid(~scenario)
ggsave("data-raw/figures/juvenile_size_at_chips_plot_balanced.png")


# THRESHOLD PM, dot plots
potential_dependent_pops <- c("Bear River", "Big Chico Creek", "Elder Creek", "Paynes Creek",  "Stoney Creek", "Thomes Creek")
non_spawn_regions <- c("Upper-mid Sacramento River", "Sutter Bypass",
                       "Lower-mid Sacramento River", "Yolo Bypass",
                       "Lower Sacramento River", "San Joaquin River")
ind_pop <- all_res |>
  select(-size_or_age, -origin, -month, -run) |>
  filter(performance_metric %in% c("Natural Spawners", "2.2 Growth Rate Spawners",
                                   "4 PHOS", "2.1 CRR: Total Adult to Returning Natural Adult"),
         !location %in% potential_dependent_pops,
         !location %in% non_spawn_regions) |>
  pivot_wider(names_from = performance_metric, values_from = value) |>
  # round results
  mutate(`2.1 CRR: Total Adult to Returning Natural Adult` = round(`2.1 CRR: Total Adult to Returning Natural Adult`, 1),
         intrinsic_growth_rate = round(log(`Natural Spawners`) - log(lag(`Natural Spawners`))),
         `2.2 Growth Rate Spawners` = round(`2.2 Growth Rate Spawners`, 1),
         `4 PHOS` = round(`4 PHOS`, 2),
         `Natural Spawners` = round(`Natural Spawners`)) |>
  # categorize as meets threshold or not
  mutate(above_500_spawners = if_else(`Natural Spawners` > 500, TRUE, FALSE),
         phos_less_than_5_percent = ifelse(`4 PHOS` < .05, TRUE, FALSE),
         crr_above_1 = ifelse(`2.1 CRR: Total Adult to Returning Natural Adult` >= 1, TRUE, FALSE),
         growth_rate_above_1 = ifelse(`2.2 Growth Rate Spawners` >= 0, TRUE, FALSE),
         independent_population = ifelse(above_500_spawners & phos_less_than_5_percent &
                                           growth_rate_above_1 & crr_above_1, TRUE, FALSE))
# Platypus
ind_pop |>
  filter(scenario == "Platypus") |>
  mutate(phos_less_than_5_percent = ifelse(is.na(phos_less_than_5_percent), FALSE, phos_less_than_5_percent)) |>
  select(year, location, above_500_spawners, phos_less_than_5_percent) |>
  pivot_longer(above_500_spawners:phos_less_than_5_percent, names_to = "metric", values_to = "value") |>
  mutate(metric = ifelse(metric == "above_500_spawners", "Above 500 Spawners", "PHOS < 5%")) |>
  ggplot(aes(x = year, y = location, color = value)) +
  geom_point(size = 4) +
  scale_color_manual(values = c("#CCC591", "cadetblue4", "#CCC591"), name = "") +
  theme_minimal() +
  labs(y = "",
       x = "",
       title = "Pouplation and Hatchery Biological Parameters",
       subtitle = "Platypus") +
  theme(legend.position = "bottom") +
  facet_wrap(~metric)
ggsave("data-raw/figures/platypus_phos&abundance_params.png")


ind_pop |>
  filter(scenario == "Platypus") |>
  filter(year > 5) |>
  group_by(location) |>
  summarise("Average CRR" = round(mean(`2.1 CRR: Total Adult to Returning Natural Adult`, na.rm = TRUE), 2),
            "Average Growth Rate" = round(mean(`2.2 Growth Rate Spawners`, na.rm = TRUE), 2)) |>
  pivot_longer("Average CRR":"Average Growth Rate", names_to = "stat", values_to = "value") |>
  mutate(above_threshold = case_when(stat == "Average CRR" & value > 1 ~ TRUE,
                                     stat ==  "Average Growth Rate" & value > 0 ~ TRUE,
                                     T ~ FALSE)) |>
  ggplot(aes(x = location, y = value, fill = above_threshold)) +
  geom_col(size = 4) +
  scale_fill_manual(values = c("cadetblue4"), name = "") +
  ylim(-1, 8) +
  theme_minimal() +
  labs(y = "",
       x = "",
       title = "Average Productivity Metrics (yr 5 - 20) of simulation",
       subtitle = "Platypus") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  facet_wrap(~stat, nrow = 2)
ggsave("data-raw/figures/kitchen_sink_gr&crr.png")

# Elephant
ind_pop |>
  filter(scenario == "Elephant") |>
  mutate(phos_less_than_5_percent = ifelse(is.na(phos_less_than_5_percent), FALSE, phos_less_than_5_percent)) |>
  select(year, location, above_500_spawners, phos_less_than_5_percent) |>
  pivot_longer(above_500_spawners:phos_less_than_5_percent, names_to = "metric", values_to = "value") |>
  mutate(metric = ifelse(metric == "above_500_spawners", "Above 500 Spawners", "PHOS < 5%")) |>
  ggplot(aes(x = year, y = location, color = value)) +
  geom_point(size = 4) +
  scale_color_manual(values = c("#CCC591", "cadetblue4", "#CCC591"), name = "") +
  theme_minimal() +
  labs(y = "",
       x = "",
       title = "Pouplation and Hatchery Biological Parameters",
       subtitle = "Elephant") +
  theme(legend.position = "bottom") +
  facet_wrap(~metric)
ggsave("data-raw/figures/elephant_phos&abundance_params.png")


ind_pop |>
  filter(scenario == "Elephant") |>
  filter(year > 5) |>
  group_by(location) |>
  summarise("Average CRR" = round(mean(`2.1 CRR: Total Adult to Returning Natural Adult`, na.rm = TRUE), 2),
            "Average Growth Rate" = round(mean(`2.2 Growth Rate Spawners`, na.rm = TRUE), 2)) |>
  pivot_longer("Average CRR":"Average Growth Rate", names_to = "stat", values_to = "value") |>
  mutate(above_threshold = case_when(stat == "Average CRR" & value > 1 ~ TRUE,
                                     stat ==  "Average Growth Rate" & value > 0 ~ TRUE,
                                     T ~ FALSE)) |>
  ggplot(aes(x = location, y = value, fill = above_threshold)) +
  geom_col(size = 4) +
  scale_fill_manual(values = c("#CCC591", "cadetblue4"), name = "") +
  theme_minimal() +
  ylim(-1, 8) +
  labs(y = "",
       x = "",
       title = "Average Productivity Metrics (yr 5 - 20) of simulation",
       subtitle = "Habitat and Hatchery") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  facet_wrap(~stat, nrow = 2)
ggsave("data-raw/figures/elephant_gr&crr.png")

# Tortoise
ind_pop |>
  filter(scenario == "Tortoise") |>
  mutate(phos_less_than_5_percent = ifelse(is.na(phos_less_than_5_percent), FALSE, phos_less_than_5_percent)) |>
  select(year, location, above_500_spawners, phos_less_than_5_percent) |>
  pivot_longer(above_500_spawners:phos_less_than_5_percent, names_to = "metric", values_to = "value") |>
  mutate(metric = ifelse(metric == "above_500_spawners", "Above 500 Spawners", "PHOS < 5%")) |>
  ggplot(aes(x = year, y = location, color = value)) +
  geom_point(size = 4) +
  scale_color_manual(values = c("#CCC591", "cadetblue4", "#CCC591"), name = "") +
  theme_minimal() +
  labs(y = "",
       x = "",
       title = "Pouplation and Hatchery Biological Parameters",
       subtitle = "Tortoise") +
  theme(legend.position = "bottom") +
  facet_wrap(~metric)
ggsave("data-raw/figures/tortoise_phos&abundance_params.png")


ind_pop |>
  filter(scenario == "Tortoise") |>
  filter(year > 5) |>
  group_by(location) |>
  summarise("Average CRR" = round(mean(`2.1 CRR: Total Adult to Returning Natural Adult`, na.rm = TRUE), 2),
            "Average Growth Rate" = round(mean(`2.2 Growth Rate Spawners`, na.rm = TRUE), 2)) |>
  pivot_longer("Average CRR":"Average Growth Rate", names_to = "stat", values_to = "value") |>
  mutate(above_threshold = case_when(stat == "Average CRR" & value > 1 ~ TRUE,
                                     stat ==  "Average Growth Rate" & value > 0 ~ TRUE,
                                     T ~ FALSE)) |>
  ggplot(aes(x = location, y = value, fill = above_threshold)) +
  geom_col(size = 4) +
  scale_fill_manual(values = c("#CCC591", "cadetblue4"), name = "") +
  theme_minimal() +
  ylim(-1, 8) +
  labs(y = "",
       x = "",
       title = "Average Productivity Metrics (yr 5 - 20) of simulation",
       subtitle = "Tortoise") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  facet_wrap(~stat, nrow = 2)
ggsave("data-raw/figures/tortoise_gr&crr.png")



library(plotly)
plot_ly(plot_data_juvs, x = plot_data_juvs$size_or_age, y = plot_data_juvs$month, z = plot_data_juvs$total_count,
        group = plot_data_juvs$month, type = "scatter3d", mode = "lines")

library(plotly)

dens <- with(plot_data_juvs, tapply(size_or_age, INDEX = month, density))
data <- data.frame(
  x = unlist(lapply(dens, "[[", "x")),
  y = unlist(lapply(dens, "[[", "y")),
  size_or_age = rep(names(dens), each = length(dens[[1]]$x)))

fig <- plot_ly(data, x = ~x, y = ~y, z = ~size_or_age, type = 'scatter3d', mode = 'lines', color = ~size_or_age)

fig

### DSM FLOW RESULTS ###
biop <- DSMflow::proportion_pulse_flows$biop_itp_2018_2019 |>
  as_tibble() |>
  mutate(location = fallRunDSM::watershed_labels) |>
  pivot_longer(Jan:Dec, names_to = "months", values_to = "pulse_flows") |>
  mutate(scenario = "biop") |>
  glimpse()

month_nums <- tibble(month_num = c(1:12),
                     months = month.abb)

run_of_river <- DSMflow::proportion_pulse_flows$run_of_river |>
  as_tibble() |>
  mutate(location = fallRunDSM::watershed_labels) |>
  pivot_longer(Jan:Dec, names_to = "months", values_to = "pulse_flows") |>
  mutate(scenario = "run of river") |>
  glimpse()

pulse_flows <- bind_rows(biop, run_of_river) |>
  left_join(month_nums) |> glimpse()

pulse_flows |>
  filter(location == "Butte Creek") |>
  mutate(month = factor(months, levels = month.abb)) |>
  ggplot(aes(x = month, y = pulse_flows, color = scenario, group = scenario)) +
  geom_point() +
  geom_line() +
  theme_minimal()
  # facet_wrap(~location)
  #


produce_crr_geometric_mean_pm <- function(model_results_df){
  geom_mean_calc <- function(watershed, scenario) {
    data <- model_results_df |>
      filter(performance_metric == "2.1 CRR: Total Adult to Returning Natural Adult",
             location == watershed) |>
      select(year, location, scenario, run, performance_metric, value) |>
      mutate(geometric_mean = zoo::rollapply(value, 3, psych::geometric.mean, fill = NA)) |>
      filter(!is.na(geometric_mean))
    return(data)
  }
  watersheds <- rep(fallRunDSM::watershed_labels, 7)
  scenarios_lists <- c(rep("Baseline", 31),
                       rep("Theoretical Max Habitat", 31),
                       rep("No Harvest", 31),
                       rep("No Hatchery", 31),
                       rep("Max Flow", 31),
                       rep("Max Flow & Max Habitat", 31),
                       rep("Max Hatchery", 31))

  res <- purrr::map2(watersheds,scenarios_lists, geom_mean_calc) |> reduce(bind_rows)
  goem_mean_crr <- res |>
    group_by(location, scenario) |>
    summarize(average_crr = mean(geometric_mean, na.rm = TRUE)) |>
    ungroup() |>
    mutate(average_crr = ifelse(average_crr == Inf, 0, average_crr)) |>
    group_by(scenario) |>
    summarize(avg_annual_crr = mean(average_crr, na.rm = T),
              min_annual_crr = min(average_crr, na.rm = T),
              max_annual_crr = max(average_crr, na.rm = T))
  return(goem_mean_crr)
}
produce_crr_geometric_mean_pm(all_res)


# habitat decay
produce_habitat_ratios <- function(model_parameters, watershed, scenario_name) {
  # pull base habitat from model parameters
  spawn_hab <- model_parameters$spawning_habitat
  rearing_habitat_juvenile <- model_parameters$inchannel_habitat_juvenile
  rearing_habitat_fry <- model_parameters$inchannel_habitat_fry

  # Generate floodplain scaler to multiply floodplain habitat by
  floodplain_scaler <-  model_parameters$weeks_flooded
  floodplain_scaler[floodplain_scaler == 2] <- .5
  floodplain_scaler[floodplain_scaler == 1] <- .25
  floodplain_scaler[floodplain_scaler == 3] <- .75
  floodplain_scaler[floodplain_scaler == 4] <- 1

  floodplain_habitat <- model_parameters$floodplain_habitat * floodplain_scaler

  total_habitat <- spawn_hab[,,2:22] + rearing_habitat_juvenile +
    rearing_habitat_fry + floodplain_habitat

  perc_spawn <- (spawn_hab[,, 2:22]/total_habitat) * 100
  perc_rear_fry <- (rearing_habitat_fry/total_habitat) * 100
  perc_rear_juv <- (rearing_habitat_juvenile/total_habitat) * 100
  perc_fp <- (floodplain_habitat/total_habitat) * 100

  spawners <- as_tibble(perc_spawn) |>
    dplyr::mutate(location = fallRunDSM::watershed_labels) |>
    pivot_longer(cols = c(1:252), values_to = 'percent_spawning_habitat', names_to = "month_year") |>
    mutate(month = match(gsub("\\..*","", month_year), month.abb),
           year = gsub("^.*\\.","", month_year),
           date = as.Date(paste0(year, "-", month, "-1")),
           habitat_type = "spawning") |>
    select(location, date, habitat_type, percent_habitat = percent_spawning_habitat) |> glimpse()

  fry_rearing <- as_tibble(perc_rear_fry) |>
    dplyr::mutate(location = fallRunDSM::watershed_labels) |>
    pivot_longer(cols = c(1:252), values_to = 'percent_fry_rearing_habitat', names_to = "month_year") |>
    mutate(month = match(gsub("\\..*","", month_year), month.abb),
           year = gsub("^.*\\.","", month_year),
           date = as.Date(paste0(year, "-", month, "-1")),
           habitat_type = "fry rearing") |>
    select(location, date, habitat_type, percent_habitat = percent_fry_rearing_habitat) |> glimpse()

  juv_rearing <- as_tibble(perc_rear_juv) |>
    dplyr::mutate(location = fallRunDSM::watershed_labels) |>
    pivot_longer(cols = c(1:252), values_to = 'percent_juv_rearing_habitat', names_to = "month_year") |>
    mutate(month = match(gsub("\\..*","", month_year), month.abb),
           year = gsub("^.*\\.","", month_year),
           date = as.Date(paste0(year, "-", month, "-1")),
           habitat_type = "juv rearing") |>
    select(location, date, habitat_type, percent_habitat = percent_juv_rearing_habitat) |> glimpse()

  floodplain <- as_tibble(perc_fp) |>
    dplyr::mutate(location = fallRunDSM::watershed_labels) |>
    pivot_longer(cols = c(1:252), values_to = 'percent_floodplain_habitat', names_to = "month_year") |>
    mutate(month = match(gsub("\\..*","", month_year), month.abb),
           year = gsub("^.*\\.","", month_year),
           date = as.Date(paste0(year, "-", month, "-1")),
           habitat_type = "floodplain rearing") |>
    select(location, date, habitat_type, percent_habitat = percent_floodplain_habitat) |> glimpse()

  all_hab_percents <- bind_rows(spawners, fry_rearing, juv_rearing, floodplain) |>
    mutate(month_length = lubridate::days_in_month(lubridate::month(date)))

  all_hab_percents |>
    filter(if(watershed != "All") location == watershed else TRUE) |>
    ggplot(aes(x = date, y = percent_habitat, fill = habitat_type)) +
    geom_col(position = "stack", width = 31) +
    scale_fill_manual(values = wesanderson::wes_palette("Royal1")) +
    theme_minimal() +
    theme(
      # legend.position = "bottom",
          legend.title = element_blank()) +
    labs(x = "Simulation Year",
         y = "Percentage",
         title = paste0(watershed, ": Ratio of Habitat Types for Simulation Period"),
         subtitle = scenario_name) +
    theme(text = element_text(size = 15))
}

produce_habitat_ratios(r_to_r_baseline_params, "Upper Sacramento River", "Baseline")
produce_habitat_ratios(r_to_r_kitchen_sink_params, "Upper Sacramento River", "Kitchen Sink")


# Survival step function

steps <- tibble("Flow (cms)" = c(0, 122, 122, 303, 303, 1000),
                "Migratory Survival" = c(.03, .03, .189, .189,.508, .508 ))
ggplot(steps, aes(x = `Flow (cms)`, y = `Migratory Survival`)) +
  # geom_point() +
  geom_line(color = "#7294D4", size = 1.5) +
  theme_minimal() +
  labs(title = "Upper Sacramento River Migratory Survival Curves") +
  theme(text = element_text(size = 15))

produce_habitat_ratios(model_parameters = fallRunDSM::r_to_r_baseline_params,
                       watershed = "Upper Sacramento River")

produce_habitat_ratios(model_parameters = fallRunDSM::r_to_r_kitchen_sink_params,
                       watershed = "Upper Sacramento River")
