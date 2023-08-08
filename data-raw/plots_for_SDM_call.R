library(tidyverse)
library(producePMs)
source("data-raw/shiny-materials/process_model_results.R")
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
          "Run of River" = "#899DA4", "Biop (Baseline)" = "#7294D4")

blues <- c( "Run of River" = "#899DA4", "Biop (Baseline)" = "#7294D4")
# GENERAL PLOTS
# - Figure that shows what has happened since the last workshop (conceptual model with updates)\

# CRR -------------------------------------------------------------------------
# - CRR - show some more hatchery dominated tribs, show a few less dominated tribs
plot_data <- all_res |>
  filter(performance_metric == "2.1 CRR: Total Adult to Returning Natural Adult",
         scenario != "Max Flow & Max Habitat",
         location %in% c("Clear Creek", "Yuba River", "Feather River", "American River")
         )  |>
  mutate(scenario = case_when(scenario == "Max Flow" ~ "Run of River",
                              scenario == "Theoretical Max Habitat" ~ "Max Habitat",
                              T ~ scenario)) |>
glimpse()

plot_data |>
  ggplot(aes(x = year, y = value, color = location)) +
  geom_line() +
  theme_minimal() +
  scale_color_manual(values = colors_full) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  facet_wrap(~scenario) +
  labs(x = "Simulation Year",
       y = "Cohort Replacement Rate",
       title = "Total Adult to Returning Natural Adult over Simulation Period") +
  theme(
    plot.caption = element_text(hjust = 0, face = "italic"),# move caption to the left
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 20),
    plot.title = element_text(size = 20)
  )

### All Spawn ------------------------------------------------------------------
# - Spawners - overlay flows & All spawners number(Upper Sac)
water_yt_lookup <- waterYearType::water_year_indices |>
  filter(WY > 1977, WY < 2001, location == "Sacramento Valley") |>
  mutate(water_year = WY,
         year_type = Yr_type) |> glimpse()

flow_data_biop <- DSMflow::flows_cfs$biop_itp_2018_2019 |>
  pivot_longer(cols = -date, names_to = "watershed", values_to = "flow_cfs") |>
  filter(date >= lubridate::as_date("1979-01-01"),
         watershed == "Upper Sacramento River") |>
  mutate(water_year = ifelse(month(date) %in% 10:12, year(date) + 1, year(date)),
         Hydrology = "Biop (Baseline)") |>
  filter(water_year < 2001) |>
  left_join(water_yt_lookup) |>
  glimpse()

flow_data_ror <- DSMflow::flows_cfs$run_of_river |>
  pivot_longer(cols = -date, names_to = "watershed", values_to = "flow_cfs") |>
  filter(date >= lubridate::as_date("1979-01-01"),
         watershed == "Upper Sacramento River") |>
  mutate(water_year = ifelse(month(date) %in% 10:12, year(date) + 1, year(date)),
         Hydrology = "Run of River") |>
  filter(water_year < 2001) |>
  left_join(water_yt_lookup) |> glimpse()

flow_data <-bind_rows(flow_data_biop, flow_data_ror)

year_lookup <- tibble(year = 1:21,
                      actual_year = 1980:2000)
flow_spawn_plot_data <- all_res |>
  filter(performance_metric == "1 All Spawners") |>
  group_by(year, scenario, run) |>
  summarize(value = sum(value, na.rm = TRUE)) |>
  left_join(year_lookup) |>
  mutate(date = as.Date(paste0(actual_year, "-12-31"))) |>
  filter(scenario != "No Hatchery") |> glimpse()

# Spawn plot with 20 years of data
ggplot() +
  # geom_area(data = flow_data, aes(x = date, y = flow_cfs, fill = Hydrology), alpha = .5, position = "identity") +
  geom_col(data = flow_data, aes(x = date, y = -4000, fill = year_type), alpha = 1, width = 31) +
  scale_fill_manual(values = muted,
                    # limits = c("Run of River", "Biop (Baseline)")
                   #  limits = c('Critical', 'Dry', 'Below Normal', 'Above Normal', 'Wet')
                    ) +
  # geom_line(data = flow_data, aes(x = date, y = flow_cfs), color = "black", linewidth = .1) +
  geom_line(data = flow_spawn_plot_data, aes(x = date, y = value, color = scenario),linewidth = .5, alpha = 1) +
  scale_color_manual(values = c(rep("gray", 7))) +
  geom_line(data = flow_spawn_plot_data |> filter(scenario == "Baseline"), aes(x = date, y = value), color = "black", linewidth = .5, alpha = 1) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Historical Water Year Types, and Total Spawner Abundance",
       y = "Spawner Abundance",
       x = "Year",
       linetype = "Bookend Scenario",
       fill = "Hydrology"
       # caption = "Note: These numbers only reflect Upper Sacramento River Fall Run Chinook Spawners. Baseline and No Hatchery perform very simmilarly in the Upper Sacramento River."
       ) +
  theme_minimal() +
  theme(
    plot.caption = element_text(hjust = 0, face = "italic"),# move caption to the left
    # legend.position = c(0.85, 0.7),
    legend.position = "none",
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 20),
    plot.title = element_text(size = 20)
  )


# JUST HYDROLOGY
ggplot() +
  geom_area(data = flow_data, aes(x = date, y = flow_cfs, fill = Hydrology), alpha = .5, position = "identity") +
  # geom_col(data = flow_data, aes(x = date, y = -4000, fill = year_type), alpha = 1, width = 31) +
  scale_fill_manual(values = muted,
                    limits = c("Run of River", "Biop (Baseline)")
                    ) +
  # geom_line(data = flow_data, aes(x = date, y = flow_cfs), color = "black", linewidth = .1) +
  # geom_line(data = flow_spawn_plot_data, aes(x = date, y = value, color = scenario),linewidth = .5, alpha = 1) +
  # scale_color_manual(values = c(rep("gray", 7))) +
  # geom_line(data = flow_spawn_plot_data |> filter(scenario == "Baseline"), aes(x = date, y = value), color = "black", linewidth = .5, alpha = 1) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Upper Sacramento River Flows",
       subtitle = "Baseline and Run of River",
       y = "Flow CFS",
       x = "Year",
       # linetype = "Bookend Scenario",
       fill = "Hydrology") +
  theme_minimal() +
  theme(
    plot.caption = element_text(hjust = 0, face = "italic"),# move caption to the left
    # legend.position = c(0.85, 0.7),
    # legend.position = "none",
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 20),
    plot.title = element_text(size = 20)
  )

# just abundance
ggplot() +
  # geom_area(data = flow_data, aes(x = date, y = flow_cfs, fill = Hydrology), alpha = .5, position = "identity") +
  # geom_col(data = flow_data, aes(x = date, y = -4000, fill = year_type), alpha = 1, width = 31) +
  # scale_fill_manual(values = muted,
                    # limits = c("Run of River", "Biop (Baseline)")
                    #  limits = c('Critical', 'Dry', 'Below Normal', 'Above Normal', 'Wet')
  # ) +
  # geom_line(data = flow_data, aes(x = date, y = flow_cfs), color = "black", linewidth = .1) +
  geom_line(data = flow_spawn_plot_data, aes(x = date, y = value, color = scenario),linewidth = .5, alpha = 1) +
  scale_color_manual(values = scenario_six_colors) +
  # geom_line(data = flow_spawn_plot_data |> filter(scenario == "Baseline"), aes(x = date, y = value), color = "black", linewidth = .5, alpha = 1) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Total Spawner Abundance over Time",
       y = "Spawner Abundance",
       x = "Year",
       color = "Bookend Scenario",
       fill = "Hydrology",
      #  caption = "Note: These numbers only reflect Upper Sacramento River Fall Run Chinook Spawners. Baseline and No Hatchery perform very simmilarly in the Upper Sacramento River."
       ) +
  theme_minimal() +
  theme(
    plot.caption = element_text(hjust = 0, face = "italic"),# move caption to the left
    legend.position = c(0.85, 0.7),
    # legend.position = "none",
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 20),
    plot.title = element_text(size = 20)
  )

## Fish Present Map ------------------------------------------------------------
# - potential map through the 20 year simulation indicating where fish are present
# - helpful to have some representation for the max habitat of which 7 populations are succeeding
library(leaflet.minicharts)
library(leaflet)
library(leaflet)
library(rgdal)
library(tidyverse)
library(readxl)

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
         scenario != "Theoretical Max Habitat") |>
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

# - Wetted acre day plot
plot_data <- all_inputs |>
  filter(performance_metric == "")

plot_data |>
  ggplot(aes(x = year, y = value, color = scenario)) +
  geom_line() +
  theme_minimal() +
  scale_color_manual(values = colors_full) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  labs(x = "Simulation Year",
       y = "Cohort Replacement Rate",
       title = "Total Adult to Returning Natural Adult over Simulation Period")

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
         scenario %in% c("Baseline",
                         "Theoretical Max Habitat",
                         # "Max Flow",
                         "No Hatchery",
                         "Max Hatchery")
         # location %in% c("Clear Creek", "Yuba River", "Feather River", "American River")
  )  |>
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
  geom_line() +
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


# OTHER PLOT IDEAS
# - think about plot to convey how each bookend compares to the baseline - got the max hab covered above


scenarios = "Baseline"

plot_data_juvs <- all_res |>
  filter(performance_metric == "Juveniles Size at Ocean Entry",
         scenario %in% c("Baseline", "Max Flow"),
         location == "Battle Creek") |>
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
       x = "Month",
       fill = "Size Class") +
  theme(
    plot.caption = element_text(hjust = 0, face = "italic"),# move caption to the left
    # legend.position = c(0.85, 0.7),
    # legend.position = "none",
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 20),
    plot.title = element_text(size = 20)
  ) +
  facet_grid(~scenario)

library(plotly)
plot_ly(plot_data, x = plot_data$size_or_age, y = plot_data$month, z = plot_data$total_count,
        group = plot_data$month, type = "scatter3d", mode = "lines")

library(plotly)

dens <- with(plot_data, tapply(size_or_age, INDEX = month, density))
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
produce_habitat_ratios(r_to_r_tmh_params, "Upper Sacramento River", "Max Habitat")


# Survival step function

steps <- tibble("Flow (cms)" = c(0, 122, 122, 303, 303, 1000),
                "Migratory Survival" = c(.03, .03, .189, .189,.508, .508 ))
ggplot(steps, aes(x = `Flow (cms)`, y = `Migratory Survival`)) +
  # geom_point() +
  geom_line(color = "#7294D4", size = 1.5) +
  theme_minimal() +
  labs(title = "Upper Sacramento River Migratory Survival Curves") +
  theme(text = element_text(size = 15))
