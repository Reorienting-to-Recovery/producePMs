library(fallRunDSM)
library(tidyverse)
library(plotly)

colors_small <-  c("#9A8822", "#F5CDB4", "#F8AFA8", "#FDDDA0", "#74A089", #Royal 2
                   "#899DA4", "#C93312", "#DC863B" # royal 1 (- 3)
)
colors_full <-  c("#9A8822", "#F5CDB4", "#F8AFA8", "#FDDDA0", "#74A089", #Royal 2
                  "#899DA4", "#C93312", "#DC863B", # royal 1 (- 3)
                  "#F1BB7B", "#FD6467", "#5B1A18", "#D67236",# Grand Budapest 1 (-4)
                  "#D8B70A", "#02401B", "#A2A475", # Cavalcanti 1
                  "#E6A0C4", "#C6CDF7", "#D8A499", "#7294D4", #Grand Budapest 2
                  "#9986A5", "#EAD3BF", "#AA9486", "#B6854D", "#798E87", # Isle of dogs 2 altered slightly
                  "#F3DF6C", "#CEAB07", "#D5D5D3", "#24281A", # Moonriese 1,
                  "#798E87", "#C27D38", "#CCC591", "#29211F", # moonrise 2
                  "#85D4E3", "#F4B5BD", "#9C964A", "#CDC08C", "#FAD77B") # moonrise 3

# MODEL OUTPUT PERFORMANCE METRICS ---------------------------------------------
# Shaded bar plots
# Can be used for juvenile size distibution and adult age at return
plot_shaded_bar <- function(model_results_df, selected_scenario, selected_run,
                            selected_lifestage = c("adult", "juv"),
                            watershed,
                            shiny_version = FALSE){
  if (shiny_version == FALSE){
    selected_preformance_metric <- switch(selected_lifestage,
                                          "adult" = "Adult Age of Return",
                                          "juv" = "Juveniles Size at Ocean Entry")
    model_results_df <- model_results_df |>
      filter(scenario == selected_scenario,
             run == selected_run,
             performance_metric == selected_preformance_metric,
             location == watershed)
  }
  y_label <- switch(selected_lifestage,
                    "juv" = "Juvenile Size Distribution at Chipps",
                    "adult" = "Adult Age of Return")
  model_results_df |>
    mutate(size = factor(size, levels = c("s", "m", "l", "vl"))) |>
    ggplot(aes(x = year, y = value, fill = size)) +
    geom_col(position = "fill") +
    theme_minimal() +
    scale_fill_manual(values = colors_small) +
    labs(x = "Year",
         y = y_label)
}

# Line Plot -
plot_line_bar <- function(model_results_df, selected_scenario, selected_run,
                            selected_performance_metric,
                            shiny_version = FALSE){
  if (shiny_version == FALSE){
    model_results_df <- model_results_df |>
      filter(scenario == selected_scenario,
             run == selected_run,
             performance_metric == selected_preformance_metric)
  }
  model_results_df |>
    ggplot(aes(x = year, y = value, color = location)) +
    geom_line() +
    theme_minimal() +
    scale_color_manual(values = colors_full) +
    scale_y_continuous(labels = scales::comma) +
    labs(x = "Year",
         y = selected_performance_metric)
}
