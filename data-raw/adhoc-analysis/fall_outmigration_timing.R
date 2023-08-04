library(tidyverse)
library(plotly)
colors_full <-  c("#9A8822", "#F5CDB4", "#F8AFA8", "#FDDDA0", "#74A089", #Royal 2
                  "#899DA4", "#C93312", "#DC863B", # royal 1 (- 3)
                  "#F1BB7B", "#FD6467", "#5B1A18", # Grand Budapest 1 (-4)
                  "#D8B70A", "#02401B", "#A2A475", # Cavalcanti 1
                  "#E6A0C4", "#C6CDF7", "#D8A499", "#7294D4", #Grand Budapest 2
                  "#9986A5", "#EAD3BF", "#AA9486", "#B6854D", "#798E87" # Isle of dogs 2 altered slightly
)

muted = c("#972D15", "#D8B70A", "#A2A475",
          "#81A88D",
          "#02401B")
# Fall Run Outmigration Timing Visuals
# Save standard catch from JPE efforts
catch_raw <- read_csv("data-raw/adhoc-analysis/standard_catch.csv")

# pull in water year type data
water_yt_lookup <- waterYearType::water_year_indices |>
  filter(WY > 1977, WY < 2021, location == "Sacramento Valley") |>
  rename(water_year = WY,
         year_type = Yr_type) |> glimpse()

fall_run_catch <- catch_raw |> #filter(catch_raw, run == "fall" | stream %in% c("mill creek", "deer creek")) %>%
  mutate(day = day(date),
         month = month(date),
         year = year(date),
         water_year = ifelse(month %in% 10:12, year + 1, year),
         fake_date = as_date(paste(ifelse(month %in% 10:12, 1899, 1900), month, day))) %>%
  group_by(fake_date, water_year, stream, date) %>%
  summarize(count = sum(count))

# Yearly counts by stream/year
fall_run_total_catch <- fall_run_catch %>%
  group_by(water_year, stream) %>%
  summarize(total = sum(count))

fall_run_cumulative <- fall_run_catch %>%
  arrange(date) %>%
  group_by(stream, water_year) %>%
  mutate(count = ifelse(is.na(count), 0, count),
         total_count = sum(count, na.rm = T),
         cumulative_catch = cumsum(count),
         prop_cuml_catch = cumulative_catch/total_count * 100) |>
  left_join(water_yt_lookup)

vline <- function(x = 0, color = "black") {
  list(
    type = "line",
    y0 = 0,
    y1 = 1,
    yref = "paper",
    x0 = x,
    x1 = x,
    line = list(color = color)
  )
}

plot_cumulative_catch <- function(selected_stream){
  plot_data <- fall_run_cumulative |>
    filter(stream == selected_stream,
           !is.na(year_type)) |> glimpse()
  plot_title <- ifelse(selected_stream == "sacramento river",
                  "Sacramento River (Knights Landing and Tisdale) Cummulative Catch & Water Year Type",
                  paste(stringr::str_to_title(selected_stream), "Cummulative Catch & Water Year Type"))
  plot_ly(plot_data, x = ~fake_date, y = ~prop_cuml_catch,
          text = ~water_year,
          hovertemplate = paste(
            "Water Year: %{text}"),
          color = ~year_type,
          colors = muted,
          type = 'scatter', mode = 'lines') |>
    layout(xaxis = list(title = "Months", tickformat = "%b"),
           yaxis = list(title = "Percent Cumulative Catch"),
           title = plot_title,
           shapes = list(vline(x = as.Date("1900-08-01"))) # sending fish out in aug
           )
}
plot_cumulative_catch("sacramento river")
