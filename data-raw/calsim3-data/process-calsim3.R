library(tidyverse)
library(readxl)

raw_storage <- read_excel("data-raw/calsim3-data/HRL-storage.xlsx")

raw_storage |>
  select(date, S_WKYTN, S_KSWCK)
