library(tidyverse)
library(readxl)
library(here)

# Storage data -----------------------------------------------------------------
raw_storage_path <- here("data-raw", "calsim3-data", "HRL-storage.xlsx")
raw_storage <- read_excel(raw_storage_path)

# all nodes
colnames(raw_storage)[str_detect(colnames(raw_storage), "FR")]

raw_storage |>
  select(date, S_WKYTN, S_KSWCK, S_THRMA, S_OROVL, S_ENGLB,S_FOLSM, S_MELON, S_PEDRO,
         S_MCLRE, S_NHGAN, S_SGRGE, S_SHSTA)


storage_locations_from_calsim2 <- c("Wiskeytown Lake",
                                    "Shasta Lake",
                                    "Keswich Reservoir",
                                    'Thermalito Complex',
                                    "Lake Oroville",
                                    "Stony Gorge",
                                    "Engilbright",
                                    "Folsom",
                                    "New Hogan",
                                    "New Melones",
                                    "New Don Padro",
                                    "Lake McClure",
                                    "Friant")
