library(tidyverse)
library(readxl)
library(here)

# Storage data -----------------------------------------------------------------
raw_storage_path <- here("data-raw", "calsim3-data", "HRL-storage.xlsx")
raw_storage <- read_excel(raw_storage_path)

# replace the string with a regex to id potential nodes
colnames(raw_storage)[str_detect(colnames(raw_storage), "FR")]

# thes are potential mappings for each of the storage locations in the original script
# TODO - missing Friant Dam!!!
raw_storage |>
  select(date, S_WKYTN, S_KSWCK, S_THRMA, S_OROVL, S_ENGLB,S_FOLSM, S_MELON, S_PEDRO,
         S_MCLRE, S_NHGAN, S_SGRGE, S_SHSTA)

