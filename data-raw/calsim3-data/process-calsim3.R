library(tidyverse)
library(readxl)
library(here)

base_path <- here("data-raw", "calsim3-data")

# Storage data -----------------------------------------------------------------
raw_storage_path <- here(base_path, "HRL-storage.xlsx")
raw_storage <- read_excel(raw_storage_path)

# replace the string with a regex to id potential nodes
colnames(raw_storage)[str_detect(colnames(raw_storage), "FR")]

# thes are potential mappings for each of the storage locations in the original script
lto_12a_storage <- raw_storage |>
  select(date, S_WKYTN, S_KSWCK, S_THRMA, S_OROVL, S_ENGLB,S_FOLSM, S_MELON, S_PEDRO,
         S_MCLRE, S_NHGAN, S_SGRGE, S_SHSTA, S_MLRTN) # Friant dam is on Millerton


# Delivery Data ----------------------------------------------------------------
raw_delivery_data_path <- here(base_path, "HRL-deliveries-swp-cvp.xlsx")
raw_delivery <- readxl::read_excel(raw_delivery_data_path)

# TODO - DEL_SWP_TOT_N NOT in the dataset!
raw_delivery |>
  select(DEL_CVP_PAG_N, DEL_CVP_PMI_N, DEL_CVP_PRF_N, DEL_CVP_PSC_N,
         DEL_CVP_PAG_S, DEL_CVP_PMI_S, DEL_CVP_PRF_S, DEL_CVP_PEX_S,
         DEL_CVP_TOTAL_N, DEL_CVP_TOTAL_S, DEL_SWP_PAG_N, DEL_SWP_PMI_N,
         DEL_SWP_PAG_S, DEL_SWP_PMI_S, DEL_SWP_TOT_S)

colnames(raw_delivery)[str_detect(colnames(raw_delivery), "AG")]


# Delta Outflow ----------------------------------------------------------------



