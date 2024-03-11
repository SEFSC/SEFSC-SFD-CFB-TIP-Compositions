# 03_prep

# filter the data to the correct sector, length measurements, and years

# Load libraries ####
librarian::shelf(here, tidyverse)

# Specify settings ####
tip_spp_rds <- "pr_yts_forest_tip_20240307.rds" # rds from end of 02 script
spp <- "yts"
isl <- "pr"
len_type <- "FORK LENGTH"
len_mode <- "COMMERCIAL"
min_size <- 1
max_size <- 122   
start_yr <- 1983
end_yr <- 2022 

# Read in formatted data ####
tip_spp <- readRDS(here::here("data", tip_spp_rds))

# Prep data for analysis ####
# filter to specified settings 
tip_spp_len <- tip_spp |>
  dplyr::filter(
    length_type1 == len_type,
    sector == len_mode,
    year <= end_yr,
    length1_cm > min_size,
    length1_cm <= max_size
  ) 
  # dplyr::mutate(
  #   k_keep = k >= ext_k_lower & k <= ext_k_upper
  # )

# Save prepped tip_spp_len ####
saveRDS(
  tip_spp_len,
  file = here::here(
    "data",
    paste0(
      isl, "_",
      spp, "_prep_tip_",
      format(Sys.time(), "%Y%m%d"), ".rds"
    )
  )
)
