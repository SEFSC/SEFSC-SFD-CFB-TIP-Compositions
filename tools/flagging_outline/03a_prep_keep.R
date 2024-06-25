# 03_prep

# filter the data to the correct sector, length measurements, and years

# Load libraries ####
librarian::shelf(here, tidyverse)

# Specify settings ####
# if you want just complete l/w pairs use this one
tip_spp_rds <- "pr_yts_spp_size_flag_20240618.rds" # rds from end of 02d script
# if you want all lengths regardless of weights use this one 
tip_spp_rds <- "pr_yts_spp_size_prep_20240529.rds" # rds from end of 02a script
spp <- "yts"
isl <- "pr"
len_mode <- "COMMERCIAL"
# min_size <- 1 # add if there are outliers that need to be removed
# max_size <- 122
start_yr <- 1984 # based on 01a settings unless running truncated time series 
end_yr <- 2022
print_isl <- "Puerto Rico"


# Read in formatted data ####
tip_spp <- readRDS(here::here("data", tip_spp_rds))

# Prep data for analysis ####
# filter to specified settings
tip_spp_len <- tip_spp |>
  dplyr::filter(
    sector == len_mode,
    year >= start_yr & year <= end_yr,
    length1_cm > min_size,
    length1_cm <= max_size,
    k_flag == "keep"
  )

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
