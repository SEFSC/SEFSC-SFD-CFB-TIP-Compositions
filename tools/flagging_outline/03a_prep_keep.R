# 03_prep

# filter the data to the correct sector, length measurements, and years

# Load libraries ####
librarian::shelf(here, tidyverse)

# Specify settings ####
tip_spp_rds <- "pr_yts_spp_size_prep_20240529.rds" # rds from end of 02a script
spp <- "yts"
isl <- "pr"
len_mode <- "COMMERCIAL"
len_type <- "FORK LENGTH"
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
  group_by(gear) |> 
  dplyr::mutate(n_ID = n_distinct(id)) |>
  dplyr::filter(n_ID >= 3) |>
  ungroup() |>  
  group_by(year) |> 
  filter(n() >= 30) |>
  ungroup()  |> 
  dplyr::filter(
    sector == len_mode,
    length_type1 == len_type,
    year >= start_yr & year <= end_yr,
    # length1_cm > min_size,
    # length1_cm <= max_size
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
