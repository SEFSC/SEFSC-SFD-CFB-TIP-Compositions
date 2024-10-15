# 03_prep

# filter the data to the correct sector, length measurements, and years

# Load libraries ####
  librarian::shelf(here, tidyverse)

# Specify settings ####
# rds from end of 02aa script
  tip_spp_rds <- "prusvi_csl_spp_quant_check_20241010.rds" 
  spp <- "csl"
# from here on chose one island to work with moving forward
  isl <- "stx" 
  data_keep <- "TIP"
  len_mode <- "COMMERCIAL"
  len_type <- "CARAPACE LENGTH"
# add if there are outliers that need to be removed
  # min_size <- 1 
  # max_size <- 122
# based on 01a settings unless running truncated time series
  start_yr <- 1984 
  end_yr <- 2022
  print_isl <- "Puerto Rico"

# Read in formatted data ####
  tip_spp <- readRDS(here::here("data", tip_spp_rds))

# replace "," with ";"
  tip_spp$gear <- str_replace(tip_spp$gear, ",", ";")

# Prep data for analysis ####
# filter to specified settings
  tip_spp_len <- tip_spp |>
    filter(
      island == isl,
      # data_source == data_keep,
      !is.na(length1_mm),
      sector == len_mode,
      length_type1 == len_type,
      year >= start_yr & year <= end_yr,
    ) |>
    group_by(gear) |>
    dplyr::mutate(n_ID = n_distinct(id)) |>
    dplyr::filter(n_ID >= 3) |>
    ungroup() |>
    group_by(year) |>
    filter(n() >= 30) |>
    ungroup() |>
    dplyr::filter(
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
        spp, "_prep_keep_tip_",
        format(Sys.time(), "%Y%m%d"), ".rds"
      )
    )
  )
