# 03_prep

# filter the data to the correct sector, length measurements, and years

# Load libraries ####
  librarian::shelf(here, tidyverse)

# Specify settings ####
# rds from end of 02aa script
  date <- "20241024" 
  spp <- "csl"
# from here on chose one island to work with moving forward
  isl <- "stt" 
  data_keep <- "TIP"
  len_mode <- "COMMERCIAL"
  len_type <- "CARAPACE LENGTH"
# add if there are outliers that need to be removed
  min_size <- 2.5
  max_size <- 25
# all complete years unless running truncated time series
  start_yr <- 1981 
  end_yr <- 2022
  print_isl <- "St. Thomas"
  sedar <- "sedar91"
  
#### make sure you have run 02c so you have the correct file structure to save ####
  
# Read in formatted data ####
  tip_spp_rds <- paste0("prusvi_csl_spp_size_quantity_", date, ".rds" )
  tip_spp <- readRDS(here::here("data", sedar, "rds", spp, "all", tip_spp_rds))

# Prep data for analysis ####
# filter to specified settings
  tip_spp_len <- tip_spp |>
    filter(
      island == isl,
      # sampling_program == data_keep,
      !is.na(length1_mm),
      fishery == len_mode,
      length_type1 == len_type,
      year >= start_yr & year <= end_yr,
    ) |>
    group_by(gear) |>
    dplyr::mutate(n_ID = n_distinct(sampling_unit_id)) |>
    dplyr::filter(n_ID >= 3) |>
    ungroup() |>
    group_by(year) |>
    filter(n() >= 30) |>
    ungroup() |>
    dplyr::filter(
      length1_cm > min_size,
      length1_cm <= max_size
    )

# Save prepped tip_spp_len ####
  saveRDS(
    tip_spp_len,
    file = here::here(
      "data",
      sedar,
      "rds",
      spp, 
      isl,
      paste0(
        isl, "_",
        spp, "_prep_keep_tip_",
        format(Sys.time(), "%Y%m%d"), ".rds"
      )
    )
  )
