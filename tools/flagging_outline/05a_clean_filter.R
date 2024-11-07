#05a_clean
# final filtering to break year, k-keep records, and gears >2% representation 

# Load libraries ####
  librarian::shelf(here, tidyverse)

# Specify settings ####
# if k is being used to filter, specify this script
  # tip_spp_rds <- "prusvi_csl_spp_size_flag_20240711.rds" # rds from end of 02d script
# if k is not being used to filter, specify this script  
# date from end of 03a script
  date <- "20241106" 
# 2% gear list from correct time series 
# date from end of 04a script
  # gear_date <- "20241105" 
  spp <- "csl"
  isl <- "stt"
  break_yr <- "2012"
  sedar <- "sedar91"

# reminder: variables that have already been filtered for in 03a - 
# Island, Data_source, Len_mode, Len_type, Year, Min and max size, 
# Gears  with > 3 unique trip ids, Years with >= 30 length samples

# Read in formatted data ####
  tip_spp_rds <- paste0(isl, "_", spp, "_prep_keep_tip_", date, ".rds" )
  tip_spp <- readRDS(here::here("data", sedar, "rds", spp, isl, tip_spp_rds))
  # 
  # gear_list <- paste0(isl, "_", spp, "_clean_gear_list_", gear_date, ".rds" )
  # glmm_gear <- readRDS(here::here("data", sedar, "rds", spp, isl, gear_list))

# Not using k: Prep data for analysis ####
  tip_spp_len <- tip_spp |> 
    dplyr::filter(
  # if there is a break year, use this line
      # year >= break_yr, 
  # filter to gears representing more than 2% individually of length records 
      # gear %in% glmm_gear$Gear
    )

# # Using k: Prep data for analysis ####
  # tip_spp_len <- tip_spp |> 
  #   dplyr::filter(
  #  # if there is a break year, use this line
  #     year >= break_yr, 
  # # filter to gears representing more than 2% individually of length records 
  #     gear %in% glmm_gear$Gear,
  # # filter to records with k values within acceptable margins 
  #     k_flag == "keep",
  #   )

# Save cleaned tip_clean ##### 
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
        spp, "_clean_filtered_",
        format(Sys.time(), "%Y%m%d"), ".rds"
      )
    )
  )
