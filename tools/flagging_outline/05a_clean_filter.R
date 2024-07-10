#05a_clean
# filter to complete pairs

# Load libraries ####
librarian::shelf(here, tidyverse)

# Specify settings ####
# if k is being used to filter, specify this script
tip_spp_rds_k <- "pr_yts_spp_size_flag_20240618.rds" # rds from end of 02d script
# if k is not being used to filter, specify this script  
tip_spp_rds <- "pr_yts_prep_keep_tip_20240709.rds" # rds from end of 03a script
# gear list from correct time series and % representation
gear_list <- "pr_yts_clean_gear_list_20240328" # rds from end of 04a script
script <- "tip_spp_rds"
spp <- "yts"
isl <- "pr"
print_isl <- "Puerto Rico"
break_yr <- "2012"

# Read in formatted data ####
tip_spp <- readRDS(here::here("data", script))
glmm_gear <- readRDS(here::hear("data", gear_list))

# Using k: Prep data for analysis ####
tip_spp_len <- tip_spp |> 
  dplyr::filter(
    year >= break_yr, 
    k_flag == "keep",
    gear %in% glmm_gear$Gear
  )

# Not using k: Prep data for analysis ####
tip_spp_len <- tip_spp |> 
  dplyr::filter(
    year >= break_yr, 
    gear %in% glmm_gear$Gear
  )