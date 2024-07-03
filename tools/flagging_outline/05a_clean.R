#05a_clean
# filter to complete pairs

# Load libraries ####
librarian::shelf(here, tidyverse)

# Specify settings ####
# if you want just complete l/w pairs use this one
tip_spp_rds <- "pr_yts_spp_size_flag_20240618.rds" # rds from end of 02d script
# if you want all lengths regardless of weights use this one 
tip_spp_rds <- "pr_yts_glmm_analysis_20240529.rds" # rds from end of 04a script
spp <- "yts"
isl <- "pr"
print_isl <- "Puerto Rico"

# Read in formatted data ####
tip_spp <- readRDS(here::here("data", tip_spp_rds))

# Prep data for analysis ####
tip_spp_len <- tip_spp |> 
  dplyr::filter(
    k_flag == "keep"
  )