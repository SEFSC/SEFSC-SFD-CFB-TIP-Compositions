# 03b_removed
# document all records removed throughout analysis 

# Load libraries ####
librarian::shelf(here, tidyverse)

# Specify settings ####
tip_spp_rds <- "pr_yts_spp_size_prep_20240529.rds" # rds from end of 02a script
tip_spp_rds_k <- "pr_yts_spp_size_flag_20240618.rds" # rds from end of 02d script
spp <- "yts"
isl <- "pr"
len_type <- "FORK LENGTH"
len_mode <- "COMMERCIAL"
# min_size <- 1 # add if there are outliers that need to be removed
# max_size <- 122
start_yr <- 1984 # based on 01a settings unless running truncated time series 
end_yr <- 2022
print_isl <- "Puerto Rico"


# Read in formatted data ####
tip_spp <- readRDS(here::here("data", tip_spp_rds))

# Create flag to denote removed records ####
tip_spp_flag <- tip_spp |>
  filter(island = isl) |> 
  dplyr::mutate(
    remove_flag = case_when(
      length_type1 != len_type ~ "drop",
      # length1_cm < min_size ~ "drop",
      # length1_cm > max_size ~ "drop",
      sector != len_mode ~ "drop",
      year < start_yr ~ "drop",
      year > end_yr ~ "drop",
      record_type == "incomplete" ~ "drop", 
      .default = "keep"
    )
  )

tip_settings_removed <- tip_spp_flag |> 
  filter(remove_flag == "drop")

# Read in k data ####
tip_spp_k <- readRDS(here::here("data", tip_spp_rds_k))

tip_settings_removed <- tip_spp_flag |> 
  filter(island = isl,
         remove_flag == "drop")