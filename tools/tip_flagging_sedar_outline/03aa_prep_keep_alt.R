# 03aa_kevin/larry_confidentiality 

# Load libraries ####
    librarian::shelf(here, tidyverse)

# Specify settings ####
# rds from end of 02aa script
    date <- "20241104" 
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
    end_yr <- 2023
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

# create confidentiality variable 
  # my way  
    tip_flag_k <- tip_spp_len |> 
      group_by(island, date, gear) |> 
      mutate(n_ID = n_distinct(sampling_unit_id)) |> 
      ungroup() |> 
      mutate(isl_yr_gear_c = 
               as.character(ifelse(n_ID < 2, "Y", "N")))
      
    tip_nonconf_k <- tip_flag_k |> 
      filter(isl_yr_gear_c == "N")
    # 2595 nonconf data for STT
    
  # adyans way  
    tip_count_a <- tip_spp_len |> 
      group_by(island, date, year, gear) |> 
      mutate(n_ID = n_distinct(sampling_unit_id)) |> 
      ungroup() |> 
      group_by(island, year, gear) |> 
      summarize(min_c = min(n_ID)) |> 
      filter(min_c < 2)
    
    tip_flag_a <- tip_count_a |> 
      mutate(isl_yr_gear_c = "TRUE") 
    
    tip_join_a <- right_join(tip_spp_len, tip_flag_a) 
    
    tip_truefalse_a <- tip_join_a |> 
      mutate(isl_yr_gear_c = 
               as.character(ifelse(is.na(isl_yr_gear_c), "FALSE", isl_yr_gear_c)))
             
    tip_nonconf_a <- tip_truefalse_a |> 
      filter(isl_yr_gear_c == "FALSE")
    # 0 nonconf data for STT 
    
    