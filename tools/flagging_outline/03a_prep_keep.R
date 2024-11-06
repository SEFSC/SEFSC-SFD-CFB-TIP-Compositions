# 03_prep

# filter the data to the correct sector, length measurements, and years

# Load libraries ####
  librarian::shelf(here, tidyverse)

# Specify settings ####
# rds from end of 02aa script
  date <- "20241104" 
  spp <- "csl"
# from here on chose one island to work with moving forward
  isl <- "pr" 
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
    conf <- tip_spp_len %>% 
      arrange(year, sampling_unit_id, gear) %>% 
      group_by(year, species_code, gear) %>% 
      mutate(n_ID_c = n_distinct(sampling_unit_id),
             # POUNDS_LANDED = sum(POUNDS_LANDED)
             ) %>% 
      group_by(year, sampling_unit_id,  gear) %>% 
      summarize(confidential_id = n_distinct(n_ID_c)) %>% 
      group_by(year, gear) %>% 
      summarize(confidential_id = sum(confidential_id)) %>% 
      mutate(is_confidential_id = as.character(ifelse(confidential_id < 3, "Y", "N"))) %>% 
      arrange(year) 
    
# create confidentiality variable  
    conf2 <- tip_spp_len %>%
      arrange(year, gear) %>%
      group_by(year,  gear) %>%
      mutate(n_c = n(),
      ) |> 
      select(year,gear, n_c) |>
      distinct() |> 
      # %>%
      # group_by(year, species_code, gear) %>%
      # summarize(confidential = n_distinct(n_c)) %>%
      group_by(year, gear) %>%
      summarize(confidential_n = sum(n_c)) %>% 
      mutate(is_confidential_n = as.character(ifelse(confidential_n < 30, "Y", "N"))) %>% 
      arrange(year) 
  
# join conf to tip
    tip_conf <- 
      left_join(tip_spp_len, conf) 
    tip_conf2 <-
      left_join(tip_conf, conf2)

# filter out conf data 
    tip_filtered <- tip_conf2 |> 
      filter(is_confidential_id == "N" & is_confidential_n == "N")
      
# Save prepped tip_spp_len ####
  saveRDS(
    tip_filtered,
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
