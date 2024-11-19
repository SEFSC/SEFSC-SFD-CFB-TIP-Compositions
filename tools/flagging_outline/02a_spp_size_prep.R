# 02a_spp_size_prep
# filter to species
# calculate table of length stats
# calculate k

# Load libraries ####
  librarian::shelf(
    here, tidyverse, janitor, flextable, ggplot2
  )

# Specify settings ####
# rds from end of 01a script
  date <- "20241118" 
# find on itis.gov - all spp itis codes that could be assiciated with target species
  spp_itis <- c("097648", "097646", "097651") 
  spp <- "csl"
  isl <- c("pr", "stt", "stx")
  print_spp <- "Caribbean Spiny Lobster"
  print_isl <- "Puerto Rico - USVI"
  save_spp <- "csl"
  save_isl <- "prusvi"
# folder name 
  sedar <- "sedar91"
  
# create folder structure for sedar overall data
  if (!dir.exists(here("data", sedar, "figure", spp))){ dir.create(here("data", sedar, "figure", spp)) }
  if (!dir.exists(here("data", sedar, "rds", spp))){ dir.create(here("data", sedar, "rds", spp)) }
  if (!dir.exists(here("data", sedar, "figure", spp, "all"))){ dir.create(here("data", sedar, "figure", spp, "all")) }
  if (!dir.exists(here("data", sedar, "rds", spp, "all"))){ dir.create(here("data", sedar, "rds", spp, "all")) }

# Read in formatted data ####
  tip_spp_rds <- paste0("format_tip_", date, ".rds" )
  tip_spp <- readRDS(here::here("data", sedar, "rds", tip_spp_rds))

# Filter to target species ####
  tip_spp_prep <- tip_spp |>
    dplyr::filter(species_code %in% spp_itis)

# Convert units and calculate k ####
  spp_size_calc <- tip_spp_prep |>
    dplyr::mutate(
      length1_cm = measurements::conv_unit(length1_mm, "mm", "cm"),
      length1_inch = measurements::conv_unit(length1_mm, "mm", "inch"),
      obs_weight_lbs = measurements::conv_unit(obs_weight_kg, "kg", "lbs"),
      k = 10^5 * obs_weight_kg / length1_cm^3,
      record_type = case_when(
        !is.na(k) ~ "complete",
        .default = "incomplete"
      )
    )

# Find upper and lower estimates of k ####
# dont need k for lobster  
  tip_k_iqr <- IQR(spp_size_calc$k, na.rm = TRUE)
  tip_k_25q <- quantile(spp_size_calc$k, 0.25, na.rm = TRUE)
  tip_k_75q <- quantile(spp_size_calc$k, 0.75, na.rm = TRUE)
  tip_k_lower <- tip_k_25q - 1.5 * tip_k_iqr
  tip_k_upper <- tip_k_75q + 1.5 * tip_k_iqr

# compare type1 and type2 lengths ####
  table(tip_spp_prep$length_type1, useNA = "always")
  table(tip_spp_prep$length_type2, useNA = "always")
  table(tip_spp_prep$length_type1,tip_spp_prep$length_type2)
  
# compare type1 and type2 gears ####
  table(tip_spp_prep$gear, useNA = "always")
  table(tip_spp_prep$standardgearname_2, useNA = "always")
  table(tip_spp_prep$gear,tip_spp_prep$standardgearname_2 )
  
# look at min and max standardized lengths ####
### use this as preliminary look at possible inaccurate lengths
  min(spp_size_calc$length1_cm, na.rm = TRUE)
  max(spp_size_calc$length1_cm, na.rm = TRUE)

# view largest 25 length values (cm)
  tip_range <- spp_size_calc[with(spp_size_calc, order(-length1_cm)), ]
  tip_range$length1_cm[1:25]
  
# view largest 25 length values (in)
  tip_range11 <- spp_size_calc[with(spp_size_calc, order(-length1_inch)), ]
  tip_range11$length1_inch[1:25]   
  
# view largest 50 length values (in)
  tip_range12 <- spp_size_calc[with(spp_size_calc, order(-length1_inch)), ]
  tip_range12$length1_inch[1:50] 

# view smallest 25 length values (cm)
  tip_range2 <- spp_size_calc[with(spp_size_calc, order(length1_cm)), ]
  tip_range2$length1_cm[1:25]
  
# view smallest 25 length values (in)
  tip_range3 <- spp_size_calc[with(spp_size_calc, order(length1_inch)), ]
  tip_range3$length1_inch[1:25]
  
# view smallest 50 length values (in)
  tip_range4 <- spp_size_calc[with(spp_size_calc, order(length1_inch)), ]
  tip_range4$length1_inch[1:50]
  
# Tabulate lengths, weights, and k stats ####
    # when table returns "inf" or "-inf" it means the calculation 
    # had no non-zero values to calculate
  length_count <- spp_size_calc |>
    dplyr::group_by(
      island,
      species_code,
      length_type1,
      fishery
    ) |>
    dplyr::summarize(
      .groups = "drop",
      n = dplyr::n(),
      percent_tip = round(n / nrow(tip_spp) * 100, 1),
      first_year = min(year),
      last_year = max(year),
      n_years = dplyr::n_distinct(year),
      na_length1_cm = sum(is.na(length1_cm)),
      percent_na_length1_cm = round(na_length1_cm / n * 100, 1),
      na_obs_weight_kg = sum(is.na(obs_weight_kg)),
      percent_na_obs_weight_kg = round(na_obs_weight_kg / n * 100, 1),
      min_length_cm = min(length1_cm, na.rm = TRUE),
      max_length_cm = max(length1_cm, na.rm = TRUE),
      avg_length_cm = round(mean(length1_cm, na.rm = TRUE), 2),
      na_length_cm = sum(is.na(length1_cm)),
      min_length_inch = round(min(length1_inch, na.rm = TRUE), 1),
      max_length_inch = round(max(length1_inch, na.rm = TRUE), 1),
      avg_length_in = round(mean(length1_inch, na.rm = TRUE), 1),
      min_weight_kg = min(obs_weight_kg, na.rm = TRUE),
      max_weight_kg = max(obs_weight_kg, na.rm = TRUE),
      avg_weight_kg = round(mean(obs_weight_kg, na.rm = TRUE), 2),
      na_weight_kg = sum(is.na(obs_weight_kg)),
      min_weight_lbs = round(min(obs_weight_lbs, na.rm = TRUE), 1),
      max_weight_lbs = round(min(obs_weight_lbs, na.rm = TRUE), 1),
      avg_weight_lbs = round(min(obs_weight_lbs, na.rm = TRUE), 1),
      min_k_cm = round(min(k, na.rm = TRUE), 2),
      max_k_cm = round(max(k, na.rm = TRUE), 2),
      avg_k_cm = round(mean(k, na.rm = TRUE), 2),
      # n_k_keep = sum(k_keep == FALSE, na.rm = TRUE),
      # percent_k_keep = round(n_k_keep / dplyr::n() * 100, 1)
    )

# create formated table of length, weight, and k stats
  len_count_tbl <- flextable(length_count) |>
    theme_box() %>%
    align(align = "center", part = "all") %>%
    fontsize(size = 8, part = "all") %>%
    autofit() |>
    colformat_num(j = c("first_year", "last_year"), big.mark = "")

# view 
  len_count_tbl
# save  
  save_as_image(x = len_count_tbl, path =  
                  here::here("data", sedar, "figure", spp, "all", "length_count_tbl.png"))  

# Tabulate weight types ####
  weight_types <- spp_size_calc |>
    dplyr::group_by(
      island,
      species_code,
      condition_type,
      # sample_condition
    ) |>
    dplyr::summarize(
      .groups = "drop",
      n = dplyr::n(),
      percent_tip = round(n / nrow(tip_spp) * 100, 1),
      percent_spp = round(n / nrow(tip_spp_prep) * 100, 1),
      na_weight_kg = sum(is.na(obs_weight_kg)),
      percent_na_weight_kg = round(na_weight_kg / n * 100, 1),
      first_year = min(year),
      last_year = max(year),
      n_years = dplyr::n_distinct(year),
      min_weight_kg = min(obs_weight_kg, na.rm = TRUE),
      max_weight_kg = max(obs_weight_kg, na.rm = TRUE),
      avg_weight_kg = round(mean(obs_weight_kg, na.rm = TRUE), 2),
      na_weight_kg = sum(is.na(obs_weight_kg)),
      min_weight_lbs = round(min(obs_weight_lbs, na.rm = TRUE), 1),
      max_weight_lbs = round(min(obs_weight_lbs, na.rm = TRUE), 1),
      avg_weight_lbs = round(min(obs_weight_lbs, na.rm = TRUE), 1),
    )

# create formated table of weight types 
  weight_type_tbl <- flextable(weight_types) |>
    theme_box() %>%
    align(align = "center", part = "all") %>%
    fontsize(size = 8, part = "all") %>%
    autofit() |>
    colformat_num(j = c("first_year", "last_year"), big.mark = "")
# view 
  weight_type_tbl
# save  
  save_as_image(x = weight_type_tbl, path =  
                  here::here("data", sedar, "figure", spp, "all", "weight_type_tbl.png"))  
  
# filter to final spp codes that we want to continue with 
# specifiy ITIS codes
  spp_itis_final <- c("097648", "097646")
# filter to species code                
  spp_filtered <- spp_size_calc |>
    dplyr::filter(species_code %in% spp_itis_final)
  
# Save formatted tip_spp ####
  saveRDS(
    spp_filtered,
    file = here::here(
      "data",
      sedar,
      "rds",
      spp, 
      "all",
      paste0(
        save_isl, "_",
        save_spp, "_spp_size_prep_",
        format(Sys.time(), "%Y%m%d"),
        ".rds"
      )
    )
  )
