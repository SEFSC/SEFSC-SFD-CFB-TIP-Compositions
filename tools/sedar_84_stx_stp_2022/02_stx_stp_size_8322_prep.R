# Load libraries ####
librarian::shelf(here, tidyverse)

# Specify settings ####
tip_spp_rds <- "stx_slp_format_tip_20240123.rds"
len_type <- "FORK LENGTH"
len_mode <- "COMMERCIAL"
end_year <- 2022
spp <- "slp"
isl <- "stx"
ext_k_lower <- 1.71
ext_k_upper <- 2.68

# Read in formatted data ####
tip_spp <- readRDS(here::here("data", tip_spp_rds))

# Prep data for analysis ####
tip_spp_len <- tip_spp |>
  dplyr::filter(
    length_type1 == len_type,
    fishing_mode == len_mode,
    year <= end_year
  ) |>
  dplyr::mutate(
    k_keep = k >= ext_k_lower & k <= ext_k_upper
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

# Tabulate lengths ####
length_count <- tip_spp_len |>
  dplyr::group_by(
    county_landed,
    obs_standard_species_code,
    length_type1,
    fishing_mode
  ) |>
  dplyr::summarize(
    .groups = "drop",
    n = dplyr::n(),
    first_year = min(year),
    last_year = max(year),
    n_years = dplyr::n_distinct(year),
    min_length_cm = min(length1_cm),
    max_length_cm = max(length1_cm),
    avg_length_cm = round(mean(length1_cm), 2),
    na_length_cm = sum(is.na(length1_cm)),
    min_length_inch = round(min(length1_inch), 1),
    max_length_inch = round(max(length1_inch), 1),
    avg_length_in = round(mean(length1_inch), 1),
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
    n_k_keep = sum(k_keep == FALSE, na.rm = TRUE),
    percent_k_keep = round(n_k_keep / dplyr::n() * 100, 1)
  )

t_length_count <- t(length_count)
view(t_length_count)
