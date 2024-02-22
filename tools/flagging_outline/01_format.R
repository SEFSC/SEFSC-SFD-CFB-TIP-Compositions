# Load libraries ####
librarian::shelf(here, tidyverse, measurements)

# Specify settings ####
tip_rds <- "com_tip_PR_VI_170867_20240131.RDS"
spp_itis <- 170867
spp <- "slp"
isl <- "stx"

# Read in raw data ####
tip <- readRDS(here::here("data", "raw", tip_rds))

# Prep raw data ####
tip_spp <- tip |>
  # Standardize variable format
  janitor::clean_names() |>
  # Create variable for island
  dplyr::mutate(
    island = dplyr::case_when(
      state_landed == "PUERTO RICO" ~ "pr",
      state_landed == "VIRGIN ISLANDS" &
        county_landed %in% c("ST JOHN", "ST THOMAS") ~ "stt",
      state_landed == "VIRGIN ISLANDS" &
        county_landed == "ST CROIX" ~ "stx",
      .default = "not coded"
    )
  ) |>
  # Filter to island and species
  dplyr::filter(
    island == isl,
    obs_standard_species_code == spp_itis
  ) |>
  # Simplify interview_date and create fork_length_cm and k
  dplyr::mutate(
    interview_date = lubridate::as_date(interview_date),
    year = as.numeric(year),
    length1_cm = measurements::conv_unit(length1_mm, "mm", "cm"),
    length1_inch = measurements::conv_unit(length1_mm, "mm", "inch"),
    obs_weight_lbs = measurements::conv_unit(obs_weight_kg, "kg", "lbs"),
    k = 10^5 * obs_weight_kg / length1_cm^3
  )

# Save formatted tip_spp ####
saveRDS(
  tip_spp,
  file = here::here(
    "data",
    paste0(
      isl, "_",
      spp, "_format_tip_",
      format(Sys.time(), "%Y%m%d"), ".rds"
    )
  )
)

# Tabulate length types ####
length_types <- tip_spp |>
  dplyr::group_by(
    island,
    obs_standard_species_code,
    length_type1
  ) |>
  dplyr::summarize(
    .groups = "drop",
    n = dplyr::n(),
    percent = round(n / nrow(tip_spp) * 100, 1),
    first_year = min(year),
    last_year = max(year),
    n_years = dplyr::n_distinct(year),
    na_length1_cm = sum(is.na(length1_cm)),
    percent_na_length1_cm = round(na_length1_cm / n * 100, 1),
    na_obs_weight_kg = sum(is.na(obs_weight_kg)),
    percent_na_obs_weight_kg = round(na_obs_weight_kg / n * 100, 1),
  )

view(length_types)

# OBTAIN LOWER AND UPPER ESTIMATES OF K ####
tip_k_iqr <- IQR(tip_spp$k, na.rm = TRUE)
tip_k_25q <- quantile(tip_spp$k, 0.25, na.rm = TRUE)
tip_k_75q <- quantile(tip_spp$k, 0.75, na.rm = TRUE)
tip_k_lower <- tip_k_25q - 1.5 * tip_k_iqr
tip_k_upper <- tip_k_75q + 1.5 * tip_k_iqr

tip_k_iqr # 0.2531215
tip_k_25q # 2.159224
tip_k_75q # 2.412345
tip_k_lower # 1.779541
tip_k_upper # 2.792027