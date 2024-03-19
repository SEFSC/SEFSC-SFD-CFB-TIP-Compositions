# Load libraries ####
librarian::shelf(here, tidyverse, measurements)

# Specify settings ####
tip_rds <- "com_tip_PR_VI_168907_20240227.RDS" # add pull of correct species and region
spp_itis <- 168907
spp <- "yts"
isl <- "pr"

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
    id = as.character(id),
    interview_date = lubridate::as_date(interview_date),
    year = as.numeric(year),
    length1_cm = measurements::conv_unit(length1_mm, "mm", "cm"),
    length1_inch = measurements::conv_unit(length1_mm, "mm", "inch"),
    obs_weight_lbs = measurements::conv_unit(obs_weight_kg, "kg", "lbs"),
    k = 10^5 * obs_weight_kg / length1_cm^3,
    gear = case_when(
      land_standard_gear_name == "NOT CODED" ~ gearname_1,
      TRUE ~ land_standard_gear_name
    ),
    # Create variable for sector (rec or com)
    sector = dplyr::case_when(
      fishing_mode %in% c(
        "HEADBOAT",
        "PARTY/CHARTER",
        "PRIVATE RECREATIONAL",
        "TOURNAMENT"
      ) ~
        "RECREATIONAL",
      is.na(fishing_mode) ~ "UNKNOWN",
      .default = fishing_mode
    )
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

