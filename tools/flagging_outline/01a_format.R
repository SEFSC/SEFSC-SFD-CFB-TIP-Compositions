# Load libraries ####
librarian::shelf(here, tidyverse, measurements)

# Specify settings ####
tip_rds <- "com_tip_PR_VI_20230627.RDS" # add pull of all CAR region and species

# Read in raw data ####
tip <- readRDS(here::here("data", "raw", tip_rds))


# Prep raw data ####
tip_spp <- tip |>
  # Standardize variable format
  janitor::clean_names() |>
  # Create variable for island
  dplyr::mutate(
    data_source = "TIP",
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
  # dplyr::filter(
  #   island != "not coded"
    # obs_standard_species_code == spp_itis
  # ) |>
  # Simplify interview_date and create fork_length_cm and k
  dplyr::mutate(
    id = as.character(id),
    interview_date = lubridate::as_date(interview_date),
    year = as.numeric(year),
    # length1_cm = measurements::conv_unit(length1_mm, "mm", "cm"),
    # length1_inch = measurements::conv_unit(length1_mm, "mm", "inch"),
    # obs_weight_lbs = measurements::conv_unit(obs_weight_kg, "kg", "lbs"),
    # k = 10^5 * obs_weight_kg / length1_cm^3,
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
  ) |> 
  # Simplify variable names
  dplyr::rename(
    date = interview_date,
    species_code = obs_standard_species_code
  )

# Find first year and min/max lengths ####
min(tip_spp$year,na.rm = TRUE)

# Specify settings ####
## Range currently set to not drop any obs 
min_year <- 1979
max_year <- 2022 

# Select variables relevant to flagging investigation ####
tip_spp_relevant <- tip_spp |>
  select(
    data_source,
    id, # trip interview id
    date,
    year,
    island, # pr, stt, stx
    state_landed,
    county_landed,
    species_code,
    # length1_cm, # calculated from length1_mm
    # length1_inch, # calculated from length1_mm
    # obs_weight_lbs, # calculated from obs_weight
    # k, # calculated 01
    sex_name, # male, female, unknown, not sexed
    gear, # gear from land_standard_gear_name subbed w/ gear_1 when unavailable
    length1_mm,
    length_unit1, # original length unit
    length_type1, # original length type
    length_type2, 
    obs_weight_kg,
    obs_weight_unit, # original weight unit
    sample_condition,
    sector,
    fishing_mode,
  ) |> 
  dplyr::filter(
    year < 2022
  )

# Save formatted tip_spp ####
saveRDS(
  tip_spp_relevant,
  file = here::here(
    "data",
    paste0(
      isl, "_",
      spp, "_format_tip_",
      format(Sys.time(), "%Y%m%d"), ".rds"
    )
  )
)
