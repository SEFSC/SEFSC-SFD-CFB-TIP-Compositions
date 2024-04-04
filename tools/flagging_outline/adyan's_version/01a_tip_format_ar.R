# adyan's 

# Format TIP data ####
tip_r <- tip |>
  # Standardize variable format
  janitor::clean_names() |>
  # Create variable for data_source, island and simplify date and sector
  dplyr::mutate(
    data_source = "TIP",
    island = dplyr::case_when(
      state_landed == "PUERTO RICO" ~ "pr",
      state_landed == "VIRGIN ISLANDS" &
        county_landed %in% c("ST JOHN", "ST THOMAS") ~ "sttj",
      state_landed == "VIRGIN ISLANDS" &
        county_landed == "ST CROIX" ~ "stx",
      .default = "not coded",
    ),
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
    ),
    interview_date = lubridate::as_date(interview_date),
    year = as.numeric(year),
    obs_standard_species_code = as.numeric(obs_standard_species_code)
  ) |>
  # Simplify variable names
  dplyr::rename(
    date = interview_date,
    gear_name = land_standard_gear_name,
    species_code = obs_standard_species_code
  ) |>
  # Keep only variables of interest
  dplyr::select(
    data_source,
    island, state_landed, county_landed,
    sector, fishing_mode, gear_name,
    species_code,
    date, year, id,
    length1_mm, length_type1,
    obs_weight_kg
  )
