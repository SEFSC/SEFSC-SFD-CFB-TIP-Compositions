# Load libraries ####
librarian::shelf(here, tidyverse, measurements)

# if pulling new data from oracle ->
# cr_tip(state_codes = c("PR", "VI"))

# Specify settings ####
tip_rds <- "com_tip_PR_VI_20240703.RDS" # add pull of all CAR region and species
## Range currently set to not drop any obs 
# note 1984 is first full year for fish, 1980 for USVI and 1981 for PR spiny lobster 
min_year <- 1984
max_year <- 2022 

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
    ),
    # Simplify interview_date and create fork_length_cm and k
    id = as.character(id),
    interview_date = lubridate::as_date(interview_date),
    year = as.numeric(year),
    gear = case_when(
      land_standard_gear_name == "NOT CODED" ~ standardgearname_1,
      TRUE ~ land_standard_gear_name
    ),
    area_square = case_when(
      land_standard_area_name == "NA" ~ standardareaname_1,
      TRUE ~ land_standard_area_name
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
    ),
    # add state v federal denotion
    area = case_when(
      land_standard_gear_name == standardgearname_1 & 
        state_landed == "PUERTO RICO" & 
        distance_to_shore_miles_1 >= 9 ~ "federal",
      land_standard_gear_name == standardgearname_1 & 
        state_landed == "PUERTO RICO" & 
        distance_to_shore_miles_1 < 9 ~ "state",
      land_standard_gear_name == standardgearname_1 & 
        state_landed == "VIRGIN ISLANDS" & 
        distance_to_shore_miles_1 >= 3 ~ "federal",
      land_standard_gear_name == standardgearname_1 & 
        state_landed == "VIRGIN ISLANDS" & 
        distance_to_shore_miles_1 < 3 ~ "state",
      land_standard_gear_name == standardgearname_2 & 
        state_landed == "PUERTO RICO" & 
        distance_to_shore_miles_2 >= 9 ~ "federal",
      land_standard_gear_name == standardgearname_2 & 
        state_landed == "PUERTO RICO" & 
        distance_to_shore_miles_2 < 9 ~ "state",
      land_standard_gear_name == standardgearname_2 & 
        state_landed == "VIRGIN ISLANDS" & 
        distance_to_shore_miles_2 >= 3 ~ "federal",
      land_standard_gear_name == standardgearname_2 & 
        state_landed == "VIRGIN ISLANDS" & 
        distance_to_shore_miles_2 < 3 ~ "state",
      land_standard_gear_name == standardgearname_3 & 
        state_landed == "PUERTO RICO" & 
        distance_to_shore_miles_3 >= 9 ~ "federal",
      land_standard_gear_name == standardgearname_3 & 
        state_landed == "PUERTO RICO" & 
        distance_to_shore_miles_3 < 9 ~ "state",
      land_standard_gear_name == standardgearname_3 & 
        state_landed == "VIRGIN ISLANDS" & 
        distance_to_shore_miles_3 >= 3 ~ "federal",
      land_standard_gear_name == standardgearname_3 & 
        state_landed == "VIRGIN ISLANDS" & 
        distance_to_shore_miles_3 < 3 ~ "state",
      land_standard_gear_name == standardgearname_4 & 
        state_landed == "PUERTO RICO" & 
        distance_to_shore_miles_4 >= 9 ~ "federal",
      land_standard_gear_name == standardgearname_4 & 
        state_landed == "PUERTO RICO" & 
        distance_to_shore_miles_4 < 9 ~ "state",
      land_standard_gear_name == standardgearname_4 & 
        state_landed == "VIRGIN ISLANDS" & 
        distance_to_shore_miles_4 >= 3 ~ "federal",
      land_standard_gear_name == standardgearname_4 & 
        state_landed == "VIRGIN ISLANDS" & 
        distance_to_shore_miles_4 < 3 ~ "state",
      land_standard_gear_name == standardgearname_5 & 
        state_landed == "PUERTO RICO" & 
        distance_to_shore_miles_5 >= 9 ~ "federal",
      land_standard_gear_name == standardgearname_5 & 
        state_landed == "PUERTO RICO" & 
        distance_to_shore_miles_5 < 9 ~ "state",
      land_standard_gear_name == standardgearname_5 & 
        state_landed == "VIRGIN ISLANDS" & 
        distance_to_shore_miles_5 >= 3 ~ "federal",
      land_standard_gear_name == standardgearname_5 & 
        state_landed == "VIRGIN ISLANDS" & 
        distance_to_shore_miles_5 < 3 ~ "state",
      .default = "unknown"
    )
  ) |> 
  # Simplify variable names
  dplyr::rename(
    date = interview_date,
    species_code = obs_standard_species_code
  )|> 
  # filter to years 
  dplyr::filter(
    year < (max_year - 1) & year >= min_year
  )


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
    area, # state vs federal area
    area_square,# grid area when available
    species_code,
    sex_name, # male, female, unknown, not sexed
    gear, # gear from land_standard_gear_name subbed w/ gear_1 when unavailable
    length1_mm,
    length_unit1, # original length unit
    length_type1, # original length type
    length_type2, # for future comparisons
    obs_weight_kg,
    obs_weight_unit, # original weight unit
    sample_condition,
    sector,
    fishing_mode,
  ) 

# Save formatted tip_spp ####
saveRDS(
  tip_spp_relevant,
  file = here::here(
    "data",
    paste0( "format_tip_",
      format(Sys.time(), "%Y%m%d"), 
      ".rds"
    )
  )
)

# export data as csv if needed 
# write.csv(tip_spp_relevant, 
#           file = "data/CSVs/tip_rbr_04042024.csv", 
#           row.names=FALSE)
