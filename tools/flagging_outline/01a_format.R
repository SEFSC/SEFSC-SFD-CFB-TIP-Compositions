# Load libraries ####
  librarian::shelf(here, tidyverse, measurements)

# if pulling new data from oracle ->
# cr_tip(state_codes = c("PR", "VI"))

# Specify settings ####
# specify date of extraction of all CAR region and species
  date <- "20241016" 
## Range currently set to not drop any obs 
# note 1984 is first full year for fish, 1980 for USVI and 1981 for PR spiny lobster 
  min_year <- 1977
  max_year <- 2023 
  sedar <- "sedar91"
  
# create folder structure for sedar overall data
  if (!dir.exists(here("data", sedar))){ dir.create(here("data", sedar)) }
  if (!dir.exists(here("data", sedar, "figure"))){ dir.create(here("data", sedar, "figure")) }
  if (!dir.exists(here("data", sedar, "rds"))){ dir.create(here("data", sedar, "rds")) }
  if (!dir.exists(here("data", sedar, "figure", "all"))){ dir.create(here("data", sedar, "figure", "all")) }

# Read in raw data ####
  tip_rds <- paste0("com_tip_PR_VI_", date, ".RDS" )
  tip <- readRDS(here::here("data", "raw", tip_rds))
  gear_nmfs <- read.csv(here::here("data", "CSVs", "gear_groups_nmfs.csv"))

# Prep raw data ####
  tip_spp <- tip |>
# Standardize variable format
    janitor::clean_names() |>
    dplyr::mutate(
# Simplify and standardize variable names
      sampling_program = "TIP",
      data_provider = "NMFS Miami",
      stock = "Caribbean",
      sampling_unit_id = as.character(id),
      specimen_id = sample_id, 
      interview_date = lubridate::as_date(interview_date),
      month = format(as.Date(interview_date,format="%Y-%m-%d"), format = "%m"),
      day = format(as.Date(interview_date,format="%Y-%m-%d"), format = "%d"),
      year = as.numeric(year), 
# Create island variable 
      island = dplyr::case_when(
        state_landed == "PUERTO RICO" ~ "pr",
        state_landed == "VIRGIN ISLANDS" &
          county_landed %in% c("ST JOHN", "ST THOMAS") ~ "stt",
        state_landed == "VIRGIN ISLANDS" &
          county_landed == "ST CROIX" ~ "stx",
        .default = "not coded"
      ),
# Create gear variable 
      gear = case_when(
        land_standard_gear_name == "NOT CODED" ~ standardgearname_1,
        TRUE ~ land_standard_gear_name
      ),
# Create area grid variable 
      area_square = case_when(
        land_standard_area_name == "NA" ~ standardareaname_1,
        TRUE ~ land_standard_area_name
      ),
# Create variable for fishery (rec or com)
      fishery = dplyr::case_when(
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
# Create state v federal variable 
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
      species_code = obs_standard_species_code, 
      species_name = obs_standard_species_name,
      fishery_mode = fishing_mode
    )|> 
# filter to years 
    dplyr::filter(
      year < (max_year+1) & year >= min_year
    )
  
# replace "," with ";"
  tip_spp$gear <- str_replace(tip_spp$gear, ",", ";")  
# check gears
  unique(tip_spp$gear)

# add nmfs gear groupings 
  tip_nmfs <- tip_spp |> 
    mutate(gear_group_code = gear_nmfs$gear_group[match(
                 tip_spp$gear,
                 gear_nmfs$gear
               )]
           )
  
# check for and convert 0's to NA in weight variables
  tip_range <- tip_nmfs[with(tip_nmfs, order(obs_weight_kg)), ]
  tip_range$obs_weight_kg[1:25]
  tip_nmfs$obs_weight_kg[tip_nmfs$obs_weight_kg == 0] <- NA
  tip_range <- tip_nmfs[with(tip_nmfs, order(obs_weight_kg)), ]
  tip_range$obs_weight_kg[1:25]
  
# check for and convert 0's to NA in length variables
  tip_range <- tip_nmfs[with(tip_nmfs, order(length1_mm)), ]
  tip_range$length1_mm[1:25]
  # tip_nmfs$length1_mm[tip_nmfs$length1_mm == 0] <- NA
  # tip_range <- tip_nmfs[with(tip_nmfs, order(length1_mm)), ]
  # tip_range$length1_mm[1:25]
  
  tip_range <- tip_nmfs[with(tip_nmfs, order(length2_mm)), ]
  tip_range$length2_mm[1:25]
  # tip_nmfs$length2_mm[tip_nmfs$length2_mm == 0] <- NA
  # tip_range <- tip_nmfs[with(tip_nmfs, order(length2_mm)), ]
  # tip_range$length2_mm[1:25]
  
# Select variables relevant to flagging investigation ####
  tip_spp_relevant <- tip_nmfs |>
    select(
      sampling_program, # denotes who recorded data
      data_provider, # noaa office handling data 
      stock, # region 
      sampling_unit_id, # trip interview id
      date, # interview month/day/year 
      month, # month of interview
      day, # day of interview
      year, # interview year
      island, # pr, stt, stx
      state_landed, # original state value
      county_landed, # original county value
      area, # state vs federal area
      area_square,# grid area when available
      species_code, # ITIS code of species 
      species_name, # common name of species given in TIP
      sex_name, # male, female, unknown, not sexed
      gear, # gear from land_standard_gear_name subbed w/ gear_1 when unavailable
      gear_group_code, # nmfs gear group code 
      length1_mm, # length1 converted by TIP system to mm 
      length_unit1, # original length unit
      length_type1, # original length type
      length2_mm, # for future comparisons
      length_type2, # for future comparisons
      obs_weight_kg, # weight converted to kg by TIP system
      obs_weight_unit, # original weight unit
      quantity, # number of individuals that were weighed/measured 
      sample_condition, # weight type
      fishery, # denotes recreational or commercial
      fishery_mode, # fishing_mode value
    ) 

# Save formatted tip_spp ####
  saveRDS(
    tip_spp_relevant,
    file = here::here(
      "data",
      sedar,
      "rds",
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
