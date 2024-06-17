# create interchangable script for yearly record comparisons

# Specify settings ####
# these created in historical_comparison_by_yr.qmd
hist_yr <- "pr_historical_2003.rds" 
tip_yr <- "com_tip_PR_2003.rds"
mis_dates <- "unique_dates_comparison_2003.rds"
year <- 2003

# set up ####
librarian::shelf(here, tidyverse, ROracle, keyring, dotenv, lubridate, flextable)

# Read in raw data ####
historic <- readRDS(here::here("data", "pr_historic", hist_yr))
oracle <- readRDS(here::here("data", "pr_historic", tip_yr))
unique_dates <- readRDS(here::here("data", "pr_historic", mis_dates))

# create needed functiosns ####
`%notin%` <- Negate(`%in%`)


# find dates where records are misaligned 
misaligned_dates <- unique_dates |> 
  filter(is.na(COMPARE) | COMPARE < 0 | COMPARE > 0)
unique(misaligned_dates$date)


# remove mismatched dates for easier organisation #### 
# replace NA's with 0's
# oracle
oracle_aligned <- oracle |> 
  filter(date %notin% misaligned_dates$date) 

# historical 
historic_aligned <- historic |> 
  filter(date %notin% misaligned_dates$date) 

# datasets should have same number of records now 


# input area names #### 
## create conversion table ####
#### NEED TO USE COUNTY CODE, NOT PLACE CODE, AS NOT ALL ORACLE HAS PLACE CODE AND 
# HISTORICAL HAS PLACE CODE NOT MATCHING ORACLE CODES ####
hist_muni_code <- 
  read_csv("data/CSVs/updated_tip_municodes_coordinates.csv") |> 
  janitor::clean_names()
hist_muni_code$place <- 
  tolower(hist_muni_code$place)

oracle_muni_code <- 
  read_csv("data/CSVs/updated_pr_place_codes_oracle.csv")|> 
  janitor::clean_names()|> 
  dplyr::rename(place = place_name)
oracle_muni_code$place <- 
  tolower(oracle_muni_code$place)

# join based off place name
place_code_conversion <-
  full_join(oracle_muni_code,
            hist_muni_code,
            by = "place") |>
  mutate(historic_code = replace_na(historic_code, 0))

# join based off county name
# place_code_conversion <-
#   full_join(oracle_muni_code,
#             hist_muni_code,
#             by = "place") |>
#   mutate(historic_code = replace_na(historic_code, 0))


## oracle ####
oracle_muni <- oracle_aligned %>%
  mutate(
    place_name =
      place_code_conversion$place[match(
        oracle_aligned$sample_area_place_code,
        place_code_conversion$place_id
      )]
  )
unique(oracle_muni$place_name)

## historical ####
historic_muni <- historic_aligned %>%
  mutate(
    hold_name =
      place_code_conversion$place[match(
        historic_aligned$areazip_kg,
        place_code_conversion$historic_code
      )]
  ) |> 
  mutate(place_name = case_when(is.na(hold_name) ~
                                  place_code_conversion$place[match(
                                    historic_aligned$areazip,
                                    place_code_conversion$historic_code )],
                                TRUE ~ hold_name))
  # exploration to see if the longer numbers were place codes that had been 
  # retroactivly input, change muni_name above to hold_name 
  # mutate(place_name = case_when(is.na(hold_name) ~
  #                                 place_code_conversion$place[match(
  #                                   historic_aligned$areazip,
  #                                   place_code_conversion$place_id )],
  #                               TRUE ~ hold_name))

unique(historic_muni$place_name)

# input county names ####
## oracle ####
oracle_muni <- oracle_aligned %>%
  mutate(
    place_name =
      place_code_conversion$place[match(
        oracle_aligned$sample_area_place_code,
        place_code_conversion$place_id
      )]
  )
unique(oracle_muni$place_name)

## historical ####
historic_muni <- historic_aligned %>%
  mutate(
    hold_name =
      place_code_conversion$place[match(
        historic_aligned$areazip_kg,
        place_code_conversion$historic_code
      )]
  ) |> 
  mutate(place_name = case_when(is.na(hold_name) ~
                                  place_code_conversion$place[match(
                                    historic_aligned$areazip,
                                    place_code_conversion$historic_code )],
                                TRUE ~ hold_name))

# input gear names #### 
## read in conversion table ####
gear_codes <- 
  read_csv("~/SEFSC-SFD-CFB-TIP-Compositions/data/CSVs/TIPS_GEAR_GROUPS.csv")|> 
  janitor::clean_names()

## oracle ####
oracle_gear <- oracle_muni %>%
  mutate(
    gear_name =
      gear_codes$standard_name[match(
        oracle_muni$gear_code,
        gear_codes$pr_gear
      )]
  )
unique(oracle_gear$gear_name)

## historical ####
historic_gear <- historic_muni %>%
  mutate(
    gear_name =
      gear_codes$standard_name[match(
        historic_muni$gearcode,
        gear_codes$pr_gear
      )]
  )
unique(historic_gear$gear_name)


# input species names ####
## read in conversion table ####
species_codes <- 
  read_csv("~/SEFSC-SFD-CFB-TIP-Compositions/data/CSVs/pr_species_codes_oracle_conv.csv")|> 
  janitor::clean_names()


## oracle ####
# clean species codes
oracle_gear$species_code <- 
  str_remove(oracle_gear$species_code, "^0+")

oracle_spp <- oracle_gear %>%
  mutate(
    spp_name =
      species_codes$name[match(
        oracle_gear$species_code,
        species_codes$standard_species_id
      )]
  )
unique(oracle_spp$spp_name)

## historical ####
historic_spp <- historic_gear %>%
  mutate(
    spp_name =
      species_codes$name[match(
        historic_gear$species_code,
        species_codes$id
      )]
  )
unique(historic_spp$spp_name)

# organize comparable variables ####
# in order of original comparison script
# date, area, gear, species, size

## oracle ####
oracle_reordered <- oracle_spp |> 
  select(id, date, place_name, gear_name, spp_name, length) |> 
  rename(place = place_name,
         gear = gear_name,
         spp = spp_name)

oracle_clean <- oracle_reordered %>%
  arrange(desc(date), place, gear, spp, length)

# create compare id to compare individual records
oracle_clean$compare_id <- seq.int(nrow(oracle_clean))

## historical ####
historic_reordered <- historic_spp |> 
  select(date, place_name, gear_name, spp_name, length)|> 
  rename(place = place_name,
         gear = gear_name,
         spp = spp_name)
# organize for best comparison results
hist_clean <- historic_reordered %>%
  arrange(desc(date), place, gear, spp, length)

# create compare id to compare individual records
hist_clean$compare_id <- seq.int(nrow(hist_clean))

# records that completely match ####
all_match <- semi_join(oracle_clean, hist_clean,
                       by = c("date", "place", "gear", "spp", "length"))

## remove previously matched records  ####
# for easier organisation 
# filter data sets to not those records 
## oracle ####
oracle_1 <- oracle_clean |> 
  filter(compare_id %notin% all_match$compare_id) |> 
  select(-compare_id)
# create new comparison id 
oracle_1$compare_id <- seq.int(nrow(oracle_1))


## historical  ####
hist_1 <- hist_clean |> 
  filter(compare_id %notin% all_match$compare_id)|> 
  select(-compare_id)
# create new comparison id 
hist_1$compare_id <- seq.int(nrow(hist_1))

