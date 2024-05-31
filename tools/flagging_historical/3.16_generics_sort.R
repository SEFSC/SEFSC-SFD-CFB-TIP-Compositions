# Redo comparison of variables 
# point of pain: needing a id value 

# same setup as 3.15 to add variable names, create id value (compare_id) and organize
# set up ####
librarian::shelf(here, tidyverse, ROracle, keyring, dotenv, lubridate, flextable)
# load full formated dataset for oracle and historical
  load("~/SEFSC-SFD-CFB-TIP-Compositions/data/dataframes/pr_historical_1997.Rda")
  load("~/SEFSC-SFD-CFB-TIP-Compositions/data/dataframes/com_tip_PR_1997.Rda")
# load dataset of records with mismatched dates
  load("~/SEFSC-SFD-CFB-TIP-Compositions/data/dataframes/misaligned_dates_1997.Rda")
# create function 
  `%notin%` <- Negate(`%in%`)

# remove mismatched dates for easier organisation 
# filter datasets to not those dates 
# oracle
aligned_dates_oracle <- tip_ORGANIZED |> 
  filter(FINAL_DATE %notin% misaligned_dates$FINAL_DATE)

# historical 
aligned_dates_his <- pr_hist_date |> 
  filter(INTDATE %notin% misaligned_dates$FINAL_DATE)
# datasets should have same number of records now 

# input area names #### 
## create conversion table ####
updated_tip_municodes_coordinates <- 
  read_csv("data/CSVs/updated_tip_municodes_coordinates.csv")
updated_tip_municodes_coordinates$place <- 
  tolower(updated_tip_municodes_coordinates$place)
hist_muni_code <- updated_tip_municodes_coordinates

updated_pr_place_codes_oracle <- 
  read_csv("data/CSVs/updated_pr_place_codes_oracle.csv")
updated_pr_place_codes_oracle$PLACE_NAME <- 
  tolower(updated_pr_place_codes_oracle$PLACE_NAME)
oracle_muni_code <- updated_pr_place_codes_oracle |> 
  dplyr::rename(place = PLACE_NAME)

place_code_conversion <- 
  full_join(oracle_muni_code,
            hist_muni_code, 
            by = "place")
# relationship = "many-to-many" 

## oracle ####
aligned_dates_oracle_muni <- aligned_dates_oracle %>%
  mutate(
    place_name =
      place_code_conversion$place[match(
        aligned_dates_oracle$SAMPLE_AREA_PLACE_CODE,
        place_code_conversion$PLACE_ID
      )]
  )

## historical ####
aligned_dates_his_muni <- aligned_dates_his %>%
  mutate(
    hold_name =
      place_code_conversion$place[match(
        aligned_dates_his$AREAZIP,
        place_code_conversion$tip_code
      )]
  ) |> 
  # exploration to see if the longer numbers were place codes that had been 
  # retroactivly input, change muni_name above to hold_name 
  mutate(place_name = case_when(is.na(hold_name) ~
                                  place_code_conversion$place[match(
                                    aligned_dates_his$AREAZIP,
                                    place_code_conversion$PLACE_ID )],
                                TRUE ~ hold_name))

# input gear names #### 
## read in conversion table ####
gear_codes <- 
  read_csv("~/SEFSC-SFD-CFB-TIP-Compositions/data/CSVs/TIPS_GEAR_GROUPS.csv")

## oracle ####
aligned_dates_oracle_gear <- aligned_dates_oracle_muni %>%
  mutate(
    gear_name =
      gear_codes$STANDARD_NAME[match(
        aligned_dates_oracle_muni$GEAR_1,
        gear_codes$PR_GEAR
      )]
  )

## historical ####
aligned_dates_his_gear <- aligned_dates_his_muni %>%
  mutate(
    gear_name =
      gear_codes$STANDARD_NAME[match(
        aligned_dates_his_muni$GEARCODE,
        gear_codes$PR_GEAR
      )]
  )


# input species names ####
## read in conversion table ####
species_codes <- 
  read_csv("~/SEFSC-SFD-CFB-TIP-Compositions/data/CSVs/pr_species_codes_oracle_conv.csv")
tip_ORGANIZED$OBS_STANDARD_SPECIES_CODE <- 
  str_remove(tip_ORGANIZED$OBS_STANDARD_SPECIES_CODE, "^0+")#

## oracle ####
aligned_dates_oracle_spp <- aligned_dates_oracle_gear %>%
  mutate(
    spp_name =
      species_codes$NAME[match(
        aligned_dates_oracle_gear$OBS_STANDARD_SPECIES_CODE,
        species_codes$STANDARD_SPECIES_ID
      )]
  )

## historical ####
# if data 2000 to 2007, use SPECIES_B
aligned_dates_his_spp <- aligned_dates_his_gear %>%
  mutate(
    spp_name =
      species_codes$NAME[match(
        aligned_dates_his_gear$SPECIES,
        species_codes$ID
      )]
  )

# organize comparable variables ####
# in order of original comparison script
# date, area, gear, species, size
# historical length variable STLENGTH, update based on variable used

## oracle ####
reordered_oracle <- aligned_dates_oracle_spp |> 
  select(ID, FINAL_DATE, place_name, gear_name, spp_name, LENGTH1_MM) |> 
  rename(date = FINAL_DATE,
         place = place_name,
         gear = gear_name,
         spp = spp_name,
         length = LENGTH1_MM)

oracle_clean <- reordered_oracle %>%
  arrange(desc(date), place, gear, spp, length)

# create compare id to compare individual records
oracle_clean$compare_id <- seq.int(nrow(oracle_clean))

## historical ####
reordered_hist <- aligned_dates_his_spp |> 
  select(INTDATE, place_name, gear_name, spp_name, STLENGTH)|> 
  rename(date = INTDATE,
         place = place_name,
         gear = gear_name,
         spp = spp_name,
         length = STLENGTH)

hist_clean <- reordered_hist %>%
  arrange(desc(date), place, gear, spp, length)

# create compare id to compare individual records
hist_clean$compare_id <- seq.int(nrow(hist_clean))

# id records that completely match 
all_match <- semi_join(oracle_clean, hist_clean,
                       by = c("date", "place", "gear", "spp", "length"))

## remove previously matched records  ####
# for easier organisation 
# filter datasets to not those records 
## oracle ####
oracle_1 <- oracle_clean |> 
  filter(compare_id %notin% all_match$compare_id) |> 
  select(-compare_id)
oracle_1$compare_id <- seq.int(nrow(oracle_1))


## historical  ####
hist_1 <- hist_clean |> 
  filter(compare_id %notin% all_match$compare_id)|> 
  select(-compare_id)
hist_1$compare_id <- seq.int(nrow(hist_1))

records_compare_unmatched <-
  merge(oracle_1, hist_1, 
        by="compare_id")

# comparison of date and length ####
records_compare_unmatched <-
  merge(oracle_1, hist_1, 
        by="compare_id")

records_compare_lengths <-
  merge(oracle_1, hist_1, 
        by=c("compare_id", "length"))

# place is the cause of most of the mismatches in records 
# comparison of date, gear, and length, place and species may be mismatching 
records_compare_gearandlength <-
  merge(oracle_1, hist_1, 
        by=c("compare_id", "gear", "length"))

# comparison of date, gear, spp, and length, place may be mismatching 
records_compare_gearspplength <-
  merge(oracle_1, hist_1, 
        by=c("compare_id", "gear", "spp", "length"))

# remove records matched in previous step ####
## oracle ####
oracle_2 <- oracle_1 |> 
  filter(compare_id %notin% records_compare_gearspplength$compare_id) |> 
  select(-compare_id)
oracle_2$compare_id <- seq.int(nrow(oracle_2))
oracle_2_clean <- oracle_2 %>%
  arrange(desc(date), place, gear, spp, length)

## historical  ####
hist_2 <- hist_1 |> 
  filter(compare_id %notin% records_compare_gearspplength$compare_id)|> 
  select(-compare_id)
hist_2$compare_id <- seq.int(nrow(hist_2))

# comparison of reduced records #### 
records_compare_unmatched2 <-
  merge(oracle_2, hist_2, 
        by="compare_id")

records_compare_length2 <-
  merge(oracle_2, hist_2, 
        by=c("compare_id", "length"))

# mismatches are happening because some within the same date have place when others dont

# example
df4 <- bind_rows(
  anti_join(df1, df2, by = c("id", "col1", "col2")),
  anti_join(df2, df1, by = c("id", "col1", "col2"))
)