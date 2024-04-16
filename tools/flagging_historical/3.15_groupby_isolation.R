# try group_by strategy to align similar records

# set up ####
librarian::shelf(here, tidyverse, ROracle, keyring, dotenv, lubridate, flextable)
load("~/SEFSC-SFD-CFB-TIP-Compositions/data/dataframes/pr_historical_1997.Rda")
load("~/SEFSC-SFD-CFB-TIP-Compositions/data/dataframes/com_tip_PR_1997.Rda")
load("~/SEFSC-SFD-CFB-TIP-Compositions/data/dataframes/misaligned_dates_1997.Rda")
`%notin%` <- Negate(`%in%`)

# remove mismatched dates for easier organisation 
# filter datasets to not those dates 
# oracle
aligned_dates_oracle <- tip_ORGANIZED |> 
  filter(FINAL_DATE %notin% misaligned_dates$FINAL_DATE)

# historical 
aligned_dates_his <- pr_hist_date |> 
  filter(INTDATE %notin% misaligned_dates$FINAL_DATE)

# input area names #### 

# create conversion table 
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

# oracle
aligned_dates_oracle_muni <- aligned_dates_oracle %>%
  mutate(
    place_name =
      place_code_conversion$place[match(
        aligned_dates_oracle$SAMPLE_AREA_PLACE_CODE,
        place_code_conversion$PLACE_ID
      )]
  )

# historical
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

# read in conversion table
gear_codes <- 
  read_csv("~/SEFSC-SFD-CFB-TIP-Compositions/data/CSVs/TIPS_GEAR_GROUPS.csv")

# oracle
aligned_dates_oracle_gear <- aligned_dates_oracle_muni %>%
  mutate(
    gear_name =
      gear_codes$STANDARD_NAME[match(
        aligned_dates_oracle_muni$GEAR_1,
        gear_codes$PR_GEAR
      )]
  )

# historical 
aligned_dates_his_gear <- aligned_dates_his_muni %>%
  mutate(
    gear_name =
      gear_codes$STANDARD_NAME[match(
        aligned_dates_his_muni$GEARCODE,
        gear_codes$PR_GEAR
      )]
  )


# input species names ####

# read in conversion table
species_codes <- 
  read_csv("~/SEFSC-SFD-CFB-TIP-Compositions/data/CSVs/pr_species_codes_oracle_conv.csv")
tip_ORGANIZED$OBS_STANDARD_SPECIES_CODE <- 
  str_remove(tip_ORGANIZED$OBS_STANDARD_SPECIES_CODE, "^0+")#

# oracle
aligned_dates_oracle_spp <- aligned_dates_oracle_gear %>%
  mutate(
    spp_name =
      species_codes$NAME[match(
        aligned_dates_oracle_gear$OBS_STANDARD_SPECIES_CODE,
        species_codes$STANDARD_SPECIES_ID
      )]
  )

# historical
# if data 2000 to 2007, use SPECIES_B
aligned_dates_his_spp <- aligned_dates_his_gear %>%
  mutate(
    spp_name =
      species_codes$NAME[match(
        aligned_dates_his_gear$SPECIES,
        species_codes$ID
      )]
  )

# organize data sheets in order of original comparison script
# date, area, gear, species, size
# historical length variable STLENGTH, update based on variable used

# grouped_oracle <- aligned_dates_oracle_spp |> 
#   arrange(desc(LENGTH1_MM)) |> 
#   group_by(FINAL_DATE, place_name, gear_name, spp_name) |> 
#   arrange(desc(FINAL_DATE)) |> 
#   select(ID, FINAL_DATE, place_name, gear_name, spp_name, LENGTH1_MM) |> 
#   rename(date = FINAL_DATE,
#          place_o = place_name,
#          gear_o = gear_name,
#          spp_o = spp_name,
#          length_o = LENGTH1_MM)
# grouped_oracle$compare_id <- seq.int(nrow(grouped_oracle))
# 
# 
# grouped_hist <- aligned_dates_his_spp |> 
#   arrange(desc(STLENGTH)) |> 
#   group_by(INTDATE, place_name, gear_name, spp_name ) |> 
#   arrange(desc(INTDATE)) |> 
#   select(INTDATE, place_name, gear_name, spp_name, STLENGTH) |> 
#   rename(date = INTDATE,
#          place_h = place_name,
#          gear_h = gear_name,
#          spp_h = spp_name,
#          length_h = STLENGTH)
# grouped_hist$compare_id <- seq.int(nrow(grouped_hist))

# redo on reordering variables
# oracle
reordered_oracle <- aligned_dates_oracle_spp |> 
  select(ID, FINAL_DATE, place_name, gear_name, spp_name, LENGTH1_MM) |> 
  rename(date = FINAL_DATE,
         place_o = place_name,
         gear_o = gear_name,
         spp_o = spp_name,
         length_o = LENGTH1_MM)

oracle_clean <- reordered_oracle %>%
  arrange(desc(date), place_o, gear_o, spp_o, length_o)

oracle_clean$compare_id <- seq.int(nrow(oracle_clean))

# historical
reordered_hist <- aligned_dates_his_spp |> 
  select(INTDATE, place_name, gear_name, spp_name, STLENGTH)|> 
  rename(date = INTDATE,
         place_h = place_name,
         gear_h = gear_name,
         spp_h = spp_name,
         length_h = STLENGTH)

hist_clean <- reordered_hist %>%
  arrange(desc(date), place_h, gear_h, spp_h, length_h)

hist_clean$compare_id <- seq.int(nrow(hist_clean))

# compare records ####
records_compare_all <-
  merge(oracle_clean, hist_clean, by.x=c("compare_id", "place_o",
                                             "gear_o","spp_o","length_o"), 
        by.y=c("compare_id", "place_h",
               "gear_h","spp_h","length_h")) 


# remove matched records  for easier organisation 
# filter datasets to not those dates 
# oracle
round1_oracle <- oracle_clean |> 
  filter(compare_id %notin% records_compare_all$compare_id) |> 
  select(-compare_id)
round1_oracle$compare_id <- seq.int(nrow(round1_oracle))


# historical 
round1_historical <- hist_clean |> 
  filter(compare_id %notin% records_compare_all$compare_id)|> 
  select(-compare_id)
round1_historical$compare_id <- seq.int(nrow(round1_historical))


# these are the records that match except for location name
records_compare_area <-
  merge(round1_oracle, round1_historical, by.x=c("compare_id", 
                                             "gear_o","spp_o","length_o"), 
        by.y=c("compare_id", 
               "gear_h","spp_h","length_h")) 


# remove previously matched records for easier organisation 
# filter datasets to not those dates 
# oracle
round2_oracle <- round1_oracle |> 
  filter(compare_id %notin% records_compare_area$compare_id) |> 
  select(-compare_id)
round2_oracle$compare_id <- seq.int(nrow(round2_oracle))


# historical 
round2_historical <- round1_historical |> 
  filter(compare_id %notin% records_compare_area$compare_id)|> 
  select(-compare_id)
round2_historical$compare_id <- seq.int(nrow(round2_historical))

# these are the records that match except for gear name
records_compare_gear <-
  merge(round1_oracle, round1_historical, by.x=c("compare_id", 
                                                 "spp_o","length_o"), 
        by.y=c("compare_id", 
               "spp_h","length_h")) 
