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
gear_codes <- read_csv("~/SEFSC-SFD-CFB-TIP-Compositions/data/CSVs/TIPS_GEAR_GROUPS.csv")

# oracle
aligned_dates_oracle_gear <- aligned_dates_oracle_muni %>%
  mutate(
    GEAR_NAME =
      gear_codes$STANDARD_NAME[match(
        aligned_dates_oracle_muni$GEAR_1,
        gear_codes$PR_GEAR
      )]
  )

# historical 
aligned_dates_his_gear <- aligned_dates_his_muni %>%
  mutate(
    GEAR_NAME =
      gear_codes$STANDARD_NAME[match(
        aligned_dates_his_muni$GEARCODE,
        gear_codes$PR_GEAR
      )]
  )


# organize data sheets in order of original comparison script
# date, area, gear, species, size

grouped_oracle <- aligned_dates_oracle |> 
  group_by(FINAL_DATE, )


