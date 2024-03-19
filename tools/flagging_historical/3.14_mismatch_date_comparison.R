# find records that are mismatching ####
yr = 1997

# select records by date that are not aligned ####
librarian::shelf(here, tidyverse, ROracle, keyring, dotenv, lubridate, flextable)
load("~/SEFSC-SFD-CFB-TIP-Compositions/data/dataframes/unique_dates_comparison_1997.Rda")
load("~/SEFSC-SFD-CFB-TIP-Compositions/data/dataframes/pr_historical_1997.Rda")
load("~/SEFSC-SFD-CFB-TIP-Compositions/data/dataframes/com_tip_PR_1997.Rda")

# find dates where records are misaligned 
misaligned_dates <- unique_dates_comparison |> 
  filter(is.na(COMPARE) | COMPARE < 0 | COMPARE > 0)
unique(misaligned_dates$FINAL_DATE)

# file is saved in data folder
save(misaligned_dates,
     file = here::here(
       "data", "dataframes",
       paste0(
         "misaligned_dates",
         "_",
         yr,
         ".Rda"
       )
     )
)

# filter datasets to those dates 
# oracle
misaligned_dates_oracle <- tip_ORGANIZED |> 
  filter(FINAL_DATE %in% misaligned_dates$FINAL_DATE)

# historical 
misaligned_dates_his <- pr_hist_date |> 
  filter(INTDATE %in% misaligned_dates$FINAL_DATE)

# redo do area, gear, species comparison ####

## area ####
# Create new MUNI(county code) conversion table
# condensed_tip_muni <- read_csv("~/SEFSC-SFD-CFB-TIP-Compositions/data/CSVs/condensed_tip_muni.csv")
# condensed_tip_muni$muni <- tolower(condensed_tip_muni$muni)

# try again with updated csv and place codes ####

# tip_municodes_coordinates <- read_csv("data/CSVs/tip_municodes_coordinates.csv")
# tip_municodes_coordinates$site_name <- tolower(tip_municodes_coordinates$site_name)
# hist_muni_code <- tip_municodes_coordinates |> 
#   dplyr::rename(place = site_name)
updated_tip_municodes_coordinates <- read_csv("data/CSVs/updated_tip_municodes_coordinates.csv")
updated_tip_municodes_coordinates$place <- tolower(updated_tip_municodes_coordinates$place)
hist_muni_code <- updated_tip_municodes_coordinates

# pr_place_codes_oracle <- read_csv("data/CSVs/pr_place_codes_oracle.csv")
# pr_place_codes_oracle$CNTY_ID <- 
#   str_remove(pr_place_codes_oracle$CNTY_ID, "^0+")
# pr_place_codes_oracle[1, 3] <- "0"
# pr_place_codes_oracle[2, 3] <- "0"
# pr_place_codes_oracle[3, 3] <- "0"
# pr_place_codes_oracle$CNTY_NAME <- tolower(pr_place_codes_oracle$CNTY_NAME)
# pr_place_codes_oracle$PLACE_NAME <- tolower(pr_place_codes_oracle$PLACE_NAME)
# oracle_muni_code <- pr_place_codes_oracle |> 
#   dplyr::rename(place = PLACE_NAME)

updated_pr_place_codes_oracle <- read_csv("data/CSVs/updated_pr_place_codes_oracle.csv")
updated_pr_place_codes_oracle$PLACE_NAME <- tolower(updated_pr_place_codes_oracle$PLACE_NAME)
oracle_muni_code <- updated_pr_place_codes_oracle |> 
  dplyr::rename(place = PLACE_NAME)

place_code_conversion <- 
  full_join(oracle_muni_code,
            hist_muni_code, 
            by = "place")
            # relationship = "many-to-many"

# unique MUNI CODES and number of occurrences - ORACLE 
misaligned_dates_oracle_UNIQUE_MUNI <- misaligned_dates_oracle %>%
  group_by(SAMPLE_AREA_PLACE_CODE) %>%
  summarize(count = n())
misaligned_dates_oracle_UNIQUE_MUNI_updated <- misaligned_dates_oracle_UNIQUE_MUNI %>%
  rename(COUNT_ORACLE = count)
misaligned_dates_oracle_UNIQUE_MUNI_names <- misaligned_dates_oracle_UNIQUE_MUNI_updated %>%
  mutate(
    place_name =
      place_code_conversion$place[match(
        misaligned_dates_oracle_UNIQUE_MUNI_updated$SAMPLE_AREA_PLACE_CODE,
        place_code_conversion$PLACE_ID
      )]
  )

# unique dates and number of occurrences- HISTORICAL
misaligned_dates_his_unique_MUNI <- misaligned_dates_his %>%
  group_by(AREAZIP) %>%
  summarize(count = n())
misaligned_dates_his_MUNI_updated <- misaligned_dates_his_unique_MUNI %>%
  rename(COUNT_HISTORICAL = count)
# pr_97_sp88_unique_MUNI_updated$muni_zip_HISTORICAL = as.numeric(as.character(pr_97_sp88_unique_MUNI_updated$muni_zip_HISTORICAL))
misaligned_dates_his_MUNI_names <- misaligned_dates_his_MUNI_updated %>%
  mutate(
    hold_name =
      place_code_conversion$place[match(
        misaligned_dates_his_MUNI_updated$AREAZIP,
        place_code_conversion$tip_code
      )]
  ) |> 
# exploration to see if the longer numbers were place codes that had been 
# retroactivly input, change muni_name above to hold_name 
  mutate(place_name = case_when(is.na(hold_name) ~
                                  place_code_conversion$place[match(
                                   misaligned_dates_his_MUNI_updated$AREAZIP,
                                   place_code_conversion$PLACE_ID )],
                               TRUE ~ hold_name))


# merge tables
misaligned_unique_place_merge <- 
  full_join(misaligned_dates_oracle_UNIQUE_MUNI_names,
            misaligned_dates_his_MUNI_names, 
            by = "place_name")

# COMPARE
misaligned_place_comparison <- misaligned_unique_place_merge %>%
  mutate(COMPARE = (COUNT_ORACLE - COUNT_HISTORICAL))

flextable(misaligned_place_comparison) |>
  theme_box() %>%
  align(align = "center", part = "all") %>%
  fontsize(size = 8, part = "all") %>%
  autofit()

# total comparison - ensure there is the same difference
total_yr_muni_comparison <- misaligned_place_comparison |>
  summarize(
    total_tip = sum(COUNT_ORACLE, na.rm = TRUE),
    total_historical = sum(COUNT_HISTORICAL, na.rm = TRUE)
  ) |>
  mutate(comparison = total_tip - total_historical)

flextable(total_yr_muni_comparison) |>
  theme_box() %>%
  align(align = "center", part = "all") %>%
  fontsize(size = 8, part = "all") %>%
  autofit()

# GEAR COMPARISON #### 

# read in conversion table
gear_codes <- read_csv("~/SEFSC-SFD-CFB-TIP-Compositions/data/CSVs/TIPS_GEAR_GROUPS.csv")

# unique GEAR CODES and number of occurrences - ORACLE -
# use GEAR_1 because those are the island specific codes and will match with
# those used in historic
mis_tip_unique_GEAR <- misaligned_dates_oracle %>%
  group_by(GEAR_1) %>%
  summarize(count = n())
mis_tip_unique_GEAR_updated <- mis_tip_unique_GEAR %>%
  rename(
    COUNT_ORACLE = count,
    PR_GEAR = GEAR_1
  )
# com_PR_lob_unique_MUNI_updated$muni_zip_ORACLE = as.numeric(as.character(com_PR_lob_unique_MUNI_updated$muni_zip_ORACLE))
mis_tip_unique_GEAR_NAMES <- mis_tip_unique_GEAR_updated %>%
  mutate(
    GEAR_NAME =
      gear_codes$STANDARD_NAME[match(
        mis_tip_unique_GEAR_updated$PR_GEAR,
        gear_codes$PR_GEAR
      )]
  )


# unique dates and number of occurrences- HISTORICAL- 13 UNIQUE MUNI_ZIP
mis_pr_hist_unique_GEAR <- misaligned_dates_his %>%
  group_by(GEARCODE) %>%
  summarize(count = n())
mis_pr_hist_unique_GEAR_UPDATED <- mis_pr_hist_unique_GEAR %>%
  rename(
    COUNT_HISTORICAL = count,
    PR_GEAR = GEARCODE
  )

mis_pr_hist_unique_GEAR_NAMES <- mis_pr_hist_unique_GEAR_UPDATED %>%
  mutate(
    GEAR_NAME =
      gear_codes$STANDARD_NAME[match(
        mis_pr_hist_unique_GEAR_UPDATED$PR_GEAR,
        gear_codes$PR_GEAR
      )]
  )

# merge tables
mis_unique_GEAR_merge_PR <-
  merge(mis_tip_unique_GEAR_NAMES,
        mis_pr_hist_unique_GEAR_NAMES,
        by = "GEAR_NAME",
        all = TRUE
  )

# COMPARE
mis_GEAR_comparison <- mis_unique_GEAR_merge_PR %>%
  mutate(COMPARE = (COUNT_ORACLE - COUNT_HISTORICAL))

flextable(mis_GEAR_comparison) |>
  theme_box() %>%
  align(align = "center", part = "all") %>%
  fontsize(size = 8, part = "all") %>%
  autofit()

# total comparison - ensure there is the same difference
mis_total_yr_gear_comparison <- mis_GEAR_comparison |>
  summarize(
    total_tip = sum(COUNT_ORACLE, na.rm = TRUE),
    total_historical = sum(COUNT_HISTORICAL, na.rm = TRUE)
  ) |>
  mutate(comparison = total_tip - total_historical)

flextable(mis_total_yr_gear_comparison) |>
  theme_box() %>%
  align(align = "center", part = "all") %>%
  fontsize(size = 8, part = "all") %>%
  autofit()

# try group by strategy 


         