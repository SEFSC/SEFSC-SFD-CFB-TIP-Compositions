# find records that are mismatching ####

# select records by date that are not aligned ####

# find dates where records are misaligned 
misaligned_dates <- unique_dates_comparison |> 
  filter(is.na(COMPARE) | COMPARE < 0 | COMPARE > 0)
unique(misaligned_dates$FINAL_DATE)
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

flextable(misaligned_muni_comparison) |>
  theme_box() %>%
  align(align = "center", part = "all") %>%
  fontsize(size = 8, part = "all") %>%
  autofit()

# total comparison - ensure there is the same difference
total_yr_muni_comparison <- muni_comparison |>
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
         