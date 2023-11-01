
# extract oracle by yr and state: 

cr_tip_yr(state_codes = 'PR', year = "2003")

# load ORACLE 
com_tip_PR_2003 <- readRDS("~/SEFSC-SFD-CFB-TIP-Compositions/data/raw/com_tip_PR_2003_20231020.RDS")

# Create a comparable table 
com_tip_PR_2003_skeleton <- com_tip_PR_2003 %>%  # select comparable variables 
  select(ID, INTERVIEW_DATE, YEAR, REPORTING_AREA_ZIP, SAMPLE_AREA_STATE_CODE, SAMPLE_AREA_COUNTY_CODE,
         SAMPLE_AREA_ZIP, LANDING_AREA_PLACE_CODE, LANDING_AREA_COUNTY_CODE, SAMPLE_AREA_PLACE_CODE, 
         GEAR_1, GEARNAME_1, GEAR_QTY_1, GEAR_FREQUENCY1, MIN_DEPTH1, MAX_DEPTH1, 
         OBS_STANDARD_SPECIES_CODE, OBS_STANDARD_SPECIES_NAME, TRIP_DAYS_FISHED, 
         LENGTH1, LENGTH_UNIT1, LENGTH_UNIT_CODE1, LENGTH_TYPE1, LENGTH_TYPE_CODE1, 
         LENGTH1_MM, OBS_WEIGHT, OBS_WEIGHT_KG, OBS_WEIGHT_UNIT, OBS_WEIGHT_UNIT_ID)

# create new filterable date value
com_tip_PR_2003_NEWDATE <- com_tip_PR_2003_skeleton %>%
  mutate(TEST_DATE = as.Date(ymd_hms(INTERVIEW_DATE)),
         FINAL_DATE = case_when(is.na(TEST_DATE) ~ INTERVIEW_DATE, 
                                TRUE ~ TEST_DATE))

com_tip_PR_2003_ORGANIZED <- com_tip_PR_2003_NEWDATE[,c(1,2,3,31, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30)]

save(com_tip_PR_2003_ORGANIZED,file="data/dataframes/com_tip_PR_2003_ORGANIZED.Rda") # file is saved in data folder  

# Load HISTORICAL data 
PR_historical_03 <- read.csv("data/raw/for_import_03.csv")

# Format dates to be mm/dd/yyyy 
pr_03 <- PR_historical_03 %>%
  # Format dates to be mm/dd/yyyy 
  mutate(INTDATE = as.Date(INTDATE, "%m/%d/%Y"))

save(pr_03,file="data/dataframes/pr_03.Rda") # file is saved in data folder

# DATE COMPARISON ####

load(file = "data/dataframes/pr_03.Rda")
load(file = "data/dataframes/com_tip_PR_2003_ORGANIZED.Rda")

# ORACLE unique dates and number of occurrences 
com_tip_PR_2003_UNIQUEDATES <- com_tip_PR_2003_ORGANIZED %>% 
  group_by(FINAL_DATE) %>% 
  summarize(count=n())
com_tip_PR_2003_UNIQUEDATES_updated <- com_tip_PR_2003_UNIQUEDATES %>%
  rename(COUNT_ORACLE = count)

# HISTORICAL unique dates and number of occurrences 
pr_03_historical_UNIQUEDATES <- pr_03 %>% 
  group_by(INTDATE) %>% 
  summarize(count=n())
pr_03_historical_UNIQUEDATES_updated <- pr_03_historical_UNIQUEDATES %>%
  rename(COUNT_HISTORICAL = count,
         FINAL_DATE = INTDATE)

# combine two tables
unique_dates_merge_03 <- full_join(com_tip_PR_2003_UNIQUEDATES_updated, pr_03_historical_UNIQUEDATES_updated, by = 'FINAL_DATE')
# DATES ARE MISALIGNED FROM JAN 1 TO APRIL 3 AND OCT 26 TO DEC 31


# change ORACLE dates so they align with HISTORICAL

com_tip_PR_2003_corrected_dates <- com_tip_PR_2003_ORGANIZED %>%
  mutate(NEWDATE = case_when(FINAL_DATE <= "2003-04-03" ~ (FINAL_DATE + days(1)),
                             TRUE ~ FINAL_DATE),
         NEWDATE = case_when(NEWDATE > "2003-10-25" ~ (NEWDATE + days(1)),
                             TRUE ~ NEWDATE)) 


save(com_tip_PR_2003_corrected_dates,file="data/dataframes/com_tip_PR_2003_corrected_dates.Rda") # file is saved in data folder  


# ORACLE unique dates and number of occurrences 
com_tip_PR_2003_UNIQUEDATES_2 <- com_tip_PR_2003_corrected_dates %>% 
  group_by(NEWDATE) %>% 
  summarize(count=n())
com_tip_PR_2003_UNIQUEDATES_updated_2 <- com_tip_PR_2003_UNIQUEDATES_2 %>%
  rename(COUNT_ORACLE = count)

# HISTORICAL unique dates and number of occurrences 
pr_03_historical_UNIQUEDATES <- pr_03 %>% 
  group_by(INTDATE) %>% 
  summarize(count=n())
pr_03_historical_UNIQUEDATES_updated_2 <- pr_03_historical_UNIQUEDATES %>%
  rename(COUNT_HISTORICAL = count,
         NEWDATE = INTDATE) 

# combine two tables
unique_dates_merge_03_2 <- full_join(com_tip_PR_2003_UNIQUEDATES_updated_2, pr_03_historical_UNIQUEDATES_updated_2, by = 'NEWDATE')

# compare 
unique_dates_comparison_03 <- unique_dates_merge_03_2 %>%
  mutate(COMPARE = (COUNT_ORACLE - COUNT_HISTORICAL))
# ORACLE IS MISSING 126 RECORDS (125 FROM 4/30 AND 1 FROM 11/6)

write.csv(unique_dates_comparison_03, file = "data/CSVs/dates_comparison_03.csv", row.names = FALSE)
save(unique_dates_comparison_03,file="data/dataframes/dates_comparison_03.Rda") # file is saved in data fold

# AREAZIP COMPARISON ####

# Load MUNI conversion table 
load("~/SEFSC-SFD-CFB-TIP-Compositions/data/dataframes/muni_code_clean_UPDATED.Rda")

# translate muni_zip -> CNTY_ID 
  # split AREAZIP into 2-1 chr columns 
  pr_03$muni_zip_HISTORICAL <- str_sub(pr_03$AREAZIP, -3, -2)
  
  com_tip_PR_2003_corrected_dates$muni_zip_ORACLE <- as.numeric(as.character(com_tip_PR_2003_corrected_dates$SAMPLE_AREA_COUNTY_CODE))

# FIND UNIQUE MUNI CODES AND # OF OCCURANCES 

# unique MUNI CODES and number of occurrences - ORACLE - 16 UNIQUE MUNI_ZIPS 
com_tip_PR_2003_UNIQUE_MUNI <- com_tip_PR_2003_corrected_dates %>% 
  group_by(muni_zip_ORACLE) %>% 
  summarize(count=n())
com_tip_PR_2003_MUNI_updated <- com_tip_PR_2003_UNIQUE_MUNI %>%
  rename(count_ORACLE = count) 
com_tip_PR_2003_MUNI_names <- com_tip_PR_2003_MUNI_updated %>%
  mutate(muni_name = muni_code_clean_UPDATED$CNTY_NAME[match(com_tip_PR_2003_MUNI_updated$muni_zip_ORACLE, muni_code_clean_UPDATED$muni_zip_ORACLE)])

# unique dates and number of occurrences- HISTORICAL- 14 UNIQUE MUNI_ZIP
pr_03_unique_MUNI <- pr_03 %>% 
  group_by(muni_zip_HISTORICAL) %>% 
  summarize(count=n())
pr_03_MUNI_updated <- pr_03_unique_MUNI %>%
  rename(count_HISTORICAL = count)
#pr_03_sp88_unique_MUNI_updated$muni_zip_HISTORICAL = as.numeric(as.character(pr_03_sp88_unique_MUNI_updated$muni_zip_HISTORICAL))
pr_03_MUNI_names <- pr_03_MUNI_updated %>%
  mutate(muni_name = muni_code_clean_UPDATED$CNTY_NAME[match(pr_03_MUNI_updated$muni_zip_HISTORICAL, muni_code_clean_UPDATED$muni_zip_HISTORICAL)])

# merge tables
unique_muni_merge_03 <- full_join(com_tip_PR_2003_MUNI_names, pr_03_MUNI_names, by = 'muni_name')

# COMPARE 
muni_comparison_03 <- unique_muni_merge_03 %>%
  mutate(COMPARE = (count_ORACLE - count_HISTORICAL))

write.csv(muni_comparison_03, file = "data/CSVs/muni_comparison_03.csv", row.names = FALSE)
save(muni_comparison_03,file="data/dataframes/muni_comparison_03.Rda") # file is saved in data folder  

# GEAR COMPARISON ####

load(file = "data/dataframes/gear_codes.Rda")

# unique GEAR CODES and number of occurrences - ORACLE - 11 UNIQUE GEAR CODES
com_tip_PR_2003_unique_GEAR <- com_tip_PR_2003 %>% 
  group_by(STANDARDGEAR_1) %>% 
  summarize(count=n())
com_tip_PR_2003_unique_GEAR_updated <- com_tip_PR_2003_unique_GEAR %>%
  rename(count_ORACLE = count,
         STANDARD_GEAR = STANDARDGEAR_1) 
#com_PR_lob_unique_MUNI_updated$muni_zip_ORACLE = as.numeric(as.character(com_PR_lob_unique_MUNI_updated$muni_zip_ORACLE))
com_tip_PR_2003_unique_GEAR_NAMES <- com_tip_PR_2003_unique_GEAR_updated %>%
  mutate(GEAR_name = gear_codes$STANDARD_NAME[match(com_tip_PR_2003_unique_GEAR_updated$STANDARD_GEAR, gear_codes$STANDARD_GEAR)])


# unique dates and number of occurrences- HISTORICAL- 13 UNIQUE MUNI_ZIP
pr_03_unique_GEAR <- pr_03 %>% 
  group_by(GEARCODE) %>% 
  summarize(count=n())
pr_03_unique_GEAR_UPDATED <- pr_03_unique_GEAR %>%
  rename(count_HISTORICAL = count)
#pr_03_sp901_unique_MUNI_updated$muni_zip_HISTORICAL = as.numeric(as.character(pr_03_sp901_unique_MUNI_updated$muni_zip_HISTORICAL))
pr_03_unique_GEAR_NAMES <- pr_03_unique_GEAR_UPDATED %>%
  mutate(GEAR_name = gear_codes$STANDARD_NAME[match(pr_03_unique_GEAR_UPDATED$GEARCODE, gear_codes$GEARCODE)])

# merge tables
unique_GEAR_merge_PR <- merge(com_tip_PR_2003_unique_GEAR_NAMES,pr_03_unique_GEAR_NAMES, by = 'GEAR_name', all = TRUE)

# ORACLE by hand, diving gear (snare) vs HISTORICAL scuba diving and skin diving 

# COMPARE 
GEAR_comparison_03 <- unique_GEAR_merge_PR %>%
  mutate(COMPARE = (count_ORACLE - count_HISTORICAL))

write.csv(GEAR_comparison_03, file = "data/CSVs/GEAR_code_comparison_03.csv", row.names = FALSE)
save(GEAR_comparison_03,file="data/dataframes/GEAR_code_comparison_03.Rda") # file is saved in data folder  
# the missing 126 records are bottom/handline gear records 
