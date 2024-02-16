# redo with full 2004 data

# FILTER DATAFRAMES ####

# Load libraries
librarian::shelf(here, tidyverse, ROracle, keyring, dotenv, lubridate)

# Load HISTORICAL data 
PR_historical_04 <- read.csv("data/raw/hist_04.csv")

# Format dates to be mm/dd/yyyy 
pr_04 <- PR_historical_04 %>%
  # Format dates to be mm/dd/yyyy 
  mutate(INTDATE = as.Date(INTDATE, "%m/%d/%Y"))

save(pr_04,file="data/dataframes/pr_04.Rda") # file is saved in data folder

# PULL FROM ORACLE - BEFORE DAYLIGHT SAVINGS
com_tip_PR_2004 <- readRDS("~/SEFSC-SFD-CFB-TIP-Compositions/data/raw/com_tip_PR_2004_20231004.RDS")

# Create a comparable table 
com_tip_PR_2004_skeleton <- com_tip_PR_2004 %>%  # select comparable variables 
  select(ID, INTERVIEW_DATE, YEAR, REPORTING_AREA_ZIP, SAMPLE_AREA_COUNTY_CODE, 
         SAMPLE_AREA_PLACE_CODE, SAMPLE_AREA_STATE_CODE,  SAMPLE_AREA_ZIP, 
         SITE_LOCATION, LAND_STANDARD_GEAR_NAME,
         LAND_STANDARD_GEAR, OBS_STANDARD_SPECIES_CODE, OBS_STANDARD_SPECIES_NAME,
         LENGTH1_MM, OBS_WEIGHT_KG)

# create new filterable date value
com_tip_PR_2004_NEWDATE <- com_tip_PR_2004_skeleton %>%
  mutate(TEST_DATE = as.Date(ymd_hms(INTERVIEW_DATE)),
         FINAL_DATE = case_when(is.na(TEST_DATE) ~ INTERVIEW_DATE, 
                                TRUE ~ TEST_DATE))

com_tip_PR_2004_ORGANIZED <- com_tip_PR_2004_NEWDATE[,c(1,2,3,17, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)]

save(com_tip_PR_2004_ORGANIZED,file="data/dataframes/com_tip_PR_2004_ORGANIZED.Rda") # file is saved in data folder  


# DATE COMPARISON ####

load(file = "data/dataframes/pr_04.Rda")
load(file = "data/dataframes/com_tip_PR_2004_ORGANIZED.Rda")

# ORACLE unique dates and number of occurrences 
com_tip_PR_2004_UNIQUEDATES <- com_tip_PR_2004_ORGANIZED %>% 
  group_by(FINAL_DATE) %>% 
  summarize(count=n())
com_tip_PR_2004_UNIQUEDATES_updated <- com_tip_PR_2004_UNIQUEDATES %>%
  rename(COUNT_ORACLE = count)

# HISTORICAL unique dates and number of occurrences 
pr_04_historical_UNIQUEDATES <- pr_04 %>% 
  group_by(INTDATE) %>% 
  summarize(count=n())
pr_04_historical_UNIQUEDATES_updated <- pr_04_historical_UNIQUEDATES %>%
  rename(COUNT_HISTORICAL = count,
         FINAL_DATE = INTDATE)

# combine two tables
unique_dates_merge_04 <- full_join(com_tip_PR_2004_UNIQUEDATES_updated, pr_04_historical_UNIQUEDATES_updated, by = 'FINAL_DATE')
# DATES ARE MISALIGNED FROM JAN 1 TO APRIL 1 AND NOV 3 TO DEC 31
write.csv(unique_dates_merge_04, file = "data/CSVs/date_comparison_2004_b4dls.csv", row.names = FALSE)

# PULL FROM ORACLE - AFTER DAYLIGHT SAVINGS
cr_tip_yr(state_codes = "PR", year = 2004)

com_tip_PR_2004_new <- readRDS("~/SEFSC-SFD-CFB-TIP-Compositions/data/raw/com_tip_PR_2004_20240216.RDS")

# Create a comparable table 
com_tip_PR_2004_skeleton_new <- com_tip_PR_2004_new %>%  # select comparable variables 
  select(ID, INTERVIEW_DATE, YEAR, REPORTING_AREA_ZIP, SAMPLE_AREA_COUNTY_CODE, 
         SAMPLE_AREA_PLACE_CODE, SAMPLE_AREA_STATE_CODE,  SAMPLE_AREA_ZIP, 
         SITE_LOCATION, LAND_STANDARD_GEAR_NAME,
         LAND_STANDARD_GEAR, OBS_STANDARD_SPECIES_CODE, OBS_STANDARD_SPECIES_NAME,
         LENGTH1_MM, OBS_WEIGHT_KG)

# create new filterable date value
com_tip_PR_2004_NEWDATE_new <- com_tip_PR_2004_skeleton_new %>%
  mutate(TEST_DATE = as.Date(ymd_hms(INTERVIEW_DATE)),
         FINAL_DATE = case_when(is.na(TEST_DATE) ~ INTERVIEW_DATE, 
                                TRUE ~ TEST_DATE))

com_tip_PR_2004_ORGANIZED_new <- com_tip_PR_2004_NEWDATE_new[,c(1,2,3,17, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)]

save(com_tip_PR_2004_ORGANIZED_new,file="data/dataframes/com_tip_PR_2004_ORGANIZED_new.Rda") # file is saved in data folder  


# ORACLE unique dates and number of occurrences 
com_tip_PR_2004_UNIQUEDATES_new <- com_tip_PR_2004_ORGANIZED_new %>% 
  group_by(FINAL_DATE) %>% 
  summarize(count=n())
com_tip_PR_2004_UNIQUEDATES_updated_new <- com_tip_PR_2004_UNIQUEDATES_new %>%
  rename(COUNT_ORACLE = count)

# combine two tables
unique_dates_merge_04_new <- full_join(com_tip_PR_2004_UNIQUEDATES_updated_new, pr_04_historical_UNIQUEDATES_updated, by = 'FINAL_DATE')
write.csv(unique_dates_merge_04_new, file = "data/CSVs/date_comparison_2004_afterdls.csv", row.names = FALSE)



# DATES ARE MISALIGNED FROM JAN 1 TO APRIL 1 AND NOV 3 TO DEC 31

# change ORACLE dates so they align with HISTORICAL

# pr_04_historical_corrected_dates <- pr_04 %>%
#   mutate(NEWDATE = case_when(INTDATE <= "2004-04-01" ~ (INTDATE - days(1)),
#                              TRUE ~ INTDATE),
#          NEWDATE = case_when(NEWDATE > "2004-11-03" ~ (NEWDATE - days(1)),
#                              TRUE ~ NEWDATE)) 

com_tip_PR_2004_corrected_dates <- com_tip_PR_2004_ORGANIZED %>%
  mutate(NEWDATE = case_when(FINAL_DATE <= "2004-04-01" ~ (FINAL_DATE + days(1)),
                             TRUE ~ FINAL_DATE),
         NEWDATE = case_when(NEWDATE > "2004-11-02" ~ (NEWDATE + days(1)),
                             TRUE ~ NEWDATE)) 
 

save(com_tip_PR_2004_corrected_dates,file="data/dataframes/com_tip_PR_2004_corrected_dates.Rda") # file is saved in data folder  

# # HISTORICAL unique dates and number of occurrences 
# pr_04_historical_UNIQUEDATES_2 <- pr_04_historical_corrected_dates %>% 
#   group_by(NEWDATE) %>% 
#   summarize(count=n())
# pr_04_historical_UNIQUEDATES_updated_2 <- pr_04_historical_UNIQUEDATES_2 %>%
#   rename(COUNT_HISTORICAL = count)

# ORACLE unique dates and number of occurrences 
com_tip_PR_2004_UNIQUEDATES_2 <- com_tip_PR_2004_corrected_dates %>% 
  group_by(NEWDATE) %>% 
  summarize(count=n())
com_tip_PR_2004_UNIQUEDATES_updated_2 <- com_tip_PR_2004_UNIQUEDATES_2 %>%
  rename(COUNT_ORACLE = count)

# HISTORICAL unique dates and number of occurrences 
pr_04_historical_UNIQUEDATES <- pr_04 %>% 
  group_by(INTDATE) %>% 
  summarize(count=n())
pr_04_historical_UNIQUEDATES_updated_2 <- pr_04_historical_UNIQUEDATES %>%
  rename(COUNT_HISTORICAL = count,
         NEWDATE = INTDATE) 

# combine two tables
unique_dates_merge_04_2 <- full_join(com_tip_PR_2004_UNIQUEDATES_updated_2, pr_04_historical_UNIQUEDATES_updated_2, by = 'NEWDATE')

# compare 
unique_dates_comparison_04 <- unique_dates_merge_04_2 %>%
  mutate(COMPARE = (COUNT_ORACLE - COUNT_HISTORICAL))
# NO INSTANCED OF HISTORICAL HAVING MORE THAN ORACLE 

write.csv(unique_dates_comparison_04, file = "data/CSVs/dates_comparison_04.csv", row.names = FALSE)
save(unique_dates_comparison_04,file="data/dataframes/dates_comparison_04.Rda") # file is saved in data fold

# AREAZIP COMPARISON ####

# Load areazips 
AREAZIP_CODES <- read_csv("data/CSVs/AREAZIP_CODES.csv")

# AREAZIP_CODES$PLACE_NAME<-gsub(" MUNICIPIO","",as.character(AREAZIP_CODES$PLACE_NAME))
AREAZIP_CODES$CNTY_NAME <- tolower(AREAZIP_CODES$CNTY_NAME)
AREAZIP_CODES[38,4] = 'guayanilla'
AREAZIP_CODES[45,4] = 'guayanilla'
AREAZIP_CODES[88,4] = 'guayanilla'

save(AREAZIP_CODES,file="data/dataframes/AREAZIP_CODES.Rda") # file is saved in data folder  

# Load historical zips 
PR_municipio_codes_xlsx_Sheet1 <- read_csv("data/CSVs/PR_municipio_codes.xlsx - Sheet1.csv")

PR_muni_codes <- PR_municipio_codes_xlsx_Sheet1 %>%
  rename(CNTY_NAME = muni_name)
PR_muni_codes$CNTY_NAME<- tolower(PR_muni_codes$CNTY_NAME)

# CREATE CONVERSION TABLE 
muni_codes <- merge(PR_muni_codes, AREAZIP_CODES, by = 'CNTY_NAME', all = TRUE)
muni_code_clean <- muni_codes[,c(1,3,8)]
muni_code_clean_UPDATED <- muni_code_clean %>%
  rename(muni_zip_HISTORICAL = muni_zip,
         muni_zip_ORACLE = CNTY_ID)
muni_code_clean_UPDATED$muni_zip_HISTORICAL = as.numeric(as.character(muni_code_clean_UPDATED$muni_zip_HISTORICAL))

save(muni_code_clean_UPDATED,file="data/dataframes/muni_code_clean_UPDATED.Rda") # file is saved in data folder  


# translate muni_zip -> CNTY_ID 

# split AREAZIP into 2-1 chr columns 
pr_04$muni_zip_HISTORICAL <- str_sub(pr_04$AREAZIP, -3, -2)

com_tip_PR_2004_corrected_dates$muni_zip_ORACLE <- as.numeric(as.character(com_tip_PR_2004_corrected_dates$SAMPLE_AREA_COUNTY_CODE))

# FIND UNIQUE MUNI CODES AND # OF OCCURANCES 

# unique MUNI CODES and number of occurrences - ORACLE - 16 UNIQUE MUNI_ZIPS 
com_tip_PR_2004_UNIQUE_MUNI <- com_tip_PR_2004_corrected_dates %>% 
  group_by(muni_zip_ORACLE) %>% 
  summarize(count=n())
com_tip_PR_2004_MUNI_updated <- com_tip_PR_2004_UNIQUE_MUNI %>%
  rename(count_ORACLE = count) 
com_tip_PR_2004_MUNI_names <- com_tip_PR_2004_MUNI_updated %>%
  mutate(muni_name = muni_code_clean_UPDATED$CNTY_NAME[match(com_tip_PR_2004_MUNI_updated$muni_zip_ORACLE, muni_code_clean_UPDATED$muni_zip_ORACLE)])

# unique dates and number of occurrences- HISTORICAL- 14 UNIQUE MUNI_ZIP
pr_04_unique_MUNI <- pr_04 %>% 
  group_by(muni_zip_HISTORICAL) %>% 
  summarize(count=n())
pr_04_MUNI_updated <- pr_04_unique_MUNI %>%
  rename(count_HISTORICAL = count)
#pr_04_sp88_unique_MUNI_updated$muni_zip_HISTORICAL = as.numeric(as.character(pr_04_sp88_unique_MUNI_updated$muni_zip_HISTORICAL))
pr_04_MUNI_names <- pr_04_MUNI_updated %>%
  mutate(muni_name = muni_code_clean_UPDATED$CNTY_NAME[match(pr_04_MUNI_updated$muni_zip_HISTORICAL, muni_code_clean_UPDATED$muni_zip_HISTORICAL)])

# merge tables
unique_muni_merge_04 <- full_join(com_tip_PR_2004_MUNI_names, pr_04_MUNI_names, by = 'muni_name')

# COMPARE 
muni_comparison_04 <- unique_muni_merge_04 %>%
  mutate(COMPARE = (count_ORACLE - count_HISTORICAL))

write.csv(muni_comparison_04, file = "data/CSVs/muni_comparison_04.csv", row.names = FALSE)
save(muni_comparison_04,file="data/dataframes/muni_comparison_04.Rda") # file is saved in data folder  

# GEAR COMPARISON ####

load(file = "data/dataframes/gear_codes.Rda")

# unique GEAR CODES and number of occurrences - ORACLE - 11 UNIQUE GEAR CODES
com_tip_PR_2004_unique_GEAR <- com_tip_PR_2004 %>% 
  group_by(STANDARDGEAR_1) %>% 
  summarize(count=n())
com_tip_PR_2004_unique_GEAR_updated <- com_tip_PR_2004_unique_GEAR %>%
  rename(count_ORACLE = count,
         STANDARD_GEAR = STANDARDGEAR_1) 
#com_PR_lob_unique_MUNI_updated$muni_zip_ORACLE = as.numeric(as.character(com_PR_lob_unique_MUNI_updated$muni_zip_ORACLE))
com_tip_PR_2004_unique_GEAR_NAMES <- com_tip_PR_2004_unique_GEAR_updated %>%
  mutate(GEAR_name = gear_codes$STANDARD_NAME[match(com_tip_PR_2004_unique_GEAR_updated$STANDARD_GEAR, gear_codes$STANDARD_GEAR)])


# unique dates and number of occurrences- HISTORICAL- 13 UNIQUE MUNI_ZIP
pr_04_unique_GEAR <- pr_04 %>% 
  group_by(GEARCODE) %>% 
  summarize(count=n())
pr_04_unique_GEAR_UPDATED <- pr_04_unique_GEAR %>%
  rename(count_HISTORICAL = count)
#pr_04_sp901_unique_MUNI_updated$muni_zip_HISTORICAL = as.numeric(as.character(pr_04_sp901_unique_MUNI_updated$muni_zip_HISTORICAL))
pr_04_unique_GEAR_NAMES <- pr_04_unique_GEAR_UPDATED %>%
  mutate(GEAR_name = gear_codes$STANDARD_NAME[match(pr_04_unique_GEAR_UPDATED$GEARCODE, gear_codes$GEARCODE)])

# merge tables
unique_GEAR_merge_PR <- merge(com_tip_PR_2004_unique_GEAR_NAMES,pr_04_unique_GEAR_NAMES, by = 'GEAR_name', all = TRUE)

# ORACLE by hand, diving gear (snare) vs HISTORICAL scuba diving and skin diving 

# COMPARE 
GEAR_comparison_04 <- unique_GEAR_merge_PR %>%
  mutate(COMPARE = (count_ORACLE - count_HISTORICAL))

write.csv(GEAR_comparison_04, file = "data/CSVs/GEAR_code_comparison_04.csv", row.names = FALSE)
save(GEAR_comparison_04,file="data/dataframes/GEAR_code_comparison_04.Rda") # file is saved in data folder  

