# REPEAT WITH RED HIND ####

# FILTER DATAFRAMES ####

# Load libraries
librarian::shelf(here, tidyverse, ROracle, keyring, dotenv, lubridate)

# Load historical data 
PR_historical_04 <- read.csv(here('data/raw', "for_import_04.csv"))

# summary(PR_historical_04)

# Filter to red hind 
pr_historical_04_sp88 <- PR_historical_04 %>%
  filter(SPECIES_B == 088)

# Format dates to be mm/dd/yyyy 
pr_historical_04_sp88_filtered <- pr_historical_04_sp88 %>%
  mutate(INTDATE = as.Date(INTDATE, "%m/%d/%Y"))

save(pr_historical_04_sp88_filtered,file="data/dataframes/pr_historical_04_sp88_filtered.Rda") # file is saved in data folder

# PULL FROM ORACLE 
com_tip_PR_RH <- readRDS("~/SEFSC-SFD-CFB-TIP-Compositions/data/raw/com_tip_PR_167700_20230929.RDS")

# Create a comparable table 
com_tip_PR_RH_skeleton <- com_tip_PR_RH %>%  # select comparable variables 
  select(ID, INTERVIEW_DATE, YEAR, REPORTING_AREA_ZIP, SAMPLE_AREA_STATE_CODE, SAMPLE_AREA_COUNTY_CODE,
         SAMPLE_AREA_ZIP, LANDING_AREA_PLACE_CODE, LANDING_AREA_COUNTY_CODE, SAMPLE_AREA_PLACE_CODE, 
         GEAR_1, GEARNAME_1, GEAR_QTY_1, GEAR_FREQUENCY1, MIN_DEPTH1, MAX_DEPTH1, 
         OBS_STANDARD_SPECIES_CODE, OBS_STANDARD_SPECIES_NAME, TRIP_DAYS_FISHED, 
         LENGTH1, LENGTH_UNIT1, LENGTH_UNIT_CODE1, LENGTH_TYPE1, LENGTH_TYPE_CODE1, 
         LENGTH1_MM, OBS_WEIGHT, OBS_WEIGHT_KG, OBS_WEIGHT_UNIT, OBS_WEIGHT_UNIT_ID)%>%
  filter(YEAR == 2004) #filter to 2004

# create new filterable date value
com_RH_date_new <- com_tip_PR_RH_skeleton %>%
  mutate(TEST_DATE = as.Date(ymd_hms(INTERVIEW_DATE)),
         FINAL_DATE = case_when(is.na(TEST_DATE) ~ INTERVIEW_DATE, 
                                TRUE ~ TEST_DATE))
com_RH_organized <- com_RH_date_new[,c(1,2,3,31, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30)]

save(com_RH_organized,file="data/dataframes/com_RH_organized.Rda") # file is saved in data folder  


# DATE COMPARISON ####

load(file = "data/dataframes/pr_historical_04_sp88_filtered.Rda")
load(file = "data/dataframes/com_RH_organized.Rda")

# ORACLE unique dates and number of occurrences 
com_RH_unique_dates <- com_RH_organized %>% 
  group_by(FINAL_DATE) %>% 
  summarize(count=n())
com_RH_unique_dates_updated <- com_RH_unique_dates %>%
  rename(COUNT_ORACLE = count)

# HISTORICAL unique dates and number of occurrences 
pr_04_sp88_unique_dates <- pr_historical_04_sp88_filtered %>% 
  group_by(INTDATE) %>% 
  summarize(count=n())
pr_04_sp88_unique_dates_updated <- pr_04_sp88_unique_dates %>%
  rename(COUNT_HISTORICAL = count,
         FINAL_DATE = INTDATE)

# combine two tables
RH_unique_dates_merge <- merge(pr_04_sp88_unique_dates_updated, com_RH_unique_dates_updated, by = 'FINAL_DATE', all = TRUE)
# DATES ARE MISALIGNED FROM JAN 1 TO APRIL 12 AND NOV 6 TO DEC 31


# change historical dates so they align with oracle
pr_04_sp88_corrected_dates <- pr_historical_04_sp88_filtered %>%
  mutate(NEWDATE = case_when(INTDATE <= "2004-04-12" ~ (INTDATE - days(1)),
                             TRUE ~ INTDATE),
         NEWDATE = case_when(NEWDATE > "2004-11-06" ~ (NEWDATE - days(1)),
                             TRUE ~ NEWDATE)) 

save(pr_04_sp88_corrected_dates,file="data/dataframes/pr_04_sp88_corrected_dates.Rda") # file is saved in data folder  

# HISTORICAL unique dates and number of occurrences 
pr_04_sp88_unique_dates2 <- pr_04_sp88_corrected_dates %>% 
  group_by(NEWDATE) %>% 
  summarize(count=n())
pr_04_sp88_unique_dates_UPDATED2 <- pr_04_sp88_unique_dates2 %>%
  rename(COUNT_HISTORICAL = count)

# ORACLE unique dates and number of occurrences 
com_RH_unique_dates <- com_RH_organized %>% 
  group_by(FINAL_DATE) %>% 
  summarize(count=n())
com_RH_unique_dates_updated2 <- com_RH_unique_dates %>%
  rename(COUNT_ORACLE = count,
         NEWDATE = FINAL_DATE)

# combine two tables
RH_unique_dates_merge2 <- merge(pr_04_sp88_unique_dates_UPDATED2, com_RH_unique_dates_updated2, by = 'NEWDATE', all = TRUE)

# COMPARE 
RH_dates_comparison <- RH_unique_dates_merge2 %>%
  mutate(COMPARE = (COUNT_ORACLE - COUNT_HISTORICAL))
# NO INSTANCED OF HISTORICAL HAVING MORE THAN ORACLE 

write.csv(RH_dates_comparison, file = "data/CSVs/RH_dates_comparison.csv", row.names = FALSE)
save(RH_dates_comparison,file="data/dataframes/RH_dates_comparison.Rda") # file is saved in data fold

# AREAZIP COMPARISON ####

# Load areazips 
AREAZIP_CODES <- read_csv("data/CSVs/AREAZIP_CODES.csv")

# AREAZIP_CODES$PLACE_NAME<-gsub(" MUNICIPIO","",as.character(AREAZIP_CODES$PLACE_NAME))
AREAZIP_CODES$CNTY_NAME <- tolower(AREAZIP_CODES$CNTY_NAME)
AREAZIP_CODES[38,4] = 'guayanilla'
AREAZIP_CODES[45,4] = 'guayanilla'
AREAZIP_CODES[88,4] = 'guayanilla'

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


# translate muni_zip -> CNTY_ID 

# split AREAZIP into 2-1 chr columns 
pr_04_sp88_corrected_dates$muni_zip_HISTORICAL <- str_sub(pr_04_sp88_corrected_dates$AREAZIP, -3, -2)

com_RH_organized$muni_zip_ORACLE <- as.numeric(as.character(com_RH_organized$SAMPLE_AREA_COUNTY_CODE))

# FIND UNIQUE MUNI CODES AND # OF OCCURANCES 

# unique MUNI CODES and number of occurrences - ORACLE - 16 UNIQUE MUNI_ZIPS 
com_RH_unique_MUNI <- com_RH_organized %>% 
  group_by(muni_zip_ORACLE) %>% 
  summarize(count=n())
com_RH_unique_MUNI_updated <- com_RH_unique_MUNI %>%
  rename(count_ORACLE = count) 
com_RH_muni_names <- com_RH_unique_MUNI_updated %>%
  mutate(muni_name = muni_code_clean_UPDATED$CNTY_NAME[match(com_RH_unique_MUNI_updated$muni_zip_ORACLE, muni_code_clean_UPDATED$muni_zip_ORACLE)])

# unique dates and number of occurrences- HISTORICAL- 14 UNIQUE MUNI_ZIP
pr_04_sp88_unique_MUNI <- pr_04_sp88_corrected_dates %>% 
  group_by(muni_zip_HISTORICAL) %>% 
  summarize(count=n())
pr_04_sp88_unique_MUNI_updated <- pr_04_sp88_unique_MUNI %>%
  rename(count_HISTORICAL = count)
pr_04_sp88_unique_MUNI_updated$muni_zip_HISTORICAL = as.numeric(as.character(pr_04_sp88_unique_MUNI_updated$muni_zip_HISTORICAL))
pr_04_sp88_muni_names <- pr_04_sp88_unique_MUNI_updated %>%
  mutate(muni_name = muni_code_clean_UPDATED$CNTY_NAME[match(pr_04_sp88_unique_MUNI_updated$muni_zip_HISTORICAL, muni_code_clean_UPDATED$muni_zip_HISTORICAL)])

# merge tables
RH_unique_muni_merge <- merge(pr_04_sp88_muni_names, com_RH_muni_names, by = 'muni_name', all = TRUE)

# COMPARE 
RH_muni_comparison <- RH_unique_muni_merge %>%
  mutate(COMPARE = (count_ORACLE - count_HISTORICAL))

write.csv(RH_unique_muni_merge, file = "data/CSVs/RH_muni_code_comparison.csv", row.names = FALSE)
save(RH_unique_muni_merge,file="data/dataframes/RH_unique_muni_merge.Rda") # file is saved in data folder  

