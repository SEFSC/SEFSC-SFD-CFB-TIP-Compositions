# AREAZIP Alignment ####

# Set up data #### 

# Load libraries
librarian::shelf(here, tidyverse, ROracle, keyring, dotenv, lubridate, readr)
library("stringr")

# Load historical data
load(file = "data/dataframes/pr_04_sp901_corrected_dates.Rda") # use corrected dates from 3.2

# Load oracle data
load(file = "data/dataframes/com_PR_lob_organized.Rda")

view(pr_04_sp901_corrected_dates)
view(com_PR_lob_organized)


# Load areazips 
AREAZIP_CODES <- read_csv("data/CSVs/AREAZIP_CODES.csv")

# Load historical zips 
PR_municipio_codes_xlsx_Sheet1 <- read_csv("data/CSVs/PR_municipio_codes.xlsx - Sheet1.csv")


# Combine historical and oracle zips ####

  PR_muni_codes <- PR_municipio_codes_xlsx_Sheet1 %>%
    rename(CNTY_NAME = muni_name)
  PR_muni_codes$CNTY_NAME<- tolower(PR_muni_codes$CNTY_NAME)
  
  
  # AREAZIP_CODES$PLACE_NAME<-gsub(" MUNICIPIO","",as.character(AREAZIP_CODES$PLACE_NAME))
  AREAZIP_CODES$CNTY_NAME <- tolower(AREAZIP_CODES$CNTY_NAME)
  AREAZIP_CODES[38,4] = 'guayanilla'
  AREAZIP_CODES[45,4] = 'guayanilla'
  AREAZIP_CODES[88,4] = 'guayanilla'
  
  muni_codes <- merge(PR_muni_codes, AREAZIP_CODES, by = 'CNTY_NAME', all = TRUE)
  
  muni_code_clean <- muni_codes[,c(1,3,8)]
  muni_code_clean_UPDATED <- muni_code_clean %>%
    rename(muni_zip_HISTORICAL = muni_zip,
           muni_zip_ORACLE = CNTY_ID)
  muni_code_clean_UPDATED$muni_zip_HISTORICAL = as.numeric(as.character(muni_code_clean_UPDATED$muni_zip_HISTORICAL))

# translate muni_zip -> CNTY_ID 
# split AREAZIP into 2-1 chr columns ####
  # notes: it appears that the cnty_id codes from oracle are the last two 
  # (not the first two) characters of the sample_area_county_code 
    

  # extract muni_zip from AREAZIP in historical 
  # pr_04_sp901_corrected_dates$muni_zip_HISTORICAL <- substr(pr_04_sp901_corrected_dates$AREAZIP, 1, 2)
  pr_04_sp901_corrected_dates$muni_zip_HISTORICAL <- str_sub(pr_04_sp901_corrected_dates$AREAZIP, -3, -2)
  
  # extract muni_zip from AREAZIP in historical 
  # com_PR_lob_organized$muni_zip_ORACLE <- substr(com_PR_lob_organized$SAMPLE_AREA_COUNTY_CODE, 1, 2)
  # com_PR_lob_organized$muni_zip_ORACLE <- str_sub(com_PR_lob_organized$SAMPLE_AREA_COUNTY_CODE, -2, -1)
  com_PR_lob_organized$muni_zip_ORACLE <- as.numeric(as.character(com_PR_lob_organized$SAMPLE_AREA_COUNTY_CODE))

  str(com_PR_lob_organized)
    
# merge tables to get conversion to cnty_id
# pr_04_sp901_converted_municodes <- merge(pr_04_sp901_corrected_dates, muni_code_clean, by = 'muni_zip', all = TRUE)

# FIND UNIQUE MUNI CODES AND # OF OCCURANCES ####
  
  # unique MUNI CODES and number of occurrences - ORACLE - 13 UNIQUE MUNI_ZIPS (was 10 when done with first 2 chr)
  com_PR_lob_unique_MUNI <- com_PR_lob_organized %>% 
    group_by(muni_zip_ORACLE) %>% 
    summarize(count=n())
  com_PR_lob_unique_MUNI_updated <- com_PR_lob_unique_MUNI %>%
    rename(count_ORACLE = count) 
  #com_PR_lob_unique_MUNI_updated$muni_zip_ORACLE = as.numeric(as.character(com_PR_lob_unique_MUNI_updated$muni_zip_ORACLE))
  com_PR_lob_muni_names <- com_PR_lob_unique_MUNI_updated %>%
    mutate(muni_name = muni_code_clean_UPDATED$CNTY_NAME[match(com_PR_lob_unique_MUNI_updated$muni_zip_ORACLE, muni_code_clean_UPDATED$muni_zip_ORACLE)])
  
  
  # unique dates and number of occurrences- HISTORICAL- 14 UNIQUE MUNI_ZIP
  pr_04_sp901_unique_MUNI <- pr_04_sp901_corrected_dates %>% 
    group_by(muni_zip_HISTORICAL) %>% 
    summarize(count=n())
  pr_04_sp901_unique_MUNI_updated <- pr_04_sp901_unique_MUNI %>%
    rename(count_HISTORICAL = count)
  pr_04_sp901_unique_MUNI_updated$muni_zip_HISTORICAL = as.numeric(as.character(pr_04_sp901_unique_MUNI_updated$muni_zip_HISTORICAL))
  pr_04_sp901_muni_names <- pr_04_sp901_unique_MUNI_updated %>%
    mutate(muni_name = muni_code_clean_UPDATED$CNTY_NAME[match(pr_04_sp901_unique_MUNI_updated$muni_zip_HISTORICAL, muni_code_clean_UPDATED$muni_zip_HISTORICAL)])
  
  # merge tables
  unique_muni_merge_PR <- merge(pr_04_sp901_muni_names, com_PR_lob_muni_names, by = 'muni_name', all = TRUE)
  
  write.csv(unique_muni_merge_PR, file = "data/muni_code_comparison.csv", row.names = FALSE)
  save(unique_muni_merge_PR,file="data/muni_code_comparison.Rda") # file is saved in data folder  

# ALIGN PLACE_ID WITH 3RD CHARACTER IN HISTORICAL####
  muni_codes <- merge(PR_muni_codes, AREAZIP_CODES, by = 'CNTY_NAME', all = TRUE)
  
  muni_code_clean_place <- muni_codes[,c(1,3,6,7,8)]
  muni_code_clean_place_UPDATED <- muni_code_clean_place %>%
    rename(place_id_HISTORICAL = muni_zip,
           place_id_ORACLE = PLACE_ID)
  muni_code_clean_place_UPDATED$place_id_HISTORICAL = as.numeric(as.character(muni_code_clean_place_UPDATED$place_id_HISTORICAL))
  
  
  # extract PLACE_ID from AREAZIP in historical 
  # pr_04_sp901_corrected_dates$place_id_HISTORICAL_SIMPLE <- str_sub(pr_04_sp901_corrected_dates$AREAZIP, -1, -1)
  # but without the first 2 characters, you cant tell which muni zip the subzone is in
  # so just keep it as AREAZIP characters? 
  
# FIND UNIQUE place_id CODES AND # OF OCCURANCES
  # create new column to work with with matching names to muni_code_clean_place_UPDATED
  com_PR_lob_organized2 <- com_PR_lob_organized %>%
    mutate(place_id_ORACLE = SAMPLE_AREA_PLACE_CODE)
  
  pr_04_sp901_corrected_dates_place <- pr_04_sp901_corrected_dates %>%
    mutate(place_id_HISTORICAL = AREAZIP)
  
  # unique place id CODES and number of occurrences - ORACLE - 
  com_PR_lob_unique_PLACE_ID <- com_PR_lob_organized2 %>% 
    group_by(place_id_ORACLE) %>% 
    summarize(count=n())
  com_PR_lob_unique_PLACE_ID_updated <- com_PR_lob_unique_PLACE_ID %>%
    rename(count_ORACLE = count) 
  #com_PR_lob_unique_MUNI_updated$muni_zip_ORACLE = as.numeric(as.character(com_PR_lob_unique_MUNI_updated$muni_zip_ORACLE))
  com_PR_lob_unique_PLACE_ID_names <- com_PR_lob_unique_PLACE_ID_updated %>%
    mutate(PLACE_ID_NAME = muni_code_clean_place_UPDATED$PLACE_NAME[match(com_PR_lob_unique_PLACE_ID_updated$place_id_ORACLE, muni_code_clean_place_UPDATED$place_id_ORACLE)])
  
  # unique dates and number of occurrences- HISTORICAL- 14 UNIQUE MUNI_ZIP
  pr_04_sp901_unique_PLACE_ID <- pr_04_sp901_corrected_dates_place %>% 
    group_by(place_id_HISTORICAL) %>% 
    summarize(count=n())
  pr_04_sp901_unique_PLACE_ID_updated <- pr_04_sp901_unique_PLACE_ID %>%
    rename(count_HISTORICAL = count)
  pr_04_sp901_unique_PLACE_ID_updated$place_id_HISTORICAL = as.numeric(as.character(pr_04_sp901_unique_PLACE_ID_updated$place_id_HISTORICAL))
  pr_04_sp901_PLACE_ID_names <- pr_04_sp901_unique_PLACE_ID_updated %>%
    mutate(PLACE_ID_name = muni_code_clean_place_UPDATED$CNTY_NAME[match(pr_04_sp901_unique_PLACE_ID_updated$place_id_HISTORICAL, muni_code_clean_place_UPDATED$place_id_HISTORICAL)])
  
  # NEED PLACE ID LIST FOR HISTORICAL 
  
  
  # MAYBE TRY WITH A DIFFERENT SPECIES - LETS TRY YELLOWTAIL SNAPPER ? red hind 
  

  
  
  
  
  
  # # unique dates and number of occurrences- HISTORICAL- 14 UNIQUE MUNI_ZIP
  # pr_04_sp901_unique_MUNI <- pr_04_sp901_corrected_dates %>% 
  #   group_by(muni_zip_HISTORICAL) %>% 
  #   summarize(count=n())
  # pr_04_sp901_unique_MUNI_updated <- pr_04_sp901_unique_MUNI %>%
  #   rename(count_HISTORICAL = count)
  # pr_04_sp901_unique_MUNI_updated$muni_zip_HISTORICAL = as.numeric(as.character(pr_04_sp901_unique_MUNI_updated$muni_zip_HISTORICAL))
  # pr_04_sp901_muni_names <- pr_04_sp901_unique_MUNI_updated %>%
  #   mutate(muni_name = muni_code_clean_UPDATED$CNTY_NAME[match(pr_04_sp901_unique_MUNI_updated$muni_zip_HISTORICAL, muni_code_clean_UPDATED$muni_zip_HISTORICAL)])
  # 
  # # merge tables
  # unique_muni_merge_PR <- merge(pr_04_sp901_muni_names, com_PR_lob_muni_names, by = 'muni_name', all = TRUE)
  # 
  
# # COMBINE BOTH DATASETS?     
# # Reduce size of table to make records easier to work with ####
#   # need to remove leading 0's 
#   com_PR_lob_reduced <- com_PR_lob_organized[,c(1,4,5,7,8, 28)]
#   # com_PR_lob_reduced$muni_zip_ORACLE = as.numeric(as.character(com_PR_lob_reduced$muni_zip_ORACLE))
#   
#   str(pr_04_sp901_reduced)
# 
#   pr_04_sp901_reduced <- pr_04_sp901_corrected_dates[,c(1,3,4,15, 16)]
#   # pr_04_sp901_reduced$muni_zip_HISTORICAL = as.numeric(as.character(pr_04_sp901_reduced$muni_zip_HISTORICAL))
#   
# # convert historical muni code to oracle muni code ####
#   # need to add column to each df with name of muni so that codes can be translated
#   
#   com_PR_lob_names <- com_PR_lob_reduced %>%
#     mutate(muni_name = muni_code_clean_UPDATED$CNTY_NAME[match(com_PR_lob_reduced$muni_zip_ORACLE, muni_code_clean_UPDATED$muni_zip_ORACLE)])
# 
#   
#   pr_04_sp901_names <- pr_04_sp901_reduced %>%
#     mutate(muni_name = muni_code_clean_UPDATED$CNTY_NAME[match(pr_04_sp901_reduced$muni_zip_HISTORICAL, muni_code_clean_UPDATED$muni_zip_HISTORICAL)]) %>%
#     rename(FINAL_DATE = NEWDATE)
#   
#   
#   unique_muni_merge_PR_TOTAL <- merge(pr_04_sp901_names, com_PR_lob_names, by = c('muni_name', 'FINAL_DATE'), all = TRUE)
