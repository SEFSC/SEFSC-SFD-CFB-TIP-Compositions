# Comparing historical and oracle data for Caribbean Spiny Lobster 2004 ####

# pulling from 3.0 Historical 

# TRIAL AND ERROR ####

      # Align dates and number of records between oracle and historical ####
      
      # count unique dates in oracle 
      
      # unique dates with unique area codes
        com_lob_organized %>%
          group_by(FINAL_DATE) %>%
          summarize(distinct_points = n_distinct(SAMPLE_AREA_COUNTY_CODE))
      
      # unique dates and number of occurrences 
        com_lob_unique_dates <- com_lob_organized %>% 
          group_by(FINAL_DATE) %>% 
          summarize(count=n())
      
      # update date column
        com_lob_unique_dates_updated <- com_lob_unique_dates %>%
          mutate(INTDATE = (FINAL_DATE + days(1))) %>%
          rename(COUNT_ORACLE = count)
      
      
      # count unique dates in historical 
      
      # uniquew dates and number of occurances 
        pr_04_sp901_unique_dates <- pr_04_sp901_filtered %>% 
          group_by(INTDATE) %>% 
          summarize(count=n())
        pr_04_sp901_unique_dates_updated <- pr_04_sp901_unique_dates %>%
          rename(COUNT_HISTORICAL = count)
      
      # combine two tables ####
      
        unique_dates_merge <- merge(pr_04_sp901_unique_dates_updated, com_lob_unique_dates_updated, by = 'INTDATE', all = TRUE)
        
        unique_dates_merge_clean <- unique_dates_merge[,c(1,2,4)]
      
      
      # try again without date correction ####
      
      # unique dates and number of occurrences 
        com_lob_unique_dates2 <- com_lob_organized %>% 
          group_by(FINAL_DATE) %>% 
          summarize(count=n())
        com_lob_unique_dates_updated2 <- com_lob_unique_dates2 %>%
          rename(COUNT_ORACLE2 = count,
                 INTDATE = FINAL_DATE)
      
      
      # unique dates and number of occurrences 
        pr_04_sp901_unique_dates2 <- pr_04_sp901_filtered %>% 
          group_by(INTDATE) %>% 
          summarize(count=n())
        pr_04_sp901_unique_dates_updated2 <- pr_04_sp901_unique_dates2 %>%
          rename(COUNT_HISTORICAL2 = count)
      
      # combine two tables ####
        unique_dates_merge2 <- merge(pr_04_sp901_unique_dates_updated2, com_lob_unique_dates_updated2, by = 'INTDATE', all = TRUE)
        
        str(unique_dates_merge2)
        
        sum(unique_dates_merge2$COUNT_ORACLE2)
      
      # SO UNFORTUNATELY THE DATES DON'T MATCH UP WHETHER ONE DAY IS ADDED OR NOT, AT LEAST WITH SPINY LOBSTER, I CAN TRY WITH ALL OF 2004
# START HERE ####      
      # Align dates by blocking the dates ####
        load(file = "data/dataframes/pr_04_sp901_filtered.Rda")
      
      # change historical dates so they align with oracle
        pr_04_sp901_corrected_dates <- pr_04_sp901_filtered %>%
          mutate(NEWDATE = case_when(INTDATE <= "2004-03-29" ~ (INTDATE - days(1)),
                                     TRUE ~ INTDATE),
                 NEWDATE = case_when(NEWDATE > "2004-11-11" ~ (NEWDATE - days(1)),
                                     TRUE ~ NEWDATE)) 
        
        save(pr_04_sp901_corrected_dates,file="pr_04_sp901_corrected_dates.Rda") # file is saved in data folder  
        
        
        str(pr_04_sp901_corrected_dates)
                
      # # unique dates and number of occurrences 
      #   com_lob_unique_dates3 <- com_lob_organized %>% 
      #     group_by(FINAL_DATE) %>% 
      #     summarize(count=n())
      #   com_lob_unique_dates_update3 <- com_lob_unique_dates3 %>%
      #     rename(COUNT_ORACLE3 = count,
      #            NEWDATE = FINAL_DATE)
      
      # unique dates and number of occurrences 
        pr_04_sp901_unique_dates3 <- pr_04_sp901_corrected_dates %>% 
          group_by(NEWDATE) %>% 
          summarize(count=n())
        pr_04_sp901_unique_dates_updated3 <- pr_04_sp901_unique_dates3 %>%
          rename(COUNT_HISTORICAL3 = count)
        
        save(pr_04_sp901_unique_dates_updated3,file="pr_04_sp901_unique_dates_updated3.Rda") # file is saved in data folder  
      
      # # combine two tables ####
      #   unique_dates_merge3 <- merge(pr_04_sp901_unique_dates_updated3, com_lob_unique_dates_update3, by = 'NEWDATE', all = TRUE)
      #   # dates align!!
  
  
# Redo all this with just PR cause I'm dumb ####
  # cr_tip_sp(state_codes = c('PR'), sp_codes = c(97648, 97646))
  
# read in PR data
  com_tip_PR_lob <- readRDS("~/SEFSC-SFD-CFB-TIP-Compositions/data/raw/com_tip_PR_97648_97646_20230828.RDS")
 
  com_PR_lobster_skeleton <- com_tip_PR_lob %>%  # select comprable variables 
    select(ID, INTERVIEW_DATE, YEAR, REPORTING_AREA_ZIP, SAMPLE_AREA_STATE_CODE, SAMPLE_AREA_COUNTY_CODE,
           SAMPLE_AREA_ZIP, LANDING_AREA_PLACE_CODE, LANDING_AREA_COUNTY_CODE, SAMPLE_AREA_PLACE_CODE, 
           GEAR_1, GEARNAME_1, GEAR_QTY_1, GEAR_FREQUENCY1, MIN_DEPTH1, MAX_DEPTH1, 
           OBS_STANDARD_SPECIES_CODE, OBS_STANDARD_SPECIES_NAME, TRIP_DAYS_FISHED, 
           LENGTH1, LENGTH_UNIT1, LENGTH_UNIT_CODE1, LENGTH_TYPE1, LENGTH_TYPE_CODE1, 
           LENGTH1_MM, OBS_WEIGHT, OBS_WEIGHT_KG, OBS_WEIGHT_UNIT, OBS_WEIGHT_UNIT_ID)%>%
    filter(YEAR == 2004) #filter to 2004
  
  # create new filterable date value
  com_PR_lob_date_new <- com_PR_lobster_skeleton %>%
    mutate(TEST_DATE = as.Date(ymd_hms(INTERVIEW_DATE)),
           FINAL_DATE = case_when(is.na(TEST_DATE) ~ INTERVIEW_DATE, 
                                  TRUE ~ TEST_DATE))
  com_PR_lob_organized <- com_PR_lob_date_new[,c(1,2,3,31, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27 ,28 ,29 ,30)]
  
  save(com_PR_lob_organized,file="data/dataframes/com_PR_lob_organized.Rda") # file is saved in data folder  

  # Load Data #### 
  load(file = "data/dataframes/pr_04_sp901_unique_dates_updated3.Rda")
  load(file = "data/dataframes/com_PR_lob_organized.Rda")
  
   # unique dates and number of occurrences - ORACLE
  com_PR_lob_unique_dates3 <- com_PR_lob_organized %>% 
    group_by(FINAL_DATE) %>% 
    summarize(count=n())
  com_PR_lob_unique_dates_update3 <- com_PR_lob_unique_dates3 %>%
    rename(COUNT_ORACLE3 = count,
           NEWDATE = FINAL_DATE)  
  
  # # unique dates and number of occurrences- HISTORICAL
  # pr_04_sp901_unique_dates3 <- pr_04_sp901_corrected_dates %>% 
  #   group_by(NEWDATE) %>% 
  #   summarize(count=n())
  # pr_04_sp901_unique_dates_updated3 <- pr_04_sp901_unique_dates3 %>%
  #   rename(COUNT_HISTORICAL3 = count)
  
  # combine two tables ####
  unique_dates_merge_PR <- merge(pr_04_sp901_unique_dates_updated3, com_PR_lob_unique_dates_update3, by = 'NEWDATE', all = TRUE)
  
  dates_comparison <- unique_dates_merge_PR %>%
    mutate(COMPARE = (COUNT_ORACLE3 - COUNT_HISTORICAL3))
  # 2 instances of new interview dates with records, 2 instances of interview dates with one more record than historical, 
  # one instance of 8 more records on date
  
  write.csv(dates_comparison, file = "data/CSVs/dates_comparison.csv", row.names = FALSE)
  save(dates_comparison,file="data/dataframes/dates_comparison.Rda") # file is saved in data folder  
  