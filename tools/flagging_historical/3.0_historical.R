# HISTORIC DATA INVESTIGATION ####

# Load libraries ####
librarian::shelf(here, tidyverse, ROracle, keyring, dotenv, lubridate)

# Load historical data ####
PR_historical_04 <- read.csv(here('data/raw', "for_import_04.csv"))

# summary(PR_historical_04)

# Filter to Spiny lobster
  pr_historical_04_sp901 <- PR_historical_04 %>%
    filter(SPECIES_B == 901)
 # plot(pr_historical_04_sp901$AREAZIP,  pr_historical_04_sp901$STLENGTH)

# Flag 1 - max number of characters for predetermined values ####
  # Example - AREAZIP = 3 CHARACTERS
    # 347 is incorrectly input as 34700 on 13 occasions in 2004
  # unique(pr_historical_04_sp901$AREAZIP)
  # nrow(pr_historical_04_sp901 == '37400')
  # pr_historical_04_sp901[1956, ]

# Testing Spiny lobster data sp 901 - Fix incorrect character count in areazip and date format
  pr_04_sp901_filtered <- pr_historical_04_sp901 %>%
    mutate(AREAZIP = case_when(AREAZIP == 37400 ~ 374, #Fix AREAZIP
                              TRUE ~ AREAZIP)) %>%
  # Format dates to be mm/dd/yyyy 
    mutate(INTDATE = as.Date(INTDATE, "%m/%d/%Y"))

  save(pr_04_sp901_filtered,file="pr_04_sp901_filtered.Rda") # file is saved in data folder
  
# PULL FROM ORACLE ####

  com_lobster <- 
    readRDS("~/SEFSC-SFD-CFB-TIP-Compositions/data/raw/com_tip_PR_VI_97648_97646_20230712.RDS")
  #view(com_lobster)
  summary(com_lobster)
  colnames(com_lobster)
  str(com_lobster)
  
# Create a comparable table 
  com_lobster_skeleton <- com_lobster %>%  # select comprable variables 
    select(ID, INTERVIEW_DATE, YEAR, REPORTING_AREA_ZIP, SAMPLE_AREA_STATE_CODE, SAMPLE_AREA_COUNTY_CODE,
           SAMPLE_AREA_ZIP, LANDING_AREA_PLACE_CODE, LANDING_AREA_COUNTY_CODE, SAMPLE_AREA_PLACE_CODE, 
           GEAR_1, GEARNAME_1, GEAR_QTY_1, GEAR_FREQUENCY1, MIN_DEPTH1, MAX_DEPTH1, 
           OBS_STANDARD_SPECIES_CODE, OBS_STANDARD_SPECIES_NAME, TRIP_DAYS_FISHED, 
           LENGTH1, LENGTH_UNIT1, LENGTH_UNIT_CODE1, LENGTH_TYPE1, LENGTH_TYPE_CODE1, 
           LENGTH1_MM, OBS_WEIGHT, OBS_WEIGHT_KG, OBS_WEIGHT_UNIT, OBS_WEIGHT_UNIT_ID)%>%
    filter(YEAR == 2004) #filter to 2004
  
  # create new filterable date value
   com_lob_date_new <- com_lobster_skeleton %>%
     mutate(TEST_DATE = as.Date(ymd_hms(INTERVIEW_DATE)),
            FINAL_DATE = case_when(is.na(TEST_DATE) ~ INTERVIEW_DATE, 
                                   TRUE ~ TEST_DATE))
   com_lob_organized <- com_lob_date_new[,c(1,2,3,31, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30)]
  
   save(com_lob_organized,file="data/dataframes/com_lob_organized.Rda") # file is saved in data folder  
  #view(com_lobster_skeleton)
  
# Combine both data sets #### 
  # find which variables are comprable 
  unique(com_lobster_skeleton$SAMPLE_AREA_ZIP)
  unique(com_lobster_skeleton$SAMPLE_AREA_COUNTY_CODE)
  # unique(com_lobster_skeleton$SAMPLE_AREA_STATE_CODE) # pr or vi
  unique(pr_04_sp901_filtered$AREACOUNTY)
  unique(pr_04_sp901_filtered$AREAZIP)
  unique(com_lobster$AREA_1) #area is lat/long grid coordinates 
  unique(pr_04_sp901_filtered$area)
  unique(com_lobster$AREANAME_1) #grid map names
  unique(com_lobster$REPORTING_AREA_ZIP) #have similar numbers to areazip in historical
  unique(com_lobster_skeleton$GEAR_1)
  unique(pr_04_sp901_filtered$GEARCODE) #gear codes are similar
  # sequence and ID dont match up 
  # com_lob area_1 and historical areafish would be similar but areafish is mostly NAs
  
  
  
  # possible variables to link to merge:
    # com_lob reporting_area_zip and historical areazip - problem is there are many NAs in reporting_area_zip
    # com_lob gear code and historical gearcode
    # could convert historical nodccode_B or species_B codes into ITIS species codes and match that way
    # STLENGTH in historical = standard length, could filter com_lob to standard lengths and try to match?

  
  
  
  
  
# Past attempts ####
  # figure out where the na's are
   # is.na(com_lobster_skeleton)
   # colSums(is.na(com_lobster_skeleton))
   # which(colSums(is.na(com_lobster_skeleton))>0)
   # names(which(colSums(is.na(com_lobster_skeleton))>0))  
  
  
  # summary(com_lobster_skeleton)
  
# Fix dates 
  # Create separate time and date columns
  # Attempt 1 
   # com_lobster_date <- com_lobster_skeleton %>%
   #   com_lobster_skeleton$DATE <- as.Date("INTERVIEW_DATE") %>% # Add date column
   #   com_lobster_skeleton$TIME <- format(as.POSIXct("INTERVIEW_DATE"),    # Add time column
   #                     format = "%H:%M:%S")
# Check variable types
  # str(com_lobster_skeleton)
  # str(com_lobster)
  
# Test extracting the date, 00:00:00 time stamp is creating na's
  # as.Date(ymd_hms(com_lobster_skeleton$INTERVIEW_DATE))
  # view(com_lobster_skeleton)
  
 # check for date na's 
  # com_lob_date_na <- com_lobster_skeleton %>%
  #   filter(is.na(INTERVIEW_DATE))
  # view(com_lob_date_na)  
  
# create new filterable date value
  # com_lob_date_new <- com_lobster_skeleton %>%
  #   mutate(TEST_DATE = as.Date(ymd_hms(INTERVIEW_DATE)),
  #          FINAL_DATE = case_when(is.na(TEST_DATE) ~ INTERVIEW_DATE, 
  #                                 TRUE ~ TEST_DATE))
# filter to 2004   
  # com_lob_2004 <- com_lobster_skeleton %>%
  #   filter(YEAR == 2004)
  
  # str(com_lobster_skeleton)
  # unique(com_lobster_skeleton$YEAR)
  
  

  
  
  
  
  # try to reformat date 
     # com_lobster_date <- com_lobster_skeleton %>%
     #   as.POSIXct(INTERVIEW_DATE, " ", format, 
     #              tryFormats = c("%Y-%m-%d %H:%M:%OS",
     #                             "%Y/%m/%d %H:%M:%OS",
     #                             "%Y-%m-%d %H:%M",
     #                             "%Y/%m/%d %H:%M",
     #                             "%Y-%m-%d",
     #                             "%Y/%m/%d"),
     #              optional = FALSE)
     
 #com_lobster_skeleton <- as.numeric(as.character("INTERVIEW_DATE"))  # Convert to numeric
  
  # com_lobster_skeleton <- as.numeric(as.character("TIME")) 
  # summary(com_lobster_skeleton) # Check work
  # try to separate dates 
    # View(com_lobster_skeleton)
    # com_lobster_separate <- com_lobster_skeleton %>%
    #   separate(com_lobster_skeleton, # Separate date into three columns
    #          col = INTERVIEW_DATE,
    #          into = c("INT DATE", "INT TIME"),
    #          sep = " ",
    #          remove = FALSE,
    #          convert = TRUE)
    # view(com_lobster_skeleton)
    
  # com_lobster_time <- com_lobster_skeleton %>% 
  #   mutate(INTERVIEW_DATE = mdy_hms(INTERVIEW_DATE)) #convert Time to a timestamp
  # com_lobster_separate
  # #>                  Time
  # #> 1 2019-06-28 17:09:07
  # df <- df %>% mutate(Year = year(Time), 
  #                     Month = month(Time), 
  #                     Day = day(Time),
  #                     hour = hour(Time),
  #                     minute = minute(Time),
  #                     second = second(Time))

  
  # attempt 2 at separating and reformating dates
    # com_lobster_skeleton_date <- com_lobster_skeleton(date = datetxt,
    #                  year = as.numeric(format(datetxt, format = "%Y")),
    #                  month = as.numeric(format(datetxt, format = "%m")),
    #                  day = as.numeric(format(datetxt, format = "%d")))
    # 
    # com_lobster_skeleton_04 <- com_lobster_skeleton %>%
    #   filter(INTERVIEW_DATE )
  
  
# dont need to pull every time
  # Pull spiny lobster  
  #  cr_tip_sp(state_codes = c('PR', 'VI'), sp_codes = 97648)


