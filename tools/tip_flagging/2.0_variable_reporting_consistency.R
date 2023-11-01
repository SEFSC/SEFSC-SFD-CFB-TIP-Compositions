# Variable Reporting Consistency across SEDAR species in PR and USVI #### 

# Run Tools 1.0

# What percentage of each variable is reported for each sample?
  
# All Species ####   
  # Gather the data ###
  # Caribbean Yellowtail Snapper (PR, STT) (168907) (SEDAR 84, 46)
  cr_tip_sp(state_codes = c('PR', 'VI'), sp_codes = 168907)
  # Hogfish (PR) (170566) (SEDAR 46)
  cr_tip_sp(state_codes = c('PR', 'VI'), sp_codes = 170566)
  # Spiny Lobster (STT, STX) (97648, 97646) (SEDAR 57, 46)
  cr_tip_sp(state_codes = c('PR', 'VI'), sp_codes = c(97648, 97646))
  # Queen Triggerfish (STT, PR) (173139) (SEDAR 46, 80)
  cr_tip_sp(state_codes = c('PR', 'VI'), sp_codes = 173139)
  # Stoplight Parrotfish (STX) (170867) (SEDAR 84, 46)
  cr_tip_sp(state_codes = c('PR', 'VI'), sp_codes = 170867)
  # Red Hind (PR, VI) (167700) (SEDAR 35)
  cr_tip_sp(state_codes = c('PR', 'VI'), sp_codes = 167700)
  
  
# Read in data
  # Yellowtail Snapper
  com_tip_ys <- readRDS("~/SEFSC-SFD-CFB-TIP-Compositions/data/raw/20230802/com_tip_PR_VI_168907_20230802.RDS")
  # Hogfish 
  com_tip_hog <- readRDS("~/SEFSC-SFD-CFB-TIP-Compositions/data/raw/20230802/com_tip_PR_VI_170566_20230802.RDS")
  # Spiny Lobster
  com_tip_lob <- readRDS("~/SEFSC-SFD-CFB-TIP-Compositions/data/raw/20230802/com_tip_PR_VI_97648_97646_20230802.RDS")
  # Queen Triggerfish
  com_tip_qt <- readRDS("~/SEFSC-SFD-CFB-TIP-Compositions/data/raw/20230802/com_tip_PR_VI_173139_20230802.RDS")
  # Stoplight Parrotfish
  com_tip_sp <- readRDS("~/SEFSC-SFD-CFB-TIP-Compositions/data/raw/20230802/com_tip_PR_VI_170867_20230802.RDS")
  # Red Hind
  com_tip_rh <- readRDS("~/SEFSC-SFD-CFB-TIP-Compositions/data/raw/20230802/com_tip_PR_VI_167700_20230802.RDS")
 
# Create table to show how many na's in each column
  # Yellowtail snapper
    na_count_ys <-sapply(com_tip_ys, function(y) sum(length(which(is.na(y)))))
    na_count_ys <- data.frame(na_count_ys)
    view(na_count_ys) 
    # how many records? 
      nrow(com_tip_ys) # 130564 records total
  # Hogfish
    na_count_hog <-sapply(com_tip_hog, function(y) sum(length(which(is.na(y)))))
    na_count_hog <- data.frame(na_count_hog)
    view(na_count_hog)
    # how many records? 
      nrow(com_tip_hog) # 8672 records total
  # Spiny Lobster
    na_count_lob <-sapply(com_tip_lob, function(y) sum(length(which(is.na(y)))))
    na_count_lob <- data.frame(na_count_lob)
    view(na_count_lob)
    # how many records? 
      nrow(com_tip_lob) # 103281 records total
  # Queen Triggerfish
    na_count_qt <-sapply(com_tip_qt, function(y) sum(length(which(is.na(y)))))
    na_count_qt <- data.frame(na_count_qt)
    view(na_count_qt)
    # how many records? 
      nrow(com_tip_qt) # 36626 records total
  # Stoplight Parrotfish
    na_count_sp <-sapply(com_tip_sp, function(y) sum(length(which(is.na(y)))))
    na_count_sp <- data.frame(na_count_sp)
    view(na_count_sp)
    # how many records? 
       nrow(com_tip_sp) # 50720 records total
  # Red Hind 
    na_count_rh <-sapply(com_tip_rh, function(y) sum(length(which(is.na(y)))))
    na_count_rh <- data.frame(na_count_rh)
    view(na_count_rh)
    # how many records? 
      nrow(com_tip_rh) # 53335 records total
   
  # merge the tables to show total 
  na_count_merge <- merge(na_count_ys, na_count_hog, by = 'row.names', all = TRUE)
  na_count_merge_1 <- merge(na_count_lob, na_count_qt, by = 'row.names', all = TRUE)
  na_count_merge_2 <- merge(na_count_sp,  na_count_rh, by = 'row.names', all = TRUE) 
  
  # put all data frames into list
  count_list <- list(na_count_merge, na_count_merge_1, na_count_merge_2)
  # count_list <- list(na_count_ys, na_count_hog, na_count_lob, na_count_qt,na_count_sp,  na_count_rh) didn't work because they don't share a column name
  
  
  # merge all data frames in list
  count_list %>% reduce(full_join, by='Row.names')
  count_list <- data.frame(count_list)
  
  # remove extra columns 
  count_na = subset(count_list, select = -c(Row.names.1, Row.names.2))
  
 # save dataframe 
   save(count_na,file="count_na.Rda") # file is saved in data folder
  
  
  # # These are pretty much the columns we want to look at
  # com_tip_splob_skeleton <- com_tip_splob %>%
  #   select(ID, INTERVIEW_DATE, SAMPLE_AREA_STATE_CODE, SAMPLE_AREA_COUNTY_CODE,
  #          GEAR_1, GEARNAME_1, GEAR_QTY_1, GEAR_FREQUENCY1, MIN_DEPTH1, MAX_DEPTH1, 
  #          OBS_STANDARD_SPECIES_CODE, OBS_STANDARD_SPECIES_NAME, TRIP_DAYS_FISHED, 
  #          LENGTH1, LENGTH_UNIT1, LENGTH_UNIT_CODE1, LENGTH_TYPE1, LENGTH_TYPE_CODE1, 
  #          LENGTH1_MM, OBS_WEIGHT, OBS_WEIGHT_KG, OBS_WEIGHT_UNIT, OBS_WEIGHT_UNIT_ID)
  # colnames(com_tip_skeleton) #get column names
  # is.na(com_tip_skeleton) # true-false na?
  # colSums(is.na(com_tip_skeleton)) # how many na's in each column
  # which(colSums(is.na(com_tip_skeleton))>0) # Just the columns with na's
  # names(which(colSums(is.na(com_tip_skeleton))>0))  # 11 of 23 contain n/a
  # nrow(com_tip_skeleton) # 181284 total rows
  
 
    
     
# Initial trials #### 
  
  # Check if any columns are all n/a
  # all_miss <- apply(df, 2, function(x) all(is.na(x)))
  # names(all_miss[all_miss>0])
  
  # # How many n/a's in each column
  # na_count_lob <-sapply(com_tip_splob, function(y) sum(length(which(is.na(y)))))
  # na_count_lob <- data.frame(na_count_lob)
  # view(na_count_lob)
  # na_lob_merge <- na_count_lob %>%
  #   mutate(COLUMN_NAME = row)
  # 
  # # repeat with yellowtail snapper and stoplight parrotfish
  # na_count_yssp <-sapply(com_tip_YS_SP, function(y) sum(length(which(is.na(y)))))
  # na_count_yssp <- data.frame(na_count_yssp)
  # view(na_count_yssp)
  
# Caribbean Spiny lobster ###
  # Gather the data ###
  # cr_tip_sp(state_codes = c('PR', 'VI'), sp_codes = c(97648, 97646))
   # Read in data
    # com_tip_splob <- readRDS("~/SEFSC-SFD-CFB-TIP-Compositions/data/raw/com_tip_PR_VI_97648_97646_20230712.RDS")  
# U.S. Caribbean Data-Limited Species ###
  # Yellowtail Snapper (PR) (168907), Hogfish (PR) (170566), 
  # Spiny Lobster (STT, STX) (97648, 97646), Queen Triggerfish (STT) (173139), Stoplight Parrotfish (STX) (170867)
# Yellowtail snapper and spotlight parrotfish ###
# Gather the data ###
  # Caribbean Yellowtail Snapper (168907) and Stoplight Parrotfish (170867)
    # cr_tip_sp(state_codes = c('PR', 'VI'), sp_codes = c(168907, 170867))
  # Read in data
    # com_tip_YS_SP <- readRDS("~/SEFSC-SFD-CFB-TIP-Compositions/data/raw/com_tip_PR_VI_168907_170867_20230720.RDS" )

   # # These are pretty much the columns we want to look at
 #  com_tip_skeleton <- com_tip_YS_SP %>%
 #    select(ID, INTERVIEW_DATE, SAMPLE_AREA_STATE_CODE, SAMPLE_AREA_COUNTY_CODE,
 #           GEAR_1, GEARNAME_1, GEAR_QTY_1, GEAR_FREQUENCY1, MIN_DEPTH1, MAX_DEPTH1, 
 #           OBS_STANDARD_SPECIES_CODE, OBS_STANDARD_SPECIES_NAME, TRIP_DAYS_FISHED, 
 #           LENGTH1, LENGTH_UNIT1, LENGTH_UNIT_CODE1, LENGTH_TYPE1, LENGTH_TYPE_CODE1, 
 #           LENGTH1_MM, OBS_WEIGHT, OBS_WEIGHT_KG, OBS_WEIGHT_UNIT, OBS_WEIGHT_UNIT_ID)
 #  colnames(com_tip_skeleton) #get column names
 #  is.na(com_tip_skeleton) # true-false na?
 #  colSums(is.na(com_tip_skeleton)) # how many na's in each column
 #  which(colSums(is.na(com_tip_skeleton))>0) # Just the columns with na's
 #  names(which(colSums(is.na(com_tip_skeleton))>0))  # 11 of 23 contain n/a
 #  nrow(com_tip_skeleton) # 181284 total rows  
  
# # Find columns with NA  
#   summary(com_tip_YS_SP)
#   nrow(com_tip_YS_SP)
#   colnames(com_tip_YS_SP) #get column names
#   colnames(com_tip_YS_SP[,sapply(com_tip_YS_SP,is.numeric)]) #which columns are numeric?
#     # Find which columns contain NA
#     is.na(com_tip_YS_SP)
#     colSums(is.na(com_tip_YS_SP))
#     which(colSums(is.na(com_tip_YS_SP))>0)
#     names(which(colSums(is.na(com_tip_YS_SP))>0))  # 202 of 304 contain n/a
#   
# # How many n/a's in each column  
#   sum(is.na(com_tip_skeleton$GEAR_1)) # Repeat for every n/a variable
#   
# # Find columns with NA  
#   summary(com_tip_splob)
#   nrow(com_tip_splob)
#   colnames(com_tip_splob) #get column names
#   colnames(com_tip_splob[,sapply(com_tip_splob,is.numeric)]) #which columns are numeric? 99 total
#   # Find which columns contain NA
#   is.na(com_tip_splob)
#   colSums(is.na(com_tip_splob))
#   which(colSums(is.na(com_tip_splob))>0)
#   names(which(colSums(is.na(com_tip_splob))>0))  # 208 of 304 contain n/a
#   nrow(com_tip_splob) # 103249 total rows