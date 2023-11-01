# reporting consistencies over time

# read in data for all sedar species
cr_tip_sp(state_codes = c('PR', 'VI'), sp_codes = c(97648, 97646, 170566, 168907, 173139, 170867, 167700))
          
com_tip_sedar <- readRDS("~/SEFSC-SFD-CFB-TIP-Compositions/data/raw/com_tip_PR_VI_97648_97646_170566_168907_173139_170867_167700_20230804.RDS")

# We need to show changes in reporting consistency across yrs
# remove variables that are all or nothing: 

  # Find which columns contain NA
  is.na(com_tip_sedar)
  colSums(is.na(com_tip_sedar))
  which(colSums(is.na(com_tip_sedar))>0)
  names(which(colSums(is.na(com_tip_sedar))>0))  # 208 of 304 contain n/a
  
  # which columns have no NAs
  which(colSums(is.na(com_tip_sedar)) == 0)
  names(which(colSums(is.na(com_tip_sedar)) == 0))
  
  # which columns are all NAs
  which(colSums(is.na(com_tip_sedar)) == 383323)
  names(which(colSums(is.na(com_tip_sedar)) == 383323))
  
  
  # na_count_sedar <-sapply(com_tip_sedar, function(y) sum(length(which(is.na(y)))))
  # na_count_sedar <- data.frame(na_count_sedar)
  # view(na_count_sedar)
  # 
  # filter(na_count_sedar %in% c(0, 383323))
  # na_count_sedar[na_count_sedar == 383323]
  
# Make a table for each variable that shows reporting consistency across years
  # x = yr y = % reported 
  # make table for each non- zero or 100% variable 
  
  # make table of NAs
  na_count_sedar <-sapply(com_tip_sedar, function(y) sum(length(which(is.na(y)))))
  na_count_sedar <- data.frame(na_count_sedar)
  view(na_count_sedar) 
  # 1980 is start year, probs just need to look at last decade first 
  
# Filter to each year over the last decade and create table
  # # 2012
  # com_tip_12 <- com_tip_sedar %>%
  #   filter(YEAR == 2012)
  # na_count_12 <-sapply(com_tip_12, function(y) sum(length(which(is.na(y)))))
  # na_count_12 <- data.frame(na_count_12)
  # na_count_12_calc <- na_count_12 %>%
  #   mutate(percent_na_12 = ((na_count_12/8961)*100)) %>%
  #   mutate(percent_reported_12 = (100-percent_na_12))
  # 2013
  com_tip_13 <- com_tip_sedar %>%
    filter(YEAR == 2013)
  na_count_13 <-sapply(com_tip_13, function(y) sum(length(which(is.na(y)))))
  na_count_13 <- data.frame(na_count_13)
  na_count_13_calc <- na_count_13 %>%
    mutate(percent_na_13 = ((na_count_13/7431)*100)) %>%
    mutate(percent_reported_13 = (100-percent_na_13))
  # 2014
  com_tip_14 <- com_tip_sedar %>%
    filter(YEAR == 2014)
  na_count_14 <-sapply(com_tip_14, function(y) sum(length(which(is.na(y)))))
  na_count_14 <- data.frame(na_count_14)
  na_count_14_calc <- na_count_14 %>%
    mutate(percent_na_14 = ((na_count_14/8725)*100)) %>%
    mutate(percent_reported_14 = (100-percent_na_14))
  # 2015
  com_tip_15 <- com_tip_sedar %>%
    filter(YEAR == 2015)
  na_count_15 <-sapply(com_tip_15, function(y) sum(length(which(is.na(y)))))
  na_count_15 <- data.frame(na_count_15)
  na_count_15_calc <- na_count_15 %>%
    mutate(percent_na_15 = ((na_count_15/8248)*100)) %>%
    mutate(percent_reported_15 = (100-percent_na_15))
  # 2016
  com_tip_16 <- com_tip_sedar %>%
    filter(YEAR == 2016)
  na_count_16 <-sapply(com_tip_16, function(y) sum(length(which(is.na(y)))))
  na_count_16 <- data.frame(na_count_16)
  na_count_16_calc <- na_count_16 %>%
    mutate(percent_na_16 = ((na_count_16/11906)*100)) %>%
    mutate(percent_reported_16 = (100-percent_na_16))
  # 2017
  com_tip_17 <- com_tip_sedar %>%
    filter(YEAR == 2017)
  na_count_17 <-sapply(com_tip_17, function(y) sum(length(which(is.na(y)))))
  na_count_17 <- data.frame(na_count_17)
  na_count_17_calc <- na_count_17 %>%
    mutate(percent_na_17 = ((na_count_17/9795)*100)) %>%
    mutate(percent_reported_17 = (100-percent_na_17))
  # 2018
  com_tip_18 <- com_tip_sedar %>%
    filter(YEAR == 2018)
  na_count_18 <-sapply(com_tip_18, function(y) sum(length(which(is.na(y)))))
  na_count_18 <- data.frame(na_count_18)
  na_count_18_calc <- na_count_18 %>%
    mutate(percent_na_18 = ((na_count_18/9460)*100)) %>%
    mutate(percent_reported_18 = (100-percent_na_18))
  # 2019
  com_tip_19 <- com_tip_sedar %>%
    filter(YEAR == 2019)
  na_count_19 <-sapply(com_tip_19, function(y) sum(length(which(is.na(y)))))
  na_count_19 <- data.frame(na_count_19)
  na_count_19_calc <- na_count_19 %>%
    mutate(percent_na_19 = ((na_count_19/9836)*100)) %>%
    mutate(percent_reported_19 = (100-percent_na_19))
  # 2020
  com_tip_20 <- com_tip_sedar %>%
    filter(YEAR == 2020)
  na_count_20 <-sapply(com_tip_20, function(y) sum(length(which(is.na(y)))))
  na_count_20 <- data.frame(na_count_20)
  na_count_20_calc <- na_count_20 %>%
    mutate(percent_na_20 = ((na_count_20/978)*100)) %>%
    mutate(percent_reported_20 = (100-percent_na_20))
  # 2021
  com_tip_21 <- com_tip_sedar %>%
    filter(YEAR == 2021)
  na_count_21 <-sapply(com_tip_21, function(y) sum(length(which(is.na(y)))))
  na_count_21 <- data.frame(na_count_21)
  na_count_21_calc <- na_count_21 %>%
    mutate(percent_na_21 = ((na_count_21/2270)*100)) %>%
    mutate(percent_reported_21 = (100-percent_na_21))
  # 2022
  com_tip_22 <- com_tip_sedar %>%
    filter(YEAR == 2022)
  na_count_22 <-sapply(com_tip_22, function(y) sum(length(which(is.na(y)))))
  na_count_22 <- data.frame(na_count_22)
  na_count_22_calc <- na_count_22 %>%
    mutate(percent_na_22 = ((na_count_22/5149)*100)) %>%
    mutate(percent_reported_22 = (100-percent_na_22))
  # # 2023
  # com_tip_23 <- com_tip_sedar %>%
  #   filter(YEAR == 2023)
  # na_count_23 <-sapply(com_tip_23, function(y) sum(length(which(is.na(y)))))
  # na_count_23 <- data.frame(na_count_23)
  # na_count_23_calc <- na_count_23 %>%
  #   mutate(percent_na_23 = ((na_count_23/1454)*100)) %>%
  #   mutate(percent_reported_23 = (100-percent_na_23))

# Merge tables 
  
  # merge the tables to show total 
  na_count_merge_sedar <- merge(na_count_13_calc, na_count_14_calc, by = 'row.names', all = TRUE)
  na_count_merge_sedar1 <- merge(na_count_15_calc, na_count_16_calc, by = 'row.names', all = TRUE)
  na_count_merge_sedar2 <- merge(na_count_17_calc,  na_count_18_calc, by = 'row.names', all = TRUE) 
  na_count_merge_sedar3 <- merge(na_count_19_calc,  na_count_20_calc, by = 'row.names', all = TRUE) 
  na_count_merge_sedar4 <- merge(na_count_21_calc,  na_count_22_calc, by = 'row.names', all = TRUE) 
  # na_count_merge_sedar5 <- merge(  na_count_23_calc, by = 'row.names', all = TRUE) 
  
  # put all data frames into list
  count_list_sedar <- list(na_count_merge_sedar, na_count_merge_sedar1, na_count_merge_sedar2, 
                     na_count_merge_sedar3, na_count_merge_sedar4)
  
  # merge all data frames in list
  count_list_sedar %>% reduce(full_join, by='Row.names')
  count_list_sedar <- data.frame(count_list_sedar)
  
  # remove extra columns 
  count_na_sedar = subset(count_list_sedar, select = -c(Row.names.1, Row.names.2, Row.names.3, Row.names.4))
  
  # save dataframe 
  save(count_na_sedar,file="count_na_sedar.Rda") # file is saved in data folder
# WRITE CSV
  write.csv(count_na_sedar, here("data", paste0("count_na_sedar_", format(Sys.Date(), "%Y%m%d"), ".csv")), row.names = TRUE)
  