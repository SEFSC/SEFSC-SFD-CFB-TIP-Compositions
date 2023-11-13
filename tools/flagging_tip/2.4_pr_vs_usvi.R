# separate USVI and PR data to see differences in SEDAR species reporting ####

library(ggplot2)
library(reshape2)

# gather VI data:
cr_tip_sp(state_codes = c('VI'), sp_codes = c(97648, 97646, 170566, 168907, 173139, 170867, 167700))
  
# gather PR data: 
cr_tip_sp(state_codes = c('PR'), sp_codes = c(97648, 97646, 170566, 168907, 173139, 170867, 167700))

# read in data: 
com_tip_PR_SEDAR <- readRDS("~/SEFSC-SFD-CFB-TIP-Compositions/data/raw/com_tip_PR_97648_97646_170566_168907_173139_170867_167700_20230822.RDS")

com_tip_VI_SEDAR <- readRDS("~/SEFSC-SFD-CFB-TIP-Compositions/data/raw/com_tip_VI_97648_97646_170566_168907_173139_170867_167700_20230822.RDS")

# PR ####

# Find which columns contain NA: PR
colnames(com_tip_PR_SEDAR)
is.na(com_tip_PR_SEDAR)
colSums(is.na(com_tip_PR_SEDAR))
which(colSums(is.na(com_tip_PR_SEDAR))>0)
names(which(colSums(is.na(com_tip_PR_SEDAR))>0))  # 200 of 305 contain NA -> 65.57377% contain NA's

# which columns have no NAs
which(colSums(is.na(com_tip_PR_SEDAR)) == 0)
names(which(colSums(is.na(com_tip_PR_SEDAR)) == 0)) #105/305 = 34.4262 - not accurate, redo

# which columns are all NAs
which(colSums(is.na(com_tip_PR_SEDAR)) == 240675)
names(which(colSums(is.na(com_tip_PR_SEDAR)) == 240675)) #46/305 = 15.0819 - not accurate, redo

# Filter to each year over the last decade and create table

com_tip_PR_13 <- com_tip_PR_SEDAR %>%
  filter(YEAR == 2013)
na_count_PR_13 <-sapply(com_tip_PR_13, function(y) sum(length(which(is.na(y)))))
na_count_PR_13 <- data.frame(na_count_PR_13)
na_count_PR_13_calc <- na_count_PR_13 %>%
  mutate(percent_na_PR_13 = ((na_count_PR_13/6273)*100)) %>%
  mutate(percent_reported_PR_13 = (100-percent_na_PR_13))

com_tip_PR_14 <- com_tip_PR_SEDAR %>%
  filter(YEAR == 2014)
na_count_PR_14 <-sapply(com_tip_PR_14, function(y) sum(length(which(is.na(y)))))
na_count_PR_14 <- data.frame(na_count_PR_14)
na_count_PR_14_calc <- na_count_PR_14 %>%
  mutate(percent_na_PR_14 = ((na_count_PR_14/8630)*100)) %>%
  mutate(percent_reported_PR_14 = (100-percent_na_PR_14))

com_tip_PR_15 <- com_tip_PR_SEDAR %>%
  filter(YEAR == 2015)
na_count_PR_15 <-sapply(com_tip_PR_15, function(y) sum(length(which(is.na(y)))))
na_count_PR_15 <- data.frame(na_count_PR_15)
na_count_PR_15_calc <- na_count_PR_15 %>%
  mutate(percent_na_PR_15 = ((na_count_PR_15/7966)*100)) %>%
  mutate(percent_reported_PR_15 = (100-percent_na_PR_15))

com_tip_PR_16 <- com_tip_PR_SEDAR %>%
  filter(YEAR == 2016)
na_count_PR_16 <-sapply(com_tip_PR_16, function(y) sum(length(which(is.na(y)))))
na_count_PR_16 <- data.frame(na_count_PR_16)
na_count_PR_16_calc <- na_count_PR_16 %>%
  mutate(percent_na_PR_16 = ((na_count_PR_16/7064)*100)) %>%
  mutate(percent_reported_PR_16 = (100-percent_na_PR_16))

com_tip_PR_17 <- com_tip_PR_SEDAR %>%
  filter(YEAR == 2017)
na_count_PR_17 <-sapply(com_tip_PR_17, function(y) sum(length(which(is.na(y)))))
na_count_PR_17 <- data.frame(na_count_PR_17)
na_count_PR_17_calc <- na_count_PR_17 %>%
  mutate(percent_na_PR_17 = ((na_count_PR_17/3865)*100)) %>%
  mutate(percent_reported_PR_17 = (100-percent_na_PR_17))

com_tip_PR_18 <- com_tip_PR_SEDAR %>%
  filter(YEAR == 2018)
na_count_PR_18 <-sapply(com_tip_PR_18, function(y) sum(length(which(is.na(y)))))
na_count_PR_18 <- data.frame(na_count_PR_18)
na_count_PR_18_calc <- na_count_PR_18 %>%
  mutate(percent_na_PR_18 = ((na_count_PR_18/4130)*100)) %>%
  mutate(percent_reported_PR_18 = (100-percent_na_PR_18))

com_tip_PR_19 <- com_tip_PR_SEDAR %>%
  filter(YEAR == 2019)
na_count_PR_19 <-sapply(com_tip_PR_19, function(y) sum(length(which(is.na(y)))))
na_count_PR_19 <- data.frame(na_count_PR_19)
na_count_PR_19_calc <- na_count_PR_19 %>%
  mutate(percent_na_PR_19 = ((na_count_PR_19/5115)*100)) %>%
  mutate(percent_reported_PR_19 = (100-percent_na_PR_19))

com_tip_PR_20 <- com_tip_PR_SEDAR %>%
  filter(YEAR == 2020)
na_count_PR_20 <-sapply(com_tip_PR_20, function(y) sum(length(which(is.na(y)))))
na_count_PR_20 <- data.frame(na_count_PR_20)
na_count_PR_20_calc <- na_count_PR_20 %>%
  mutate(percent_na_PR_20 = ((na_count_PR_20/351)*100)) %>%
  mutate(percent_reported_PR_20 = (100-percent_na_PR_20))

com_tip_PR_21 <- com_tip_PR_SEDAR %>%
  filter(YEAR == 2021)
na_count_PR_21 <-sapply(com_tip_PR_21, function(y) sum(length(which(is.na(y)))))
na_count_PR_21 <- data.frame(na_count_PR_21)
na_count_PR_21_calc <- na_count_PR_21 %>%
  mutate(percent_na_PR_21 = ((na_count_PR_21/1231)*100)) %>%
  mutate(percent_reported_PR_21 = (100-percent_na_PR_21))

com_tip_PR_22 <- com_tip_PR_SEDAR %>%
  filter(YEAR == 2022)
na_count_PR_22 <-sapply(com_tip_PR_22, function(y) sum(length(which(is.na(y)))))
na_count_PR_22 <- data.frame(na_count_PR_22)
na_count_PR_22_calc <- na_count_PR_22 %>%
  mutate(percent_na_PR_22 = ((na_count_PR_22/3457)*100)) %>%
  mutate(percent_reported_PR_22 = (100-percent_na_PR_22))

# Merge tables 

# merge the tables to show total 
na_count_merge_PR <- merge(na_count_PR_13_calc, na_count_PR_14_calc, by = 'row.names', all = TRUE)
na_count_merge_PR_1 <- merge(na_count_PR_15_calc, na_count_PR_16_calc, by = 'row.names', all = TRUE)
na_count_merge_PR_2 <- merge(na_count_PR_17_calc,  na_count_PR_18_calc, by = 'row.names', all = TRUE) 
na_count_merge_PR_3 <- merge(na_count_PR_19_calc,  na_count_PR_20_calc, by = 'row.names', all = TRUE) 
na_count_merge_PR_4 <- merge(na_count_PR_21_calc,  na_count_PR_22_calc, by = 'row.names', all = TRUE) 


# put all data frames into list
count_list_PR <- list(na_count_merge_PR, na_count_merge_PR_1, na_count_merge_PR_2, 
                      na_count_merge_PR_3, na_count_merge_PR_4)

# merge all data frames in list
count_list_PR %>% reduce(full_join, by='Row.names')
count_list_PR <- data.frame(count_list_PR)

# remove extra columns 
count_na_PR = subset(count_list_PR, select = -c(Row.names.1, Row.names.2, Row.names.3, Row.names.4))

# save dataframe 
save(count_na_PR,file="count_na_PR.Rda") # file is saved in data folder

----------------------------------------------------

# Reporting visualization PR  ####

# Load data 
load(file = "~/SEFSC-SFD-CFB-TIP-Compositions/data/count_na_PR.Rda")

# Clean table 

View(count_na_PR)
str(count_reported_PR)
str(count_reported_PR_year)

# Subset to just % reported and clean dataframe
count_reported_PR = subset(count_na_PR, select = c(Row.names, percent_reported_PR_13, percent_reported_PR_14, percent_reported_PR_15, percent_reported_PR_16,
                                                   percent_reported_PR_17, percent_reported_PR_18, percent_reported_PR_19, percent_reported_PR_20,
                                                   percent_reported_PR_21, percent_reported_PR_22))

count_reported_PR_year <- count_reported_PR %>%
  rename("2013" = percent_reported_PR_13,
         "2014" = percent_reported_PR_14,
         "2015" = percent_reported_PR_15,
         "2016" = percent_reported_PR_16,
         "2017" = percent_reported_PR_17,
         "2018" = percent_reported_PR_18,
         "2019" = percent_reported_PR_19,
         "2020" = percent_reported_PR_20,
         "2021" = percent_reported_PR_21,
         "2022" = percent_reported_PR_22)


# make row.names the row names
count_reported_PR_clean <- count_reported_PR_year[,-1]
rownames(count_reported_PR_clean) <- count_reported_PR_year[,1]

str(count_reported_PR_clean)

# save dataframe
save(count_reported_PR_clean,file="count_reported_PR_clean.Rda") # file is saved in data folder

# Load new data 
# load(file = "data/count_reported_PR_clean.Rda")

view(count_reported_PR_clean)

# find mean reporting frequency - each yr about 54-55% across all variables 
count_means_PR <- map(count_reported_PR_clean, mean)
count_means <- map_dbl(count_reported_PR_clean, mean)

view(count_means)
str(count_means)
str(count_reported_PR_clean)

# remove variables that are either all or no reporting
  # remove 0's
  count_reported_PR_reduced <- count_reported_PR_clean[apply(count_reported_PR_clean[,-1], 1, function(x) !all(x==0)),]
  # remove 100's
  count_reported_PR_reduced2 <- count_reported_PR_reduced[apply(count_reported_PR_reduced[,-1], 1, function(x) !all(x==100)),]
    # how many variables are we left with? - 117 of 305 variables 
    rownames(count_reported_PR_reduced2)

# plot one row at a time to show variance across yrs ####

# transpose dataframe so that year is x (row) and variables are y (columns)
count_reported_PR_transposed <- data.frame(t(count_reported_PR_year[-1]))
colnames(count_reported_PR_transposed) <- count_reported_PR_year[, 1]

# remove variables that are either all or no reporting ####
  # remove all 0's columns 
  count_reported_PR_transposed_reduced <- count_reported_PR_transposed[, colSums(count_reported_PR_transposed != 0) > 0]
  # remove 100's
  vapply(count_reported_PR_transposed_reduced, function(x) length(unique(x)) > 1, logical(1L))
  count_reported_PR_transposed_reduced2 <- count_reported_PR_transposed_reduced[vapply(count_reported_PR_transposed_reduced, function(x) length(unique(x)) > 1, logical(1L))]

# Rename to shorter name :) 
count_reported_PR_plot <- count_reported_PR_transposed_reduced2

# Select columns with 1-99% reporting consistency ####

  colMeans(count_reported_PR_plot) #gives means of each column 
  
  count_PR_averages <- count_reported_PR_year %>%
    mutate(mean_reporting = rowMeans(select(count_reported_PR_year, "2013":"2022"), na.rm = TRUE))
  
  # find which variables are 100% 
  DONT_plot_these_PR_100 <- count_PR_averages[count_PR_averages$mean_reporting == 100,]
  
  # find which variables are 0%  
  DONT_plot_these_PR_0 <- count_PR_averages[count_PR_averages$mean_reporting == 0,]

  # these are the variables we want to plot
  plot_these_PR <- count_PR_averages[count_PR_averages$mean_reporting <99 & count_PR_averages$mean_reporting > 1,]
  # ALSO FIND WHICH VARIABLES ARE <1% AND >99% 
  DONT_plot_these_PR_99 <- count_PR_averages[count_PR_averages$mean_reporting <100 & count_PR_averages$mean_reporting >99,]

  DONT_plot_these_PR_1 <- count_PR_averages[count_PR_averages$mean_reporting >0 & count_PR_averages$mean_reporting < 1,]

  
# transpose the table again for easier plotting and remove mean_reporting variable 
  plot_these_PR_clean <- subset(plot_these_PR, select = -mean_reporting)
  plot_these_PR_transposed <- data.frame(t(plot_these_PR_clean[-1]))
  colnames(plot_these_PR_transposed) <- plot_these_PR_clean[, 1]
  
  colnames(plot_these_PR_transposed) #61 variables with consistencies between 1-99% 

  # Rename to shorter name :) 
  highlighted_PR <- plot_these_PR_transposed
  
  highlighted_PR$rn <- row.names(plot_these_PR_transposed) 

# Variables with same percentages
  # AREA2/AREANAME_2
  # LAND_AREA/LAND_AREA_NAME
  # LAND_MODIFIED/LAND_MODIFIED_DATE
  # LAND_STANDARD_AREA_ID/LAND_STANDARD_AREA_NAME
  # LAND_WEIGHT/LAND_WEIGHT_KG
  # MAX_DEPTH1/MIN_DEPTH1
  # MAX_DEPTH2/MIN_DEPTH2
  # OBS_MODIFIED_BY/OBS_MODIFIED_DATE
  # OBS_OLD_SAMPLE_ID/OBS_REPLICATE
  # OBS_WEIGHT/OBS_WEIGHT_KG
  # SAMPLE_MODIFIED_BY/SAMPLE_MODIFIED_DATE
  # SAMPLE_REPLICATE/SAMPLE_SAMPLE_NUMBER_OLD
  # SAMPLE_TYPE_1/SAMPLE_TYPE_2/SAMPLE_TYPE_3
  # SAMPLE_WEIGHT/SAMPLE_WEIGHT_KG

# Create new table with combined column titles and remove duplicate columns
  highlighted_PR_condensed <- highlighted_PR %>%
    select(-AREANAME_2, -LAND_AREA_NAME, -LAND_MODIFIED_DATE, -LAND_STANDARD_AREA_NAME,
           -LAND_WEIGHT_KG, -MIN_DEPTH1, -MIN_DEPTH2, -OBS_MODIFIED_DATE, -OBS_REPLICATE,
           -OBS_WEIGHT_KG, -SAMPLE_MODIFIED_DATE, -SAMPLE_SAMPLE_NUMBER_OLD, 
           -SAMPLE_TYPE3, -SAMPLE_TYPE_2, -SAMPLE_WEIGHT_KG, -STANDARDAREANAME_2,
           -STANDARDGEARNAME_2) %>%
    rename(AREA_2.AREANAME_2 = AREA_2,
           LAND_AREA.LAND_AREA_NAME = LAND_AREA,
           LAND_STANDARD_AREA_ID.LAND_STANDARD_AREA_NAME = LAND_STANDARD_AREA_ID,
           LAND_WEIGHT.LAND_WEIGHT_KG = LAND_WEIGHT,
           MAX_DEPTH1.MIN_DEPTH1 = MAX_DEPTH1,
           MAX_DEPTH2.MIN_DEPTH2 = MAX_DEPTH2,
           OBS_MODIFIED_BY.OBS_MODIFIED_DATE = OBS_MODIFIED_BY,
           OBS_OLD_SAMPLE_ID.OBS_REPLICATE = OBS_OLD_SAMPLE_ID,
           OBS_WEIGHT.OBS_WEIGHT_KG = OBS_WEIGHT,
           SAMPLE_MODIFIED_BY.SAMPLE_MODIFIED_DATE = SAMPLE_MODIFIED_BY,
           SAMPLE_REPLICATE.SAMPLE_SAMPLE_NUMBER_OLD = SAMPLE_REPLICATE,
           SAMPLE_TYPE_1.SAMPLE_TYPE_2.SAMPLE_TYPE_3 = SAMPLE_TYPE_1,
           SAMPLE_WEIGHT.SAMPLE_WEIGHT_KG = SAMPLE_WEIGHT,
           STANDARDAREA_2.STANDARDAREANAME_2.STANDARDGEARNAME_2 = STANDARDAREA_2,
           Year = rn) 
  colnames(highlighted_PR_condensed) # 45 columns now 
  
  save(highlighted_PR_condensed,file="highlighted_PR_condensed.Rda") # file is saved in data folder
  
  # Begin Plotting ####
  
  # Load new data 
  load(file = "data/highlighted_PR_condensed.Rda")
  
# plot by sections
  
  # interview - 
  # BEGIN_HOUR_MIN, BIAS_TYPE, CHECKED_DATE, CHECKER, 
  # CREW_SIZE, DEALER, DEALER_CODE, END_HOUR_MIN, INT_TYPE, INTERVIEW_COMMENT,
  # LANDING_AREA_COUNTY_CODE, LANDING_AREA_PLACE_CODE, LANDING_DATE, 
  # VESSEL_ID, OBSERVER_ONBOARD, 
  # OFFICIAL_VESSEL_NAME, VESSEL_NAME, PURCHASING_DEALER_CODE1, 
  # PURCHASING_DEALER_COUNTY_CODE1, PURCHASING_DEALER_COUNTY_NAME1, 
  # PURCHASING_DEALER_NAME1, PURCHASING_DEALER_STATES_CODE1, TICKET, 
  # TICKET_AGENCY, TICKET_AGENCY2, TICKET2, NBR_VESSELS, LICENSE, TRIP_NUMBER,
  # START_DATE, UNLOAD_DATE, TRIP_DAYS_OUT, TRIP_DAYS_FISHED, PLACE_LANDED
  
  INTERVIEW_VARIABLES_PLOT_PR <- highlighted_PR_condensed %>%
    pivot_longer(c(INTERVIEW_COMMENT, LANDING_AREA_COUNTY_CODE,LANDING_AREA_PLACE_CODE,
                   LANDING_DATE, OBSERVER_ONBOARD, START_DATE, TICKET_AGENCY2, TRIP_NUMBER,
                   UNLOAD_DATE, TRIP_DAYS_OUT, TRIP_DAYS_FISHED),
                 names_to = "variable", values_to = "percentage") %>%
    ggplot(aes(x=Year, y=percentage , group=variable , colour=variable)) +
    facet_wrap(vars(variable), ncol = 3) +
    geom_line() +
    geom_point() +
    labs(x = "Year", y = "Percent Reported (%)", 
         title = "TIP Reporting Consistency - INTERVIEW VARIABLES PR") +
    theme(legend.position = "none")
  
  ggsave("INTERVIEW_VARIABLES_PLOT_PR_nolabel.jpeg", plot = INTERVIEW_VARIABLES_PLOT_PR, width = 15,
         height = 8)

  # observation - 
  # CONDITION_TYPE, LENGTH_INCREMENT, LENGTH1, 
  # LENGTH1_MM, LENGTH2, LENGTH2_MM, OBS_COMMENT, OBS_GROUP,
  # OBS_MODIFIED_BY, OBS_MODIFIED_DATE, OBS_OLD_SAMPLE_ID, OBS_REPLICATE, 
  # OBS_WEIGHT, OBS_WEIGHT_KG, SEX_NAME, SECONDARY_SEX, 
  
  OBSERVATION_VARIABLES_PLOT_PR <- highlighted_PR_condensed %>%
    pivot_longer(c(OBS_GROUP, OBS_MODIFIED_BY.OBS_MODIFIED_DATE, OBS_OLD_SAMPLE_ID.OBS_REPLICATE, 
                   OBS_WEIGHT.OBS_WEIGHT_KG, SECONDARY_SEX),
                 names_to = "variable", values_to = "percentage") %>%
    ggplot(aes(x=Year, y=percentage , group=variable , colour=variable)) +
    facet_wrap(vars(variable), ncol = 2) +
    geom_line() +
    geom_point() +
    labs(x = "Year", y = "Percent Reported (%)", 
         title = "TIP Reporting Consistency - OBSERVATION VARIABLES PR") +
    theme(legend.position = "none")
  
  ggsave("OBSERVATION_VARIABLES_PLOT_PR_nolabel.jpeg", plot = OBSERVATION_VARIABLES_PLOT_PR, width = 10,
         height = 6)
  
  # sample - 
  # SAMPLE_COMMENT, SAMPLE_MODIFIED_BY, SAMPLE_MODIFIED_DATE, SAMPLE_REPLICATE, 
  # SAMPLE_SAMPLE_NUMBER_OLD, SAMPLE_SCIENTIFIC_NAME, SAMPLE_STANDARD_SPECIES_CODE,
  # SAMPLE_STANDARD_SPECIES_NAME, SAMPLE_TYPE_1, SAMPLE_TYPE_2, SAMPLE_TYPE3, 
  # SAMPLE_WEIGHT, SAMPLE_WEIGHT_KG, S_SAMPLE_COUNT, SUB_SAMPLE_CONDITION,
  # SUB_SAMPLE_COUNT, SUB_SAMPLE_RANDOM, SUB_SAMPLE_WEIGHT, 
  # SUB_SAMPLE_WEIGHT_KG, SUB_SAMPLE_WEIGHT_UNIT,
  # SUB_SAMPLE_WEIGHT_UNIT_CODE
  
  SAMPLE_VARIABLES_PLOT_PR <- highlighted_PR_condensed %>%
    pivot_longer(c(SAMPLE_MODIFIED_BY.SAMPLE_MODIFIED_DATE, SAMPLE_REPLICATE.SAMPLE_SAMPLE_NUMBER_OLD, 
                   SAMPLE_TYPE_1.SAMPLE_TYPE_2.SAMPLE_TYPE_3, SAMPLE_WEIGHT.SAMPLE_WEIGHT_KG, 
                   S_SAMPLE_COUNT),
                 names_to = "variable", values_to = "percentage") %>%
    ggplot(aes(x=Year, y=percentage , group=variable , colour=variable)) +
    facet_wrap(vars(variable), ncol = 2) +
    geom_line() +
    geom_point() +
    labs(x = "Year", y = "Percent Reported (%)", 
         title = "TIP Reporting Consistency - SAMPLE VARIABLES PR") +
    theme(legend.position = "none")
  
  ggsave("SAMPLE_VARIABLES_PLOT_PR_nolabel.jpeg", plot = SAMPLE_VARIABLES_PLOT_PR, width = 12,
         height = 6)
  
  # effort - 
  # EFFORT_INTERVIEW_ID, NUMBER_OF_EFFORT_RECORDS, GEAR_1, GEARNAME_1, 
  # STANDARDGEAR_1, STANDARDGEARNAME_1, GEAR_QTY_1, GEAR_FREQUENCY1, 
  # NUMBER_OF_GEAR_1, SOAK_TIME_1, REGIONNAME_1, AREA_1, AREANAME_1, 
  # STANDARDAREA_1, STANDARDAREANAME_1, MIN_DEPTH1, MAX_DEPTH1, EFF_CREATED_DATE1,
  # EFF_MODIFIED_DATE1, GEAR_2,
  # GEARNAME_2, STANDARDGEAR_1, STANDARDGEARNAME_2,GEAR_QTY_2, 
  # GEAR_FREQUENCY2, NUMBER_OF_GEAR_2, SOAK_TIME_2, REGIONNAME_2,
  # AREA_2, AREANAME_2,STANDARDAREA_2, STANDARDAREANAME_2, MIN_DEPTH2, 
  # MAX_DEPTH2,EFF_CREATED_DATE2, EFF_MODIFIED_DATE2, GEAR_3, GEARNAME_3, 
  # STANDARDGEAR_3, STANDARDGEARNAME_3, 
  # GEAR_QTY_3, GEAR_FREQUENCY3, NUMBER_OF_GEAR_3, SOAK_TIME_3, REGIONNAME_3, 
  # AREA_3, AREANAME_3, STANDARDAREA_3, STANDARDAREANAME_3, MIN_DEPTH3, 
  # MAX_DEPTH3, EFF_CREATED_DATE3, EFF_MODIFIED_DATE3,GEARNAME_4, STANDARDGEAR_4,
  # GEARNAME_5, STANDARDGEAR_5
  
  EFFORT_VARIABLES_PLOT_PR <- highlighted_PR_condensed %>%
    pivot_longer(c(GEAR_QTY_1, GEAR_FREQUENCY1, NUMBER_OF_GEAR_1, SOAK_TIME_1, MAX_DEPTH1.MIN_DEPTH1,
                   EFF_MODIFIED_DATE1, GEAR_2,GEAR_QTY_2, 
                   GEAR_FREQUENCY2, NUMBER_OF_GEAR_2, SOAK_TIME_2, REGIONNAME_2,
                   AREA_2.AREANAME_2, STANDARDAREA_2.STANDARDAREANAME_2.STANDARDGEARNAME_2,
                   MAX_DEPTH2.MIN_DEPTH2, EFF_CREATED_DATE2),
                 names_to = "variable", values_to = "percentage") %>%
    ggplot(aes(x=Year, y=percentage , group=variable , colour=variable)) +
    facet_wrap(vars(variable), ncol = 3) +
    geom_line() +
    geom_point() +
    labs(x = "Year", y = "Percent Reported (%)", 
         title = "TIP Reporting Consistency - EFFORT VARIABLES PR") +
    theme(legend.position = "none")
  
  ggsave("EFFORT_VARIABLES_PLOT_PR_nolabel.jpeg", plot = EFFORT_VARIABLES_PLOT_PR, width = 15,
         height = 8)
  
  # landing - 
  # LAND_AREA, 
  # LAND_AREA_NAME, LAND_GEAR_NAME, LAND_MODIFIED_BY, LAND_MODIFIED_DATE,
  # LAND_REGION, LAND_REPLICATE, LAND_SAMPLE_COUNT, LAND_SCIENTIFIC_NAME,
  # LAND_STANDARD_AREA_ID, LAND_STANDARD_AREA_NAME, LAND_STANDARD_SPC_NAME, 
  # LAND_STANDARD_SPECIES_CODE, LAND_WEIGHT, LAND_WEIGHT_KG, LANDING_COMMENT,
  # 
  
  LANDING_VARIABLES_PLOT_PR <- highlighted_PR_condensed %>%
    pivot_longer(c(LAND_AREA.LAND_AREA_NAME, LAND_MODIFIED_BY, 
                   LAND_REGION, LAND_REPLICATE, LAND_SAMPLE_COUNT,
                   LAND_STANDARD_AREA_ID.LAND_STANDARD_AREA_NAME, 
                   LAND_WEIGHT.LAND_WEIGHT_KG),
                 names_to = "variable", values_to = "percentage") %>%
    ggplot(aes(x=Year, y=percentage , group=variable , colour=variable)) +
    facet_wrap(vars(variable), ncol = 3) +
    geom_line() +
    geom_point() +
    labs(x = "Year", y = "Percent Reported (%)", 
         title = "TIP Reporting Consistency - LANDING VARIABLES PR") +
    theme(legend.position = "none")
  
  ggsave("LANDING_VARIABLES_PLOT_PR_nolabel.jpeg", plot = LANDING_VARIABLES_PLOT_PR, width = 15,
         height = 6)
  
  
#  ---------------------------------------------------


# VI ####

# Find which columns contain NA: VI

is.na(com_tip_VI_SEDAR)
colSums(is.na(com_tip_VI_SEDAR))
which(colSums(is.na(com_tip_VI_SEDAR))>0)
names(which(colSums(is.na(com_tip_VI_SEDAR))>0))  # 204 of 305 contain n/a
colnames(com_tip_VI_SEDAR)

# which columns have no NAs
which(colSums(is.na(com_tip_VI_SEDAR)) == 0)
names(which(colSums(is.na(com_tip_VI_SEDAR)) == 0)) #101/305 = 33.1147

# which columns are all NAs
which(colSums(is.na(com_tip_VI_SEDAR)) == 142659)
names(which(colSums(is.na(com_tip_VI_SEDAR)) == 142659)) # 37/305 = 12.1311

# Filter to each year over the last decade and create table

com_tip_VI_13 <- com_tip_VI_SEDAR %>%
  filter(YEAR == 2013)
na_count_VI_13 <-sapply(com_tip_VI_13, function(y) sum(length(which(is.na(y)))))
na_count_VI_13 <- data.frame(na_count_VI_13)
na_count_VI_13_calc <- na_count_VI_13 %>%
  mutate(percent_na_VI_13 = ((na_count_VI_13/1158)*100)) %>%
  mutate(percent_reported_VI_13 = (100-percent_na_VI_13))

com_tip_VI_14 <- com_tip_VI_SEDAR %>%
  filter(YEAR == 2014)
na_count_VI_14 <-sapply(com_tip_VI_14, function(y) sum(length(which(is.na(y)))))
na_count_VI_14 <- data.frame(na_count_VI_14)
na_count_VI_14_calc <- na_count_VI_14 %>%
  mutate(percent_na_VI_14 = ((na_count_VI_14/95)*100)) %>%
  mutate(percent_reported_VI_14 = (100-percent_na_VI_14))

com_tip_VI_15 <- com_tip_VI_SEDAR %>%
  filter(YEAR == 2015)
na_count_VI_15 <-sapply(com_tip_VI_15, function(y) sum(length(which(is.na(y)))))
na_count_VI_15 <- data.frame(na_count_VI_15)
na_count_VI_15_calc <- na_count_VI_15 %>%
  mutate(percent_na_VI_15 = ((na_count_VI_15/282)*100)) %>%
  mutate(percent_reported_VI_15 = (100-percent_na_VI_15))

com_tip_VI_16 <- com_tip_VI_SEDAR %>%
  filter(YEAR == 2016)
na_count_VI_16 <-sapply(com_tip_VI_16, function(y) sum(length(which(is.na(y)))))
na_count_VI_16 <- data.frame(na_count_VI_16)
na_count_VI_16_calc <- na_count_VI_16 %>%
  mutate(percent_na_VI_16 = ((na_count_VI_16/4842)*100)) %>%
  mutate(percent_reported_VI_16 = (100-percent_na_VI_16))

com_tip_VI_17 <- com_tip_VI_SEDAR %>%
  filter(YEAR == 2017)
na_count_VI_17 <-sapply(com_tip_VI_17, function(y) sum(length(which(is.na(y)))))
na_count_VI_17 <- data.frame(na_count_VI_17)
na_count_VI_17_calc <- na_count_VI_17 %>%
  mutate(percent_na_VI_17 = ((na_count_VI_17/5930)*100)) %>%
  mutate(percent_reported_VI_17 = (100-percent_na_VI_17))

com_tip_VI_18 <- com_tip_VI_SEDAR %>%
  filter(YEAR == 2018)
na_count_VI_18 <-sapply(com_tip_VI_18, function(y) sum(length(which(is.na(y)))))
na_count_VI_18 <- data.frame(na_count_VI_18)
na_count_VI_18_calc <- na_count_VI_18 %>%
  mutate(percent_na_VI_18 = ((na_count_VI_18/5330)*100)) %>%
  mutate(percent_reported_VI_18 = (100-percent_na_VI_18))

com_tip_VI_19 <- com_tip_VI_SEDAR %>%
  filter(YEAR == 2019)
na_count_VI_19 <-sapply(com_tip_VI_19, function(y) sum(length(which(is.na(y)))))
na_count_VI_19 <- data.frame(na_count_VI_19)
na_count_VI_19_calc <- na_count_VI_19 %>%
  mutate(percent_na_VI_19 = ((na_count_VI_19/4721)*100)) %>%
  mutate(percent_reported_VI_19 = (100-percent_na_VI_19))

com_tip_VI_20 <- com_tip_VI_SEDAR %>%
  filter(YEAR == 2020)
na_count_VI_20 <-sapply(com_tip_VI_20, function(y) sum(length(which(is.na(y)))))
na_count_VI_20 <- data.frame(na_count_VI_20)
na_count_VI_20_calc <- na_count_VI_20 %>%
  mutate(percent_na_VI_20 = ((na_count_VI_20/627)*100)) %>%
  mutate(percent_reported_VI_20 = (100-percent_na_VI_20))

com_tip_VI_21 <- com_tip_VI_SEDAR %>%
  filter(YEAR == 2021)
na_count_VI_21 <-sapply(com_tip_VI_21, function(y) sum(length(which(is.na(y)))))
na_count_VI_21 <- data.frame(na_count_VI_21)
na_count_VI_21_calc <- na_count_VI_21 %>%
  mutate(percent_na_VI_21 = ((na_count_VI_21/1039)*100)) %>%
  mutate(percent_reported_VI_21 = (100-percent_na_VI_21))

com_tip_VI_22 <- com_tip_VI_SEDAR %>%
  filter(YEAR == 2022)
na_count_VI_22 <-sapply(com_tip_VI_22, function(y) sum(length(which(is.na(y)))))
na_count_VI_22 <- data.frame(na_count_VI_22)
na_count_VI_22_calc <- na_count_VI_22 %>%
  mutate(percent_na_VI_22 = ((na_count_VI_22/1692)*100)) %>%
  mutate(percent_reported_VI_22 = (100-percent_na_VI_22))

# Merge tables 

# merge the tables to show total 
na_count_merge_VI <- merge(na_count_VI_13_calc, na_count_VI_14_calc, by = 'row.names', all = TRUE)
na_count_merge_VI_1 <- merge(na_count_VI_15_calc, na_count_VI_16_calc, by = 'row.names', all = TRUE)
na_count_merge_VI_2 <- merge(na_count_VI_17_calc,  na_count_VI_18_calc, by = 'row.names', all = TRUE) 
na_count_merge_VI_3 <- merge(na_count_VI_19_calc,  na_count_VI_20_calc, by = 'row.names', all = TRUE) 
na_count_merge_VI_4 <- merge(na_count_VI_21_calc,  na_count_VI_22_calc, by = 'row.names', all = TRUE) 


# put all data frames into list
count_list_VI <- list(na_count_merge_VI, na_count_merge_VI_1, na_count_merge_VI_2, 
                         na_count_merge_VI_3, na_count_merge_VI_4)

# merge all data frames in list
count_list_VI %>% reduce(full_join, by='Row.names')
count_list_VI <- data.frame(count_list_VI)

# remove extra columns 
count_na_VI = subset(count_list_VI, select = -c(Row.names.1, Row.names.2, Row.names.3, Row.names.4))

# save dataframe 
save(count_na_VI,file="count_na_VI.Rda") # file is saved in data folder.


# -------------------------------------------------------------------------

# Reporting visualization VI ####

# Load data 
load(file = ("~/SEFSC-SFD-CFB-TIP-Compositions/data/count_na_VI.Rda"))

# Clean table 

View(count_na_VI)
str(count_reported_VI)
str(count_reported_sedar_year)

# Subset to just % reported and clean dataframe
count_reported_VI = subset(count_na_VI, select = c(Row.names, percent_reported_VI_13, percent_reported_VI_14, percent_reported_VI_15, percent_reported_VI_16,
                                                   percent_reported_VI_17, percent_reported_VI_18, percent_reported_VI_19, percent_reported_VI_20,
                                                   percent_reported_VI_21, percent_reported_VI_22))

count_reported_VI_year <- count_reported_VI %>%
  rename("2013" = percent_reported_VI_13,
         "2014" = percent_reported_VI_14,
         "2015" = percent_reported_VI_15,
         "2016" = percent_reported_VI_16,
         "2017" = percent_reported_VI_17,
         "2018" = percent_reported_VI_18,
         "2019" = percent_reported_VI_19,
         "2020" = percent_reported_VI_20,
         "2021" = percent_reported_VI_21,
         "2022" = percent_reported_VI_22)


# make row.names the row names
count_reported_VI_clean <- count_reported_VI_year[,-1]
rownames(count_reported_VI_clean) <- count_reported_VI_year[,1]

str(count_reported_VI_clean)

# save dataframe
save(count_reported_VI_clean,file="count_reported_VI_clean.Rda") # file is saved in data folder

 # Load new data 
load(file = "data/count_reported_VI_clean.Rda")

view(count_reported_VI_clean)

# find mean reporting frequency - each yr about 56-60% across all variables 
count_means <- map(count_reported_VI_clean, mean)
count_means <- map_dbl(count_reported_VI_clean, mean)
view(count_means)
str(count_means)
str(count_reported_VI_clean)

# remove variables that are either all or no reporting
# remove 0's
count_reported_VI_reduced <- count_reported_VI_clean[apply(count_reported_VI_clean[,-1], 1, function(x) !all(x==0)),]
# remove 100's
count_reported_VI_reduced2 <- count_reported_VI_reduced[apply(count_reported_VI_reduced[,-1], 1, function(x) !all(x==100)),]
# how many variables are we left with? - 116 of 305 variables 
rownames(count_reported_VI_reduced2)

# plot one row at a time to show variance across yrs ####
library(ggplot2)
library(reshape2)

# transpose dataframe so that year is x (row) and variables are y (columns)
count_reported_VI_transposed <- data.frame(t(count_reported_VI_year[-1]))
colnames(count_reported_VI_transposed) <- count_reported_VI_year[, 1]

# remove variables that are either all or no reporting ####
# remove all 0's columns 
count_reported_VI_transposed_reduced <- count_reported_VI_transposed[, colSums(count_reported_VI_transposed != 0) > 0]
# remove 100's
vapply(count_reported_VI_transposed_reduced, function(x) length(unique(x)) > 1, logical(1L))
count_reported_VI_transposed_reduced2 <- count_reported_VI_transposed_reduced[vapply(count_reported_VI_transposed_reduced, function(x) length(unique(x)) > 1, logical(1L))]

# Rename to shorter name :) 
count_reported_VI_plot <- count_reported_VI_transposed_reduced2


# Select columns with 1-99% reporting consistency ####

colMeans(count_reported_VI_plot) #gives means of each column 

count_VI_averages <- count_reported_VI_year %>%
  mutate(mean_reporting = rowMeans(select(count_reported_VI_year, "2013":"2022"), na.rm = TRUE))

# these are the variables we want to plot
plot_these_VI <- count_VI_averages[count_VI_averages$mean_reporting <99 & count_VI_averages$mean_reporting > 1,]

# find which variables are 100% 
DONT_plot_these_VI_100 <- count_VI_averages[count_VI_averages$mean_reporting == 100,]

# find which variables are 0%  
DONT_plot_these_VI_0 <- count_VI_averages[count_VI_averages$mean_reporting == 0,]

# ALSO FIND WHICH VARIABLES ARE <1% AND >99% 
DONT_plot_these_VI_99 <- count_VI_averages[count_VI_averages$mean_reporting <100 & count_VI_averages$mean_reporting >99,]

DONT_plot_these_VI_1 <- count_PR_averages[count_VI_averages$mean_reporting >0 & count_VI_averages$mean_reporting < 1,]


# transpose the table again for easier plotting and remove mean_reporting variable 
plot_these_VI_clean <- subset(plot_these_VI, select = -mean_reporting)
plot_these_VI_transposed <- data.frame(t(plot_these_VI_clean[-1]))
colnames(plot_these_VI_transposed) <- plot_these_VI_clean[, 1]

colnames(plot_these_VI_transposed) #62 variables with consistencies between 1-99% 

# Rename to shorter name :) 
highlighted_VI <- plot_these_VI_transposed


highlighted_VI$rn <- row.names(plot_these_VI_transposed) 


# Variables with same percentages
# AREA2/AREANAME_2
# LAND_AREA/LAND_AREA_NAME
# LAND_MODIFIED/LAND_MODIFIED_DATE
# LAND_STANDARD_AREA_ID/LAND_STANDARD_AREA_NAME
# LAND_WEIGHT/LAND_WEIGHT_KG
# MAX_DEPTH1/MIN_DEPTH1
# MAX_DEPTH2/MIN_DEPTH2
# OBS_MODIFIED_BY/OBS_MODIFIED_DATE
# OBS_OLD_SAMPLE_ID/OBS_REPLICATE
# OBS_WEIGHT/OBS_WEIGHT_KG
# SAMPLE_MODIFIED_BY/SAMPLE_MODIFIED_DATE
# SAMPLE_REPLICATE/SAMPLE_SAMPLE_NUMBER_OLD
# SAMPLE_TYPE_1/SAMPLE_TYPE_2/SAMPLE_TYPE_3
# SAMPLE_WEIGHT/SAMPLE_WEIGHT_KG

# Create new table with combined column titles and remove duplicate columns
highlighted_VI_condensed <- highlighted_VI %>%
  select(-AREANAME_2, -CHECKER, -DEALER_CODE, -GEAR_QTY_2, -EFF_CREATED_DATE2, -MIN_DEPTH2,
         -LAND_MODIFIED_DATE, -OBS_MODIFIED_DATE, -OBS_REPLICATE, -PURCHASING_DEALER_COUNTY_CODE1, -PURCHASING_DEALER_COUNTY_NAME1,
         -PURCHASING_DEALER_STATE_CODE1, -SAMPLE_MODIFIED_DATE, -SAMPLE_SAMPLE_NUMBER_OLD,
         -SAMPLE_STANDARD_SPECIES_CODE, -SAMPLE_STANDARD_SPECIES_NAME, -SAMPLE_TYPE_2, -SAMPLE_TYPE3,
         -STANDARDAREANAME_2, -TICKET_AGENCY) %>%
  rename(AREA_2.AREANAME_2 = AREA_2,
         CHECKED_DATE.CHECKER = CHECKED_DATE,
         DEALER.DEALER_CODE = DEALER, 
         EFF_CREATED_DATE2.GEAR_2.GEAR_QTY_2 = GEAR_2, 
         LAND_MODIFIED_BY.LAND_MODIFIED_DATE = LAND_MODIFIED_BY,
         MAX_DEPTH2.MIN_DEPTH2 = MAX_DEPTH2,
         OBS_MODIFIED_BY.OBS_MODIFIED_DATE = OBS_MODIFIED_BY,
         OBS_OLD_SAMPLE_ID.OBS_REPLICATE = OBS_OLD_SAMPLE_ID,
         PURCHASING_DEALER_CODE1.PURCHASING_DEALER_COUNTY_CODE1.PURCHASING_DEALER_COUNTY_NAME1.PURCHASING_DEALER_STATE_CODE1 = PURCHASING_DEALER_CODE1,
         SAMPLE_MODIFIED_BY.SAMPLE_MODIFIED_DATE = SAMPLE_MODIFIED_BY,
         SAMPLE_REPLICATE.SAMPLE_SAMPLE_NUMBER_OLD = SAMPLE_REPLICATE,
         SAMPLE_SCIENTIFIC_NAME.SAMPLE_STANDARD_SPECIES_CODE.SAMPLE_STANDARD_SPECIES_NAME = SAMPLE_SCIENTIFIC_NAME, 
         SAMPLE_TYPE_1.SAMPLE_TYPE_2.SAMPLE_TYPE_3 = SAMPLE_TYPE_1,
         STANDARDAREA_2.STANDARDAREANAME_2 = STANDARDAREA_2, 
         TICKET.TICKET_AGENCY = TICKET, 
         Year = rn) 
colnames(highlighted_VI_condensed) # 43 columns now 


save(highlighted_VI_condensed,file="highlighted_VI_condensed.Rda") # file is saved in data folder

# Begin Plotting ####

# Load new data 
load(file = "data/highlighted_VI_condensed.Rda")

# plot by sections

# interview - 
# BEGIN_HOUR_MIN, BIAS_TYPE, CHECKED_DATE, CHECKER, 
# CREW_SIZE, DEALER, DEALER_CODE, END_HOUR_MIN, INT_TYPE, INTERVIEW_COMMENT,
# LANDING_AREA_COUNTY_CODE, LANDING_AREA_PLACE_CODE, LANDING_DATE, 
# VESSEL_ID, OBSERVER_ONBOARD, 
# OFFICIAL_VESSEL_NAME, VESSEL_NAME, PURCHASING_DEALER_CODE1, 
# PURCHASING_DEALER_COUNTY_CODE1, PURCHASING_DEALER_COUNTY_NAME1, 
# PURCHASING_DEALER_NAME1, PURCHASING_DEALER_STATE_CODE1, TICKET, 
# TICKET_AGENCY, TICKET_AGENCY2, TICKET2, NBR_VESSELS, LICENSE, TRIP_NUMBER,
# START_DATE, UNLOAD_DATE, TRIP_DAYS_OUT, TRIP_DAYS_FISHED, PLACE_LANDED

INTERVIEW_VARIABLES_PLOT_VI <- highlighted_VI_condensed %>%
  pivot_longer(c(CHECKED_DATE.CHECKER, DEALER.DEALER_CODE, INTERVIEW_COMMENT,
                 LANDING_AREA_PLACE_CODE,VESSEL_ID, OBSERVER_ONBOARD, OFFICIAL_VESSEL_NAME, 
                 VESSEL_NAME, 
                 PURCHASING_DEALER_CODE1.PURCHASING_DEALER_COUNTY_CODE1.PURCHASING_DEALER_COUNTY_NAME1.PURCHASING_DEALER_STATE_CODE1,
                 PURCHASING_DEALER_NAME1, TICKET.TICKET_AGENCY, TICKET_AGENCY2, LICENSE,
                 UNLOAD_DATE, TRIP_DAYS_OUT, TRIP_DAYS_FISHED),
               names_to = "variable", values_to = "percentage") %>%
  ggplot(aes(x=Year, y=percentage , group=variable , colour=variable)) +
  facet_wrap(vars(variable), ncol = 3) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Percent Reported (%)", 
       title = "TIP Reporting Consistency - INTERVIEW VARIABLES VI") +
  theme(legend.position = "none")

ggsave("INTERVIEW_VARIABLES_PLOT_VI_nolabel.jpeg", plot = INTERVIEW_VARIABLES_PLOT_VI, width = 20,
       height = 12)

# observation - 
# CONDITION_TYPE, LENGTH_INCREMENT, LENGTH1, 
# LENGTH1_MM, LENGTH2, LENGTH2_MM, OBS_COMMENT, OBS_GROUP,
# OBS_MODIFIED_BY, OBS_MODIFIED_DATE, OBS_OLD_SAMPLE_ID, OBS_REPLICATE, 
# OBS_WEIGHT, OBS_WEIGHT_KG, SEX_NAME, SECONDARY_SEX, 

OBSERVATION_VARIABLES_PLOT_VI <- highlighted_VI_condensed %>%
  pivot_longer(c(OBS_GROUP, OBS_MODIFIED_BY.OBS_MODIFIED_DATE, OBS_OLD_SAMPLE_ID.OBS_REPLICATE),
               names_to = "variable", values_to = "percentage") %>%
  ggplot(aes(x=Year, y=percentage , group=variable , colour=variable)) +
  facet_wrap(vars(variable), ncol = 3) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Percent Reported (%)", 
       title = "TIP Reporting Consistency - OBSERVATION VARIABLES VI") +
  theme(legend.position = "none")

ggsave("OBSERVATION_VARIABLES_PLOT_VI_nolabel.jpeg", plot = OBSERVATION_VARIABLES_PLOT_VI, width = 15,
       height = 4)


# sample - 
# SAMPLE_COMMENT, SAMPLE_MODIFIED_BY, SAMPLE_MODIFIED_DATE, SAMPLE_REPLICATE, 
# SAMPLE_SAMPLE_NUMBER_OLD, SAMPLE_SCIENTIFIC_NAME, SAMPLE_STANDARD_SPECIES_CODE,
# SAMPLE_STANDARD_SPECIES_NAME, SAMPLE_TYPE_1, SAMPLE_TYPE_2, SAMPLE_TYPE3, 
# SAMPLE_WEIGHT, SAMPLE_WEIGHT_KG, S_SAMPLE_COUNT, SUB_SAMPLE_CONDITION,
# SUB_SAMPLE_COUNT, SUB_SAMPLE_RANDOM, SUB_SAMPLE_WEIGHT, 
# SUB_SAMPLE_WEIGHT_KG, SUB_SAMPLE_WEIGHT_UNIT,
# SUB_SAMPLE_WEIGHT_UNIT_CODE

SAMPLE_VARIABLES_PLOT_VI <- highlighted_VI_condensed %>%
  pivot_longer(c(SAMPLE_COMMENT, SAMPLE_MODIFIED_BY.SAMPLE_MODIFIED_DATE, 
                 SAMPLE_REPLICATE.SAMPLE_SAMPLE_NUMBER_OLD, SAMPLE_SCIENTIFIC_NAME.SAMPLE_STANDARD_SPECIES_CODE.SAMPLE_STANDARD_SPECIES_NAME, 
                 SAMPLE_TYPE_1.SAMPLE_TYPE_2.SAMPLE_TYPE_3),
               names_to = "variable", values_to = "percentage") %>%
  ggplot(aes(x=Year, y=percentage , group=variable , colour=variable)) +
  facet_wrap(vars(variable), ncol = 3) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Percent Reported (%)", 
       title = "TIP Reporting Consistency - SAMPLE VARIABLES VI") +
  theme(legend.position = "none")

ggsave("SAMPLE_VARIABLES_PLOT_VI_nolabel.jpeg", plot = SAMPLE_VARIABLES_PLOT_VI, width = 20,
       height = 6)

# effort - 
# EFFORT_INTERVIEW_ID, NUMBER_OF_EFFORT_RECORDS, GEAR_1, GEARNAME_1, 
# STANDARDGEAR_1, STANDARDGEARNAME_1, GEAR_QTY_1, GEAR_FREQUENCY1, 
# NUMBER_OF_GEAR_1, SOAK_TIME_1, REGIONNAME_1, AREA_1, AREANAME_1, 
# STANDARDAREA_1, STANDARDAREANAME_1, MIN_DEPTH1, MAX_DEPTH1, EFF_CREATED_DATE1,
# EFF_MODIFIED_DATE1, GEAR_2,
# GEARNAME_2, STANDARDGEAR_1, STANDARDGEARNAME_2,GEAR_QTY_2, 
# GEAR_FREQUENCY2, NUMBER_OF_GEAR_2, SOAK_TIME_2, REGIONNAME_2,
# AREA_2, AREANAME_2,STANDARDAREA_2, STANDARDAREANAME_2, MIN_DEPTH2, 
# MAX_DEPTH2,EFF_CREATED_DATE2, EFF_MODIFIED_DATE2, GEAR_3, GEARNAME_3, 
# STANDARDGEAR_3, STANDARDGEARNAME_3, 
# GEAR_QTY_3, GEAR_FREQUENCY3, NUMBER_OF_GEAR_3, SOAK_TIME_3, REGIONNAME_3, 
# AREA_3, AREANAME_3, STANDARDAREA_3, STANDARDAREANAME_3, MIN_DEPTH3, 
# MAX_DEPTH3, EFF_CREATED_DATE3, EFF_MODIFIED_DATE3,GEARNAME_4, STANDARDGEAR_4,
# GEARNAME_5, STANDARDGEAR_5

EFFORT_VARIABLES_PLOT_VI <- highlighted_VI_condensed %>%
  pivot_longer(c(GEAR_FREQUENCY1, NUMBER_OF_GEAR_1, SOAK_TIME_1, EFF_MODIFIED_DATE1, 
                 MAX_DEPTH1, STANDARDGEARNAME_2,
                 GEAR_FREQUENCY2, NUMBER_OF_GEAR_2, SOAK_TIME_2, REGIONNAME_2, 
                 AREA_2.AREANAME_2, STANDARDAREA_2.STANDARDAREANAME_2, MAX_DEPTH2.MIN_DEPTH2, 
                 EFF_MODIFIED_DATE2, EFF_CREATED_DATE2.GEAR_2.GEAR_QTY_2),
               names_to = "variable", values_to = "percentage") %>%
  ggplot(aes(x=Year, y=percentage , group=variable , colour=variable)) +
  facet_wrap(vars(variable), ncol = 3) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Percent Reported (%)", 
       title = "TIP Reporting Consistency - EFFORT VARIABLES VI") +
  theme(legend.position = "none")

ggsave("EFFORT_VARIABLES_PLOT_VI_nolabel.jpeg", plot = EFFORT_VARIABLES_PLOT_VI, width = 15,
       height = 8)

# landing - 
# LAND_AREA, 
# LAND_AREA_NAME, LAND_GEAR_NAME, LAND_MODIFIED_BY, LAND_MODIFIED_DATE,
# LAND_REGION, LAND_REPLICATE, LAND_SAMPLE_COUNT, LAND_SCIENTIFIC_NAME,
# LAND_STANDARD_AREA_ID, LAND_STANDARD_AREA_NAME, LAND_STANDARD_SPC_NAME, 
# LAND_STANDARD_SPECIES_CODE, LAND_WEIGHT, LAND_WEIGHT_KG, LANDING_COMMENT,
# 

LANDING_VARIABLES_PLOT_VI <- highlighted_VI_condensed %>%
  pivot_longer(c(LAND_MODIFIED_BY.LAND_MODIFIED_DATE, LAND_REPLICATE, LANDING_COMMENT),
               names_to = "variable", values_to = "percentage") %>%
  ggplot(aes(x=Year, y=percentage , group=variable , colour=variable)) +
  facet_wrap(vars(variable), ncol = 2) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Percent Reported (%)", 
       title = "TIP Reporting Consistency - LANDING VARIABLES VI") +
  theme(legend.position = "none")

ggsave("LANDING_VARIABLES_PLOT_VI_nolabel.jpeg", plot = LANDING_VARIABLES_PLOT_VI, width = 15,
       height = 6)


# end ---------------------------------------------------
  
