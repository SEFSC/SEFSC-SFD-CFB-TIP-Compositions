# Reporting visualization  ####

# Load data 
  load(file = "data/count_na_sedar.Rda")

# Plot timeline of each variable ####

  View(count_na_sedar)
  str(count_reported_sedar)

# Subset to just % reported and clean dataframe
  count_reported_sedar = subset(count_na_sedar, select = c(Row.names, percent_reported_13, percent_reported_14, percent_reported_15, percent_reported_16,
                                                           percent_reported_17, percent_reported_18, percent_reported_19, percent_reported_20,
                                                           percent_reported_21, percent_reported_22))
  
  count_reported_sedar_year <- count_reported_sedar %>%
    rename("2013" = percent_reported_13,
           "2014" = percent_reported_14,
           "2015" = percent_reported_15,
           "2016" = percent_reported_16,
           "2017" = percent_reported_17,
           "2018" = percent_reported_18,
           "2019" = percent_reported_19,
           "2020" = percent_reported_20,
           "2021" = percent_reported_21,
           "2022" = percent_reported_22)
  
 
  
  str(count_reported_sedar_year)

# make row.names the row names
  count_reported_sedar_clean <- count_reported_sedar_year[,-1]
  rownames(count_reported_sedar_clean) <- count_reported_sedar_year[,1]
  
  # WRITE CSV
  write.csv(count_reported_sedar_clean, here("data", paste0("count_reported_sedar_clean_", format(Sys.Date(), "%Y%m%d"), ".csv")), row.names = TRUE)
   
  str(count_reported_sedar_clean)

# save dataframe
  save(count_reported_sedar_clean,file="count_reported_sedar_clean.Rda") # file is saved in data folder

# Load new data 
  load(file = "data/count_reported_sedar_clean.Rda")
  
  view(count_reported_sedar_clean)

# play with plots 
  plot(count_reported_sedar_clean)
  matplot(t(count_reported_sedar_clean),type="l")

# find mean reporting frequency - each yr about 56% across all variables 
  count_means <- map(count_reported_sedar_clean, mean)
  count_means <- map_dbl(count_reported_sedar_clean, mean)
  view(count_means)
  str(count_means)
  str(count_reported_sedar_clean)

# remove variables that are either all or no reporting
  # remove 0's
  count_reported_sedar_reduced <- count_reported_sedar_clean[apply(count_reported_sedar_clean[,-1], 1, function(x) !all(x==0)),]
  # remove 100's
  count_reported_sedar_reduced2 <- count_reported_sedar_reduced[apply(count_reported_sedar_reduced[,-1], 1, function(x) !all(x==100)),]
  # how many variables are we left with? - 143 of 305 variables 
  rownames(count_reported_sedar_reduced2)
  
# plot one row at a time to show variance across yrs ####
  library(ggplot2)
  library(reshape2)
  
  # transpose dataframe so that year is x (row) and variables are y (columns)
  count_reported_sedar_transposed <- data.frame(t(count_reported_sedar_year[-1]))
  colnames(count_reported_sedar_transposed) <- count_reported_sedar_year[, 1]
  
  # remove variables that are either all or no reporting ####
    # remove all 0's columns 
    count_reported_sedar_transposed_reduced <- count_reported_sedar_transposed[, colSums(count_reported_sedar_transposed != 0) > 0]
    # remove 100's
    vapply(count_reported_sedar_transposed_reduced, function(x) length(unique(x)) > 1, logical(1L))
    count_reported_sedar_transposed_reduced2 <- count_reported_sedar_transposed_reduced[vapply(count_reported_sedar_transposed_reduced, function(x) length(unique(x)) > 1, logical(1L))]
  
    #RENAME TO SHORTER NAME
    count_reported_plot <- count_reported_sedar_transposed_reduced2
# figure out plots   
    ## TRIAL AND ERROR 
  # # move row names into columns
    
  # count_reported_plot <- tibble::rownames_to_column(count_reported_plot, "year")
  # 
  # #plot
  # ggplot(count_reported_plot) + 
  #   geom_point(aes(x = year, y= AREA_2))
  # 
  # ggplot(count_reported_plot, aes(year)) +       # Create ggplot2 plot
  #   geom_path(aes(y = AREA_1), color = "red") +
  #   geom_path(aes(y = AREA_2), color = "blue") + 
  #   geom_path(aes(y = AREA_3), color = "green") +
  #   labs(x = "year",
  #        y = "variable",
  #        color = "Legend") +
  #   scale_color_manual(values = colors)
  # 
  # ggplot(count_reported_plot, aes(year, AREA_1, group=1)) +
  #   geom_point() +
  #   geom_line() +
  #   labs(x = "Year", y = "TIP Variables", 
  #        title = "TIP Reporting Consistency")
  # View(plot1)
  #              
  # ggplot(count_reported_plot, aes(x = year, y = , color = year))
  # 
  # ggplot(df, aes(x = x, color = group)) +
  #   geom_density(alpha = 0.5)
  # 
  # count_reported_plot_all <- melt(count_reported_plot, id = "year", measure = c("AREA_1", "AREA_2", "AREA_3"))
  # ggplot(count_reported_plot_all, aes(year, value, colour = variable)) + geom_line()
    
  # ggplot(count_reported_plot, aes(year, AREA_1, group=1)) +
  #   geom_point() +
  #   geom_line() +
  #   labs(x = "Year", y = "TIP Variables", 
  #        title = "TIP Reporting Consistency")
  # 
  # 
  # count_reported_plot %>%
  #   pivot_longer(AREA_1:VESSEL_NAME, names_to = "variable", values_to = "percentage") %>%
  #   ggplot(aes(x = year, colour = variable)) +
  #   facet_wrap(vars(year), ncol = 3) +
  #   geom_point(stat = "count") +
  #   geom_line(stat = "count") +
  #   labs(x = "Response (on a 1 to 5 scale)", y = "Number of respondents")

    
  # change to long table  
  colnames(count_reported_plot)
  longer_data <- count_reported_plot %>%
    pivot_longer(AREA_1:VESSEL_NAME, names_to = "variable", values_to = "percentage")
  print(longer_data)

  # create wrapped? table
  # ended up not doing a wrapped table, group by variable type 
  
# Graphing ####  
  # AREA graph
  count_reported_plot %>%
    pivot_longer(AREA_1:AREA_3, names_to = "variable", values_to = "percentage") %>%
    ggplot(aes(x=year, y=percentage , group=variable , colour=variable)) +
    geom_line() +
    geom_point() +
    labs(x = "Year", y = "Percent Reported (%)", 
         title = "TIP Reporting Consistency - AREA")
  
  # AREANAME graph
  count_reported_plot %>%
    pivot_longer(AREANAME_1:AREANAME_3, names_to = "variable", values_to = "percentage") %>%
    ggplot(aes(x=year, y=percentage , group=variable , colour=variable)) +
    geom_line() +
    geom_point() +
    labs(x = "Year", y = "Percent Reported (%)", 
         title = "TIP Reporting Consistency - AREANAME")
  
  # METADATA graph
  count_reported_plot %>%
    pivot_longer(BEGIN_HOUR_MIN:DEALER_CODE, names_to = "variable", values_to = "percentage") %>%
    ggplot(aes(x=year, y=percentage , group=variable , colour=variable)) +
    geom_line() +
    geom_point() +
    labs(x = "Year", y = "Percent Reported (%)", 
         title = "TIP Reporting Consistency - SHIP METADATA")

  # METADATA graph part 2
  count_reported_plot %>%
    pivot_longer(EFF_CREATED_DATE1:END_HOUR_MIN, names_to = "variable", values_to = "percentage") %>%
    ggplot(aes(x=year, y=percentage , group=variable , colour=variable)) +
    geom_line() +
    geom_point() +
    labs(x = "Year", y = "Percent Reported (%)", 
         title = "TIP Reporting Consistency - SHIP METADATA part 2")
  
  # GEAR graph
  count_reported_plot %>%
    pivot_longer(GEAR_1:GEAR_3, names_to = "variable", values_to = "percentage") %>%
    ggplot(aes(x=year, y=percentage , group=variable , colour=variable)) +
    geom_line() +
    geom_point() +
    labs(x = "Year", y = "Percent Reported (%)", 
         title = "TIP Reporting Consistency - GEAR")
  
  # GEAR FREQUENCY graph
  count_reported_plot %>%
    pivot_longer(GEAR_FREQUENCY1:GEAR_FREQUENCY3, names_to = "variable", values_to = "percentage") %>%
    ggplot(aes(x=year, y=percentage , group=variable , colour=variable)) +
    geom_line() +
    geom_point() +
    labs(x = "Year", y = "Percent Reported (%)", 
         title = "TIP Reporting Consistency - GEAR FREQUENCY")
  
  # GEAR QTY graph
  count_reported_plot %>%
    pivot_longer(GEAR_QTY_1:GEAR_QTY_3, names_to = "variable", values_to = "percentage") %>%
    ggplot(aes(x=year, y=percentage , group=variable , colour=variable)) +
    geom_line() +
    geom_point() +
    labs(x = "Year", y = "Percent Reported (%)", 
         title = "TIP Reporting Consistency - GEAR QTY")
  
  # GEAR NAME graph - revisit, all 5 variables have exact same percentages, seems weird
  count_reported_plot %>%
    pivot_longer(GEARNAME_1:GEARNAME_5, names_to = "variable", values_to = "percentage") %>%
    ggplot(aes(x=year, y=percentage , group=variable , colour=variable)) +
    geom_line() +
    geom_point() +
    labs(x = "Year", y = "Percent Reported (%)", 
         title = "TIP Reporting Consistency - GEAR NAME")
  
  # INTERVIEW  graph part 1 - land_area and land_area_name are equal
  count_reported_plot %>%
    pivot_longer(INT_TYPE:LAND_GEAR_NAME, names_to = "variable", values_to = "percentage") %>%
    ggplot(aes(x=year, y=percentage , group=variable , colour=variable)) +
    geom_line() +
    geom_point() +
    labs(x = "Year", y = "Percent Reported (%)", 
         title = "TIP Reporting Consistency - INTERVIEW Metadata Part 1")
  
  # INTERVIEW  graph part 2 - land_area and land_area_name are equal
  count_reported_plot %>%
    pivot_longer(LAND_MODIFIED_BY:LAND_SCIENTIFIC_NAME, names_to = "variable", values_to = "percentage") %>%
    ggplot(aes(x=year, y=percentage , group=variable , colour=variable)) +
    geom_line() +
    geom_point() +
    labs(x = "Year", y = "Percent Reported (%)", 
         title = "TIP Reporting Consistency - INTERVIEW Metadata Part 2")
  
  # LANDING graph part 1
  count_reported_plot %>%
    pivot_longer(LAND_STANDARD_AREA_ID:LAND_WEIGHT_KG, names_to = "variable", values_to = "percentage") %>%
    ggplot(aes(x=year, y=percentage , group=variable , colour=variable)) +
    geom_line() +
    geom_point() +
    labs(x = "Year", y = "Percent Reported (%)", 
         title = "TIP Reporting Consistency - LANDING Metadata part 1")
  
  # LANDING graph part 2
  count_reported_plot %>%
    pivot_longer(LANDING_AREA_COUNTY_CODE:LANDING_DATE, names_to = "variable", values_to = "percentage") %>%
    ggplot(aes(x=year, y=percentage , group=variable , colour=variable)) +
    geom_line() +
    geom_point() +
    labs(x = "Year", y = "Percent Reported (%)", 
         title = "TIP Reporting Consistency - LANDING Metadata part 2")
  
  # LENGTH1 graph
  count_reported_plot %>%
    pivot_longer(LENGTH1:LENGTH1_MM, names_to = "variable", values_to = "percentage") %>%
    ggplot(aes(x=year, y=percentage , group=variable , colour=variable)) +
    geom_line() +
    geom_point() +
    labs(x = "Year", y = "Percent Reported (%)", 
         title = "TIP Reporting Consistency - LENGTH1")
  
  # LENGTH2 graph
  count_reported_plot %>%
    pivot_longer(LENGTH2:LENGTH2_MM, names_to = "variable", values_to = "percentage") %>%
    ggplot(aes(x=year, y=percentage , group=variable , colour=variable)) +
    geom_line() +
    geom_point() +
    labs(x = "Year", y = "Percent Reported (%)", 
         title = "TIP Reporting Consistency - LENGTH2")
  
  # LENGTH INCREMENT AND LICENSE graph
  count_reported_plot %>%
    pivot_longer(c(LENGTH_INCREMENT,LICENSE), names_to = "variable", values_to = "percentage") %>%
    ggplot(aes(x=year, y=percentage , group=variable , colour=variable)) +
    geom_line() +
    geom_point() +
    labs(x = "Year", y = "Percent Reported (%)", 
         title = "TIP Reporting Consistency - LENGTH1")
  
# Restart graphing grouping by TIP data collection order ####
  
  # interview - BEGIN_HOUR_MIN, BIAS_TYPE, CHECKED_DATE, CHECKER, 
    # CREW_SIZE, DEALER, DEALER_CODE, END_HOUR_MIN, INT_TYPE, INTERVIEW_COMMENT,
    # LANDING_AREA_COUNTY_CODE, LANDING_AREA_PLACE_CODE, LANDING_DATE, 
    # VESSEL_ID, OBSERVER_ONBOARD, 
    # OFFICIAL_VESSEL_NAME, VESSEL_NAME, PURCHASING_DEALER_CODE1, 
    # PURCHASING_DEALER_COUNTY_CODE1, PURCHASING_DEALER_COUNTY_NAME1, 
    # PURCHASING_DEALER_NAME1, PURCHASING_DEALER_STATES_CODE1, TICKET, 
    # TICKET_AGENCY, TICKET_AGENCY2, TICKET2, NBR_VESSELS, LICENSE, TRIP_NUMBER,
    # START_DATE, UNLOAD_DATE, TRIP_DAYS_OUT, TRIP_DAYS_FISHED, PLACE_LANDED
  

  
  INTERVIEW_VARIABLES_PLOT <- count_reported_plot %>%
    pivot_longer(c(CREW_SIZE, DEALER, DEALER_CODE, END_HOUR_MIN, INT_TYPE, INTERVIEW_COMMENT,
                   LANDING_AREA_COUNTY_CODE, LANDING_AREA_PLACE_CODE, LANDING_DATE, 
                   VESSEL_ID, OBSERVER_ONBOARD, 
                   OFFICIAL_VESSEL_NAME, VESSEL_NAME, PURCHASING_DEALER_CODE1, 
                   PURCHASING_DEALER_COUNTY_CODE1, PURCHASING_DEALER_COUNTY_NAME1, 
                   PURCHASING_DEALER_NAME1, PURCHASING_DEALER_STATE_CODE1, TICKET, 
                   TICKET_AGENCY, TICKET2, TICKET_AGENCY2, NBR_VESSELS, LICENSE, TRIP_NUMBER,
                   START_DATE, UNLOAD_DATE, TRIP_DAYS_OUT, TRIP_DAYS_FISHED, PLACE_LANDED),
                 names_to = "variable", values_to = "percentage") %>%
    ggplot(aes(x=year, y=percentage , group=variable , colour=variable)) +
    facet_wrap(vars(variable), ncol = 5) +
    geom_line() +
    geom_point() +
    labs(x = "Year", y = "Percent Reported (%)", 
         title = "TIP Reporting Consistency - INTERVIEW VARIABLES")
  ggsave("INTERVIEW_VARIABLES_PLOT.jpeg", plot = INTERVIEW_VARIABLES_PLOT, width = 5,
         height = 4)
  
  # caribbean in general will not have dealer info (dealer, dealer_code, purchasing_dealer_county_code, purchasing_dealer_county_name)
  
  pdf("INTERVIEW_VARIABLES_PLOT.pdf")
  print(INTERVIEW_VARIABLES_PLOT)
  dev.off()
  
  
  # observation - 
    # CONDITION_TYPE, LENGTH_INCREMENT, LENGTH1, 
    # LENGTH1_MM, LENGTH2, LENGTH2_MM, OBS_COMMENT, OBS_GROUP,
    # OBS_MODIFIED_BY, OBS_MODIFIED_DATE, OBS_OLD_SAMPLE_ID, OBS_REPLICATE, 
    # OBS_WEIGHT, OBS_WEIGHT_KG, SEX_NAME, SECONDARY_SEX, 
  
  OBSERVATION_VARIABLES_PLOT <- count_reported_plot %>%
    pivot_longer(c(CONDITION_TYPE, LENGTH_INCREMENT, LENGTH1, 
                  LENGTH1_MM, LENGTH2, LENGTH2_MM, OBS_COMMENT, OBS_GROUP,
                  OBS_MODIFIED_BY, OBS_MODIFIED_DATE, OBS_OLD_SAMPLE_ID, OBS_REPLICATE, 
                  OBS_WEIGHT, OBS_WEIGHT_KG, SEX_NAME, SECONDARY_SEX),
                 names_to = "variable", values_to = "percentage") %>%
    ggplot(aes(x=year, y=percentage , group=variable , colour=variable)) +
    facet_wrap(vars(variable), ncol = 5) +
    geom_line() +
    geom_point() +
    labs(x = "Year", y = "Percent Reported (%)", 
         title = "TIP Reporting Consistency - OBSERVATION VARIABLES")
  ggsave("OBSERVATION_VARIABLES_PLOT.jpeg", plot = OBSERVATION_VARIABLES_PLOT, width = 5,
         height = 4)
  
  pdf("INTERVIEW_VARIABLES_PLOT.pdf")
  print(INTERVIEW_VARIABLES_PLOT)
  dev.off()
  
  # sample - 
    # SAMPLE_COMMENT, SAMPLE_MODIFIED_BY, SAMPLE_MODIFIED_DATE, SAMPLE_REPLICATE, 
    # SAMPLE_SAMPLE_NUMBER_OLD, SAMPLE_SCIENTIFIC_NAME, SAMPLE_STANDARD_SPECIES_CODE,
    # SAMPLE_STANDARD_SPECIES_NAME, SAMPLE_TYPE_1, SAMPLE_TYPE_2, SAMPLE_TYPE3, 
    # SAMPLE_WEIGHT, SAMPLE_WEIGHT_KG, S_SAMPLE_COUNT, SUB_SAMPLE_CONDITION,
    # SUB_SAMPLE_COUNT, SUB_SAMPLE_RANDOM, SUB_SAMPLE_WEIGHT, 
    # SUB_SAMPLE_WEIGHT_KG, SUB_SAMPLE_WEIGHT_UNIT,
    # SUB_SAMPLE_WEIGHT_UNIT_CODE
  
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
  
  # landing - 
    # LAND_AREA, 
    # LAND_AREA_NAME, LAND_GEAR_NAME, LAND_MODIFIED_BY, LAND_MODIFIED_DATE,
    # LAND_REGION, LAND_REPLICATE, LAND_SAMPLE_COUNT, LAND_SCIENTIFIC_NAME,
    # LAND_STANDARD_AREA_ID, LAND_STANDARD_AREA_NAME, LAND_STANDARD_SPC_NAME, 
    # LAND_STANDARD_SPECIES_CODE, LAND_WEIGHT, LAND_WEIGHT_KG, LANDING_COMMENT,
    # 
 
  colnames(count_reported_plot)
  colnames(com_tip_sedar)
  
  