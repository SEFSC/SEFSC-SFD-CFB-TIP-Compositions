# Yellowtail snapper investigation - PR and STT/STJ

# READ IN DATASETS ####

  librarian::shelf(here, tidyverse, ROracle, keyring, dotenv)
  
  # read in data 
  com_tip_PR_yts <- readRDS("~/SEFSC-SFD-CFB-TIP-Compositions/data/raw/com_tip_PR_168907_20230906.RDS")
  com_tip_VI_yts <- readRDS("~/SEFSC-SFD-CFB-TIP-Compositions/data/raw/com_tip_VI_168907_20230912.RDS")
  
  # create new filterable date value for PR
  com_tip_PR_yts_new <- com_tip_PR_yts %>%
    mutate(TEST_DATE = as.Date(ymd_hms(INTERVIEW_DATE)),
           FINAL_DATE = case_when(is.na(TEST_DATE) ~ INTERVIEW_DATE, 
                                  TRUE ~ TEST_DATE))
  
  # create new filterable date value for VI
  com_tip_VI_yts_new <- com_tip_VI_yts %>%
    mutate(TEST_DATE = as.Date(ymd_hms(INTERVIEW_DATE)),
           FINAL_DATE = case_when(is.na(TEST_DATE) ~ INTERVIEW_DATE, 
                                  TRUE ~ TEST_DATE))
  
  # create count of observed records for each area  
  com_tip_PR_YTS_count <- com_tip_PR_yts_new %>%
    add_count(COUNTY_SAMPLED) %>%
    mutate(COUNTY_SAMPLEDn = paste0(COUNTY_SAMPLED, ' (', n, ')')) 
  
  com_tip_VI_YTS_count <- com_tip_VI_yts_new %>%
    add_count(COUNTY_SAMPLED) %>%
    mutate(COUNTY_SAMPLEDn = paste0(COUNTY_SAMPLED, ' (', n, ')')) 



# Gear groupings ####

# VISUALIZE   
  # PR graph
  ggplot(data = com_tip_PR_YTS_count, aes(x = FINAL_DATE, y = STANDARDGEARNAME_1, group = COUNTY_SAMPLED , color = COUNTY_SAMPLEDn )) + 
    # facet_wrap(vars(COUNTY_SAMPLED), ncol = 3) +
    geom_point() + 
    labs(x = "Year", y = "STANDARDGEARNAME_1", 
         title = "area-time distribution of sample gear PR", color = "COUNTY_SAMPLED (# obs)") 
  # theme(legend.position = "none")
  
  # VI graph
  ggplot(data = com_tip_VI_YTS_count, aes(x = FINAL_DATE, y = STANDARDGEARNAME_1, color = COUNTY_SAMPLEDn )) + 
    facet_wrap(vars(COUNTY_SAMPLED), ncol = 1) +
    geom_point() + 
    labs(x = "Year", y = "STANDARDGEARNAME_1", 
         title = "area-time distribution of sample gear VI", color = "COUNTY_SAMPLED (# obs)") 
  # theme(legend.position = "none")
  
# GROUP GEARS 
  # find unique gears
      unique(com_tip_PR_YTS_count$STANDARDGEARNAME_1) # 16
      #"POTS AND TRAPS, FISH"          "HAUL SEINES"                  
      # "LINES HAND"                    "TRAMMEL NETS"                 
      # "POTS AND TRAPS, SPINY LOBSTER" "ENTANGLING NETS (GILL) UNSPC" 
      # "BY HAND, DIVING GEAR"          "NOT CODED"                    
      # "ROD AND REEL"                  "LINES POWER TROLL OTHER"      
      # NA                              "LINES LONG SET WITH HOOKS"    
      # "BY HAND"                       "UNSPECIFIED GEAR"             
      # "CAST NETS"                     "SKIN DIVING"  
      unique(com_tip_PR_YTS_count$STANDARDGEAR_1)
      # "139" "010" "700" "210" "140" "200" "752" 
      # "000" "300" "320" "400" "750" "801" "551" "751"
      # NA
      
      #unique combos of gear number and name: 
      unique(com_tip_PR_YTS_count[c("STANDARDGEAR_1", "STANDARDGEARNAME_1")])
      unique(com_tip_VI_YTS_count[c("STANDARDGEAR_1", "STANDARDGEARNAME_1")])
      unique(com_tip_VI_YTS_count$STANDARDGEARNAME_1) # 27
      # "POTS AND TRAPS, FISH"          "LINES HAND"                   
      # "POTS AND TRAPS, CMB"           "NOT CODED"                    
      # "HOOKS, SPONGE"                 "DIP NETS"                     
      # "GRABS, HOOKS"                  "ROD AND REEL"                 
      # "LINES POWER TROLL OTHER"       "LINES LONG DRIFT WITH HOOKS"  
      # "ENTANGLING NETS (GILL) UNSPC"  "LINES LONG, REEF FISH"        
      # "GILL NETS, OTHER"              NA                             
      # "ENCIRCLING NETS (PURSE)"       "POTS AND TRAPS, SPINY LOBSTER"
      # "BUOY GEAR, VERTICAL"           "SPEARS"                       
      # "POTS AND TRAPS, BOX TRAP"      "TRAMMEL NETS"                 
      # "BY HAND, DIVING GEAR"          "HAUL SEINES"                  
      # "LINES LONG SET WITH HOOKS"     "SPEARS, DIVING"               
      # "REEL, ELECTRIC OR HYDRAULIC"   "STOP SEINES"                  
      # "ROD AND REEL, ELECTRIC (HAND)"
  
  # Load gear groupings 
    geargroupings_yts <- read_csv("data/CSVs/gear_groupings_yts.csv")
  
  # Convert gear to gear group based off TIP manual gear groupings 
    com_tip_PR_YTS_geargroup <- com_tip_PR_YTS_count %>%
      mutate(GEAR_GROUP = geargroupings_yts$gear_group[match(com_tip_PR_YTS_count$STANDARDGEARNAME_1, geargroupings_yts$STANDARDGEARNAME_1)])
    
    com_tip_VI_YTS_geargroup <- com_tip_VI_YTS_count %>%
      mutate(GEAR_GROUP = geargroupings_yts$gear_group[match(com_tip_VI_YTS_count$STANDARDGEARNAME_1, geargroupings_yts$STANDARDGEARNAME_1)])
 
  # create count of observed records for each GEAR GROUP
    com_tip_PR_YTS_geargroup_COUNT <- subset(com_tip_PR_YTS_geargroup, select = -n) %>%
      add_count(GEAR_GROUP) %>%
      mutate(GEAR_GROUPn = paste0(GEAR_GROUP, '(', n, ')')) 
   
    com_tip_VI_YTS_geargroup_COUNT <- subset(com_tip_VI_YTS_geargroup, select = -n) %>%
      add_count(GEAR_GROUP) %>%
      mutate(GEAR_GROUPn = paste0(GEAR_GROUP, '(', n, ')')) 
    
  # Run graphs with new groupings
    # PR graph
    ggplot(data = com_tip_PR_YTS_geargroup_COUNT, aes(x = FINAL_DATE, y = GEAR_GROUPn, group = COUNTY_SAMPLED , color = COUNTY_SAMPLEDn )) + 
      # facet_wrap(vars(COUNTY_SAMPLED), ncol = 3) +
      geom_point() + 
      labs(x = "Year", y = "STANDARDGEARNAME_1", 
           title = "area-time distribution of sample gear PR", 
           color = "COUNTY_SAMPLED (# obs)",
           subtitle = paste("N = ", nrow(com_tip_PR_YTS_geargroup_COUNT))) 
     
    # PR  graph - filtered to STT and most used gear groups 
      com_tip_PR_YTS_geargroup_COUNT_filter <- com_tip_PR_YTS_geargroup_COUNT %>%
        filter(GEAR_GROUP %in% c("Seine Nets", "Pots and Traps", "Hand Lines"))
      
      com_tip_PR_YTS_geargroup_COUNT_stt_IND <- subset(com_tip_PR_YTS_geargroup_COUNT_filter, select = -n) %>%
        add_count(STANDARDGEARNAME_1) %>%
        mutate(STANDARDGEARNAME_1n = paste0(STANDARDGEARNAME_1, '(', n, ')')) 
      
      ggplot(data = com_tip_PR_YTS_geargroup_COUNT_stt_IND, aes(x = FINAL_DATE, y = STANDARDGEARNAME_1n, color = COUNTY_SAMPLEDn )) + 
        # facet_wrap(vars(COUNTY_SAMPLED), ncol = 1) +
        geom_point() + 
        labs(x = "Year", y = "STANDARDGEARNAME_1", 
             title = "area-time distribution of sample gear VI", 
             color = "COUNTY_SAMPLED (# obs)",
             subtitle = paste("N = ", nrow(com_tip_PR_YTS_geargroup_COUNT_stt_IND))) 
    
      
    # VI graph
      ggplot(data = com_tip_VI_YTS_geargroup_COUNT, aes(x = FINAL_DATE, y = GEAR_GROUPn, group = COUNTY_SAMPLED , color = COUNTY_SAMPLEDn )) + 
        facet_wrap(vars(COUNTY_SAMPLED), ncol = 1) +
        geom_point() + 
        labs(x = "Year", y = "STANDARDGEARNAME_1", 
             title = "area-time distribution of sample gear PR", 
             color = "COUNTY_SAMPLED (# obs)",
             subtitle = paste("N = ", nrow(com_tip_VI_YTS_geargroup_COUNT)))   
    
    # VI graph - filtered to STT and most used gear groups 
    com_tip_VI_YTS_geargroup_COUNT_stt <- com_tip_VI_YTS_geargroup_COUNT %>%
      filter(COUNTY_SAMPLED == "ST THOMAS",
                # JUST HAND LINES, AND POTS AND TRAPS
             GEAR_GROUP %in% c("Seine Nets", "Pots and Traps", "Hand Lines"))
    
    com_tip_VI_YTS_geargroup_COUNT_stt_IND <- subset(com_tip_VI_YTS_geargroup_COUNT_stt, select = -n) %>%
      add_count(STANDARDGEARNAME_1) %>%
      mutate(STANDARDGEARNAME_1n = paste0(STANDARDGEARNAME_1, '(', n, ')')) 
    
    
    ggplot(data = com_tip_VI_YTS_geargroup_COUNT_stt_IND, aes(x = FINAL_DATE, y = STANDARDGEARNAME_1n, color = COUNTY_SAMPLEDn )) + 
      # facet_wrap(vars(COUNTY_SAMPLED), ncol = 1) +
      geom_point() + 
      labs(x = "Year", y = "STANDARDGEARNAME_1", 
           title = "area-time distribution of sample gear VI", 
           color = "COUNTY_SAMPLED (# obs)",
           subtitle = paste("N = ", nrow(com_tip_VI_YTS_geargroup_COUNT_stt_IND))) 
   
    
    unique(com_tip_VI_YTS_count[c("STANDARDGEAR_1", "STANDARDGEARNAME_1")]) 
  
    