# PR vs VI variable content analysis ####

# read in data: 
# com_tip_PR_SEDAR <- readRDS("~/SEFSC-SFD-CFB-TIP-Compositions/data/raw/com_tip_PR_97648_97646_170566_168907_173139_170867_167700_20230822.RDS")
# 
# com_tip_VI_SEDAR <- readRDS("~/SEFSC-SFD-CFB-TIP-Compositions/data/raw/com_tip_VI_97648_97646_170566_168907_173139_170867_167700_20230822.RDS")

# Puerto Rico ####

colnames(com_tip_PR_yts)
str(com_tip_PR_yts$LANDING_AREA_COUNTY_CODE)
str(com_tip_PR_yts_new)

# Read in Yellowtail snapper= 168907 #### - complete, start at "begin plotting" 
# cr_tip_sp(state_codes = 'PR', sp_codes = 168907)
# cr_tip_sp(state_codes = 'VI', sp_codes = 168907)

 com_tip_PR_yts <- readRDS("~/SEFSC-SFD-CFB-TIP-Compositions/data/raw/com_tip_PR_168907_20230906.RDS")
 com_tip_VI_yts <- readRDS("~/SEFSC-SFD-CFB-TIP-Compositions/data/raw/com_tip_VI_168907_20230912.RDS")

# create new filterable date value for PR
 com_tip_PR_yts_new <- com_tip_PR_yts %>%
   mutate(TEST_DATE = as.Date(ymd_hms(INTERVIEW_DATE)),
          FINAL_DATE = case_when(is.na(TEST_DATE) ~ INTERVIEW_DATE, 
                                 TRUE ~ TEST_DATE))
 save(com_tip_PR_yts_new,file="data/dataframes/com_tip_PR_yts_new.Rda") # file is saved in data folder  

# create new filterable date value for VI
 com_tip_VI_yts_new <- com_tip_VI_yts %>%
   mutate(TEST_DATE = as.Date(ymd_hms(INTERVIEW_DATE)),
          FINAL_DATE = case_when(is.na(TEST_DATE) ~ INTERVIEW_DATE, 
                                 TRUE ~ TEST_DATE))
 save(com_tip_VI_yts_new,file="data/dataframes/com_tip_VI_yts_new.Rda") # file is saved in data folder

summary(com_tip_PR_SEDAR)
colnames(com_tip_PR_SEDAR)

  # Begin Plotting ####
  load("~/SEFSC-SFD-CFB-TIP-Compositions/data/dataframes/com_tip_PR_yts_new.Rda")
  load("~/SEFSC-SFD-CFB-TIP-Compositions/data/dataframes/com_tip_VI_yts_new.Rda")
  
  # SAMPLE WEIGHT VS. TIME ####
  
  # PR - 
    # # sample weight vs. time - seem to only appear after 2005 (sample weight is aggregate weight)
    # sample_weight_PR_YTS <- com_tip_PR_yts_new
    # ggplot(data = com_tip_PR_yts_new, aes(x = FINAL_DATE, y = SAMPLE_WEIGHT_KG, color = LANDING_AREA_COUNTY_CODE )) + 
    #   geom_point() + 
    #   labs(x = "Year", y = "Sample Weight kg", 
    #        title = "area-time distribution of sample weight") 
  
  com_tip_PR_yts_2005 <- com_tip_PR_yts_new %>%
    filter(YEAR > "2005")
  
    # sample weight vs. time - after 2005 
    sample_weight_PR_YTS <- com_tip_PR_yts_new %>%
      filter(YEAR > "2005") %>%
      ggplot(aes(x = FINAL_DATE, y = SAMPLE_WEIGHT_KG, color = LANDING_AREA_COUNTY_CODE )) + 
      geom_point() + 
      labs(x = "Year", y = "Sample Weight kg", 
           title = "area-time distribution of sample weight PR - 2005")  
    ggsave("area-time distribution of sample weight - 2005.jpeg", plot = sample_weight_PR_YTS, width = 15,
           height = 8)
  
  
  # VI -   
     # remove "not coded" county_sampled 
    com_tip_VI_yts_simp <- com_tip_VI_yts_new[com_tip_VI_yts_new$COUNTY_SAMPLED != "NOT CODED",]
    
    # sample weight vs. time - SOLID COVERAGE OVERALL, STT = NO SAMPLES 1990-2000, 
        # STJ = SPARCE SAMPLES 2012-NOW (sample weight is aggregate weight) 
    sample_weight_VI_YTS <- com_tip_VI_yts_simp %>% 
    ggplot(aes(x = FINAL_DATE, y = SAMPLE_WEIGHT_KG, color = COUNTY_SAMPLED )) + 
      facet_wrap(vars(COUNTY_SAMPLED), ncol = 1) +
      geom_point() + 
      labs(x = "Year", y = "Sample Weight kg", 
           title = "area-time distribution of sample weight VI") +
      theme(legend.position = "none")
    
    ggsave("area-time distribution of sample weight VI.jpeg", plot = sample_weight_VI_YTS, width = 15,
           height = 8)
   
  
  # GEAR CODES ####
    
  # PR -   
    # # gear codes over time - PR
    # sample_gear_PR_YTS <- com_tip_PR_yts_new 
    # ggplot(data = com_tip_PR_yts_new, aes(x = FINAL_DATE, y = GEARNAME_1, color = LANDING_AREA_COUNTY_CODE )) + 
    #   geom_point() + 
    #   labs(x = "Year", y = "GEARNAME_1", 
    #        title = "area-time distribution of sample gear PR") 
    
    # gear codes over time - PR - standardgearname_1 is official codes used
    sample_gear_PR_YTS <- com_tip_PR_yts_new %>%
      ggplot(aes(x = FINAL_DATE, y = STANDARDGEARNAME_1, group = COUNTY_SAMPLED , color = COUNTY_SAMPLED )) + 
      # facet_wrap(vars(COUNTY_SAMPLED), ncol = 3) +
      geom_point() + 
      labs(x = "Year", y = "STANDARDGEARNAME_1", 
           title = "area-time distribution of sample gear PR") 
      # theme(legend.position = "none")
     
       ggsave("area-time distribution of standard gear PR YTS.jpeg", plot = sample_gear_PR_YTS, width = 20,
             height = 12)
   
  # VI -   
    # # gear codes over time - VI
    # sample_gear_VI_YTS <- com_tip_VI_yts_new 
    # ggplot(data = com_tip_VI_yts_new, aes(x = FINAL_DATE, y = GEARNAME_1, color = COUNTY_SAMPLED )) + 
    #   geom_point() + 
    #   labs(x = "Year", y = "GEARNAME_1", 
    #        title = "area-time distribution of sample gear VI") 
    
    
    # gear codes over time - VI - standardgearname_1 is official codes used - removed not coded ones
    sample_gear_VI_YTS <- com_tip_VI_yts_simp %>%
    ggplot(aes(x = FINAL_DATE, y = STANDARDGEARNAME_1, color = COUNTY_SAMPLED )) + 
      facet_wrap(vars(COUNTY_SAMPLED), ncol = 1) +
      geom_point() + 
      labs(x = "Year", y = "STANDARDGEARNAME_1", 
           title = "area-time distribution of sample gear VI") +
      theme(legend.position = "none")
    ggsave("area-time distribution of standard gear VI YTS.jpeg", plot = sample_gear_VI_YTS, width = 20,
           height = 12)
    
    unique(com_tip_VI_yts_simp$STANDARDGEARNAME_1)
    
    
    
       
  # OBS_WEIGHT_KG - individual weights ####
    
  # PR -   
     # OBS_WEIGHT codes over time - PR
       sample_OBS_WEIGHT_PR_YTS <- com_tip_PR_yts_new %>%
         ggplot(aes(x = FINAL_DATE, y = OBS_WEIGHT_KG, group = COUNTY_SAMPLED , color = COUNTY_SAMPLED )) + 
         # facet_wrap(vars(COUNTY_SAMPLED), ncol = 3) +
         geom_point() + 
         labs(x = "Year", y = "OBS_WEIGHT_KG", 
              title = "area-time distribution of OBS_WEIGHT_KG PR") +
         theme(legend.position = "none")    
    
         ggsave("area-time distribution of OBS WEIGHT PR YTS.jpeg", plot = sample_OBS_WEIGHT_PR_YTS, width = 20,
                height = 12) 
  
  # VI -        
     # OBS_WEIGHT codes over time - VI
       sample_OBS_WEIGHT_VI_YTS <- com_tip_VI_yts_new %>%
        ggplot(aes(x = FINAL_DATE, y = OBS_WEIGHT_KG, group = COUNTY_SAMPLED , color = COUNTY_SAMPLED )) + 
         facet_wrap(vars(COUNTY_SAMPLED), ncol = 2) +
         geom_point() + 
         labs(x = "Year", y = "OBS_WEIGHT_KG", 
         title = "area-time distribution of OBS_WEIGHT_KG VI") +
         theme(legend.position = "none")    
           
         ggsave("area-time distribution of OBS WEIGHT VI YTS.jpeg", plot = sample_OBS_WEIGHT_VI_YTS, width = 20,
                height = 12)     
         
         
   # practice adding it to quarto   - success!!     
         ggplot(data = com_tip_PR_yts_new, aes(x = FINAL_DATE, y = OBS_WEIGHT_KG, group = COUNTY_SAMPLED , color = COUNTY_SAMPLED )) + 
            # facet_wrap(vars(COUNTY_SAMPLED), ncol = 2) +
           geom_point() + 
           labs(x = "Year", y = "OBS_WEIGHT_KG", 
                title = "area-time distribution of OBS_WEIGHT_KG VI") +
           theme(legend.position = "none")  
         
         
  unique(com_tip_VI_yts_new$LANDING_AREA_COUNTY_CODE)
  colnames(com_tip_PR_yts_new)
  
  # LENGTH1_MM ####
  
  # PR 
  
  ggplot(data = com_tip_PR_yts_new, aes(x = FINAL_DATE, y = LENGTH1_MM, group = COUNTY_SAMPLED , color = COUNTY_SAMPLED )) + 
    # facet_wrap(vars(COUNTY_SAMPLED), ncol = 2) +
    geom_point() + 
    labs(x = "Year", y = "LENGTH1_MM", 
         title = "area-time distribution of LENGTH1_MM PR") +
    theme(legend.position = "none")  
  
  # VI
  
  ggplot(data = com_tip_VI_yts_new, aes(x = FINAL_DATE, y = LENGTH1_MM, group = COUNTY_SAMPLED , color = COUNTY_SAMPLED )) + 
    facet_wrap(vars(COUNTY_SAMPLED), ncol = 2) +
    geom_point() + 
    labs(x = "Year", y = "LENGTH1_MM", 
         title = "area-time distribution of LENGTH1_MM VI") +
    theme(legend.position = "none")  
  
  
  # COUNTY_LANDED
  
  # PR 
  
  ggplot(data = com_tip_PR_yts_new, aes(x = FINAL_DATE, y = COUNTY_LANDED )) + 
    # facet_wrap(vars(COUNTY_SAMPLED), ncol = 2) +
    geom_point() + 
    labs(x = "Year", y = "COUNTY_LANDED", 
         title = "time distribution of COUNTY_LANDED PR") +
    theme(legend.position = "none")  
  
  # VI
  
  ggplot(data = com_tip_VI_yts_new, aes(x = FINAL_DATE, y = COUNTY_LANDED )) + 
    #facet_wrap(vars(COUNTY_SAMPLED), ncol = 2) +
    geom_point() + 
    labs(x = "Year", y = "COUNTY_LANDED", 
         title = "time distribution of COUNTY_LANDED VI") +
    theme(legend.position = "none")  
  
# REMOVE OUTLIERS ####
# 0 - remove records with only L or W values or neither
# 1 - L-W 95 % confidence interval
# 2 - Fulton K outliers 

# SELECT RECORDS WITH BOTH LENGTH1_MM AND OBS_WEIGHT_KG

com_tip_PR_yts_wl <- com_tip_PR_yts_new[!(is.na(com_tip_PR_yts_new$LENGTH1_MM)) & !(is.na(com_tip_PR_yts_new$OBS_WEIGHT_KG)), ]

com_tip_VI_yts_wl <- com_tip_VI_yts_new[!(is.na(com_tip_VI_yts_new$LENGTH1_MM)) & !(is.na(com_tip_VI_yts_new$OBS_WEIGHT_KG)), ]


# rerun visuals with new datasets 

  # GEAR CODES ####
  
  # PR -   
  
  # gear codes over time - PR - standardgearname_1 is official codes used
  ggplot(data = com_tip_PR_yts_wl, aes(x = FINAL_DATE, y = STANDARDGEARNAME_1, group = COUNTY_SAMPLED , color = COUNTY_SAMPLED )) + 
    # facet_wrap(vars(COUNTY_SAMPLED), ncol = 3) +
    geom_point() + 
    labs(x = "Year", y = "STANDARDGEARNAME_1", 
         title = "area-time distribution of sample gear PR") +
    theme(legend.position = "none")
  
  
  # VI -   
  
  # gear codes over time - VI - standardgearname_1 is official codes used - removed not coded ones
  sample_gear_VI_YTS <- com_tip_VI_yts_simp %>%
    ggplot(aes(x = FINAL_DATE, y = STANDARDGEARNAME_1, color = COUNTY_SAMPLED )) + 
    facet_wrap(vars(COUNTY_SAMPLED), ncol = 1) +
    geom_point() + 
    labs(x = "Year", y = "STANDARDGEARNAME_1", 
         title = "area-time distribution of sample gear VI") +
    theme(legend.position = "none")
  ggsave("area-time distribution of standard gear VI YTS.jpeg", plot = sample_gear_VI_YTS, width = 20,
         height = 12)
  
  unique(com_tip_VI_yts_simp$STANDARDGEARNAME_1)
  
  
  
  
  # OBS_WEIGHT_KG - individual weights ####
  
  # PR -   
  # OBS_WEIGHT codes over time - PR
  sample_OBS_WEIGHT_PR_YTS <- com_tip_PR_yts_new %>%
    ggplot(aes(x = FINAL_DATE, y = OBS_WEIGHT_KG, group = COUNTY_SAMPLED , color = COUNTY_SAMPLED )) + 
    # facet_wrap(vars(COUNTY_SAMPLED), ncol = 3) +
    geom_point() + 
    labs(x = "Year", y = "OBS_WEIGHT_KG", 
         title = "area-time distribution of OBS_WEIGHT_KG PR") +
    theme(legend.position = "none")    
  
  ggsave("area-time distribution of OBS WEIGHT PR YTS.jpeg", plot = sample_OBS_WEIGHT_PR_YTS, width = 20,
         height = 12) 
  
  # VI -        
  # OBS_WEIGHT codes over time - VI
  sample_OBS_WEIGHT_VI_YTS <- com_tip_VI_yts_new %>%
    ggplot(aes(x = FINAL_DATE, y = OBS_WEIGHT_KG, group = COUNTY_SAMPLED , color = COUNTY_SAMPLED )) + 
    facet_wrap(vars(COUNTY_SAMPLED), ncol = 2) +
    geom_point() + 
    labs(x = "Year", y = "OBS_WEIGHT_KG", 
         title = "area-time distribution of OBS_WEIGHT_KG VI") +
    theme(legend.position = "none")    
  
  ggsave("area-time distribution of OBS WEIGHT VI YTS.jpeg", plot = sample_OBS_WEIGHT_VI_YTS, width = 20,
         height = 12)     
  
  
  # practice adding it to quarto   - success!!     
  ggplot(data = com_tip_PR_yts_new, aes(x = FINAL_DATE, y = OBS_WEIGHT_KG, group = COUNTY_SAMPLED , color = COUNTY_SAMPLED )) + 
    # facet_wrap(vars(COUNTY_SAMPLED), ncol = 2) +
    geom_point() + 
    labs(x = "Year", y = "OBS_WEIGHT_KG", 
         title = "area-time distribution of OBS_WEIGHT_KG VI") +
    theme(legend.position = "none")  
  
  
  unique(com_tip_VI_yts_new$LANDING_AREA_COUNTY_CODE)
  colnames(com_tip_PR_yts_new)
  
  # LENGTH1_MM ####
  
  # PR 
  
  ggplot(data = com_tip_PR_yts_new, aes(x = FINAL_DATE, y = LENGTH1_MM, group = COUNTY_SAMPLED , color = COUNTY_SAMPLED )) + 
    # facet_wrap(vars(COUNTY_SAMPLED), ncol = 2) +
    geom_point() + 
    labs(x = "Year", y = "LENGTH1_MM", 
         title = "area-time distribution of LENGTH1_MM PR") +
    theme(legend.position = "none")  
  
  com_tip_PR_yts_filtered <- com_tip_PR_yts_new %>%
    filter(LENGTH1_MM > 2000) # 5 inaccurate weight records
  
  # VI
  
  ggplot(data = com_tip_VI_yts_new, aes(x = FINAL_DATE, y = LENGTH1_MM, group = COUNTY_SAMPLED , color = COUNTY_SAMPLED )) + 
    facet_wrap(vars(COUNTY_SAMPLED), ncol = 2) +
    geom_point() + 
    labs(x = "Year", y = "LENGTH1_MM", 
         title = "area-time distribution of LENGTH1_MM VI") +
    theme(legend.position = "none")
  
# Redo initial graphs with STOPLIGHT PARROTFISH ####
cr_tip_sp(state_codes = 'PR', sp_codes = 170867)
cr_tip_sp(state_codes = 'VI', sp_codes = 170867)
com_tip_PR_STP <- readRDS("~/SEFSC-SFD-CFB-TIP-Compositions/data/raw/com_tip_PR_170867_20230929.RDS")
com_tip_VI_STP <- readRDS("~/SEFSC-SFD-CFB-TIP-Compositions/data/raw/com_tip_VI_170867_20230929.RDS")

# create new filterable date value for PR
 com_tip_PR_STP_new <- com_tip_PR_STP %>%
   mutate(TEST_DATE = as.Date(ymd_hms(INTERVIEW_DATE)),
          FINAL_DATE = case_when(is.na(TEST_DATE) ~ INTERVIEW_DATE, 
                                 TRUE ~ TEST_DATE))
 save(com_tip_PR_STP_new,file="data/dataframes/com_tip_PR_STP_new.Rda") # file is saved in data folder 
 
# create new filterable date value for VI
 com_tip_VI_STP_new <- com_tip_VI_STP %>%
   mutate(TEST_DATE = as.Date(ymd_hms(INTERVIEW_DATE)),
          FINAL_DATE = case_when(is.na(TEST_DATE) ~ INTERVIEW_DATE, 
                                 TRUE ~ TEST_DATE))
 save(com_tip_VI_STP_new,file="data/dataframes/com_tip_VI_STP_new.Rda") # file is saved in data folder

 # COUNTY_LANDED####
 
 # PR 
 ggplot(data = com_tip_PR_STP_count, aes(x = FINAL_DATE, y = COUNTY_SAMPLEDn, color = COUNTY_SAMPLEDn, useNA='always' )) + 
   # facet_wrap(vars(COUNTY_SAMPLED), ncol = 2) +
   geom_point() + 
   labs(x = "Year", y = "COUNTY_LANDED", 
        title = "time distribution of COUNTY_LANDED PR", color = "COUNTY_SAMPLED (# obs)") 
   # theme(legend.position = "none") 
   
 
 # VI
 
 ggplot(data = com_tip_VI_STP_new, aes(x = FINAL_DATE, y = COUNTY_LANDED )) + 
   #facet_wrap(vars(COUNTY_SAMPLED), ncol = 2) +
   geom_point() + 
   labs(x = "Year", y = "COUNTY_LANDED", 
        title = "time distribution of COUNTY_LANDED VI") +
   theme(legend.position = "none") 
 
 # SAMPLE_WEIGHT_KG - WORKING IN HERE #### 
 
 com_tip_PR_STP_count <- com_tip_PR_STP_new %>%
   add_count(COUNTY_SAMPLED) %>%
   mutate(COUNTY_SAMPLEDn = paste0(COUNTY_SAMPLED, ' (', n, ')')) 
 
 com_tip_VI_STP_count <- com_tip_VI_STP_new %>%
   add_count(COUNTY_SAMPLED) %>%
   mutate(COUNTY_SAMPLEDn = paste0(COUNTY_SAMPLED, ' (', n, ')')) 
 
 colnames(com_tip_PR_STP_count)
 
 com_tip_PR_STP_count2 <- subset(com_tip_PR_STP_count, select = -n) %>%
   add_count(YEAR) %>%
   mutate(YEARn = paste0(YEAR, '(', n, ')'))
   
 # can i change x axis label to be by the year without losing the specific dates of each point 
 
 # PR - change from points to count to avoid confidentiality breach? 
 ggplot(data = com_tip_PR_STP_count, aes(x = FINAL_DATE, y = SAMPLE_WEIGHT_KG, color = COUNTY_SAMPLEDn )) + 
   geom_count() + 
   labs(x = "Year", y = "Sample Weight kg", 
        title = "area-time distribution of sample weight PR", color = "COUNTY_SAMPLED (# obs)")  
    # theme(legend.position = "none") 
   # geom_text(aes(label=..count..), y=0, stat = 'count', color="red", size=4)
 
 
 # VI
 ggplot(data = com_tip_VI_STP_count, aes(x = FINAL_DATE, y = SAMPLE_WEIGHT_KG, color = COUNTY_SAMPLEDn )) + 
   facet_wrap(vars(COUNTY_SAMPLED), ncol = 2) +
   geom_count() + 
   labs(x = "Year", y = "Sample Weight kg", 
        title = "area-time distribution of sample weight VI") 
   #theme(legend.position = "none")
 
 
 # GEAR CODES ####
 
 # PR -   
 
 # gear codes over time - PR - standardgearname_1 is official codes used
 ggplot(data = com_tip_PR_STP_new, aes(x = FINAL_DATE, y = STANDARDGEARNAME_1, group = COUNTY_SAMPLED , color = COUNTY_SAMPLED )) + 
   # facet_wrap(vars(COUNTY_SAMPLED), ncol = 3) +
   geom_point() + 
   labs(x = "Year", y = "STANDARDGEARNAME_1", 
        title = "area-time distribution of sample gear PR") +
   theme(legend.position = "none")
 
 
 # VI -   
 
 # gear codes over time - VI - standardgearname_1 is official codes used - removed not coded ones
 
 ggplot(data = com_tip_VI_STP_new, aes(x = FINAL_DATE, y = STANDARDGEARNAME_1, color = COUNTY_SAMPLED )) + 
   facet_wrap(vars(COUNTY_SAMPLED), ncol = 1) +
   geom_point() + 
   labs(x = "Year", y = "STANDARDGEARNAME_1", 
        title = "area-time distribution of sample gear VI") +
   theme(legend.position = "none")
 
 # OBS_WEIGHT_KG - individual weights ####
 
 # PR -   
 # OBS_WEIGHT codes over time - PR
 ggplot(data = com_tip_PR_STP_new, aes(x = FINAL_DATE, y = OBS_WEIGHT_KG, group = COUNTY_SAMPLED , color = COUNTY_SAMPLED )) + 
   # facet_wrap(vars(COUNTY_SAMPLED), ncol = 2) +
   geom_point() + 
   labs(x = "Year", y = "OBS_WEIGHT_KG", 
        title = "area-time distribution of OBS_WEIGHT_KG PR") +
   theme(legend.position = "none") 
 
 com_tip_PR_STP_filtered <- com_tip_PR_STP_new %>%
   filter(OBS_WEIGHT_KG > 10) # 5 inaccurate weight records 
 
 # VI -        
 # OBS_WEIGHT codes over time - VI
 ggplot(data = com_tip_VI_STP_new, aes(x = FINAL_DATE, y = OBS_WEIGHT_KG, group = COUNTY_SAMPLED , color = COUNTY_SAMPLED )) + 
   facet_wrap(vars(COUNTY_SAMPLED), ncol = 2) +
   geom_point() + 
   labs(x = "Year", y = "OBS_WEIGHT_KG", 
        title = "area-time distribution of OBS_WEIGHT_KG VI") +
   theme(legend.position = "none")      
 
 
 # LENGTH1_MM ####
 
 # PR 
 
 ggplot(data = com_tip_PR_STP_new, aes(x = FINAL_DATE, y = LENGTH1_MM, group = COUNTY_SAMPLED , color = COUNTY_SAMPLED )) + 
   # facet_wrap(vars(COUNTY_SAMPLED), ncol = 2) +
   geom_point() + 
   labs(x = "Year", y = "LENGTH1_MM", 
        title = "area-time distribution of LENGTH1_MM PR") +
   theme(legend.position = "none") 
 
 # VI
 
 ggplot(data = com_tip_VI_STP_new, aes(x = FINAL_DATE, y = LENGTH1_MM, group = COUNTY_SAMPLED , color = COUNTY_SAMPLED )) + 
   facet_wrap(vars(COUNTY_SAMPLED), ncol = 2) +
   geom_point() + 
   labs(x = "Year", y = "LENGTH1_MM", 
        title = "area-time distribution of LENGTH1_MM VI") +
   theme(legend.position = "none")

 