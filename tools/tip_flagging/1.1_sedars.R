#### Oracle exploration ####

# SEDAR 57U ####
    # Caribbean Spiny Lobster
  cr_tip_sp(state_codes = c('PR', 'VI'), sp_codes = c(97648, 97646))
  n_cr_tip(state_codes = c('PR', 'VI'))
  
  # Read in data 
  com_tip_PR_VI_97648_97646_20230712 <- 
    readRDS("~/SEFSC-SFD-CFB-TIP-Compositions/data/raw/com_tip_PR_VI_97648_97646_20230712.RDS")
  
  # Orient yourself         
  summary(com_tip_PR_VI_97648_97646_20230712$EFFORT_INTERVIEW_ID)
  head(com_tip_PR_VI_97648_97646_20230712)
  unique(com_tip_PR_VI_97648_97646_20230712$OBS_SPECIES_CODE) # Codes 901 and 618201000000
  unique(com_tip_PR_VI_97648_97646_20230712$EFFORT_INTERVIEW_ID)
  
  # All data points 
  plot(com_tip_PR_VI_97648_97646_20230712$LANDING_DATE, com_tip_PR_VI_97648_97646_20230712$LENGTH1)
  
  ## Find obvious outliers ##
  # LENGTH1 = 99104.00 ??? literally cannot figure out how to isolate this row of data, may need to clear enviro
  nrow(com_tip_PR_VI_97648_97646_20230712 == 99104.00)
  com_tip_PR_VI_97648_97646_20230712[103249, ]  # ID 424915 SAMPLE_INTERVIEW_ID EFFORT_INTERVIEW_ID
  # try again with new dataframe ??
  summary(com_tip_prvi_carsplob_length$LENGTH1)
  nrow(com_tip_prvi_carsplob_length$LENGTH1 == 99104.00)
  com_tip_prvi_carsplob_length[75318, "EFFORT_INTERVIEW_ID"]
  unique(com_tip_prvi_carsplob_length$LENGTH_UNIT1)
  
  # Filter to one length type 
  unique(com_tip_PR_VI_97648_97646_20230712$LENGTH_UNIT1) # MILLIMETERS, INCHES, CENTIMETERS, NO LENGTH 
  com_tip_prvi_carsplob_length <- com_tip_PR_VI_97648_97646_20230712 %>%
    filter(LENGTH_UNIT1 == 'MILLIMETERS')
           # LENGTH1 < 1000) #remove outliers 
  plot(com_tip_prvi_carsplob_length$LANDING_DATE, com_tip_prvi_carsplob_length$LENGTH1)
  


# SEDAR 46 ####
  # U.S. Caribbean Data-Limited Species: Yellowtail Snapper (PR) (168907), Hogfish (PR) (170566), 
  # Spiny Lobster (STT, STX) (97648, 97646), Queen Triggerfish (STT) (173139), Stoplight Parrotfish (STX) (170867)



# Other Species Considered for Data-Limited SEDAR: 
# PR:
  # Spiny Lobster (97648, 97646)
  # Silk Snapper (168861)
  # Queen Conch (72558)
  # Lane Snapper (168860)
  # White Grunt (169059)
  # King Mackerel (172435)
  # Dolphin (168790)
  # Queen Snapper (168902)
  # Mutton Snapper (168849)
  # Queen Triggerfish (173139)
  cr_tip_sp(state_codes = 'PR', sp_codes = 173139)
  cr_tip_sp(state_codes = 'VI', sp_codes = 173139)
  cr_tip_sp(state_codes = c('PR', 'VI'), sp_codes = 173139)
  # Red Hind (167700)
  cr_tip_sp(state_codes = 'PR', sp_codes = 167700)
  cr_tip_sp(state_codes = 'VI', sp_codes = 167700)
  cr_tip_sp(state_codes = c('PR', 'VI'), sp_codes = 167700)
  # Cero (172437)
  # Blackfin Tuna (172427)
  # Vermilion (168909)
  # Coney (167739, 167740)
  # Wahoo (172451)
  # Great Barracuda (170429)
  # Tripletail (169007)
  # Stoplight Parrotfish (170867)
  # Crevalle Jack (168609)

#STT
  # Red Hind (167700)
  cr_tip_sp(state_codes = 'PR', sp_codes = 167700)
  cr_tip_sp(state_codes = 'VI', sp_codes = 167700)
  cr_tip_sp(state_codes = c('PR', 'VI'), sp_codes = 167700)
  
  # Yellowtail Snapper (168907)
  # White Grunt (169059)
  # Blue Tang (172254)

#STX
  # Queen Conch (72558)
  # Dolphin (168790) 
  # Queen Parrotfish (170816)
  # Queen Triggerfish (173139)
  # Redtail Parrotfish (170864)
  # White Grunt (169059)


# SEDAR 84 ####
  # Caribbean Yellowtail Snapper (168907) and Stoplight Parrotfish (170867)
  cr_tip_sp(state_codes = 'PR', sp_codes = 170867)
  cr_tip_sp(state_codes = 'VI', sp_codes = 170867)
  cr_tip_sp(state_codes = c('PR', 'VI'), sp_codes = 170867)
  
  
cr_tip_sp(state_codes = c('PR', 'VI'), sp_codes = c(168907, 170867))
n_cr_tip(state_codes = c('PR', 'VI'))