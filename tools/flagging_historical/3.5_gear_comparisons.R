# Gear comparisons 

# extract oracle by yr and state: 

cr_tip_yr(state_codes = 'PR', year = "2004")

cr_tip_yr2(state_codes = "PR", st_year = "2003", end_year = "2005")

# Load data ####
librarian::shelf(here, tidyverse, ROracle, keyring, dotenv, lubridate, readr)

# Load historical data
PR_historical_04 <- read.csv("data/raw/for_import_04.csv")

pr_04 <- PR_historical_04 %>%
    # Format dates to be mm/dd/yyyy 
  mutate(INTDATE = as.Date(INTDATE, "%m/%d/%Y"))

# Load oracle data 
com_tip_PR_2004 <- readRDS("~/SEFSC-SFD-CFB-TIP-Compositions/data/raw/com_tip_PR_2004_20231004.RDS")


# Compile gear code tables 

# Load tip gear table 
tip_gear_groups <- read_csv("data/CSVs/TIPS_GEAR_GROUPS.csv")
tip_gear_groups$STANDARD_NAME<- tolower(tip_gear_groups$STANDARD_NAME)
# edit entries to make alignment easier 
tip_gear_groups[2,4] = 'beach/haul seine' # was haul seines
tip_gear_groups[7,4] = 'trammel net' # was trammel nets
tip_gear_groups[12,4] = 'cast net' # was cast nets
#tip_gear_groups[10,4] = 'troll line' # was troll line-manual
tip_gear_groups[14,4] = 'bottom/hand line' # was lines hand (LINEA DE MANO-HAND LINE)
tip_gear_groups[15,4] = 'bottom/hand line' # was lines hand (LINEA DE FONDO-BOTTOM LINE)
tip_gear_groups[19,4] = 'scuba diving' # was by hand, diving gear (BUCEO CON TANQUE-SCUBA DIVING)
tip_gear_groups[5,4] = 'land crab trap'# was pots, other (TRAMPA DE JUEYES-CRAB TRAP)

save(tip_gear_groups,file="data/dataframes/tip_gear_groups.Rda") # file is saved in data folder  

  
# Load historical gear table
PR_historical_gear_groups <- read_csv("data/CSVs/CFSP Gear Code.xlsx - Sheet1.csv")
PR_historical_gear_groups$English<- tolower(PR_historical_gear_groups$English)
PR_historical_gear_groups_grammar <- PR_historical_gear_groups %>%
  rename(STANDARD_NAME = English,
         GEARCODE = `Gear Code`)
PR_historical_gear_groups_grammar[5,3] = 'bottom/hand line' # was bottom or hand line
PR_historical_gear_groups_grammar[4,3] = 'entangling nets (gill) unspc' # was gill net
PR_historical_gear_groups_grammar[7,3] = 'lines long set with hooks' # was long line
PR_historical_gear_groups_grammar[1,3] = 'beach/haul seine' # was beach seine
PR_historical_gear_groups_grammar[2,3] = 'pots and traps, fish' # was fish trap
PR_historical_gear_groups_grammar[3,3] = 'pots and traps, spiny lobster' # was lobster traps
PR_historical_gear_groups_grammar[6,3] = 'lines power troll other' # was troll line

save(PR_historical_gear_groups_grammar,file="data/dataframes/PR_historical_gear_groups_grammar.Rda") # file is saved in data folder  

# combine tables 
gear_codes <- merge(tip_gear_groups, PR_historical_gear_groups_grammar, by = 'STANDARD_NAME', all = TRUE)

save(gear_codes,file="data/dataframes/gear_codes.Rda") # file is saved in data folder  

# FIND UNIQUE GEAR CODES AND # OF OCCURANCES ####
colnames(com_tip_PR_2004)

# unique GEAR CODES and number of occurrences - ORACLE - 11 UNIQUE GEAR CODES
com_tip_PR_2004_unique_GEAR <- com_tip_PR_2004 %>% 
  group_by(STANDARDGEAR_1) %>% 
  summarize(count=n())
com_tip_PR_2004_unique_GEAR_updated <- com_tip_PR_2004_unique_GEAR %>%
  rename(count_ORACLE = count,
         STANDARD_GEAR = STANDARDGEAR_1) 
#com_PR_lob_unique_MUNI_updated$muni_zip_ORACLE = as.numeric(as.character(com_PR_lob_unique_MUNI_updated$muni_zip_ORACLE))
com_tip_PR_2004_unique_GEAR_NAMES <- com_tip_PR_2004_unique_GEAR_updated %>%
  mutate(GEAR_name = gear_codes$STANDARD_NAME[match(com_tip_PR_2004_unique_GEAR_updated$STANDARD_GEAR, gear_codes$STANDARD_GEAR)])


# unique dates and number of occurrences- HISTORICAL- 13 UNIQUE MUNI_ZIP
pr_04_unique_GEAR <- pr_04 %>% 
  group_by(GEARCODE) %>% 
  summarize(count=n())
pr_04_unique_GEAR_UPDATED <- pr_04_unique_GEAR %>%
  rename(count_HISTORICAL = count)
#pr_04_sp901_unique_MUNI_updated$muni_zip_HISTORICAL = as.numeric(as.character(pr_04_sp901_unique_MUNI_updated$muni_zip_HISTORICAL))
pr_04_unique_GEAR_NAMES <- pr_04_unique_GEAR_UPDATED %>%
  mutate(GEAR_name = gear_codes$STANDARD_NAME[match(pr_04_unique_GEAR_UPDATED$GEARCODE, gear_codes$GEARCODE)])

# merge tables
unique_GEAR_merge_PR <- merge(com_tip_PR_2004_unique_GEAR_NAMES,pr_04_unique_GEAR_NAMES, by = 'GEAR_name', all = TRUE)

# ORACLE by hand, diving gear (snare) vs HISTORICAL scuba diving and skin diving 

write.csv(unique_GEAR_merge_PR, file = "data/CSVs/GEAR_code_comparison_2004.csv", row.names = FALSE)
save(unique_GEAR_merge_PR,file="data/dataframes/GEAR_code_comparison_2004.Rda") # file is saved in data folder  

