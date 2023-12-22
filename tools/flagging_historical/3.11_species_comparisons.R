# species comparision

# using 2004 data for example 

# Load data ####
librarian::shelf(here, tidyverse, ROracle, keyring, dotenv, lubridate, readr)

# Load historical data
PR_historical_04 <- read.csv("data/raw/for_import_04.csv")

pr_04 <- PR_historical_04 %>%
  # Format dates to be mm/dd/yyyy 
  mutate(INTDATE = as.Date(INTDATE, "%m/%d/%Y"))

# Load oracle data 
load(file = "data/dataframes/com_tip_PR_2004_ORGANIZED.Rda")

# FIND UNIQUE SPECIES CODES AND # OF OCCURANCES ####

com_tip_PR_2004_unique_SPC <- com_tip_PR_2004_ORGANIZED %>% 
  group_by(OBS_STANDARD_SPECIES_NAME) %>% 
  summarize(count=n())

pr_04_unique_spc <- pr_04 %>% 
  group_by(SPECIES_B) %>% 
  summarize(count=n())

# load historic species codes
pr_species_codes_historic <- read_csv("tools/flagging_historical/lookup_tables/pr_species_codes_historic.csv")



pr_04_unique_spc_name <- pr_04_unique_spc %>%
  mutate(spc_name = pr_species_codes_historic$CNTY_NAME[match(pr_04_unique_spc$place_id_HISTORICAL, pr_species_codes_historic$place_id_HISTORICAL)])

