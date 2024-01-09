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
  tally()

com_tip_PR_04_spc_name <- com_tip_PR_2004_unique_SPC |> 
  dplyr::rename(n_tip = n) |> 
  mutate(spc_name = )
com_tip_PR_04_spc_name$spc_name <- tolower(com_tip_PR_04_spc_name$spc_name)

pr_04_unique_spc <- pr_04 %>% 
  group_by(SPECIES_B) %>% 
  tally()
pr_04_unique_spc_renamed <- pr_04_unique_spc |> 
  dplyr::rename(n_hist = n)

# load historic species codes
pr_species_codes_historic <- read_csv("tools/flagging_historical/lookup_tables/pr_species_codes_historic.csv")

pr_species_codes_historic$sp_dner = str_remove(pr_species_codes_historic$sp_dner, "^0+")

# add species names to list of historic codes
pr_04_unique_spc_name <- pr_04_unique_spc_renamed %>%
  mutate(spc_name = pr_species_codes_historic$common[match(pr_04_unique_spc$SPECIES_B, pr_species_codes_historic$sp_dner)])

pr_04_unique_spc_name$spc_name <- tolower(pr_04_unique_spc_name$spc_name)


# compare species names
unique_species_merge_PR <- merge(com_tip_PR_04_spc_name,pr_04_unique_spc_name, by = 'spc_name', all = TRUE)


