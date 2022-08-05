# Create or update an earlier extraction ####

# Load libraries
librarian::shelf(here, tidyverse, ROracle, keyring, dotenv)

# Run prep files
source(file <- here("Tools", "0.1_create_folder_structure.R"))
source(file <- here("Tools", "0.2_access_oracle.R"))
source(file <- here("Tools", "0.3_create_functions.R"))

# SEDAR 57U
  cr_tip_sp(state_codes = c('PR', 'VI'), sp_codes = c(97648, 97646))
