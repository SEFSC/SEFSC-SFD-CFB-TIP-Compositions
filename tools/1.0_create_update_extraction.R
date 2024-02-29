# Create or update an earlier extraction ####

# Load libraries ####
librarian::shelf(here, tidyverse, ROracle, keyring, dotenv)

# Run prep files ####
source(file <- here("tools", "0.1_create_folder_structure.R"))
source(file <- here("tools", "0.2_access_oracle.R"))
source(file <- here("tools", "0.3_create_functions.R"))

# SEDAR 57U ####
cr_tip_sp(state_codes = c("PR", "VI"), sp_codes = c(97648, 97646))
n_cr_tip(state_codes = c("PR", "VI"))

# SEDAR 80 ####
cr_tip_sp(state_codes = c("PR", "VI"), sp_codes = c(173139))

# SEDAR 84 ####
cr_tip_sp(state_codes = c("PR", "VI"), sp_codes = c(170867))
cr_tip_sp(state_codes = c("PR", "VI"), sp_codes = c(168907))
cr_tip(state_codes = c("PR", "VI"))
