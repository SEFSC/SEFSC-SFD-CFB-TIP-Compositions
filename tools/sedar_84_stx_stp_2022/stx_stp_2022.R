# Load libraries ####
librarian::shelf(here, tidyverse, measurements)

# Specify settings ####
print_isl <- "St. Croix"
print_spp <- "Stoplight Parrotfish"
spp_code <- 170867
spp <- "slp"
isl <- "stx"
sedar <- "sedar_84ba_cr_2022"

# Specify File names ####
tip_rds <- "com_tip_PR_VI_20240216.rds"
drna_rds <- "slp_size_drna_1118_20240222.rds"
seampc_rds <- "slp_size_drna_9697_20240222.rds"
usca_rds <- "slp_size_usca_1522_20240222.rds"

# Read in, format and tabulate TIP data ####
tip <- readRDS(here::here("data", "raw", tip_rds))
source(here::here("tools", "01a_tip_format.r"))
source(here::here("tools", "01b_tip_overview.r"))

plot_count_overview
plot_percent_overview
plot_count_spp_sector

# Read in other sources of data ####
drna <- readRDS(here::here("data", sedar, "use", drna_rds))
seampc <- readRDS(here::here("data", sedar, "use", seampc_rds))
usca <- readRDS(here::here("data", sedar, "use", usca_rds))

# Combine all data and filter to species of interest ####
spp_size <- dplyr::bind_rows(tip_r, drna, seampc, usca) %>%
  dplyr::filter(species_code == spp_code)

# Convert units and calculate K ####
source(here::here("tools", "02a_size_prep.R"))

plot_count_lw_pairs
plot_lw_pairs
plot_spp_length
plot_spp_length_year
plot_spp_weight
plot_spp_weight_year

# Specify settings ####
len_type <- "FORK LENGTH"

# Analyze K across sources of data ####
source(here::here("tools", "02b_size_k.R"))
k_summary
plot_k
plot_k_island

# Specify settings ####
k_range_source <- "usca"

# Flag TIP using K analysis ####
source(here::here("tools", "02c_size_flag.R"))
k_range_lower
K_range_upper
plot_count_lw_flag

# Specify settings ####
tip_mode <- "COMMERCIAL"

# Focus on island and mode ####
source(here::here("tools", "02d_size_sedar.R"))
plot_spp_isl_flag
plot_spp_isl_year_flag
plot_count_spp_isl_overview

# Specify settings ####
tip_start_year <- 2012
tip_end_year <- 2022



# Filter to data for SEDAR assessment ####





# Specify settings for plots ####

# Specify settings for binning ####


