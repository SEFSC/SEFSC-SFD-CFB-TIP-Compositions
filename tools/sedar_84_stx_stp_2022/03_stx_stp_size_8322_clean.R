# Load libraries ####
librarian::shelf(here, tidyverse)

# Specify settings ####
tip_spp_len_rds <- "stx_slp_prep_tip_20240123.rds"
spp <- "slp"
isl <- "stx"
subset_years <- c(2012:2022)

# Read in formatted data ####
tip_spp_len <- readRDS(here::here("data", tip_spp_len_rds))

# Remove outliers ####
tip_spp_use <- tip_spp_len |>
  filter(k_keep == TRUE)

# Save clean tip_spp_use ####
saveRDS(
  tip_spp_use,
  file = here::here(
    "data",
    paste0(
      isl, "_",
      spp, "_use_tip_",
      format(Sys.time(), "%Y%m%d"), ".rds"
    )
  )
)

# Do gear statistical comparison ####

# Do gear statistical comparison on subset of years####
