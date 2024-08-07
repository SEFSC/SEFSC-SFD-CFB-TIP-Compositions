# 3a_species

# Load libraries ####
librarian::shelf(here, tidyverse, measurements, flextable, ggplot2, reshape2)

# Specify settings ####
tip_pr <- "pr_gear_grouped_tip_20240807.rds" # add formatted data

# Read in raw data ####
tip <- readRDS(here::here("data", tip_pr))
