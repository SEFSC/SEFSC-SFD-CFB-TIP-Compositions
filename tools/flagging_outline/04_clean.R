# 04_clean

#' execute statistical analysis of gear-length relationships (glmm) and 
#' k calculations to determine possible gear groupings and data outliers

# Load libraries ####
librarian::shelf(here, tidyverse)

# Specify settings ####
tip_spp_rds <- "pr_yts_prep_tip_20240229.rds" # rds from end of 02 script
