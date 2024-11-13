# 05c_nonconf figures requested for sedar


# 05_plot

#' plot the filtered data included
#' 1 - gears across time
#' 2 - aggregated density of lengths 


# Load libraries ####
librarian::shelf(here, tidyverse, flextable, ggplot2, janitor)

# Specify settings ####
# date from end of 05a script
date <- "20241108" 
# date from non-confidential gears (rds from 04a)
gear_date <- "20241108" 
spp <- "csl"
print_spp <- "Caribbean Spiny Lobster"

# isl <- "pr"
# print_isl <- "Puerto Rico"
isl <- "stx"
print_isl <- "St. Croix"
# isl <- "stt"
# print_isl <- "St. Thomas/St. John"

min_year <- 1981
max_year <- 2023
len_type <- "CARAPACE LENGTH"
target_len <- "Carapace Length"
sedar <- "sedar91"
disclaimer <- "Only nonconfidential data shown."


# Read in formatted data ####
tip_spp_rds <- paste0(isl, "_", spp, "_clean_filtered_", date, ".rds" )
tip_spp <- readRDS(here::here("data",  sedar, "rds", spp, isl, tip_spp_rds))

gear_list <- paste0(isl, "_", spp, "_nonconf_gear_list_", gear_date, ".rds" )
gear_nonconf <- readRDS(here::here("data",  sedar, "rds", spp, isl, gear_list))

# create bin sizes
qtr_in = seq(2,10.25,.25) #STARTING AT 2 INCHES
qtr_in_mm = round(qtr_in*25.4)
binSizes <- qtr_in_mm[2:34]-qtr_in_mm[1:33]
binPrep <- data.frame(cbind(bin = qtr_in_mm[1:33], space = binSizes))
binVector <- uncount(binPrep, space)$bin
lengths <- seq(bottomBin, max(binVector), 1)
bin <- binVector[1:length(lengths)]
bin_merge <- data.frame(cbind(lengths, bin))
binSize <- "mm_qtrINCH"

# Plot length values recorded over time ####
length_bin <- tip_spp_count |>
  ggplot(aes(
    x = date,
    y = length1_cm,
    group = island,
    color = length_type1
  )) +
  # facet_wrap(~islandn, ncol = 2) +
  # facet_grid(species_code ~ island) +
  geom_point() +
  labs(
    x = "Year", y = "Length (cm)",
    title = paste(print_isl,"Area-time distribution of Lengths sampled"),
    color = "Length Type (# obs)",
    subtitle = paste("N = ", nrow(tip_spp_count))
  )
# view plot  
length_bin

