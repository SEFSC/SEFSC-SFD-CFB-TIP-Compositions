# 4a_lf_filter

# Load libraries ####
librarian::shelf(here, tidyverse, measurements, flextable, ggplot2, reshape2)

# Specify settings ####
tip_pr <- "pr_species_name_tip_20240822.rds" # add formatted data
spp <- 173139
print_spp <- "qtf"
# queen triggerfish qtf 173139, 
# redtail parrotfish rtp 170864, 
# stoplight parrotfish slp 170867,
# lane snapper ls 168860, 
# silk snapper ss 168861, 
# queen snapper qs 168902, 
# schoolmaster sms 168850, 
# red hind reh 167700, 
  
# Read in raw data ####
tip <- readRDS(here::here("data", tip_pr))

# filter to species 
tip_species <- tip |> 
  filter(species_code %in% spp
         ) 

# apply interview and gear filters 
tip_filtered <- tip_species |> 
  group_by(st_yr, region, gear, ) |>
  dplyr::mutate(n_ID = n_distinct(id)) |>
  dplyr::filter(n_ID >= 2) |>
  ungroup() |>
  group_by(st_yr, region, gear, ) |>
  filter(n() >= 20) |>
  ungroup()

# Save formatted tip_spp ####
saveRDS(
  tip_filtered,
  file = here::here(
    "data",
    paste0("pr_filtered_tip_",print_spp, "_",
           format(Sys.time(), "%Y%m%d"), 
            ".rds"
    )
  )
)
