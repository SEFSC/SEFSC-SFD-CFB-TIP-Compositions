# 02dd_spp_size_discard_survey
# check for discard entries and survey interview types

# Load libraries ####
librarian::shelf(
  here, tidyverse, janitor, flextable, ggplot2
)

# Specify settings #### 
# rds from end of 02aa script
date <- "20241118" 
spp <- "csl"
print_spp <- "Caribbean Spiny Lobster"
isl <- c("pr", "stt", "stx")
print_isl <- "Puerto Rico - USVI"
# folder name  
sedar <- "sedar91"

# Read in formatted data ####
tip_spp_rds <- paste0("prusvi_csl_spp_size_quantity_", date, ".rds" )
tip_spp <- readRDS(here::here("data", sedar, "rds", spp, "all", tip_spp_rds))

unique(tip_spp$int_type)
unique(tip_spp$obs_grade)

# Filter to discard flagged records ####
tip_spp_discard <- tip_spp |>
  filter(discard_flag == "TRUE")

# Plot discard records by fishery and len type and island
plot_discard_flag <- tip_spp_discard |>
  dplyr::mutate(fishery_sampling_program = paste(int_type, sampling_program)) |>
  # dplyr::filter(record_type == "complete") |>
  ggplot2::ggplot(aes(
    x = year,
    y = length1_cm,
    color = fishery_sampling_program
  )) +
  ggplot2::geom_point() +
  ggplot2::facet_grid(~ island) +
  ggplot2::theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )+
  labs(
    x = "Year", y = "Length (cm)",
    title = paste(print_spp, print_isl, "Discard Flag"),
    subtitle = paste("Total N = ", nrow(tip_spp_check))
  )
# view  
plot_discard_flag
# save
ggsave(filename = 
         here::here("data", sedar, "figure", spp, "all", "plot_discard_flag.png"),
       width = 14, height = 8)

# Filter to discard flagged records ####
tip_spp_survey <- tip_spp |>
  filter(vi_survey_flag == "TRUE")

# Plot discard records by fishery and len type and island
plot_survey_flag <- tip_spp_survey |>
  dplyr::mutate(fishery_sampling_program = paste(obs_grade, sampling_program)) |>
  # dplyr::filter(record_type == "complete") |>
  ggplot2::ggplot(aes(
    x = year,
    y = length1_cm,
    color = fishery_sampling_program
  )) +
  ggplot2::geom_point() +
  ggplot2::facet_grid(~ island) +
  ggplot2::theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )+
  labs(
    x = "Year", y = "Length (cm)",
    title = paste(print_spp, print_isl, "Survey Flag"),
    subtitle = paste("Total N = ", nrow(tip_spp_check))
  )
# view  
plot_survey_flag
# save
ggsave(filename = 
         here::here("data", sedar, "figure", spp, "all", "plot_survey_flag.png"),
       width = 14, height = 8)


# create flag to flag whole trip if one discard is included
discard_id <- tip_spp |> 
  mutate(discard_trip_flag = case_when(sampling_unit_id %in% tip_spp_discard$sampling_unit_id ~ "TRUE", 
                                        TRUE ~ "FALSE"))

# check trips included discards 
discard_trips <- discard_id |> 
  select(year, sampling_unit_id, island, int_type, obs_grade, gear, length1_cm, 
         length1_inch, obs_weight_kg, discard_flag, vi_survey_flag, discard_trip_flag ) |> 
  filter(discard_trip_flag == "TRUE")

# removed discard_trip flags
remove_discard <- discard_id |> 
  filter(discard_trip_flag == "FALSE")

# save dataframe of discards
saveRDS(
  discard_trips,
  file = here::here(
    "data",
    sedar,
    "rds",
    spp, 
    "all",
    paste0(
      spp, "_spp_size_discard_trips_",
      format(Sys.time(), "%Y%m%d"), ".rds"
    )
  )
)

# save dataframe of discards
saveRDS(
  remove_discard,
  file = here::here(
    "data",
    sedar,
    "rds",
    spp, 
    "all",
    paste0(
      spp, "_spp_size_discard_removed_",
      format(Sys.time(), "%Y%m%d"), ".rds"
    )
  )
)
