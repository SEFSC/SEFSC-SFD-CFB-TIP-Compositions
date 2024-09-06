# 03b_removed
# document all records removed throughout analysis

# Load libraries ####
librarian::shelf(here, tidyverse)

# Specify settings ####
tip_spp_rds <- "prusvi_csl_spp_size_prep_20240906.rds" # rds from end of 02a script
# tip_spp_rds_k <- "pr_yts_spp_size_flag_20240618.rds" # rds from end of 02d script
spp <- "csl"
isl <- "pr" # from here on chose one island to work with moving forward
data_keep <- "TIP"
len_mode <- "COMMERCIAL"
len_type <- "CARAPACE LENGTH"
# min_size <- 1 # add if there are outliers that need to be removed
# max_size <- 122
start_yr <- 1984 # based on 01a settings unless running truncated time series
end_yr <- 2022
print_isl <- "Puerto Rico"


# Read in formatted data ####
tip_spp <- readRDS(here::here("data", tip_spp_rds))

# Create flag to denote removed records ####
## flag incorrect island and na lengths
tip_spp_flag1 <- tip_spp |>
  dplyr::mutate(
    remove_flag = case_when(
      island != isl ~ "drop",
      # data_source != data_keep ~ "drop",
      is.na(length1_mm) ~ "drop",
      .default = "keep"
    )
  )

## flag too small sample sizes
tip_spp_flag2 <- tip_spp |>
  filter(
    island == isl,
    # data_source == data_keep,
    !is.na(length1_mm),
  ) |>
  group_by(gear) |>
  dplyr::mutate(n_ID = n_distinct(id)) |>
  ungroup() |>
  group_by(year) |>
  dplyr::mutate(n_yr = n()) |>
  ungroup() |> 
dplyr::mutate(
  remove_flag = case_when(
    n_ID < 3 ~ "drop",
    n_yr < 30 ~ "drop",
    .default = "keep"
  )
) |> 
  select(-n_ID, -n_yr)

## flag incorrect sector, len_type, year
tip_spp_flag3 <- tip_spp |>
  filter(
    island == isl,
    # data_source == data_keep,
    !is.na(length1_mm),
  ) |>
  group_by(gear) |>
  dplyr::mutate(n_ID = n_distinct(id)) |>
  dplyr::filter(n_ID >= 3) |>
  ungroup() |>
  group_by(year) |>
  filter(n() >= 30) |>
  ungroup() |>
  dplyr::mutate(
    remove_flag = case_when(
    sector != len_mode ~ "drop",
    length_type1 != len_type ~ "drop",
    year < start_yr ~ "drop", 
    year > end_yr ~ "drop",
    # length1_cm < min_size ~ "drop",
    # length1_cm > max_size ~ "drop", 
    .default = "keep"
  )) |> 
  select(-n_ID)

# filter to only removed data from 3 levels of filtering 
tip_settings_removed1 <- tip_spp_flag1 |>
  filter(remove_flag == "drop")

tip_settings_removed2 <- tip_spp_flag2 |>
  filter(remove_flag == "drop")

tip_settings_removed3 <- tip_spp_flag3 |>
  filter(remove_flag == "drop")

# combine flagged records into one data set ####
tip_dropped <- tip_settings_removed1 |> 
  bind_rows(tip_settings_removed2) |> 
  bind_rows(tip_settings_removed3)
  

# Save general rds ####
saveRDS(
  tip_dropped,
  file = here::here(
    "data",
    paste0(
      isl, "_",
      spp, "_prep_dropped_tip_",
      format(Sys.time(), "%Y%m%d"), ".rds"
    )
  )
)


# Read in k data ####
tip_spp_k <- readRDS(here::here("data", tip_spp_rds_k))

tip_settings_removed_k <- tip_spp_flag |>
  filter(
    island = isl,
    remove_flag == "drop"
  )
# Save k rds ####
saveRDS(
  tip_settings_removed_k,
  file = here::here(
    "data",
    paste0(
      isl, "_",
      spp, "_prep_remove_tip_",
      format(Sys.time(), "%Y%m%d"), ".rds"
    )
  )
)
