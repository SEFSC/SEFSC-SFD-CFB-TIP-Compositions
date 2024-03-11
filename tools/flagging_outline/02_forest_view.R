# 02_forest_view

# Load libraries ####
librarian::shelf(
  here, tidyverse, dotenv, reshape, openxlsx, janitor, DT,
  pander, knitr, flextable, ggplot2, lmerTest, meantables
)

# Specify settings ####
tip_spp_rds <- "pr_yts_format_tip_20240307.rds" # rds from end of 01 script
spp <- "yts"
isl <- "pr"

# Read in formatted data ####
tip_spp <- readRDS(here::here("data", tip_spp_rds))

# Select variables relevant to flagging investigation ####
tip_spp_relevant <- tip_spp |>
  select(
    id, # trip interview id
    interview_date,
    year,
    island, # pr, stt, stx
    obs_standard_species_code,
    length1_cm, # calculated from length1_mm
    length1_inch, # calculated from length1_mm
    obs_weight_lbs, # calculated from obs_weight
    k, # calculated 01
    sex_name, # male, female, unknown, not sexed
    gear, # gear from land_standard_gear_name subbed w/ gear_1 when unavailable
    county_sampled, # sampled county in PR
    length_unit1, # original length unit
    length_type1, # original length type
    obs_weight_unit, # original weight unit
    sample_condition,
    sector
  )

# Find first year and min/max lengths ####
### use this as preliminary look at possible inaccurate lengths
# compare type1 and type2 lengths 
table(tip_spp$length_type1, useNA='always')
table(tip_spp$length_type2, useNA='always')

min(tip_spp_relevant$length1_cm,na.rm = TRUE)
max(tip_spp_relevant$length1_cm,na.rm = TRUE)

tip_range  <- tip_spp_relevant[with(tip_spp_relevant,order(-length1_cm)),]
tip_range$length1_cm[1:25]

tip_range2 <- tip_spp_relevant[with(tip_spp_relevant,order(length1_cm)),]
tip_range2$length1_cm[1:25]

min(tip_spp_relevant$year,na.rm = TRUE)

# Specify settings ####
## Range currently set to not drop any obs 
min_size <- 1
max_size <- 122   
min_year <- 1983
max_year <- 2022 


# Create count of observed records for each area  ####
tip_spp_count <- tip_spp_relevant |>
  add_count(county_sampled) |>
  dplyr::mutate(county_sampledn = paste0(county_sampled, " (", n, ")")) |>
  select(-n) |>
  add_count(gear) |>
  dplyr::mutate(gearn = paste0(gear, " (", n, ")")) |>
  select(-n)

# Plot regions sampled over time ####
## If working with PR ####
county_data <- tip_spp_count |>
  # group_by(gear) %>%
  # dplyr::mutate(n_id = n_distinct(id)) |>
  # dplyr::filter(n_id >= 3) %>% ungroup %>%
  group_by(year, county_sampledn) |>
  dplyr::summarize(n = n(), .groups = "drop") |>
  mutate(year = as.integer(year))

county_by_year <- county_data |>
  group_by(county_sampledn) |>
  dplyr::mutate(total_n = sum(n)) |>
  ungroup() |>
  dplyr::mutate(county_sampledn = fct_reorder(county_sampledn, total_n)) %>%
  ggplot(aes(x = year, y = county_sampledn, color = county_sampledn, size = n)) +
  geom_point() +
  labs(
    x = "Year", y = "", colour = "", shape = "",
    title = paste(isl, "Length Samples")
  ) +
  theme_bw() +
  theme(
    legend.position = "null", text = element_text(size = 12),
    title = element_text(size = 15)
  )


# Plot gears used over time ####
gear_data <- tip_spp_count |>
  # group_by(gear) %>%
  # dplyr::mutate(n_id = n_distinct(id)) |>
  # dplyr::filter(n_id >= 3) %>% ungroup %>%
  group_by(year, gearn) |>
  dplyr::summarize(n = n(), .groups = "drop") |>
  mutate(year = as.integer(year))

gear_by_yr <- gear_data |>
  group_by(gearn) |>
  dplyr::mutate(total_n = sum(n)) |>
  ungroup() |>
  dplyr::mutate(gearn = fct_reorder(gearn, total_n)) %>%
  ggplot(aes(x = year, y = gearn, color = gearn, size = n)) +
  geom_point() +
  labs(
    x = "Year", y = "", colour = "", shape = "",
    title = paste(isl, "Length Samples")
  ) +
  theme_bw() +
  theme(
    legend.position = "null", text = element_text(size = 15),
    title = element_text(size = 15)
  )

# Tabulate weight types ####
weight_types <- tip_spp_relevant |>
  dplyr::group_by(
    island,
    obs_standard_species_code,
    sample_condition
  ) |>
  dplyr::summarize(
    .groups = "drop",
    n = dplyr::n(),
    percent = round(n / nrow(tip_spp) * 100, 1),
    first_year = min(year),
    last_year = max(year),
    n_years = dplyr::n_distinct(year)
  )

# Plot weight values recorded over time ####
weight_time <- tip_spp_count |>
  ggplot(aes(x = interview_date, 
             y = obs_weight_lbs)) +
           # group = COUNTY_SAMPLED , 
           # color = COUNTY_SAMPLEDn )) +
  # if pr include group and color
  facet_wrap(vars(county_sampledn), ncol = 5) + # include if pr
  geom_point() +
  labs(
    x = "Year", y = "Weight (lbs)",
    title = "Area-time distribution of Weights sampled",
    color = "COUNTY_SAMPLED (# obs)",
    subtitle = paste("N = ", nrow(tip_spp_relevant))
  )


# Tabulate length types ####
length_types <- tip_spp_relevant |>
  dplyr::group_by(
    island,
    obs_standard_species_code,
    length_type1
  ) |>
  dplyr::summarize(
    .groups = "drop",
    n = dplyr::n(),
    percent = round(n / nrow(tip_spp) * 100, 1),
    first_year = min(year),
    last_year = max(year),
    n_years = dplyr::n_distinct(year),
    na_length1_cm = sum(is.na(length1_cm)),
    percent_na_length1_cm = round(na_length1_cm / n * 100, 1),
    na_obs_weight_lbs = sum(is.na(obs_weight_lbs)),
    percent_na_obs_weight_lbs = round(na_obs_weight_lbs / n * 100, 1),
  )

view(length_types)

# Plot length values recorded over time ####
length_time <- tip_spp_count |>
  ggplot(aes(
    x = interview_date,
    y = length1_cm,
    group = county_sampled,
    color = county_sampledn
  )) +
  # facet_wrap(vars(COUNTY_SAMPLED), ncol = 2) +
  geom_point() +
  labs(
    x = "Year", y = "Length (cm)",
    title = "Area-time distribution of Lengths sampled",
    color = "COUNTY_SAMPLED (# obs)",
    subtitle = paste("N = ", nrow(tip_spp_count))
  )

# Save formatted tip_spp ####
saveRDS(
  tip_spp_count,
  file = here::here(
    "data",
    paste0(
      isl, "_",
      spp, "_forest_tip_",
      format(Sys.time(), "%Y%m%d"), ".rds"
    )
  )
)
