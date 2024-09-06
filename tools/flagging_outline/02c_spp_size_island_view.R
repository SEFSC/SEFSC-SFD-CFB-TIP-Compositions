# 02c_spp_size_island_view
# plot counties, gears, length/weight pairs specific to island

# Load libraries ####
librarian::shelf(
  here, tidyverse, janitor, flextable, ggplot2
)

# Specify settings #### 
tip_spp_rds <- "prusvi_csl_spp_size_prep_20240906.rds" # rds from end of 02a script
spp_itis <- c("097648", "097646") # find on itis.gov
spp <- "csl"
isl <- c("pr", "stt", "stx") 
print_spp <- "Caribbean Spiny Lobster"
print_isl <- "Puerto Rico - USVI"
save_spp <- "csl"
save_isl <- "prusvi"

# Read in formatted data ####
tip_spp <- readRDS(here::here("data", tip_spp_rds))

# Filter to target island ####
tip_spp_prep <- tip_spp |>
  dplyr::filter(island %in% isl)

# check for gears with same name but grammatical differences
unique(tip_spp_prep$gear)
# replace "," with ";"
tip_spp_prep$gear <- str_replace(tip_spp_prep$gear, ",", ";")

# Create count of observed records for each area  ####
tip_spp_count <- tip_spp_prep |>
  add_count(county_landed) |>
  dplyr::mutate(county_landedn = paste0(county_landed, " (", n, ")")) |>
  select(-n) |>
  add_count(gear) |>
  dplyr::mutate(gearn = paste0(gear, " (", n, ")")) |>
  select(-n)|>
  add_count(island) |>
  dplyr::mutate(islandn = paste0(island, " (", n, ")")) |>
  select(-n)

# Plot regions sampled over time ####
## plots individual counties of PR along with usvi islands 
county_data <- tip_spp_count |>
  group_by(year, island, county_landedn) |>
  dplyr::summarize(n = n(), .groups = "drop") |>
  mutate(year = as.integer(year))

county_by_year <- county_data |>
  group_by(county_landedn) |>
  dplyr::mutate(total_n = sum(n)) |>
  ungroup() |>
  dplyr::mutate(county_landedn = fct_reorder(county_landedn, total_n)) %>%
  ggplot(aes(x = year,
             y = county_landedn, 
             color = county_landedn, 
             size = n)) +
  facet_wrap(~island) +
  geom_point() +
  labs(
    x = "Year", y = "", colour = "", shape = "",
    title = paste(print_isl, "Length Samples")
  ) +
  theme_bw() +
  theme(
    legend.position = "null", text = element_text(size = 10),
    title = element_text(size = 12)
  )
county_by_year

# Plot gears used over time ####
gear_data <- tip_spp_count |>
  group_by(year, island, species_code, gearn) |>
  dplyr::summarize(n = n(), .groups = "drop") |>
  mutate(year = as.integer(year))

gear_by_yr <- gear_data |>
  group_by(gearn) |>
  dplyr::mutate(total_n = sum(n)) |>
  ungroup() |>
  dplyr::mutate(gearn = fct_reorder(gearn, total_n)) |> 
  ggplot(aes(x = year, y = gearn, color = gearn, size = n)) +
  facet_grid(species_code ~ island) +
  geom_point() +
  labs(
    x = "Year", y = "", colour = "", shape = "",
    title = paste(print_isl, "Length Samples")
  ) +
  theme_bw() +
  theme(
    legend.position = "null", text = element_text(size = 10),
    title = element_text(size = 12)
  )
gear_by_yr

# Plot weight values recorded over time ####
weight_time <- tip_spp_count |>
  ggplot(aes(x = date, 
             y = obs_weight_lbs, 
             color = sample_condition)) +
  # facet_wrap(~islandn, ncol = 2) + 
  facet_grid(species_code ~ island) +
  geom_point() +
  labs(
    x = "Year", y = "Weight (lbs)",
    title = "Area-time distribution of Weights sampled",
    color = "Sample Condition (# obs)",
    subtitle = paste("N = ", nrow(tip_spp_count))
  )
weight_time

# Plot length values recorded over time ####
length_time <- tip_spp_count |>
  ggplot(aes(
    x = date,
    y = length1_cm,
    group = island,
    color = length_type1
  )) +
  # facet_wrap(~islandn, ncol = 2) +
  facet_grid(species_code ~ island) +
  geom_point() +
  labs(
    x = "Year", y = "Length (cm)",
    title = "Area-time distribution of Lengths sampled",
    color = "Length Type (# obs)",
    subtitle = paste("N = ", nrow(tip_spp_count))
  )
length_time


# # Save formatted tip_spp ####
# saveRDS(
#   tip_spp_count,
#   file = here::here(
#     "data",
#     paste0(
#       isl, "_",
#       spp, "_spp_size_island_view_",
#       format(Sys.time(), "%Y%m%d"),
#       ".rds"
#     )
#   )
# )
