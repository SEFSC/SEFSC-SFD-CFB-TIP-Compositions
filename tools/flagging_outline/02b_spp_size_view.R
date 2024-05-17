# 02b_spp_size_view
# plot counties, gears, length/weight pairs

# Load libraries ####
librarian::shelf(
  here, tidyverse, janitor, flextable, ggplot2
)

# Specify settings #### 
tip_spp_rds <- "pr_yts_spp_size_prep_20240410.rds" # rds from end of 02a script
spp_itis <- "168907" # find on itis.gov
spp <- "yts"
isl <- "pr"
print_spp <- "Yellowtail Snapper"
print_isl <- "Puerto Rico"

# Read in formatted data ####
tip_spp <- readRDS(here::here("data", tip_spp_rds))

# Create count of observed records for each area  ####
tip_spp_count <- tip_spp |>
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
## If working with PR ####
county_data <- tip_spp_count |>
  group_by(year, county_landedn) |>
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
  geom_point() +
  labs(
    x = "Year", y = "", colour = "", shape = "",
    title = paste(print_isl, "Length Samples")
  ) +
  theme_bw() +
  theme(
    legend.position = "null", text = element_text(size = 12),
    title = element_text(size = 15)
  )


# Plot gears used over time ####
gear_data <- tip_spp_count |>
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
    title = paste(print_isl, "Length Samples")
  ) +
  theme_bw() +
  theme(
    legend.position = "null", text = element_text(size = 15),
    title = element_text(size = 15)
  )

# Plot weight values recorded over time ####
weight_time <- tip_spp_count |>
  ggplot(aes(x = date, 
             y = obs_weight_lbs, 
             color = sample_condition)) +
  facet_wrap(~islandn, ncol = 2) + # include if pr
  geom_point() +
  labs(
    x = "Year", y = "Weight (lbs)",
    title = "Area-time distribution of Weights sampled",
    color = "COUNTY_SAMPLED (# obs)",
    subtitle = paste("N = ", nrow(tip_spp_count))
  )

# Plot length values recorded over time ####
length_time <- tip_spp_count |>
  ggplot(aes(
    x = date,
    y = length1_cm,
    group = island,
    color = length_type1
  )) +
  facet_wrap(~islandn, ncol = 2) +
  geom_point() +
  labs(
    x = "Year", y = "Length (cm)",
    title = "Area-time distribution of Lengths sampled",
    color = "COUNTY_SAMPLED (# obs)",
    subtitle = paste("N = ", nrow(tip_spp_count))
  )

# do we need to print these graphs? 
