# 02d_spp_size_k
# k stats and flag of all islands

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
# length type should be the one used for remainder of analysis
len_type <- "CARAPACE LENGTH" 
save_spp <- "csl"
save_isl <- "prusvi"

# Read in formatted data ####
tip_spp <- readRDS(here::here("data", tip_spp_rds))

# Filter to length type and complete records
spp_k_prep <- tip_spp |>
  dplyr::filter(
    length_type1 == len_type,
    record_type == "complete"
  )

# Obtain lower and upper estimates of k across all data
k_overall <- spp_k_prep |>
  dplyr::summarize(
    .groups = "drop",
    n = n(),
    k_iqr = IQR(k, na.rm = TRUE),
    k_q25 = quantile(k, 0.25, na.rm = TRUE),
    k_q75 = quantile(k, 0.75, na.rm = TRUE),
  ) |>
  dplyr::mutate(
    data_source = "all",
    years = "all",
    island = "all",
    k_lower = k_q25 - 1.5 * k_iqr,
    k_upper = k_q75 + 1.5 * k_iqr
  )

# Obtain lower and upper estimates of k across all TIP data
k_tip <- spp_k_prep |>
  dplyr::filter(data_source == "TIP") |>
  dplyr::summarize(
    .groups = "drop",
    n = n(),
    k_iqr = IQR(k, na.rm = TRUE),
    k_q25 = quantile(k, 0.25, na.rm = TRUE),
    k_q75 = quantile(k, 0.75, na.rm = TRUE),
  ) |>
  dplyr::mutate(
    data_source = "TIP",
    years = "all",
    island = "all",
    k_lower = k_q25 - 1.5 * k_iqr,
    k_upper = k_q75 + 1.5 * k_iqr
  )

# Obtain lower and upper estimates of k by source and island
k_summary <- spp_k_prep |>
  dplyr::group_by(data_source, island) |>
  summarize(
    .groups = "drop",
    years = "all",
    n = n(),
    k_iqr = IQR(k, na.rm = TRUE),
    k_q25 = quantile(k, 0.25, na.rm = TRUE),
    k_q75 = quantile(k, 0.75, na.rm = TRUE),
  ) |>
  mutate(
    k_lower = round(k_q25 - 1.5 * k_iqr, 2),
    k_upper = round(k_q75 + 1.5 * k_iqr, 2)
  ) |>
  dplyr::bind_rows(k_tip, k_overall)

# Plot k by sector and data source
plot_k <- spp_k_prep |>
  mutate(sector_data_source = paste(sector, data_source)) |>
  ggplot(
    aes(
      x = length1_inch,
      y = obs_weight_lbs,
      color = sector_data_source
    )
  ) +
  geom_point()
plot_k

# Plot k by sector, data source, and island
plot_k_island <- spp_k_prep |>
  ggplot(aes(x = length1_inch, y = obs_weight_lbs, color = data_source)) +
  geom_point() +
  facet_grid(sector ~ island)
plot_k_island

# Flagging process ####

# SPECIFY SETTINGS #
k_range_source  <- "TIP"
k_isl <- "pr"
# Obtain k limits from specified source
k_range_lower <- k_summary |>
  filter(data_source == k_range_source,
         island == k_isl) |> 
  pull(k_lower)

k_range_upper <- k_summary |>
  filter(data_source == k_range_source,
         island == k_isl) |> 
  pull(k_upper)

# Flag records outside the range
spp_size_flag <- spp_k_prep |>
  dplyr::mutate(
    k_flag = case_when(
      k >= k_range_lower & k <= k_range_upper ~ "keep",
      k < k_range_lower ~ "drop",
      k > k_range_upper ~ "drop",
      .default = "missing"
    )
  )

# Tabulate flagged length and weight pairs
count_lw_flag <- spp_size_flag |>
  dplyr::filter(data_source == "TIP") |>
  dplyr::group_by(island, year, data_source, sector, length_type1, k_flag) |>
  dplyr::summarize(
    .groups = "drop",
    records = n()
  )

# Plot count of flagged length and weight pairs
plot_count_lw_flag <- count_lw_flag |>
  ggplot2::ggplot(aes(x = year, y = records, fill = length_type1)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::facet_grid(k_flag ~ island) +
  ggplot2::theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )
plot_count_lw_flag


# Save prepped spp_size_flag ####
saveRDS(
  spp_size_flag,
  file = here::here(
    "data",
    paste0(
      save_isl, "_",
      save_spp, "_spp_size_flag_",
      format(Sys.time(), "%Y%m%d"), ".rds"
    )
  )
)
