# adyan's 

# Filter to length type and complete records
spp_k_prep <- spp_size_prep |>
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

# Obtain lower and upper estimates of k by source and location
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

# Plot k by sector, data source, and island
plot_k_island <- spp_k_prep |>
  ggplot(aes(x = length1_inch, y = obs_weight_lbs, color = data_source)) +
  geom_point() +
  facet_grid(sector ~ island)