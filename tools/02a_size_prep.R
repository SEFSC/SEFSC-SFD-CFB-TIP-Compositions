# Convert units and calculate and k
spp_size_prep <- spp_size |>
  dplyr::mutate(
    length1_cm = measurements::conv_unit(length1_mm, "mm", "cm"),
    length1_inch = measurements::conv_unit(length1_mm, "mm", "inch"),
    obs_weight_lbs = measurements::conv_unit(obs_weight_kg, "kg", "lbs"),
    k = 10^5 * obs_weight_kg / length1_cm^3,
    record_type = case_when(
      !is.na(k) ~ "complete",
      .default = "incomplete"
    )
  )

# Tabulate complete and incomplete length and weight pairs
count_lw_pairs <- spp_size_prep |>
  dplyr::group_by(island, year, data_source, sector, length_type1, record_type) |>
  dplyr::summarize(
    .groups = "drop",
    records = n()
  )

# Plot count of complete and incomplete length and weight pairs
plot_count_lw_pairs <- count_lw_pairs |>
  ggplot2::ggplot(aes(x = year, y = records, fill = length_type1)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::facet_grid(record_type ~ island) +
  ggplot2::theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )

# Plot length and weight pairs
plot_lw_pairs <- spp_size_prep |>
  dplyr::mutate(sector_data_source = paste(sector, data_source)) |>
  dplyr::filter(record_type == "complete") |>
  ggplot2::ggplot(aes(
    x = length1_inch,
    y = obs_weight_lbs,
    color = sector_data_source
  )) +
  ggplot2::geom_point() +
  ggplot2::facet_grid(~length_type1) +
  ggplot2::theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )

# Plot cumulative length by island
plot_spp_length <- spp_size_prep |>
  dplyr::filter(
    data_source == "TIP",
    island != "not coded",
    length1_cm <= quantile(spp_size_prep$length1_cm, 0.99, na.rm = TRUE)
  ) |>
  ggplot2::ggplot(aes(x = length1_cm, color = length_type1)) +
  ggplot2::geom_step(stat = "ecdf") +
  ggplot2::facet_grid(~island) +
  ggplot2::theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )

# Plot cumulative length by island and year
plot_spp_length_year <- spp_size_prep |>
  filter(
    data_source == "TIP",
    island != "not coded",
    length1_cm <= quantile(spp_size_prep$length1_cm, 0.99, na.rm = TRUE)
  ) |>
  ggplot2::ggplot(aes(x = length1_cm, color = factor(year))) +
  ggplot2::geom_step(stat = "ecdf") +
  ggplot2::facet_grid(~island) +
  ggplot2::theme(legend.position = "none")

# Plot cumulative weight by island
plot_spp_weight <- spp_size_prep |>
  dplyr::filter(
    data_source == "TIP",
    island != "not coded",
    obs_weight_kg <= quantile(spp_size_prep$obs_weight_kg, 0.99, na.rm = TRUE)
  ) |>
  ggplot2::ggplot(aes(x = obs_weight_kg, color = island)) +
  ggplot2::geom_step(stat = "ecdf") +
  ggplot2::theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )

# Plot cumulative weight by island and year
plot_spp_weight_year <- spp_size_prep |>
  filter(
    data_source == "TIP",
    island != "not coded",
    obs_weight_kg <= quantile(spp_size_prep$obs_weight_kg, 0.99, na.rm = TRUE),
  ) |>
  ggplot2::ggplot(aes(x = obs_weight_kg, color = factor(year))) +
  ggplot2::geom_step(stat = "ecdf") +
  ggplot2::facet_grid(~island) +
  ggplot2::theme(legend.position = "none")
