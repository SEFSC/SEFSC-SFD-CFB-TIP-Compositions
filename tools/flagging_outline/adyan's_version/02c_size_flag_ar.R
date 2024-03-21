# adyan's

# Obtain k limits from specified source
k_range_lower <- k_summary |>
  filter(data_source == k_range_source) |> 
  pull(k_lower)

k_range_upper <- k_summary |>
  filter(data_source == k_range_source) |> 
  pull(k_upper)

# Flag records outside the range
spp_size_flag <- spp_size_prep |>
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