# Tabulate TIP interviews and records by island and year ####
count_all <- tip_r |>
  dplyr::group_by(island, year) |>
  dplyr::summarize(
    .groups = "drop",
    all_interviews = n_distinct(id),
    all_records = n()
  )

count_spp <- tip_r |>
  dplyr::filter(species_code == spp_code) |>
  dplyr::group_by(island, year) |>
  dplyr::summarize(
    .groups = "drop",
    spp_interviews = n_distinct(id),
    spp_records = n()
  )

count_overview <- count_all |>
  dplyr::left_join(count_spp, by = join_by(island, year)) |>
  dplyr::mutate(
    across(everything(), ~ replace_na(.x, 0))
  ) |>
  tidyr::pivot_longer(
    cols = !c(island, year),
    names_to = c("subset", "category"),
    names_sep = "_",
    values_to = "count"
  )

percent_overview <- count_overview |>
  tidyr::pivot_wider(
    names_from = subset,
    values_from = count
  ) |>
  dplyr::mutate(
    percent = round(100 * spp / all, 2)
  )

count_spp_sector <- tip_r |>
  dplyr::filter(
    species_code == spp_code,
    length_type1 != "NO LENGTH",
    island != "not coded"
  ) |>
  dplyr::group_by(island, year, sector, length_type1) |>
  dplyr::summarize(
    .groups = "drop",
    records = n()
  )

# Plot interviews and records by island and year ####
plot_count_overview <- count_overview |>
  ggplot2::ggplot(aes(x = year, y = count, fill = subset)) +
  ggplot2::geom_col(position = position_dodge(preserve = "single")) +
  ggplot2::facet_grid(category ~ island, scales = "free_y") +
  ggplot2::theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )

plot_percent_overview <- percent_overview |>
  ggplot2::ggplot(aes(x = year, y = percent)) +
  ggplot2::geom_col() +
  ggplot2::facet_grid(category ~ island)

plot_count_spp_sector <- count_spp_sector |>
  ggplot2::ggplot(aes(x = year, y = records, fill = length_type1)) +
  ggplot2::geom_col(position = position_dodge(preserve = "single")) +
  ggplot2::facet_grid(sector ~ island) +
  ggplot2::theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )
