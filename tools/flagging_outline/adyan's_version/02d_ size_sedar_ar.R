# adyan's 

spp_isl_size <- spp_size_flag |>
  dplyr::mutate(
    spp_isl = dplyr::case_when(
      data_source == "TIP" &
        island == isl &
        length_type1 == len_type &
        fishing_mode == tip_mode ~ TRUE,
      .default = FALSE
    )
  )

# Plot length weight pairs by k flag
plot_spp_isl_flag <- spp_isl_size |>
  dplyr::filter(spp_isl == TRUE,
                k_flag != "missing") |>
  ggplot2::ggplot(aes(x = length1_inch, y = obs_weight_lbs, color = k_flag)) +
  ggplot2::geom_point() 

# Plot length weight pairs by k flag and year
plot_spp_isl_year_flag <- spp_isl_size |>
  dplyr::filter(spp_isl == TRUE,
                k_flag != "missing") |>
  ggplot2::ggplot(aes(x = length1_inch, y = obs_weight_lbs, color = k_flag)) +
  ggplot2::geom_point() +
  ggplot2::facet_wrap(~year)

plot_spp_isl_year_flag_80 <- spp_isl_size |>
  dplyr::filter(spp_isl == TRUE,
                k_flag != "missing",
                year < 1990) |>
  ggplot2::ggplot(aes(x = length1_inch, y = obs_weight_lbs, color = k_flag)) +
  ggplot2::geom_point() +
  ggplot2::facet_wrap(~year)

plot_spp_isl_year_flag_90 <- spp_isl_size |>
  dplyr::filter(spp_isl == TRUE,
                k_flag != "missing",
                year >= 1990 & year < 2000) |>
  ggplot2::ggplot(aes(x = length1_inch, y = obs_weight_lbs, color = k_flag)) +
  ggplot2::geom_point() +
  ggplot2::facet_wrap(~year)

plot_spp_isl_year_flag_00 <- spp_isl_size |>
  dplyr::filter(spp_isl == TRUE,
                k_flag != "missing",
                year >= 2000 & year < 2010) |>
  ggplot2::ggplot(aes(x = length1_inch, y = obs_weight_lbs, color = k_flag)) +
  ggplot2::geom_point() +
  ggplot2::facet_wrap(~year)

plot_spp_isl_year_flag_10 <- spp_isl_size |>
  dplyr::filter(spp_isl == TRUE,
                k_flag != "missing",
                year >= 2010 & year < 2020) |>
  ggplot2::ggplot(aes(x = length1_inch, y = obs_weight_lbs, color = k_flag)) +
  ggplot2::geom_point() +
  ggplot2::facet_wrap(~year)

plot_spp_isl_year_flag_20 <- spp_isl_size |>
  dplyr::filter(spp_isl == TRUE,
                k_flag != "missing",
                year >= 2020) |>
  ggplot2::ggplot(aes(x = length1_inch, y = obs_weight_lbs, color = k_flag)) +
  ggplot2::geom_point() +
  ggplot2::facet_wrap(~year)

# Tabulate number records and interviews by k_flag
count_keep <- spp_isl_size |>
  dplyr::filter(spp_isl == TRUE) |>
  dplyr::group_by(k_flag) |>
  dplyr::summarize(
    .groups = "drop",
    interviews = n_distinct(id),
    records = n()
  )

count_keep2 <- spp_isl_size |>
  dplyr::filter(spp_isl == TRUE,
                year >= 2012,
                year <= 2022) |>
  dplyr::group_by(k_flag) |>
  dplyr::summarize(
    .groups = "drop",
    interviews = n_distinct(id),
    records = n()
  )

# Tabulate number records and interviews by k_flag and year
count_spp_isl_overview <- spp_isl_size |>
  dplyr::filter(spp_isl == TRUE) |>
  dplyr::group_by(k_flag, year) |>
  dplyr::summarize(
    .groups = "drop",
    interviews = n_distinct(id),
    records = n()
  ) |>
  tidyr::pivot_longer(
    cols = !c(k_flag, year),
    names_to = c("category"),
    values_to = "count"
  )

plot_count_spp_isl_overview <- count_spp_isl_overview |>
  ggplot2::ggplot(aes(x = year, y = count, fill = k_flag)) +
  ggplot2::geom_col(position = position_dodge(preserve = "single")) +
  ggplot2::facet_wrap( ~ category, scales = "free_y") +
  ggplot2::theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )