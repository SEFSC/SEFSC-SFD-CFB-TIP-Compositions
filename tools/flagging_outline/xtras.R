# extra tables

length_types <- spp_size_calc |>
  dplyr::group_by(
    island,
    species_code,
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
    na_obs_weight_kg = sum(is.na(obs_weight_kg)),
    percent_na_obs_weight_kg = round(na_obs_weight_kg / n * 100, 1),
  )

flextable(length_types)|> 
  theme_box() %>%
  align(align = "center", part = "all") %>%
  fontsize(size=8, part="all") %>%
  autofit() 