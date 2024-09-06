# 02b_spp_size_overview
# length/weight visualization of species across all islands

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

# Read in formatted data ####
tip_spp <- readRDS(here::here("data", tip_spp_rds))

# Tabulate complete and incomplete length and weight pairs
count_lw_pairs <- tip_spp |>
  dplyr::group_by(island, year, data_source, sector, length_type1, record_type) |>
  dplyr::summarize(
    .groups = "drop",
    records = n()
  )

# create formatted table of length/weight pair completion 
flextable(count_lw_pairs) |>
  theme_box() %>%
  align(align = "center", part = "all") %>%
  fontsize(size = 8, part = "all") %>%
  autofit() |>
  colformat_num(j = "year", big.mark = "")

# Plot count of complete and incomplete length and weight pairs
plot_count_lw_pairs <- count_lw_pairs |>
  ggplot2::ggplot(aes(x = year, y = records, fill = length_type1)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::facet_grid(record_type ~ island) +
  ggplot2::theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )

# Plot complete length and weight pairs by sector and len type
plot_lw_pairs <- tip_spp |>
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
plot_spp_length <- tip_spp |>
  dplyr::filter(
    data_source == "TIP",
    island != "not coded",
    length1_cm <= quantile(tip_spp$length1_cm, 0.99, na.rm = TRUE)
  ) |>
  ggplot2::ggplot(aes(x = length1_cm, color = length_type1)) +
  ggplot2::geom_step(stat = "ecdf") +
  ggplot2::facet_grid(~island) +
  ggplot2::theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )

# Plot cumulative length by island and year
plot_spp_length_year <- tip_spp |>
  filter(
    data_source == "TIP",
    island != "not coded",
    length1_cm <= quantile(tip_spp$length1_cm, 0.99, na.rm = TRUE)
  ) |>
  ggplot2::ggplot(aes(x = length1_cm, color = factor(year))) +
  ggplot2::geom_step(stat = "ecdf") +
  ggplot2::facet_grid(~island) +
  ggplot2::theme(legend.position = "right")

# Plot cumulative weight by island
plot_spp_weight <- tip_spp |>
  dplyr::filter(
    data_source == "TIP",
    island != "not coded",
    obs_weight_kg <= quantile(tip_spp$obs_weight_kg, 0.99, na.rm = TRUE)
  ) |>
  ggplot2::ggplot(aes(x = obs_weight_kg, color = island)) +
  ggplot2::geom_step(stat = "ecdf") +
  ggplot2::theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )

# Plot cumulative weight by island and year
plot_spp_weight_year <- tip_spp |>
  filter(
    data_source == "TIP",
    island != "not coded",
    obs_weight_kg <= quantile(tip_spp$obs_weight_kg, 0.99, na.rm = TRUE),
  ) |>
  ggplot2::ggplot(aes(x = obs_weight_kg, color = factor(year))) +
  ggplot2::geom_step(stat = "ecdf") +
  ggplot2::facet_grid(~island) +
  ggplot2::theme(legend.position = "right")
