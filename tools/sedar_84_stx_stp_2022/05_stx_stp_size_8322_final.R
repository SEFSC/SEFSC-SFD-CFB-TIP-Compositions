# Load libraries ####
librarian::shelf(here, tidyverse)

# Specify settings ####
tip_spp_use_rds <- "stx_slp_use_tip_20240123.rds"
spp <- "slp"
isl <- "stx"

# Read in filtered data ####
tip_spp_len <- readRDS(here::here("data", tip_spp_use_rds))

bin_data <- function(data, bin_size, ...) {
  id_count <- data |>
    filter(...) |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      .groups = "drop",
      n_id = dplyr::n_distinct(id)
    )

  bin_data <- data |>
    filter(...) |>
    dplyr::mutate(
      lbin_use = floor(length1_cm / bin_size) * bin_size
    ) |>
    dplyr::group_by(
      year,
      lbin_use
    ) |>
    dplyr::summarize(
      .groups = "drop",
      lbin_n = n()
    )

  saveRDS(
    bin_data,
    file = here::here(
      "data",
      paste0(spp, "_bin_tip_", format(Sys.time(), "%Y%m%d"), ".rds")
    )
  )

  spread_data <- bin_data |>
    dplyr::group_by(year) |>
    dplyr::mutate(n = sum(lbin_n)) |>
    tidyr::pivot_wider(
      names_from = lbin_use,
      values_from = lbin_n,
      values_fill = 0,
      names_sort = TRUE
    ) |>
    dplyr::left_join(id_count, by = join_by(year)) |>
    relocate(n_id, .after = year)
}

# Bin data ####
spp_length_comp_c <- tip_spp_len |>
  bin_data(bin_size = 2, year == 2017)

# Filter to binned data that are not confidential ####
spp_length_comp <- spp_length_comp_c |>
  dplyr::filter(n_id >= 3)

# Save data as csv file for SEDAR data provision ####
write.csv(
  spp_length_comp_c,
  file = here::here(
    "data",
    paste0(
      isl, "_", spp, "_lfd_tip_",
      substr(min(spp_length_comp_c$year), 3, 4),
      substr(max(spp_length_comp_c$year), 3, 4), "_",
      format(Sys.time(), "%Y%m%d"), ".csv"
    )
  )
)
