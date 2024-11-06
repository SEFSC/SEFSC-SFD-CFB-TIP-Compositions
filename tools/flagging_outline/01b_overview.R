# This script is for investigating the representation of a specific 
# species/island to the tip dataset as a whole

# run the 01a script with a data pull of the entire dataset first

# Load libraries ####
  librarian::shelf(here, tidyverse, measurements, expss, openxlsx, flextable)

# Specify settings ####
# rds from end of 01a script
  date <- "20241104" 
# find on itis.gov - all spp itis codes that could be assiciated with target species
  spp_itis <- c("097648", "097646", "097651") 
  spp <- "csl"
  print_spp <- "Caribbean Spiny Lobster"
  print_isl <- "Puerto Rico - USVI"
  sedar <- "sedar91"
  
# Read in formatted data ####
  tip_spp_rds <- paste0("format_tip_", date, ".rds" )
  tip_spp <- readRDS(here::here("data", sedar, "rds", tip_spp_rds))

# Tabulate TIP interviews and records by island and year ####
# count all records and interviews
  count_all <- tip_spp |>
    dplyr::group_by(island, year) |>
    dplyr::summarize(
      .groups = "drop",
      all_interviews = n_distinct(sampling_unit_id),
      all_records = n()
    )
  
# count only specific species
  count_spp <- tip_spp |>
    dplyr::filter(species_code %in% spp_itis) |>
    dplyr::group_by(island, year) |>
    dplyr::summarize(
      .groups = "drop",
      spp_interviews = n_distinct(sampling_unit_id),
      spp_records = n()
    ) 

# summarize both counts
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
  
# create formatted table 
  flextable(count_overview) |>
    theme_box() %>%
    align(align = "center", part = "all") %>%
    fontsize(size = 8, part = "all") %>%
    autofit() |>
    colformat_num(j = "year", big.mark = "")

# calculate percent spp each year
  percent_overview <- count_overview |>
    tidyr::pivot_wider(
      names_from = subset,
      values_from = count
    ) |>
    dplyr::mutate(
      percent = round(100 * spp / all, 2)
    )

# count spp based on fishery
  count_spp_fishery <- tip_spp |>
    dplyr::filter(
      species_code %in% spp_itis,
      length_type1 != "NO LENGTH",
      island != "not coded"
    ) |>
    dplyr::group_by(island, year, fishery, length_type1) |>
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
      legend.title = element_blank())+ 
    labs(x = "Year", 
         y = "Count",
         title = paste0(print_spp, 
                        " vs. All Species Combined in TIP"))
# view
  plot_count_overview
# save  
  ggsave(filename = here::here("data", sedar, "figure", "all", "plot_count_overview.png"))

# plot percent representation     
  plot_percent_overview <- percent_overview |>
    ggplot2::ggplot(aes(x = year, y = percent)) +
    ggplot2::geom_col() +
    ggplot2::facet_grid(category ~ island)+
    labs(x = "Year", 
         y = "Percent",
         title = paste0(print_spp, 
                        " Percent Composition of Total TIP Interviews and Records"))
# view
  plot_percent_overview  
# save  
  ggsave(filename = here::here("data", sedar, "figure", "all", "plot_percent_overview.png"))

# plot coverage by fishery     
  plot_count_spp_fishery <- count_spp_fishery |>
    ggplot2::ggplot(aes(x = year, y = records, fill = length_type1)) +
    ggplot2::geom_col(position = position_dodge(preserve = "single")) +
    ggplot2::facet_grid(fishery ~ island) +
    labs(x = "Year", 
         y = "Records",
         title = paste0(print_spp, 
                        " Length Types"))+
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = element_blank()
    )
  
# view
  plot_count_spp_fishery
# save  
  ggsave(filename = here::here("data", sedar, "figure", "all", "plot_count_spp_fishery.png"))

