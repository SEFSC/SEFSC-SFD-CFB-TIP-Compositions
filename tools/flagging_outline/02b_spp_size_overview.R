# 02b_spp_size_overview
# length/weight visualization of species across all islands

# Load libraries ####
  librarian::shelf(
    here, tidyverse, janitor, flextable, ggplot2
  )

# Specify settings #### 
# date from end of 02aa script
  date <- "20241104"
# find on itis.gov
  spp_itis <- c("097648", "097646") 
  spp <- "csl"
  isl <- c("pr", "stt", "stx")
  print_spp <- "Caribbean Spiny Lobster"
  print_isl <- "Puerto Rico - USVI"
  sedar <- "sedar91"

# Read in formatted data ####
  tip_spp_rds <- paste0("prusvi_csl_spp_size_quantity_", date, ".rds" )
  tip_spp <- readRDS(here::here("data", sedar, "rds", spp, "all", tip_spp_rds))

# Tabulate complete and incomplete length and weight pairs
  count_lw_pairs <- tip_spp |>
    dplyr::group_by(island, year, sampling_program, fishery, length_type1, record_type) |>
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
    ) +
    labs(
      x = "Year", y = "Records",
      title = paste(print_spp, print_isl),
      subtitle = paste("Total N = ", nrow(tip_spp))
    )
# view  
  plot_count_lw_pairs
# save  
  ggsave(filename = 
           here::here("data", sedar, "figure", spp, "all", "plot_count_lw_pairs.png"),
         width = 14, height = 8)
  
# Plot complete length and weight pairs by fishery and len type
  plot_lw_pairs <- tip_spp |>
    dplyr::mutate(fishery_sampling_program = paste(fishery, sampling_program)) |>
    dplyr::filter(record_type == "complete") |>
    ggplot2::ggplot(aes(
      x = length1_inch,
      y = obs_weight_lbs,
      color = fishery_sampling_program
    )) +
    ggplot2::geom_point() +
    ggplot2::facet_grid(~length_type1) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = element_blank()
    )+
    labs(
      x = "Length (cm)", y = "Weight (lbs)",
      title = paste(print_spp, print_isl),
      subtitle = paste("Total N = ", nrow(tip_spp))
    )
# view  
  plot_lw_pairs
# save
  ggsave(filename = 
           here::here("data", sedar, "figure", spp, "all", "plot_lw_pairs.png"),
         width = 14, height = 8)
  
# Plot cumulative length by island
  plot_spp_length_all <- tip_spp |>
    dplyr::filter(
      sampling_program == "TIP",
      island != "not coded",
      length1_cm <= quantile(tip_spp$length1_cm, 0.99, na.rm = TRUE)
    ) |>
    ggplot2::ggplot(aes(x = length1_cm, color = island)) +
    ggplot2::geom_step(stat = "ecdf") +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = element_blank()
    )+
    labs(
      x = "Length (cm)", y = "ecdf",
      title = paste(print_spp, print_isl),
      subtitle = paste("Total N = ", nrow(tip_spp))
    )
# view  
  plot_spp_length_all
# save
  ggsave(filename = 
           here::here("data", sedar, "figure", spp, "all", "plot_spp_length_all.png"),
         width = 14, height = 8)

# Plot cumulative length by island by type
  plot_spp_length_type <- tip_spp |>
    dplyr::filter(
      sampling_program == "TIP",
      island != "not coded",
      length1_cm <= quantile(tip_spp$length1_cm, 0.99, na.rm = TRUE)
    ) |>
    ggplot2::ggplot(aes(x = length1_cm, color = length_type1)) +
    ggplot2::geom_step(stat = "ecdf") +
    ggplot2::facet_grid(~island) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = element_blank()
    )+
    labs(
      x = "Length (cm)", y = "ecdf",
      title = paste(print_spp, print_isl),
      subtitle = paste("Total N = ", nrow(tip_spp))
    )
# view  
  plot_spp_length_type
# save
  ggsave(filename = 
           here::here("data", sedar, "figure", spp, "all", "plot_spp_length_type.png"),
         width = 14, height = 8)
  
# Plot cumulative length by island and year
  plot_spp_length_year <- tip_spp |>
    filter(
      sampling_program == "TIP",
      island != "not coded",
      length1_cm <= quantile(tip_spp$length1_cm, 0.99, na.rm = TRUE)
    ) |>
    ggplot2::ggplot(aes(x = length1_cm, color = factor(year))) +
    ggplot2::geom_step(stat = "ecdf") +
    ggplot2::facet_grid(~island) +
    ggplot2::theme(legend.position = "right")+
    labs(
      x = "Length (cm)", y = "ecdf",
      title = paste(print_spp, print_isl),
      subtitle = paste("Total N = ", nrow(tip_spp))
    )
# view  
  plot_spp_length_year
# save
  ggsave(filename = 
           here::here("data", sedar, "figure", spp, "all", "plot_spp_length_year.png"),
         width = 14, height = 8)
  
# Plot cumulative weight by island
  plot_spp_weight <- tip_spp |>
    dplyr::filter(
      sampling_program == "TIP",
      island != "not coded",
      obs_weight_kg <= quantile(tip_spp$obs_weight_kg, 0.99, na.rm = TRUE)
    ) |>
    ggplot2::ggplot(aes(x = obs_weight_kg, color = island)) +
    ggplot2::geom_step(stat = "ecdf") +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = element_blank()
    )+
    labs(
      x = "Weight (lbs)", y = "ecdf",
      title = paste(print_spp, print_isl),
      subtitle = paste("Total N = ", nrow(tip_spp))
    )
# view  
  plot_spp_weight
# save
  ggsave(filename = 
           here::here("data", sedar, "figure", spp, "all", "plot_spp_weight.png"),
         width = 14, height = 8)
  
# Plot cumulative weight by island and year
  plot_spp_weight_year <- tip_spp |>
    filter(
      sampling_program == "TIP",
      island != "not coded",
      obs_weight_kg <= quantile(tip_spp$obs_weight_kg, 0.99, na.rm = TRUE),
    ) |>
    ggplot2::ggplot(aes(x = obs_weight_kg, color = factor(year))) +
    ggplot2::geom_step(stat = "ecdf") +
    ggplot2::facet_grid(~island) +
    ggplot2::theme(legend.position = "right")+
    labs(
      x = "Weight (lbs)", y = "ecdf",
      title = paste(print_spp, print_isl),
      subtitle = paste("Total N = ", nrow(tip_spp))
    )
# view  
  plot_spp_weight_year
# save
  ggsave(filename = 
           here::here("data", sedar, "figure", spp, "all", "plot_spp_weight_year.png"),
         width = 14, height = 8)
  
