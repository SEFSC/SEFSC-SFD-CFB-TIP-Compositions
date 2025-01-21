# 02d_spp_size_k
# k stats and flag of all islands

# Load libraries ####
    librarian::shelf(
      here, tidyverse, janitor, flextable, ggplot2
    )

# Specify settings #### 
# rds from end of 02aa script
    date <- "20241113" 
# find on itis.gov
    spp_itis <- c("097648", "097646") 
    spp <- "csl"
    isl <- c("pr", "stt", "stx")
    print_spp <- "Caribbean Spiny Lobster"
    print_isl <- "Puerto Rico - USVI"
# length type should be the one used for remainder of analysis
    len_type <- "CARAPACE LENGTH" 
    save_spp <- "csl"
    save_isl <- "prusvi"
    sedar <- "sedar91"

# Read in formatted data ####
    tip_spp_rds <- paste0("prusvi_csl_spp_size_quantity_", date, ".rds" )
    tip_spp <- readRDS(here::here("data", sedar, "rds", spp, "all", tip_spp_rds))

# Filter to length type and complete records
    spp_k_prep <- tip_spp |>
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
        sampling_program = "all",
        years = "all",
        island = "all",
        k_lower = k_q25 - 1.5 * k_iqr,
        k_upper = k_q75 + 1.5 * k_iqr
      )

# Obtain lower and upper estimates of k across all TIP data
    k_tip <- spp_k_prep |>
      dplyr::filter(sampling_program == "TIP") |>
      dplyr::summarize(
        .groups = "drop",
        n = n(),
        k_iqr = IQR(k, na.rm = TRUE),
        k_q25 = quantile(k, 0.25, na.rm = TRUE),
        k_q75 = quantile(k, 0.75, na.rm = TRUE),
      ) |>
      dplyr::mutate(
        sampling_program = "TIP",
        years = "all",
        island = "all",
        k_lower = k_q25 - 1.5 * k_iqr,
        k_upper = k_q75 + 1.5 * k_iqr
      )

# Obtain lower and upper estimates of k by source and island
    k_summary <- spp_k_prep |>
      dplyr::group_by(sampling_program, island) |>
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

# Plot wl by fishery and data source
    plot_wl <- spp_k_prep |>
      mutate(fishery_sampling_program = paste(fishery, sampling_program)) |>
      ggplot(
        aes(
          x = length1_inch,
          y = obs_weight_lbs,
          color = fishery_sampling_program
        )
      ) +
      geom_point()
  
# view plot  
    plot_wl
  
# save   
    ggsave(filename = 
             here::here("data", sedar, "figure", spp, "all", "plot_wl.png"),
           width = 14, height = 8)

# Plot wl by fishery, data source, and island
    plot_wl_island_fishery <- spp_k_prep |>
      ggplot(aes(x = length1_inch, y = obs_weight_lbs, color = sampling_program)) +
      geom_point() +
      facet_grid(fishery ~ island)
  
# view plot  
    plot_wl_island_fishery
  
# save   
    ggsave(filename = 
             here::here("data", sedar, "figure", spp, "all", "plot_wl_island_fishery.png"),
           width = 14, height = 8)
  
  
# Plot wl by data source, and island
    plot_wl_island <- spp_k_prep |>
      ggplot(aes(x = length1_inch, y = obs_weight_lbs, color = sampling_program)) +
      geom_point() +
      facet_grid( ~ island)
    
# view plot  
    plot_wl_island
  
# save   
    ggsave(filename = 
             here::here("data", sedar, "figure", spp, "all", "plot_wl_island.png"),
           width = 14, height = 8)

# Plot wl by sex and island
    plot_wl_island_sex <- spp_k_prep |>
      filter(sex_name %in% c("FEMALE", "MALE")) |> 
      ggplot(aes(x = length1_inch, y = obs_weight_lbs, color = sex_name)) +
      geom_point(show.legend = FALSE) +
      facet_grid(sex_name ~ island)
  
# view plot  
    plot_wl_island_sex
  
# save   
    ggsave(filename = 
             here::here("data", sedar, "figure", spp, "all", "plot_wl_island_sex.png"),
           width = 14, height = 8)
  
# Flagging process ####

# SPECIFY SETTINGS #
    k_range_source  <- "TIP"
    k_isl <- "pr"
  
# Obtain k limits from specified source
    k_range_lower <- k_summary |>
      filter(sampling_program == k_range_source,
             island == k_isl) |> 
      pull(k_lower)
    
    k_range_upper <- k_summary |>
      filter(sampling_program == k_range_source,
             island == k_isl) |> 
      pull(k_upper)

# Flag records outside the range
    spp_size_flag <- spp_k_prep |>
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
      dplyr::filter(sampling_program == "TIP") |>
      dplyr::group_by(island, year, sampling_program, fishery, length_type1, k_flag) |>
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
    
# view plot  
    plot_count_lw_flag

# Save prepped spp_size_flag ####
    saveRDS(
      spp_size_flag,
      file = here::here(
        "data",
        sedar,
        "rds",
        spp, 
        "all",
        paste0(
          save_isl, "_",
          save_spp, "_spp_size_flag_",
          format(Sys.time(), "%Y%m%d"), ".rds"
        )
      )
    )
    
  
# Save only complete wl pairs spp_k_prep ####
    saveRDS(
      spp_k_prep,
      file = here::here(
        "data",
        sedar,
        "rds",
        spp, 
        "all",
        paste0(
          save_isl, "_",
          save_spp, "_spp_size_wl_complete_",
          format(Sys.time(), "%Y%m%d"), ".rds"
        )
      )
    )
  
# write csv
    write.csv(spp_k_prep,
              file = here::here(
                "data",
                sedar,
                "rds",
                spp, 
                "all",
                paste0(
                  spp, "_spp_size_wl_complete_",
                  format(Sys.time(), "%Y%m%d"),
                  ".csv"
                )
              ),
              row.names = FALSE
    )
  