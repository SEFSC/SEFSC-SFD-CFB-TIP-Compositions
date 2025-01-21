# 02c_spp_size_island_view
# plot counties, gears, length/weight pairs specific to island

# Load libraries ####
    librarian::shelf(
      here, tidyverse, janitor, flextable, ggplot2
    )

# Specify settings #### 
# rds from end of 02aa script
    date <- "20241118" 
    
# find on itis.gov
    spp_itis <- c("097648", "097646") 
    spp <- "csl"
    print_spp <- "Caribbean Spiny Lobster"
  
# chose island platform to focus on   
    # isl <- "stt" 
    # print_isl <- "St. Thomas/St. John"
    # isl <- "stx" 
    # print_isl <- "St. Croix"
    isl <- "pr"
    print_isl <- "Puerto Rico"
    
# folder name  
    sedar <- "sedar91"
  
# create folder structure for sedar overall data
    if (!dir.exists(here("data", sedar, "figure", spp, isl))){ dir.create(here("data", sedar, "figure", spp, isl)) }
    if (!dir.exists(here("data", sedar, "rds", spp, isl))){ dir.create(here("data", sedar, "rds", spp, isl)) }
  
# Read in formatted data ####
    tip_spp_rds <- paste0("prusvi_csl_spp_size_quantity_", date, ".rds" )
    tip_spp <- readRDS(here::here("data", sedar, "rds", spp, "all", tip_spp_rds))

# Filter to target island ####
    tip_spp_prep <- tip_spp |>
      dplyr::filter(island %in% isl)

# Create count of observed records for each area  ####
    tip_spp_count <- tip_spp_prep |>
      add_count(county_landed) |>
      dplyr::mutate(county_landedn = paste0(county_landed, " (", n, ")")) |>
      select(-n) |>
      add_count(gear) |>
      dplyr::mutate(gearn = paste0(gear, " (", n, ")")) |>
      select(-n)|>
      add_count(island) |>
      dplyr::mutate(islandn = paste0(island, " (", n, ")")) |>
      select(-n)

# FOR PUERTO RICO plot counties sampled over time ####
  # skip if working with USVI
    county_data <- tip_spp_count |>
      group_by(year, island, county_landedn) |>
      dplyr::summarize(n = n(), .groups = "drop") |>
      mutate(year = as.integer(year))
    
    county_by_year <- county_data |>
      group_by(county_landedn) |>
      dplyr::mutate(total_n = sum(n)) |>
      ungroup() |>
      dplyr::mutate(county_landedn = fct_reorder(county_landedn, total_n)) %>%
      ggplot(aes(x = year,
                 y = county_landedn, 
                 color = county_landedn, 
                 size = n)) +
      facet_wrap(~island) +
      geom_point() +
      labs(
        x = "Year", y = "", colour = "", shape = "",
        title = paste(print_isl, "Length Samples")
      ) +
      theme_bw() +
      theme(
        legend.position = "null", text = element_text(size = 10),
        title = element_text(size = 12)
      )
    
# view plot  
    county_by_year
    
# save   
    ggsave(filename = 
             here::here("data", sedar, "figure", spp, isl, "county_by_year.png"),
           width = 14, height = 8)
    

# Plot gears used over time ####
    gear_data <- tip_spp_count |>
      group_by(year, island, species_code, gearn) |>
      dplyr::summarize(n = n(), .groups = "drop") |>
      mutate(year = as.integer(year))
    
    gear_by_yr <- gear_data |>
      group_by(gearn) |>
      dplyr::mutate(total_n = sum(n)) |>
      ungroup() |>
      dplyr::mutate(gearn = fct_reorder(gearn, total_n)) |> 
      ggplot(aes(x = year, y = gearn, color = gearn, size = n)) +
      # facet_grid(species_code ~ island) +
      geom_point() +
      labs(
        x = "Year", y = "", colour = "", shape = "",
        title = paste(print_isl, "Length Samples")
      ) +
      theme_bw() +
      theme(
        legend.position = "null", text = element_text(size = 10),
        title = element_text(size = 12)
      )
    
# view plot  
    gear_by_yr
    
# save   
    ggsave(filename = 
             here::here("data", sedar, "figure", spp, isl, "gear_by_yr.png"),
           width = 14, height = 8)

# Plot weight values recorded over time ####
    weight_time <- tip_spp_count |>
      ggplot(aes(x = date, 
                 y = obs_weight_lbs, 
                 color = condition_type)) +
      # facet_wrap(~islandn, ncol = 2) + 
      # facet_grid(species_code ~ island) +
      geom_point() +
      labs(
        x = "Year", y = "Weight (lbs)",
        title = paste(print_isl, "Area-time distribution of Weights sampled"),
        color = "Sample Condition (# obs)",
        subtitle = paste("N = ", nrow(tip_spp_count))
      )
    
# view plot  
    weight_time
    
# save  
    ggsave(filename = 
             here::here("data", sedar, "figure", spp, isl, "weight_time.png"),
           width = 14, height = 8)

# Plot length values recorded over time ####
    length_time <- tip_spp_count |>
      ggplot(aes(
        x = date,
        y = length1_cm,
        group = island,
        color = length_type1
      )) +
      # facet_wrap(~islandn, ncol = 2) +
      # facet_grid(species_code ~ island) +
      geom_point() +
      labs(
        x = "Year", y = "Length (cm)",
        title = paste(print_isl,"Area-time distribution of Lengths sampled"),
        color = "Length Type (# obs)",
        subtitle = paste("N = ", nrow(tip_spp_count))
      )
    
# view plot  
    length_time
    
# save
    ggsave(filename = 
             here::here("data", sedar, "figure", spp, isl, "length_time.png"),
           width = 14, height = 8)

# Save formatted tip_spp ####
    saveRDS(
      tip_spp_count,
      file = here::here(
        "data",
        sedar,
        "rds",
        spp, 
        isl,
        paste0(
          isl, "_",
          spp, "_spp_size_island_view_",
          format(Sys.time(), "%Y%m%d"),
          ".rds"
        )
      )
    )
