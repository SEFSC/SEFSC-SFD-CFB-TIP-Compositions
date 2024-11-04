# 02bb_spp_size_min
# length/weight visualization of species across all islands

# Load libraries ####
  librarian::shelf(
    here, tidyverse, janitor, flextable, ggplot2
  )

# Specify settings #### 
# rds from end of 02aa script
  date <- "20241024" 
# find on itis.gov
  spp_itis <- c("097648", "097646") 
  spp <- "csl"
  isl <- c("pr", "stt", "stx")
  print_spp <- "Caribbean Spiny Lobster"
  print_isl <- "Puerto Rico - USVI"
# if there is a regulated min/max size, use that. 
# if not, use min size based on 02a:67-89/02b:figures
  min_size <- 3.5
  max_size <- 7.5
  sedar <- "sedar91"

# Read in formatted data ####
  tip_spp_rds <- paste0("prusvi_csl_spp_size_quantity_", date, ".rds" )
  tip_spp <- readRDS(here::here("data", sedar, "rds", spp, "all", tip_spp_rds))
  
# filter to lengths smaller than minimum ####
  spp_min <- tip_spp |> 
    filter(length1_inch < min_size)|>
    add_count(island) |>
    dplyr::mutate(islandn = paste0(island, " (", n, ")")) |>
    select(-n)|>
    add_count(gear) |>
    dplyr::mutate(gearn = paste0(gear, " (", n, ")")) |>
    select(-n)

# Plot length values smaller than min size over time ####
  min_length_time <- spp_min |> 
    ggplot(aes(
      x = date,
      y = length1_cm,
      group = islandn,
      color = length_type1
    )) +
    facet_wrap(~islandn, ncol = 2) +
    # facet_grid(species_code ~ islandn) +
    geom_point() +
    labs(
      x = "Year", y = "Length (cm)",
      title = paste("Area-time Distribution of Minimum", print_spp,"Lengths Sampled"),
      color = "Length Type",
      subtitle = paste("N = ", nrow(spp_min))
    ) +
    theme(
      legend.position = "bottom", text = element_text(size = 10),
      title = element_text(size = 12)
    )
# view plot  
  min_length_time
# save
  ggsave(filename = 
           here::here("data", sedar, "figure", spp, "all", "min_length_time.png"),
         width = 14, height = 8)

  
# Plot length values smaller than min size over by gear ####
  min_length_gear <- spp_min |>
    ggplot(aes(
      x = length1_cm,
      y = gearn,
      group = islandn,
      color = length_type1
    )) +
    facet_wrap(~islandn, ncol = 2) +
    # facet_grid(species_code ~ islandn) +
    geom_point() +
    labs(
      x = "Length (cm)", y = "Gear",
      title =  paste("Area-gear Distribution of Minimum", print_spp,"Lengths Sampled"),
      color = "Length Type",
      subtitle = paste("N = ", nrow(spp_min))
    ) +
    theme(
      legend.position = "bottom", text = element_text(size = 10),
      title = element_text(size = 12)
    )
# view plot  
  min_length_gear
# save  
  ggsave(filename = 
           here::here("data", sedar, "figure", spp, "all", "min_length_gear.png"),
         width = 14, height = 8)
  
# Plot length values smaller than max size over by year ####
  min_count <- spp_min |>
    group_by(year, islandn, gearn) |>
    dplyr::summarize(n = n(), .groups = "drop") |>
    mutate(year = as.integer(year))
  
  min_length_gear_date <- min_count |>
    group_by(gearn) |>
    dplyr::mutate(total_n = sum(n)) |>
    ungroup() |>
    dplyr::mutate(gearn = fct_reorder(gearn, total_n)) %>%
    ggplot(aes(x = year,
               y = gearn, 
               color = gearn, 
               size = n))  +
    facet_wrap(~islandn, ncol = 2) +
    # facet_grid(species_code ~ islandn) +
    geom_point() +
    labs(
      x = "Length (cm)", y = "Gear",
      title =  paste("Distribution of Minimum", print_spp,"Lengths Sampled Over Time"),
      color = "Length Type",
      subtitle = paste("N = ", nrow(spp_min))
    ) +
    theme(
      legend.position = "none", text = element_text(size = 10),
      title = element_text(size = 12)
    )
# view plot  
  min_length_gear_date
# save
  ggsave(filename = 
           here::here("data", sedar, "figure", spp, "all", "min_length_gear_date.png"),
         width = 14, height = 8)
  
  
# set minimum size
  min_size <- 2.5
  
# extra data falling below min_size limit to send to Sarah for investigation 
  tip_min <- tip_spp |> 
    filter(length1_cm < min_size)
  
# save dataframe 
  saveRDS(
    tip_min,
    file = here::here(
      "data",
      sedar,
      "rds",
      spp, 
      "all",
      paste0(
        spp, "_spp_size_min_",
        format(Sys.time(), "%Y%m%d"), ".rds"
      )
    )
  )
  
# write csv
  write.csv(tip_min,
            file = here::here(
              "data",
              sedar,
              "rds",
              spp, 
              "all",
              paste0(
                spp, "_spp_size_min_",
                format(Sys.time(), "%Y%m%d"),
                ".csv"
              )
            ),
            row.names = FALSE
  )
  
 
# filter to lengths larger than maximum ####
  spp_max <- tip_spp |> 
    filter(length1_inch >= max_size)|>
    add_count(island) |>
    dplyr::mutate(islandn = paste0(island, " (", n, ")")) |>
    select(-n)|>
    add_count(gear) |>
    dplyr::mutate(gearn = paste0(gear, " (", n, ")")) |>
    select(-n)
   
# Plot length values larger than max size over time ####
  max_length_time <- spp_max |> 
    ggplot(aes(
      x = date,
      y = length1_cm,
      group = islandn,
      color = length_type1
    )) +
    facet_wrap(~islandn, ncol = 2) +
    # facet_grid(species_code ~ islandn) +
    geom_point() +
    labs(
      x = "Year", y = "Length (cm)",
      title = paste("Area-time Distribution of Maximum", print_spp,"Lengths Sampled"),
      color = "Length Type",
      subtitle = paste("N = ", nrow(spp_max))
    ) +
    theme(
      legend.position = "bottom", text = element_text(size = 10),
      title = element_text(size = 12)
    )
# view plot  
  max_length_time
# save  
  ggsave(filename = 
           here::here("data", sedar, "figure", spp, "all", "max_length_time.png"),
         width = 14, height = 8)
  
  
# Plot length values larger than max size over by gear ####
  max_length_gear <- spp_max |>
    ggplot(aes(
      x = length1_cm,
      y = gearn,
      group = islandn,
      color = length_type1
    )) +
    facet_wrap(~islandn, ncol = 2) +
    # facet_grid(species_code ~ islandn) +
    geom_point() +
    labs(
      x = "Length (cm)", y = "Gear",
      title =  paste("Area-gear Distribution of Maximum", print_spp,"Lengths Sampled"),
      color = "Length Type",
      subtitle = paste("N = ", nrow(spp_max))
    ) +
    theme(
      legend.position = "bottom", text = element_text(size = 10),
      title = element_text(size = 12)
    )
# view plot  
  max_length_gear
# save
  ggsave(filename = 
           here::here("data", sedar, "figure", spp, "all", "max_length_gear.png"),
         width = 14, height = 8)
  
# Plot length values larger than max size over by year ####
  max_count <- spp_max |>
    group_by(year, islandn, gearn) |>
    dplyr::summarize(n = n(), .groups = "drop") |>
    mutate(year = as.integer(year))
  
  max_length_gear_date <- max_count |>
    group_by(gearn) |>
    dplyr::mutate(total_n = sum(n)) |>
    ungroup() |>
    dplyr::mutate(gearn = fct_reorder(gearn, total_n)) %>%
    ggplot(aes(x = year,
               y = gearn, 
               color = gearn, 
               size = n))  +
    facet_wrap(~islandn, ncol = 2) +
    # facet_grid(species_code ~ islandn) +
    geom_point() +
    labs(
      x = "Length (cm)", y = "Gear",
      title =  paste("Distribution of Maximum", print_spp,"Lengths Sampled Over Time"),
      color = "Length Type",
      subtitle = paste("N = ", nrow(spp_max))
    ) +
    theme(
      legend.position = "none", text = element_text(size = 10),
      title = element_text(size = 12)
    )
# view plot  
  max_length_gear_date
# save
  ggsave(filename = 
           here::here("data", sedar, "figure", spp, "all", "max_length_gear_date.png"),
         width = 14, height = 8)
  
# set minimum size
  max_size <- 25
  
# extra data falling below min_size limit to send to Sarah for investigation 
  tip_max <- tip_spp |> 
    filter(length1_cm > max_size)
  
# save dataframe 
  saveRDS(
    tip_max,
    file = here::here(
      "data",
      sedar,
      "rds",
      spp, 
      "all",
      paste0(
        spp, "_spp_size_max_",
        format(Sys.time(), "%Y%m%d"), ".rds"
      )
    )
  )
  
# write csv
  write.csv(tip_max,
            file = here::here(
              "data",
              sedar,
              "rds",
              spp, 
              "all",
              paste0(
                spp, "_spp_size_max_",
                format(Sys.time(), "%Y%m%d"),
                ".csv"
              )
            ),
            row.names = FALSE
  )
  