# 6b_target_spp_region

# box and whisker of each region facet_wrap gear

# apply interview and gear filters 
    spp_filtered <- target_spp |> 
      group_by(st_yr, region, gear_group ) |>
      dplyr::mutate(n_ID = n_distinct(id)) |>
      dplyr::filter(n_ID >= 2) |>
      dplyr::ungroup() |>
      dplyr::group_by(st_yr, region, gear_group ) |>
      dplyr::filter(n() >= 20) |>
      dplyr::ungroup()
    
## find median across all gears, time, regions ####
    med_all <- median(target_spp$length1_cm, na.rm = TRUE)    
    
    footnote <- paste("Grey line is median legth caught of", print_spp, "across all gears, regions, and time.")
    
# Plot east ####
# run and then select ylim
    plot_spp_east <- spp_filtered |>
      dplyr::filter(region == "East") |> 
      ggplot(aes(x = st_yr, y = length1_cm)) +
      geom_boxplot(notch = FALSE,
                   outlier.shape = NA) +
      ggplot2::facet_wrap( ~ gear_group) +
      # coord_cartesian(ylim = NULL ) +
      coord_cartesian(ylim = c(10,40) ) +
      labs(
        color = "Gear Group",
        x = "Start Year",
        y = "Length (cm)", 
        title = paste(print_spp, "East Length Samples"),
        caption = footnote) +
      theme(
        # legend.title = element_text(size = 14),
        legend.text = element_text(size = 8),
        legend.position = "bottom",
        legend.title = element_blank()
      ) +
      geom_hline(yintercept = med_all,  
                 color = "darkgrey", linewidth=1)

# view 
    plot_spp_east
    
# save  
    ggsave(filename = here::here("data",
                                 "historic", 
                                 "figure", 
                                 paste0(save_spp, "_bp_east.png")) ,
           width = 14, height = 8)
    
# Plot north ####
    # run and then select ylim
    plot_spp_north <- spp_filtered |>
      dplyr::filter(region == "North") |> 
      ggplot(aes(x = st_yr, y = length1_cm)) +
      geom_boxplot(notch = FALSE,
                   outlier.shape = NA) +
      ggplot2::facet_wrap( ~ gear_group) +
      # coord_cartesian(ylim = NULL ) +
      coord_cartesian(ylim = c(10, 40) ) +
      labs(
        color = "Gear Group",
        x = "Start Year",
        y = "Length (cm)", 
        title = paste(print_spp, "North Length Samples"),
        caption = footnote
      ) +
      theme(
        # legend.title = element_text(size = 14),
        legend.text = element_text(size = 8),
        legend.position = "bottom",
        legend.title = element_blank()
      ) +
      geom_hline(yintercept = med_all,  
                 color = "darkgrey", linewidth=1)
    
# view 
    plot_spp_north
    
# save  
    ggsave(filename = here::here("data",
                                 "historic", 
                                 "figure", 
                                 paste0( save_spp, "_bp_north.png")) ,
           width = 14, height = 8)
    
# Plot west ####
    # run and then select ylim
    plot_spp_west <- spp_filtered |>
      dplyr::filter(region == "West") |> 
      ggplot(aes(x = st_yr, y = length1_cm)) +
      geom_boxplot(notch = FALSE,
                   outlier.shape = NA) +
      ggplot2::facet_wrap( ~ gear_group) +
      # coord_cartesian(ylim = NULL ) +
      coord_cartesian(ylim = c(10,45) ) +
      labs(
        color = "Gear Group",
        x = "Start Year",
        y = "Length (cm)", 
        title = paste(print_spp, "West Length Samples")
      ) +
      theme(
        # legend.title = element_text(size = 14),
        legend.text = element_text(size = 8),
        legend.position = "bottom",
        legend.title = element_blank()
      ) +
      geom_hline(yintercept = med_all,  
                 color = "darkgrey", linewidth=1)
    
# view 
    plot_spp_west
    
# save  
    ggsave(filename = here::here("data",
                                 "historic", 
                                 "figure", 
                                 paste0(save_spp, "_bp_west.png")) ,
           width = 14, height = 8)
    
# Plot south ####
    # run and then select ylim
    plot_spp_south <- spp_filtered |>
      dplyr::filter(region == "South") |> 
      ggplot(aes(x = st_yr, y = length1_cm)) +
      geom_boxplot(notch = FALSE,
                   outlier.shape = NA) +
      ggplot2::facet_wrap( ~ gear_group) +
      # coord_cartesian(ylim = NULL ) +
      coord_cartesian(ylim = c(10,50) ) +
      labs(
        color = "Gear Group",
        x = "Start Year",
        y = "Length (cm)", 
        title = paste(print_spp, "South Length Samples")
      ) +
      theme(
        # legend.title = element_text(size = 14),
        legend.text = element_text(size = 8),
        legend.position = "bottom",
        legend.title = element_blank()
      ) +
      geom_hline(yintercept = med_all,  
                 color = "darkgrey", linewidth=1)
    
# view 
    plot_spp_south

# save  
    ggsave(filename = here::here("data",
                                 "historic", 
                                 "figure", 
                                 paste0(save_spp, "_bp_south.png")) ,
           width = 14, height = 8)
                  
                  