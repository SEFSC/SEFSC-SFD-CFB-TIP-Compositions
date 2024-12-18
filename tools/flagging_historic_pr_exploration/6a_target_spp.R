# 6a_species_region_gear graphs
# run 1a-3b

# Load libraries ####
    librarian::shelf(here, tidyverse, measurements, flextable, ggplot2, reshape2)

# Specify settings ####
    tip_pr <- "pr_target_spp_20241216.rds" # add formatted data

# Read in raw data ####
    tip <- readRDS(here::here("data", tip_pr))
    
    unique(tip$species_name)

# choose species to graph
    print_spp <- "Lane Snapper"
    
    target_spp <- tip |> 
      filter(species_name == "LANE SNAPPER (ARRAYADO)")


# Calculate modes ####
## create mode function ####
    lenmode <- function(x) {
      ux <- unique(x)
      ux[which.max(tabulate(match(x, ux)))]
    }

## find mode across all gears, time, region ####
    mode_spp <- lenmode(target_spp$length1_cm)

# summarize stats of target species ####
    target_summary <- target_spp |> 
      dplyr::group_by(st_yr, region, gear_group) |> 
      dplyr::summarise(n_fish = n(),
                       n_id = n_distinct(id),
                       first_year = min(year),
                       last_year = max(year),
                       n_years = dplyr::n_distinct(year),
                       mode = lenmode(length1_cm)) |> 
      dplyr::ungroup()

## plot mode by gears across time and regions ####  
    plot_mode_target_spp <- target_summary |>
      ggplot(aes(x = st_yr, 
                 y = mode, 
                 group = gear_group,
                 )) +
      geom_line(aes(color = gear_group)) +
      geom_point(aes(color = gear_group)) +
      ggplot2::facet_wrap( ~ region) +
      labs(
        color = "Gear Type",
        x = "Start Year",
        y = "Mode Length (cm)",
        title = paste0(print_spp," Mode of Each Gear Group Across Time and Regions")
      ) +
      theme(
        # legend.title = element_text(size = 14),
        legend.text = element_text(size = 8),
        legend.position = "bottom",
        legend.title = element_blank()
      ) +
      geom_hline(yintercept = mode_spp,  
                 color = "darkgrey", linewidth=1)

# view
    plot_mode_target_spp

    
## plot count by gears across time and regions ####  
    plot_count_target_spp <- target_summary |>
      ggplot(aes(x = st_yr, 
                 y = n_fish, 
                 group = gear_group,
      )) +
      geom_line(aes(color = gear_group)) +
      geom_point(aes(color = gear_group)) +
      ggplot2::facet_wrap( ~ region) +
      labs(
        color = "Gear Type",
        x = "Start Year",
        y = "Sample Count",
        title = paste0(print_spp," Sample Count of Each Gear Group Across Time and Regions")
      ) +
      theme(
        # legend.title = element_text(size = 14),
        legend.text = element_text(size = 8),
        legend.position = "bottom",
        legend.title = element_blank()
      ) +
      geom_hline(yintercept = mode_spp,  
                 color = "darkgrey", linewidth=1)
    
# view
    plot_count_target_spp   

  
# do we want percent representation?
    # count of interviews? 