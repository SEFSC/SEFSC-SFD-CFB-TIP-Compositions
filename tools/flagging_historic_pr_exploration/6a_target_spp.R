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
    save_spp <- "lane_snapper"
    
    target_spp <- tip |> 
      filter(species_name == "LANE SNAPPER (ARRAYADO)")
    


# summarize stats of target species ####
    target_summary <- target_spp |> 
      dplyr::group_by(st_yr, region, gear_group) |> 
      dplyr::summarise(
        # n_fish = n(),
                       n_id = n_distinct(id),
                       # first_year = min(year),
                       # last_year = max(year),
                       # n_years = dplyr::n_distinct(year),
                       # mode = lenmode(length1_cm)
                       ) |> 
      dplyr::ungroup()
    
# pivot wider to cacl total per year
    target_count <- target_summary |> 
      tidyr::pivot_wider(
        names_from = gear_group,
        values_from = n_id
      ) |> 
      dplyr::rename(hook_line = "hook-line")
    
    target_count$total <- rowSums(target_count[, c( 3, 4, 5, 6)], na.rm = TRUE)

# calc percentage 
    target_percent <-target_count |> 
      dplyr::mutate(
        diving_percent = round(100 * diving / total, 2),
        hookline_percent = round(100 * hook_line / total, 2),
        net_percent = round(100 * net / total, 2),
        trap_percent = round(100 * trap / total, 2),
      ) 
    
# pivot longer
    target_graph <- target_percent |> 
      dplyr::select(st_yr, region, diving_percent, hookline_percent, net_percent, trap_percent) |> 
      dplyr::rename(diving = diving_percent, 
                    hook_line = hookline_percent, 
                    net = net_percent, 
                    trap = trap_percent) |> 
      tidyr::pivot_longer(
        cols = !c(st_yr, region),
        names_to = "gear_group",
        values_to = "percent"
      )
    
# graph spp breakdown by region and gear grouping 
    plot_target_spp <- target_graph |> 
      ggplot(aes(x = st_yr, y = percent, group = gear_group)) +
      geom_line(aes(color = gear_group)) +
      geom_point(aes(color = gear_group)) +
      ggplot2::facet_wrap( ~ region) +
      labs(
        color = "Gear Type",
        x = "Start Year",
        y = "Percent",
        title = paste("Percent Interview Count of ", print_spp, " by Gear Grouping")
      ) +
      theme(
        # legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.position = "bottom",
        legend.title = element_blank()
      )
    
# view
    plot_target_spp
    
    
# # Calculate modes ####
# ## create mode function ####
#     lenmode <- function(x) {
#       ux <- unique(x)
#       ux[which.max(tabulate(match(x, ux)))]
#     }
# 
# ## find mode across all gears, time, region ####
#     mode_spp <- lenmode(target_spp$length1_cm)
    
# ## plot mode by gears across time and regions ####  
#     plot_mode_target_spp <- target_summary |>
#       ggplot(aes(x = st_yr, 
#                  y = mode, 
#                  group = gear_group,
#                  )) +
#       geom_line(aes(color = gear_group)) +
#       geom_point(aes(color = gear_group)) +
#       ggplot2::facet_wrap( ~ region) +
#       labs(
#         color = "Gear Type",
#         x = "Start Year",
#         y = "Mode Length (cm)",
#         title = paste0(print_spp," Mode of Each Gear Group Across Time and Regions")
#       ) +
#       theme(
#         # legend.title = element_text(size = 14),
#         legend.text = element_text(size = 8),
#         legend.position = "bottom",
#         legend.title = element_blank()
#       ) +
#       geom_hline(yintercept = mode_spp,  
#                  color = "darkgrey", linewidth=1)
# 
# # view
#     plot_mode_target_spp
# 
#     
# ## plot count by gears across time and regions ####  
#     plot_count_target_spp <- target_summary |>
#       ggplot(aes(x = st_yr, 
#                  y = n_fish, 
#                  group = gear_group,
#       )) +
#       geom_line(aes(color = gear_group)) +
#       geom_point(aes(color = gear_group)) +
#       ggplot2::facet_wrap( ~ region) +
#       labs(
#         color = "Gear Type",
#         x = "Start Year",
#         y = "Sample Count",
#         title = paste0(print_spp," Sample Count of Each Gear Group Across Time and Regions")
#       ) +
#       theme(
#         # legend.title = element_text(size = 14),
#         legend.text = element_text(size = 8),
#         legend.position = "bottom",
#         legend.title = element_blank()
#       ) +
#       geom_hline(yintercept = mode_spp,  
#                  color = "darkgrey", linewidth=1)
#     
# # view
#     plot_count_target_spp   
# 
#   
# # do we want percent representation?
#     # count of interviews? 