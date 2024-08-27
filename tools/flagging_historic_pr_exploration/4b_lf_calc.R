# 4b_lf_calc

# Load libraries ####
librarian::shelf(here, tidyverse, measurements, flextable, ggplot2, reshape2)

# Specify settings ####
tip_pr <- "pr_filtered_tip_qtf_20240822.rds" # add formatted data
print_spp <- "Queen triggerfish"
# Queen triggerfish qtf  
# Redtail parrotfish rtp  
# Stoplight parrotfish slp 
# Lane snapper ls 
# Silk snapper ss 
# Queen snapper qs 
# Schoolmaster sms 
# Red hind reh 

# Read in raw data ####
tip <- readRDS(here::here("data", tip_pr))

# Calculate modes ####
## create mode function ####
lenmode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

## find mode across all gears, time, region ####
mode_all <- lenmode(tip$length1_cm)

## filter to gear group and time ####
mode_gear_group_time <- tip |> 
  group_by(gear_group, st_yr) |> 
  dplyr::summarize(
    .groups = "drop",
    n = dplyr::n(),
    n_id = n_distinct(id),
    first_year = min(year),
    last_year = max(year),
    n_years = dplyr::n_distinct(year),
    mode = lenmode(length1_cm)
  )

# Plot modes of gears across time ####
# add grand mode reference line
## Gear grouping count ####
spp_geargroup_id_count <- tip |>
  group_by(gear_group) |>
  dplyr::summarize(
    .groups = "drop",
    ggroup_count = n_distinct(id),
  ) |>
  dplyr::mutate(ggroup_id = paste0(gear_group, " (", ggroup_count, ")"))

## add gear counts to table    
spp_ggroup_count <- mode_gear_group_time |> 
  dplyr::mutate(
    ggroup_id =
      spp_geargroup_id_count$ggroup_id[match(
        mode_gear_group_time$gear_group,
        spp_geargroup_id_count$gear_group
      )]) 


## plot gear groups across time ####  
plot_spp_ggroup_count <- spp_ggroup_count |>
  ggplot(aes(x = st_yr, y = mode, group = ggroup_id)) +
  geom_line(aes(color = ggroup_id)) +
  geom_point(aes(color = ggroup_id)) +
  # ggplot2::facet_wrap( ~ region_name) +
  labs(
    color = "Gear Group",
    x = "Start Year",
    y = "Mode Length (cm)",
    title = paste(print_spp,"Mode of Each Gear Group Across Time")
  ) +
  theme(
    # legend.title = element_text(size = 14),
    legend.text = element_text(size = 8),
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  geom_hline(yintercept = mode_all,  
             color = "darkgrey", linewidth=1)

## filter to gear group and time ####
mode_gear_group_time_region <- tip |> 
  group_by(gear_group, st_yr, region) |> 
  dplyr::summarize(
    .groups = "drop",
    n = dplyr::n(),
    n_id = n_distinct(id),
    first_year = min(year),
    last_year = max(year),
    n_years = dplyr::n_distinct(year),
    mode = lenmode(length1_cm)
  )

## Create count of total observed unique interviews for each region  ####
spp_regions_id_count <- tip |>
  group_by(region)|>
  dplyr::summarize(
    .groups = "drop",
    region_count = n_distinct(id),
  )|>
  dplyr::mutate(region_id = paste0(region, " (", region_count, ")"))

## add region counts to table    
spp_region_count <- mode_gear_group_time_region |> 
  dplyr::mutate(
    region_id =
      spp_regions_id_count$region_id[match(
        mode_gear_group_time_region$region,
        spp_regions_id_count$region
      )]) 

## add gear counts to table    
spp_ggroup_region_count <- spp_region_count |> 
  dplyr::mutate(
    ggroup_id =
      spp_geargroup_id_count$ggroup_id[match(
        spp_region_count$gear_group,
        spp_geargroup_id_count$gear_group
      )]) 

## plot gears across time ####  
plot_spp_ggroup_region_count <- spp_ggroup_region_count |>
  ggplot(aes(x = st_yr, y = mode, group = ggroup_id)) +
  geom_line(aes(color = ggroup_id)) +
  geom_point(aes(color = ggroup_id)) +
  ggplot2::facet_wrap( ~ region_id) +
  labs(
    color = "Gear Type",
    x = "Start Year",
    y = "Mode Length (cm)",
    title = paste(print_spp,"Mode of Each Gear Group Across Time")
  ) +
  theme(
    # legend.title = element_text(size = 14),
    legend.text = element_text(size = 8),
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  geom_hline(yintercept = mode_all,  
             color = "darkgrey", linewidth=1)


################################################
# Specific Gears ####
################################################

# ## filter to gear and time ####
# unique(tip$gear)
# mode_gear_time <- tip |> 
#   group_by(gear, st_yr) |> 
#   dplyr::summarize(
#     .groups = "drop",
#     n = dplyr::n(),
#     n_id = n_distinct(id),
#     first_year = min(year),
#     last_year = max(year),
#     n_years = dplyr::n_distinct(year),
#     mode = lenmode(length1_cm)
#   )

# ## plot gears across time ####  
# plot_mode_gear_time <- mode_gear_time |>
#   ggplot(aes(x = st_yr, y = mode, group = gear)) +
#   geom_line(aes(color = gear)) +
#   geom_point(aes(color = gear)) +
#   # ggplot2::facet_wrap( ~ region_name) +
#   labs(
#     color = "Gear Type",
#     x = "Mode Length (cm)",
#     title = paste(print_spp,"Mode of Each Gear Across Time")
#   ) +
#   theme(
#     # legend.title = element_text(size = 14),
#     legend.text = element_text(size = 8),
#     legend.position = "bottom",
#     legend.title = element_blank()
#   ) +
#   geom_hline(yintercept = mode_all,  
#            color = "darkgrey", linewidth=1)

# # plot gears across time by region ####
# ## filter to gear and time ####
# mode_gear_time_region <- tip |> 
#   group_by(gear, st_yr, region) |> 
#   dplyr::summarize(
#     .groups = "drop",
#     n = dplyr::n(),
#     n_id = n_distinct(id),
#     first_year = min(year),
#     last_year = max(year),
#     n_years = dplyr::n_distinct(year),
#     mode = lenmode(length1_cm)
#   )|>
#   mutate(
#     region_name = case_when(
#       region == "W" ~ "West",
#       region == "N" ~ "North",
#       region == "E" ~ "East",
#       region == "S" ~ "South",
#       .default = "not coded"
#     ))

# ## plot gear across time ####  
# plot_mode_gear_time_region <- mode_gear_time_region |>
#   ggplot(aes(x = st_yr, y = mode, group = gear)) +
#   geom_line(aes(color = gear)) +
#   geom_point(aes(color = gear)) +
#   ggplot2::facet_wrap( ~ region_name) +
#   labs(
#     color = "Gear Group",
#     x = "Start Year",
#     y = "Mode Length (cm)",
#     title = paste(print_spp,"Mode of Each Gear Group Across Time")
#   ) +
#   theme(
#     # legend.title = element_text(size = 14),
#     legend.text = element_text(size = 8),
#     legend.position = "bottom",
#     legend.title = element_blank()
#   ) +
#   geom_hline(yintercept = mode_all,  
#              color = "darkgrey", linewidth=1)
