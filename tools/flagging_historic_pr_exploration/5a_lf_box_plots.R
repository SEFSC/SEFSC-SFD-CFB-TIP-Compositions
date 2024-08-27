# 5a_lf_box_plots
# reminder - species is the same thats set in 4a

# Load libraries ####
librarian::shelf(here, tidyverse, measurements, flextable, ggplot2, reshape2)

# Specify settings ####
tip_pr <- "pr_filtered_tip_qtf_20240822.rds" # add formatted data
gr_one <- "trap" # trap, hook-line, net, diving
gr_one_print <- "Trap" # Hook-line, Trap, Net, Diving
gr_two <- "net" # trap, hook-line, net, diving
gr_two_print <- "Net" # Hook-line, Trap, Net, Diving
gr_three <- "diving" # trap, hook-line, net, diving
gr_three_print <- "Diving" # Hook-line, Trap, Net, Diving
spp_print <- "Queen triggerfish"
# Queen triggerfish qtf  
# Redtail parrotfish rtp  
# Stoplight parrotfish slp 
# Lane snapper ls 
# Silk snapper ss 
# Queen snapper qs 
# Schoolmaster sms 
# Red hind reh 

# gr_print <- "Hand Line"
# gr <- "LINES HAND" 

# Read in raw data ####
tip <- readRDS(here::here("data", tip_pr))

## find mode across all gears, time, region ####
med_all <- median(tip$length1_cm, na.rm = TRUE)

# filter to first gear ####
spp_gear_one <- tip |>
  filter(gear_group %in% gr_one)

## Create count of total observed unique interviews for each region  ####
spp_regions_gr_one <- spp_gear_one |>
  group_by(region)|>
  dplyr::summarize(
    .groups = "drop",
    region_count = n_distinct(id),
  )|>
  dplyr::mutate(region_id = paste0(region, " (", region_count, ")"))

## add region counts to table    
spp_region_one_count <- spp_gear_one |> 
  dplyr::mutate(
    region_id =
      spp_regions_gr_one$region_id[match(
        spp_gear_one$region,
        spp_regions_gr_one$region
      )]) 

# Plot first gear group across regions ####
# run and then select ylim
plot_gear_group_one <- spp_region_one_count |>
  ggplot(aes(x = st_yr, y = length1_cm)) +
  geom_boxplot(notch = FALSE,
               outlier.shape = NA) +
  ggplot2::facet_wrap( ~ region_id) +
  # coord_cartesian(ylim = NULL ) +
  coord_cartesian(ylim = c(15,60) ) +
  labs(
    color = "Gear Group",
    x = "Start Year",
    y = "Length (cm)", 
    title = paste(spp_print, gr_one_print, "Length Samples")
  ) +
  theme(
    # legend.title = element_text(size = 14),
    legend.text = element_text(size = 8),
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  geom_hline(yintercept = med_all,  
             color = "darkgrey", linewidth=1)

# filter to second gear ####
spp_gear_two <- tip |>
  filter(gear_group %in% gr_two)

## Create count of total observed unique interviews for each region  ####
spp_regions_gr_two <- spp_gear_two |>
  group_by(region)|>
  dplyr::summarize(
    .groups = "drop",
    region_count = n_distinct(id),
  )|>
  dplyr::mutate(region_id = paste0(region, " (", region_count, ")"))

## add region counts to table    
spp_region_two_count <- spp_gear_two |> 
  dplyr::mutate(
    region_id =
      spp_regions_gr_two$region_id[match(
        spp_gear_two$region,
        spp_regions_gr_two$region
      )]) 

# Plot first gear group across regions ####
# run and then select ylim
plot_gear_group_two <- spp_region_two_count |>
  ggplot(aes(x = st_yr, y = length1_cm)) +
  geom_boxplot(notch = FALSE,
               outlier.shape = NA) +
  ggplot2::facet_wrap( ~ region_id) +
  # coord_cartesian( ylim = NULL,) +
  coord_cartesian( ylim = c(15,60)) +
  labs(
    color = "Gear Group",
    x = "Start Year",
    y = "Length (cm)", 
    title = paste(spp_print, gr_two_print, "Length Samples")
  ) +
  theme(
    # legend.title = element_text(size = 14),
    legend.text = element_text(size = 8),
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  geom_hline(yintercept = med_all,  
             color = "darkgrey", linewidth=1)

# filter to third gear ####
spp_gear_three <- tip |>
  filter(gear_group %in% gr_three)

## Create count of total observed unique interviews for each region  ####
spp_regions_gr_three <- spp_gear_three |>
  group_by(region)|>
  dplyr::summarize(
    .groups = "drop",
    region_count = n_distinct(id),
  )|>
  dplyr::mutate(region_id = paste0(region, " (", region_count, ")"))

## add region counts to table    
spp_region_three_count <- spp_gear_three |> 
  dplyr::mutate(
    region_id =
      spp_regions_gr_three$region_id[match(
        spp_gear_three$region,
        spp_regions_gr_three$region
      )]) 

# Plot first gear group across regions ####
# run and then select ylim
plot_gear_group_three <- spp_region_three_count |>
  ggplot(aes(x = st_yr, y = length1_cm)) +
  geom_boxplot(notch = FALSE,
               outlier.shape = NA) +
  ggplot2::facet_wrap( ~ region_id) +
  # coord_cartesian( ylim = NULL,) +
  coord_cartesian( ylim = c(10,60)) +
  labs(
    color = "Gear Group",
    x = "Start Year",
    y = "Length (cm)", 
    title = paste(spp_print, gr_three_print, "Length Samples")
  ) +
  theme(
    # legend.title = element_text(size = 14),
    legend.text = element_text(size = 8),
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  geom_hline(yintercept = med_all,  
             color = "darkgrey", linewidth=1)

# # Plot gear across regions ####
# plot_gear <- tip |>
#   filter(gear %in% gr) |> 
#   ggplot(aes(x = st_yr, y = length1_cm)) +
#   geom_boxplot(notch = FALSE) +
#   ggplot2::facet_wrap( ~ region) +
#   labs(
#     color = "Gear",
#     x = "Year Group",
#     y = "Length (cm)", 
#     title = paste(spp_print, gr_print, "Length Samples")
#   ) +
#   theme(
#     # legend.title = element_text(size = 14),
#     legend.text = element_text(size = 8),
#     legend.position = "bottom",
#     legend.title = element_blank()
#   ) +
#   geom_hline(yintercept = med_all,  
#              color = "darkgrey", linewidth=1)
