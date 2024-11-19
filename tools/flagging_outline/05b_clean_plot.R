# 05_plot

#' plot the filtered data included
#' 1 - gears across time
#' 2 - aggregated density of lengths 


# Load libraries ####
  librarian::shelf(here, tidyverse, flextable, ggplot2, janitor)

# Specify settings ####
# date from end of 05a script
  date <- "20241119" 
# date from non-confidential gears (rds from 04a)
  gear_date <- "20241119" 
# date of 02c
  unfiltered_date <- "20241118"
# gears representing >2% after break year (rds from 04a)
  # clean_gear_bkyr <- "stx_csl_clean_gear_list_break_year_20241003.rds" 
  spp <- "csl"
  print_spp <- "Caribbean Spiny Lobster"
  
  isl <- "pr"
  print_isl <- "Puerto Rico"
  # isl <- "stx"
  # print_isl <- "St. Croix"
  # isl <- "stt"
  # print_isl <- "St. Thomas/St. John"
  
  break_year <- 2012
  min_year <- 1981
  max_year <- 2023
  len_type <- "CARAPACE LENGTH"
  target_len <- "Carapace Length"
  sedar <- "sedar91"
  disclaimer <- "Only nonconfidential data shown."

# Read in formatted data ####
  tip_spp_rds <- paste0(isl, "_", spp, "_clean_filtered_", date, ".rds" )
  tip_spp <- readRDS(here::here("data",  sedar, "rds", spp, isl, tip_spp_rds))
  
  gear_list <- paste0(isl, "_", spp, "_nonconf_gear_list_", gear_date, ".rds" )
  gear_nonconf <- readRDS(here::here("data",  sedar, "rds", spp, isl, gear_list))
  
  # gear_list_bkyr <- paste0(isl, "_", spp, "_clean_gear_list_break_year_", clean_gear_bkyr, ".rds" )
  # gear_2percent_bkyr <- readRDS(here::here("data",  sedar, "rds", spp, isl, gear_list_bkyr))
  
  unfiltered <- paste0(isl, "_", spp, "_spp_size_island_view_", unfiltered_date, ".rds" )
  unfiltered_isl <- readRDS(here::here("data",  sedar, "rds", spp, isl, unfiltered))

# Summary stats ####    
# calculate stats on all available target species/island data
# total number of target length type measurements before filtering
  n_target_len <- sum(unfiltered_isl$length_type1 == len_type)
# total number of all lengths measured before filtering  
  n_all_len <- length(unfiltered_isl$length_type1)
# percent representation of target lengths   
  p_target_len <- round(n_target_len / n_all_len, 3) * 100
# total number of unique interviews before filtering   
  trip_id_unique <- as.data.frame(table(unfiltered_isl$sampling_unit_id, useNA = "always"))
  total_trip_id_unique <- nrow(trip_id_unique)
  
  
# Create count of observed records for each area  ####
  tip_spp_count <- tip_spp |>
    add_count(county_landed) |>
    dplyr::mutate(county_landedn = paste0(county_landed, " (", n, ")")) |>
    select(-n) |>
    add_count(gear) |>
    dplyr::mutate(gearn = paste0(gear, " (", n, ")")) |>
    select(-n)|>
    add_count(island) |>
    dplyr::mutate(islandn = paste0(island, " (", n, ")")) |>
    select(-n) 
  
# create non-confidential gears dataframe ####
  tip_spp_nc <- gear_nonconf |> 
    select(Gear, confidential) |> 
    mutate(gear = as.character(Gear)) |> 
    select(-Gear) |> 
    right_join(tip_spp_count, by = join_by(gear)) |> 
    filter(confidential == "FALSE")

# Plot gears by number of fish measured used over time ####
  gear_data <- tip_spp_nc |>
    group_by(year, gearn) |>
    dplyr::summarize(n = n(), .groups = "drop") |>
    mutate(year = as.integer(year))
  
  gear_by_yr <- gear_data |>
    group_by(gearn) |>
    dplyr::mutate(total_n = sum(n)) |>
    ungroup() |>
    dplyr::mutate(gearn = fct_reorder(gearn, total_n)) %>%
    ggplot(aes(x = year, y = gearn, color = gearn, size = n)) +
    geom_point() +
    labs(
      x = "Year", y = "", colour = "", shape = "",
      title = paste(print_isl, "Length Samples"),
      caption = disclaimer
    ) +
    theme_bw() +
    theme(
      legend.position = "null", text = element_text(size = 15),
      title = element_text(size = 15)
    )
# view  
  gear_by_yr
# save  
  ggsave(filename = 
           here::here("data", sedar, "figure", spp, isl, "gear_by_yr.png"),
         width = 14, height = 8)
# save figure under alphanumeric identifier    
  abc1 = gear_by_yr

# Plot gears by number of unqiue interviews over time ####
  gear_data_id <- tip_spp_nc %>% 
    group_by(gear) %>% 
    dplyr::mutate(n_ID = n_distinct(sampling_unit_id)) |> 
    dplyr::filter(n_ID >= 3) %>% ungroup %>%
    group_by(year, gear) |>
    dplyr::summarize(n_ID = n_distinct(sampling_unit_id), .groups = "drop") |> 
    mutate(year = as.integer(year))
  
  gear_by_id <- gear_data_id |>
    # filter(YEAR > 2011) |> 
    group_by(gear) |>
    dplyr::mutate(total_n = sum(n_ID)) |> 
    ungroup() |>   
    dplyr::mutate(gear = fct_reorder(gear, total_n)) %>%
    ggplot(aes(x = year, y = gear, color = gear, size = n_ID)) +
    geom_point()  +
    labs(x = "Year", y = "", colour = "", shape = "", 
         title = paste(print_isl, "Unique Interviews"),
         caption = disclaimer) +
    theme_bw() + 
    theme(legend.position="null", text = element_text(size = 15), 
          title = element_text(size = 15))
# add line at breakyear if needed 
    # geom_vline(xintercept = (break_year - 0.5),  
    #            color = "darkgrey", linewidth=1.5)
# view  
  gear_by_id
# save  
  ggsave(filename = 
           here::here("data", sedar, "figure", spp, isl, "gear_by_id.png"),
         width = 14, height = 8)
# save figure under alphanumeric identifier    
  abc2 = gear_by_id
  
# Plot gears by number of fish measured used over time ####
  mean_data <- tip_spp_nc |>
    group_by(year) |>
    dplyr::summarize(n = n(), 
                     mean_mm = mean(length1_mm),
                     mean_cm = mean(length1_cm),
                     mean_in = mean(length1_inch),
                     .groups = "drop") |>
    mutate(year = as.integer(year))
  
  library(ggpmisc)
  
  mean_by_yr <- mean_data |>
    # filter(YEAR > 2011) |> 
    # group_by(gear) |>
    # dplyr::mutate(total_n = sum(n_ID)) |> 
    # ungroup() |>   
    # dplyr::mutate(gear = fct_reorder(gear, total_n)) %>%
    ggplot(aes(x = year, y = mean_in, 
               # color = gear, size = n_ID
               )) +
    geom_point()  +
    stat_summary(fun.data= mean_cl_normal) +
    geom_smooth(method = "lm", se=TRUE) +
    stat_poly_eq(mapping = use_label("R2")) +
    labs(x = "Year", y = "Length (in)", colour = "", shape = "", 
         title = paste(print_isl, "Mean Length Over Time"),
         caption = disclaimer) +
    theme_bw() + 
    theme(legend.position="null", text = element_text(size = 15), 
          title = element_text(size = 15))

# view  
  mean_by_yr
# save  
  ggsave(filename = 
           here::here("data", sedar, "figure", spp, isl, "mean_by_yr.png"),
         width = 14, height = 8)
# save figure under alphanumeric identifier    
  abc21 = mean_by_yr
  
# histogram plot of means across time   
  plot_mean_hist <- tip_spp_nc |>
    group_by(year) |> 
    ggplot(aes(x = year, y =length1_inch, group = year)) +
    geom_boxplot(notch = FALSE,
                 outlier.shape = NA
                 ) +
    # ggplot2::facet_wrap( ~ region_id) +
    # coord_cartesian(ylim = NULL ) +
    coord_cartesian(ylim = c(2,6.75) ) +
    labs(
      # color = "Gear Group",
      x = "Start Year",
      y = "Length (in)", 
      title = paste(print_spp, print_isl, "Length Samples")
    ) +
    theme(
      # legend.title = element_text(size = 14),
      legend.text = element_text(size = 8),
      legend.position = "bottom",
      legend.title = element_blank()
    ) 
  # +
    # geom_hline(yintercept = med_all,  
               # color = "darkgrey", linewidth=1)
  
# view
  plot_mean_hist
  
# save  
  ggsave(filename = 
           here::here("data", sedar, "figure", spp, isl, "plot_mean_hist.png"),
         width = 14, height = 8)
# save figure under alphanumeric identifier    
  abc23 = plot_mean_hist
  
# graph number of total lengths measured per year    
  count_by_yr <- mean_data |>
    # filter(YEAR > 2011) |> 
    # group_by(gear) |>
    # dplyr::mutate(total_n = sum(n_ID)) |> 
    # ungroup() |>   
    # dplyr::mutate(gear = fct_reorder(gear, total_n)) %>%
    ggplot(aes(x = year, y = n, 
               # color = gear, size = n_ID
    )) +
    geom_point()  +
    stat_summary(fun.data= mean_cl_normal) +
    geom_smooth(method = "lm", se=TRUE) +
    stat_poly_eq(mapping = use_label("R2")) +
    labs(x = "Year", y = "Count of Records", colour = "", shape = "", 
         title = paste(print_isl, "Count of Lengths Over Time"),
         caption = disclaimer) +
    theme_bw() + 
    theme(legend.position="null", text = element_text(size = 15), 
          title = element_text(size = 15))
  
# view  
  count_by_yr
# save  
  ggsave(filename = 
           here::here("data", sedar, "figure", spp, isl, "count_by_yr.png"),
         width = 14, height = 8)
  # save figure under alphanumeric identifier    
  abc22 = count_by_yr 

# Gear Density Plots ####

## Aggregated density plots ####
### overlay time periods 

# calculate mean of all lengths  
  full_mean = round(mean(tip_spp$length1_cm), 2)

# filter years after break year  
  tip_spp_break <- tip_spp |>
    filter(year >= break_year) 
# calculate mean of all lengths after break year
  truncated_mean = round(mean(tip_spp_break$length1_cm), 2)

# create density plot of all lengths comparing the full and truncated time periods
  agr_den_NOgears_compare <- 
    ggplot() +
    geom_density(aes(length1_cm, 
                     color = "tip_spp"),
                 linewidth = 1.0, 
                 alpha = .2, 
                 data = tip_spp) +
    geom_density(aes(length1_cm, 
                     color = "tip_spp_break"),
                 linewidth = 1.0, 
                 alpha = .2, 
                 data = tip_spp_break) +
    geom_vline(data = tip_spp, 
               aes(xintercept=mean(length1_cm), 
                   color = "tip_spp"),
               linetype="dashed", linewidth=1) +
    geom_vline(data=tip_spp_break, 
               aes(xintercept=mean(length1_cm), 
                   color = "tip_spp_break"),
               linetype="dashed", linewidth=1) +
    labs(x = paste0(target_len, " (cm)"), 
         title = paste0(print_isl, " ", print_spp,
                        " aggregated length density"))+
    guides(color=guide_legend(title="Years-Mean"))+
    scale_color_discrete(
      labels=c(paste0(min_year, "_", max_year, "-", full_mean), 
               paste0(break_year, "_", max_year, "-", truncated_mean)))+
    theme(legend.title = element_text(size=15), 
          legend.text = element_text(size=15),
          legend.position = "bottom",
          axis.text.x=element_text(size = 15),
          axis.text.y=element_text(size = 15),
          axis.title.x = element_text( size = 15),
          axis.title.y = element_text( size = 15),
          title = element_text(size = 15))
# view  
  agr_den_NOgears_compare
# save  
  ggsave(filename = 
           here::here("data", sedar, "figure", spp, isl, "agr_den_NOgears_compare.png"),
         width = 14, height = 8)
  
# save figure under alphanumeric identifier    
  abc3 = agr_den_NOgears_compare

  
# create density plot of all lengths of just the full time period
  agr_den_NOgears_full <- 
    ggplot() +
    geom_density(aes(length1_cm, 
                     color = "tip_spp"),
                 linewidth = 1.0, 
                 alpha = .2, 
                 data = tip_spp) +
    geom_vline(data = tip_spp, 
               aes(xintercept=mean(length1_cm), 
                   color = "tip_spp"),
               linetype="dashed", linewidth=1) +
    labs(x = paste0(target_len, " (cm)"), 
         title = paste0(print_isl, " ", print_spp, 
                        " aggregated length density"))+
    guides(color=guide_legend(title="Mean"))+
    scale_color_discrete(
      labels=c(paste0(full_mean)))+
    theme(legend.title = element_text(size=15), 
          legend.text = element_text(size=15),
          legend.position = "bottom",
          axis.text.x=element_text(size=15),
          axis.text.y=element_text(size=15),
          axis.title.x = element_text( size = 15),
          axis.title.y = element_text( size = 15),
          title = element_text(size = 15))
# view
  agr_den_NOgears_full
# save  
  ggsave(filename = 
           here::here("data", sedar, "figure", spp, isl, "agr_den_NOgears_full.png"),
         width = 14, height = 8)
  
# save figure under alphanumeric identifier    
  abc4 = agr_den_NOgears_full  
  
### GEAR INDIVIDUALS ####
# create aggregated density graph of all lengths separated by gear  
# create count of records to use in figure labels   
  ycounts_all =tip_spp %>% 
    tabyl("gear") %>%
    mutate(n_labels = paste0(gear, " (n= ", n, ")" ))
  
# calculate means to use in figure    
  muv <- plyr::ddply(tip_spp, 
                     "gear", 
                     summarise, 
                     grp.mean=mean(length1_cm))
# view mean values  
  head(muv)
  
# create density plot showing individual gears representing >2% of data   
  agr_den_all_gears <- tip_spp %>% 
    ggplot(aes(length1_cm))+
    geom_density(aes(color = gear),
                 linewidth = 0.75)+
    scale_color_hue(labels=ycounts_all$n_labels)+
    labs(color = "Gear" , 
         x = paste0(target_len, " (cm)"), 
         title = paste0(print_isl, " ", print_spp, " all gears"))+ 
    #title = paste0(county,  "\n (N = ", sum(ycounts$n), ")"))+
    # theme_minimal()
    theme(legend.title = element_text(size=12), 
          legend.text = element_text(size=12),
          legend.position = "bottom",
          axis.text.x=element_text(size=15),
          axis.text.y=element_text(size=15),
          axis.title.x = element_text( size = 15),
          axis.title.y = element_text( size = 15),
          title = element_text(size = 15))+
    guides(color=guide_legend(ncol = 2))+
    geom_vline(data=muv, aes(xintercept=grp.mean, color=gear),
               linetype="dashed")
# view  
  agr_den_all_gears
# save  
  ggsave(filename = 
           here::here("data", sedar, "figure", spp, isl, "agr_den_all_gears.png"),
         width = 14, height = 8)
  
# save figure under alphanumeric identifier    
  abc5 = agr_den_all_gears
  
  

# use NON CONFIDENTIAL gears ####  
# create count of records to use in figure labels   
  ycounts_top = tip_spp_nc %>% 
    tabyl("gear") %>%
    mutate(n_labels = paste0(gear, " (n= ", n, ")" ))

# calculate means to use in figure    
  muv <- plyr::ddply(tip_spp_nc, 
                     "gear", 
                     summarise, 
                     grp.mean=mean(length1_cm))
# view mean values  
  head(muv)
  
# create density plot showing individual gears representing >2% of data   
  agr_den_top_gears <- tip_spp_nc %>% 
    ggplot(aes(length1_cm))+
    geom_density(aes(color = gear),
                 linewidth = 0.75)+
    scale_color_hue(labels=ycounts_top$n_labels)+
    labs(color = "Gear" , 
         x = paste0(target_len, " (cm)"), 
         title = paste0(print_isl, " ", print_spp, " non-confidential gears"))+ 
    #title = paste0(county,  "\n (N = ", sum(ycounts$n), ")"))+
    # theme_minimal()
    theme(legend.title = element_text(size=12), 
          legend.text = element_text(size=12),
          legend.position = "bottom",
          axis.text.x=element_text(size=15),
          axis.text.y=element_text(size=15),
          axis.title.x = element_text( size = 15),
          axis.title.y = element_text( size = 15),
          title = element_text(size = 15))+
    guides(color=guide_legend(ncol = 2))+
    geom_vline(data=muv, aes(xintercept=grp.mean, color=gear),
               linetype="dashed")
# view
  agr_den_top_gears
# save  
  ggsave(filename = 
           here::here("data", sedar, "figure", spp, isl, "agr_den_top_gears.png"),
         width = 14, height = 8)
  
# save figure under alphanumeric identifier    
  abc6 = agr_den_top_gears
  
  

##### if using time break continue, IF NOT - SKIP to next figure #####  
### top gears after time break #### 
# # filter to gears representing >2% of records
#   tip_spp_break_gears <- tip_spp_break |> 
#       filter(gear %in% gear_2percent_bkyr$Gear) 
# 
# # create count of records for figure labels    
#   ycounts_bkyr =tip_spp_break_gears %>% 
#     tabyl("gear") %>%
#     mutate(n_labels = paste0(gear, " (n= ", n, ")" ))
# 
# # calculate mean to display on density plot     
#   muv_break <- 
#     plyr::ddply(tip_spp_break_gears, 
#                 "gear", summarise, grp.mean=mean(length1_cm))
# # view means
#     head(muv_break)
# 
# # create density graph of gears representing >2% of records after break year       
#   agr_den_break <- tip_spp_break_gears %>% 
#     ggplot(aes(length1_cm))+
#     geom_density(aes(color = gear),linewidth = 0.75)+
#     scale_color_hue(labels=ycounts_bkyr$n_labels)+
#     labs(color = "Gear" , x = paste0(target_len, " (cm)"), title = print_isl)+ 
#     #title = paste0(print_isl,  "\n (N = ", sum(ycounts$n), ")"))+
#     theme(legend.title = element_text(size=20), 
#           legend.text = element_text(size=20),
#           legend.position = "bottom",
#           axis.text.x=element_text(size=20),
#           axis.text.y=element_text(size=20),
#           axis.title.x = element_text( size = 20),
#           axis.title.y = element_text( size = 20),
#           title = element_text(size = 20))+
#     guides(color=guide_legend(ncol = 2))+
#     geom_vline(data=muv12, aes(xintercept=grp.mean, color=gear),
#                linetype="dashed")
# # view
#   agr_den_break
# # save
#   ggsave(filename = 
#            here::here("data", sedar, "figure", spp, isl, "agr_den_break.png"),
#          width = 14, height = 8)
#   
# # save figure under alphanumeric identifier    
#   abc7 = agr_den_break
  
#### SKIP TO HERE ####
## Annual Density plots ####

### gears together ####
# create annual density plot of lengths over all years 
plot_ann_den_full <-
  tip_spp %>%  
  group_by(year) %>%
  dplyr::mutate(year_labs = paste0(year, "\n n = ", n())) %>%
  ggplot(aes(length1_cm))+
  geom_density(linewidth = 0.75)+
  # geom_vline(data = fleet_final, aes(xintercept=mean(FL_CM)),
  # linetype="dashed", linewidth=1)+
  #scale_color_manual(values = gearcols, labels = counts$n_labels)+
  # scale_color_hue(labels=fcounts$n_labels)+
  labs(x = paste0(target_len, " (cm)"), 
       title = paste0(print_isl," ", print_spp,  "\n (N = ", sum(ycounts_all$n), ")"))+
  facet_wrap(~year_labs, ncol = 7)
# view  
  plot_ann_den_full
# save  
  ggsave(filename = 
           here::here("data", sedar, "figure", spp, isl, "plot_ann_den_full.png"),
         width = 14, height = 8)
  
# save figure under alphanumeric identifier    
  abc8 = plot_ann_den_full
  
# create annual density plot of top gears across all years  
# filter to top gears
   # ann_den_top_gears <- tip_spp|>
   #   filter(gear %in% gear_2percent$Gear)

   # fcounts = ann_den_top_gears %>%  group_by(year) %>% filter(n() >= 30) %>% ungroup %>%
   #   tabyl(gear) %>%
   #   mutate(n_labels = paste0(gear, " (n= ", n, ")" ))
   
# create annual density plot of lengths over all years 
  plot_ann_den_top <-
    tip_spp_nc %>% 
    group_by(year) %>%
    dplyr::mutate(year_labs = paste0(year, "\n n = ", n())) %>%
    ggplot(aes(length1_cm))+
    geom_density(linewidth = 0.75)+
    # geom_vline(data = fleet_final, aes(xintercept=mean(FL_CM)),
    # linetype="dashed", linewidth=1)+
    #scale_color_manual(values = gearcols, labels = counts$n_labels)+
    # scale_color_hue(labels=fcounts$n_labels)+
    labs(x = paste0(target_len, " (cm)"), 
         title = paste0(print_isl, " ", print_spp, " Non- Gears", "\n (N = ", sum(ycounts_top$n), ")"))+
    facet_wrap(~year_labs, ncol = 7)  
# view 
  plot_ann_den_top
# save
  ggsave(filename = 
           here::here("data", sedar, "figure", spp, isl, "plot_ann_den_top.png"),
         width = 14, height = 8)
  
# save figure under alphanumeric identifier    
  abc9 = plot_ann_den_top
  
### top gears separated ####
# fcounts = ann_den_top_gears %>%  group_by(year) %>% filter(n() >= 30) %>% ungroup %>%
#   tabyl(gear) %>%
#   mutate(n_labels = paste0(gear, " (n= ", n, ")" ))

plot_ann_den_separate <-
    tip_spp_nc %>% 
  group_by(year) %>%
  dplyr::mutate(year_labs = paste0(year, "\n n = ", n())) %>%
  ggplot(aes(length1_cm, color = gear))+
  geom_density(linewidth = 0.75)+
  #scale_color_manual(values = gearcols, labels = counts$n_labels)+
  scale_color_hue(labels=ycounts_top$n_labels)+
  labs(color = "Gear Type", 
       x = paste0(target_len, " (cm)"), 
       title = paste0(print_isl, " ", print_spp, "\n (N = ", sum(ycounts_top$n), ")"))+
  facet_wrap(~year_labs, ncol = 7)+
  # theme_minimal()
  guides(color=guide_legend(ncol = 2))+
  theme(legend.title = element_text(size=14), 
        legend.text = element_text(size=12),
        legend.position = "bottom")
# view 
  plot_ann_den_separate
# save
  ggsave(filename = 
           here::here("data", sedar, "figure", spp, isl, "plot_ann_den_separate.png"),
         width = 14, height = 8)
  
# save figure under alphanumeric identifier     
  abc10 = plot_ann_den_separate

# Aggregated cumulative density ####

# create count and percent calculation for each gear  
  counts = tip_spp %>%
    tabyl(gear) %>%
    mutate(n_labels = paste0(gear, " (n= ", n, ")" ))
# graph aggregated cumulative density 
  cumulative_den <- tip_spp %>%
    ggplot(aes(length1_cm))+
    stat_ecdf()+
    labs(x = paste0(target_len, " (cm)"),
         title = paste0(print_isl, " ", print_spp, "\n (N = ", sum(counts$n), ")"))+
    theme_minimal()
# view  
  cumulative_den
# save
  ggsave(filename = 
           here::here("data", sedar, "figure", spp, isl, "cumulative_den.png"),
         width = 14, height = 8)
  
# save figure under alphanumeric identifier    
  abc20 = cumulative_den


# SAVE WORKSPACE ####
# workspace needed when pulling values into quarto document  
  save.image(
    file = here::here(
      "data",
      sedar,
      "rdata",
      spp, 
      isl,
      paste0(
        isl, "_",
        spp, "_sedar_figures_",
        format(Sys.time(), "%Y%m%d"), ".RData"
      )
    )
  )
  