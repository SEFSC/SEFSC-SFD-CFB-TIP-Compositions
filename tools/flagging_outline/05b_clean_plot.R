# 05_plot

#' plot the filtered data included
#' 1 - gears across time
#' 2 - aggregated density of lengths 


# Load libraries ####
  librarian::shelf(here, tidyverse, flextable, ggplot2, janitor)

# Specify settings ####
# rds from end of 05a script
  tip_spp_rds <- "stx_csl_clean_filtered_20241003.rds" 
# gears representing >2% (rds from 04a)
  clean_gear <- "stx_csl_clean_gear_list_20241003.rds" 
# gears representing >2% after break year (rds from 04a)
  # clean_gear_bkyr <- "stx_csl_clean_gear_list_break_year_20241003.rds" 
  spp <- "csl"
  isl <- "stx"
  print_isl <- "St. Croix"
  print_spp <- "Caribbean Spiny Lobster"
  break_year <- 2012
  min_year <- 1984
  max_year <- 2022

# Read in formatted data ####
  tip_spp <- readRDS(here::here("data", tip_spp_rds))
  gear_2percent <- readRDS(here::here("data", clean_gear))
  # gear_2percent_bkyr <- readRDS(here::here("data", clean_gear_bkyr))
  
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

# Plot gears by number of fish measured used over time ####
  gear_data <- tip_spp_count |>
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
      title = paste(print_isl, "Length Samples")
    ) +
    theme_bw() +
    theme(
      legend.position = "null", text = element_text(size = 15),
      title = element_text(size = 15)
    )

# save figure under alphanumeric identifier    
  abc1 = gear_by_yr

# Plot gears by number of unqiue interviews over time ####
  gear_data_id <- tip_spp_count %>% 
    group_by(gear) %>% 
    dplyr::mutate(n_ID = n_distinct(id)) |> 
    dplyr::filter(n_ID >= 3) %>% ungroup %>%
    group_by(year, gear) |>
    dplyr::summarize(n_ID = n_distinct(id), .groups = "drop") |> 
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
         title = paste(print_isl, " Unique Interviews")) +
    theme_bw() + 
    theme(legend.position="null", text = element_text(size = 20), 
          title = element_text(size = 15))
# add line at breakyear if needed 
    # geom_vline(xintercept = (break_year - 0.5),  
    #            color = "darkgrey", linewidth=1.5)

# save figure under alphanumeric identifier    
  abc2 = gear_by_id

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
    labs(x = "Fork Length (cm)", 
         title = paste0(print_isl, " ", print_spp,
                        " aggregated length density"))+
    guides(color=guide_legend(title="Years-Mean"))+
    scale_color_discrete(
      labels=c(paste0(min_year, "_", max_year, "-", full_mean), 
               paste0(break_year, "_", max_year, "-", truncated_mean)))+
    theme(legend.title = element_text(size=20), 
          legend.text = element_text(size=20),
          legend.position = "bottom",
          axis.text.x=element_text(size=20),
          axis.text.y=element_text(size=20),
          axis.title.x = element_text( size = 20),
          axis.title.y = element_text( size = 20),
          title = element_text(size = 20))
  
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
    labs(x = "Fork Length (cm)", 
         title = paste0(print_isl, " ", print_spp, 
                        " aggregated length density"))+
    guides(color=guide_legend(title="Mean"))+
    scale_color_discrete(
      labels=c(paste0(full_mean)))+
    theme(legend.title = element_text(size=20), 
          legend.text = element_text(size=20),
          legend.position = "bottom",
          axis.text.x=element_text(size=20),
          axis.text.y=element_text(size=20),
          axis.title.x = element_text( size = 20),
          axis.title.y = element_text( size = 20),
          title = element_text(size = 20))
  
# save figure under alphanumeric identifier    
  abc4 = agr_den_NOgears_full  

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
         x = "Fork Length (cm)", 
         title = paste0(print_isl, " ", print_spp, " all gears"))+ 
    #title = paste0(county,  "\n (N = ", sum(ycounts$n), ")"))+
    # theme_minimal()
    theme(legend.title = element_text(size=12), 
          legend.text = element_text(size=12),
          legend.position = "bottom",
          axis.text.x=element_text(size=20),
          axis.text.y=element_text(size=20),
          axis.title.x = element_text( size = 20),
          axis.title.y = element_text( size = 20),
          title = element_text(size = 20))+
    guides(color=guide_legend(ncol = 2))+
    geom_vline(data=muv, aes(xintercept=grp.mean, color=gear),
               linetype="dashed")
  
# save figure under alphanumeric identifier    
  abc5 = agr_den_all_gears
  
  
### GEAR INDIVIDUALS ####
# filter to gears >2% representation  
  tip_spp_top_gears <- tip_spp |> 
    filter(gear %in% gear_2percent$Gear) 

# create count of records to use in figure labels   
  ycounts_top =tip_spp_top_gears %>% 
    tabyl("gear") %>%
    mutate(n_labels = paste0(gear, " (n= ", n, ")" ))

# calculate means to use in figure    
  muv <- plyr::ddply(tip_spp_top_gears, 
                     "gear", 
                     summarise, 
                     grp.mean=mean(length1_cm))
# view mean values  
  head(muv)
  
# create density plot showing individual gears representing >2% of data   
  agr_den_top_gears <- tip_spp_top_gears %>% 
    ggplot(aes(length1_cm))+
    geom_density(aes(color = gear),
                 linewidth = 0.75)+
    scale_color_hue(labels=ycounts_top$n_labels)+
    labs(color = "Gear" , 
         x = "Fork Length (cm)", 
         title = paste0(print_isl, " ", print_spp, " relevant gears"))+ 
    #title = paste0(county,  "\n (N = ", sum(ycounts$n), ")"))+
    # theme_minimal()
    theme(legend.title = element_text(size=12), 
          legend.text = element_text(size=12),
          legend.position = "bottom",
          axis.text.x=element_text(size=20),
          axis.text.y=element_text(size=20),
          axis.title.x = element_text( size = 20),
          axis.title.y = element_text( size = 20),
          title = element_text(size = 20))+
    guides(color=guide_legend(ncol = 2))+
    geom_vline(data=muv, aes(xintercept=grp.mean, color=gear),
               linetype="dashed")

# save figure under alphanumeric identifier    
  abc6 = agr_den_top_gears

##### if using time break continue, if not: skip to next figure #####  
### top gears after time break #### 
# filter to gears representing >2% of records
  tip_spp_break_gears <- tip_spp_break |> 
      filter(gear %in% gear_2percent_bkyr$Gear) 

# create count of records for figure labels    
  ycounts_bkyr =tip_spp_break_gears %>% 
    tabyl("gear") %>%
    mutate(n_labels = paste0(gear, " (n= ", n, ")" ))

# calculate mean to display on density plot     
  muv_break <- 
    plyr::ddply(tip_spp_break_gears, 
                "gear", summarise, grp.mean=mean(length1_cm))
# view means
    head(muv_break)

# create density graph of gears representing >2% of records after break year       
  agr_den_break <- tip_spp_break_gears %>% 
    ggplot(aes(length1_cm))+
    geom_density(aes(color = gear),linewidth = 0.75)+
    scale_color_hue(labels=ycounts_bkyr$n_labels)+
    labs(color = "Gear" , x = "Fork Length (cm)", title = print_isl)+ 
    #title = paste0(print_isl,  "\n (N = ", sum(ycounts$n), ")"))+
    theme(legend.title = element_text(size=20), 
          legend.text = element_text(size=20),
          legend.position = "bottom",
          axis.text.x=element_text(size=20),
          axis.text.y=element_text(size=20),
          axis.title.x = element_text( size = 20),
          axis.title.y = element_text( size = 20),
          title = element_text(size = 20))+
    guides(color=guide_legend(ncol = 2))+
    geom_vline(data=muv12, aes(xintercept=grp.mean, color=gear),
               linetype="dashed")
  
# save figure under alphanumeric identifier    
  abc7 = agr_den_break
  

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
  labs(x = "Fork Length (cm)", 
       title = paste0(print_isl," ", print_spp,  "\n (N = ", sum(ycounts_all$n), ")"))+
  facet_wrap(~year_labs, ncol = 7)

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
    tip_spp_top_gears %>% 
    group_by(year) %>%
    dplyr::mutate(year_labs = paste0(year, "\n n = ", n())) %>%
    ggplot(aes(length1_cm))+
    geom_density(linewidth = 0.75)+
    # geom_vline(data = fleet_final, aes(xintercept=mean(FL_CM)),
    # linetype="dashed", linewidth=1)+
    #scale_color_manual(values = gearcols, labels = counts$n_labels)+
    # scale_color_hue(labels=fcounts$n_labels)+
    labs(x = "Fork Length (cm)", 
         title = paste0(print_isl, " ", print_spp, "\n (N = ", sum(ycounts_top$n), ")"))+
    facet_wrap(~year_labs, ncol = 7)  

# save figure under alphanumeric identifier    
  abc9 = plot_ann_den_top
  
### top gears separated ####
# fcounts = ann_den_top_gears %>%  group_by(year) %>% filter(n() >= 30) %>% ungroup %>%
#   tabyl(gear) %>%
#   mutate(n_labels = paste0(gear, " (n= ", n, ")" ))

plot_ann_den_separate <-
  tip_spp_top_gears %>% 
  group_by(year) %>%
  dplyr::mutate(year_labs = paste0(year, "\n n = ", n())) %>%
  ggplot(aes(length1_cm, color = gear))+
  geom_density(linewidth = 0.75)+
  #scale_color_manual(values = gearcols, labels = counts$n_labels)+
  scale_color_hue(labels=ycounts_top$n_labels)+
  labs(color = "Gear Type", 
       x = "Fork Length (cm)", 
       title = paste0(print_isl, " ", print_spp, "\n (N = ", sum(ycounts_top$n), ")"))+
  facet_wrap(~year_labs, ncol = 7)+
  # theme_minimal()
  guides(color=guide_legend(ncol = 2))+
  theme(legend.title = element_text(size=14), 
        legend.text = element_text(size=12),
        legend.position = "bottom")

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
    labs(x = "Fork Length (cm)",
         title = paste0(print_isl, " ", print_spp, "\n (N = ", sum(counts$n), ")"))+
    theme_minimal()

# save figure under alphanumeric identifier    
  abc20 = cumulative_den


# SAVE WORKSPACE ####
# workspace needed when pulling values into quarto document  
  save.image(
    file = here::here(
      "tools",
      "figures",
      "flagging_outline",
      paste0(
        isl, "_",
        spp, "_sedar_figures_",
        format(Sys.time(), "%Y%m%d"), ".RData"
      )
    )
  )
  