# 04_glmm_analysis

#' remember island is set in 03a script 
#' execute statistical analysis of gear-length relationships (glmm) and
#' k calculations to determine possible gear groupings and data outliers

# Load libraries ####
  librarian::shelf(here, tidyverse, flextable, ggplot2, ggpubr, lmerTest, meantables)

# Specify settings ####
  tip_spp_rds <- "stx_csl_prep_keep_tip_20240920.rds" # rds from end of 03a script
  spp <- "csl"
  isl <- "stx"
  break_year <- 2012
  print_isl <- "St. Croix"
  print_spp <- "Caribbean Spiny Lobster"

  # Read in formatted data ####
  tip_spp <- readRDS(here::here("data", tip_spp_rds))

# Filter to needed variables for GLMM ####
  tip_spp_glm <- tip_spp |>
    select(year, date, id, island, length1_cm, gear) |>
    mutate(id = as.character(id))

# plot data ####
## Create box plot across years ####
  box_plot <- ggboxplot(
    tip_spp_glm,
    x = "gear", y = "length1_cm",
    color = "gear",
  ) +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
      legend.position = "none"
    ) +
    labs(
      x = "Gear", y = "Length(cm)", colour = "", shape = "",
      title = paste(print_isl, "Length Samples")
    )
  box_plot

## Create density plot across years and combined gears ####
  density_plot <- ggdensity(
    tip_spp_glm,
    x = "length1_cm",
    add = "mean", rug = TRUE
    # color = "gear", fill = "gear",
  ) +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 10),
      legend.position = "none"
    ) +
    labs(
      x = "All Gears", y = "Length(cm)", colour = "", shape = "",
      title = paste(print_isl, "Length Samples")
    )
  
# view density plot   
  density_plot
  
# Create density plot across years with mean line for each gear 
  density_plot_gear <- ggdensity(
    tip_spp_glm,
    x = "length1_cm",
    add = "mean", rug = TRUE,
    color = "gear", fill = "gear",
  ) +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 10),
      legend.position = "bottom"
    ) +
    labs(
      x = "All Gears", y = "Length(cm)", colour = "", shape = "",
      title = paste(print_isl, print_spp, "Length Samples")
    )
  
# view density plot  
  density_plot_gear

## Scatter plot with line of smoothed conditional mean ####
  # takes forever to load
#' filtered to gears with 30 or more occurrences for the purposes
#' of plotting visibility
  tip_spp_glm_filtered <- tip_spp_glm %>%
    group_by(gear) %>%
    filter(n() >= 30) %>%
    ungroup() 
  options(repr.plot.width = 5, repr.plot.height =2) 
  allgears_glm_plot <- tip_spp_glm_filtered |> 
    ggplot(aes(x = date, y = length1_cm)) +
    geom_point(aes(colour = gear, shape = gear), size = 1, alpha = 0.5) +
    scale_shape_manual(values = c(
      0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
      13, 14, 15, 16, 17, 18, 21, 22, 23
    )) +
    geom_smooth(method = "lm", formula = "y ~ x", col = "black") +
    # facet_wrap(~ COUNTY_LANDED) +
    labs(x = "", y = "Length (cm)", colour = "", shape = "") +
    theme_bw() +
    theme(
      legend.position = "bottom", legend.text = element_text(size = 7),
      legend.box.spacing = unit(0, "npc"), panel.grid = element_blank()
    ) +
    guides(colour = guide_legend(override.aes = list(size = 2)))
  allgears_glm_plot
  
# GLMM model analysis ####
# Comparing length to date and gear in a gamma full model 
# takes a long time to run 
  mod2 <- glmer(length1_cm ~ scale(date) + gear + (1 | year) + (1 | id),
                data = tip_spp_glm,
                family = Gamma(link = log)
  )

## pairwise comparisons -  ####
# compares each gear to each other and gives p value
  mod_contr <- emmeans::emmeans(
    object = mod2,
    pairwise ~ "gear",
    adjust = "tukey"
  )

# cld provides gear groupings based on which gears are
# similar vs significantly different from each other
  allgears_multcompcld <- multcomp::cld(object = mod_contr$emmeans)

# Count number of length records (aka number of fish measured)
  length_data_fishcount <- tip_spp |>
    group_by(gear) |>
    tally()
# Count number of unique trip interviews (aka unique id)
  length_data_tripcount <- aggregate(
    data = tip_spp, # Applying aggregate
    id ~ gear,
    function(id) length(unique(id))
  )

# Add counts to GLMM table
  allgears_multcompcld_fish <- full_join(allgears_multcompcld,
    length_data_fishcount,
    by = "gear"
  )
  allgears_multcompcld_trip <- full_join(allgears_multcompcld_fish,
    length_data_tripcount,
    by = "gear"
  )

## create table of stats ####
# Clean table of GLMM and summary stats and filter to gears with more than 3
# unique interviews
  allgears_multcompcld_final <- allgears_multcompcld_trip |>
    mutate(
      Percentage = round(n / sum(n) * 100, 2),
      emmean = round(emmean, 2),
      asymp.LCL = round(asymp.LCL, 2),
      asymp.UCL = round(asymp.UCL, 2)
    ) |>
    dplyr::rename(
      "Group" = ".group",
      "Gear" = "gear",
      "Estimated Marginal Mean" = "emmean",
      "LCL" = "asymp.LCL",
      "UCL" = "asymp.UCL",
      "Fish(n)" = "n",
      "Interview(n)" = "id"
    ) |>
    # dplyr::filter(`Interview(n)` >= 3) |>
    select(
      Gear,
      "Estimated Marginal Mean",
      LCL,
      UCL,
      Group,
      "Fish(n)",
      "Interview(n)",
      Percentage
    ) 
# add groupings based off glmm groupings if requested 
    ### UPDATE THE GEAR GROUPINGS ###
    # mutate("Gear Group" = case_when(
    #   Gear == "LINES HAND" ~ "Hand Line",
    #   Gear == "POTS AND TRAPS; FISH" ~ "Traps",
    #   Gear == "HAUL SEINES" ~ "Hand Line or Traps",
    #   Gear == "POTS AND TRAPS; CMB" ~ "Hand Line or Traps",
    #   Gear == "POTS AND TRAPS;SPINY LOBSTER" ~ "Hand Line or Traps",
    #   Gear == "ROD AND REEL" ~ "Rod and Reel",
    #   TRUE ~ "Hand Line, Traps, or Rod and Reel"
    # ))

# Create table of means for each gear
  mean_allgears <- tip_spp %>%
    group_by(gear) %>%
    dplyr::mutate(n_ID = n_distinct(id)) |>
    dplyr::filter(n_ID >= 3) |>
    mean_table(length1_cm) |>
    dplyr::rename(
      "Gear" = "group_cat",
      "Mean" = "mean"
    ) |>
    select(Gear, Mean)

# Join means to GLMM/summary stats table
  allgears_multcom_mean <- full_join(mean_allgears,
    allgears_multcompcld_final,
    by = "Gear"
  )

# Arrange table in decreasing order of gear percent representation
  allgears_multcom_mean_final <- allgears_multcom_mean |>
    arrange(desc(Percentage))
  # dplyr::filter(`Interview(n)` >= 3)

# format table for export
  glm_tbl <- flextable(allgears_multcom_mean_final) |>
    theme_box() %>%
    align(align = "center", part = "all") %>%
    fontsize(size = 8, part = "all") %>%
    autofit()

# list of gears that represent >2% of lengths each
  grtr2percent_gears <- allgears_multcom_mean_final |>
    filter(Percentage > 2)

## Save gears >2% representation ####
  saveRDS(
    grtr2percent_gears,
    file = here::here(
      "data",
      paste0(
        isl, "_",
        spp, "_clean_gear_list_",
        format(Sys.time(), "%Y%m%d"), ".rds"
      )
    )
  )
  
#### IF NOT USING BREAK YEAR, SKIP TO END AND SAVE WORKSPACE #####  

# Repeat GLMM with data after data break year ####
# filter to break year and select needed variables 
  tip_spp_break_year <- tip_spp |>
    filter(year >= break_year) |>
    select(year, interview_date, id, island, length1_cm, gear)

# plot filtered data
  allgears_glm_plot_break <- tip_spp_break_year |>
    ggplot(aes(x = as.Date(interview_date), y = length1_cm)) +
    geom_point(aes(colour = gear, shape = gear), size = 1, alpha = 0.5) +
    geom_smooth(method = "lm", formula = "y ~ x", col = "black") +
    # facet_wrap(~ COUNTY_LANDED) +
    labs(x = "", y = "Length (cm)", colour = "", shape = "") +
    theme_bw() +
    theme(
      legend.position = "bottom", legend.text = element_text(size = 15),
      legend.box.spacing = unit(0, "npc"), panel.grid = element_blank()
    ) +
    guides(colour = guide_legend(override.aes = list(size = 2)))


## fit models ####
# comparing length to date and gear in a gamma full model
  mod2 <- glmer(FL_CM ~ scale(FINAL_DATE) + gear + (1 | YEAR) + (1 | ID),
    data = tip_spp_break_year, family = Gamma(link = log)
  )

# pairwise comparisons (if needed)
  mod_contr <- emmeans::emmeans(
    object = mod2,
    pairwise ~ "gear",
    adjust = "tukey"
  )
  
# cld provides gear groupings based on which gears are
# similar vs significantly different from each other
  allgears_multcompcld_break <- multcomp::cld(object = mod_contr$emmeans)

# Count number of length records (aka number of fish measured)
  length_data_fishcount_break <- tip_spp_break_year |>
    group_by(gear) |>
    tally()
# Count number of unique trip interviews (aka unique id)  
  length_data_tripcount_break <- aggregate(
    data = tip_spp_break_year, # Applying aggregate
    ID ~ gear,
    function(id) length(unique(id))
  )

# Add counts to GLMM table    
  allgears_multcompcld_fish_break <-
    full_join(allgears_multcompcld_break,
      tip_spp_break_year,
      by = "gear"
    )
  allgears_multcompcld_trip_break <-
    full_join(allgears_multcompcld_fish_break,
      length_data_tripcount_break,
      by = "gear"
    )

## create table of stats ####
  allgears_multcompcld_finaL_break <- allgears_multcompcld_trip_break |>
    mutate(
      Percentage = round(n / sum(n) * 100, 2),
      emmean = round(emmean, 2),
      asymp.LCL = round(asymp.LCL, 2),
      asymp.UCL = round(asymp.UCL, 2)
    ) |>
    dplyr::rename(
      "Group" = ".group",
      "Gear" = "gear",
      "Estimated Marginal Mean" = "emmean",
      "LCL" = "asymp.LCL",
      "UCL" = "asymp.UCL",
      "Fish(n)" = "n",
      "Interview(n)" = "id"
    ) |>
    # filter("Interview(n)" >= 3) |>
    dplyr::filter(`Interview(n)` >= 3) |>
    select(
      Gear,
      "Estimated Marginal Mean",
      LCL,
      UCL,
      Group,
      "Fish(n)",
      "Interview(n)",
      Percentage
    ) 
# add groupings based off glmm groupings if requested 
  ### UPDATE THE GROUPINGS ###
    # mutate("Gear Group" = case_when(
    #   Gear == "LINES HAND" ~ "Hand Line",
    #   Gear == "POTS AND TRAPS; FISH" ~ "Traps",
    #   Gear == "HAUL SEINES" ~ "Hand Line or Traps",
    #   Gear == "POTS AND TRAPS; CMB" ~ "Hand Line or Traps",
    #   Gear == "POTS AND TRAPS;SPINY LOBSTER" ~ "Hand Line or Traps",
    #   Gear == "ROD AND REEL" ~ "Rod and Reel",
    #   TRUE ~ "Hand Line, Traps, or Rod and Reel"
    # ))

# create table of means for each gear
  mean_allgears_break <- tip_spp_break_year %>%
    group_by(gear) %>%
    dplyr::mutate(n_ID = n_distinct(ID)) |>
    dplyr::filter(n_ID >= 3) |>
    mean_table(FL_CM) |>
    dplyr::rename(
      "Gear" = "group_cat",
      "Mean" = "mean"
    ) |>
    select(Gear, Mean)
# add means to table  
  allgears_multicom_mean_break <- full_join(mean_allgears_break, 
                                          allgears_multcompcld_finaL_break, 
                                          by = "Gear")
# sort table   
  allgears_multicom_mean_break_final <- allgears_multicom_mean_break |>
    arrange(desc(Percentage))
  
# format table
  glm_tlb2 <- flextable(allgears_multicom_mean_break_final) |>
    theme_box() %>%
    align(align = "center", part = "all") %>%
    fontsize(size = 8, part = "all") %>%
    autofit()

# list of gears that represent >2% of lengths each
  grtr2percent_gears_break <- allgears_multicom_mean_break_final |>
    filter(Percentage > 2)

## Save gears >2% representation ####
  saveRDS(
    grtr2percent_gears_break,
    file = here::here(
      "data",
      paste0(
        isl, "_",
        spp, "_clean_gear_list_break_year_",
        format(Sys.time(), "%Y%m%d"), ".rds"
      )
    )
  )

  
# SAVE WORKSPACE ####
  # workspace needed when pulling values into quarto document  
  save.image(
    file = here::here(
      "tools",
      "figures",
      "flagging_outline",
      paste0(
        isl, "_",
        spp, "_sedar_glmm_",
        format(Sys.time(), "%Y%m%d"), ".RData"
    )
  )
  )
  