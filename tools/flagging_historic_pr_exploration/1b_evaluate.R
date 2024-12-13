# 1b_overview

# # Load libraries ####
#   librarian::shelf(here, tidyverse, measurements, flextable, ggplot2, reshape2)
# 
# # Specify settings ####
#   tip_pr <- "pr_format_tip_20241021.rds" # add formatted data
# 
# # Read in raw data ####
#   tip <- readRDS(here::here("data", "historic", "rds", tip_pr))

# Set up data ####
## Convert units and calculate k ####
  size_calc <- tip_spp_relevant |>
    dplyr::mutate(
      length1_cm = measurements::conv_unit(length1_mm, "mm", "cm"),
      length1_inch = measurements::conv_unit(length1_mm, "mm", "inch"),
      obs_weight_lbs = measurements::conv_unit(obs_weight_kg, "kg", "lbs"),
      k = 10^5 * obs_weight_kg / length1_cm^3,
      record_type = case_when(
        !is.na(k) ~ "complete",
        .default = "incomplete"
      )
    )

## Group into 5 yr groups
  tip_yr_groups <- size_calc |>
    mutate(st_yr = case_when(year < 1985 ~ "1980",
      year > 1984 & year < 1990 ~ "1985",
      year > 1989 & year < 1995 ~ "1990",
      year > 1994 & year < 2000 ~ "1995",
      year > 1999 & year < 2005 ~ "2000",
      year > 2004 & year < 2010 ~ "2005",
      year > 2009 & year < 2015 ~ "2010",
      year > 2014 & year < 2020 ~ "2015",
      year > 2019 ~ "2020",
      .default = "not coded"
    ), )
  # tip_yr_groups$st_yr <- str_sub(tip_yr_groups$yr_group, 3)

## overview stats ####
  tip_yr_summary <- tip_yr_groups |>
    group_by(st_yr) |>
    summarise(
      .groups = "drop",
      n = dplyr::n(),
      n_id = n_distinct(id),
      len_q25 = quantile(length1_cm, 0.25, na.rm = TRUE),
      len_q75 = quantile(length1_cm, 0.75, na.rm = TRUE),
      min_length_cm = min(length1_cm, na.rm = TRUE),
      max_length_cm = max(length1_cm, na.rm = TRUE),
      avg_length_cm = round(mean(length1_cm, na.rm = TRUE), 2),
      na_length_cm = sum(is.na(length1_cm)),
    )

## plot number of interviews #### 
  plot_int_count <- tip_yr_summary |>
    ggplot(aes(x = st_yr, y = n_id)) +
    geom_point(size = 3) +
    # ggplot2::facet_grid(record_type ~ island) +
    labs(
      x = "Start year",
      y = "# unique interviews",
      title = "Number of unique interviews per 5 year group in Puerto Rico"
    ) +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      # legend.title = element_text(size = 12),
      legend.text = element_text(size = 10),
      text = element_text(size = 12),
    )

# view 
  plot_int_count
  
  ggsave(filename = here::here("data", "historic", "figure", "all", "plot_int_count.png"))
  
# Save formatted tip_spp ####
  saveRDS(
    tip_yr_groups,
    file = here::here(
      "data",
      "historic",
      "rds",
      paste0( "pr_time_grouped_tip_",
              format(Sys.time(), "%Y%m%d"), 
              ".rds"
      )
    )
  )
