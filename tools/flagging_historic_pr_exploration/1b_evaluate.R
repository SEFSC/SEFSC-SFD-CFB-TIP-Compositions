# 1b_evaluate

# Load libraries ####
  librarian::shelf(here, tidyverse, measurements, flextable, ggplot2, reshape2)

# Specify settings ####
  tip_pr <- "pr_format_tip_20240822.rds" # add formatted data

# Read in raw data ####
  tip <- readRDS(here::here("data", tip_pr))

# Set up data ####
## Convert units and calculate k ####
  size_calc <- tip |>
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
    geom_point() +
    # ggplot2::facet_grid(record_type ~ island) +
    labs(
      x = "Start year",
      y = "# unique interviews",
      title = "Number of unique interviews per 5 year group in Puerto Rico"
    ) +
    theme(
      legend.position = "bottom",
      legend.title = element_blank()
    )

# Region designation ####
## read in muni region table ####
  pr_muni_code <-
    read_csv("data/CSVs/updated_pr_place_codes_oracle.csv") |>
    janitor::clean_names()

## correct muni name spelling ####  
  tip_guayanilla_fix <- tip_yr_groups |>
    mutate(
      county = case_when(
        county_landed == "GUAYAMILLA" ~ "GUAYANILLA",
        TRUE ~ county_landed
      )
    )
  
## add regions to dataframe ####  
  tip_regions <- tip_guayanilla_fix |>
    mutate(
      region =
        pr_muni_code$region[match(
          tip_guayanilla_fix$county,
          pr_muni_code$cnty_name
        )]
    )

## check for na's  ####
  tip_regions_na <- tip_regions |>
    filter(is.na(region))
  unique(tip_regions_na$county_landed)
  
## filter out na's ####
  tip_regions_clean <- tip_regions |> 
    filter(!is.na(region))
  
# Create count of total observed unique interviews for each region  ####
  tip_regions_id_count <- tip_regions_clean |>
    group_by(region)|>
    dplyr::summarize(
      .groups = "drop",
      region_id = n_distinct(id),
    )
  
# Tabulate TIP interviews and records by year grouping and region ####
## count all records and interviews ####
  count_all <- tip_regions_clean |>
    dplyr::group_by(st_yr) |>
    dplyr::summarize(
      .groups = "drop",
      all_interviews = n_distinct(id),
      all_records = n()
    )

## count only specific region ####
  count_w <- tip_regions_clean |>
    dplyr::filter(region %in% "West") |>
    dplyr::group_by(st_yr) |>
    dplyr::summarize(
      .groups = "drop",
      west_interviews = n_distinct(id),
      west_records = n()
    )
  
  count_s <- tip_regions_clean |>
    dplyr::filter(region %in% "South") |>
    dplyr::group_by(st_yr) |>
    dplyr::summarize(
      .groups = "drop",
      south_interviews = n_distinct(id),
      south_records = n()
    )
  
  count_n <- tip_regions_clean |>
    dplyr::filter(region %in% "North") |>
    dplyr::group_by(st_yr) |>
    dplyr::summarize(
      .groups = "drop",
      north_interviews = n_distinct(id),
      north_records = n()
    )
  
  count_e <- tip_regions_clean |>
    dplyr::filter(region %in% "East") |>
    dplyr::group_by(st_yr) |>
    dplyr::summarize(
      .groups = "drop",
      east_interviews = n_distinct(id),
      east_records = n()
    )
  
  
## summarize both counts ####
  count_overview <- count_all |>
    dplyr::left_join(count_w, by = join_by(st_yr)) |>
    dplyr::left_join(count_s, by = join_by(st_yr)) |>
    dplyr::left_join(count_n, by = join_by(st_yr)) |>
    dplyr::left_join(count_e, by = join_by(st_yr)) |>
    dplyr::mutate(
      across(everything(), ~ replace_na(.x, 0))
    ) |>
    tidyr::pivot_longer(
      cols = !c(st_yr),
      names_to = c("subset", "category"),
      names_sep = "_",
      values_to = "count"
    )

## calculate percent region of each year grouping ####
  percent_overview <- count_overview |>
    tidyr::pivot_wider(
      names_from = subset,
      values_from = count
    ) |>
    dplyr::mutate(
      west_percent = round(100 * west / all, 2),
      south_percent = round(100 * south / all, 2),
      north_percent = round(100 * north / all, 2),
      east_percent = round(100 * east / all, 2),
    )

## count year grouping based on sector ####
  count_sector <- tip_regions_clean |>
    dplyr::filter(
      length_type1 != "NO LENGTH",
    ) |>
    dplyr::group_by(st_yr, sector, length_type1) |>
    dplyr::summarize(
      .groups = "drop",
      records = n()
    )

# plot number of interviews by region ####
## format dataframe to interview specific variables   
  percent_int <- percent_overview |>
    filter(category == "interviews", ) |>
    select(st_yr, west_percent, north_percent, east_percent, south_percent)
  
## rotate table to plot easier   
  percent_plot <- melt(percent_int, 
                       id.vars = "st_yr", 
                       variable.name = "region"
                       ) |>
    rename(percent = value) |>
    mutate(region_name = case_when(
      region == "west_percent" ~ "West",
      region == "north_percent" ~ "North",
      region == "east_percent" ~ "East",
      region == "south_percent" ~ "South",
      .default = "not coded"
    ))  
  
## add counts to table    
  percent_plot_count <- percent_plot |> 
    dplyr::mutate(
      region_count =
        tip_regions_id_count$region_id[match(
          percent_plot$region_name,
          tip_regions_id_count$region
        )]) |> 
    dplyr::mutate(region_id = paste0(region_name, " (", region_count, ")"))

## plot ####  
  plot_int_region <- percent_plot_count |>
    ggplot(aes(x = st_yr, y = percent, group = region_id)) +
    geom_line(aes(color = region_id)) +
    geom_point(aes(color = region_id)) +
    # ggplot2::facet_grid(record_type ~ island) +
    labs(
      color = "Region (# interviews)",
      x = "Start Year",
      y = "Percent Representation", 
      title = "Puerto Rico Region Representation of Each 5 Year Grouping"
    ) +
    theme(
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10),
      legend.position = "bottom",
      # legend.title = element_blank()
    )+
    guides(col = guide_legend(title.position = "top",title.hjust =0.5)) 

# Seasons analysis ####
## designate 4 month seasons ####
  tip_seasons <- tip_regions_clean |> 
    mutate(
      mon = month(date),
      season = case_when(mon %in% c(1, 2, 3, 4) ~ "S1",
                         mon %in% c(5, 6, 7, 8) ~ "S2",
                         mon %in% c(9, 10, 11, 12) ~ "S3",
                         .default = "not coded"
                         )
      )

# Create count of total observed unique interviews for each season  ####
  tip_seasons_id_count <- tip_seasons |>
    group_by(season)|>
    dplyr::summarize(
      .groups = "drop",
      season_count = n_distinct(id),
    )|>
    mutate(season_name = case_when(season == "S1" ~ "Jan_Apr",
                                   season == "S2" ~ "May_Aug",
                                   season == "S3" ~ "Sep_Dec",
                                   .default = "not coded"
    ))|> 
    dplyr::mutate(season_id = paste0(season_name, " (", season_count, ")"))

## calculate percent representation by season by region  
## count all records and interviews ####
  count_all_season <- tip_seasons |>
    dplyr::group_by(st_yr, region) |>
    dplyr::summarize(
      .groups = "drop",
      all_interviews = n_distinct(id),
      all_records = n()
    )
  
## count only specific seasons by region ####
  count_S1 <- tip_seasons |>
    dplyr::filter(season %in% "S1") |>
    dplyr::group_by(st_yr, region) |>
    dplyr::summarize(
      .groups = "drop",
      S1_interviews = n_distinct(id),
      S1_records = n()
    )
  
  count_S2 <- tip_seasons |>
    dplyr::filter(season %in% "S2") |>
    dplyr::group_by(st_yr, region) |>
    dplyr::summarize(
      .groups = "drop",
      S2_interviews = n_distinct(id),
      S2_records = n()
    )
  
  count_S3 <- tip_seasons |>
    dplyr::filter(season %in% "S3") |>
    dplyr::group_by(st_yr, region) |>
    dplyr::summarize(
      .groups = "drop",
      S3_interviews = n_distinct(id),
      S3_records = n()
    )
  
  
## summarize both counts ####
  count_overview_season <- count_all_season |>
    dplyr::left_join(count_S1, by = join_by(st_yr, region)) |>
    dplyr::left_join(count_S2, by = join_by(st_yr, region)) |>
    dplyr::left_join(count_S3, by = join_by(st_yr, region)) |>
    dplyr::mutate(
      across(everything(), ~ replace_na(.x, 0))
    ) |>
    tidyr::pivot_longer(
      cols = !c(st_yr, region),
      names_to = c("subset", "category"),
      names_sep = "_",
      values_to = "count"
    )
  
## calculate percent region of each year grouping ####
  percent_overview_season <- count_overview_season |>
    tidyr::pivot_wider(
      names_from = subset,
      values_from = count
    ) |>
    dplyr::mutate(
      s1_percent = round(100 * S1 / all, 2),
      s2_percent = round(100 * S2 / all, 2),
      s3_percent = round(100 * S3 / all, 2)
    ) 
  
# plot number of interviews by region ####
## format dataframe to interview specific variables   
  percent_int_season <- percent_overview_season |>
    filter(category == "interviews", ) |>
    select(st_yr, region, s1_percent, s2_percent, s3_percent)

  percent_season_plot <- melt(percent_int_season, 
                              id.vars = c("st_yr", "region") , 
                              variable.name = "season"
                              ) |>
    rename(percent = value) |>
    mutate(season_name = case_when(season == "s1_percent" ~ "Jan_Apr",
                                   season == "s2_percent" ~ "May_Aug",
                                   season == "s3_percent" ~ "Sep_Dec",
                                   .default = "not coded"
    ))
  
## add season counts to table    
  percent_season_plot_count <- percent_season_plot |> 
    dplyr::mutate(
      season_id =
        tip_seasons_id_count$season_id[match(
          percent_season_plot$season_name,
          tip_seasons_id_count$season_name
        )])
  
## add region counts to table    
  tip_season_region_count_clean <- percent_season_plot_count |> 
    dplyr::mutate(
      region_count =
        tip_regions_id_count$region_id[match(
          percent_season_plot_count$region,
          tip_regions_id_count$region
        )]) |> 
    dplyr::mutate(region_id = paste0(region, " (", region_count, ")"))
  
  
## plot ####  
  plot_int_season <- tip_season_region_count_clean |>
    ggplot(aes(x = st_yr, y = percent, group = season_id)) +
    geom_line(aes(color = season_id)) +
    geom_point(aes(color = season_id)) +
    ggplot2::facet_wrap( ~ region_id) +
    labs(
      color = "Season",
      x = "Start Year",
      title = "Interview Count by Region and Season"
    ) +
    theme(
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.position = "right",
      # legend.title = element_blank()
    )+
    guides(col = guide_legend(title.position = "top",title.hjust =0.5)) 
  
  
  
# Save formatted tip_spp ####
  saveRDS(
    tip_seasons,
    file = here::here(
      "data",
      paste0( "pr_time_grouped_tip_",
              format(Sys.time(), "%Y%m%d"), 
              ".rds"
      )
    )
  )
  