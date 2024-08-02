# 1b_evaluate

# Load libraries ####
  librarian::shelf(here, tidyverse, measurements, flextable, ggplot2, reshape2)

# Specify settings ####
  tip_pr <- "pr_format_tip_20240731.rds" # add formatted data

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
    mutate(yr_group = case_when(year < 1985 ~ "g_1980",
      year > 1984 & year < 1990 ~ "g_1985",
      year > 1989 & year < 1995 ~ "g_1990",
      year > 1994 & year < 2000 ~ "g_1995",
      year > 1999 & year < 2005 ~ "g_2000",
      year > 2004 & year < 2010 ~ "g_2005",
      year > 2009 & year < 2015 ~ "g_2010",
      year > 2014 & year < 2020 ~ "g_2015",
      year > 2019 ~ "g_2020",
      .default = "not coded"
    ), )
  tip_yr_groups$st_yr <- str_sub(tip_yr_groups$yr_group, 3)

## overview stats ####
  tip_yr_summary <- tip_yr_groups |>
    group_by(yr_group) |>
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

## check for na's  
  tip_regions_na <- tip_regions |>
    filter(is.na(region))

# Tabulate TIP interviews and records by year grouping and region ####
## count all records and interviews ####
  count_all <- tip_regions |>
    dplyr::group_by(st_yr) |>
    dplyr::summarize(
      .groups = "drop",
      all_interviews = n_distinct(id),
      all_records = n()
    )

## count only specific region ####
  count_w <- tip_regions |>
    dplyr::filter(region %in% "W") |>
    dplyr::group_by(st_yr) |>
    dplyr::summarize(
      .groups = "drop",
      w_interviews = n_distinct(id),
      w_records = n()
    )
  
  count_s <- tip_regions |>
    dplyr::filter(region %in% "S") |>
    dplyr::group_by(st_yr) |>
    dplyr::summarize(
      .groups = "drop",
      s_interviews = n_distinct(id),
      s_records = n()
    )
  
  count_n <- tip_regions |>
    dplyr::filter(region %in% "N") |>
    dplyr::group_by(st_yr) |>
    dplyr::summarize(
      .groups = "drop",
      n_interviews = n_distinct(id),
      n_records = n()
    )
  
  count_e <- tip_regions |>
    dplyr::filter(region %in% "E") |>
    dplyr::group_by(st_yr) |>
    dplyr::summarize(
      .groups = "drop",
      e_interviews = n_distinct(id),
      e_records = n()
    )
  
  count_c <- tip_regions |>
    dplyr::filter(region %in% "C") |>
    dplyr::group_by(st_yr) |>
    dplyr::summarize(
      .groups = "drop",
      c_interviews = n_distinct(id),
      c_records = n()
    )
  
## summarize both counts ####
  count_overview <- count_all |>
    dplyr::left_join(count_w, by = join_by(st_yr)) |>
    dplyr::left_join(count_s, by = join_by(st_yr)) |>
    dplyr::left_join(count_n, by = join_by(st_yr)) |>
    dplyr::left_join(count_e, by = join_by(st_yr)) |>
    dplyr::left_join(count_c, by = join_by(st_yr)) |>
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
      w_percent = round(100 * w / all, 2),
      s_percent = round(100 * s / all, 2),
      n_percent = round(100 * n / all, 2),
      e_percent = round(100 * e / all, 2),
      c_percent = round(100 * c / all, 2),
    )

## count year grouping based on sector ####
  count_sector <- tip_regions |>
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
    select(st_yr, w_percent, n_percent, e_percent, s_percent, c_percent)
  percent_plot <- melt(percent_int, 
                       id.vars = "st_yr", 
                       variable.name = "region"
                       ) |>
    rename(percent = value) |>
    mutate(region_name = case_when(
      region == "w_percent" ~ "West",
      region == "n_percent" ~ "North",
      region == "e_percent" ~ "East",
      region == "s_percent" ~ "South",
      region == "c_percent" ~ "Central",
      .default = "not coded"
    ))

## plot ####  
  plot_int_region <- percent_plot |>
    ggplot(aes(x = st_yr, y = percent, group = region_name)) +
    geom_line(aes(color = region_name)) +
    geom_point(aes(color = region_name)) +
    # ggplot2::facet_grid(record_type ~ island) +
    labs(
      color = "Gear Type",
      x = "Fork Length (cm)",
      title = "Interview Count by Region"
    ) +
    theme(
      # legend.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.position = "bottom",
      legend.title = element_blank()
    )

# Seasons analysis ####
## designate 4 month seasons ####
  tip_seasons <- tip_regions |> 
    mutate(
      mon = month(date),
      season = case_when(mon %in% c(1, 2, 3, 4) ~ "S1",
                         mon %in% c(5, 6, 7, 8) ~ "S2",
                         mon %in% c(9, 10, 11, 12) ~ "S3",
                         .default = "not coded"
                         )
      )

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
    ) |>
    mutate(region_name = case_when(
      region == "W" ~ "West",
      region == "N" ~ "North",
      region == "E" ~ "East",
      region == "S" ~ "South",
      region == "C" ~ "Central",
      .default = "not coded"
    ))
  
# plot number of interviews by region ####
## format dataframe to interview specific variables   
  percent_int_season <- percent_overview_season |>
    filter(category == "interviews", ) |>
    select(st_yr, region_name, s1_percent, s2_percent, s3_percent)
  percent_season_plot <- melt(percent_int_season, 
                              id.vars = c("st_yr", "region_name") , 
                              variable.name = "season"
                              ) |>
    rename(percent = value) |>
    mutate(season_name = case_when(season == "s1_percent" ~ "Jan_Apr",
                                   season == "s2_percent" ~ "May_Aug",
                                   season == "s3_percent" ~ "Sep_Dec",
                                   .default = "not coded"
    ))
  
  ## plot ####  
  plot_int_season <- percent_season_plot |>
    ggplot(aes(x = st_yr, y = percent, group = season_name)) +
    geom_line(aes(color = season_name)) +
    geom_point(aes(color = season_name)) +
    ggplot2::facet_wrap( ~ region_name) +
    labs(
      color = "Gear Type",
      x = "Fork Length (cm)",
      title = "Interview Count by Region and Season"
    ) +
    theme(
      # legend.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.position = "bottom",
      legend.title = element_blank()
    )
  