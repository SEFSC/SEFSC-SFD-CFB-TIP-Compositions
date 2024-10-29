
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

## create table to print  ####
  season_tbl_print <- tip_seasons |> 
    group_by(season)|>
    dplyr::summarize(
      .groups = "drop",
      interviews = n_distinct(id),
    )|>
    mutate(season = case_when(season == "S1" ~ "Jan_Apr",
                              season == "S2" ~ "May_Aug",
                              season == "S3" ~ "Sep_Dec",
                              .default = "not coded"
    ))|> 
    mutate(percent = round(100 * interviews / sum(interviews), 2),)|> 
    select(season, percent)
  
  flextable(season_tbl_print) |>
    theme_box() %>%
    align(align = "center", part = "all") %>%
    fontsize(size = 8, part = "all") %>%
    autofit()

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
      "historic",
      "rds",
      paste0( "pr_time_grouped_tip_",
              format(Sys.time(), "%Y%m%d"), 
              ".rds"
      )
    )
  )
