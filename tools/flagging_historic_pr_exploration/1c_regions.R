# 1c_regions_seasons
# 
# # Load libraries ####
#   librarian::shelf(here, tidyverse, measurements, flextable, ggplot2, reshape2)
# 
# # Specify settings ####
#   tip_pr <- "pr_format_tip_20241021.rds" # add formatted data
# 
# # Read in raw data ####
#   tip <- readRDS(here::here("data", "historic", "rds", tip_pr))


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

## create table to print   #### 
  region_tbl_print <- tip_regions_clean |> 
    group_by(region)|>
    dplyr::summarize(
      .groups = "drop",
      interviews = n_distinct(id),
    ) |> 
    mutate(percent = round(100 * interviews / sum(interviews), 2),) |> 
    select(region, percent)
  
  flextable(region_tbl_print) |>
    theme_box() %>%
    align(align = "center", part = "all") %>%
    fontsize(size = 8, part = "all") %>%
    autofit()

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


# Save formatted tip_spp ####
  saveRDS(
    tip_regions_clean,
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
  