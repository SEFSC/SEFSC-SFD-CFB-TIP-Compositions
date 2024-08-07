# 2a_gears

# Load libraries ####
librarian::shelf(here, tidyverse, measurements, flextable, ggplot2, reshape2)

# Specify settings ####
tip_pr <- "pr_time_grouped_tip_20240807.rds" # add formatted data

# Read in raw data ####
tip <- readRDS(here::here("data", tip_pr))

# read in gear group table ####
gear_group <- 
  read_csv("~/SEFSC-SFD-CFB-TIP-Compositions/data/CSVs/gear_groups_pr.csv")|> 
  janitor::clean_names()

## attach gear groups to records ####
tip_gear_group <- tip %>%
  mutate(
    gear_group =
      gear_group$major_gear_group[match(
        tip$gear,
        gear_group$gear_name
      )]
  )

# Tabulate TIP interviews and records by gear grouping ####
## count all records and interviews ####
gear_group_count <- tip_gear_group |>
  dplyr::group_by(gear_group) |>
  dplyr::summarize(
    .groups = "drop",
    all_interviews = n_distinct(id),
    all_records = n()
  )

# check for na's
tip_gear_group_na <- tip_gear_group |> 
  filter(gear_group == "na")
unique(tip_gear_group_na$gear)

# remove not coded records (na)
tip_gear_clean <- tip_gear_group |> 
  filter(gear_group != "na")

# count gear groups by year group
gear_group_count_notna <- tip_gear_clean |>
  dplyr::group_by(st_yr, gear_group) |>
  dplyr::summarize(
    .groups = "drop",
    all_interviews = n_distinct(id),
    all_records = n()
  )

## plot number of interviews #### 
plot_gear_int_count <- gear_group_count_notna |>
  ggplot(aes(x = st_yr, y = all_interviews, group = gear_group)) +
  geom_line(aes(color = gear_group)) +
  geom_point(aes(color = gear_group)) +
  # ggplot2::facet_grid(record_type ~ island) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )

# Percentage calculation of gear by region  ####
## count all records and interviews ####
gear_count_all <- tip_gear_clean |>
  dplyr::group_by(st_yr, region) |>
  dplyr::summarize(
    .groups = "drop",
    all_interviews = n_distinct(id),
    all_records = n()
  )

## count only specific region ####
count_h <- tip_gear_clean |>
  dplyr::filter(gear_group %in% "hook-line") |>
  dplyr::group_by(st_yr, region) |>
  dplyr::summarize(
    .groups = "drop",
    h_interviews = n_distinct(id),
    h_records = n()
  )

count_t <- tip_gear_clean |>
  dplyr::filter(gear_group %in% "trap") |>
  dplyr::group_by(st_yr, region) |>
  dplyr::summarize(
    .groups = "drop",
    t_interviews = n_distinct(id),
    t_records = n()
  )

count_d <- tip_gear_clean |>
  dplyr::filter(gear_group %in% "diving") |>
  dplyr::group_by(st_yr, region) |>
  dplyr::summarize(
    .groups = "drop",
    d_interviews = n_distinct(id),
    d_records = n()
  )

count_n <- tip_gear_clean |>
  dplyr::filter(gear_group %in% "net") |>
  dplyr::group_by(st_yr, region) |>
  dplyr::summarize(
    .groups = "drop",
    n_interviews = n_distinct(id),
    n_records = n()
  )

## summarize both counts ####
gear_count_overview <- gear_count_all |>
  dplyr::left_join(count_h, by = join_by(st_yr, region)) |>
  dplyr::left_join(count_t, by = join_by(st_yr, region)) |>
  dplyr::left_join(count_d, by = join_by(st_yr, region)) |>
  dplyr::left_join(count_n, by = join_by(st_yr, region)) |>
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
gear_percent_overview <- gear_count_overview |>
  tidyr::pivot_wider(
    names_from = subset,
    values_from = count
  ) |>
  dplyr::mutate(
    h_percent = round(100 * h / all, 2),
    t_percent = round(100 * t / all, 2),
    d_percent = round(100 * d / all, 2),
    n_percent = round(100 * n / all, 2),
  )

# plot number of interviews by region ####
## format dataframe to interview specific variables   
percent_gear_int <- gear_percent_overview |>
  filter(category == "interviews", ) |>
  select(st_yr, region, h_percent, t_percent, d_percent, n_percent)

percent_gear_plot <- melt(percent_gear_int, 
                     id.vars = c("st_yr", "region"), 
                     variable.name = "gear"
                     ) |>
  rename(percent = value) |>
  mutate(gear_name = case_when(
    gear == "h_percent" ~ "Hook-line",
    gear == "t_percent" ~ "Trap",
    gear == "d_percent" ~ "Diving",
    gear == "n_percent" ~ "Net",
    .default = "not coded"),
    region_name = case_when(
      region == "W" ~ "West",
      region == "N" ~ "North",
      region == "E" ~ "East",
      region == "S" ~ "South",
      .default = "not coded"
      ))

## plot ####  
plot_int_gear <- percent_gear_plot |>
  ggplot(aes(x = st_yr, y = percent, group = gear_name)) +
  geom_line(aes(color = gear_name)) +
  geom_point(aes(color = gear_name)) +
  ggplot2::facet_wrap( ~ region_name) +
  labs(
    color = "Gear Type",
    x = "Fork Length (cm)",
    title = "Interview Count by Region and Gear"
  ) +
  theme(
    # legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.position = "bottom",
    legend.title = element_blank()
  )

# Save formatted tip_spp ####
saveRDS(
  tip_gear_clean,
  file = here::here(
    "data",
    paste0( "pr_gear_grouped_tip_",
            format(Sys.time(), "%Y%m%d"), 
            ".rds"
    )
  )
)
