# 2a_gears

# Load libraries ####
librarian::shelf(here, tidyverse, measurements, flextable, ggplot2, reshape2)

# Specify settings ####
tip_pr <- "pr_time_grouped_tip_20240822.rds" # add formatted data

# Read in raw data ####
tip <- readRDS(here::here("data", tip_pr))

## check for gears with same name but grammatical differences ####
unique(tip$gear)
# replace "," with ";"
tip$gear <- str_replace(tip$gear, ",", ";")
# recheck that gears have been corrected
unique(tip$gear)

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

# Create count of total observed unique interviews for each gear and gear group  ####
## Gear count ####
tip_gear_id_count <- tip_gear_group |>
  group_by(gear)|>
  dplyr::summarize(
    .groups = "drop",
    gear_id = n_distinct(id),
  )
## Gear grouping count ####
tip_geargroup_id_count <- tip_gear_group |>
  group_by(gear_group)|>
  dplyr::summarize(
    .groups = "drop",
    geargroup_id = n_distinct(id),
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

## add counts to table    
geargroup_plot_count <- gear_group_count_notna |> 
  dplyr::mutate(
    ggroup_count =
      tip_geargroup_id_count$geargroup_id[match(
        gear_group_count_notna$gear_group,
        tip_geargroup_id_count$gear_group
      )]) |> 
  dplyr::mutate(ggroup_id = paste0(gear_group, " (", ggroup_count, ")"))

## plot number of interviews #### 
plot_gear_int_count <- geargroup_plot_count |>
  ggplot(aes(x = st_yr, y = all_interviews, group = ggroup_id)) +
  geom_line(aes(color = ggroup_id)) +
  geom_point(aes(color = ggroup_id)) +
  # ggplot2::facet_grid(record_type ~ island) +
  labs(
    color = "Gear",
    x = "Start Year",
    y = "Unique Interviews", 
    title = "Interview Count Gear Grouping"
  ) +
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
    hookline_interviews = n_distinct(id),
    hookline_records = n()
  )

count_t <- tip_gear_clean |>
  dplyr::filter(gear_group %in% "trap") |>
  dplyr::group_by(st_yr, region) |>
  dplyr::summarize(
    .groups = "drop",
    trap_interviews = n_distinct(id),
    trap_records = n()
  )

count_d <- tip_gear_clean |>
  dplyr::filter(gear_group %in% "diving") |>
  dplyr::group_by(st_yr, region) |>
  dplyr::summarize(
    .groups = "drop",
    diving_interviews = n_distinct(id),
    diving_records = n()
  )

count_n <- tip_gear_clean |>
  dplyr::filter(gear_group %in% "net") |>
  dplyr::group_by(st_yr, region) |>
  dplyr::summarize(
    .groups = "drop",
    net_interviews = n_distinct(id),
    net_records = n()
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
    hookline_percent = round(100 * hookline / all, 2),
    trap_percent = round(100 * trap / all, 2),
    diving_percent = round(100 * diving / all, 2),
    net_percent = round(100 * net / all, 2),
  )

# prep to plot number of interviews by region ####
## format dataframe to interview specific variables ####  
percent_gear_int <- gear_percent_overview |>
  filter(category == "interviews", ) |>
  select(st_yr, region, hookline_percent, trap_percent, diving_percent, net_percent)

percent_gear_plot <- melt(percent_gear_int, 
                     id.vars = c("st_yr", "region"), 
                     variable.name = "gear"
                     ) |>
  rename(percent = value) |>
  mutate(gear_group = case_when(
    gear == "hookline_percent" ~ "Hook-line",
    gear == "trap_percent" ~ "Trap",
    gear == "diving_percent" ~ "Diving",
    gear == "net_percent" ~ "Net",
    .default = "not coded"))

# ## Create count of total observed unique interviews for each gear  ####
# tip_ggroup_region_id_count <- tip_gear_clean |>
#   group_by(region, gear_group)|>
#   dplyr::summarize(
#     .groups = "drop",
#     ggroup_count = n_distinct(id),
#   )|>
#   mutate(gear_name = case_when(gear_group == "diving" ~ "Diving",
#                                 gear_group == "hook-line" ~ "Hook-line",
#                                 gear_group == "net" ~ "Net",
#                                 gear_group == "trap" ~ "Trap",
#                                  .default = "not coded"
#   ))|> 
#   dplyr::mutate(ggroup_id = paste0(region, " ", gear_name, " (", ggroup_count, ")"))
# 
# ## add gear counts to table    
# tip_ggroup_region_id_count_clean <- tip_ggroup_region_id_count |> 
#   select(region, gear_name, ggroup_id)
# 
# percent_gear_region_plot_count <-  
#   left_join(tip_gear_region_count,
#             tip_ggroup_region_id_count_clean, 
#             by = c("region", "gear_name"))

## Create count of total observed unique interviews for each region  ####
tip_regions_id_count <- tip_gear_clean |>
  group_by(region)|>
  dplyr::summarize(
    .groups = "drop",
    region_id = n_distinct(id),
  )

## add region counts to table    
tip_gear_region_count <- percent_gear_plot |> 
  dplyr::mutate(
    region_count =
      tip_regions_id_count$region_id[match(
        percent_gear_plot$region,
        tip_regions_id_count$region
      )]) |> 
  dplyr::mutate(region_id = paste0(region, " (", region_count, ")"))

## Gear grouping count ####
tip_geargroup_id_count <- tip_gear_clean |>
  group_by(gear_group) |>
  dplyr::summarize(
    .groups = "drop",
    ggroup_count = n_distinct(id),
  ) |>
  mutate(gear_group = case_when(
    gear_group == "diving" ~ "Diving",
    gear_group == "hook-line" ~ "Hook-line",
    gear_group == "net" ~ "Net",
    gear_group == "trap" ~ "Trap",
    .default = "not coded"
  )) |>
  dplyr::mutate(ggroup_id = paste0(gear_group, " (", ggroup_count, ")"))

## add gear counts to table    
tip_ggroup_region_id_count_clean <- tip_gear_region_count |> 
  dplyr::mutate(
    ggroup_id =
      tip_geargroup_id_count$ggroup_id[match(
        tip_gear_region_count$gear_group,
        tip_geargroup_id_count$gear_group
      )]) 

## plot ####
plot_int_gear_region <- tip_ggroup_region_id_count_clean |>
  ggplot(aes(x = st_yr, y = percent, group = ggroup_id)) +
  geom_line(aes(color = ggroup_id)) +
  geom_point(aes(color = ggroup_id)) +
  ggplot2::facet_wrap( ~ region_id) +
  labs(
    color = "Gear Type",
    x = "Start Year",
    title = "Interview Count by Region and Gear Grouping"
  ) +
  theme(
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.position = "right",
    # legend.title = element_blank()
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
