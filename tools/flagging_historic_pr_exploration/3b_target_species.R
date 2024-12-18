# 3b_target_species
# caribbean target species 

# # Specify settings ####
tip_pr <- "pr_species_name_tip_20241216.rds" # add formatted data

# Read in raw data ####
tip <- readRDS(here::here("data", tip_pr))


# read in target species table ####
carb_target_species <- 
  readr::read_csv("data/CSVs/reef_fish_spp_itis_codes.csv") |> 
  janitor::clean_names()  

summary(species_conver)

# filter to target species 
tip_target_species <- tip |> 
  filter(species_code %in% carb_target_species$itis_code)

# # filter to target species 
# tip_target_species <- tip_species_clean |> 
#   filter(species_code %in% carb_target_species$itis_code)

# Tabulate TIP interviews and records by year grouping and species ####
## count all records and interviews for each species####
count_target_species <- tip_target_species |>
  dplyr::group_by(species_name, species_code) |>
  dplyr::summarize(
    .groups = "drop",
    all_interviews = n_distinct(id),
    all_records = n()
  ) |>
  arrange(desc(all_interviews))

flextable(count_target_species) |>
  theme_box() %>%
  align(align = "center", part = "all") %>%
  fontsize(size = 8, part = "all") %>%
  autofit()

# count species by gear 
target_species_gear <- tip_target_species |>
  dplyr::group_by(species_name, gear_group) |>
  dplyr::summarize(
    .groups = "drop",
    all_interviews = n_distinct(id),
    all_records = n()
  ) 

# Percentage calculation of gear by region  ####
## count all records and interviews ####
target_species_all <- tip_target_species |>
  dplyr::group_by(species_name) |>
  dplyr::summarize(
    .groups = "drop",
    all_interviews = n_distinct(id),
    all_records = n()
  ) 

## count only specific region ####
target_h <- tip_target_species |>
  dplyr::filter(gear_group %in% "hook-line") |>
  dplyr::group_by(species_name) |>
  dplyr::summarize(
    .groups = "drop",
    h_interviews = n_distinct(id),
    h_records = n()
  )

target_t <- tip_target_species |>
  dplyr::filter(gear_group %in% "trap") |>
  dplyr::group_by(species_name) |>
  dplyr::summarize(
    .groups = "drop",
    t_interviews = n_distinct(id),
    t_records = n()
  )

target_d <- tip_target_species |>
  dplyr::filter(gear_group %in% "diving") |>
  dplyr::group_by(species_name) |>
  dplyr::summarize(
    .groups = "drop",
    d_interviews = n_distinct(id),
    d_records = n()
  )

target_n <- tip_target_species |>
  dplyr::filter(gear_group %in% "net") |>
  dplyr::group_by(species_name) |>
  dplyr::summarize(
    .groups = "drop",
    n_interviews = n_distinct(id),
    n_records = n()
  )

## summarize both counts ####
target_species_overview <- target_species_all |>
  dplyr::left_join(target_h, by = join_by(species_name)) |>
  dplyr::left_join(target_t, by = join_by(species_name)) |>
  dplyr::left_join(target_d, by = join_by(species_name)) |>
  dplyr::left_join(target_n, by = join_by(species_name)) |>
  dplyr::mutate(
    across(everything(), ~ replace_na(.x, 0))
  ) |>
  tidyr::pivot_longer(
    cols = !c(species_name),
    names_to = c("subset", "category"),
    names_sep = "_",
    values_to = "count"
  )|>
  mutate(gear_name = case_when(
    subset == "h" ~ "Hook-line",
    subset == "t" ~ "Trap",
    subset == "d" ~ "Diving",
    subset == "n" ~ "Net",
    subset == "all" ~ "All",
    .default = "not coded"))

# reformat table 
target_species_tbl <- target_species_overview |>
  select(species_name, gear_name, category, count)   |> 
  # pivot_longer(cols = c(Var1, Var2)) |> 
  pivot_wider(names_from = gear_name, values_from = count)


flextable(target_species_tbl) |>
  theme_box() %>%
  align(align = "center", part = "all") %>%
  fontsize(size = 8, part = "all") %>%
  autofit()

# Save formatted tip_spp ####
saveRDS(
  tip_target_species,
  file = here::here(
    "data",
    paste0( "pr_target_spp_",
            format(Sys.time(), "%Y%m%d"), 
            ".rds"
    )
  )
)
