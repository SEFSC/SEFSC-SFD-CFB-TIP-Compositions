# 3a_species

# # Load libraries ####
# librarian::shelf(here, tidyverse, measurements, flextable, ggplot2, reshape2)

# Specify settings ####
tip_pr <- "pr_gear_grouped_tip_20241213.rds" # add formatted data

# Read in raw data ####
tip <- readRDS(here::here("data", tip_pr))

# read in species table ####
species_conver <- 
  readr::read_csv("data/CSVs/pr_species_codes_oracle_conv.csv") |> 
  janitor::clean_names()  
  # mutate(standard_species_id = as.numeric(standard_species_id))
summary(species_conver)

## attach gear groups to records ####
tip_species <- tip  |> 
  # mutate(species_code = as.numeric(species_code)) |> 
  mutate(
    species_name =
      species_conver$name[match(
        tip$species_code,
        species_conver$standard_species_id
      )]
  )

## attach gear groups to records ####
# tip_species <- tip_gear_clean  |> 
#   # mutate(species_code = as.numeric(species_code)) |> 
#   mutate(
#     species_name =
#       species_conver$name[match(
#         tip_gear_clean$species_code,
#         species_conver$standard_species_id
#       )]
#   )
summary(tip_species$species_code)
# check for na's
tip_species_na <- tip_species |> 
  filter(is.na(species_name))
unique(tip_species_na$species_code)

# remove not coded records (na)
tip_species_clean <- tip_species |> 
  filter(!is.na(species_name))

# Tabulate TIP interviews and records by year grouping and species ####
## count all records and interviews for each species####
count_species <- tip_species_clean |>
  dplyr::group_by(species_name, species_code) |>
  dplyr::summarize(
    .groups = "drop",
    all_interviews = n_distinct(id),
    all_records = n()
  ) |>
  arrange(desc(species_name))

flextable(count_species) |>
  theme_box() %>%
  align(align = "center", part = "all") %>%
  fontsize(size = 8, part = "all") %>%
  autofit()

# count species by gear 
count_species_gear <- tip_species_clean |>
  dplyr::group_by(species_name, gear_group) |>
  dplyr::summarize(
    .groups = "drop",
    all_interviews = n_distinct(id),
    all_records = n()
  ) 

# Percentage calculation of gear by region  ####
## count all records and interviews ####
count_species_all <- tip_species_clean |>
  dplyr::group_by(species_name) |>
  dplyr::summarize(
    .groups = "drop",
    all_interviews = n_distinct(id),
    all_records = n()
  ) 

## count only specific region ####
count_h <- tip_species_clean |>
  dplyr::filter(gear_group %in% "hook-line") |>
  dplyr::group_by(species_name) |>
  dplyr::summarize(
    .groups = "drop",
    h_interviews = n_distinct(id),
    h_records = n()
  )

count_t <- tip_species_clean |>
  dplyr::filter(gear_group %in% "trap") |>
  dplyr::group_by(species_name) |>
  dplyr::summarize(
    .groups = "drop",
    t_interviews = n_distinct(id),
    t_records = n()
  )

count_d <- tip_species_clean |>
  dplyr::filter(gear_group %in% "diving") |>
  dplyr::group_by(species_name) |>
  dplyr::summarize(
    .groups = "drop",
    d_interviews = n_distinct(id),
    d_records = n()
  )

count_n <- tip_species_clean |>
  dplyr::filter(gear_group %in% "net") |>
  dplyr::group_by(species_name) |>
  dplyr::summarize(
    .groups = "drop",
    n_interviews = n_distinct(id),
    n_records = n()
  )

## summarize both counts ####
species_count_overview <- count_species_all |>
  dplyr::left_join(count_h, by = join_by(species_name)) |>
  dplyr::left_join(count_t, by = join_by(species_name)) |>
  dplyr::left_join(count_d, by = join_by(species_name)) |>
  dplyr::left_join(count_n, by = join_by(species_name)) |>
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
species_count_tbl <- species_count_overview |>
  select(species_name, gear_name, category, count)   |> 
  # pivot_longer(cols = c(Var1, Var2)) |> 
  pivot_wider(names_from = gear_name, values_from = count)


flextable(species_count_tbl) |>
  theme_box() %>%
  align(align = "center", part = "all") %>%
  fontsize(size = 8, part = "all") %>%
  autofit()

# Save formatted tip_spp ####
saveRDS(
  tip_species_clean,
  file = here::here(
    "data",
    paste0( "pr_species_name_tip_",
            format(Sys.time(), "%Y%m%d"), 
            ".rds"
    )
  )
)

