# This script is for investigating the representation of a specific 
# species/island to the tip dataset as a whole

# run the 01a script with a data pull of the entire dataset first

# Load libraries ####
librarian::shelf(here, tidyverse, measurements, expss, openxlsx)

# Specify settings ####
tip_spp_rds <- "car_rbr_format_tip_20240404.rds" # rds from end of 01a script
spp_itis <- 168738
spp <- "rbr"
isl <- "car"
print_spp <- "Rainbow Runner"
print_isl <- "Caribbean"

# Read in formatted data ####
tip_spp <- readRDS(here::here("data", tip_spp_rds))

# Tabulate TIP interviews and records by island and year ####
# count all records and interviews
count_all <- tip_spp |>
  dplyr::group_by(island, year) |>
  dplyr::summarize(
    .groups = "drop",
    all_interviews = n_distinct(id),
    all_records = n()
  )

# count only specific species
count_spp <- tip_spp |>
  dplyr::filter(species_code == spp_itis) |>
  dplyr::group_by(island, year) |>
  dplyr::summarize(
    .groups = "drop",
    spp_interviews = n_distinct(id),
    spp_records = n()
  ) |> 
  mutate(year = as.character(year)) 

# export xlsx of length counts by island
count_spp_pr <- count_spp |>
  filter(island == "pr")

count_spp_sttj <- count_spp |>
  filter(island == "stt") 

count_spp_stx <- count_spp |>
  filter(island == "stx") 

total_isl <- count_spp |> 
  group_by(island) |>
  dplyr::summarise(total_interviews = sum(spp_interviews),
                total_records = sum(spp_records)) 
wb = createWorkbook()
sh1 = addWorksheet(wb, "total")
sh2 = addWorksheet(wb, "pr")
sh3 = addWorksheet(wb, "sttj")
sh4 = addWorksheet(wb, "stx")

xl_write(total_isl, wb, sh1)
xl_write(count_spp_pr, wb, sh2)
xl_write(count_spp_sttj, wb, sh3)
xl_write(count_spp_stx, wb, sh4)

saveWorkbook(wb, 
             paste0(spp, 
                    "_stats_", 
                    format(Sys.time(), "%Y%m%d"),  
                    ".xlsx"), 
             overwrite = TRUE)

# summarize both counts
count_overview <- count_all |>
  dplyr::left_join(count_spp, by = join_by(island, year)) |>
  dplyr::mutate(
    across(everything(), ~ replace_na(.x, 0))
  ) |>
  tidyr::pivot_longer(
    cols = !c(island, year),
    names_to = c("subset", "category"),
    names_sep = "_",
    values_to = "count"
  )

# calculate percent spp each year
percent_overview <- count_overview |>
  tidyr::pivot_wider(
    names_from = subset,
    values_from = count
  ) |>
  dplyr::mutate(
    percent = round(100 * spp / all, 2)
  )

# count spp based on sector
count_spp_sector <- tip_spp |>
  dplyr::filter(
    species_code == spp_itis,
    length_type1 != "NO LENGTH",
    island != "not coded"
  ) |>
  dplyr::group_by(island, year, sector, length_type1) |>
  dplyr::summarize(
    .groups = "drop",
    records = n()
  )

# Plot interviews and records by island and year ####
plot_count_overview <- count_overview |>
  ggplot2::ggplot(aes(x = year, y = count, fill = subset)) +
  ggplot2::geom_col(position = position_dodge(preserve = "single")) +
  ggplot2::facet_grid(category ~ island, scales = "free_y") +
  ggplot2::theme(
    legend.position = "bottom",
    legend.title = element_blank())+ 
  labs(x = "Year", 
       y = "Count",
       title = paste0(print_spp, 
                      " Subset Comparison of Interviews and Records by Year"))

plot_percent_overview <- percent_overview |>
  ggplot2::ggplot(aes(x = year, y = percent)) +
  ggplot2::geom_col() +
  ggplot2::facet_grid(category ~ island)+
  labs(x = "Year", 
       y = "Percent",
       title = paste0(print_spp, 
                      " Percent Composition of Total TIP Interviews and Records"))

plot_count_spp_sector <- count_spp_sector |>
  ggplot2::ggplot(aes(x = year, y = records, fill = length_type1)) +
  ggplot2::geom_col(position = position_dodge(preserve = "single")) +
  ggplot2::facet_grid(sector ~ island) +
  labs(x = "Year", 
       y = "Records",
       title = paste0(print_spp, 
                      " Length Types"))+
  ggplot2::theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )
