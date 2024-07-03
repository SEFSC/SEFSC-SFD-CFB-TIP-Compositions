# data pull request
# run the 01a script with a data pull of the entire dataset first

# Load libraries ####
librarian::shelf(here, tidyverse, measurements, expss, openxlsx, flextable)

# Specify settings ####
tip_spp_rds <- "format_tip_20240703.rds" # rds from end of 01a script
spp_itis <- "97648" # 97646 97648 find on itis.gov or catalogueoflife.org
spp <- "csl"
isl <- "pr_usvi"
print_spp <- "Caribbean Spiny Lobster"
print_isl <- "US Caribbean"

# Read in formatted data ####
tip_spp <- readRDS(here::here("data", tip_spp_rds))

# filter to species ####
tip_filter <- tip_spp |>
  # Add isl filter if needed
  dplyr::filter(species_code == spp_itis) |>
  # Redo variable for island
  dplyr::mutate(
    island = dplyr::case_when(
      state_landed == "PUERTO RICO" ~ "pr",
      # use if stt/stx are non-confidential
      state_landed == "VIRGIN ISLANDS" &
        county_landed %in% c("ST JOHN", "ST THOMAS") ~ "stt",
      state_landed == "VIRGIN ISLANDS" &
        county_landed == "ST CROIX" ~ "stx",
      # use if stt/stx are confidential
      # state_landed == "VIRGIN ISLANDS" ~ "usvi", 
      .default = "not coded"
    ),
    # Convert units and calculate k ####
    length1_cm = measurements::conv_unit(length1_mm, "mm", "cm"),
    length1_inch = measurements::conv_unit(length1_mm, "mm", "inch"),
    obs_weight_lbs = measurements::conv_unit(obs_weight_kg, "kg", "lbs"),
    k = 10^5 * obs_weight_kg / length1_cm^3,
    record_type = case_when(
      !is.na(k) ~ "complete",
      .default = "incomplete"
    )
  )

# create non-confidential overview table ####
length_types <- tip_filter |>
  dplyr::group_by(
    island,
    species_code,
  ) |>
  dplyr::summarize(
    .groups = "drop",
    n_records = dplyr::n(),
    first_year = min(year),
    last_year = max(year),
    n_years = dplyr::n_distinct(year),
    min_length_cm = min(length1_cm),
    max_length_cm = max(length1_cm),
    avg_length_cm = round(mean(length1_cm), 2),
    na_length_cm = sum(is.na(length1_cm)),
  )

flextable(length_types) |>
  theme_box() %>%
  align(align = "center", part = "all") %>%
  fontsize(size = 8, part = "all") %>%
  autofit()|>
  colformat_num(j = c("first_year", "last_year"), big.mark = "")


# export raw data of just target species
write.csv(tip_filter,
  file = here::here(
    "data",
    paste0(
      isl, "_",
      spp, "_data_request_",
      format(Sys.time(), "%Y%m%d"),
      ".csv"
    )
  ),
  row.names = FALSE
)

# SKIP if not needed
# export xlsx of length counts by island if needed
count_spp_pr <- length_types |>
  filter(island == "pr")

count_spp_sttj <- length_types |>
  filter(island == "stt")

count_spp_stx <- length_types |>
  filter(island == "stx")

total_isl <- length_types |>
  group_by(island) |>
  dplyr::summarise(
    total_interviews = sum(spp_interviews),
    total_records = sum(spp_records)
  )
wb <- createWorkbook()
sh1 <- addWorksheet(wb, "total")
sh2 <- addWorksheet(wb, "pr")
sh3 <- addWorksheet(wb, "sttj")
sh4 <- addWorksheet(wb, "stx")

xl_write(total_isl, wb, sh1)
xl_write(count_spp_pr, wb, sh2)
xl_write(count_spp_sttj, wb, sh3)
xl_write(count_spp_stx, wb, sh4)

saveWorkbook(wb,
  paste0(
    spp,
    "_stats_",
    format(Sys.time(), "%Y%m%d"),
    ".xlsx"
  ),
  overwrite = TRUE
)
