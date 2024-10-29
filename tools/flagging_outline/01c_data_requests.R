# data pull request
# run the 01a script with a data pull of the entire dataset first

# Load libraries ####
librarian::shelf(here, tidyverse, measurements, expss, openxlsx, flextable)

# Specify settings ####
tip_spp_rds <- "format_tip_20240827.rds" # rds from end of 01a script
spp_itis <- c("172419", "172421", "172409", "550888", "160200", "768126",
              "159921", "172428", "160307", "160304", "160318", "160318",
              "172491", "160424", "160502", "160275", "160336", "160206", 
              "168791", "160604", '168790', '168792', '160268', '160409', 
              '160515', "160497", '172435', '160433', '172402', '172502', 
              '159926', '160413', '159977', '160330', '159911', '160178',
              '172488', '172487', '172486', '160289', '160808', '160807', 
              '160508', '159785', '159901', '159923', '160267', '159851', 
              '159915', '159844', '172503', '159924', '160310', '172401',
              '172400', '159786', '160505', '160401', '172482', '160189', 
              '172418', '172451', '159903', '159903', '172423'
              ) 
# find on itis.gov or catalogueoflife.org
# if itis is only 5 #'s add a 0 before spp code 
spp <- "sharks_pelagics"
isl <- "pr_usvi"
print_spp <- "Caribbean Pelagics and Sharks"
print_isl <- "US Caribbean"
sedar <- "sedar91"

# Read in formatted data ####
tip_spp <- readRDS(here::here("data", sedar, "rds", tip_spp_rds))

# filter to species ####
tip_filter <- tip_spp |>
  # Add isl filter if needed
  dplyr::filter(species_code %in% spp_itis) |>
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

unique(tip_filter$species_name)

# create non-confidential overview table ####
length_types <- tip_filter |>
  dplyr::group_by(
    island,
    species_code,
    species_name
  ) |>
  dplyr::summarize(
    .groups = "drop",
    n_records = dplyr::n(),
    n_id = n_distinct(id),
    first_year = min(year),
    last_year = max(year),
    n_years = dplyr::n_distinct(year),
    min_length_cm = min(length1_cm, na.rm = TRUE),
    max_length_cm = max(length1_cm, na.rm = TRUE),
    avg_length_cm = round(mean(length1_cm, na.rm = TRUE), 2),
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
    "data_request",
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
    total_interviews = sum(n_id),
    total_records = sum(n_records)
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
             file = here::here(
               "data",
               "data_request",
               paste0(
                isl, "_", spp,
                "_stats_",
                format(Sys.time(), "%Y%m%d"),
                ".xlsx"
              ),
              overwrite = TRUE
            )
)
