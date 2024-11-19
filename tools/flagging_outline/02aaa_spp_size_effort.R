# 02aaa_spp_size_effort 

# Load libraries ####
librarian::shelf(
  here, tidyverse, janitor, flextable, ggplot2
)

# Specify settings #### 
# date from end of 02aa script
date <- "20241118"
# find on itis.gov
spp_itis <- c("097648", "097646") 
spp <- "csl"
isl <- c("pr", "stt", "stx")
print_spp <- "Caribbean Spiny Lobster"
print_isl <- "Puerto Rico - USVI"
# folder name 
sedar <- "sedar91"

# Read in formatted data ####
tip_spp_rds <- paste0("prusvi_csl_spp_size_quantity_", date, ".rds" )
tip_spp <- readRDS(here::here("data", sedar, "rds", spp, "all", tip_spp_rds))


# summarize effort data
tip_effort <- tip_spp |> 
  # filter(!is.na(gear_qty_1)) |> 
  group_by(island, gear) |> 
  summarize(
    .groups = "drop",
    n = dplyr::n(),
    first_year = min(year),
    last_year = max(year),
    min_num_gear = min(number_of_gear_1, na.rm = TRUE),
    max_num_gear = max(number_of_gear_1, na.rm = TRUE),
    min_effort = min(gear_qty_1, na.rm = TRUE),
    max_effort = max(gear_qty_1, na.rm = TRUE),
    )

# create formatted table of length/weight pair completion 
tip_effort_tbl <- flextable(tip_effort) |>
  theme_box() %>%
  align(align = "center", part = "all") %>%
  fontsize(size = 8, part = "all") %>%
  autofit() |>
  colformat_num(j = c("first_year", "last_year"), big.mark = "")

# view 
tip_effort_tbl

# save  
save_as_image(x = tip_effort_tbl, path =  
                here::here("data", sedar, "figure", spp, "all", "tip_effort_tbl.png"))  

