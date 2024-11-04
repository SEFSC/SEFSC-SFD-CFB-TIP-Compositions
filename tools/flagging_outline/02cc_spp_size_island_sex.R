# 02cc_spp_size_island_sex
# explore completness of sex records for specific island

# Load libraries ####
  librarian::shelf(
    here, tidyverse, janitor, flextable, ggplot2
  )

# Specify settings #### 
# rds from end of 02c script
  date <- "20241029" 
# find on itis.gov
  spp_itis <- c("097648", "097646") 
  spp <- "csl"
  print_spp <- "Caribbean Spiny Lobster"
# chose island platform to focus on   
  isl <- "pr" 
  print_isl <- "Puerto Rico"
  sedar <- "sedar91"

# Read in formatted data ####
  tip_spp_rds <- paste0(isl, "_", spp, "_spp_size_island_view_", date, ".rds" )
  tip_spp <- readRDS(here::here("data", sedar, "rds", spp, isl, tip_spp_rds))

# count sex records #### 
  tip_sex <- tip_spp |> 
    group_by(sex_name) |> 
    summarize(
      .groups = "drop",
      n = dplyr::n(),)

# create formated table 
 tip_sex_tbl <- flextable(tip_sex) |>
    theme_box() %>%
    align(align = "center", part = "all") %>%
    fontsize(size = 8, part = "all") %>%
    autofit()
  
# save  
  save_as_image(x = tip_sex_tbl, path =  
                  here::here("data", sedar, "figure", spp, isl, "tip_sex_tbl.png"))

# count sex records by year #### 
  tip_sex_year <- tip_spp |> 
    group_by(year, sex_name) |> 
    summarize(
      .groups = "drop",
      n = dplyr::n(),) |> 
    tidyr::pivot_wider(
      names_from = sex_name,
      values_from = n
      )
  tip_sex_year$total_n_records <- rowSums(tip_sex_year[ , c(2,3,4,5,6)], na.rm=TRUE)
    

# create formatted table 
  tip_sex_year_tbl <- flextable(tip_sex_year) |>
    theme_box() %>%
    align(align = "center", part = "all") %>%
    fontsize(size = 8, part = "all") %>%
    autofit() |>
    colformat_num(j = c("year"), big.mark = "")

# save  
  save_as_image(x = tip_sex_year_tbl, path =  
                  here::here("data", sedar, "figure", spp, isl, "tip_sex_year_tbl.png"))
