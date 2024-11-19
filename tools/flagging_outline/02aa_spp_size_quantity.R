# 02aa_spp_size_quantity ####
# investigate "quantity" variable - indicates number of individuals measured 
# noted in CSL EDA

# Load libraries ####
  librarian::shelf(
    here, tidyverse, janitor, flextable, ggplot2
  )

# Specify settings #### 
# rds from end of 02a script
  date <- "20241118" 
# find on itis.gov
  spp_itis <- c("097648", "097646") 
  spp <- "csl"
  isl <- c("pr", "stt", "stx")
  print_spp <- "Caribbean Spiny Lobster"
  print_isl <- "Puerto Rico - USVI"
  save_isl <- "prusvi"
# folder name 
  sedar <- "sedar91"
  
# Read in formatted data ####
  tip_spp_rds <- paste0("prusvi_csl_spp_size_prep_", date, ".rds" )
  tip_spp <- readRDS(here::here("data", sedar, "rds", spp, "all", tip_spp_rds))

# View records that have "quantity" variable ####
  tip_quantity <- tip_spp |> 
    filter(quantity > 1) |> 
    group_by(species_code, island, quantity) |> 
    summarise(
      .groups = "drop",
      interviews = n_distinct(sampling_unit_id),
      records = n())
  
  tip_quantity_total <- tip_quantity |> 
    summarise(
      .groups = "drop",
      all_interviews = sum(interviews),
      all_records = sum(records))

# create formatted table of quantity variables  
 tip_quantity_tbl <- flextable(tip_quantity) |>
    theme_box() %>%
    align(align = "center", part = "all") %>%
    fontsize(size = 8, part = "all") %>%
    autofit() 
  
# view 
 tip_quantity_tbl
# save  
  save_as_image(x = tip_quantity_tbl, path =  
                  here::here("data", sedar, "figure", spp, "all", "tip_quantity_tbl.png"))  
  
# Tabulate complete and incomplete length and weight pairs
  count_lw_pairs <- tip_spp |>
    filter(quantity > 1) |> 
    dplyr::group_by(island, year, sampling_program, fishery, length_type1, record_type) |>
    dplyr::summarize(
      .groups = "drop",
      records = n()
    )

# create formatted table of length/weight pair completion 
  count_lw_pairs_tbl <- flextable(count_lw_pairs) |>
    theme_box() %>%
    align(align = "center", part = "all") %>%
    fontsize(size = 8, part = "all") %>%
    autofit() |>
    colformat_num(j = "year", big.mark = "")
# view 
  count_lw_pairs_tbl
# save  
  save_as_image(x = count_lw_pairs_tbl, path =  
                  here::here("data", sedar, "figure", spp, "all", "count_lw_pairs_tbl.png"))  
  
# create variable counts by isl 
  tip_spp_quant_count <- tip_spp |>
    filter(quantity > 1) |> 
    add_count(island) |>
    dplyr::mutate(islandn = paste0(island, " (", n, ")")) |>
    select(-n)
  
  tip_dup_count <- tip_spp_quant_count |> 
    group_by(island) |> 
    summarize(n_records = n(),
              n_fix = sum(quantity)) 
  
# Save dataframe of just records with quantity >1 ####
  saveRDS(
    tip_spp_quant_count,
    file = here::here(
      "data",
      sedar,
      "rds",
      spp, 
      "all",
      paste0(
        save_isl, "_",
        spp, "_spp_quant_count_",
        format(Sys.time(), "%Y%m%d"),
        ".rds"
      )
    )
  )

# Plot length values recorded by quantity ####
  quantity_time <- tip_spp_quant_count |>
    # filter(quantity > 1) |> 
    ggplot(aes(x = date, 
               y = length1_cm, 
               # color = as.factor(quantity)
               )
           ) +
    # facet_wrap(~islandn, ncol = 2) + 
    facet_wrap( ~ islandn) +
    geom_point(aes(colour = cut(quantity, c(-Inf, 5, 20, Inf)))) +
    scale_color_manual(name = "quantity",
                       values = c("(-Inf,5]" = "blue",
                                  "(5,20]" = "green",
                                  "(20, Inf]" = "red"),
                       labels = c("<= 5", "5 < quantity <= 20", "> 20")) +
    labs(
      x = "Year", y = "Length (cm)",
      title = "Quantity distribution of Lengths sampled",
      color = "Quantity Value",
      subtitle = paste("Total N = ", nrow(tip_spp_quant_count))
    )
# view plot  
  quantity_time
# save  
  ggsave(filename = 
           here::here("data", sedar, "figure", spp, "all", "quantity_time.png"),
         width = 14, height = 8)
  

# duplicate records with quantity value larger than 1  
  tip_dup <- uncount(tip_spp, quantity)
    
# Save formatted tip_spp ####
  saveRDS(
    tip_dup,
    file = here::here(
      "data",
      sedar,
      "rds",
      spp, 
      "all",
      paste0(
        save_isl, "_",
        spp, "_spp_size_quantity_",
        format(Sys.time(), "%Y%m%d"),
        ".rds"
      )
    )
  )
  