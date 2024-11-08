# Load libraries ####
  librarian::shelf(here, tidyverse, measurements)
 
# check gears
  unique(tip_spp$gear)  
  
# replace "," with ";"
  tip_spp$gear <- str_replace(tip_spp$gear, ",", ";") 

# replace "NA" with "NOT CODED" 
  tip_spp <- tip_spp |> 
    mutate(gear =
           as.character(ifelse(is.na(gear),
                               "NOT CODED",
                               gear)))
  
# check gears
  unique(tip_spp$gear)

# add nmfs gear groupings 
  tip_nmfs <- tip_spp |> 
    mutate(gear_group_code = gear_nmfs$gear_group[match(
                 tip_spp$gear,
                 gear_nmfs$gear
               )]
           )
  
# check for and convert 0's to NA in weight variables
  tip_range <- tip_nmfs[with(tip_nmfs, order(obs_weight_kg)), ]
  tip_range$obs_weight_kg[1:25]
  tip_nmfs$obs_weight_kg[tip_nmfs$obs_weight_kg == 0] <- NA
  tip_range <- tip_nmfs[with(tip_nmfs, order(obs_weight_kg)), ]
  tip_range$obs_weight_kg[1:25]
  
# check for and convert 0's to NA in length variables
  tip_range <- tip_nmfs[with(tip_nmfs, order(length1_mm)), ]
  tip_range$length1_mm[1:25]
  # tip_nmfs$length1_mm[tip_nmfs$length1_mm == 0] <- NA
  # tip_range <- tip_nmfs[with(tip_nmfs, order(length1_mm)), ]
  # tip_range$length1_mm[1:25]
  
  tip_range <- tip_nmfs[with(tip_nmfs, order(length2_mm)), ]
  tip_range$length2_mm[1:25]
  # tip_nmfs$length2_mm[tip_nmfs$length2_mm == 0] <- NA
  # tip_range <- tip_nmfs[with(tip_nmfs, order(length2_mm)), ]
  # tip_range$length2_mm[1:25]
  
  
# confidentiality by year and gear and trip ID
  tip_count_a <- tip_nmfs |> 
    group_by(island, date, year, gear) |> 
    mutate(n_ID_yr_gr = n_distinct(sampling_unit_id) ) |> 
    ungroup() |> 
    group_by(island, year, gear) |> 
    summarize(max_yr_gr_c = max(n_ID_yr_gr), 
              .groups = "drop") |> 
    right_join(tip_nmfs, 
               by = join_by(island, year, gear)) |> 
    mutate(isl_yr_gear_c = case_when(max_yr_gr_c <= 2 ~ "TRUE",
                                  TRUE ~ "FALSE")) 
# summary stats
    tip_sum_yr_gr <- tip_count_a |>
      select(island, year, gear, isl_yr_gear_c) |>
      distinct() |>
      filter(isl_yr_gear_c == "TRUE")

    table(tip_sum_yr_gr$island, tip_sum_yr_gr$year)
    table(tip_sum_yr_gr$island )
    table(tip_count_a$isl_yr_gear_c)
    table(tip_count_a$isl_yr_gear_c)/nrow(tip_count_a)

# confidentiality by gear and trip ID
  tip_count_b <- tip_count_a |> 
    group_by(island, date, gear) |> 
    mutate(n_ID_gr = n_distinct(sampling_unit_id) ) |> 
    ungroup() |> 
    group_by(island, gear) |> 
    summarize(max_c_gr = max(n_ID_gr), 
              .groups = "drop") |> 
    right_join(tip_count_a, 
               by = join_by(island, gear)) |> 
    mutate(isl_gear_c = case_when(max_c_gr <= 2 ~ "TRUE",
                                  TRUE ~ "FALSE")) 
  
# summary stats   
  tip_sum_yr <- tip_count_b |> 
    select(island, gear, isl_gear_c) |> 
    distinct() |> 
    filter(isl_gear_c == "TRUE")
  
  table(tip_sum_yr$gear,tip_sum_yr$island )
  table(tip_sum_yr$island )
  table(tip_count_b$isl_gear_c)
  table(tip_count_b$isl_gear_c)/nrow(tip_count_b)
  
# confidentiality by gear and vessel id 
  tip_count_c <- tip_count_b |> 
    group_by(island, gear) |> 
    mutate(n_ves = n_distinct(vessel_id) ) |> 
    ungroup() |> 
    mutate(vess_isl_gear_c = case_when(n_ves <= 2 ~ "TRUE",
                                  TRUE ~ "FALSE"))

# summary stats   
  tip_sum_yr_vess <- tip_count_c |> 
    select(island, gear, vess_isl_gear_c) |> 
    distinct() |> 
    filter(vess_isl_gear_c == "TRUE")
  
  table(tip_sum_yr_vess$gear,tip_sum_yr_vess$island )
  table(tip_sum_yr_vess$island )
  table(tip_count_c$vess_isl_gear_c)
  table(tip_count_c$vess_isl_gear_c)/nrow(tip_count_c)

  
# conf by gear, year, and vessel id 
  tip_count_d <- tip_count_c |> 
    group_by(island, year, gear) |> 
    mutate(n_ves_yr = n_distinct(vessel_id) ) |> 
    ungroup() |> 
    mutate(vess_isl_yr_gear_c = case_when(n_ves_yr <= 2 ~ "TRUE",
                                       TRUE ~ "FALSE"))

# summary stats   
  tip_sum_yr_vess_yr <- tip_count_d |> 
    select(island, year, gear, vess_isl_yr_gear_c) |> 
    distinct() |> 
    filter(vess_isl_yr_gear_c == "TRUE")
  
  table(tip_sum_yr_vess_yr$gear,tip_sum_yr_vess_yr$island )
  table(tip_sum_yr_vess_yr$island )
  table(tip_count_d$vess_isl_yr_gear_c)
  table(tip_count_d$vess_isl_yr_gear_c)/nrow(tip_count_d)
  
  
# conf by gear and license
  tip_count_e <- tip_count_d |> 
    group_by(island, date, gear) |> 
    mutate(n_lic = n_distinct(license) ) |> 
    ungroup() |> 
    mutate(lic_isl_gear_c = case_when(n_lic <= 2 ~ "TRUE",
                                      TRUE ~ "FALSE"))
 
# summary stats   
  tip_sum_yr_lic <- tip_count_e |> 
    select(island, gear, lic_isl_gear_c) |> 
    distinct() |> 
    filter(lic_isl_gear_c == "TRUE")
  
  table(tip_sum_yr_lic$gear,tip_sum_yr_lic$island )
  table(tip_sum_yr_lic$island )
  table(tip_count_e$lic_isl_gear_c)
  table(tip_count_e$lic_isl_gear_c)/nrow(tip_count_e)

  
# Select variables relevant to flagging investigation ####
  tip_spp_relevant <- tip_count_e |>
    select(
      sampling_program, # denotes who recorded data
      data_provider, # noaa office handling data 
      stock, # region 
      sampling_unit_id, # trip interview id
      date, # interview month/day/year 
      month, # month of interview
      day, # day of interview
      year, # interview year
      island, # pr, stt, stx
      state_landed, # original state value
      county_landed, # original county value
      area, # state vs federal area
      area_square,# grid area when available
      species_code, # ITIS code of species 
      species_name, # common name of species given in TIP
      sex_name, # male, female, unknown, not sexed
      gear, # gear from land_standard_gear_name subbed w/ gear_1 when unavailable
      gear_group_code, # nmfs gear group code 
      standardgearname_2, # check to see if second gear was recorded 
      length1_mm, # length1 converted by TIP system to mm 
      length_unit1, # original length unit
      length_type1, # original length type
      length2_mm, # for future comparisons
      length_type2, # for future comparisons
      obs_weight_kg, # weight converted to kg by TIP system
      obs_weight_unit, # original weight unit
      quantity, # number of individuals that were weighed/measured 
      sample_condition, # weight type
      fishery, # denotes recreational or commercial
      fishery_mode, # fishing_mode value
      isl_yr_gear_c, # confidentiality flag island-year-gear-trip id  strata
      isl_gear_c, # confidentiality flag island-gear-trip id strata 
      vess_isl_gear_c, # confidentality flag island-gear-vessel id strata
    ) 
  

# Save formatted tip_spp ####
  saveRDS(
    tip_spp_relevant,
    file = here::here(
      "data",
      sedar,
      "rds",
      paste0( "format_tip_",
        format(Sys.time(), "%Y%m%d"), 
        ".rds"
      )
    )
  )

# export data as csv if needed 
# write.csv(tip_spp_relevant, 
#           file = "data/CSVs/tip_rbr_04042024.csv", 
#           row.names=FALSE)
