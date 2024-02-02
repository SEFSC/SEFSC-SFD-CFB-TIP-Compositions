# Load libraries
librarian::shelf(here, tidyverse)

# Load objects
load(
  here("data", "raw",
       "slp_stx_com_landals_1222_20231130_raw_C.RData")
)

# Quick views
View(data.qcd)

data.qcd |>
  select(GEAR_NM, GEAR_GROUP) |>
  distinct() |>
  arrange(GEAR_GROUP, GEAR_NM)

# What fraction is trips by TRIP_YEAR and GEAR_GROUP where GEAR_GROUP == SCUBA),
# compared to interviews by year and gear, gear %in%  c("SPEARS; DIVING", "BY HAND; DIVING GEAR")

# Get LBS by year to identify main gears
by_year <- data.qcd |>
  group_by(TRIP_YEAR) |>
  summarise(YEAR_LBS = sum(POUNDS_LANDED))

# Get LBS by year and gear
by_year_gear <- data.qcd |>
  group_by(TRIP_YEAR, GEAR_NM) |>
  summarize(TOTAL_LBS = sum(POUNDS_LANDED),
            TOTAL_TRIPS = n_distinct(TRIP_ID),
            IS_CONFIDENTIAL = case_when(n_distinct(VESSEL_CD) >= 3 ~ "N",
                                        .default = "Y"),
            .groups = "drop") |>
  left_join(by_year, by = join_by(TRIP_YEAR)) |>
  mutate(PERCENT_LBS = round(TOTAL_LBS/YEAR_LBS,4),
         TRIP_YEAR = as.numeric(TRIP_YEAR))

# Get LBS and trips across all years
across_years <- data.qcd |>
  summarise(YEAR_LBS = sum(POUNDS_LANDED))

# Get LBS by year and gear
across_years_by_gear <- data.qcd |>
  group_by(GEAR_NM) |>
  summarize(TOTAL_LBS = sum(POUNDS_LANDED),
            TOTAL_TRIPS = n_distinct(TRIP_ID),
            IS_CONFIDENTIAL = case_when(n_distinct(VESSEL_CD) >= 3 ~ "N",
                                        .default = "Y"),
            .groups = "drop") 




# Reproduce non-confidential final, with the confidential information
by_year_group <- data.qcd |>
  group_by(ISLAND_ABV, TRIP_YEAR, 
           ITIS_CODE, ITIS_COMMONNAME, ITIS_SCIENTIFICNAME,
           GEAR_GROUP) |>
  summarize(TOTAL_LBS = sum(POUNDS_LANDED),
            TOTAL_MT = measurements::conv_unit(TOTAL_LBS, "lbs", "metric_ton"),
            TOTAL_TRIPS = n_distinct(TRIP_ID),
            IS_CONFIDENTIAL = case_when(n_distinct(VESSEL_CD) >= 3 ~ "N",
                                        .default = "Y"),
            .groups = "drop")

# anti_join(final, by_year_group, by = 
#             join_by(ISLAND_ABV, TRIP_YEAR, 
#                     ITIS_CODE, ITIS_COMMONNAME, ITIS_SCIENTIFICNAME, 
#                     GEAR_GROUP,TOTAL_LBS, TOTAL_TRIPS, IS_CONFIDENTIAL))

view(by_year_group)
# Very minor slight discrepancy, no issue
# Ask for metric to be added to final

# Get LBS by year to identify main gears
by_year <- data.qcd |>
  group_by(TRIP_YEAR) |>
  summarize(YEAR_LBS = sum(POUNDS_LANDED))

# Get LBS by year and gear
by_year_gear <- data.qcd |>
  group_by(TRIP_YEAR, GEAR_NM) |>
  summarize(TOTAL_LBS = sum(POUNDS_LANDED),
            TOTAL_TRIPS = n_distinct(TRIP_ID),
            IS_CONFIDENTIAL = case_when(n_distinct(VESSEL_CD) >= 3 ~ "N",
                                        .default = "Y"),
            .groups = "drop") |>
  left_join(by_year, by = join_by(TRIP_YEAR)) |>
  mutate(PERCENT_LBS = round(TOTAL_LBS/YEAR_LBS,4),
         TRIP_YEAR = as.numeric(TRIP_YEAR))

# Use subjective 5% cut off to identify main gears
by_year_main_gear <- by_year_gear |>
  filter(PERCENT_LBS >= 0.05)

# Quick view of main gears and note confidential information
view(by_year_main_gear)

# Plot main gears over time - SUPPORTS MY THEORY THAT by hand with scuba/by hand 
    # WAS REPLACED BY spearfishing with scuba
by_year_main_gear |>
  ggplot(aes(x =  TRIP_YEAR, y = TOTAL_LBS, color = GEAR_NM)) +
  geom_line()

# Create list of main gears
main_gears <- unique(by_year_main_gear$GEAR_NM)

# Count main gears
length(main_gears)

# Get sense of how much data is represented in the main gears
sum(by_year_main_gear$TOTAL_LBS)/sum(data.qcd$POUNDS_LANDED)

# Get sense of how much data is represented in the main gears by gear group
by_group <- data.qcd |>
  group_by(GEAR_GROUP) |>
  summarize(TOTAL_LBS = sum(POUNDS_LANDED),
            TOTAL_TRIPS = n_distinct(TRIP_ID),
            TOTAL_CONFIDENTIAL = n_distinct(VESSEL_CD) < 3)

by_group_main_gears <- data.qcd |>
  filter(GEAR_NM %in% main_gears) |>
  group_by(GEAR_GROUP) |>
  summarize(MAIN_LBS = sum(POUNDS_LANDED),
            MAIN_TRIPS = n_distinct(TRIP_ID),
            MAIN_CONFIDENTIAL = n_distinct(VESSEL_CD) < 3)

percent_main <- by_group |>
  left_join(by_group_main_gears, by = join_by(GEAR_GROUP)) |>
  mutate(percent_main = round(MAIN_LBS / TOTAL_LBS, 4))

# Likely to be useful to communicate main groupings components and associated %
view(percent_main)

# 
# by_main_gears_make_up <- data.qcd |>
#   group_by(GEAR_GROUP, GEAR_NM) |>
#   summarize(MAIN_LBS = sum(POUNDS_LANDED),
#             MAIN_TRIPS = n_distinct(TRIP_ID),
#             MAIN_CONFIDENTIAL = n_distinct(VESSEL_CD) < 3) |>
#   left_join(by_group, by = join_by(GEAR_GROUP)) |>
#   mutate(percent_main = round(MAIN_LBS / TOTAL_LBS, 4))
#   
# by_main_gears_make_up <- data.qcd |>
#   group_by(GEAR_GROUP, FIN_GEAR_NAME) |>
#   summarize(MAIN_LBS = sum(POUNDS_LANDED),
#             MAIN_TRIPS = n_distinct(TRIP_ID),
#             MAIN_CONFIDENTIAL = n_distinct(VESSEL_CD) < 3) |>
#   left_join(by_group, by = join_by(GEAR_GROUP)) |>
#   mutate(percent_main = round(MAIN_LBS / TOTAL_LBS, 4))
# 
# by_group_main_gears <- final |>
#   group_by(GEAR_GROUP) |>
#   summarize(MAIN_LBS = sum(TOTAL_LBS))


# tabulate TIP gear groupings ----------------

# by year

yearly_tip_gears_clean <- length_data_glm_2012 |>
  group_by(YEAR, LAND_STANDARD_GEAR_NAME) |>
  summarize(T_TOTAL_TRIPS = n_distinct(ID),
            .groups = "drop")|>
  mutate("GEAR_GROUP" = case_when(
    LAND_STANDARD_GEAR_NAME == "POTS AND TRAPS; FISH" ~ "OTHER",
    TRUE ~ "SCUBA"
  )) 

by_year_tip <- yearly_tip_gears_clean |>
  group_by(YEAR) |>
  summarize(T_YEAR_TRIPS = sum(T_TOTAL_TRIPS))

by_year_tip_scuba <- yearly_tip_gears_clean |>
  filter(GEAR_GROUP == "SCUBA") |> 
  group_by(YEAR) |>
  summarize(T_YEAR_TRIPS_SCUBA = sum(T_TOTAL_TRIPS))

by_year_tip_percentage <- by_year_tip |>
  left_join(by_year_tip_scuba, by = join_by(YEAR)) |>
  mutate(T_percent_scuba_trips = round(T_YEAR_TRIPS_SCUBA/T_YEAR_TRIPS,4),
           YEAR = as.numeric(YEAR))
  
view(by_year_tip_percentage)

# full time series 

tip_all <- by_year_tip_percentage |> 
  summarise(T_TOTAL_TRIPS = sum(T_YEAR_TRIPS),
            T_TOTAL_TRIPS_SCUBA = sum(T_YEAR_TRIPS_SCUBA)) |> 
  mutate(T_PERCENT_SCUBA = round(T_TOTAL_TRIPS_SCUBA / T_TOTAL_TRIPS, 4))

View(tip_all)

# tabulate logbook gear groupings ----------------- 

# by year

yearly_log_gears_clean <- data.qcd |>
  group_by(TRIP_YEAR, GEAR_NM) |>
  summarize(L_TOTAL_TRIPS = n_distinct(TRIP_ID),
            .groups = "drop")|>
  mutate("GEAR_GROUP" = case_when(
    GEAR_NM == "BEACH SEINE" ~ "OTHER",
    GEAR_NM == "CAST NET" ~ "OTHER",
    GEAR_NM == "GILL NET" ~ "OTHER",
    GEAR_NM == "GILL NET, FISHED USING SCUBA" ~ "OTHER",
    GEAR_NM == "GILL NET, SURFACE" ~ "OTHER",
    GEAR_NM == "HANDLINE" ~ "OTHER",
    GEAR_NM == "LOBSTER TRAP" ~ "OTHER",
    GEAR_NM == "NET-UNKNOWN TYPE" ~ "OTHER",
    GEAR_NM == "ROD AND REEL" ~ "OTHER",
    GEAR_NM == "SEINE NET" ~ "OTHER",
    GEAR_NM == "TRAP-UNKNOWN TYPE" ~ "OTHER",
    TRUE ~ "SCUBA"
  )) 

by_year_log <- yearly_log_gears_clean |>
  group_by(TRIP_YEAR) |>
  summarize(L_YEAR_TRIPS = sum(L_TOTAL_TRIPS))

by_year_log_scuba <- yearly_log_gears_clean |>
  filter(GEAR_GROUP == "SCUBA") |> 
  group_by(TRIP_YEAR) |>
  summarize(L_YEAR_TRIPS_SCUBA = sum(L_TOTAL_TRIPS))

by_year_log_percentage <- by_year_log |>
  left_join(by_year_log_scuba, by = join_by(TRIP_YEAR)) |>
  mutate(L_percent_scuba_trips = round(L_YEAR_TRIPS_SCUBA/L_YEAR_TRIPS,4),
         TRIP_YEAR = as.numeric(TRIP_YEAR))

by_year_log_percentage <- by_year_log_percentage |> 
  dplyr::rename(YEAR = TRIP_YEAR)

view(by_year_log_percentage)

# full time series 

log_all <- by_year_log_percentage |> 
  summarise(L_TOTAL_TRIPS = sum(L_YEAR_TRIPS),
            L_TOTAL_TRIPS_SCUBA = sum(L_YEAR_TRIPS_SCUBA)) |> 
  mutate(PERCENT_SCUBA = round(L_TOTAL_TRIPS_SCUBA / L_TOTAL_TRIPS, 4))

View(log_all)


# combine tip and logbook ------------------

# tip summaries have percentage of SCUBA gear group represented 
# summary of tip total - tip_all
# summary of tip by year - by_year_tip_percentage

# logbook summaries have percentage of SCUBA gear group represented 
# summary of logbook total - log_all
# summary of logbook by year - by_year_log_percentage

combo_years <- by_year_log_percentage |> 
  left_join(by_year_tip_percentage, by = join_by(YEAR)) |> 
  mutate(TIP_PERCENT_REPRESENTATION_ALL = round(T_YEAR_TRIPS/L_YEAR_TRIPS*100,2),
         TIP_PERCENT_REPRESENTATION_SCUBA = round(T_YEAR_TRIPS_SCUBA/L_YEAR_TRIPS_SCUBA*100,2))

across_years_TvL <- log_all |> 
  cross_join(tip_all)|> 
  mutate(TIP_PERCENT_REPRESENTATION_ALL = round(T_TOTAL_TRIPS/L_TOTAL_TRIPS*100,2),
         TIP_PERCENT_REPRESENTATION_SCUBA = round(T_TOTAL_TRIPS_SCUBA/L_TOTAL_TRIPS_SCUBA*100,2))

tip_percentage = across_years_TvL$TIP_PERCENT_REPRESENTATION_ALL
scuba_percentage = across_years_TvL$TIP_PERCENT_REPRESENTATION_SCUBA
scuba_trips = across_years_TvL$T_TOTAL_TRIPS_SCUBA
trips_2012 = across_years_TvL$T_TOTAL_TRIPS

# SAVE WORKSPACE ####
save.image(
  file = here::here(
    "tools",
    "sedar_84_stx_stp_2022",
    "stx_stp_2022_landings.RData"
  )
)
