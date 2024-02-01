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

anti_join(final, by_year_group, by = 
            join_by(ISLAND_ABV, TRIP_YEAR, 
                    ITIS_CODE, ITIS_COMMONNAME, ITIS_SCIENTIFICNAME, 
                    GEAR_GROUP,TOTAL_LBS, TOTAL_TRIPS, IS_CONFIDENTIAL))

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

# Plot main gears over time
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


by_main_gears_make_up <- data.qcd |>
  group_by(GEAR_GROUP, GEAR_NM) |>
  summarize(MAIN_LBS = sum(POUNDS_LANDED),
            MAIN_TRIPS = n_distinct(TRIP_ID),
            MAIN_CONFIDENTIAL = n_distinct(VESSEL_CD) < 3) |>
  left_join(by_group, by = join_by(GEAR_GROUP)) |>
  mutate(percent_main = round(MAIN_LBS / TOTAL_LBS, 4))
  
by_main_gears_make_up <- data.qcd |>
  group_by(GEAR_GROUP, FIN_GEAR_NAME) |>
  summarize(MAIN_LBS = sum(POUNDS_LANDED),
            MAIN_TRIPS = n_distinct(TRIP_ID),
            MAIN_CONFIDENTIAL = n_distinct(VESSEL_CD) < 3) |>
  left_join(by_group, by = join_by(GEAR_GROUP)) |>
  mutate(percent_main = round(MAIN_LBS / TOTAL_LBS, 4))

by_group_main_gears <- final |>
  group_by(GEAR_GROUP) |>
  summarize(MAIN_LBS = sum(TOTAL_LBS))
