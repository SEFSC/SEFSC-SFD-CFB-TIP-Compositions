# Load libraries
librarian::shelf(here, tidyverse)

# Load objects
load(
  here(
    "data", "raw",
    "yts_sttj_com_landals_1222_20231129_raw_C.RData"
  )
)
# run sttj_yts_figures.R to get length_data_glm_2012

# Quick views
View(data.qcd)

data.qcd |>
  select(GEAR_NM, GEAR_GROUP) |>
  distinct() |>
  arrange(GEAR_GROUP, GEAR_NM)


# tabulate TIP gear groupings ----------------

# by year

yearly_tip_gears_clean <- length_data_glm_2012 |>
  group_by(YEAR, gear) |>
  summarize(
    T_TOTAL_TRIPS = n_distinct(ID),
    .groups = "drop"
  ) |>
  mutate("GEAR_GROUP" = case_when(
    gear == "LINES HAND" ~ "HANDLINE",
    gear == "POTS AND TRAPS; FISH" ~ "FISH_TRAP_SEINE_NET",
    TRUE ~ "OTHER"
  ))

by_year_tip <- yearly_tip_gears_clean |>
  group_by(YEAR) |>
  summarize(T_YEAR_TRIPS = sum(T_TOTAL_TRIPS))

by_year_tip_handline <- yearly_tip_gears_clean |>
  filter(GEAR_GROUP == "HANDLINE") |>
  group_by(YEAR) |>
  summarize(T_YEAR_TRIPS_HANDLINE = sum(T_TOTAL_TRIPS))

by_year_tip_trap <- yearly_tip_gears_clean |>
  filter(GEAR_GROUP == "FISH_TRAP_SEINE_NET") |>
  group_by(YEAR) |>
  summarize(T_YEAR_TRIPS_TRAP = sum(T_TOTAL_TRIPS))

by_year_tip_top <- yearly_tip_gears_clean |>
  filter(GEAR_GROUP != "OTHER") |>
  group_by(YEAR) |>
  summarize(T_YEAR_TRIPS_TOP = sum(T_TOTAL_TRIPS))


by_year_tip_hl_percentage <- by_year_tip |>
  left_join(by_year_tip_handline, by = join_by(YEAR)) |>
  mutate(
    T_percent_handline_trips = round(T_YEAR_TRIPS_HANDLINE / T_YEAR_TRIPS, 4),
    YEAR = as.numeric(YEAR)
  )

by_year_tip_tr_percentage <- by_year_tip_hl_percentage |>
  left_join(by_year_tip_trap, by = join_by(YEAR)) |>
  mutate(
    T_percent_trap_trips = round(T_YEAR_TRIPS_TRAP / T_YEAR_TRIPS, 4),
    YEAR = as.numeric(YEAR)
  )

by_year_tip_top_percentage <- by_year_tip_tr_percentage |>
  left_join(by_year_tip_top, by = join_by(YEAR)) |>
  mutate(
    T_percent_top_trips = round(T_YEAR_TRIPS_TOP / T_YEAR_TRIPS, 4),
    YEAR = as.numeric(YEAR)
  )

view(by_year_tip_top_percentage)

# full time series

tip_all <- by_year_tip_tr_percentage |>
  summarise(
    T_TOTAL_TRIPS = sum(T_YEAR_TRIPS),
    T_TOTAL_TRIPS_HANDLINE = sum(by_year_tip_handline$T_YEAR_TRIPS_HANDLINE),
    T_TOTAL_TRIPS_TRAP = sum(T_YEAR_TRIPS_TRAP),
    T_TOTAL_TRIPS_TOP = sum(by_year_tip_handline$T_YEAR_TRIPS_HANDLINE, T_YEAR_TRIPS_TRAP)
  ) |>
  mutate(
    T_PERCENT_HANDLINE = round(T_TOTAL_TRIPS_HANDLINE / T_TOTAL_TRIPS, 4),
    T_PERCENT_TRAP = round(T_TOTAL_TRIPS_TRAP / T_TOTAL_TRIPS, 4),
    T_PERCENT_TOP = round(T_TOTAL_TRIPS_TOP / T_TOTAL_TRIPS, 4)
  )


View(tip_all)

# tabulate logbook gear groupings -----------------

# by year

yearly_log_gears_clean <- data.qcd |>
  group_by(TRIP_YEAR, GEAR_NM) |>
  summarize(
    L_TOTAL_TRIPS = n_distinct(TRIP_ID),
    .groups = "drop"
  ) |>
  mutate("GEAR_GROUP" = case_when(
    GEAR_NM == "FISH TRAP" ~ "FISH_TRAP_SEINE_NET",
    GEAR_NM == "SEINE NET" ~ "FISH_TRAP_SEINE_NET",
    GEAR_NM == "HANDLINE" ~ "HANDLINE",
    TRUE ~ "OTHER"
  ))

by_year_log <- yearly_log_gears_clean |>
  group_by(TRIP_YEAR) |>
  summarize(L_YEAR_TRIPS = sum(L_TOTAL_TRIPS))

by_year_log_handline <- yearly_log_gears_clean |>
  filter(GEAR_GROUP == "HANDLINE") |>
  group_by(TRIP_YEAR) |>
  summarize(L_YEAR_TRIPS_HANDLINE = sum(L_TOTAL_TRIPS))

by_year_log_trap <- yearly_log_gears_clean |>
  filter(GEAR_GROUP == "FISH_TRAP_SEINE_NET") |>
  group_by(TRIP_YEAR) |>
  summarize(L_YEAR_TRIPS_TRAP = sum(L_TOTAL_TRIPS))

by_year_log_top <- yearly_log_gears_clean |>
  filter(GEAR_GROUP != "OTHER") |>
  group_by(TRIP_YEAR) |>
  summarize(L_YEAR_TRIPS_TOP = sum(L_TOTAL_TRIPS))

by_year_log_hl_percentage <- by_year_log |>
  left_join(by_year_log_handline, by = join_by(TRIP_YEAR)) |>
  mutate(
    L_percent_handline_trips = round(L_YEAR_TRIPS_HANDLINE / L_YEAR_TRIPS, 4)
  )

by_year_log_tr_percentage <- by_year_log_hl_percentage |>
  left_join(by_year_log_trap, by = join_by(TRIP_YEAR)) |>
  mutate(
    L_percent_trap_trips = round(L_YEAR_TRIPS_TRAP / L_YEAR_TRIPS, 4)
  )

by_year_log_top_percentage <- by_year_log_tr_percentage |>
  left_join(by_year_log_top, by = join_by(TRIP_YEAR)) |>
  mutate(
    L_percent_top_trips = round(L_YEAR_TRIPS_TOP / L_YEAR_TRIPS, 4),
    TRIP_YEAR = as.numeric(TRIP_YEAR)
  )

by_year_log_top_percentage <- by_year_log_top_percentage |>
  dplyr::rename(YEAR = TRIP_YEAR)

view(by_year_log_top_percentage)

# full time series

log_all <- by_year_log_tr_percentage |>
  summarise(
    L_TOTAL_TRIPS = sum(L_YEAR_TRIPS),
    L_TOTAL_TRIPS_HANDLINE = sum(L_YEAR_TRIPS_HANDLINE),
    L_TOTAL_TRIPS_TRAP = sum(L_YEAR_TRIPS_TRAP),
    L_TOTAL_TRIPS_TOP = sum(L_YEAR_TRIPS_HANDLINE, L_YEAR_TRIPS_TRAP)
  ) |>
  mutate(
    L_PERCENT_HANDLINE = round(L_TOTAL_TRIPS_HANDLINE / L_TOTAL_TRIPS, 4),
    L_PERCENT_TRAP = round(L_TOTAL_TRIPS_TRAP / L_TOTAL_TRIPS, 4),
    L_PERCENT_TOP = round(L_TOTAL_TRIPS_TOP / L_TOTAL_TRIPS, 4)
  )

View(log_all)


# combine tip and logbook ------------------

# tip summaries have percentage of HANDLINE, TRAP, and top gears combined
# summary of tip total - tip_all
# summary of tip by year - by_year_tip_top_percentage

# logbook summaries have percentage of HANDLINE gear group represented
# summary of logbook total - log_all
# summary of logbook by year - by_year_log_top_percentage

combo_years <- by_year_log_top_percentage |>
  left_join(by_year_tip_top_percentage, by = join_by(YEAR)) |>
  mutate(
    TIP_PERCENT_REPRESENTATION_ALL =
      round(T_YEAR_TRIPS / L_YEAR_TRIPS * 100, 2),
    TIP_PERCENT_REPRESENTATION_HANDLINE =
      round(T_YEAR_TRIPS_HANDLINE / L_YEAR_TRIPS_HANDLINE * 100, 2),
    TIP_PERCENT_REPRESENTATION_TRAP =
      round(T_YEAR_TRIPS_TRAP / L_YEAR_TRIPS_TRAP * 100, 2),
    TIP_PERCENT_REPRESENTATION_TOP =
      round(T_YEAR_TRIPS_TOP / L_YEAR_TRIPS_TOP * 100, 2)
  )

across_years_TvL <- log_all |>
  cross_join(tip_all) |>
  mutate(
    TIP_PERCENT_REPRESENTATION_ALL =
      round(T_TOTAL_TRIPS / L_TOTAL_TRIPS * 100, 2),
    TIP_PERCENT_REPRESENTATION_HANDLINE =
      round(T_TOTAL_TRIPS_HANDLINE / L_TOTAL_TRIPS_HANDLINE * 100, 2),
    TIP_PERCENT_REPRESENTATION_TRAP =
      round(T_TOTAL_TRIPS_TRAP / L_TOTAL_TRIPS_TRAP * 100, 2),
    TIP_PERCENT_REPRESENTATION_TOP =
      round(T_TOTAL_TRIPS_TOP / L_TOTAL_TRIPS_TOP * 100, 2)
  )

tip_percentage <- across_years_TvL$TIP_PERCENT_REPRESENTATION_ALL
# handline_percentage <- across_years_TvL$TIP_PERCENT_REPRESENTATION_HANDLINE
# handline_trips <- across_years_TvL$T_TOTAL_TRIPS_HANDLINE
top_percentage <- across_years_TvL$TIP_PERCENT_REPRESENTATION_TOP
top_trips <- across_years_TvL$T_TOTAL_TRIPS_TOP
trips_2012 <- across_years_TvL$T_TOTAL_TRIPS

# SAVE WORKSPACE ####
save.image(
  file = here::here(
    "tools",
    "sedar_84_sttj_yt_2022",
    "sttj_yt_2022_landings.RData"
  )
)
