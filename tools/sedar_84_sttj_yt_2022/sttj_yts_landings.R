# Load libraries
librarian::shelf(here, tidyverse)

# Load objects
load(
  here("data", "raw",
       "yts_sttj_com_landals_1222_20231129_raw_C.RData")
)

# Quick views
View(data.qcd)

data.qcd |>
  select(GEAR_NM, GEAR_GROUP) |>
  distinct() |>
  arrange(GEAR_GROUP, GEAR_NM)
