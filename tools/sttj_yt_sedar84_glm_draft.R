# Set up library  ####
librarian::shelf(here, tidyverse, ROracle, keyring, dotenv, reshape, openxlsx, janitor, DT, pander, knitr, ggpubr)


# Read in data ####

# Find out the date of the most recent extraction
tip_date <- max(
  as.numeric(gsub(".*?([0-9]+).RDS*", "\\1",
                  list.files(here("data", "raw"),
                             pattern = "com_tip_PR_VI"))))

# Find out the name of the most recent extraction
tip_file <- list.files(here("data", "raw"),
                       pattern = paste0("^com_tip_PR_VI_+", tip_date))


# Read in the most recent extraction
tip <- readRDS(file = here("data", "raw", tip_file))

# Read in initial gear groupings
TIP_gears <- read_csv(here("data", "CSVs", "tip_gears_yts_sttj_landstdgearname.csv"), show_col_types = FALSE)

# Filter and format data ####

# Filter to STTJ and Yellowtail Snapper
sttj_yt <- tip |>
  filter(COUNTY_LANDED %in% c("ST THOMAS", "ST JOHN"),
         OBS_STANDARD_SPECIES_CODE == "168907",
         FISHING_MODE == 'COMMERCIAL',
         LENGTH1_MM > 0) |>
  left_join(TIP_gears, by = join_by(LAND_STANDARD_GEAR_NAME))


# Summary Stats ####

# By gear name
land_gear_name_summary <- sttj_yt |>
  group_by(LAND_GEAR_NAME) %>%
  summarise(
    count = n(),
    mean = mean(LENGTH1_MM, na.rm = TRUE),
    sd = sd(LENGTH1_MM, na.rm = TRUE),
    median = median(LENGTH1_MM, na.rm = TRUE),
    IQR = IQR(LENGTH1_MM, na.rm = TRUE)
  )


# By gear group
gear_group_summary <- sttj_yt |>
  group_by(gear) %>%
  summarise(
    count = n(),
    mean = mean(LENGTH1_MM, na.rm = TRUE),
    sd = sd(LENGTH1_MM, na.rm = TRUE),
    median = median(LENGTH1_MM, na.rm = TRUE),
    IQR = IQR(LENGTH1_MM, na.rm = TRUE)
  )


# Gear name within gear group plots ####

# Gears
unique(sttj_yt$gear) # "Hook and Line", "Trap", "Other", "Net"

use_gear <- sttj_yt |>
  filter(gear == "Trap")


# Create box plot across years
box_plot <- ggboxplot(use_gear, x = "LAND_GEAR_NAME", y = "LENGTH1_MM",
                 color = "LAND_GEAR_NAME",
                 ylab = "LENGTH1_MM", xlab = "Gear")
box_plot
# Create density plot across years
density_plot <- ggdensity(use_gear, x = "LENGTH1_MM",
                          add = "mean", rug = TRUE,
                          color = "LAND_GEAR_NAME", fill = "LAND_GEAR_NAME",
                          ylab = "LENGTH1_MM", xlab = "Gear")
density_plot

# GLMs
