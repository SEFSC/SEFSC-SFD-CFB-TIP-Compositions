# https://www.r-bloggers.com/2022/05/how-to-perform-the-kruskal-wallis-test-in-r/#google_vignette

# Load libraries ####
librarian::shelf(here, tidyverse, car, ggpubr)

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

# Filter to STTJ and Yellowtail Snapper
sttj_yt <- tip |>
  filter(COUNTY_LANDED %in% c("ST THOMAS", "ST JOHN"),
         OBS_STANDARD_SPECIES_CODE == "168907")

# Get unique list of reported gears and summary of the number of years they are used
sttj_gears <- sttj_yt |>
  mutate(STANDARDGEARNAME_3 = case_when(STANDARDGEARNAME_3 != STANDARDGEARNAME_2 ~
                                          STANDARDGEARNAME_3,
                                        TRUE ~ NA_character_),
         STANDARDGEARNAME_2 = case_when(STANDARDGEARNAME_2 != STANDARDGEARNAME_1 ~
                                          STANDARDGEARNAME_2,
                                        TRUE ~ NA_character_)) |>
  group_by(STANDARDGEARNAME_1,
         STANDARDGEARNAME_2,
         STANDARDGEARNAME_3,
         STANDARDGEARNAME_4,
         STANDARDGEARNAME_5) |>
  summarize(n = sum(QUANTITY, na.rm = TRUE),
            min_year = min(YEAR),
            max_year = max(YEAR),
            years = n_distinct(YEAR),
            n_year = round(n/years)) |>
  arrange(desc(n_year)) |>
  ungroup() |>
  mutate(gear_id = row_number())

# Look at unique list of reported gears
sttj_gears

# Run some more summary stats
sttj_yt |>
  left_join(sttj_gears) |>
  group_by(gear_id) %>%
  summarise(
    count = n(),
    mean = mean(LENGTH1_MM, na.rm = TRUE),
    sd = sd(LENGTH1_MM, na.rm = TRUE),
    median = median(LENGTH1_MM, na.rm = TRUE),
    IQR = IQR(LENGTH1_MM, na.rm = TRUE)
  )


# Create functions that will compare the distributions of reported lengths
# Only across years where both gears are reported
test_gear_years = function(gear_a, gear_b) {
  
  # Merge gear_id assignments to STTJ YTS data set
  prep_gears <- sttj_yt |>
    left_join(sttj_gears) |> 
    mutate(gear_id = as.factor(gear_id))
  
  # Identify the years of overlap across the two gears
  years_a <- prep_gears |> filter(gear_id == gear_a) |> distinct(YEAR)
  years_b <- prep_gears |> filter(gear_id == gear_b) |> distinct(YEAR)
  years_ab <- pull(inner_join(years_a, years_b, by = "YEAR"), YEAR)
  
  # Filter to the gears and years for the comparison
  use_gears <- prep_gears |>
    filter(gear_id %in% c(gear_a, gear_b),
           YEAR %in% years_ab)
  
  # Run Kruskal test
  kt = kruskal.test(LENGTH1_MM ~ gear_id, data = use_gears)
  
  # Run Wilcox test
  wt <- pairwise.wilcox.test(use_gears$LENGTH1_MM, use_gears$gear_id,
                             p.adjust.method = "BH")
  
  # Create box plot by year
  box_years = ggboxplot(use_gears, x = "YEAR", y = "LENGTH1_MM",
            color = "gear_id", palette = c("#00AFBB", "#E7B800"),
            ylab = "LENGTH1_MM", xlab = "Gear",
            orientation = "horizontal")
  
  # Create box plot across years
  box = ggboxplot(use_gears, x = "gear_id", y = "LENGTH1_MM",
            color = "gear_id", palette = c("#00AFBB", "#E7B800"),
            ylab = "LENGTH1_MM", xlab = "Gear")
  
  print(kt)
  print(wt)
  print(box_years)
  print(box)
  print(years_ab)
}


# Create functions that will compare the distributions of reported lengths
# Without filtering to years where both gears are reported
test_gear = function(gear_a, gear_b) {
  
  # Merge gear_id assignments to STTJ YTS data set
  prep_gears <- sttj_yt |>
    left_join(sttj_gears) |> 
    mutate(gear_id = as.factor(gear_id))
  
  # Filter to the gears and years for the comparison
  use_gears <- prep_gears |>
    filter(gear_id %in% c(gear_a, gear_b))
  
  # Run Kruskal test
  kt = kruskal.test(LENGTH1_MM ~ gear_id, data = use_gears)
  
  # Run Wilcox test
  wt <- pairwise.wilcox.test(use_gears$LENGTH1_MM, use_gears$gear_id,
                       p.adjust.method = "BH")
  
  # Create box plot across years
  box <- ggboxplot(use_gears, x = "gear_id", y = "LENGTH1_MM",
            color = "gear_id", palette = c("#00AFBB", "#E7B800"),
            ylab = "LENGTH1_MM", xlab = "Gear")
  
  # Create density plot across years
  density_plot <- ggdensity(use_gears, x = "LENGTH1_MM",
            add = "mean", rug = TRUE,
            color = "gear_id", fill = "gear_id",
            palette = c("#00AFBB", "#E7B800"),
            ylab = "LENGTH1_MM", xlab = "Gear")
  
  print(kt)
  print(wt)
  print(box)
  print(density_plot)
}

# CHECK DIFFERNCE BETWEEN (8) POTS AND TRAPS, FISH and (31) POTS AND TRAPS, SPINY LOBSTER
test_gear(8,31) #Not different
test_gear_years(8,31) #Not different

# CHECK DIFFERNCE BETWEEN (6) HAUL SEINES and (9) ENCIRCLING NETS (PURSE)
test_gear(6,9) #Not different
test_gear_years(6,9) #Not different

# CHECK DIFFERNCE BETWEEN (1) LINES HAND & (15) ROD AND REEL
test_gear(1,15) #Not different
test_gear_years(1,15) #Not different

# CHECK DIFFERNCE BETWEEN (1) LINES HAND & (15) ROD AND REEL
test_gear(1,15) #Not different
test_gear_years(1,15) #Not different

# CHECK DIFFERNCE BETWEEN (1) LINES HAND & (2) LINES POWER TROLL OTHER
test_gear(1,2) #Different
test_gear_years(1,2) #Different

# CHECK DIFFERNCE BETWEEN (1) LINES HAND & (2) LINES POWER TROLL OTHER
test_gear(1,2) #Different
test_gear_years(1,2) #Different

# CHECK DIFFERNCE BETWEEN (1) LINES HAND & (11) REEL, ELECTRIC OR HYDRAULIC
test_gear(1,11) #Different
test_gear_years(1,11) #Not different
