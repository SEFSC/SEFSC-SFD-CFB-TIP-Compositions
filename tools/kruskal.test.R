# https://www.r-bloggers.com/2022/05/how-to-perform-the-kruskal-wallis-test-in-r/#google_vignette

# https://fisheries.org/docs/books/55049C/9.pdf (See section 9.4.2)

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

# Run some summary stats
sttj_yts_summary <- sttj_yt |>
  left_join(sttj_gears) |>
  group_by(LAND_GEAR_NAME) %>%
  summarise(
    count = n(),
    mean = mean(LENGTH1_MM, na.rm = TRUE),
    sd = sd(LENGTH1_MM, na.rm = TRUE),
    median = median(LENGTH1_MM, na.rm = TRUE),
    IQR = IQR(LENGTH1_MM, na.rm = TRUE)
  )

# Create function that will compare the distributions of lenghts acorss vaious gears
test_multi_gear =  function(gears){
  use_gears <- sttj_yt |>
    filter(LAND_GEAR_NAME %in% gears)
  
kt = kruskal.test(LENGTH1_MM ~ LAND_GEAR_NAME, data = use_gears)
  
paste(kt)
}



# Create functions that will compare the distributions of reported lengths
# Only across years where both gears are reported
test_gear_years = function(gear_a, gear_b) {
  
  # Identify the years of overlap across the two gears
  years_a <- sttj_yt |> filter(LAND_GEAR_NAME == gear_a) |> distinct(YEAR)
  years_b <- sttj_yt |> filter(LAND_GEAR_NAME == gear_b) |> distinct(YEAR)
  years_ab <- pull(inner_join(years_a, years_b, by = "YEAR"), YEAR)
  
  # Filter to the gears and years for the comparison
  use_gears <- sttj_yt |>
    filter(LAND_GEAR_NAME %in% c(gear_a, gear_b),
           YEAR %in% years_ab)
  
  # Run Kruskal test
  kt = kruskal.test(LENGTH1_MM ~ LAND_GEAR_NAME, data = use_gears)
  
  # Run Wilcox test
  wt <- pairwise.wilcox.test(use_gears$LENGTH1_MM, use_gears$LAND_GEAR_NAME,
                             p.adjust.method = "BH")
  
  # Create box plot by year
  box_years = ggboxplot(use_gears, x = "YEAR", y = "LENGTH1_MM",
            color = "LAND_GEAR_NAME", palette = c("#00AFBB", "#E7B800"),
            ylab = "LENGTH1_MM", xlab = "Gear",
            orientation = "horizontal")
  
  # Create box plot across years
  box = ggboxplot(use_gears, x = "LAND_GEAR_NAME", y = "LENGTH1_MM",
            color = "LAND_GEAR_NAME", palette = c("#00AFBB", "#E7B800"),
            ylab = "LENGTH1_MM", xlab = "Gear")
  
  density_plot <- ggdensity(use_gears, x = "LENGTH1_MM",
                            add = "mean", rug = TRUE,
                            color = "LAND_GEAR_NAME", fill = "LAND_GEAR_NAME",
                            palette = c("#00AFBB", "#E7B800"),
                            ylab = "LENGTH1_MM", xlab = "Gear")
  
  print(kt)
  print(wt)
  print(box_years)
  print(box)
  print(years_ab)
  print(density_plot)
}


# Create functions that will compare the distributions of reported lengths
# Without filtering to years where both gears are reported
test_gear = function(gear_a, gear_b) {
  
  # Filter to the gears and years for the comparison
  use_gears <- sttj_yt |>
    filter(LAND_GEAR_NAME %in% c(gear_a, gear_b))
  
  # Run Kruskal test
  kt = kruskal.test(LENGTH1_MM ~ LAND_GEAR_NAME, data = use_gears)
  
  # Run Wilcox test
  wt <- pairwise.wilcox.test(use_gears$LENGTH1_MM, use_gears$LAND_GEAR_NAME,
                       p.adjust.method = "BH")
  
  # Create box plot across years
  box <- ggboxplot(use_gears, x = "LAND_GEAR_NAME", y = "LENGTH1_MM",
            color = "LAND_GEAR_NAME", palette = c("#00AFBB", "#E7B800"),
            ylab = "LENGTH1_MM", xlab = "Gear")
  
  # Create density plot across years
  density_plot <- ggdensity(use_gears, x = "LENGTH1_MM",
            add = "mean", rug = TRUE,
            color = "LAND_GEAR_NAME", fill = "LAND_GEAR_NAME",
            palette = c("#00AFBB", "#E7B800"),
            ylab = "LENGTH1_MM", xlab = "Gear")
  
  print(kt)
  print(wt)
  print(box)
  print(density_plot)
}


gears <- sttj_yt |>
  distinct(LAND_GEAR_NAME)

gears
 
# CHECK DIFFERNCE BETWEEN TRAP PAIRS

test_multi_gear(c("POTS AND TRAPS; FISH",
                  "POTS AND TRAPS; SPINY LOBSTER",
                  "POTS AND TRAPS; CMB",
                  "POTS AND TRAPS; BOX TRAP")) # Different 

# CHECK DIFFERNCE BETWEEN TRAP IN PAIRS
test_gear(gear_a = "POTS AND TRAPS; FISH", 
          gear_b = "POTS AND TRAPS; SPINY LOBSTER") # Different
test_gear_years(gear_a = "POTS AND TRAPS; FISH", 
                gear_b = "POTS AND TRAPS; SPINY LOBSTER") # Different

test_gear(gear_a = "POTS AND TRAPS; FISH", 
          gear_b = "POTS AND TRAPS; CMB") #Not different
test_gear_years(gear_a = "POTS AND TRAPS; FISH", 
                gear_b = "POTS AND TRAPS; CMB") #Not Different

test_gear(gear_a = "POTS AND TRAPS; FISH", 
          gear_b = "POTS AND TRAPS; BOX TRAP") #Not Different
test_gear_years(gear_a = "POTS AND TRAPS; FISH", 
                gear_b = "POTS AND TRAPS; BOX TRAP") #No overlap

test_gear(gear_a = "POTS AND TRAPS; SPINY LOBSTER", 
          gear_b = "POTS AND TRAPS; CMB") #Not different
test_gear_years(gear_a = "POTS AND TRAPS; SPINY LOBSTER", 
                gear_b = "POTS AND TRAPS; CMB") #Not Different

test_gear(gear_a = "POTS AND TRAPS; SPINY LOBSTER", 
          gear_b = "POTS AND TRAPS; BOX TRAP") #Different
test_gear_years(gear_a = "POTS AND TRAPS; SPINY LOBSTER", 
                gear_b = "POTS AND TRAPS; BOX TRAP") #No overlap

test_gear(gear_a = "POTS AND TRAPS; CMB", 
          gear_b = "POTS AND TRAPS; BOX TRAP") #Not different
test_gear_years(gear_a = "POTS AND TRAPS; CMB", 
                gear_b = "POTS AND TRAPS; BOX TRAP") #No overlap


# CHECK DIFFERNCE BETWEEN HAUL SEINES and ENCIRCLING NETS (PURSE)
test_gear(gear_a = "HAUL SEINES; BEACH",
          gear_b = "ENCIRCLINLING NETS (PURSE)") #Different
test_gear_years(gear_a = "HAUL SEINES; BEACH",
                gear_b = "ENCIRCLINLING NETS (PURSE)") #NO OVERLAP

test_gear(gear_a = "HAUL SEINES; LONG",
          gear_b = "ENCIRCLINLING NETS (PURSE)") #Not Different
test_gear_years(gear_a = "HAUL SEINES; LONG",
                gear_b = "ENCIRCLINLING NETS (PURSE)") #NO OVERLAP

test_gear(gear_a = "HAUL SEINES; LONG",
          gear_b = "HAUL SEINES; BEACH") #Different
test_gear_years(gear_a = "HAUL SEINES; LONG",
                gear_b = "HAUL SEINES; BEACH") #NO OVERLAP

# CHECK DIFFERNCE BETWEEN (1) LINES HAND & (15) ROD AND REEL
LINES HAND; OTHER
TROLL & HAND LINES CMB
LINES TROLL; OTHER
ROD AND REEL
REEL; ELECTRIC OR HYDRAULIC
ROD AND REEL; ELECTRIC (HAND)
LINES LONG; REEF FISH

test_gear(gear_a = "LINES HAND; OTHER", 
          gear_b = "TROLL & HAND LINES CMB") #Different
test_gear_years(gear_a = "LINES HAND; OTHER", 
          gear_b = "TROLL & HAND LINES CMB") #Not Different

test_gear_years(gear_a = "LINES HAND; OTHER", 
                gear_b = "LINES TROLL; OTHER") #Different

test_gear_years(gear_a = "LINES HAND; OTHER", 
                gear_b = "ROD AND REEL") #Not Different

test_gear_years(gear_a = "LINES HAND; OTHER", 
                gear_b = "REEL; ELECTRIC OR HYDRAULIC") #Not Different
