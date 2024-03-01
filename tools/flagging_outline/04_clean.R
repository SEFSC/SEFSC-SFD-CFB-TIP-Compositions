# 04_clean

#' execute statistical analysis of gear-length relationships (glmm) and
#' k calculations to determine possible gear groupings and data outliers

# Load libraries ####
librarian::shelf(here, tidyverse, flextable, ggplot2, lmerTest, meantables)

# Specify settings ####
tip_spp_rds <- "pr_yts_prep_tip_20240229.rds" # rds from end of 02 script
spp <- "yts"
isl <- "pr"

# Read in formatted data ####
tip_spp <- readRDS(here::here("data", tip_spp_rds))

# GLMM model analysis #### 
# comparing length to date and gear in a gamma full model
mod2 <- glmer(length1_cm ~ scale(interview_date) + gear + (1 | year) + (1 | id),
  data = tip_spp, 
  family = Gamma(link = log)
)

# pairwise comparisons - compares each gear to eachother and gives p value
mod_contr <- emmeans::emmeans(object = mod2, 
                              pairwise ~ "gear", 
                              adjust = "tukey")

# cld provides gear groupings based on which gears are 
# similar vs significantly different from each other
allgears_multcompcld <- multcomp::cld(object = mod_contr$emmeans)

length_data_fishcount <- tip_spp |>
  group_by(gear) |>
  tally()
length_data_tripcount <- aggregate(
  data = tip_spp, # Applying aggregate
  ID ~ gear,
  function(ID) length(unique(ID))
)

allgears_multcompcld_fish <- full_join(allgears_multcompcld, 
                                       length_data_fishcount, 
                                       by = "gear")
allgears_multcompcld_trip <- full_join(allgears_multcompcld_fish, 
                                       length_data_tripcount, 
                                       by = "gear")

allgears_multcompcld_final <- allgears_multcompcld_trip |>
  mutate(
    Percentage = round(n / sum(n) * 100, 2),
    emmean = round(emmean, 2),
    asymp.LCL = round(asymp.LCL, 2),
    asymp.UCL = round(asymp.UCL, 2)
  ) |>
  dplyr::rename(
    "Group" = ".group",
    "Gear" = "gear",
    "Estimated Marginal Mean" = "emmean",
    "LCL" = "asymp.LCL",
    "UCL" = "asymp.UCL",
    "Fish(n)" = "n",
    "Interview(n)" = "ID"
  ) |>
  # filter("Interview(n)" >= 3) |>
  dplyr::filter(`Interview(n)` >= 3) |>
  select(Gear, 
         "Estimated Marginal Mean",
         LCL, 
         UCL, 
         Group, 
         "Fish(n)", 
         "Interview(n)", 
         Percentage) |>
  mutate("Gear Group" = case_when(
    Gear == "LINES HAND" ~ "Hand Line",
    Gear == "POTS AND TRAPS; FISH" ~ "Traps",
    Gear == "HAUL SEINES" ~ "Hand Line or Traps",
    Gear == "POTS AND TRAPS; CMB" ~ "Hand Line or Traps",
    Gear == "POTS AND TRAPS;SPINY LOBSTER" ~ "Hand Line or Traps",
    Gear == "ROD AND REEL" ~ "Rod and Reel",
    TRUE ~ "Hand Line, Traps, or Rod and Reel"
  ))

# create table of means for each gear

mean_allgears <- length_data_glm %>%
  group_by(gear) %>%
  dplyr::mutate(n_ID = n_distinct(ID)) |>
  dplyr::filter(n_ID >= 3) |>
  mean_table(FL_CM) |>
  dplyr::rename(
    "Gear" = "group_cat",
    "Mean" = "mean"
  ) |>
  select(Gear, Mean)

allgears_multicom_mean <- full_join(mean_allgears,
  allgears_multcompcld_final,
  by = "Gear"
)

allgears_multicom_mean_final <- allgears_multicom_mean |>
  arrange(desc(Percentage)) #|>
# dplyr::filter(`Interview(n)` >= 3)


tbl1 <- flextable(allgears_multicom_mean_final) |>
  theme_box() %>%
  align(align = "center", part = "all") %>%
  fontsize(size = 8, part = "all") %>%
  autofit()
