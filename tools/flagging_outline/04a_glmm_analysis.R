# 04_glmm_analysis

#' execute statistical analysis of gear-length relationships (glmm) and
#' k calculations to determine possible gear groupings and data outliers

# Load libraries ####
librarian::shelf(here, tidyverse, flextable, ggplot2, ggpubr, lmerTest, meantables)

# Specify settings ####
tip_spp_rds <- "pr_yts_prep_tip_20240628.rds" # rds from end of 03a script
spp <- "yts"
isl <- "pr"
break_year <- 2012
print_isl <- "Puerto Rico"


# Read in formatted data ####
tip_spp <- readRDS(here::here("data", tip_spp_rds))

# GLMM model analysis ####
tip_spp_glm <- tip_spp |>
  select(year, date, id, island, length1_cm, gear)

# plot data

# Create box plot across years
box_plot <- ggboxplot(
  tip_spp_glm,
  x = "gear", y = "length1_cm",
  color = "gear",
) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 10),
    legend.position = "none"
  ) +
  labs(
    x = "Gear", y = "Length(cm)", colour = "", shape = "",
    title = paste(print_isl, "Length Samples")
  )
box_plot

 # Create density plot across years
density_plot <- ggdensity(
  tip_spp_glm,
  x = "length1_cm",
  add = "mean", rug = TRUE
  # color = "gear", fill = "gear",
) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 10),
    legend.position = "none"
  ) +
  labs(
    x = "All Gears", y = "Length(cm)", colour = "", shape = "",
    title = paste(print_isl, "Length Samples")
  )
density_plot

#' filtered to gears with 30 or more occurrences for the purposes
#' of plotting visibility
allgears_glm_plot <- tip_spp_glm %>%
  group_by(gear) %>%
  filter(n() >= 30) %>%
  ungroup() |>
  ggplot(aes(x = date, y = length1_cm)) +
  geom_point(aes(colour = gear, shape = gear), size = 1, alpha = 0.5) +
  scale_shape_manual(values = c(
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
    13, 14, 15, 16, 17, 18, 21, 22, 23
  )) +
  geom_smooth(method = "lm", formula = "y ~ x", col = "black") +
  # facet_wrap(~ COUNTY_LANDED) +
  labs(x = "", y = "Length (cm)", colour = "", shape = "") +
  theme_bw() +
  theme(
    legend.position = "bottom", legend.text = element_text(size = 7),
    legend.box.spacing = unit(0, "npc"), panel.grid = element_blank()
  ) +
  guides(colour = guide_legend(override.aes = list(size = 2)))
allgears_glm_plot

## Comparing length to date and gear in a gamma full model ####
mod2 <- glmer(length1_cm ~ 
                # scale(date) + 
                gear + 
                (1 | year) + 
                (1 | id),
  data = tip_spp_glm,
  family = Gamma(link = log)
)

# pairwise comparisons - compares each gear to each other and gives p value
mod_contr <- emmeans::emmeans(
  object = mod2,
  pairwise ~ "gear",
  adjust = "tukey"
)

# cld provides gear groupings based on which gears are
# similar vs significantly different from each other
allgears_multcompcld <- multcomp::cld(object = mod_contr$emmeans)

# Count number of length records (aka number of fish measured)
length_data_fishcount <- tip_spp |>
  group_by(gear) |>
  tally()
# Count number of unique trip interviews (aka unique id)
length_data_tripcount <- aggregate(
  data = tip_spp, # Applying aggregate
  id ~ gear,
  function(id) length(unique(id))
)

# Add counts to GLMM table
allgears_multcompcld_fish <- full_join(allgears_multcompcld,
  length_data_fishcount,
  by = "gear"
)
allgears_multcompcld_trip <- full_join(allgears_multcompcld_fish,
  length_data_tripcount,
  by = "gear"
)

## create table of stats ####
# Clean table of GLMM and summary stats and filter to gears with more than 3
# unique interviews
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
    "Interview(n)" = "id"
  ) |>
  dplyr::filter(`Interview(n)` >= 3) |>
  select(
    Gear,
    "Estimated Marginal Mean",
    LCL,
    UCL,
    Group,
    "Fish(n)",
    "Interview(n)",
    Percentage
  ) |>
  # Add gear groups related to each statistical group given by GLMM
  ### FIX THESE GEAR GROUPINGS
  mutate("Gear Group" = case_when(
    Gear == "LINES HAND" ~ "Hand Line",
    Gear == "POTS AND TRAPS; FISH" ~ "Traps",
    Gear == "HAUL SEINES" ~ "Hand Line or Traps",
    Gear == "POTS AND TRAPS; CMB" ~ "Hand Line or Traps",
    Gear == "POTS AND TRAPS;SPINY LOBSTER" ~ "Hand Line or Traps",
    Gear == "ROD AND REEL" ~ "Rod and Reel",
    TRUE ~ "Hand Line, Traps, or Rod and Reel"
  ))

# Create table of means for each gear
mean_allgears <- tip_spp %>%
  group_by(gear) %>%
  dplyr::mutate(n_ID = n_distinct(id)) |>
  dplyr::filter(n_ID >= 3) |>
  mean_table(length1_cm) |>
  dplyr::rename(
    "Gear" = "group_cat",
    "Mean" = "mean"
  ) |>
  select(Gear, Mean)

# Join means to GLMM/summary stats table
allgears_multicom_mean <- full_join(mean_allgears,
  allgears_multcompcld_final,
  by = "Gear"
)

# Arrange table in decreasing order of gear percent representation
allgears_multicom_mean_final <- allgears_multicom_mean |>
  arrange(desc(Percentage))
# dplyr::filter(`Interview(n)` >= 3)


glm_tbl <- flextable(allgears_multicom_mean_final) |>
  theme_box() %>%
  align(align = "center", part = "all") %>%
  fontsize(size = 8, part = "all") %>%
  autofit()

# list of gears that represent >2% of lengths each
grtr2percent_gears <- allgears_multicom_mean_final |>
  filter(Percentage > 2)

## Save gears >2% representation ####
saveRDS(
  grtr2percent_gears,
  file = here::here(
    "data",
    paste0(
      isl, "_",
      spp, "_clean_gear_list_",
      format(Sys.time(), "%Y%m%d"), ".rds"
    )
  )
)

# Repeat GLMM with data after data break year ####

tip_spp_break_year <- tip_spp |>
  filter(year >= break_year) |>
  select(year, interview_date, id, island, length1_cm, gear)

# plot data

allgears_glm_plot_break <- tip_spp_break_year |>
  ggplot(aes(x = as.Date(interview_date), y = length1_cm)) +
  geom_point(aes(colour = gear, shape = gear), size = 1, alpha = 0.5) +
  geom_smooth(method = "lm", formula = "y ~ x", col = "black") +
  # facet_wrap(~ COUNTY_LANDED) +
  labs(x = "", y = "Length (cm)", colour = "", shape = "") +
  theme_bw() +
  theme(
    legend.position = "bottom", legend.text = element_text(size = 15),
    legend.box.spacing = unit(0, "npc"), panel.grid = element_blank()
  ) +
  guides(colour = guide_legend(override.aes = list(size = 2)))


## fit models ####

# comparing length to date and gear in a gamma full model
mod2 <- glmer(FL_CM ~ scale(FINAL_DATE) + gear + (1 | YEAR) + (1 | ID),
  data = tip_spp_break_year, family = Gamma(link = log)
)

# pairwise comparisons (if needed)
mod_contr <- emmeans::emmeans(
  object = mod2,
  pairwise ~ "gear",
  adjust = "tukey"
)

allgears_multcompcld_break <- multcomp::cld(object = mod_contr$emmeans)

length_data_fishcount_break <- tip_spp_break_year |>
  group_by(gear) |>
  tally()
length_data_tripcount_break <- aggregate(
  data = tip_spp_break_year, # Applying aggregate
  ID ~ gear,
  function(id) length(unique(id))
)

allgears_multcompcld_fish_break <-
  full_join(allgears_multcompcld_break,
    tip_spp_break_year,
    by = "gear"
  )
allgears_multcompcld_trip_break <-
  full_join(allgears_multcompcld_fish_break,
    length_data_tripcount_break,
    by = "gear"
  )

## create table of stats ####
allgears_multcompcld_finaL_break <- allgears_multcompcld_trip_break |>
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
    "Interview(n)" = "id"
  ) |>
  # filter("Interview(n)" >= 3) |>
  dplyr::filter(`Interview(n)` >= 3) |>
  select(
    Gear,
    "Estimated Marginal Mean",
    LCL,
    UCL,
    Group,
    "Fish(n)",
    "Interview(n)",
    Percentage
  ) |>
  # group gears based off of numbered groups from glmmm
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
mean_allgears2012 <- length_data_glm_2012 %>%
  group_by(gear) %>%
  dplyr::mutate(n_ID = n_distinct(ID)) |>
  dplyr::filter(n_ID >= 3) |>
  mean_table(FL_CM) |>
  dplyr::rename(
    "Gear" = "group_cat",
    "Mean" = "mean"
  ) |>
  select(Gear, Mean)

allgears_multicom_mean2012 <- full_join(mean_allgears2012, 
                                        allgears_multcompcld_finaL_2012, 
                                        by = "Gear")

allgears_multicom_mean_final2012 <- allgears_multicom_mean2012 |>
  arrange(desc(Percentage))


glm_tlb2 <- flextable(allgears_multicom_mean_final2012) |>
  theme_box() %>%
  align(align = "center", part = "all") %>%
  fontsize(size = 8, part = "all") %>%
  autofit()


# Create pretty table for export purposes
glmm_gear_summary_table <- flextable(allgears_multicom_mean_final) |>
  theme_box() %>%
  align(align = "center", part = "all") %>%
  fontsize(size = 8, part = "all") %>%
  autofit()

# Obtain lower and upper estimates of k ####
tip_k_iqr <- IQR(tip_spp$k, na.rm = TRUE)
tip_k_25q <- quantile(tip_spp$k, 0.25, na.rm = TRUE)
tip_k_75q <- quantile(tip_spp$k, 0.75, na.rm = TRUE)
tip_k_lower <- tip_k_25q - 1.5 * tip_k_iqr
tip_k_upper <- tip_k_75q + 1.5 * tip_k_iqr

# list results 
tip_k_iqr # 0.3245058
tip_k_25q # 1.359978
tip_k_75q # 1.684483
tip_k_lower # 0.8732189
tip_k_upper # 2.171242

# Implement filtering system to remove outliers ####
#' For our purposes of the SEDAR 84 size comp, we filtered out interviews with
#' less than 3 unique interview IDs per gear
#' Other filters could include removing gears with less than 30 recorded lengths,
#' years with less than 30 recorded lengths, years with less than 10 unique
#' interview IDs, or k more or less than upper and lower limits set above

# filtered to gears with more than 3 unique trip IDs
tip_clean <- tip_spp %>%
  group_by(gear) %>%
  dplyr::mutate(n_ID = n_distinct(id)) |>
  dplyr::filter(n_ID >= 3) |>
  ungroup()


# Save cleaned tip_clean ####
saveRDS(
  tip_clean,
  file = here::here(
    "data",
    paste0(
      isl, "_",
      spp, "_clean_tip_",
      format(Sys.time(), "%Y%m%d"), ".rds"
    )
  )
)
