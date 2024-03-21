# adyan's

# Load libraries ####
librarian::shelf(here, tidyverse, scales)

# Specify settings ####
tip_spp_len_rds <- "stx_slp_prep_tip_20240123.rds"
spp <- "slp"
isl <- "stx"
subset_years <- c(2012:2022)
weight_lim <- 2.5
length_lim <- 50
len_type <- "FORK LENGTH"
len_mode <- "COMMERCIAL"
species <- "Stoplight Parrrotfish"
island <- "St. Croix"

# Read in formatted data ####
tip_spp_len <- readRDS(here::here("data", tip_spp_len_rds))


# Plot length and weight cm and kg ####
length_weight_prep <- tip_spp_len |>
  dplyr::mutate(
    within_lim = length1_cm <= length_lim &
      obs_weight_kg <= weight_lim
  )

n <- nrow(length_weight_prep)
n_kept <- sum(length_weight_prep$k_keep == TRUE, na.rm = TRUE)
percent_kept <- round(n_kept / n * 100, 1)
n_out_of_range <- sum(length_weight_prep$within_lim == FALSE, na.rm = TRUE)
percent_out_of_range <- round(n_out_of_range / n * 100, 1)
min_year <- min(length_weight_prep$year) 
max_year <- max(length_weight_prep$year) 

length_weight_caption <- str_wrap(paste0(
  "Of ", number(n, big.mark = ","), " samples taken from ", min_year, " to ", max_year, 
  ", there are ", number(n_kept, big.mark = ","), " samples kept when filtering by condition factor (", percent_kept,
  "%). There were ", n_out_of_range, 
  " samples that fell outside of the range plotted (", percent_out_of_range, "%)."
), 75)

length_weight_plot <- length_weight_prep |>
  filter(
    !is.na(k_keep),
    within_lim == TRUE
  ) |>
  ggplot() +
  aes(
    x = length1_cm,
    y = obs_weight_kg,
    color = k_keep
  ) +
  geom_point(alpha = 0.2) +
  ylim(0, weight_lim) +
  xlim(0, length_lim) +
  labs(
    x = paste(str_to_title(len_type), "(cm)"),
    y = "Weight (kg)",
    title = "TIP lengths and weights filtered by condition factor",
    subtitle = paste(str_to_title(len_mode), island, species),
    caption = length_weight_caption
  ) +
  theme(plot.caption = element_text(hjust = 0.5)) +
  guides(color = "none")
length_weight_plot

# Plot length and weight inch and lbs ####
length_weight_plot_imp <- length_weight_prep |>
  filter(
    !is.na(k_keep),
    within_lim == TRUE
  ) |>
  ggplot() +
  aes(
    x = length1_inch,
    y = obs_weight_lbs,
    color = k_keep
  ) +
  geom_point(alpha = 0.2) +
  ylim(0, measurements::conv_unit(weight_lim, "kg", "lbs")) +
  xlim(0, measurements::conv_unit(length_lim, "cm", "inch")) +
  labs(
    x = paste0(str_to_title(len_type), " (inches)"),
    y = "Weight (lbs)",
    title = "TIP lengths and weights filtered by condition factor",
    subtitle = paste(str_to_title(len_mode), island, species),
    caption = length_weight_caption
  ) +
  theme(plot.caption = element_text(hjust = 0.5)) +
  guides(color = "none")
length_weight_plot_imp