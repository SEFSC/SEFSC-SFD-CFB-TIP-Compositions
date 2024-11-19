# 05c_nonconf figures requested for sedar


# 05_plot

#' plot the filtered data included
#' 1 - gears across time
#' 2 - aggregated density of lengths 


# Load libraries ####
librarian::shelf(here, tidyverse, flextable, ggplot2, janitor)

# Specify settings ####
# date from end of 05a script
date <- "20241108" 
# date from non-confidential gears (rds from 04a)
gear_date <- "20241108" 
spp <- "csl"
print_spp <- "Caribbean Spiny Lobster"

# isl <- "pr"
# print_isl <- "Puerto Rico"
# isl <- "stx"
# print_isl <- "St. Croix"
isl <- "stt"
print_isl <- "St. Thomas/St. John"

min_year <- 1981
max_year <- 2023
len_type <- "CARAPACE LENGTH"
target_len <- "Carapace Length"
sedar <- "sedar91"
disclaimer <- "Only nonconfidential data shown."
bottomBin <- 2

# Read in formatted data ####
tip_spp_rds <- paste0(isl, "_", spp, "_clean_filtered_", date, ".rds" )
tip_spp <- readRDS(here::here("data",  sedar, "rds", spp, isl, tip_spp_rds))

gear_list <- paste0(isl, "_", spp, "_nonconf_gear_list_", gear_date, ".rds" )
gear_nonconf <- readRDS(here::here("data",  sedar, "rds", spp, isl, gear_list))

# create non-confidential gears dataframe ####
tip_spp_nc <- gear_nonconf |> 
  select(Gear, confidential) |> 
  mutate(gear = as.character(Gear)) |> 
  select(-Gear) |> 
  right_join(tip_spp, by = join_by(gear)) |> 
  filter(confidential == "FALSE")

# SUM OVER QUANTITY TO GET SAMPLE SIZES (USE THAT VARIABLE AS INTENDED)

dataIsl <- tip_spp_nc |> 
  mutate(lengthSS = round(length1_mm))

#GET BINS THAT START ON A QUARTER INCH
#THIS MEANS SOMETIMES 6 mm AND SOMETIMES 7 mm BINS
bottomBin = 25 #STARTING AT 2 INCHES (25.4 mm)
lengths <- c(bottomBin:max(dataIsl$lengthSS))

# create bin sizes
qtr_in = seq(2,12,.25) #STARTING AT 2 INCHES
qtr_in_mm = round(qtr_in*25.4)
binSizes <- qtr_in_mm[2:41]-qtr_in_mm[1:40]
binPrep <- data.frame(cbind(bin = qtr_in_mm[1:40], space = binSizes))
binVector <- uncount(binPrep, space)$bin
lengths <- seq(bottomBin, max(binVector), 1)
bin <- binVector[1:length(lengths)]
bin_merge <- data.frame(cbind(lengths, bin))
binSize <- "mm_qtrINCH"

# create tables of binned data 
allCombo <- expand.grid(lengths = c(min(dataIsl$lengthSS):max(dataIsl$lengthSS)),
                        year = c(min_year : max_year),
                        # SEX = c("FEMALE", "MALE"),
                        gear = unique(dataIsl$gear),
                        island = isl, stringsAsFactors = FALSE) %>%
  left_join(., bin_merge, by = "lengths")

minKeep = 25

dataIsl_binned <- dataIsl %>% 
  filter(
    # SEX %in% c("FEMALE", "MALE"),
         year %in% c(min_year : max_year),
         lengthSS >= minKeep) %>%
  full_join(., allCombo, by = c("island", 
                                "gear",
                                # "SEX", 
                                "year", "lengthSS" = "lengths")) %>%
  mutate(bin = ifelse(bin == bottomBin, minKeep, bin))


tip_bin <- dataIsl_binned |> 
  select( island,
    # sampling_program, 
         year, 
         gear,
         lengthSS,
         bin)

# Plot gears by number of fish measured used over time ####
bin_data <- tip_bin |>
  group_by(year, bin) |>
  dplyr::summarize(n = n(), .groups = "drop") |>
  mutate(year = as.integer(year))

bin_by_yr <- bin_data |>
  # group_by(bin) |>
  # dplyr::mutate(total_n = sum(n)) |>
  # ungroup() |>
  # dplyr::mutate(gearn = fct_reorder(lengthSS, total_n)) %>%
  ggplot(aes(x = year, y = bin, 
             # color = bin, 
             size = n)) +
  geom_point() +
  labs(
    x = "Year", y = "Length (mm)", colour = "", shape = "",
    title = paste(print_isl, "Length Samples"),
    caption = disclaimer
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom", 
    text = element_text(size = 15),
    title = element_text(size = 15)
  )
# view
  bin_by_yr

length4 <- tip_bin %>%
  # filter(Type == "N") %>%
  ggplot(aes(x = year, y = lengthSS, 
             # fill = sampling_program,
             )) +
  geom_bar(stat = 'identity') +
  # scale_fill_manual(values=c(PairedColor12Steps[c(12,10)])) +
  theme_bw() +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=10)) +
  guides(fill = "none") +
  labs(x = "Year",
       y = "Annual sample size of\ncarapace length measurements\n") + 
  # geom_vline(xintercept = 2016.5, linetype = "dashed") +
  scale_y_continuous(label=scales::comma)

# Plot length values recorded over time ####
length_bin <- tip_spp_count |>
  ggplot(aes(
    x = date,
    y = length1_cm,
    group = island,
    color = length_type1
  )) +
  # facet_wrap(~islandn, ncol = 2) +
  # facet_grid(species_code ~ island) +
  geom_point() +
  labs(
    x = "Year", y = "Length (cm)",
    title = paste(print_isl,"Area-time distribution of Lengths sampled"),
    color = "Length Type (# obs)",
    subtitle = paste("N = ", nrow(tip_spp_count))
  )
# view plot  
length_bin

