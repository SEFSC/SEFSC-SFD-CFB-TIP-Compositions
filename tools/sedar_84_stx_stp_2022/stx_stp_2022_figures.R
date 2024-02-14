# This script will be expanding upon the length-comp script c1_nominal_len_comps.Rmd
# created by Molly Stevens.
# M:\SFD\SECM-SFD\_Assessments-SEDAR\SEDAR_57_Caribbean_Spiny_Lobster\Analyses\
# StockSynthesis\2022 Update\Documentation\SEFSC-SEDAR-57U-CR-LOB-2021

# Stoplight Parrotfish STX ####

# Set up library

librarian::shelf(
  here, tidyverse, dotenv, reshape, openxlsx, janitor, DT,
  pander, knitr, flextable, ggplot2, lmerTest, meantables, styler
)
# add ROracle, keyring when pulling new data from TIP database
# if conflicts in pkgs arise, use the following:
# library(conflicted)
# conflicted::conflicts_prefer(here::here)

# reference for working paper
# cr_tip_sp(state_codes = c('PR', 'VI'), sp_codes = "170867")

# # Find out the date of the most recent extraction
# tip_date <- max(
#   as.numeric(gsub(".*?([0-9]+).RDS*", "\\1",
#                   list.files(here("data", "raw"),
#                              pattern = "com_tip_PR_VI"))))
#
# # Find out the name of the most recent extraction
# tip_file <- list.files(here("data", "raw"),
#                        pattern = paste0("^com_tip_PR_VI_+", tip_date))


# Read in the most recent extraction
# tip <- readRDS(file = here("data", "raw", tip_file))
# tip <- readRDS("~/SEFSC-SFD-CFB-TIP-Compositions/data/raw/com_tip_PR_VI_170867_20240109.RDS")

tip <- readRDS(here::here("data", "raw", "com_tip_PR_VI_170867_20240131.RDS"))

# Filter to STTJ and Yellowtail Snapper AND create new filterable date value
stx_slp <- tip |>
  filter(
    COUNTY_LANDED == "ST CROIX",
    OBS_STANDARD_SPECIES_CODE == "170867"
  ) |>
  mutate(
    TEST_DATE = as.Date(ymd_hms(INTERVIEW_DATE)),
    FINAL_DATE = case_when(
      is.na(TEST_DATE) ~ INTERVIEW_DATE,
      TRUE ~ TEST_DATE
    )
  )


source("~/SEFSC-SFD-CFB-TIP-Compositions/tools/functions/len_len_convert.R")
source("~/SEFSC-SFD-CFB-TIP-Compositions/tools/functions/fig_format_export.R")

LLconv <- read_csv("~/SEFSC-SFD-CFB-TIP-Compositions/tools/CSVs/LLconversions.csv",
  show_col_types = FALSE
)

TIP_gears <- read_csv("~/SEFSC-SFD-CFB-TIP-Compositions/tools/CSVs/tip_gears_stp_stx_LANDSTDGEARNAME.csv",
  show_col_types = FALSE
)


## Control Settings and Exploratory Frequency Tables

sp <- "SLP"

region <- "PUERTO RICO - USVI"

county <- "ST CROIX"

bin_size <- 1

len_type <- "FORK LENGTH"


# Calculate data available ####
flextable(as.data.frame(table(stx_slp$LENGTH_TYPE1, useNA = "always"))) %>%
  autofit()

n_fork_len <- sum(stx_slp$LENGTH_TYPE1 == "FORK LENGTH")
n_all_len <- length(stx_slp$LENGTH_TYPE1)
p_fork_len <- round(n_fork_len / n_all_len, 3) * 100

trip_id_unique <- as.data.frame(table(stx_slp$ID, useNA = "always"))
total_trip_id_unique <- nrow(trip_id_unique)


## range currently set to not drop any obs (centimeters)

min_size <- 12

max_size <- 83

# min(stx_slp$YEAR,na.rm = TRUE)

min_year <- 1983

max_year <- 2022

break_year <- 2012 # this is an optional value to denote a change in management.
# inclusive on the upper bound. can be set to NA if not relevant.  2005 trap
# specifications were updated and many closures went into place


#### Filter to Commercial samples

################## PICK VARIABLES THAT I NEED HERE ;
################## ASSIGN GEARS, ETC. BASED ON FLOW CHART
## FILTER OUT COMMERCIAL SAMPLES
## replace species 3 letter code and itis number in code


tip2 <-  stx_slp %>%
  filter(
    FISHING_MODE == "COMMERCIAL",
    LENGTH_TYPE1 != "NO LENGTH",
    YEAR < 2023
  ) %>%
  type_convert() %>%
  mutate(
    REGION_NAME = region,
    OBS_STANDARD_SPECIES_CODE = as.numeric(OBS_STANDARD_SPECIES_CODE),
    source = "TIP",
    SLP = ifelse(OBS_STANDARD_SPECIES_CODE == 170867, 1, 0),
    ISLAND = ifelse(STATE_LANDED == "PUERTO RICO", STATE_LANDED, COUNTY_LANDED),
    OBS_ID = as.character(OBS_ID)
  ) %>%
  select(REGION_NAME,
    source,
    ID,
    INTERVIEW_DATE,
    FINAL_DATE,
    FISHING_MODE,
    INT_TYPE,
    YEAR = YEAR,
    ITIS_CODE = OBS_STANDARD_SPECIES_CODE,
    SPECIES = OBS_STANDARD_SPECIES_NAME,
    QUANTITY,
    SLP,
    STATE_LANDED,
    COUNTY_LANDED,
    COUNTY_CODE = LANDING_AREA_COUNTY_CODE,
    PLACE_LANDED,
    DEALER,
    DEALER_CODE,
    ISLAND,
    OBS_WEIGHT_KG,
    OBS_WEIGHT_UNIT, 
    LENGTH1_MM,
    LENGTH1,
    LENGTH_TYPE1,
    LENGTH_UNIT1,
    OBS_ID,
    LAND_STANDARD_GEAR_NAME,
    LAND_GEAR_NAME,
    GEAR_1,
    GEARNAME_1,
    LANDING_TYPE,
    BIAS_TYPE,
    VESSEL_ID,
    LICENSE,
    SAMPLE_ID,
    AREA_1,
    AREANAME_1,
    STANDARDAREA_1,
    STANDARDAREANAME_1,
    AGENT_USERNAME_ID,
    DEALER_CODE,
    VESSEL_ID
  )


#### Add gear groupings, NO length-length conversions available ####


### add length-length conversions to helper_table ;
tip3 <- merge(tip2, TIP_gears, by.x = "LAND_STANDARD_GEAR_NAME", all.x = T)
# gfin2a <- merge(gfin, flc, by.x=c("State_Landed","County_Landed"),all.x=T)

tip4 <- tip3[tip3$SLP == 1, ]

# table(tip3$LAND_STANDARD_GEAR_NAME, tip3$gear, useNA='always')

# gear_groups <- c("HAND LINE", "LONGLINE")
gear_groups <- unique(TIP_gears$gear)

# look at how many records are not fork length 
# antiforklength <- tip4 |>
#   filter(LENGTH_TYPE1 != "FORK LENGTH")

## splitting here to more easily qa/qc above
join_length_dat <- tip4 %>% # bind_rows(tip3 , gfin2) %>%
  # len_len_convert(params = LLconv,
  #                 raw_length = LENGTH1,
  #                 length_type =LENGTH_TYPE1,
  #                 length_units =  LENGTH_UNIT1,
  #                 desired_type = len_type) %>%
  untable(num = .$QUANTITY) %>%
  filter(LENGTH_TYPE1 == "FORK LENGTH") %>%
  # filter(NEW_GEAR_NAME %in% c(gear_groups)) %>%
  mutate(
    FL_CM = LENGTH1_MM / 10,
    lbin = floor(FL_CM / bin_size) * bin_size,
    mgt_period = ifelse(YEAR <= break_year, paste0(min_year, " - ", break_year),
      paste0(break_year + 1, " - ", max_year)
    )
  )

# FINAL dataset here  ####

length_data_full_gear <- join_length_dat %>%
  select(YEAR, INTERVIEW_DATE, FINAL_DATE, ID, OBS_ID, STATE = STATE_LANDED,
         COUNTY=COUNTY_LANDED, COUNTY_CODE, FL_CM, LENGTH_TYPE1, LENGTH_UNIT1,
         OBS_WEIGHT_KG, OBS_WEIGHT_UNIT, lbin, source, LAND_STANDARD_GEAR_NAME,
         LAND_GEAR_NAME, GEARNAME_1, mgt_period, ISLAND, INT_TYPE, fleet, 
         DEALER_CODE, VESSEL_ID, LICENSE
  ) %>%
  filter(
    between(FL_CM, min_size, max_size),
    ISLAND != "NOT CODED"
  ) 

# if land_standard_gear_name is not coded, replace with gearname_1 (effort gear)
length_data_final <- length_data_full_gear |> 
  mutate(gear = case_when(LAND_STANDARD_GEAR_NAME == "NOT CODED" ~ GEARNAME_1, 
                          TRUE ~ LAND_STANDARD_GEAR_NAME))


unique(length_data_final$gear)

# Analyst have asked for a record of dropped observations
######### ADAPT THIS TO OUTPUT ALL DROPPED RECORDS ABOVE--MOVE HIGHER IN THE
# SCRIPT TO HAVE ALL VARS
# dropped_obs <- anti_join(join_length_dat, length_data_final, by = "OBS_ID")



################################################## DOCUMENT ALL DROPPED DATA

# drop <- list()  ##++this creates an empty list for for loop
#
# # for loop generates binned length comps for each of the final gears.
# Stored in list. RUN:  View(comps[[1]]) to see in R
#
#  drop[[1]] <-  NON-RANDOM SAMPLES
#  drop[[2]] <-  SIZE DATA
#   filter(gear == final_gears[i]) %>%
# names(comps)[[1]] <- paste0(final_gears[i], "_", bin_size, "cm") ##++tabs are
# named here ; could add _nom here if desired (instead of _lfd and _lfdw in
# main file)
#
#--------------------------------------------------#

### this is writing only size data issues (e.g. samples too small/large to be
# considered realistic)

# write.xlsx(dropped_obs, file = paste0("./outputs/", sp,
# "_size_dropped_observations_", gsub("-", "", Sys.Date()), ".xlsx"))


# this is some funky code to make sure bins with no obervations are maintained
# in the data. Open to finding a more elegant solution but this works

full_set <- crossing(
  YEAR = seq(from = min_year, to = max_year, by = 1),
  lbin = seq(from = min_size, to = max_size, by = bin_size),
  N = 0
) %>%
  pivot_wider(names_from = lbin, values_from = N)

comp_names <- c(
  "YEAR", "ln_fish", "ln_trips", "ln_dealers", "ln_vessels",
  names(full_set)[-1]
)


# GLM analysis ####

# No filtering based on number of trips/fish per year yet, analysis of all
# available data.
#
# 414 records removed due to not being measured in fork length:
#
#   108 BY HAND; DIVING GEAR - total length
#
# 11 POTS AND TRAPS; FISH - total/curved fork length
#
# 81 SPEARS; DIVING - total length
#
# 214 TRAMMEL NETS - total length

unique(length_data_final$gear) 

## All Gears ####


# str(length_data_final)

length_data_glm <- length_data_final |>
  select(YEAR, FINAL_DATE, ID, COUNTY, FL_CM, gear) |>
  mutate(ID = as.character(ID)) 

# plot data

gant_data <- length_data_final %>%
  group_by(gear) %>%
  dplyr::mutate(n_ID = n_distinct(ID)) |>
  dplyr::filter(n_ID >= 3) %>%
  ungroup() %>%
  group_by(YEAR, gear) |>
  dplyr::summarize(n = n(), .groups = "drop") |>
  mutate(YEAR = as.integer(YEAR))

abc1 <- gant_data |>
  group_by(gear) |>
  dplyr::mutate(total_n = sum(n)) |>
  ungroup() |>
  dplyr::mutate(
    gear =
      fct_reorder(gear, total_n)
  ) %>%
  ggplot(aes(
    x = YEAR, y = gear,
    color = gear, size = n
  )) +
  geom_point() +
  labs(x = "Year", y = "", colour = "", shape = "", title = county) +
  theme_bw() +
  theme(
    legend.position = "null", text = element_text(size = 20),
    title = element_text(size = 15)
  ) +
  geom_vline(
    xintercept = 2011.5,
    color = "darkgrey", linewidth = 1.5
  )


# fit models
# comparing length to date and gear in a gamma full model
mod2 <- glmer(
  FL_CM ~ scale(FINAL_DATE) + gear +
    (1 | YEAR) + (1 | ID),
  data = length_data_glm, family = Gamma(link = log)
)

mod_contr <- emmeans::emmeans(
  object = mod2, pairwise ~ "gear",
  adjust = "tukey"
)


allgears_multcompcld <- multcomp::cld(object = mod_contr$emmeans)

length_data_fishcount <- length_data_final |>
  group_by(gear) |>
  tally()

length_data_tripcount <- aggregate(
  data = length_data_final,
  ID ~ gear,
  function(ID) length(unique(ID))
)

allgears_multcompcld_fish <- full_join(allgears_multcompcld,
  length_data_fishcount,
  by = "gear"
)
allgears_multcompcld_trip <- full_join(allgears_multcompcld_fish,
  length_data_tripcount,
  by = "gear"
)

allgears_multcompcld_final <- allgears_multcompcld_trip |>
  mutate(
    Percentage = round(n / sum(n) * 100, 2),
    emmean = round(emmean, 2),
    asymp.LCL = round(asymp.LCL, 2),
    asymp.UCL = round(asymp.UCL, 2)
  ) |>
  # dplyr::filter(ID >= 3) |>
  dplyr::filter(ID >= 3) |>
  dplyr::rename(
    "Group" = ".group",
    "Gear" = "gear",
    "Estimated Marginal Mean" = "emmean",
    "LCL" = "asymp.LCL",
    "UCL" = "asymp.UCL",
    "Fish(n)" = "n",
    "Interview(n)" = "ID"
  ) |>
  select(
    Gear, "Estimated Marginal Mean", LCL, UCL, Group, "Fish(n)",
    "Interview(n)", Percentage
  ) |>
  mutate("Gear Group" = case_when(
    Gear == "POTS AND TRAPS; FISH" ~ "Fish Traps",
    Gear == "SPEARS" ~ "Spears",
    TRUE ~ "Spears or Fish Traps"
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

allgears_multicom_mean <- full_join(mean_allgears, allgears_multcompcld_final,
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

# look at mean by year

mean_by_year <- length_data_glm %>%
  group_by(YEAR) %>%
  mean_table(FL_CM) |>
  arrange(desc(group_cat)) |>
  dplyr::rename(
    "Year" = "group_cat",
    "Mean" = "mean"
  ) |>
  select(Year, Mean) |>
  mutate(Year = as.character(Year))

flextable(mean_by_year) |>
  theme_box() %>%
  align(align = "center", part = "all") %>%
  fontsize(size = 8, part = "all") %>%
  autofit()

mean_by_year$Year <- as.integer(mean_by_year$Year)


mean_by_year |>
  ggplot(aes(x = Year, y = Mean)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Fork Length (cm)", title = county) +
  theme_bw() +
  theme(
    legend.position = "null", text = element_text(size = 20),
    title = element_text(size = 15)
  )


## All Gears 2012 and after ####

length_data_glm_2012 <- length_data_final |>
  filter(YEAR >= 2012) |>
  select(
    YEAR, FINAL_DATE, ID, COUNTY, FL_CM, gear) |>
  mutate(ID = as.character(ID)) 


# fit models

# comparing length to date and gear in a gamma full model
mod2 <- glmer(
  FL_CM ~ scale(FINAL_DATE) + gear + (1 | YEAR) +
    (1 | ID),
  data = length_data_glm_2012, family = Gamma(link = log)
)

# pairwise comparisons
mod_contr <- emmeans::emmeans(
  object = mod2, pairwise ~ "gear",
  adjust = "tukey"
)

allgears_multcompcld_2012 <- multcomp::cld(object = mod_contr$emmeans)

length_data_fishcount_12 <- length_data_glm_2012 |>
  group_by(gear) |>
  tally()
length_data_tripcount_12 <- aggregate(
  data = length_data_glm_2012,
  ID ~ gear,
  function(ID) length(unique(ID))
)

allgears_multcompcld_fish_2012 <- full_join(allgears_multcompcld_2012,
  length_data_fishcount_12,
  by = "gear"
)
allgears_multcompcld_trip_2012 <- full_join(allgears_multcompcld_fish_2012,
  length_data_tripcount_12,
  by = "gear"
)

allgears_multcompcld_finaL_2012 <- allgears_multcompcld_trip_2012 |>
  filter(ID >= 3) |>
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
  select(
    Gear, "Estimated Marginal Mean", LCL, UCL, Group, "Fish(n)",
    "Interview(n)", Percentage
  ) |>
  mutate("Gear Group" = case_when(
    Gear == "POTS AND TRAPS; FISH" ~ "Spears or Fish Traps",
    Gear == "SPEARS" ~ "Spears or Fish Traps",
    TRUE ~ "Spears or Fish Traps"
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
  by = "Gear"
)

allgears_multicom_mean_final2012 <- allgears_multicom_mean2012 |>
  arrange(desc(Percentage)) #|>
# dplyr::filter(`Interview(n)` >= 3)

tbl2 <- flextable(allgears_multicom_mean_final2012) |>
  theme_box() %>%
  align(align = "center", part = "all") %>%
  fontsize(size = 8, part = "all") %>%
  autofit()


### overlay time periods ####

length_data_1983_2022 <- length_data_final %>%
  group_by(gear) %>%
  dplyr::mutate(n_ID = n_distinct(ID)) |>
  dplyr::filter(n_ID >= 3) %>% # ungroup %>%
  # group_by(YEAR) %>%
  # filter(n() >= 30) %>%
  ungroup()

full_mean = round(mean(length_data_1983_2022$FL_CM), 2)

length_data_2012_2022 <- length_data_final |>
  filter(YEAR >= 2012) |>
  group_by(gear) %>%
  dplyr::mutate(n_ID = n_distinct(ID)) |>
  dplyr::filter(n_ID >= 3) %>% # ungroup %>%
  # group_by(YEAR) %>%
  # filter(n() >= 30) %>%
  ungroup()

truncated_mean = round(mean(length_data_2012_2022$FL_CM), 2)


agr_den_NOgears <-
  ggplot() +
  geom_density(aes(FL_CM, color = "length_data_1983_2022"),
    linewidth = 1.0,
    alpha = .2, data = length_data_1983_2022
  ) +
  geom_density(aes(FL_CM, color = "length_data_2012_2022"),
    linewidth = 1.0,
    alpha = .2, data = length_data_2012_2022
  ) +
  geom_vline(
    data = length_data_1983_2022, aes(
      xintercept = mean(FL_CM),
      color = "length_data_1983_2022"
    ),
    linetype = "dashed", linewidth = 1
  ) +
  geom_vline(
    data = length_data_2012_2022, aes(
      xintercept = mean(FL_CM),
      color = "length_data_2012_2022"
    ),
    linetype = "dashed", linewidth = 1
  ) +
  labs(x = "Fork Length (cm)", title = county) +
  guides(color = guide_legend(title = "Time Series")) +
  scale_color_discrete(labels = c("1983-2022", "2012-2022")) +
  theme(
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20),
    legend.position = "bottom",
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    title = element_text(size = 20)
  )

abc14 <- agr_den_NOgears

### GEAR INDIVIDUALS ####

length_data_gears <- length_data_final %>%
  group_by(gear) %>%
  dplyr::mutate(n_ID = n_distinct(ID)) |>
  dplyr::filter(n_ID >= 3) %>%
  ungroup() |>
  filter(gear %in% c(
    "POTS AND TRAPS; CMB", "POTS AND TRAPS; FISH",
    "SPEARS", "ENTANGLING NETS (GILL) UNSPC", "SPEARS; DIVING"
  ))

ycounts <- length_data_gears %>%
  tabyl("gear") %>%
  mutate(n_labels = paste0(gear, " (n= ", n, ")"))

muv <- plyr::ddply(length_data_gears,
  "gear",
  summarise,
  grp.mean = mean(FL_CM)
)
head(muv)

agr_den_v <- length_data_gears %>%
  ggplot(aes(FL_CM)) +
  geom_density(aes(color = gear), linewidth = 0.75) +
  scale_color_hue(labels = ycounts$n_labels) +
  labs(color = "Gear", x = "Fork Length (cm)", title = county) +
  theme(
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.position = "bottom",
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    title = element_text(size = 20)
  ) +
  guides(color = guide_legend(ncol = 2)) +
  geom_vline(
    data = muv, aes(xintercept = grp.mean, color = gear),
    linetype = "dashed"
  )


abc15 <- agr_den_v

### top gears individuals ####

length_data_gears_2012 <- length_data_glm_2012 %>%
  group_by(gear) %>%
  dplyr::mutate(n_ID = n_distinct(ID)) |>
  dplyr::filter(n_ID >= 3) %>%
  ungroup() |>
  filter(gear %in% c(
    "BY HAND; DIVING GEAR",
    "SPEARS; DIVING"
  ))

ycounts <- length_data_gears_2012 %>%
  tabyl("gear") %>%
  mutate(n_labels = paste0(gear, " (n= ", n, ")"))

muv12 <- plyr::ddply(length_data_gears_2012,
  "gear",
  summarise,
  grp.mean = mean(FL_CM)
)
head(muv12)

agr_den_v12 <- length_data_gears_2012 %>%
  ggplot(aes(FL_CM)) +
  geom_density(aes(color = gear), linewidth = 0.75) +
  scale_color_hue(labels = ycounts$n_labels) +
  labs(color = "Gear", x = "Fork Length (cm)", title = county) +
  theme(
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20),
    legend.position = "bottom",
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    title = element_text(size = 20)
  ) +
  guides(color = guide_legend(ncol = 2)) +
  geom_vline(
    data = muv12, aes(xintercept = grp.mean, color = gear),
    linetype = "dashed"
  )


abc19 <- agr_den_v12

## Annual Density plots ####
### ALL GEARS ####
fleet_final <- length_data_1983_2022[length_data_1983_2022$fleet == 1, ]

fcounts <- fleet_final %>%
  group_by(YEAR) %>%
  filter(n() >= 30) %>%
  ungroup() %>%
  tabyl(gear) %>%
  mutate(n_labels = paste0(gear, " (n= ", n, ")"))

all_carSTX <-
  fleet_final %>%
  group_by(YEAR) %>%
  filter(n() >= 30) %>%
  ungroup() %>%
  group_by(YEAR) %>%
  dplyr::mutate(year_labs = paste0(YEAR, "\n n = ", n())) %>%
  ggplot(aes(FL_CM)) +
  geom_density(linewidth = 0.75) +
  labs(x = "Fork Length (cm)", title = paste0(
    county, "\n (N = ",
    sum(fcounts$n), ")"
  )) +
  facet_wrap(~year_labs, ncol = 10)

export_fig_page(all_carSTX)

abc17 <- all_carSTX

### TOP GEARS ####
fleet_final_gears <- length_data_gears[length_data_gears$fleet == 1, ]

fcounts <- fleet_final_gears %>%
  group_by(YEAR) %>%
  filter(n() >= 30) %>%
  ungroup() %>%
  tabyl(gear) %>%
  mutate(n_labels = paste0(gear, " (n= ", n, ")"))

all_car_gearsSTX <-
  fleet_final_gears %>%
  group_by(YEAR) %>%
  filter(n() >= 30) %>%
  ungroup() %>%
  group_by(YEAR) %>%
  dplyr::mutate(year_labs = paste0(YEAR, "\n n = ", n())) %>%
  ggplot(aes(FL_CM, color = gear)) +
  geom_density(linewidth = 0.75) +
  scale_color_hue(labels = fcounts$n_labels) +
  labs(
    color = "Gear Type", x = "Fork Length (cm)",
    title = paste0(county, "\n (N = ", sum(fcounts$n), ")")
  ) +
  facet_wrap(~year_labs, ncol = 7) +
  guides(color = guide_legend(ncol = 2)) +
  theme(
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.position = "bottom"
  )

export_fig_page(all_car_gearsSTX)
abc18 <- all_car_gearsSTX

# Aggregated cummulative density ####

counts <- length_data_final %>%
  tabyl(gear) %>%
  mutate(n_labels = paste0(gear, " (n= ", n, ")"))

abc20 <- length_data_final %>%
  ggplot(aes(FL_CM)) +
  stat_ecdf() +
  labs(
    x = "Fork Length (cm)",
    title = paste0(county, "\n (N = ", sum(counts$n), ")")
  ) +
  theme_minimal()



# SAVE WORKSPACE ####
save.image(
  file = here::here(
    "tools",
    "sedar_84_stx_stp_2022",
    "stx_stp_2022_figures.RData"
  )
)
