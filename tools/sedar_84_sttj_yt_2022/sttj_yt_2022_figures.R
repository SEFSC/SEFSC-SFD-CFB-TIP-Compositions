# This script will be expanding upon the length-comp script c1_nominal_len_comps.Rmd created by Molly Stevens.

# Yellowtail Snapper STTJ ####

## Set up library  ####

librarian::shelf(here, tidyverse,  dotenv, reshape, openxlsx, janitor, DT, 
                 pander, knitr, flextable, ggplot2, lmerTest, meantables)
# if conflicts in pkgs arise, use the following:
# library(conflicted)
# conflicted::conflicts_prefer(here::here)


#pull data by island
# cr_tip(state_codes = c('PR', 'VI'))

# # Find out the date of the most recent extraction
# tip_date <- max(
#   as.numeric(gsub(".*?([0-9]+).RDS*", "\\1",
#                   list.files(here("data", "raw"),
#                              pattern = "com_tip_PR_VI"))))
# 
# # Find out the name of the most recent extraction
# tip_file <- list.files(here("data", "raw"),
#                        pattern = paste0("^com_tip_PR_VI_+", tip_date))
# 

 # Read in the most recent extraction
# tip <- readRDS(file = here("data", "raw", tip_file))
# tip <- readRDS("~/SEFSC-SFD-CFB-TIP-Compositions/data/raw/com_tip_PR_VI_168907_20240111.RDS")

tip <- readRDS(here::here("data", "raw", "com_tip_PR_VI_168907_20240111.RDS"))

# Filter to STTJ and Yellowtail Snapper AND create new filterable date value 
sttj_yt <- tip |> 
  filter(COUNTY_LANDED %in% c("ST THOMAS", "ST JOHN"),
         OBS_STANDARD_SPECIES_CODE == "168907") |> 
  mutate(TEST_DATE = as.Date(ymd_hms(INTERVIEW_DATE)),
         FINAL_DATE = case_when(is.na(TEST_DATE) ~ INTERVIEW_DATE, 
                                TRUE ~ TEST_DATE))


source("~/SEFSC-SFD-CFB-TIP-Compositions/tools/functions/len_len_convert.R")
source("~/SEFSC-SFD-CFB-TIP-Compositions/tools/functions/fig_format_export.R")

LLconv <- read_csv("~/SEFSC-SFD-CFB-TIP-Compositions/tools/CSVs/LLconversions.csv",
                   show_col_types = FALSE)

TIP_gears <- read_csv("~/SEFSC-SFD-CFB-TIP-Compositions/tools/CSVs/tip_gears_yts_sttj_landstdgearname.csv",
                      show_col_types = FALSE)

gearcols <- c("Net" = "#FF0000", "Trap" = "#00A08A", "Other" = "#5D057D", "Hook and Line" ="#046C9A", "Gill Net" = "#00A08A" , "Haul Seine" = "#00A08A" , "Lobster Trap" = "#00A08A" , "Trammel Net" = "#00A08A" , "Combined" = "black")


## Control Settings and Exploratory Frequency Tables ####

sp <- "YTS"

region <- "PUERTO RICO - USVI"

county <- "ST THOMAS/ST JOHN"

bin_size <- 1

len_type <- "FORK LENGTH"

disclaimer <- "Gears with less than 3 unique interviews were removed."

flextable(as.data.frame(table(sttj_yt$LENGTH_TYPE1, useNA='always')))%>%
  autofit()

flextable(as.data.frame(table(sttj_yt$LAND_STANDARD_GEAR_NAME, useNA='always')))%>%
  autofit()
##range currently set to not drop any obs (centimeters)

n_fork_len = sum(sttj_yt$LENGTH_TYPE1 == "FORK LENGTH")
n_all_len = length(sttj_yt$LENGTH_TYPE1)
p_fork_len = round(n_fork_len/n_all_len, 3)*100

trip_id_unique <- as.data.frame(table(sttj_yt$ID, useNA='always'))
total_trip_id_unique = nrow(trip_id_unique)

min_size <- 14 

max_size <- 80   

min(sttj_yt$YEAR,na.rm = TRUE)

min_year <- 1983

max_year <- 2022 

break_year <- 2012 # this is an optional value to denote a change in management. inclusive on the upper bound. can be set to NA if not relevant.  2005 trap specifications were updated and many closures went into place


#### Filter to Commercial samples

################## PICK VARIABLES THAT I NEED HERE ;
################## ASSIGN GEARS, ETC. BASED ON FLOW CHART
## FILTER OUT COMMERCIAL SAMPLES

tip2 <- #readRDS('./data_clean/tip_GOM.Rdata') %>%
  sttj_yt %>%
  filter( #MULT_TRIP == '0')%>%  ##no mult_trip in Caribbean TIP --> ALL full catch samples
    FISHING_MODE=='COMMERCIAL',
    LENGTH_TYPE1!='NO LENGTH',
    YEAR < 2023
  ) %>%
  type_convert() %>%
  mutate(REGION_NAME = region,
         OBS_STANDARD_SPECIES_CODE = as.numeric(OBS_STANDARD_SPECIES_CODE), 
         source      = "TIP", 
         YTS         = ifelse(OBS_STANDARD_SPECIES_CODE==168907,1,0), #binary flag for queen triggerfish vs 'other'
         ISLAND      = ifelse(STATE_LANDED=='PUERTO RICO', STATE_LANDED, COUNTY_LANDED),
         #STAT_AREA = ifelse(STANDARDAREA_1>1, STANDARDAREA_1, LAND_STANDARD_AREA_ID),
         OBS_ID      = as.character(OBS_ID)
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
         YTS,
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

tip4 <- tip3[tip3$YTS == 1, ]

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
    

    # Analyst have asked for a record of dropped observations
    ######### ADAPT THIS TO OUTPUT ALL DROPPED RECORDS ABOVE--MOVE HIGHER IN THE SCRIPT TO HAVE ALL VARS
# dropped_obs <- anti_join(join_length_dat, length_data_final, by = "OBS_ID")  



#-----------------------------------------------#DOCUMENT ALL DROPPED DATA

    # drop <- list()  ##++this creates an empty list for for loop
    # 
    # # for loop generates binned length comps for each of the final gears. Stored in list. RUN:  View(comps[[1]]) to see in R
    # 
    #  drop[[1]] <-  NON-RANDOM SAMPLES
    #  drop[[2]] <-  SIZE DATA
    #   filter(gear == final_gears[i]) %>%
    # names(comps)[[1]] <- paste0(final_gears[i], "_", bin_size, "cm") ##++tabs are named here ; could add _nom here if desired (instead of _lfd and _lfdw in main file)
    #  
#------------------------------------------------#

    ###this is writing only size data issues (e.g. samples too small/large to be considered realistic)
    
    # write.xlsx(dropped_obs, file = paste0("./outputs/", sp, "_size_dropped_observations_", gsub("-", "", Sys.Date()), ".xlsx"))

    # this is some funky code to make sure bins with no obervations are maintained in the data. Open to finding a more elegant solution but this works

full_set <- crossing(YEAR = seq(from = min_year, to = max_year, by = 1),
                     lbin = seq(from = min_size, to = max_size, by = bin_size), 
                     N = 0) %>%
  pivot_wider(names_from = lbin, values_from = N)

comp_names = c("YEAR", "ln_fish", "ln_trips", "ln_dealers","ln_vessels", names(full_set)[-1])

 


# GLM analysis ####
# 
# No filtering based on number of trips/fish per year yet, analysis of all available data.
# 
# 123 records removed due to not being measured in fork length:
#   
#   20 GILL NETS; OTHER
# 
# 5 NOT CODED
# 
# 32 LINES HAND
# 
# 66 POTS AND TRAPS; FISH


unique(length_data_final$gear) # "Hook and Line", "Trap", "Other", "Net"
 

## All Gears ####

length_data_glm <- length_data_final |>
  select(YEAR, FINAL_DATE, ID, COUNTY, FL_CM, gear) |> 
  mutate(ID = as.character(ID)) 
# plot data

# # filtered to gears with 30 or more occurances for the purposes of plotting visibility
# abc1 = allgears_glm_plot <- length_data_glm %>% group_by(gear) %>% filter(n() >= 30) %>% ungroup |> 
#   ggplot(aes(x = as.Date(FINAL_DATE), y = FL_CM)) +
#   geom_point(aes(colour = gear), size = 1, alpha = 0.5) +
#   # scale_shape_manual(values=c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 13, 14))+
#   geom_smooth(method = "lm", formula = "y ~ x", col = "black") +
#   # facet_wrap(~ COUNTY_LANDED) +
#   labs(x = "", y = "Length (cm)", colour = "", shape = "") +
#   theme_bw() +
#   theme(legend.position = "bottom", legend.text = element_text(size = 10),
#         legend.box.spacing = unit(0, "npc"), panel.grid = element_blank()) +
#   guides(colour = guide_legend(override.aes = list(size = 2)))

# gant_data <- length_data_final %>% group_by(ID) %>% filter(n() >= 3) %>% ungroup %>%
#   # filter(ID >= 3) |>
#   group_by(YEAR, gear) |>
#   summarize(n = n(), .groups = "drop") 

gant_data <- length_data_final %>% 
  group_by(gear) %>% 
  dplyr::mutate(n_ID = n_distinct(ID)) |> 
  dplyr::filter(n_ID >= 3) %>% ungroup %>%
  group_by(YEAR, gear) |>
  dplyr::summarize(n = n(), .groups = "drop") |> 
  mutate(YEAR = as.integer(YEAR))

abc1 <- gant_data |>
  group_by(gear) |>
  dplyr::mutate(total_n = sum(n)) |> 
  ungroup() |>   
  dplyr::mutate(gear = fct_reorder(gear, total_n)) %>%
  ggplot(aes(x = YEAR, y = gear, color = gear, size = n)) +
  geom_point()  +
  labs(x = "Year", y = "", colour = "", shape = "", 
       title = paste(county, "Length Samples"),
       caption = disclaimer) +
  theme_bw() + 
  theme(legend.position="null", text = element_text(size = 20), 
        title = element_text(size = 15))+
  geom_vline(xintercept = 2011.5,  
             color = "darkgrey", linewidth=1.5)


gant_data_id <- length_data_final %>% 
  group_by(gear) %>% 
  dplyr::mutate(n_ID = n_distinct(ID)) |> 
  dplyr::filter(n_ID >= 3) %>% ungroup %>%
  group_by(YEAR, gear) |>
  dplyr::summarize(n_ID = n_distinct(ID), .groups = "drop") |> 
  mutate(YEAR = as.integer(YEAR))

abc21 <- gant_data_id |>
  # filter(YEAR > 2011) |> 
  group_by(gear) |>
  dplyr::mutate(total_n = sum(n_ID)) |> 
  ungroup() |>   
  dplyr::mutate(gear = fct_reorder(gear, total_n)) %>%
  ggplot(aes(x = YEAR, y = gear, color = gear, size = n_ID)) +
  geom_point()  +
  labs(x = "Year", y = "", colour = "", shape = "", title = county) +
  theme_bw() + 
  theme(legend.position="null", text = element_text(size = 20), 
        title = element_text(size = 15))+
  geom_vline(xintercept = 2011.5,  
             color = "darkgrey", linewidth=1.5)

# fit models

# comparing length to date and gear in a gamma full model
mod2 = glmer(FL_CM ~ scale(FINAL_DATE) + gear + (1 | YEAR) + (1 | ID),
             data = length_data_glm, family = Gamma(link=log))


# pairwise comparisons (if needed)(needed for Hook and Line)
### 
mod_contr = emmeans::emmeans(object = mod2, pairwise ~ "gear", adjust = "tukey")

# cld provides gear groupings based on which gears are similar vs significantly different from each other 
### 
multcomp::cld(object = mod_contr$emmeans)

allgears_multcompcld <- multcomp::cld(object = mod_contr$emmeans)

length_data_fishcount <- length_data_final |> 
  group_by(gear) |>
  tally() 
length_data_tripcount <- aggregate(data = length_data_final,                # Applying aggregate
                                   ID ~ gear,
                                   function(ID) length(unique(ID)))

allgears_multcompcld_fish <- full_join(allgears_multcompcld, length_data_fishcount, by = "gear")
allgears_multcompcld_trip <- full_join(allgears_multcompcld_fish, length_data_tripcount, by = "gear")

allgears_multcompcld_final <- allgears_multcompcld_trip |> 
  mutate(Percentage = round(n/sum(n)*100, 2), 
         emmean = round(emmean, 2),
         asymp.LCL = round(asymp.LCL, 2),
         asymp.UCL = round(asymp.UCL, 2)) |> 
  dplyr::rename("Group" = ".group",
                "Gear" = "gear",
                "Estimated Marginal Mean" = "emmean",
                "LCL" = "asymp.LCL",
                "UCL" = "asymp.UCL",
                "Fish(n)" = "n",
                "Interview(n)" = "ID") |> 
  # filter("Interview(n)" >= 3) |> 
  dplyr::filter(`Interview(n)` >= 3)|> 
  select(Gear, "Estimated Marginal Mean", LCL, UCL,  Group, "Fish(n)","Interview(n)", Percentage ) |>
  mutate("Gear Group" = case_when(Gear == "LINES HAND" ~ "Hand Line",
                                  Gear == 'POTS AND TRAPS; FISH'~ "Traps",
                                  Gear == "HAUL SEINES"~ "Hand Line or Traps",
                                  Gear == "POTS AND TRAPS; CMB"~ "Hand Line or Traps",
                                  Gear == "POTS AND TRAPS;SPINY LOBSTER" ~ "Hand Line or Traps",
                                  Gear == "ROD AND REEL" ~ "Rod and Reel",
                                  TRUE ~ "Hand Line, Traps, or Rod and Reel")) 

# create table of means for each gear

mean_allgears <- length_data_glm %>%
  group_by(gear) %>%
  dplyr::mutate(n_ID = n_distinct(ID)) |> 
  dplyr::filter(n_ID >= 3) |> 
  mean_table(FL_CM) |> 
  dplyr::rename("Gear" = "group_cat",
                "Mean" = "mean") |> 
  select(Gear, Mean)

allgears_multicom_mean <- full_join(mean_allgears, allgears_multcompcld_final, by = "Gear")

allgears_multicom_mean_final <- allgears_multicom_mean |> 
  arrange(desc(Percentage))#|> 
  # dplyr::filter(`Interview(n)` >= 3)


tbl1 = flextable(allgears_multicom_mean_final) |> 
  theme_box() %>%
  align(align = "center", part = "all") %>%
  fontsize(size=8, part="all") %>%
  autofit() 


# look at mean by year

mean_by_year <- length_data_glm %>%
  group_by(YEAR) %>%
  mean_table(FL_CM) |> 
  arrange(desc(group_cat))|> 
  dplyr::rename("Year" = "group_cat",
                "Mean" = "mean") |> 
  select(Year, Mean) |> 
  mutate(Year = as.character(Year))

flextable(mean_by_year)|> 
  theme_box() %>%
  align(align = "center", part = "all") %>%
  fontsize(size=8, part="all") %>%
  autofit() 

mean_by_year$Year <- as.integer(mean_by_year$Year) 


mean_by_year |> 
  ggplot(aes(x = Year, y = Mean)) +
  geom_line() +
  geom_point()  +
  labs(x = "Year", y = "Fork Length (cm)", title = county) +
  theme_bw() + 
  theme(legend.position="null", text = element_text(size = 20),
        title = element_text(size = 15))

## All Gears 2012 and after ####

length_data_glm_2012 <- length_data_final |>
  filter(YEAR >= 2012) |> 
  select(YEAR, FINAL_DATE, ID, COUNTY, FL_CM,  gear) |> 
  mutate(ID = as.character(ID)) 

# plot data

# abc2 = allgears_glm_plot <- length_data_glm_2012 |> 
#   ggplot(aes(x = as.Date(FINAL_DATE), y = FL_CM)) +
#   geom_point(aes(colour = gear , shape = gear), size = 1, alpha = 0.5) +
#   geom_smooth(method = "lm", formula = "y ~ x", col = "black") +
#   # facet_wrap(~ COUNTY_LANDED) +
#   labs(x = "", y = "Length (cm)", colour = "", shape = "") +
#   theme_bw() +
#   theme(legend.position = "bottom", legend.text = element_text(size = 15),
#         legend.box.spacing = unit(0, "npc"), panel.grid = element_blank()) +
#   guides(colour = guide_legend(override.aes = list(size = 2)))

# gant_data_12 <- length_data_glm_2012 %>% group_by(ID) %>% dplyr::filter(n() >= 3) %>% ungroup %>%
#   # filter(ID >= 3) |>
#   group_by(YEAR, gear) |>
#   dplyr::summarize(n = n(), .groups = "drop") |> 
#   mutate(YEAR = as.integer(YEAR))

# gant_data_12 <- length_data_glm_2012 %>% 
#   group_by(gear) %>% 
#   dplyr::mutate(n_ID = n_distinct(ID)) |> 
#   dplyr::filter(n_ID >= 3) %>% ungroup %>%
#   group_by(YEAR, gear) |>
#   dplyr::summarize(n = n(), .groups = "drop") |> 
#   mutate(YEAR = as.integer(YEAR))
# 
# abc2 <- gant_data_12 |>
#   ggplot(aes(x = YEAR, y = gear, color = gear, size = n)) +
#   geom_point()  +
#   labs(x = "Year", y = "", colour = "", shape = "") +
#   theme_bw() + 
#   theme(legend.position="null", text = element_text(size = 15)) +
#   xlim(2012,2022)

# fit models

# comparing length to date and gear in a gamma full model
mod2 = glmer(FL_CM ~ scale(FINAL_DATE) + gear + (1 | YEAR) + (1 | ID),
             data = length_data_glm_2012, family = Gamma(link=log))


# pairwise comparisons (if needed)(needed for Hook and Line)
### 
mod_contr = emmeans::emmeans(object = mod2, pairwise ~ "gear", adjust = "tukey")

allgears_multcompcld_2012 <- multcomp::cld(object = mod_contr$emmeans)

length_data_fishcount_12 <- length_data_glm_2012 |> 
  group_by(gear) |>
  tally()
length_data_tripcount_12 <- aggregate(data = length_data_glm_2012,                # Applying aggregate
                                      ID ~ gear,
                                      function(ID) length(unique(ID)))

allgears_multcompcld_fish_2012 <- full_join(allgears_multcompcld_2012, length_data_fishcount_12, by = "gear")
allgears_multcompcld_trip_2012 <- full_join(allgears_multcompcld_fish_2012, length_data_tripcount_12, by = "gear")

allgears_multcompcld_finaL_2012 <- allgears_multcompcld_trip_2012 |> 
  mutate(Percentage = round(n/sum(n)*100, 2), 
         emmean = round(emmean, 2),
         asymp.LCL = round(asymp.LCL, 2),
         asymp.UCL = round(asymp.UCL, 2))|>
  dplyr::rename("Group" = ".group",
                "Gear" = "gear",
                "Estimated Marginal Mean" = "emmean",
                "LCL" = "asymp.LCL",
                "UCL" = "asymp.UCL",
                "Fish(n)" = "n",
                "Interview(n)" = "ID") |> 
  # filter("Interview(n)" >= 3) |> 
  dplyr::filter(`Interview(n)` >= 3)|> 
  select(Gear, "Estimated Marginal Mean", LCL, UCL,  Group, "Fish(n)","Interview(n)", Percentage ) |>
  mutate("Gear Group" = case_when(Gear == "LINES HAND" ~ "Hand Line",
                                  Gear == 'POTS AND TRAPS; FISH'~ "Traps",
                                  Gear == "HAUL SEINES"~ "Hand Line or Traps",
                                  Gear == "POTS AND TRAPS; CMB"~ "Hand Line or Traps",
                                  Gear == "POTS AND TRAPS;SPINY LOBSTER" ~ "Hand Line or Traps",
                                  Gear == "ROD AND REEL" ~ "Rod and Reel",
                                  TRUE ~ "Hand Line, Traps, or Rod and Reel")) 

# create table of means for each gear
mean_allgears2012 <- length_data_glm_2012 %>%
  group_by(gear) %>%
  dplyr::mutate(n_ID = n_distinct(ID)) |> 
  dplyr::filter(n_ID >= 3) |>
  mean_table(FL_CM) |> 
  dplyr::rename("Gear" = "group_cat",
                "Mean" = "mean") |> 
  select(Gear, Mean)

allgears_multicom_mean2012 <- full_join(mean_allgears2012, allgears_multcompcld_finaL_2012, by = "Gear")

allgears_multicom_mean_final2012 <- allgears_multicom_mean2012 |> 
  arrange(desc(Percentage))


tbl2 = flextable(allgears_multicom_mean_final2012) |> 
  theme_box() %>%
  align(align = "center", part = "all") %>%
  fontsize(size=8, part="all") %>%
  autofit() 


# Gear Density Plots ####

# Filtered to years with 30 or more length records (regardless of gear) per year. STT and STJ records are grouped together.

## Aggregated density plots ####

### overlay time periods ####


length_data_1983_2022 <- length_data_final %>% 
  group_by(gear) %>% 
  dplyr::mutate(n_ID = n_distinct(ID)) |> 
  dplyr::filter(n_ID >= 3) %>% #ungroup %>%
  # group_by(YEAR) %>% 
  # filter(n() >= 30) %>% 
  ungroup() 


full_mean = round(mean(length_data_1983_2022$FL_CM), 2)

length_data_2012_2022 <- length_data_final |>
  filter(YEAR >= 2012) |>
  group_by(gear) %>% 
  dplyr::mutate(n_ID = n_distinct(ID)) |> 
  dplyr::filter(n_ID >= 3) %>% #ungroup %>%
  # group_by(YEAR) %>% 
  # filter(n() >= 30) %>% 
  ungroup()


truncated_mean = round(mean(length_data_2012_2022$FL_CM), 2)

agr_den_NOgears <- 
  ggplot() +
  geom_density(aes(FL_CM, color = "length_data_1983_2022"),linewidth = 1.0, alpha = .2, data = length_data_1983_2022) +
  geom_density(aes(FL_CM, color = "length_data_2012_2022"),linewidth = 1.0, alpha = .2, data = length_data_2012_2022) +
  geom_vline(data = length_data_1983_2022, aes(xintercept=mean(FL_CM), color = "length_data_1983_2022"),
             linetype="dashed", linewidth=1) +
  geom_vline(data=length_data_2012_2022, aes(xintercept=mean(FL_CM), color = "length_data_2012_2022"),
             linetype="dashed", linewidth=1) +
  labs(x = "Fork Length (cm)", title = county)+
  # scale_fill_discrete(name = "Time Series", labels = c("1983-2022", "2012-2022"))
  guides(color=guide_legend(title="Time Series"))+
  scale_color_discrete(labels=c('1983-2022', '2012-2022'))+
  theme(legend.title = element_text(size=20), 
        legend.text = element_text(size=20),
        legend.position = "bottom",
        axis.text.x=element_text(size=20),
        axis.text.y=element_text(size=20),
        axis.title.x = element_text( size = 20),
        axis.title.y = element_text( size = 20),
        title = element_text(size = 20))
  # scale_fill_manual(name = "dataset", values = c(length_data_1983_2022 = "red", length_data_2012_2022 = "green"))
abc14 = agr_den_NOgears

### GEAR INDIVIDUALS ####

length_data_gears <- length_data_final %>% 
  group_by(gear) %>% 
  dplyr::mutate(n_ID = n_distinct(ID)) |> 
  dplyr::filter(n_ID >= 3) %>% #ungroup %>%
  # group_by(YEAR) %>% 
  # filter(n() >= 30) %>% 
  ungroup() |> 
  filter(gear %in% c("LINES HAND", "POTS AND TRAPS; FISH",
                                    "HAUL SEINES", "LINES POWER TROLL OTHER")) 

ycounts =length_data_gears %>% #group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
  tabyl("gear") %>%
  mutate(n_labels = paste0(gear, " (n= ", n, ")" ))

muv <- plyr::ddply(length_data_gears, "gear", summarise, grp.mean=mean(FL_CM))
head(muv)

agr_den_v <- length_data_gears %>% 
  # group_by(gear) %>% 
  # dplyr::mutate(n_ID = n_distinct(ID)) |> 
  # dplyr::filter(n_ID >= 3) %>% ungroup %>%
  # group_by(YEAR) %>% 
  # filter(n() >= 30) %>% ungroup %>%
  ggplot(aes(FL_CM))+
  # geom_density( aes(color = "Combined"),lwd=1.5)+
  geom_density(aes(color = gear),linewidth = 0.75)+
  scale_color_hue(labels=ycounts$n_labels)+
  # scale_color_hue(labels=c("Combined",ycounts$n_labels))+
  #scale_color_manual(values = gearcols, labels = c("Combined", counts$n_labels))+
  labs(color = "Gear" , x = "Fork Length (cm)", title = county)+ #title = paste0(county,  "\n (N = ", sum(ycounts$n), ")"))+
  # theme_minimal()
  theme(legend.title = element_text(size=20), 
        legend.text = element_text(size=20),
        legend.position = "bottom",
        axis.text.x=element_text(size=20),
        axis.text.y=element_text(size=20),
        axis.title.x = element_text( size = 20),
        axis.title.y = element_text( size = 20),
        title = element_text(size = 20))+
  guides(color=guide_legend(ncol = 2))+
  geom_vline(data=muv, aes(xintercept=grp.mean, color=gear),
             linetype="dashed")


abc15 = agr_den_v


### top gears individuals #### 

length_data_gears_2012 <- length_data_glm_2012 %>% 
  group_by(gear) %>% 
  dplyr::mutate(n_ID = n_distinct(ID)) |> 
  dplyr::filter(n_ID >= 3) %>% #ungroup %>%
  # group_by(YEAR) %>% 
  # filter(n() >= 30) %>% 
  ungroup() |> 
  filter(gear %in% c("LINES HAND", "POTS AND TRAPS; FISH",
                                        "ROD AND REEL")) 

ycounts =length_data_gears_2012 %>% #group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
  tabyl("gear") %>%
  mutate(n_labels = paste0(gear, " (n= ", n, ")" ))

muv12 <- plyr::ddply(length_data_gears_2012, "gear", summarise, grp.mean=mean(FL_CM))
head(muv12)

agr_den_v12 <- length_data_gears_2012 %>% 
  # group_by(gear) %>% 
  # dplyr::mutate(n_ID = n_distinct(ID)) |> 
  # dplyr::filter(n_ID >= 3) %>% ungroup %>%
  # group_by(YEAR) %>% 
  # filter(n() >= 30) %>% ungroup %>%
  ggplot(aes(FL_CM))+
  # geom_density( aes(color = "Combined"),lwd=1.5)+
  geom_density(aes(color = gear),linewidth = 0.75)+
  scale_color_hue(labels=ycounts$n_labels)+
  # scale_color_hue(labels=c("Combined",ycounts$n_labels))+
  #scale_color_manual(values = gearcols, labels = c("Combined", counts$n_labels))+
  labs(color = "Gear" , x = "Fork Length (cm)", title = county)+ #title = paste0(county,  "\n (N = ", sum(ycounts$n), ")"))+
  # theme_minimal()
  theme(legend.title = element_text(size=20), 
        legend.text = element_text(size=20),
        legend.position = "bottom",
        axis.text.x=element_text(size=20),
        axis.text.y=element_text(size=20),
        axis.title.x = element_text( size = 20),
        axis.title.y = element_text( size = 20),
        title = element_text(size = 20))+
  guides(color=guide_legend(ncol = 2))+
  geom_vline(data=muv12, aes(xintercept=grp.mean, color=gear),
             linetype="dashed")


abc19 = agr_den_v12

## Annual Density plots ####
### ALL GEARS ####
fleet_final <- length_data_1983_2022[length_data_1983_2022$fleet==1,]

fcounts = fleet_final %>%  group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
  tabyl(gear) %>%
  mutate(n_labels = paste0(gear, " (n= ", n, ")" ))

all_carSTTJ <-
  fleet_final %>%  group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
  group_by(YEAR) %>%
  dplyr::mutate(year_labs = paste0(YEAR, "\n n = ", n())) %>%
  ggplot(aes(FL_CM))+
  geom_density(linewidth = 0.75)+
  # geom_vline(data = fleet_final, aes(xintercept=mean(FL_CM)),
  # linetype="dashed", linewidth=1)+
  #scale_color_manual(values = gearcols, labels = counts$n_labels)+
  # scale_color_hue(labels=fcounts$n_labels)+
  labs(x = "Fork Length (cm)", title = paste0(county,  "\n (N = ", sum(fcounts$n), ")"))+
  facet_wrap(~year_labs, ncol = 7)
# theme_minimal()
# theme(legend.title = element_text(linewidth=14), 
# legend.text = element_text(linewidth=12))+

export_fig_page(all_carSTTJ) 
abc17 = all_carSTTJ

### TOP GEARS ####
fleet_final_gears <- length_data_gears[length_data_gears$fleet==1,]

fcounts = fleet_final_gears %>%  group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
  tabyl(gear) %>%
  mutate(n_labels = paste0(gear, " (n= ", n, ")" ))

all_car_gearsSTTJ <-
  fleet_final_gears %>%  group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
  group_by(YEAR) %>%
  dplyr::mutate(year_labs = paste0(YEAR, "\n n = ", n())) %>%
  ggplot(aes(FL_CM, color = gear))+
  geom_density(linewidth = 0.75)+
  #scale_color_manual(values = gearcols, labels = counts$n_labels)+
  scale_color_hue(labels=fcounts$n_labels)+
  labs(color = "Gear Type", x = "Fork Length (cm)", title = paste0(county,  "\n (N = ", sum(fcounts$n), ")"))+
  facet_wrap(~year_labs, ncol = 7)+
  # theme_minimal()
  guides(color=guide_legend(ncol = 2))+
  theme(legend.title = element_text(size=14), 
        legend.text = element_text(size=12),
        legend.position = "bottom")

export_fig_page(all_car_gearsSTTJ) 

abc18 = all_car_gearsSTTJ

# Aggregated cummulative density ####

counts =length_data_final %>%
  tabyl(gear) %>%
  mutate(n_labels = paste0(gear, " (n= ", n, ")" ))

abc20 = length_data_final %>%
  ggplot(aes(FL_CM))+
  stat_ecdf()+
  # scale_color_manual(values = gearcols, labels = counts$n_labels)+
  # scale_color_hue(labels = counts$n_labels)+
  labs(x = "Fork Length (cm)", title = paste0(county,  "\n (N = ", sum(counts$n), ")"))+
  theme_minimal()

# SAVE WORKSPACE ####
save.image(
  file = here::here('tools',
                    "sedar_84_sttj_yt_2022",
                    "sttj_yt_2022_figures.RData") 
)

