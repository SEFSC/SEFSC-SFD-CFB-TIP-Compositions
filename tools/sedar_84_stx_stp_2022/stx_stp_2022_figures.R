# This script will be expanding upon the length-comp script c1_nominal_len_comps.Rmd created by Molly Stevens.

# Stoplight Parrotfish STX ####

# Set up library 

librarian::shelf(here, tidyverse, ROracle, keyring, dotenv, reshape, openxlsx, janitor, DT, pander, knitr, flextable) #plyr
# if conflicts in pkgs arise, use the following:
# library(conflicted)
# conflicted::conflicts_prefer(here::here)

#M:\SFD\SECM-SFD\_Assessments-SEDAR\SEDAR_57_Caribbean_Spiny_Lobster\Analyses\StockSynthesis\2022 Update\Documentation\SEFSC-SEDAR-57U-CR-LOB-2021 
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

# Example of how to cite sedar papers Donaldson, D.M. 2004. Overview of the State Trip 
# Tickets Programs in the Gulf of Mexico. SEDAR7-DW-20. SEDAR, North Charleston, SC. 11pp. 
# Poffenberger, J. 2003. Description of the Southeast Fisheries Science Center's Logbook 
# Program for Coastal Fisheries. SEDAR-DW-29. SEDAR, North Charleston, SC. 9pp.
# # 

# Read in the most recent extraction
# tip <- readRDS(file = here("data", "raw", tip_file))
tip <- readRDS("~/SEFSC-SFD-CFB-TIP-Compositions/data/raw/com_tip_PR_VI_170867_20240109.RDS")

# Filter to STTJ and Yellowtail Snapper AND create new filterable date value 
stx_slp <- tip |> 
  filter(COUNTY_LANDED == "ST CROIX",
         OBS_STANDARD_SPECIES_CODE == "170867") |> 
  mutate(TEST_DATE = as.Date(ymd_hms(INTERVIEW_DATE)),
         FINAL_DATE = case_when(is.na(TEST_DATE) ~ INTERVIEW_DATE, 
                                TRUE ~ TEST_DATE))


source("~/SEFSC-SFD-CFB-TIP-Compositions/tools/functions/len_len_convert.R")
source("~/SEFSC-SFD-CFB-TIP-Compositions/tools/functions/fig_format_export.R")

LLconv <- read_csv("~/SEFSC-SFD-CFB-TIP-Compositions/tools/CSVs/LLconversions.csv",
                   show_col_types = FALSE)

TIP_gears <- read_csv("~/SEFSC-SFD-CFB-TIP-Compositions/tools/CSVs/tip_gears_stp_stx_LANDSTDGEARNAME.csv",
                      show_col_types = FALSE)

gearcols <- c("Net" = "#FF0000", "Trap" = "#00A08A", "Other" = "#5D057D", "Hook and Line" ="#046C9A", "Gill Net" = "#00A08A" , "Haul Seine" = "#00A08A" , "Lobster Trap" = "#00A08A" , "Trammel Net" = "#00A08A" , "Combined" = "black")



## Control Settings and Exploratory Frequency Tables

sp <- "SLP"

region <- "PUERTO RICO - USVI"

county <- "ST CROIX"

bin_size <- 1

len_type <- "FORK LENGTH"


# Calculate data available ####
flextable(as.data.frame(table(stx_slp$LENGTH_TYPE1, useNA='always')))%>%
  autofit()

# len_types <- as.data.frame(table(stx_slp$LENGTH_TYPE1, useNA='always'))
# 
# total_len_count <- len_types |> 
#   mutate(Percent = round((Freq/total_length*100), 2))
# non_fork =

n_fork_len = sum(stx_slp$LENGTH_TYPE1 == "FORK LENGTH")
n_all_len = length(stx_slp$LENGTH_TYPE1)
p_fork_len = round(n_fork_len/n_all_len, 4)*100

trip_id_unique <- as.data.frame(table(stx_slp$ID, useNA='always'))
total_trip_id_unique = nrow(trip_id_unique)


##range currently set to not drop any obs (centimeters)

min_size <- 12 

max_size <- 83   

# min(stx_slp$YEAR,na.rm = TRUE)

min_year <- 1983

max_year <- 2022 

break_year <- 2005 # this is an optional value to denote a change in management. inclusive on the upper bound. can be set to NA if not relevant.  2005 trap specifications were updated and many closures went into place


#### Filter to Commercial samples

################## PICK VARIABLES THAT I NEED HERE ;
################## ASSIGN GEARS, ETC. BASED ON FLOW CHART
## FILTER OUT COMMERCIAL SAMPLES
## replace species 3 letter code and itis number in code


tip2 <- #readRDS('./data_clean/tip_GOM.Rdata') %>%
  stx_slp %>%
  filter( #MULT_TRIP == '0')%>%  ##no mult_trip in Caribbean TIP --> ALL full catch samples
    FISHING_MODE=='COMMERCIAL',
    LENGTH_TYPE1!='NO LENGTH',
    YEAR < 2023
  ) %>%
  type_convert() %>%
  mutate(REGION_NAME = region,
         OBS_STANDARD_SPECIES_CODE = as.numeric(OBS_STANDARD_SPECIES_CODE), 
         source      = "TIP", 
         SLP         = ifelse(OBS_STANDARD_SPECIES_CODE==170867,1,0), #binary flag for queen triggerfish vs 'other'
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
         ITIS_CODE   = OBS_STANDARD_SPECIES_CODE,
         SPECIES     = OBS_STANDARD_SPECIES_NAME,
         QUANTITY,
         SLP,
         STATE_LANDED,
         COUNTY_LANDED,
         COUNTY_CODE=LANDING_AREA_COUNTY_CODE,
         PLACE_LANDED,
         DEALER,
         DEALER_CODE,
         ISLAND,
         OBS_WEIGHT_KG,
         LENGTH1_MM,
         LENGTH1,
         LENGTH_TYPE1,
         LENGTH_UNIT1,
         OBS_ID,
         LAND_STANDARD_GEAR_NAME,
         LAND_GEAR_NAME,
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

# Trap gears were originally placed in "other" category based on previous landings discussions. I moved them to their own group for visualization purposes right now.


###add length-length conversions to helper_table ; 
tip3 <- merge(tip2, TIP_gears, by.x="LAND_STANDARD_GEAR_NAME",all.x=T)
    #gfin2a <- merge(gfin, flc, by.x=c("State_Landed","County_Landed"),all.x=T)

tip4 <- tip3[tip3$SLP==1,]

# table(tip3$LAND_STANDARD_GEAR_NAME, tip3$gear, useNA='always')

#gear_groups <- c("HAND LINE", "LONGLINE") 
gear_groups <- unique(TIP_gears$gear)

    # antiforklength <- tip4 |>
    #   filter(LENGTH_TYPE1 != "FORK LENGTH")

##splitting here to more easily qa/qc above
join_length_dat <- tip4 %>% #bind_rows(tip3 , gfin2) %>%
  # len_len_convert(params = LLconv, 
  #                 raw_length = LENGTH1, 
  #                 length_type =LENGTH_TYPE1, 
  #                 length_units =  LENGTH_UNIT1, 
  #                 desired_type = len_type) %>%
  untable(num = .$QUANTITY)  %>%
  filter(LENGTH_TYPE1 == "FORK LENGTH") %>%
  # filter(NEW_GEAR_NAME %in% c(gear_groups)) %>%
  mutate( #gear = ifelse(grepl("hand*", NEW_GEAR_NAME, ignore.case = T, fixed = F), "HL", "LL"), #this part will need to be generalized for more than 2 gear types ; ++HOOKLINE IS HANDLINE IN THIS FILE ; OVERWRITING ALL HL TO LL ; CHANGED "hook*" TO "hand*"
    #gear=GEAR_GRP,
    FL_CM = LENGTH1_MM/10, 
    lbin = floor(FL_CM / bin_size)*bin_size, 
    mgt_period = ifelse(YEAR <= break_year, paste0(min_year," - ", break_year), paste0( break_year+1, " - ", max_year)))
# is "select" the problem, its #out 

# FINAL dataset here  ####

length_data_final <- join_length_dat %>%
  select(YEAR, INTERVIEW_DATE, FINAL_DATE, ID, OBS_ID, STATE = STATE_LANDED, COUNTY=COUNTY_LANDED, COUNTY_CODE, FL_CM, lbin, source, gear, gear_short, LAND_STANDARD_GEAR_NAME, LAND_GEAR_NAME, mgt_period, ISLAND, INT_TYPE, fleet, DEALER_CODE, VESSEL_ID, LICENSE) %>%  #STAT_AREA
  filter(between(FL_CM , min_size, max_size),
         ISLAND != 'NOT CODED')|> 
  mutate(GEAR_GROUP = case_when(LAND_STANDARD_GEAR_NAME == "POTS AND TRAPS; FISH" ~ "OTHER",
                                  LAND_STANDARD_GEAR_NAME == "POTS AND TRAPS; CMB" ~ "OTHER",
                                  LAND_STANDARD_GEAR_NAME == "POTS AND TRAPS; BOX TRAP" ~ "OTHER",
                                  LAND_STANDARD_GEAR_NAME == "ENTANGLING NETS (GILL) UNSPC" ~ "OTHER",
                                  LAND_STANDARD_GEAR_NAME == "SPEARS" ~ "DIVING",
                                  LAND_STANDARD_GEAR_NAME == "SPEARS; DIVING" ~ "DIVING",
                                  LAND_STANDARD_GEAR_NAME == "BY HAND; DIVING GEAR" ~ "DIVING",
                                  LAND_STANDARD_GEAR_NAME == "GILL NETS; OTHER" ~ "OTHER",
                                  LAND_STANDARD_GEAR_NAME == "LINES HAND" ~ "OTHER",
                                  LAND_STANDARD_GEAR_NAME == "LINES LONG; REEF FISH"~ "OTHER",
                                  LAND_STANDARD_GEAR_NAME == "NOT CODED" ~ "OTHER",
                                  LAND_STANDARD_GEAR_NAME == "STOP SEINES" ~ "OTHER",
                                  LAND_STANDARD_GEAR_NAME == "TRAMMEL NETS" ~ "OTHER"))
    #,

unique(length_data_final$LAND_STANDARD_GEAR_NAME)
    #INT_TYPE != 'USVI MARFIN REEFFISH SAMPLING') 
    
    # Analyst have asked for a record of dropped observations
    ######### ADAPT THIS TO OUTPUT ALL DROPPED RECORDS ABOVE--MOVE HIGHER IN THE SCRIPT TO HAVE ALL VARS
# dropped_obs <- anti_join(join_length_dat, length_data_final, by = "OBS_ID")  



##################################################DOCUMENT ALL DROPPED DATA

    # drop <- list()  ##++this creates an empty list for for loop
    # 
    # # for loop generates binned length comps for each of the final gears. Stored in list. RUN:  View(comps[[1]]) to see in R
    # 
    #  drop[[1]] <-  NON-RANDOM SAMPLES
    #  drop[[2]] <-  SIZE DATA
    #   filter(gear == final_gears[i]) %>%
    # names(comps)[[1]] <- paste0(final_gears[i], "_", bin_size, "cm") ##++tabs are named here ; could add _nom here if desired (instead of _lfd and _lfdw in main file)
    #  
#--------------------------------------------------#

    ###this is writing only size data issues (e.g. samples too small/large to be considered realistic)
    
    # write.xlsx(dropped_obs, file = paste0("./outputs/", sp, "_size_dropped_observations_", gsub("-", "", Sys.Date()), ".xlsx"))
    
    
    # this is some funky code to make sure bins with no obervations are maintained in the data. Open to finding a more elegant solution but this works

full_set <- crossing(YEAR = seq(from = min_year, to = max_year, by = 1),
                     lbin = seq(from = min_size, to = max_size, by = bin_size), 
                     N = 0) %>%
  pivot_wider(names_from = lbin, values_from = N)

comp_names = c("YEAR", "ln_fish", "ln_trips", "ln_dealers","ln_vessels", names(full_set)[-1])


# GLM analysis ####

# No filtering based on number of trips/fish per year yet, analysis of all available data.
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

unique(length_data_final$gear) # "Hook and Line", "Trap", "Other", "Net"

## All Gears ####


# str(length_data_final)
library(ggplot2)
length_data_glm <- length_data_final |>
  select(YEAR, FINAL_DATE, ID, COUNTY, FL_CM, LAND_STANDARD_GEAR_NAME, gear, GEAR_GROUP) |> 
  mutate(ID = as.character(ID)) |> 
  select(-gear)

# plot data
# 
# abc1 = length_data_glm |> 
#   ggplot(aes(x = as.Date(FINAL_DATE), y = FL_CM)) +
#   geom_point(aes(colour = LAND_STANDARD_GEAR_NAME, shape=LAND_STANDARD_GEAR_NAME), size = 1, alpha = 0.5) +
#   scale_shape_manual(values=c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 13, 14))+
#   geom_smooth(method = "lm", formula = "y ~ x", col = "black") +
#   labs(x = "", y = "Length (cm)", colour = "", shape = "") +
#   theme_bw() +
#   theme(legend.position = "bottom", text = element_text(size = 15),
#         legend.box.spacing = unit(0, "npc"), panel.grid = element_blank()) +
#   guides(colour = guide_legend(override.aes = list(size = 2)))

#------------# to do #--------------#
# Review summary, add sentence of final grouping recommendation (together)
# Check the figure and table text (together)
# Say how much data there are and in what measurements

# Other
# Check if quarto table text is working with flex t
#-----------------------------------#
gant_data <- length_data_final %>% group_by(ID) %>% filter(n() >= 3) %>% ungroup %>%
  # filter(ID >= 3) |>
  group_by(YEAR, LAND_STANDARD_GEAR_NAME) |>
  summarize(n = n(), .groups = "drop") 

abc1 <- gant_data |>
  ggplot(aes(x = YEAR, y = LAND_STANDARD_GEAR_NAME, color = LAND_STANDARD_GEAR_NAME, size = n)) +
  geom_point()  +
  labs(x = "Year", y = "", colour = "", shape = "") +
  theme_bw() + 
  theme(legend.position="null", text = element_text(size = 15))
  
# + guides(fill=guide_legend(nrow=2,byrow=TRUE))

# fit models
library(lmerTest)
   
# comparing length to date and gear in a gamma full model
mod2 = glmer(FL_CM ~ scale(FINAL_DATE) + LAND_STANDARD_GEAR_NAME + (1 | YEAR) + (1 | ID),
             data = length_data_glm, family = Gamma(link=log))

mod_contr = emmeans::emmeans(object = mod2, pairwise ~ "LAND_STANDARD_GEAR_NAME", adjust = "tukey")


allgears_multcompcld <- multcomp::cld(object = mod_contr$emmeans)

length_data_fishcount <- length_data_final |> 
  group_by(LAND_STANDARD_GEAR_NAME) |>
  tally()

length_data_tripcount <- aggregate(data = length_data_final,              
                          ID ~ LAND_STANDARD_GEAR_NAME,
                          function(ID) length(unique(ID)))

allgears_multcompcld_fish <- full_join(allgears_multcompcld, length_data_fishcount, by = "LAND_STANDARD_GEAR_NAME")
allgears_multcompcld_trip <- full_join(allgears_multcompcld_fish, length_data_tripcount, by = "LAND_STANDARD_GEAR_NAME")

allgears_multcompcld_final <- allgears_multcompcld_trip |> 
  mutate(Percentage = round(n/sum(n)*100, 2), 
         emmean = round(emmean, 2),
         asymp.LCL = round(asymp.LCL, 2),
         asymp.UCL = round(asymp.UCL, 2))|>
  dplyr::rename("Group" = ".group",
                "Gear" = "LAND_STANDARD_GEAR_NAME",
                "Mean Size" = "emmean",
                "LCL" = "asymp.LCL",
                "UCL" = "asymp.UCL") |> 
  filter(ID >= 3) |> 
  select(Gear, "Mean Size", LCL, UCL,  Group, n, Percentage )  |> 
  mutate("Gear Group" = case_when(Gear == "POTS AND TRAPS; FISH" ~ "OTHER",
                                     Gear == "POTS AND TRAPS; CMB" ~ "OTHER",
                                     Gear == "ENTANGLING NETS (GILL) UNSPC" ~ "OTHER",
                                     Gear == "SPEARS" ~ "DIVING",
                                     Gear == "SPEARS; DIVING" ~ "DIVING",
                                     Gear == "BY HAND; DIVING GEAR" ~ "DIVING",
                                     Gear == "GILL NETS; OTHER" ~ "OTHER",
                                     Gear == "LINES HAND" ~ "OTHER",
                                     Gear == "NOT CODED" ~ "OTHER",
                                     Gear == "TRAMMEL NETS" ~ "OTHER"))|> 
  # order(allgears_multcompcld_final$`Gear Group`) |> cant figure out how to alphabatize groups
  arrange(desc(Percentage)) 


tbl1 = flextable(allgears_multcompcld_final) |> 
  theme_box() %>%
  align(align = "center", part = "all") %>%
  fontsize(size=8, part="all") %>%
  autofit() 

### summary stats of pots and traps; fish and spears
# why isnt it grouping properly for the summary stats?????

# length_data_glm_ptf <- length_data_glm |> 
#   filter(LAND_STANDARD_GEAR_NAME == c("POTS AND TRAPS; FISH", "SPEARS")) |> 
#   group_by(LAND_STANDARD_GEAR_NAME)  |> 
#   summarise(
#     mean = mean(FL_CM, na.rm = TRUE),
#     sd = sd(FL_CM, na.rm = TRUE),
#     median = median(FL_CM, na.rm = TRUE),
#     IQR = IQR(FL_CM, na.rm = TRUE)
#   )

## All Gears 2012 and after ####

length_data_glm_2012 <- length_data_final |>
  filter(YEAR >= 2012) |>
  select(YEAR, FINAL_DATE, ID, COUNTY, FL_CM, LAND_STANDARD_GEAR_NAME, gear, GEAR_GROUP) |>
  mutate(ID = as.character(ID)) |>
  select(-gear)
# 
# abc2 = length_data_glm_2012 |> 
#   ggplot(aes(x = as.Date(FINAL_DATE), y = FL_CM)) +
#   geom_point(aes(colour = LAND_STANDARD_GEAR_NAME, shape = LAND_STANDARD_GEAR_NAME), size = 1, alpha = 0.5) +
#   geom_smooth(method = "lm", formula = "y ~ x", col = "black") +
#   # facet_wrap(~ COUNTY_LANDED) +
#   labs(x = "", y = "Length (cm)", colour = "", shape = "") +
#   theme_bw() +
#   theme(legend.position = "bottom", legend.text = element_text(size = 15),
#         legend.box.spacing = unit(0, "npc"), panel.grid = element_blank()) +
#   guides(colour = guide_legend(override.aes = list(size = 2)))

# n() doesnt work in my code, what is an alternative for what we're trying to do
gant_data_12 <- length_data_glm_2012 %>% group_by(ID) %>% dplyr::filter(n() >= 3) %>% ungroup %>%
  # filter(ID >= 3) |>
  group_by(YEAR, LAND_STANDARD_GEAR_NAME) |>
  dplyr::summarize(n = n(), .groups = "drop") |> 
  mutate(YEAR = as.integer(YEAR))

# library(scales)

abc2 <- gant_data_12 |>
  ggplot(aes(x = YEAR, y = LAND_STANDARD_GEAR_NAME, color = LAND_STANDARD_GEAR_NAME, size = n)) +
  geom_point()  +
  labs(x = "Year", y = "", colour = "", shape = "") +
  theme_bw() + 
  theme(legend.position="null", text = element_text(size = 15))+
  xlim(2012,2022)

# fit models

# comparing length to date and gear in a gamma full model
mod2 = glmer(FL_CM ~ scale(FINAL_DATE) + LAND_STANDARD_GEAR_NAME + (1 | YEAR) + (1 | ID),
             data = length_data_glm_2012, family = Gamma(link=log))

# pairwise comparisons 
mod_contr = emmeans::emmeans(object = mod2, pairwise ~ "LAND_STANDARD_GEAR_NAME", adjust = "tukey")

allgears_multcompcld_2012 <- multcomp::cld(object = mod_contr$emmeans)

length_data_fishcount_12 <- length_data_glm_2012 |> 
  group_by(LAND_STANDARD_GEAR_NAME) |>
  tally()
length_data_tripcount_12 <- aggregate(data = length_data_glm_2012,               
                                   ID ~ LAND_STANDARD_GEAR_NAME,
                                   function(ID) length(unique(ID)))

allgears_multcompcld_fish_2012 <- full_join(allgears_multcompcld_2012, length_data_fishcount_12, by = "LAND_STANDARD_GEAR_NAME")
allgears_multcompcld_trip_2012 <- full_join(allgears_multcompcld_fish_2012, length_data_tripcount_12, by = "LAND_STANDARD_GEAR_NAME")

allgears_multcompcld_finaL_2012 <- allgears_multcompcld_trip_2012  |> 
  mutate(Percentage = round(n/sum(n)*100, 2), 
         emmean = round(emmean, 2),
         asymp.LCL = round(asymp.LCL, 2),
         asymp.UCL = round(asymp.UCL, 2))|>
  dplyr::rename("Group" = ".group",
                "Gear" = "LAND_STANDARD_GEAR_NAME",
                "Mean Size" = "emmean",
                "LCL" = "asymp.LCL",
                "UCL" = "asymp.UCL") |> 
  filter(ID >= 3) |> 
  select(Gear, "Mean Size", LCL, UCL,  Group, n, Percentage ) |> 
  mutate("Gear Group" = case_when(Gear == "BY HAND; DIVING GEAR" ~ 'DIVING',
                                  Gear == "POTS AND TRAPS; FISH" ~ 'OTHER',
                                  Gear == "SPEARS; DIVING" ~ 'DIVING')) |> 
  arrange(desc(Percentage)) 


tbl2 = flextable(allgears_multcompcld_finaL_2012) |> 
  theme_box() %>%
  align(align = "center", part = "all") %>%
  fontsize(size=8, part="all") %>%
  autofit()


## Diving ####

# use_gear_dv <- length_data_final |>
#   select(YEAR, FINAL_DATE, ID, COUNTY, FL_CM, LAND_STANDARD_GEAR_NAME, gear) |> 
#   filter(gear == "Diving") |> 
#   mutate(ID = as.character(ID)) |> 
#   select(-gear)
# 
# # plot data
# 
# abc3 = ggplot(use_gear_dv, aes(x = as.Date(FINAL_DATE), y = FL_CM)) +
#   geom_point(aes(colour = LAND_STANDARD_GEAR_NAME, shape = LAND_STANDARD_GEAR_NAME), size = 1, alpha = 0.5) +
#   geom_smooth(method = "lm", formula = "y ~ x", col = "black") +
#   # facet_wrap(~ COUNTY_LANDED) +
#   labs(x = "", y = "Length (cm)", colour = "", shape = "") +
#   theme_bw() +
#   theme(legend.position = "bottom", legend.text = element_text(size = 15),
#         legend.box.spacing = unit(0, "npc"), panel.grid = element_blank()) +
#   guides(colour = guide_legend(override.aes = list(size = 2)))
# 
# 
# # fit models
# 
# # comparing length to date and gear in a gamma full model
# mod2 = glmer(FL_CM ~ scale(FINAL_DATE) + LAND_STANDARD_GEAR_NAME + (1 | YEAR) + (1 | ID),
#              data = use_gear_dv, family = Gamma(link=log))
# 
# 
# ### no significant p values for time period or gears, further comparisons not needed 
# 
# # pairwise comparisons 
# mod_contr = emmeans::emmeans(object = mod2, pairwise ~ "LAND_STANDARD_GEAR_NAME", adjust = "tukey")
# 
# dv_multcompcld <- multcomp::cld(object = mod_contr$emmeans)
# 
# dv_fishcount <- use_gear_dv |> 
#   group_by(LAND_STANDARD_GEAR_NAME) |>
#   tally()
# dv_tripcount <- aggregate(data = use_gear_dv,                # Applying aggregate
#                                       ID ~ LAND_STANDARD_GEAR_NAME,
#                                       function(ID) length(unique(ID)))
# 
# dv_multcompcld_fish <- full_join(dv_multcompcld, dv_fishcount, by = "LAND_STANDARD_GEAR_NAME")
# dv_multcompcld_trip <- full_join(dv_multcompcld_fish, dv_tripcount, by = "LAND_STANDARD_GEAR_NAME")
# 
# dv_multcompcld_final <- dv_multcompcld_trip  |> 
#   mutate(Percentage = round(n/sum(n)*100, 2), 
#          emmean = round(emmean, 2),
#          asymp.LCL = round(asymp.LCL, 2),
#          asymp.UCL = round(asymp.UCL, 2))|>
#   dplyr::rename("Group" = ".group",
#                 "Gear" = "LAND_STANDARD_GEAR_NAME",
#                 "Mean Size" = "emmean",
#                 "LCL" = "asymp.LCL",
#                 "UCL" = "asymp.UCL") |> 
#   filter(ID >= 3) |> 
#   select(Gear, "Mean Size", LCL, UCL,  Group, n, Percentage ) |> 
#   arrange(desc(Percentage))
# 
# tbl3 = flextable(dv_multcompcld_final) |> 
#   theme_box() %>%
#   align(align = "center", part = "all") %>%
#   fontsize(size=8, part="all") %>%
#   autofit()

## Trap ####
#  
# use_gear_tr <- length_data_final |>
#   select(YEAR, FINAL_DATE, ID, COUNTY, FL_CM, LAND_STANDARD_GEAR_NAME, gear) |> 
#   filter(gear == "Trap") |> 
#   mutate(ID = as.character(ID)) |> 
#   select(-gear)
# 
# # plot data
# 
# abc4 = ggplot(use_gear_tr, aes(x = as.Date(FINAL_DATE), y = FL_CM)) +
#   geom_point(aes(colour = LAND_STANDARD_GEAR_NAME, shape = LAND_STANDARD_GEAR_NAME), size = 1, alpha = 0.5) +
#   geom_smooth(method = "lm", formula = "y ~ x", col = "black") +
#   # facet_wrap(~ COUNTY_LANDED) +
#   labs(x = "", y = "Length (cm)", colour = "", shape = "") +
#   theme_bw() +
#   theme(legend.position = "bottom", legend.text = element_text(size = 15),
#         legend.box.spacing = unit(0, "npc"), panel.grid = element_blank()) +
#   guides(colour = guide_legend(override.aes = list(size = 2)))
# 
# 
# # fit models
# 
# # comparing length to date and gear in a gamma full model
# mod2 = glmer(FL_CM ~ scale(FINAL_DATE) + LAND_STANDARD_GEAR_NAME + (1 | YEAR) + (1 | ID),
#              data = use_gear_tr, family = Gamma(link=log))
# 
# 
# # pairwise comparisons (if needed)(needed for Hook and Line)
# mod_contr = emmeans::emmeans(object = mod2, pairwise ~ "LAND_STANDARD_GEAR_NAME", adjust = "tukey")
# 
# trap_multcompcld <- multcomp::cld(object = mod_contr$emmeans)
# 
# use_gear_tr_fishcount <- use_gear_tr |> 
#   group_by(LAND_STANDARD_GEAR_NAME) |> 
#   tally()
# use_gear_tr_tripcount <- aggregate(data = use_gear_tr,                # Applying aggregate
#                                    ID ~ LAND_STANDARD_GEAR_NAME,
#                                    function(ID) length(unique(ID)))
# 
# trap_multcompcld_fish <- full_join(trap_multcompcld, use_gear_tr_fishcount, by = "LAND_STANDARD_GEAR_NAME")
# trap_multcompcld_trip <- full_join(trap_multcompcld_fish, use_gear_tr_tripcount, by = "LAND_STANDARD_GEAR_NAME")
# 
# trap_multcompcld_final <- trap_multcompcld_trip  |> 
#   mutate(Percentage = round(n/sum(n)*100, 2), 
#          emmean = round(emmean, 2),
#          asymp.LCL = round(asymp.LCL, 2),
#          asymp.UCL = round(asymp.UCL, 2))|>
#   dplyr::rename("Group" = ".group",
#                 "Gear" = "LAND_STANDARD_GEAR_NAME",
#                 "Mean Size" = "emmean",
#                 "LCL" = "asymp.LCL",
#                 "UCL" = "asymp.UCL") |> 
#   filter(ID >= 3) |> 
#   select(Gear, "Mean Size", LCL, UCL,  Group, n, Percentage ) |> 
#   arrange(desc(Percentage))
# 
# tbl4 = flextable(trap_multcompcld_final) |> 
#   theme_box() %>%
#   align(align = "center", part = "all") %>%
#   fontsize(size=8, part="all") %>%
#   autofit()

## Net - LAND_STANDARD_GEAR_NAME ####

# # net land_standard_gear_name
# use_gear_nsg <- length_data_final |>
#   select(YEAR, FINAL_DATE, ID, COUNTY, FL_CM, LAND_STANDARD_GEAR_NAME, gear) |> 
#   filter(gear == "Net") |> 
#   mutate(ID = as.character(ID)) |> 
#   select(-gear)
# 
# # plot data
# 
# abc5 = ggplot(use_gear_nsg, aes(x = as.Date(FINAL_DATE), y = FL_CM)) +
#   geom_point(aes(colour = LAND_STANDARD_GEAR_NAME, shape = LAND_STANDARD_GEAR_NAME), size = 1, alpha = 0.5) +
#   geom_smooth(method = "lm", formula = "y ~ x", col = "black") +
#   # facet_wrap(~ COUNTY_LANDED) +
#   labs(x = "", y = "Length (cm)", colour = "", shape = "") +
#   theme_bw() +
#   theme(legend.position = "bottom", legend.text = element_text(size = 15),
#         legend.box.spacing = unit(0, "npc"), panel.grid = element_blank()) +
#   guides(colour = guide_legend(override.aes = list(size = 2)))
# 
# 
# # fit models
# 
# # comparing length to date and gear in a gamma full model
# mod2 = glmer(FL_CM ~ scale(FINAL_DATE) + LAND_STANDARD_GEAR_NAME + (1 | YEAR) + (1 | ID),
#              data = use_gear_nsg, family = Gamma(link=log)) 
# 
# ### no significant p values for time period or gears, further comparisons not needed 
# 
# # pairwise comparisons 
# mod_contr = emmeans::emmeans(object = mod2, pairwise ~ "LAND_STANDARD_GEAR_NAME", adjust = "tukey")
# 
# ntsg_multcompcld <- multcomp::cld(object = mod_contr$emmeans)
# 
# ntsg_fishcount <- use_gear_nsg |> 
#   group_by(LAND_STANDARD_GEAR_NAME) |>
#   tally()
# 
# ntsg_tripcount <- aggregate(data = use_gear_nsg,                # Applying aggregate
#                           ID ~ LAND_STANDARD_GEAR_NAME,
#                           function(ID) length(unique(ID)))
# 
# ntsg_multcompcld_fish <- full_join(ntsg_multcompcld, ntsg_fishcount, by = "LAND_STANDARD_GEAR_NAME")
# ntsg_multcompcld_trip <- full_join(ntsg_multcompcld_fish, ntsg_tripcount, by = "LAND_STANDARD_GEAR_NAME")
# 
# ntsg_multcompcld_final <- ntsg_multcompcld_trip |> 
#   mutate(Percentage = round(n/sum(n)*100, 2), 
#          emmean = round(emmean, 2),
#          asymp.LCL = round(asymp.LCL, 2),
#          asymp.UCL = round(asymp.UCL, 2))|>
#   dplyr::rename("Group" = ".group",
#                 "Gear" = "LAND_STANDARD_GEAR_NAME",
#                 "Mean Size" = "emmean",
#                 "LCL" = "asymp.LCL",
#                 "UCL" = "asymp.UCL") |> 
#   filter(ID >= 3) |> 
#   select(Gear, "Mean Size", LCL, UCL,  Group, n, Percentage ) |> 
#   arrange(desc(Percentage))
# 
# tbl5 = flextable(ntsg_multcompcld_final)  |> 
#   theme_box() %>%
#   align(align = "center", part = "all") %>%
#   fontsize(size=8, part="all") %>%
#   autofit()

## Net - LAND_GEAR_NAME ####
# 
# # net land_standard_gear_name
# use_gear_nlg <- length_data_final |>
#   select(YEAR, FINAL_DATE, ID, COUNTY, FL_CM, LAND_GEAR_NAME, gear) |>
#   filter(gear == "Net") |>
#   mutate(ID = as.character(ID)) |>
#   select(-gear)
# 
# # plot data
# 
# abc6 = ggplot(use_gear_nlg, aes(x = as.Date(FINAL_DATE), y = FL_CM)) +
#   geom_point(aes(colour = LAND_GEAR_NAME, shape = LAND_GEAR_NAME), size = 1, alpha = 0.5) +
#   geom_smooth(method = "lm", formula = "y ~ x", col = "black") +
#   # facet_wrap(~ COUNTY_LANDED) +
#   labs(x = "", y = "Length (cm)", colour = "", shape = "") +
#   theme_bw() +
#   theme(legend.position = "bottom", legend.text = element_text(size = 15),
#         legend.box.spacing = unit(0, "npc"), panel.grid = element_blank()) +
#   guides(colour = guide_legend(override.aes = list(size = 2)))
# 
# 
# # fit models
# # comparing length to date and gear in a gamma full model
# mod2 = glmer(FL_CM ~ scale(FINAL_DATE) + LAND_GEAR_NAME + (1 | YEAR) + (1 | ID),
#              data = use_gear_nlg, family = Gamma(link=log))
# 
# # pairwise comparisons 
# mod_contr = emmeans::emmeans(object = mod2, pairwise ~ "LAND_GEAR_NAME", adjust = "tukey")
# 
# ntlg_multcompcld <- multcomp::cld(object = mod_contr$emmeans)
# 
# ntlg_fishcount <- use_gear_nlg |> 
#   group_by(LAND_GEAR_NAME) |>
#   tally()
# 
# ntlg_tripcount <- aggregate(data = use_gear_nlg,                # Applying aggregate
#                             ID ~ LAND_GEAR_NAME,
#                             function(ID) length(unique(ID)))
# 
# ntlg_multcompcld_fish <- full_join(ntlg_multcompcld, ntlg_fishcount, by = "LAND_GEAR_NAME")
# ntlg_multcompcld_trip <- full_join(ntlg_multcompcld_fish, ntlg_tripcount, by = "LAND_GEAR_NAME")
# 
# ntlg_multcompcld_final <- ntlg_multcompcld_trip |> 
#   mutate(Percentage = round(n/sum(n)*100, 2), 
#          emmean = round(emmean, 2),
#          asymp.LCL = round(asymp.LCL, 2),
#          asymp.UCL = round(asymp.UCL, 2))|>
#   dplyr::rename("Group" = ".group",
#                 "Gear" = "LAND_GEAR_NAME",
#                 "Mean Size" = "emmean",
#                 "LCL" = "asymp.LCL",
#                 "UCL" = "asymp.UCL") |> 
#   filter(ID >= 3) |> 
#   select(Gear, "Mean Size", LCL, UCL,  Group, n, Percentage ) |> 
#   arrange(desc(Percentage))
# 
# tbl6 = flextable(ntsg_multcompcld_final) |> 
#   theme_box() %>%
#   align(align = "center", part = "all") %>%
#   fontsize(size=8, part="all") %>%
#   autofit()

# Gear Density Plots ####

# Filtered to years with 30 or more length records (regardless of gear) per year.

## Aggregated density plots ####

# ycounts =length_data_final %>% group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
#   tabyl(gear) %>%
#   mutate(n_labels = paste0(gear, " (n= ", n, ")" ))
# 
# library(plyr)
# mu <- ddply(length_data_final, "gear", summarise, grp.mean=mean(FL_CM))
# head(mu)
# 
# agr_den_allgears <- length_data_final %>% group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
#   ggplot(aes(FL_CM))+
#   # geom_density( aes(color = "Combined"),lwd=1.5)+
#   geom_density(aes(color = gear), linewidth = 0.75)+
#   scale_color_hue(labels=ycounts$n_labels)+
#   # scale_color_hue(labels=c("Combined",ycounts$n_labels))+
#   #scale_color_manual(values = gearcols, labels = c("Combined", counts$n_labels))+
#   labs(color = "Gear Type", x = "Fork Length (cm)", title = paste0(county,  "\n (N = ", sum(ycounts$n), ")"))+
#   # theme_minimal()
#   theme(legend.title = element_text(size=14), 
#         legend.text = element_text(size=12))+
#   geom_vline(data=mu, aes(xintercept=grp.mean, color=gear),
#                    linetype="dashed")
# 
# abc7 = agr_den_allgears

### no gear groupings ####
ycounts =length_data_final %>% group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
  tabyl(GEAR_GROUP) %>%
  mutate(n_labels = paste0(GEAR_GROUP, " (n= ", n, ")" ))

library(plyr)

agr_den_NOgears <- length_data_final %>% group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
  ggplot(aes(FL_CM))+
  # geom_density( aes(color = "Combined"),lwd=1.5)+
  geom_density(linewidth = 0.75)+
  # scale_color_hue(labels=ycounts$n_labels)+
  # scale_color_hue(labels=c("Combined",ycounts$n_labels))+
  #scale_color_manual(values = gearcols, labels = c("Combined", counts$n_labels))+
  labs(x = "Fork Length (cm)", title = paste0(county,  "\n (N = ", sum(ycounts$n), ")"))+
  # theme_minimal()
  theme(legend.title = element_text(size=14), 
        legend.text = element_text(size=15))+
  geom_vline(aes(xintercept=mean(FL_CM)),
              linetype="dashed", linewidth=1)

abc14 = agr_den_NOgears

# after 2012
ycounts =length_data_glm_2012 %>% group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
  tabyl(GEAR_GROUP) %>%
  mutate(n_labels = paste0(GEAR_GROUP, " (n= ", n, ")" ))


agr_den_NOgears_12 <- length_data_glm_2012 %>% group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
  ggplot(aes(FL_CM))+
  # geom_density( aes(color = "Combined"),lwd=1.5)+
  geom_density(linewidth = 0.75)+
  # scale_color_hue(labels=ycounts$n_labels)+
  # scale_color_hue(labels=c("Combined",ycounts$n_labels))+
  #scale_color_manual(values = gearcols, labels = c("Combined", counts$n_labels))+
  labs(x = "Fork Length (cm)", title = paste0(county,  "\n (N = ", sum(ycounts$n), ")"))+
  # theme_minimal()
  theme(legend.title = element_text(size=14), 
        legend.text = element_text(size=15))+
  geom_vline(aes(xintercept=mean(FL_CM)),
             linetype="dashed", linewidth=1)

abc16 = agr_den_NOgears_12

### spear vs pots and traps;fish ####

ycounts =length_data_final %>% group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
  tabyl("GEAR_GROUP") %>%
  mutate(n_labels = paste0(GEAR_GROUP, " (n= ", n, ")" ))

library(plyr)
muv <- ddply(length_data_final, "GEAR_GROUP", summarise, grp.mean=mean(FL_CM))
head(muv)

agr_den_v <- length_data_final %>% group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
  ggplot(aes(FL_CM))+
  # geom_density( aes(color = "Combined"),lwd=1.5)+
  geom_density(aes(color = GEAR_GROUP),linewidth = 0.75)+
  scale_color_hue(labels=ycounts$n_labels)+
  # scale_color_hue(labels=c("Combined",ycounts$n_labels))+
  #scale_color_manual(values = gearcols, labels = c("Combined", counts$n_labels))+
  labs(color = "Gear Group" , x = "Fork Length (cm)", title = paste0(county,  "\n (N = ", sum(ycounts$n), ")"))+
  # theme_minimal()
  theme(legend.title = element_text(size=14), 
        legend.text = element_text(size=15))+
  geom_vline(data=muv, aes(xintercept=grp.mean, color=GEAR_GROUP),
                     linetype="dashed")


abc15 = agr_den_v

# SAVE WORKSPACE
save.image(
  file = here::here('tools',
                    "sedar_84_stx_stp_2022",
                    "stx_stp_2022_figures.RData") 
)

## Gear Groupings
# Filtered to minimum 30 fish per year

### Diving ####
# dv<- length_data_final[length_data_final$gear_short=='DV',]
# counts = dv %>% group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
#   tabyl(LAND_STANDARD_GEAR_NAME) %>%
#   mutate(n_labels = paste0(LAND_STANDARD_GEAR_NAME, " (n= ", n, ")" ))
# 
# mudv <- ddply(dv, "LAND_STANDARD_GEAR_NAME", summarise, grp.mean=mean(FL_CM))
# head(mudv)
# 
# 
# agr_den_dv <- dv %>% group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
#   ggplot(aes(FL_CM, color = LAND_STANDARD_GEAR_NAME))+
#   geom_density(size = 0.75)+
#   # scale_color_manual( values = gearcols, labels = counts$n_labels)+
#   scale_color_hue(labels = counts$n_labels)+
#   labs(color = "Gear Type", x = "Fork Length (cm)", title = paste0(county,  "\n (N = ", sum(counts$n), ")"))+
#   # facet_wrap(~ISLAND,ncol=1) +
#   # theme_minimal()
#   theme(legend.title = element_text(size=14), 
#         legend.text = element_text(size=12))+
#   geom_vline(data=mudv, aes(xintercept=grp.mean, color=LAND_STANDARD_GEAR_NAME),
#              linetype="dashed")
# abc8 = agr_den_dv

# flextable(as.data.frame(table(length_data_final$LAND_STANDARD_GEAR_NAME, useNA='always')))%>%
#   autofit()

### Traps ####
# trap <- length_data_final[length_data_final$gear_short=='TR',]
# counts = trap %>% group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
#   tabyl(LAND_STANDARD_GEAR_NAME) %>%
#   mutate(n_labels = paste0(LAND_STANDARD_GEAR_NAME, " (n= ", n, ")" ))
# 
# trap2 <- trap %>% group_by(LAND_STANDARD_GEAR_NAME) %>% filter(n() >= 2) %>% ungroup
# mutrap <- ddply(trap2, "LAND_STANDARD_GEAR_NAME", summarise, grp.mean=mean(FL_CM))
# head(mutrap)
# 
# agr_den_trap <- trap %>% group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
#   ggplot(aes(FL_CM, color = LAND_STANDARD_GEAR_NAME))+
#   geom_density(size = 0.75)+
#   # scale_color_manual( values = gearcols, labels = counts$n_labels)+
#   scale_color_hue(labels = counts$n_labels)+
#   labs(color = "Gear Type", x = "Fork Length (cm)", title = paste0(county,  "\n (N = ", sum(counts$n), ")"))+
#   # facet_wrap(~ISLAND,ncol=1) +
#   # theme_minimal()
#   theme(legend.title = element_text(size=14),
#         legend.text = element_text(size=12))+
#   geom_vline(data=mutrap, aes(xintercept=grp.mean, color=LAND_STANDARD_GEAR_NAME),
#              linetype="dashed")
# abc9 = agr_den_trap

### Nets ####
# net <- length_data_final[length_data_final$gear_short=='NT',]
# counts = net %>% group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
#   tabyl(LAND_STANDARD_GEAR_NAME) %>%
#   mutate(n_labels = paste0(LAND_STANDARD_GEAR_NAME, " (n= ", n, ")" ))
# 
# munet <- ddply(net, "LAND_STANDARD_GEAR_NAME", summarise, grp.mean=mean(FL_CM))
# head(munet)
# 
# agr_den_net <- net %>% group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
#   ggplot(aes(FL_CM,color = LAND_STANDARD_GEAR_NAME))+
#   geom_density(size = 0.75)+
#   # scale_color_manual( values = gearcols, labels = counts$n_labels)+
#   scale_color_hue(labels = counts$n_labels)+
#   labs(color = "Gear Type", x = "Fork Length (cm)", title = paste0(county,  "\n (N = ", sum(counts$n), ")"))+
#   # facet_wrap(~ISLAND,ncol=1) +
#   # theme_minimal()
#   theme(legend.title = element_text(size=14), 
#         legend.text = element_text(size=12))+
#   geom_vline(data=munet, aes(xintercept=grp.mean, color=LAND_STANDARD_GEAR_NAME),
#              linetype="dashed")
# abc10 = agr_den_net
# 
# 
# net_LANDGEAR <- length_data_final[length_data_final$gear_short=='NT',]
# counts = net_LANDGEAR %>% group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
#   tabyl(LAND_GEAR_NAME) %>%
#   mutate(n_labels = paste0(LAND_GEAR_NAME, " (n= ", n, ")" ))
# 
# # trap2 <- trap %>% group_by(YEAR) %>% filter(n() >= 30) %>% ungroup
# munetLG <- ddply(net_LANDGEAR, "LAND_GEAR_NAME", summarise, grp.mean=mean(FL_CM))
# head(munetLG)
# 
# agr_den_netLG <- net_LANDGEAR %>% group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
#   ggplot(aes(FL_CM,color = LAND_GEAR_NAME))+
#   geom_density(size = 0.75)+
#   # scale_color_manual( values = gearcols, labels = counts$n_labels)+
#   scale_color_hue(labels = counts$n_labels)+
#   labs(color = "Gear Type", x = "Fork Length (cm)", title = paste0(county,  "\n (N = ", sum(counts$n), ")"))+
#   # facet_wrap(~ISLAND,ncol=1) +
#   # theme_minimal()
#   theme(legend.title = element_text(size=14), 
#         legend.text = element_text(size=12))+
#   geom_vline(data=munetLG, aes(xintercept=grp.mean, color=LAND_GEAR_NAME),
#              linetype="dashed")
# abc11 = agr_den_netLG

### Other ####
# ot<- length_data_final[length_data_final$gear_short=='OT',]
# counts = ot %>% group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
#   tabyl(LAND_STANDARD_GEAR_NAME) %>%
#   mutate(n_labels = paste0(LAND_STANDARD_GEAR_NAME, " (n= ", n, ")" ))
# 
# ot2 <- ot %>% group_by(LAND_STANDARD_GEAR_NAME) %>% filter(n() >= 2) %>% ungroup
# muot <- ddply(ot2, "LAND_STANDARD_GEAR_NAME", summarise, grp.mean=mean(FL_CM))
# head(muot)
# 
# agr_den_ot <- ot %>% group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
#   ggplot(aes(FL_CM, color = LAND_STANDARD_GEAR_NAME))+
#   geom_density(size = 0.75)+
#   # scale_color_manual( values = gearcols, labels = counts$n_labels)+
#   scale_color_hue(labels = counts$n_labels)+
#   labs(color = "Gear Type", x = "Fork Length (cm)", title = paste0(county,  "\n (N = ", sum(counts$n), ")"))+
#   # facet_wrap(~ISLAND,ncol=1) +
#   # theme_minimal()
#   theme(legend.title = element_text(size=14), 
#         legend.text = element_text(size=12))+
#   geom_vline(data=muot, aes(xintercept=grp.mean, color=LAND_STANDARD_GEAR_NAME),
#              linetype="dashed")
# abc12 = agr_den_ot


## Break at 2005 ####
# 
# 
# length_data_final %>%  group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>% # filter(YEAR > 2011) |> 
#   ggplot(aes(FL_CM, color = gear))+
#   geom_density(size = 0.75)+
#   scale_color_hue(labels=ycounts$n_labels)+
#   #  scale_color_manual(values = gearcols, labels = counts$n_labels)+
#   labs(color = "Gear Type", x = "Fork Length (cm)", title = paste0(county,  "\n (N = ", sum(ycounts$n), ")"))+
#   # facet_wrap(~ISLAND,ncol=1) +
#   facet_wrap(~mgt_period) +
#   # theme_minimal()
#   theme(legend.title = element_text(size=14), 
#         legend.text = element_text(size=12))
# 
# 
# 
# DV <- length_data_final[length_data_final$gear_short=='DV',]
# counts = DV %>% group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
#   tabyl(LAND_STANDARD_GEAR_NAME) %>%
#   mutate(n_labels = paste0(LAND_STANDARD_GEAR_NAME, " (n= ", n, ")" ))
# 
# DV %>%  group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
#   ggplot(aes(FL_CM, color = LAND_STANDARD_GEAR_NAME))+
#   geom_density(size = 0.75)+
#   # scale_color_manual( values = gearcols, labels = counts$n_labels)+
#   scale_color_hue(labels = counts$n_labels)+
#   labs(color = "Gear Type", x = "Fork Length (cm)", title = paste0(county,  "\n (N = ", sum(counts$n), ")"))+
#   # facet_wrap(~ISLAND,ncol=1) +
#   facet_wrap(~mgt_period) +
#   # theme_minimal()
#   theme(legend.title = element_text(size=14), 
#         legend.text = element_text(size=12))
# 
# 
# 
# #nets might need to be another category because haul seines, trammel, and entangling (gill unsp) have too many occurances to be in other 
# 
# net <- length_data_final[length_data_final$gear_short=='NT',]
# counts = net %>% group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
#   tabyl(LAND_STANDARD_GEAR_NAME) %>%
#   mutate(n_labels = paste0(LAND_STANDARD_GEAR_NAME, " (n= ", n, ")" ))
# 
# net %>%  group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
#   ggplot(aes(FL_CM,color = LAND_STANDARD_GEAR_NAME))+
#   geom_density(size = 0.75)+
#   # scale_color_manual( values = gearcols, labels = counts$n_labels)+
#   scale_color_hue(labels = counts$n_labels)+
#   labs(color = "Gear Type", x = "Fork Length (cm)", title = paste0(county,  "\n (N = ", sum(counts$n), ")"))+
#   # facet_wrap(~ISLAND,ncol=1) +
#   facet_wrap(~mgt_period) +
#   # theme_minimal()
#   theme(legend.title = element_text(size=14), 
#         legend.text = element_text(size=12))
# 
# 
# 
# 
# trap <- length_data_final[length_data_final$gear_short=='TR',]
# counts = trap %>% group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
#   tabyl(LAND_STANDARD_GEAR_NAME) %>%
#   mutate(n_labels = paste0(LAND_STANDARD_GEAR_NAME, " (n= ", n, ")" ))
# 
# trap %>%  group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
#   ggplot(aes(FL_CM,color = LAND_STANDARD_GEAR_NAME))+
#   geom_density(size = 0.75)+
#   # scale_color_manual( values = gearcols, labels = counts$n_labels)+
#   scale_color_hue(labels = counts$n_labels)+
#   labs(color = "Gear Type", x = "Fork Length (cm)", title = paste0(county,  "\n (N = ", sum(counts$n), ")"))+
#   # facet_wrap(~ISLAND,ncol=1) +
#   facet_wrap(~mgt_period) +
#   # theme_minimal()
#   theme(legend.title = element_text(size=14), 
#         legend.text = element_text(size=12))
# 
# 
# 
# ot<- length_data_final[length_data_final$gear_short=='OT',]
# counts = ot %>% group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
#   tabyl(LAND_STANDARD_GEAR_NAME) %>%
#   mutate(n_labels = paste0(LAND_STANDARD_GEAR_NAME, " (n= ", n, ")" ))
# 
# ot %>%  group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
#   ggplot(aes(FL_CM, color = LAND_STANDARD_GEAR_NAME))+
#   geom_density(size = 0.75)+
#   # scale_color_manual( values = gearcols, labels = counts$n_labels)+
#   scale_color_hue(labels = counts$n_labels)+
#   labs(color = "Gear Type", x = "Fork Length (cm)", title = paste0(county,  "\n (N = ", sum(counts$n), ")"))+
#   # facet_wrap(~ISLAND,ncol=1) +
#   facet_wrap(~mgt_period) +
#   # theme_minimal()
#   theme(legend.title = element_text(size=14), 
#         legend.text = element_text(size=12))
