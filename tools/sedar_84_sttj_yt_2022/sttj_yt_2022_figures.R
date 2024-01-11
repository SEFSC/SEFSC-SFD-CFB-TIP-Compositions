# This script will be expanding upon the length-comp script c1_nominal_len_comps.Rmd created by Molly Stevens.

# Yellowtail Snapper STTJ ####

## Set up library  ####

librarian::shelf(here, tidyverse, ROracle, keyring, dotenv, reshape, openxlsx, janitor, DT, pander, knitr, flextable) #plyr
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
tip <- readRDS("~/SEFSC-SFD-CFB-TIP-Compositions/data/raw/com_tip_PR_VI_168907_20240111.RDS")

# Filter to STTJ and Yellowtail Snapper AND create new filterable date value 
sttj_yt <- tip |> 
  filter(COUNTY_LANDED %in% c("ST THOMAS", "ST JOHN"),
         OBS_STANDARD_SPECIES_CODE == "168907") |> 
  mutate(TEST_DATE = as.Date(ymd_hms(INTERVIEW_DATE)),
         FINAL_DATE = case_when(is.na(TEST_DATE) ~ INTERVIEW_DATE, 
                                TRUE ~ TEST_DATE))


source("~/SEFSC-SFD-CFB-TIP-Compositions/tools/functions/len_len_convert.R")
source("~/SEFSC-SFD-CFB-TIP-Compositions/tools/functions/fig_format_export.R")

LLconv <- read_csv("~/SEFSC-SFD-CFB-TIP-Compositions/data/CSVs/LLconversions.csv",
                   show_col_types = FALSE)

TIP_gears <- read_csv("~/SEFSC-SFD-CFB-TIP-Compositions/data/CSVs/tip_gears_yts_sttj_landstdgearname.csv",
                      show_col_types = FALSE)

gearcols <- c("Net" = "#FF0000", "Trap" = "#00A08A", "Other" = "#5D057D", "Hook and Line" ="#046C9A", "Gill Net" = "#00A08A" , "Haul Seine" = "#00A08A" , "Lobster Trap" = "#00A08A" , "Trammel Net" = "#00A08A" , "Combined" = "black")


## Control Settings and Exploratory Frequency Tables ####

sp <- "YTS"

region <- "PUERTO RICO - USVI"

county <- "ST THOMAS/ST JOHN"

bin_size <- 1

len_type <- "FORK LENGTH"

flextable(as.data.frame(table(sttj_yt$LENGTH_TYPE1, useNA='always')))%>%
  autofit()
flextable(as.data.frame(table(sttj_yt$LAND_STANDARD_GEAR_NAME, useNA='always')))%>%
  autofit()
##range currently set to not drop any obs (centimeters)

min_size <- 14 

max_size <- 80   

min(sttj_yt$YEAR,na.rm = TRUE)

min_year <- 1983

max_year <- 2022 

break_year <- 2005 # this is an optional value to denote a change in management. inclusive on the upper bound. can be set to NA if not relevant.  2005 trap specifications were updated and many closures went into place


#### Filter to Commercial samples

################## PICK VARIABLES THAT I NEED HERE ;
################## ASSIGN GEARS, ETC. BASED ON FLOW CHART
## FILTER OUT COMMERCIAL SAMPLES

tip2 <- #readRDS('./data_clean/tip_GOM.Rdata') %>%
  sttj_yt %>%
  filter( #MULT_TRIP == '0')%>%  ##no mult_trip in Caribbean TIP --> ALL full catch samples
    FISHING_MODE=='COMMERCIAL',
    LENGTH_TYPE1!='NO LENGTH',
    YEAR <= 2023
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
         ITIS_CODE   = OBS_STANDARD_SPECIES_CODE,
         SPECIES     = OBS_STANDARD_SPECIES_NAME,
         QUANTITY,
         YTS,
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

###add length-length conversions to helper_table ; 
tip3 <- merge(tip2, TIP_gears, by.x="LAND_STANDARD_GEAR_NAME",all.x=T)
    #gfin2a <- merge(gfin, flc, by.x=c("State_Landed","County_Landed"),all.x=T)

tip4 <- tip3[tip3$YTS==1,]

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

## FINAL dataset here  ####

length_data_final <- join_length_dat %>%
  select(YEAR, INTERVIEW_DATE, FINAL_DATE, ID, OBS_ID, STATE = STATE_LANDED, COUNTY=COUNTY_LANDED, COUNTY_CODE, FL_CM, lbin, source, gear, gear_short, LAND_STANDARD_GEAR_NAME, LAND_GEAR_NAME, mgt_period, ISLAND, INT_TYPE, fleet, DEALER_CODE, VESSEL_ID, LICENSE) %>%  #STAT_AREA
  filter(between(FL_CM , min_size, max_size),
         ISLAND != 'NOT CODED')
    #,
    #INT_TYPE != 'USVI MARFIN REEFFISH SAMPLING') 
    
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

library(ggplot2)
length_data_glm <- length_data_final |>
  select(YEAR, FINAL_DATE, ID, COUNTY, FL_CM, LAND_STANDARD_GEAR_NAME, gear) |> 
  # filter(gear == "Hook and Line") |> 
  mutate(ID = as.character(ID)) |> 
  select(-gear)

# plot data

# filtered to gears with 30 or more occurances for the purposes of plotting visibility
abc1 = allgears_glm_plot <- length_data_glm %>% group_by(LAND_STANDARD_GEAR_NAME) %>% filter(n() >= 30) %>% ungroup |> 
  ggplot(aes(x = as.Date(FINAL_DATE), y = FL_CM)) +
  geom_point(aes(colour = LAND_STANDARD_GEAR_NAME, shape = LAND_STANDARD_GEAR_NAME), size = 1, alpha = 0.5) +
  scale_shape_manual(values=c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 13, 14))+
  geom_smooth(method = "lm", formula = "y ~ x", col = "black") +
  # facet_wrap(~ COUNTY_LANDED) +
  labs(x = "", y = "Length (cm)", colour = "", shape = "") +
  theme_bw() +
  theme(legend.position = "bottom", legend.text = element_text(size = 10),
        legend.box.spacing = unit(0, "npc"), panel.grid = element_blank()) +
  guides(colour = guide_legend(override.aes = list(size = 2)))


# fit models
library(lmerTest)
# comparing length to date and gear in a gamma full model
mod2 = glmer(FL_CM ~ scale(FINAL_DATE) + LAND_STANDARD_GEAR_NAME + (1 | YEAR) + (1 | ID),
             data = length_data_glm, family = Gamma(link=log))


# pairwise comparisons (if needed)(needed for Hook and Line)
### 
mod_contr = emmeans::emmeans(object = mod2, pairwise ~ "LAND_STANDARD_GEAR_NAME", adjust = "tukey")

# cld provides gear groupings based on which gears are similar vs significantly different from each other 
### 
multcomp::cld(object = mod_contr$emmeans)

allgears_multcompcld <- multcomp::cld(object = mod_contr$emmeans)

length_data_fishcount <- length_data_final |> 
  group_by(LAND_STANDARD_GEAR_NAME) |>
  tally() 
length_data_tripcount <- aggregate(data = length_data_final,                # Applying aggregate
                                   ID ~ LAND_STANDARD_GEAR_NAME,
                                   function(ID) length(unique(ID)))

allgears_multcompcld_fish <- full_join(allgears_multcompcld, length_data_fishcount, by = "LAND_STANDARD_GEAR_NAME")
allgears_multcompcld_trip <- full_join(allgears_multcompcld_fish, length_data_tripcount, by = "LAND_STANDARD_GEAR_NAME")

allgears_multcompcld_final <- allgears_multcompcld_trip |> 
  dplyr::rename("group" = ".group")

tbl1 = flextable(allgears_multcompcld_final) |> autofit()



## All Gears 2012 and after ####

length_data_glm_2012 <- length_data_final |>
  filter(YEAR >= 2012) |> 
  select(YEAR, FINAL_DATE, ID, COUNTY, FL_CM, LAND_STANDARD_GEAR_NAME, gear) |> 
  # filter(gear == "Hook and Line") |> 
  mutate(ID = as.character(ID)) |> 
  select(-gear)

# plot data

abc2 = allgears_glm_plot <- length_data_glm_2012 |> 
  ggplot(aes(x = as.Date(FINAL_DATE), y = FL_CM)) +
  geom_point(aes(colour = LAND_STANDARD_GEAR_NAME, shape = LAND_STANDARD_GEAR_NAME), size = 1, alpha = 0.5) +
  geom_smooth(method = "lm", formula = "y ~ x", col = "black") +
  # facet_wrap(~ COUNTY_LANDED) +
  labs(x = "", y = "Length (cm)", colour = "", shape = "") +
  theme_bw() +
  theme(legend.position = "bottom", legend.text = element_text(size = 15),
        legend.box.spacing = unit(0, "npc"), panel.grid = element_blank()) +
  guides(colour = guide_legend(override.aes = list(size = 2)))



# fit models

# comparing length to date and gear in a gamma full model
mod2 = glmer(FL_CM ~ scale(FINAL_DATE) + LAND_STANDARD_GEAR_NAME + (1 | YEAR) + (1 | ID),
             data = length_data_glm_2012, family = Gamma(link=log))


# pairwise comparisons (if needed)(needed for Hook and Line)
### 
mod_contr = emmeans::emmeans(object = mod2, pairwise ~ "LAND_STANDARD_GEAR_NAME", adjust = "tukey")

allgears_multcompcld_2012 <- multcomp::cld(object = mod_contr$emmeans)

length_data_fishcount_12 <- length_data_glm_2012 |> 
  group_by(LAND_STANDARD_GEAR_NAME) |>
  summarise(n_fish = n()) 
length_data_tripcount_12 <- aggregate(data = length_data_glm_2012,                # Applying aggregate
                                      ID ~ LAND_STANDARD_GEAR_NAME,
                                      function(ID) length(unique(ID)))

allgears_multcompcld_fish_2012 <- full_join(allgears_multcompcld_2012, length_data_fishcount_12, by = "LAND_STANDARD_GEAR_NAME")
allgears_multcompcld_trip_2012 <- full_join(allgears_multcompcld_fish_2012, length_data_tripcount_12, by = "LAND_STANDARD_GEAR_NAME")

allgears_multcompcld_finaL_2012 <- allgears_multcompcld_trip_2012 |> 
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
  arrange(desc(Percentage)) 


tbl2 = flextable(allgears_multcompcld_finaL_2012) |> 
  theme_box() %>%
  align(align = "center", part = "all") %>%
  fontsize(size=8, part="all") %>%
  autofit() 


## Hook and Line ####

use_gear_hl <- length_data_final |>
  select(YEAR, FINAL_DATE, ID, COUNTY, FL_CM, LAND_STANDARD_GEAR_NAME, gear) |> 
  filter(gear == "Hook and Line") |> 
  mutate(ID = as.character(ID)) |> 
  select(-gear)

# plot data

abc3 = ggplot(use_gear_hl, aes(x = as.Date(FINAL_DATE), y = FL_CM)) +
  geom_point(aes(colour = LAND_STANDARD_GEAR_NAME, shape = LAND_STANDARD_GEAR_NAME), size = 1, alpha = 0.5) +
  geom_smooth(method = "lm", formula = "y ~ x", col = "black") +
  # facet_wrap(~ COUNTY_LANDED) +
  labs(x = "", y = "Length (cm)", colour = "", shape = "") +
  theme_bw() +
  theme(legend.position = "bottom", legend.text = element_text(size = 15),
        legend.box.spacing = unit(0, "npc"), panel.grid = element_blank()) +
  guides(colour = guide_legend(override.aes = list(size = 2)))


# fit models

# comparing length to date and gear in a gamma full model
mod2 = glmer(FL_CM ~ scale(FINAL_DATE) + LAND_STANDARD_GEAR_NAME + (1 | YEAR) + (1 | ID),
             data = use_gear_hl, family = Gamma(link=log))


# pairwise comparisons (if needed)(needed for Hook and Line)
mod_contr = emmeans::emmeans(object = mod2, pairwise ~ "LAND_STANDARD_GEAR_NAME", adjust = "tukey")

hl_multcompcld <- multcomp::cld(object = mod_contr$emmeans)

hl_fishcount <- use_gear_hl |> 
  group_by(LAND_STANDARD_GEAR_NAME) |>
  tally()

hl_tripcount <- aggregate(data = use_gear_hl,                # Applying aggregate
                          ID ~ LAND_STANDARD_GEAR_NAME,
                          function(ID) length(unique(ID)))

hl_multcompcld_fish <- full_join(hl_multcompcld, hl_fishcount, by = "LAND_STANDARD_GEAR_NAME")
hl_multcompcld_trip <- full_join(hl_multcompcld_fish, hl_tripcount, by = "LAND_STANDARD_GEAR_NAME")

hl_multcompcld_final <- hl_multcompcld_trip |> 
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
  arrange(desc(Percentage))


tbl3 = flextable(hl_multcompcld_final) |> 
  theme_box() %>%
  align(align = "center", part = "all") %>%
  fontsize(size=8, part="all") %>%
  autofit() 

## Net ####

# net land_standard_gear_name
use_gear_nlsg <- length_data_final |>
  select(YEAR, FINAL_DATE, ID, COUNTY, FL_CM, LAND_STANDARD_GEAR_NAME, gear) |> 
  filter(gear == "Net") |> 
  mutate(ID = as.character(ID)) |> 
  select(-gear)


# plot data

abc4 = ggplot(use_gear_nlsg, aes(x = as.Date(FINAL_DATE), y = FL_CM)) +
  geom_point(aes(colour = LAND_STANDARD_GEAR_NAME, shape = LAND_STANDARD_GEAR_NAME), size = 1, alpha = 0.5) +
  geom_smooth(method = "lm", formula = "y ~ x", col = "black") +
  # facet_wrap(~ COUNTY_LANDED) +
  labs(x = "", y = "Length (cm)", colour = "", shape = "") +
  theme_bw() +
  theme(legend.position = "bottom", legend.text = element_text(size = 15),
        legend.box.spacing = unit(0, "npc"), panel.grid = element_blank()) +
  guides(colour = guide_legend(override.aes = list(size = 2)))

 
# fit models

# comparing length to date and gear in a gamma full model
mod2 = glmer(FL_CM ~ scale(FINAL_DATE) + LAND_STANDARD_GEAR_NAME + (1 | YEAR) + (1 | ID),
             data = use_gear_nlsg, family = Gamma(link=log)) 


### no significant p values for time period or gears, further comparisons not needed 
 
mod_contr = emmeans::emmeans(object = mod2, pairwise ~ "LAND_STANDARD_GEAR_NAME", adjust = "tukey")

nt_multcompcld <- multcomp::cld(object = mod_contr$emmeans)

nt_fishcount <- use_gear_nlsg |> 
  group_by(LAND_STANDARD_GEAR_NAME) |>
  tally()

nt_tripcount <- aggregate(data = use_gear_nlsg,                # Applying aggregate
                          ID ~ LAND_STANDARD_GEAR_NAME,
                          function(ID) length(unique(ID)))

nt_multcompcld_fish <- full_join(nt_multcompcld, nt_fishcount, by = "LAND_STANDARD_GEAR_NAME")
nt_multcompcld_trip <- full_join(nt_multcompcld_fish, nt_tripcount, by = "LAND_STANDARD_GEAR_NAME")

nt_multcompcld_final <- nt_multcompcld_trip |> 
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
  arrange(desc(Percentage))

tbl4 = flextable(nt_multcompcld_final) |> 
  theme_box() %>%
  align(align = "center", part = "all") %>%
  fontsize(size=8, part="all") %>%
  autofit()

## Trap ####

use_gear_tr <- length_data_final |>
  select(YEAR, FINAL_DATE, ID, COUNTY, FL_CM, LAND_STANDARD_GEAR_NAME, gear) |> 
  filter(gear == "Trap") |> 
  mutate(ID = as.character(ID)) |> 
  select(-gear)


# plot data

abc5 = ggplot(use_gear_tr, aes(x = as.Date(FINAL_DATE), y = FL_CM)) +
  geom_point(aes(colour = LAND_STANDARD_GEAR_NAME, shape = LAND_STANDARD_GEAR_NAME), size = 1, alpha = 0.5) +
  geom_smooth(method = "lm", formula = "y ~ x", col = "black") +
  # facet_wrap(~ COUNTY_LANDED) +
  labs(x = "", y = "Length (cm)", colour = "", shape = "") +
  theme_bw() +
  theme(legend.position = "bottom", legend.text = element_text(size = 15),
        legend.box.spacing = unit(0, "npc"), panel.grid = element_blank()) +
  guides(colour = guide_legend(override.aes = list(size = 2)))


# fit models

# comparing length to date and gear in a gamma full model
mod2 = glmer(FL_CM ~ scale(FINAL_DATE) + LAND_STANDARD_GEAR_NAME + (1 | YEAR) + (1 | ID),
             data = use_gear_tr, family = Gamma(link=log))


# No significant p values for time period or gears, further comparisons not needed 
 
# pairwise comparisons (if needed)(needed for Hook and Line)
mod_contr = emmeans::emmeans(object = mod2, pairwise ~ "LAND_STANDARD_GEAR_NAME", adjust = "tukey")

trap_multcompcld <- multcomp::cld(object = mod_contr$emmeans)

use_gear_tr_fishcount <- use_gear_tr |> 
  group_by(LAND_STANDARD_GEAR_NAME) |> 
  tally()
use_gear_tr_tripcount <- aggregate(data = use_gear_tr,                # Applying aggregate
                                   ID ~ LAND_STANDARD_GEAR_NAME,
                                   function(ID) length(unique(ID)))

trap_multcompcld_fish <- full_join(trap_multcompcld, use_gear_tr_fishcount, by = "LAND_STANDARD_GEAR_NAME")
trap_multcompcld_trip <- full_join(trap_multcompcld_fish, use_gear_tr_tripcount, by = "LAND_STANDARD_GEAR_NAME")

trap_multcompcld_final <- trap_multcompcld_trip  |> 
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
  arrange(desc(Percentage))

tbl5 = flextable(trap_multcompcld_final) |> 
  theme_box() %>%
  align(align = "center", part = "all") %>%
  fontsize(size=8, part="all") %>%
  autofit()

# Gear Density Plots ####

# Filtered to years with 30 or more length records (regardless of gear) per year. STT and STJ records are grouped together.

## Aggregated density plots ####

ycounts =length_data_final %>% group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
  tabyl(gear) %>%
  mutate(n_labels = paste0(gear, " (n= ", n, ")" ))

library(plyr)
mu <- ddply(length_data_final, "gear", summarise, grp.mean=mean(FL_CM))
head(mu)

agr_den_allgears <- length_data_final %>% group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
  ggplot(aes(FL_CM))+
  # geom_density( aes(color = "Combined"),lwd=1.5)+
  geom_density(aes(color = gear), size = 0.75)+
  scale_color_hue(labels=ycounts$n_labels)+
  # scale_color_hue(labels=c("Combined",ycounts$n_labels))+
  #scale_color_manual(values = gearcols, labels = c("Combined", counts$n_labels))+
  labs(color = "Gear Type", x = "Fork Length (cm)", title = paste0(region,  "\n (N = ", sum(ycounts$n), ")"))+
  # theme_minimal()
  theme(legend.title = element_text(size=14), 
        legend.text = element_text(size=12))+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=gear),
             linetype="dashed")


abc6 = agr_den_allgears


## Gear grouping investigation ####

# Filtered to minimum 30 fish per year

 
### hook and line ####

hl<- length_data_final[length_data_final$gear_short=='HL',]
counts = hl %>% group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
  tabyl(LAND_STANDARD_GEAR_NAME) %>%
  mutate(n_labels = paste0(LAND_STANDARD_GEAR_NAME, " (n= ", n, ")" ))

hl2 <- hl %>% group_by(LAND_STANDARD_GEAR_NAME) %>% filter(n() >= 2) %>% ungroup
muhl <- ddply(hl2, "LAND_STANDARD_GEAR_NAME", summarise, grp.mean=mean(FL_CM))
head(muhl)

agr_den_hl <-hl %>% group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
  ggplot(aes(FL_CM, color = LAND_STANDARD_GEAR_NAME))+
  geom_density(size = 0.75)+
  # scale_color_manual( values = gearcols, labels = counts$n_labels)+
  scale_color_hue(labels = counts$n_labels)+
  labs(color = "Gear Type", x = "Fork Length (cm)", title = paste0(county,  "\n (N = ", sum(counts$n), ")"))+
  # facet_wrap(~ISLAND,ncol=1) +
  # theme_minimal()
  theme(legend.title = element_text(size=14), 
        legend.text = element_text(size=12))+
  geom_vline(data=muhl, aes(xintercept=grp.mean, color=LAND_STANDARD_GEAR_NAME),
             linetype="dashed")

abc8 = agr_den_hl

 
### trap ####

trap <- length_data_final[length_data_final$gear_short=='TR',]
counts = trap %>% group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
  tabyl(LAND_STANDARD_GEAR_NAME) %>%
  mutate(n_labels = paste0(LAND_STANDARD_GEAR_NAME, " (n= ", n, ")" ))

mutrap <- ddply(trap, "LAND_STANDARD_GEAR_NAME", summarise, grp.mean=mean(FL_CM))
head(mutrap)

agr_den_trap <- trap %>% group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
  ggplot(aes(FL_CM, color = LAND_STANDARD_GEAR_NAME))+
  geom_density(size = 0.75)+
  # scale_color_manual( values = gearcols, labels = counts$n_labels)+
  scale_color_hue(labels = counts$n_labels)+
  labs(color = "Gear Type", x = "Fork Length (cm)", title = paste0(county,  "\n (N = ", sum(counts$n), ")"))+
  # facet_wrap(~ISLAND,ncol=1) +
  # theme_minimal()
  theme(legend.title = element_text(size=14), 
        legend.text = element_text(size=12))+
  geom_vline(data=mutrap, aes(xintercept=grp.mean, color=LAND_STANDARD_GEAR_NAME),
             linetype="dashed")
abc9 = agr_den_trap

 
### net ####

#nets might need to be another category because haul seines, trammel, and entangling (gill unsp) have too many occurances to be in other 

net <- length_data_final[length_data_final$gear_short=='NT',]
counts = net %>% group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
  tabyl(LAND_STANDARD_GEAR_NAME) %>%
  mutate(n_labels = paste0(LAND_STANDARD_GEAR_NAME, " (n= ", n, ")" ))

munet <- ddply(net, "LAND_GEAR_NAME", summarise, grp.mean=mean(FL_CM))
head(munet)

agr_den_net <- net %>% group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
  ggplot(aes(FL_CM,color = LAND_STANDARD_GEAR_NAME))+
  geom_density(size = 0.75)+
  # scale_color_manual( values = gearcols, labels = counts$n_labels)+
  scale_color_hue(labels = counts$n_labels)+
  labs(color = "Gear Type", x = "Fork Length (cm)", title = paste0(county,  "\n (N = ", sum(counts$n), ")"))+
  # facet_wrap(~ISLAND,ncol=1) +
  # theme_minimal()
  theme(legend.title = element_text(size=14), 
        legend.text = element_text(size=12))+
  geom_vline(data=munet, aes(xintercept=grp.mean, color=LAND_GEAR_NAME),
             linetype="dashed")
abc11 = agr_den_net


 
### other ####

ot<- length_data_final[length_data_final$gear_short=='OT',]
counts = ot %>% group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
  tabyl(LAND_STANDARD_GEAR_NAME) %>%
  mutate(n_labels = paste0(LAND_STANDARD_GEAR_NAME, " (n= ", n, ")" ))

ot2 <- ot %>% group_by(LAND_STANDARD_GEAR_NAME) %>% filter(n() >= 2) %>% ungroup
muot <- ddply(ot2, "LAND_STANDARD_GEAR_NAME", summarise, grp.mean=mean(FL_CM))
head(muot)

agr_den_ot <- ot %>% group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
  ggplot(aes(FL_CM, color = LAND_STANDARD_GEAR_NAME))+
  geom_density(size = 0.75)+
  # scale_color_manual( values = gearcols, labels = counts$n_labels)+
  scale_color_hue(labels = counts$n_labels)+
  labs(color = "Gear Type", x = "Fork Length (cm)", title = paste0(county,  "\n (N = ", sum(counts$n), ")"))+
  # facet_wrap(~ISLAND,ncol=1) +
  # theme_minimal()
  theme(legend.title = element_text(size=14), 
        legend.text = element_text(size=12))+
  geom_vline(data=muot, aes(xintercept=grp.mean, color=LAND_STANDARD_GEAR_NAME),
             linetype="dashed")
abc12 = agr_den_ot

 

## Break at 2005 ####

length_data_final %>%  group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>% # filter(YEAR > 2011) |> 
  ggplot(aes(FL_CM, color = gear))+
  geom_density(size = 0.75)+
  scale_color_hue(labels=ycounts$n_labels)+
  #  scale_color_manual(values = gearcols, labels = counts$n_labels)+
  labs(color = "Gear Type", x = "Fork Length (cm)", title = paste0(county,  "\n (N = ", sum(ycounts$n), ")"))+
  # facet_wrap(~ISLAND,ncol=1) +
  facet_wrap(~mgt_period) +
  # theme_minimal()
  theme(legend.title = element_text(size=14), 
        legend.text = element_text(size=12))


 
### hook and line 

HL <- length_data_final[length_data_final$gear_short=='HL',]
counts = HL %>% group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
  tabyl(LAND_STANDARD_GEAR_NAME) %>%
  mutate(n_labels = paste0(LAND_STANDARD_GEAR_NAME, " (n= ", n, ")" ))

HL %>%  group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
  ggplot(aes(FL_CM, color = LAND_STANDARD_GEAR_NAME))+
  geom_density(size = 0.75)+
  # scale_color_manual( values = gearcols, labels = counts$n_labels)+
  scale_color_hue(labels = counts$n_labels)+
  labs(color = "Gear Type", x = "Fork Length (cm)", title = paste0(county,  "\n (N = ", sum(counts$n), ")"))+
  # facet_wrap(~ISLAND,ncol=1) +
  facet_wrap(~mgt_period) +
  # theme_minimal()
  theme(legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

 
### net 

#nets might need to be another category because haul seines, trammel, and entangling (gill unsp) have too many occurances to be in other 

net <- length_data_final[length_data_final$gear_short=='NT',]
counts = net %>% group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
  tabyl(LAND_STANDARD_GEAR_NAME) %>%
  mutate(n_labels = paste0(LAND_STANDARD_GEAR_NAME, " (n= ", n, ")" ))

net %>%  group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
  ggplot(aes(FL_CM,color = LAND_STANDARD_GEAR_NAME))+
  geom_density(size = 0.75)+
  # scale_color_manual( values = gearcols, labels = counts$n_labels)+
  scale_color_hue(labels = counts$n_labels)+
  labs(color = "Gear Type", x = "Fork Length (cm)", title = paste0(county,  "\n (N = ", sum(counts$n), ")"))+
  # facet_wrap(~ISLAND,ncol=1) +
  facet_wrap(~mgt_period) +
  # theme_minimal()
  theme(legend.title = element_text(size=14), 
        legend.text = element_text(size=12))


### trap 

trap <- length_data_final[length_data_final$gear_short=='TR',]
counts = trap %>% group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
  tabyl(LAND_STANDARD_GEAR_NAME) %>%
  mutate(n_labels = paste0(LAND_STANDARD_GEAR_NAME, " (n= ", n, ")" ))

trap %>%  group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
  ggplot(aes(FL_CM,color = LAND_STANDARD_GEAR_NAME))+
  geom_density(size = 0.75)+
  # scale_color_manual( values = gearcols, labels = counts$n_labels)+
  scale_color_hue(labels = counts$n_labels)+
  labs(color = "Gear Type", x = "Fork Length (cm)", title = paste0(county,  "\n (N = ", sum(counts$n), ")"))+
  # facet_wrap(~ISLAND,ncol=1) +
  facet_wrap(~mgt_period) +
  # theme_minimal()
  theme(legend.title = element_text(size=14), 
        legend.text = element_text(size=12))


 
### other  

ot<- length_data_final[length_data_final$gear_short=='OT',]
counts = ot %>% group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
  tabyl(LAND_STANDARD_GEAR_NAME) %>%
  mutate(n_labels = paste0(LAND_STANDARD_GEAR_NAME, " (n= ", n, ")" ))

ot %>%  group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
  ggplot(aes(FL_CM, color = LAND_STANDARD_GEAR_NAME))+
  geom_density(size = 0.75)+
  # scale_color_manual( values = gearcols, labels = counts$n_labels)+
  scale_color_hue(labels = counts$n_labels)+
  labs(color = "Gear Type", x = "Fork Length (cm)", title = paste0(county,  "\n (N = ", sum(counts$n), ")"))+
  # facet_wrap(~ISLAND,ncol=1) +
  facet_wrap(~mgt_period) +
  # theme_minimal()
  theme(legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

 