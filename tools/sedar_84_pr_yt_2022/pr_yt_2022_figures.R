
# This script will be expanding upon the length-comp script c1_nominal_len_comps.Rmd created by Molly Stevens.

# Yellowtail Snapper PR

# Set up library 
librarian::shelf(here, tidyverse,  dotenv, reshape, openxlsx, janitor, DT, 
                 pander, knitr, flextable, ggplot2, lmerTest, meantables)

# if conflicts in pkgs arise, use the following:
# library(conflicted)
# conflicted::conflicts_prefer(here::here)


#pull data by island
# cr_tip(state_codes = c('PR', 'VI'))

# Find out the date of the most recent extraction
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
# cr_tip_sp(state_codes = c('PR', 'VI'), sp_codes = "168907")
# tip <- readRDS("~/SEFSC-SFD-CFB-TIP-Compositions/data/raw/com_tip_PR_VI_168907_20240111.RDS")

tip <- readRDS(here::here("data", "raw", "com_tip_PR_VI_168907_20240111.RDS"))

# Filter to PR and Yellowtail Snapper AND create new filterable date value 
pr_yt <- tip |> 
  filter(STATE_LANDED == "PUERTO RICO",
         OBS_STANDARD_SPECIES_CODE == "168907") |> 
  mutate(TEST_DATE = as.Date(ymd_hms(INTERVIEW_DATE)),
         FINAL_DATE = case_when(is.na(TEST_DATE) ~ INTERVIEW_DATE, 
                                TRUE ~ TEST_DATE))


source("~/SEFSC-SFD-CFB-TIP-Compositions/tools/functions/len_len_convert.R")
source("~/SEFSC-SFD-CFB-TIP-Compositions/tools/functions/fig_format_export.R")

LLconv <- read_csv("~/SEFSC-SFD-CFB-TIP-Compositions/tools/CSVs/LLconversions.csv",
                   show_col_types = FALSE)

TIP_gears <- read_csv("~/SEFSC-SFD-CFB-TIP-Compositions/tools/CSVs/tip_gears_yts_pr_landstdgearname2.csv",
                      show_col_types = FALSE)

gearcols <- c("Net" = "#FF0000", "Trap" = "#00A08A", "Other" = "#5D057D", "Hook and Line" ="#046C9A", "Gill Net" = "#00A08A" , "Haul Seine" = "#00A08A" , "Lobster Trap" = "#00A08A" , "Trammel Net" = "#00A08A" , "Combined" = "black")

## Control Settings and Exploratory Frequency Tables

sp <- "YTS"

region <- "PUERTO RICO - USVI"

county <- "PUERTO RICO"

bin_size <- 1

len_type <- "FORK LENGTH"

disclaimer <- "Gears with less than 3 unique interviews were removed."

flextable(as.data.frame(table(pr_yt$LENGTH_TYPE1, useNA='always')))%>%
  autofit()

flextable(as.data.frame(table(pr_yt$LAND_STANDARD_GEAR_NAME, useNA='always')))%>%
  autofit()

# len_types <- as.data.frame(table(stx_slp$LENGTH_TYPE1, useNA='always'))
# 
# total_len_count <- len_types |> 
#   mutate(Percent = round((Freq/total_length*100), 2))
# non_fork =

n_fork_len = sum(pr_yt$LENGTH_TYPE1 == "FORK LENGTH")
n_all_len = length(pr_yt$LENGTH_TYPE1)
p_fork_len = round(n_fork_len/n_all_len, 3)*100

trip_id_unique <- as.data.frame(table(pr_yt$ID, useNA='always'))
total_trip_id_unique = nrow(trip_id_unique)
# QUICK LOOK INTO SEX
 # colnames(stx_slp)
 # unique(stx_slp$SEX_NAME)

 pr_sex <- as.data.frame(table(pr_yt$SEX_NAME, useNA='always'))
 
##range currently set to not drop any obs (centimeters)

min_size <- 5 

max_size <- 122   

min(pr_yt$YEAR,na.rm = TRUE)

min_year <- 1983

max_year <- 2022 

break_year <- 2005 # this is an optional value to denote a change in management. inclusive on the upper bound. can be set to NA if not relevant.  2005 trap specifications were updated and many closures went into place


#### Filter to Commercial samples

################## PICK VARIABLES THAT I NEED HERE ;
################## ASSIGN GEARS, ETC. BASED ON FLOW CHART
## FILTER OUT COMMERCIAL SAMPLES

tip2 <- #readRDS('./data_clean/tip_GOM.Rdata') %>%
  pr_yt %>%
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

###add length-length conversions to helper_table ; 
tip3 <- merge(tip2, TIP_gears, by.x="LAND_STANDARD_GEAR_NAME",all.x=T)
#gfin2a <- merge(gfin, flc, by.x=c("State_Landed","County_Landed"),all.x=T)

tip4 <- tip3[tip3$YTS==1,]

table(tip3$LAND_STANDARD_GEAR_NAME, tip3$gear, useNA='always')

#gear_groups <- c("HAND LINE", "LONGLINE") 
gear_groups <- unique(TIP_gears$gear)

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

# FINAL dataset here ####

length_data_final <- join_length_dat %>%
  select(YEAR, INTERVIEW_DATE, FINAL_DATE, ID, OBS_ID, STATE = STATE_LANDED, COUNTY=COUNTY_LANDED, COUNTY_CODE, FL_CM, lbin, source, gear, gear_short, LAND_STANDARD_GEAR_NAME, LAND_GEAR_NAME, mgt_period, ISLAND, INT_TYPE, fleet, DEALER_CODE, VESSEL_ID, LICENSE) %>%  #STAT_AREA
  filter(between(FL_CM , min_size, max_size),
         ISLAND != 'NOT CODED')|> 
  mutate(GEAR_GROUP = case_when(LAND_STANDARD_GEAR_NAME == "LINES HAND" ~ "HAND LINE",
                                LAND_STANDARD_GEAR_NAME == "LINES LONG SET WITH HOOKS" ~ "HAND LINE",
                                LAND_STANDARD_GEAR_NAME == "LINES POWER TROLL OTHER" ~ "HAND LINE",
                                LAND_STANDARD_GEAR_NAME == "ROD AND REEL" ~ "HAND LINE",
                                LAND_STANDARD_GEAR_NAME == "LINES LONG; REEF FISH" ~ "HAND LINE",
                                LAND_STANDARD_GEAR_NAME == "HAUL SEINES"~ "HAUL SEINE",
                                LAND_STANDARD_GEAR_NAME == "POTS AND TRAPS;SPINY LOBSTER" ~ "TRAPS",
                                LAND_STANDARD_GEAR_NAME == 'POTS AND TRAPS; FISH'~ "TRAPS",
                                TRUE ~ "OTHER"))

# Analyst have asked for a record of dropped observations
######### ADAPT THIS TO OUTPUT ALL DROPPED RECORDS ABOVE--MOVE HIGHER IN THE SCRIPT TO HAVE ALL VARS
dropped_obs <- anti_join(join_length_dat, length_data_final, by = "OBS_ID")  



#-----------------------------------------#DOCUMENT ALL DROPPED DATA

# drop <- list()  ##++this creates an empty list for for loop
# 
# # for loop generates binned length comps for each of the final gears. Stored in list. RUN:  View(comps[[1]]) to see in R
# 
#  drop[[1]] <-  NON-RANDOM SAMPLES
#  drop[[2]] <-  SIZE DATA
#   filter(gear == final_gears[i]) %>%
# names(comps)[[1]] <- paste0(final_gears[i], "_", bin_size, "cm") ##++tabs are named here ; could add _nom here if desired (instead of _lfd and _lfdw in main file)
#  
#-----------------------------------------#

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

unique(length_data_final$gear) # "Hook and Line", "Trap", "Other", "Net"
unique(length_data_final$GEAR_GROUP) # "OTHER"      "HAUL SEINE" "HAND LINE"  "TRAPS"  

n_not_coded = sum(pr_yt$LAND_STANDARD_GEAR_NAME == "NOT CODED")
n_all_len = length(pr_yt$LENGTH_TYPE1)
p_not_coded = round(n_not_coded/n_all_len, 4)*100

length_data_final_pr <- length_data_final |> 
  filter(LAND_STANDARD_GEAR_NAME != "NOT CODED")

unique(length_data_final_pr$LAND_STANDARD_GEAR_NAME)

## All Gears ####

length_data_glm <- length_data_final |>
  select(YEAR, FINAL_DATE, ID, COUNTY, FL_CM, LAND_STANDARD_GEAR_NAME, gear, GEAR_GROUP) |> 
  # filter(gear == "Hook and Line") |> 
  mutate(ID = as.character(ID)) |> 
  select(-gear)

# plot data

# abc1 = length_data_glm |> 
#   ggplot(length_data_glm, aes(x = as.Date(FINAL_DATE), y = FL_CM)) +
#   geom_point(aes(colour = LAND_STANDARD_GEAR_NAME), size = 1, alpha = 0.5) +
#   scale_shape_manual(values=c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 13, 14))+
#   geom_smooth(method = "lm", formula = "y ~ x", col = "black") +
#   # facet_wrap(~ COUNTY_LANDED) +
#   labs(x = "", y = "Length (cm)", colour = "", shape = "") +
#   theme_bw() +
#   theme(legend.position = "bottom", legend.text = element_text(size = 15),
#         legend.box.spacing = unit(0, "npc"), panel.grid = element_blank()) +
#   guides(colour = guide_legend(override.aes = list(size = 2)))

# gant_data <- length_data_final %>% group_by(ID) %>% filter(n() >= 3) %>% ungroup %>%
#   # filter(ID >= 3) |>
#   group_by(YEAR, LAND_STANDARD_GEAR_NAME) |>
#   summarize(n = n(), .groups = "drop") 

gant_data <- length_data_final %>% 
  group_by(LAND_STANDARD_GEAR_NAME) %>% 
  dplyr::mutate(n_ID = n_distinct(ID)) |> 
  dplyr::filter(n_ID >= 3) %>% ungroup %>%
  group_by(YEAR, LAND_STANDARD_GEAR_NAME) |>
  dplyr::summarize(n = n(), .groups = "drop") |> 
  mutate(YEAR = as.integer(YEAR))

abc1 <- gant_data |>
  group_by(LAND_STANDARD_GEAR_NAME) |>
  dplyr::mutate(total_n = sum(n)) |> 
  ungroup() |>   
  dplyr::mutate(LAND_STANDARD_GEAR_NAME = fct_reorder(LAND_STANDARD_GEAR_NAME, total_n)) %>%
  ggplot(aes(x = YEAR, y = LAND_STANDARD_GEAR_NAME, color = LAND_STANDARD_GEAR_NAME, size = n)) +
  geom_point()  +
  labs(x = "Year", y = "", colour = "", shape = "", 
       title = paste(county, "Length Samples"),
       caption = disclaimer) +
  theme_bw() + 
  theme(legend.position="null", text = element_text(size = 20),
        title = element_text(size = 15))
  
gant_data_id <- length_data_final %>% 
  group_by(LAND_STANDARD_GEAR_NAME) %>% 
  dplyr::mutate(n_ID = n_distinct(ID)) |> 
  dplyr::filter(n_ID >= 3) %>% ungroup %>%
  group_by(YEAR, LAND_STANDARD_GEAR_NAME) |>
  dplyr::summarize(n_ID = n_distinct(ID), .groups = "drop") |> 
  mutate(YEAR = as.integer(YEAR))

abc21 <- gant_data_id |>
  # filter(YEAR > 2011) |> 
  group_by(LAND_STANDARD_GEAR_NAME) |>
  dplyr::mutate(total_n = sum(n_ID)) |> 
  ungroup() |>   
  dplyr::mutate(LAND_STANDARD_GEAR_NAME = fct_reorder(LAND_STANDARD_GEAR_NAME, total_n)) %>%
  ggplot(aes(x = YEAR, y = LAND_STANDARD_GEAR_NAME, color = LAND_STANDARD_GEAR_NAME, size = n_ID)) +
  geom_point()  +
  labs(x = "Year", y = "", colour = "", shape = "", 
       title = paste(county, "Interviews"),
       caption = disclaimer) +
  theme_bw() + 
  theme(legend.position="null", text = element_text(size = 20), 
        title = element_text(size = 15))

# fit models

# comparing length to date and gear in a gamma full model
mod2 = glmer(FL_CM ~ scale(FINAL_DATE) + LAND_STANDARD_GEAR_NAME + (1 | YEAR) + (1 | ID),
             data = length_data_glm, family = Gamma(link=log))


# pairwise comparisons (if needed)(needed for Hook and Line)
### 
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
                "Estimated Marginal Mean" = "emmean",
                "LCL" = "asymp.LCL",
                "UCL" = "asymp.UCL",
                "Fish(n)" = "n",
                "Interview(n)" = "ID") |> 
  # filter("Interview(n)" >= 3) |> 
  dplyr::filter(`Interview(n)` >= 3)|> 
  select(Gear, "Estimated Marginal Mean", LCL, UCL,  Group, "Fish(n)","Interview(n)", Percentage ) |>  
  mutate("Gear Group" = case_when(Gear == "LINES HAND" ~ "Hand Line",
                                  Gear == "HAUL SEINES" ~ "Haul Seine",
                                  Gear == 'POTS AND TRAPS; FISH'~ "Haul Seine or Trap",
                                  Gear == "NOT CODED" ~ "Net",
                                  Gear == "ENTANGLING NETS (GILL) UNSPC"~ "Trap or Net",
                                  Gear == "TRAMMEL NETS" ~ "Net",
                                  Gear == "LINES LONG SET WITH HOOKS" ~ "Net or Hand Line",
                                  Gear == "BY HAND; DIVING GEAR" ~ "Net or Hand Line",
                                  Gear == "LINES POWER TROLL OTHER" ~ "Net or Hand Line",
                                  Gear == "ROD AND REEL" ~ "Net or Hand Line",
                                  TRUE ~ "Haul Seine, Trap, Net, or Hand Line")) 

# create table of means for each gear
mean_allgears <- length_data_glm %>%
  group_by(LAND_STANDARD_GEAR_NAME) %>%
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


# look at table without "NOT CODED"

      allgears_multcompcld_final_NC <- allgears_multcompcld_trip |> 
        filter(LAND_STANDARD_GEAR_NAME != "NOT CODED") |> 
        mutate(Percentage = round(n/sum(n)*100, 2), 
               emmean = round(emmean, 2),
               asymp.LCL = round(asymp.LCL, 2),
               asymp.UCL = round(asymp.UCL, 2))|>
        dplyr::rename("Group" = ".group",
                      "Gear" = "LAND_STANDARD_GEAR_NAME",
                      "Estimated Marginal Mean" = "emmean",
                      "LCL" = "asymp.LCL",
                      "UCL" = "asymp.UCL",
                      "Fish(n)" = "n",
                      "Interview(n)" = "ID") |> 
        # filter("Interview(n)" >= 3) |> 
        dplyr::filter(`Interview(n)` >= 3)|> 
        select(Gear, "Estimated Marginal Mean", LCL, UCL,  Group, "Fish(n)","Interview(n)", Percentage ) |>  
        mutate("Gear Group" = case_when(Gear == "LINES HAND" ~ "Hand Line",
                                        Gear == "HAUL SEINES" ~ "Haul Seine",
                                        Gear == 'POTS AND TRAPS; FISH'~ "Haul Seine or Trap",
                                        Gear == "NOT CODED" ~ "Net",
                                        Gear == "ENTANGLING NETS (GILL) UNSPC"~ "Trap or Net",
                                        Gear == "TRAMMEL NETS" ~ "Net",
                                        Gear == "LINES LONG SET WITH HOOKS" ~ "Net or Hand Line",
                                        Gear == "BY HAND; DIVING GEAR" ~ "Net or Hand Line",
                                        Gear == "LINES POWER TROLL OTHER" ~ "Net or Hand Line",
                                        Gear == "ROD AND REEL" ~ "Net or Hand Line",
                                        TRUE ~ "Haul Seine, Trap, Net, or Hand Line")) 
      
      # create table of means for each gear
      mean_allgears_NC <- length_data_glm %>%
        filter(LAND_STANDARD_GEAR_NAME != "NOT CODED") |> 
        group_by(LAND_STANDARD_GEAR_NAME) %>%
        dplyr::mutate(n_ID = n_distinct(ID)) |> 
        dplyr::filter(n_ID >= 3) |>
        mean_table(FL_CM) |> 
        dplyr::rename("Gear" = "group_cat",
                      "Mean" = "mean") |> 
        select(Gear, Mean)
      
      allgears_multicom_mean_NC <- full_join(mean_allgears_NC, allgears_multcompcld_final_NC, by = "Gear")
      
      allgears_multicom_mean_final_NC <- allgears_multicom_mean_NC |> 
        arrange(desc(Percentage))#|> 
      # dplyr::filter(`Interview(n)` >= 3)
      
      tbl2 = flextable(allgears_multicom_mean_final_NC) |> 
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


# Gear Density Plots ####

## Aggregated density plots ####

### overlay time periods ####

length_data_1983_2022 <- length_data_final %>% 
  group_by(LAND_STANDARD_GEAR_NAME) %>% 
  dplyr::mutate(n_ID = n_distinct(ID)) |> 
  dplyr::filter(n_ID >= 3) %>% #ungroup %>%
  # group_by(YEAR) %>% 
  # filter(n() >= 30) %>% 
  ungroup() 

mean(length_data_1983_2022$FL_CM)

# length_data_2012_2022 <- length_data_final |>
#   filter(YEAR >= 2012) |>
#   group_by(LAND_STANDARD_GEAR_NAME) %>% 
#   dplyr::mutate(n_ID = n_distinct(ID)) |> 
#   dplyr::filter(n_ID >= 3) %>% #ungroup %>%
#   # group_by(YEAR) %>% 
#   # filter(n() >= 30) %>% 
#   ungroup()

agr_den_NOgears <- 
  ggplot() +
  geom_density(aes(FL_CM, color = "length_data_1983_2022"),linewidth = 1.0, alpha = .2, data = length_data_1983_2022) +
  # geom_density(aes(FL_CM, color = "length_data_2012_2022"),linewidth = 0.75, alpha = .2, data = length_data_2012_2022) +
  geom_vline(data = length_data_1983_2022, aes(xintercept=mean(FL_CM), color = "length_data_1983_2022"),
             linetype="dashed", linewidth=1) +
  # geom_vline(data=length_data_2012_2022, aes(xintercept=mean(FL_CM), color = "length_data_2012_2022"),
             # linetype="dashed", linewidth=1) +
  labs(x = "Fork Length (cm)", title = county)+
  # scale_fill_discrete(name = "Time Series", labels = c("1983-2022", "2012-2022"))
  guides(color=guide_legend(title="Time Series"))+
  scale_color_discrete(labels='1983-2022')+
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
  group_by(LAND_STANDARD_GEAR_NAME) %>% 
  dplyr::mutate(n_ID = n_distinct(ID)) |> 
  dplyr::filter(n_ID >= 3) %>% #ungroup %>%
  # group_by(YEAR) %>% 
  # filter(n() >= 30) %>% 
  ungroup() |> 
  filter(LAND_STANDARD_GEAR_NAME %in% c("LINES HAND", "POTS AND TRAPS; FISH",
                                        "HAUL SEINES", "NOT CODED")) 

ycounts =length_data_gears %>% #group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
  tabyl("LAND_STANDARD_GEAR_NAME") %>%
  mutate(n_labels = paste0(LAND_STANDARD_GEAR_NAME, " (n= ", n, ")" ))

muv <- plyr::ddply(length_data_gears, "LAND_STANDARD_GEAR_NAME", summarise, grp.mean=mean(FL_CM))
head(muv)

agr_den_v <- length_data_gears %>% 
  # group_by(LAND_STANDARD_GEAR_NAME) %>% 
  # dplyr::mutate(n_ID = n_distinct(ID)) |> 
  # dplyr::filter(n_ID >= 3) %>% ungroup %>%
  # group_by(YEAR) %>% 
  # filter(n() >= 30) %>% ungroup %>%
  ggplot(aes(FL_CM))+
  # geom_density( aes(color = "Combined"),lwd=1.5)+
  geom_density(aes(color = LAND_STANDARD_GEAR_NAME),linewidth = 0.75)+
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
  geom_vline(data=muv, aes(xintercept=grp.mean, color=LAND_STANDARD_GEAR_NAME),
             linetype="dashed")


abc15 = agr_den_v

## Annual Density plots ####
### ALL GEARS ####
fleet_final <- length_data_1983_2022[length_data_1983_2022$fleet==1,]

fcounts = fleet_final %>%  group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
  tabyl(gear) %>%
  mutate(n_labels = paste0(gear, " (n= ", n, ")" ))

all_carPR <-
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
  facet_wrap(~year_labs, ncol = 10)
# theme_minimal()
# theme(legend.title = element_text(linewidth=14), 
# legend.text = element_text(linewidth=12))+

export_fig_page(all_carPR)
abc17 = all_carPR

### TOP GEARS ####
fleet_final_gears <- length_data_gears[length_data_gears$fleet==1,]

fcounts = fleet_final_gears %>%  group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
  tabyl(LAND_STANDARD_GEAR_NAME) %>%
  mutate(n_labels = paste0(LAND_STANDARD_GEAR_NAME, " (n= ", n, ")" ))

all_car_gearsPR <-
  fleet_final_gears %>%  group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
  group_by(YEAR) %>%
  dplyr::mutate(year_labs = paste0(YEAR, "\n n = ", n())) %>%
  ggplot(aes(FL_CM, color = LAND_STANDARD_GEAR_NAME))+
  geom_density(size = 0.75)+
  #scale_color_manual(values = gearcols, labels = counts$n_labels)+
  scale_color_hue(labels=fcounts$n_labels)+
  labs(color = "Gear Type", x = "Fork Length (cm)", title = paste0(county,  "\n (N = ", sum(fcounts$n), ")"))+
  facet_wrap(~year_labs, ncol = 10)+
  # theme_minimal()
  guides(color=guide_legend(ncol = 2))+
  theme(legend.title = element_text(size=14), 
        legend.text = element_text(size=12),
        legend.position = "bottom")

export_fig_page(all_car_gearsPR)

abc18 = all_car_gearsPR

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
                    "sedar_84_pr_yt_2022",
                    "pr_yt_2022_figures.RData") 
)

