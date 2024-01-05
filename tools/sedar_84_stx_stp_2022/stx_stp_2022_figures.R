# This script will be expanding upon the length-comp script c1_nominal_len_comps.Rmd created by Molly Stevens.
# NOTES: lines #-# out are useful but not necessary for this document as it stands
#         lines #-# and tabbed over were #-# in the original script before editing for this document

# Stoplight Parrotfish STX ####

# Set up library 

librarian::shelf(here, tidyverse, ROracle, keyring, dotenv, reshape, openxlsx, janitor, DT, pander, knitr, flextable) #plyr
# if conflicts in pkgs arise, use the following:
# library(conflicted)
# conflicted::conflicts_prefer(here::here)

#M:\SFD\SECM-SFD\_Assessments-SEDAR\SEDAR_57_Caribbean_Spiny_Lobster\Analyses\StockSynthesis\2022 Update\Documentation\SEFSC-SEDAR-57U-CR-LOB-2021 
# reference for working paper 

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

# Filter to STTJ and Yellowtail Snapper AND create new filterable date value 
stx_slp <- tip |> 
  filter(COUNTY_LANDED == "ST CROIX",
         OBS_STANDARD_SPECIES_CODE == "170867") |> 
  mutate(TEST_DATE = as.Date(ymd_hms(INTERVIEW_DATE)),
         FINAL_DATE = case_when(is.na(TEST_DATE) ~ INTERVIEW_DATE, 
                                TRUE ~ TEST_DATE))


source("~/SEFSC-SFD-CFB-TIP-Compositions/data/functions/len_len_convert.R")
source("~/SEFSC-SFD-CFB-TIP-Compositions/data/functions/fig_format_export.R")

LLconv <- read_csv("~/SEFSC-SFD-CFB-TIP-Compositions/data/CSVs/LLconversions.csv",
                   show_col_types = FALSE)

TIP_gears <- read_csv("~/SEFSC-SFD-CFB-TIP-Compositions/data/CSVs/tip_gears_stp_stx_LANDSTDGEARNAME.csv",
                      show_col_types = FALSE)

gearcols <- c("Net" = "#FF0000", "Trap" = "#00A08A", "Other" = "#5D057D", "Hook and Line" ="#046C9A", "Gill Net" = "#00A08A" , "Haul Seine" = "#00A08A" , "Lobster Trap" = "#00A08A" , "Trammel Net" = "#00A08A" , "Combined" = "black")



## Control Settings and Exploratory Frequency Tables

sp <- "SLP"

region <- "PUERTO RICO - USVI"

county <- "ST CROIX"

bin_size <- 1

len_type <- "FORK LENGTH"

# table(stx_slp$LENGTH_TYPE1, useNA='always')
# table(stx_slp$LENGTH_TYPE2, useNA='always')
# 
# min(stx_slp$LENGTH1_MM,na.rm = TRUE)
# max(stx_slp$LENGTH1_MM,na.rm = TRUE)
# 
# tip_range  <- stx_slp[with(stx_slp,order(-LENGTH1_MM)),]
# tip_range$LENGTH1_MM[1:25]
# 
# tip_range2 <- stx_slp[with(stx_slp,order(LENGTH1_MM)),]
# tip_range2$LENGTH1_MM[1:25]

##range currently set to not drop any obs (centimeters)

min_size <- 12 

max_size <- 83   

# min(stx_slp$YEAR,na.rm = TRUE)

min_year <- 1983

max_year <- 2022 

break_year <- 2005 # this is an optional value to denote a change in management. inclusive on the upper bound. can be set to NA if not relevant.  2005 trap specifications were updated and many closures went into place

#---------------------------------------------------------------------# 
##  TABLES START HERE 
#---------------------------------------------------------------------# 
# table(stx_slp$OBS_STANDARD_SPECIES_NAME, useNA='always')
# 
# table(stx_slp$LAND_STANDARD_GEAR_NAME, useNA='always')
# table(stx_slp$LAND_STANDARD_GEAR_NAME, stx_slp$STATE_LANDED, useNA='always')
# 
# table(stx_slp$LAND_GEAR_NAME, stx_slp$STATE_LANDED, useNA='always')
# 
# table(stx_slp$FISHING_MODE, stx_slp$INT_TYPE, useNA='always')

      # marfin <- stx_slp%>%
      #             filter(INT_TYPE == "USVI MARFIN REEFFISH SAMPLING")


# table(stx_slp$YEAR,stx_slp$OBS_STANDARD_SPECIES_NAME, useNA='always')
# 
# table(stx_slp$COUNTY_LANDED,stx_slp$STATE_LANDED,useNA = 'always')
# 
# table(stx_slp$YEAR,stx_slp$STATE_LANDED,useNA = 'always')


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
    YEAR <= 2023
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


# Length composition after filtering to commercial records


# table(tip2$COUNTY_LANDED,tip2$ISLAND,useNA = 'always')
# table(tip2$YEAR,tip2$ISLAND,useNA = 'always')
# 
# 
# lencheck <- tip2[tip2$LENGTH1_MM>2000,] #no lengths greater than 2000

    # tip_range  <- tip2[with(tip2,order(-LENGTH1_MM)),]
    # tip_range$LENGTH1_MM[1:25]
    # 
    # tip_range2 <- tip2[with(tip2,order(LENGTH1_MM)),]
    # tip_range2$LENGTH1_MM[1:25]
# table(tip2$LENGTH_TYPE1,useNA='always')
# 
# 
# table(stx_slp$LAND_STANDARD_GEAR_NAME,useNA='always')
    # 
    # table(tip2$ITIS_CODE,tip2$SPECIES,useNA='always')
    # table(tip2$SPECIES,tip2$SLP,useNA='always')


# Extract marfin samples, look at island distribution


##extract marfin samples, look at island distribution
# marfin <- tip2%>%
#   filter(INT_TYPE == "USVI MARFIN REEFFISH SAMPLING")
# table(marfin$YEAR, marfin$ISLAND,useNA='always')
# 
# table(marfin$LAND_STANDARD_GEAR_NAME, marfin$ISLAND,useNA='always')
# 
# table(marfin$LAND_STANDARD_GEAR_NAME,marfin$YEAR, marfin$ISLAND,useNA='always')
# 
# table(tip2$YEAR,tip2$LENGTH_TYPE1,useNA='always')  ##TL more prevalent in recent years
    # table(tip2$LENGTH_TYPE1,tip2$LENGTH_TYPE2,useNA='always') 

# table(tip2$ISLAND,tip2$STATE_LANDED)

    #plot(tip2$LENGTH1_MM,tip2$OBS_WEIGHT_KG,color=tip2$LENGTH_TYPE1)
    #plot(tip2$LENGTH1_MM,tip2$SAMPLE_WEIGHT_KG)
    #plot(tip2$LENGTH1_MM,tip2$SUB_SAMPLE_WEIGHT_KG)
# tip2$yearc <- as.character(tip2$YEAR)

    ##############export all plots starting w/ year>=2015, all islands
    #### here, isolated ST THOMAS 2018 for bulk of problems w single port sampler
    #### create new RMarkdown script describing this issue for internal distribution
    
    # tip2[(tip2$YEAR==2018 & tip2$ISLAND=='ST THOMAS'),] %>%
    # ggplot(aes(LENGTH1_MM,OBS_WEIGHT_KG))+
    #   geom_point(aes(color = AGENT_USERNAME_ID))  +  ##fork length is splitting
    #   # geom_point(aes(color = ISLAND))  +
    #   #labs(title = paste0(tip2$ISLAND,  "\n", min(tip2$YEAR), "-", max(tip2$YEAR)))+
    #   theme_minimal()
#---------------------------------------------------------------#
# merge in gear tables 

##LOOK AT GEAR ASSIGNMENTS BY SPECIES
# table(stx_slp$LAND_STANDARD_GEAR_NAME, stx_slp$STANDARDGEARNAME_2)
    #table(stx_slp$GEAR, useNA='always')  ##N gears?


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
         ISLAND != 'NOT CODED')
    #,
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



# Filtration level investigation ####

## Unique trip ID vs gear by year investigation

#### How many unique trip ID's per year?

# Group by year, count unique trip id's
# 
# When we remove data from years with less than 10 unique trip ID's, we are left with 24 years of data (vs 33 without filter).


# length_data_tripcount <- length_data_final |> 
#   group_by(YEAR) |>
#   summarise(n_distinct(ID)) 
# print.data.frame(length_data_tripcount)
# 
# length_data_trip10 <- length_data_final |> 
#   group_by(YEAR) |>
#   filter(n_distinct(ID) >= 10)|>
#   summarise(n_distinct(ID)) 
# print.data.frame(length_data_trip10)


## Unique length records per gear per year ####

### How many length records per gear per year?

# Group by year, count occurrences of each gear
# 
# There are years when individual gears occur less than 30 times but when grouped, the gear groups exceed 30 occurrences per year. Do we want to rely on gear groups with larger than 30 occurrences per year or individual gears with more than 30 occurrences?


# # count how many of each gear name occur each year
# length_data_gearcount <- length_data_final |> 
#   group_by(YEAR, LAND_STANDARD_GEAR_NAME) |>
#   summarise(count = n()) 
# print.data.frame(length_data_gearcount)
# 
# # count how many of each gear name occur 30 or more times each year
# length_data_gear30 <- length_data_final |> 
#   group_by(YEAR, LAND_STANDARD_GEAR_NAME) |>
#   filter(n() >= 30)|>
#   summarise(count = n()) 
# print.data.frame(length_data_gear30)
# 
# # group gears that occur 30 or more times per year by gear grouping 
# length_data_geartogether30 <- length_data_final |> 
#   group_by(YEAR, LAND_STANDARD_GEAR_NAME) |>
#   filter(n() >= 30)|>
#   ungroup() |> 
#   group_by(YEAR, gear) |> 
#   summarise(count = n())
# print.data.frame(length_data_geartogether30)
# # frequency of gear groupings when records are counted by specific gear per year
# # 29 years of data available 
# table(length_data_geartogether30$YEAR, length_data_geartogether30$gear)
# 
# # count how many of each gear grouping occur each year
# length_data_geargroupcount <- length_data_final |> 
#   group_by(YEAR, gear) |>
#   summarise(count = n()) 
# print.data.frame(length_data_geargroupcount)
# 
# # count how many of each gear grouping occur 30 or more times each year
# length_data_geargroup30 <- length_data_final |> 
#   group_by(YEAR, gear) |>
#   filter(n() >= 30)|>
#   summarise(count = n()) 
# print.data.frame(length_data_geargroup30)
# 
# # frequency of gear groupings when records are counted by gear grouping per year
# # 29 years of data available 
# table(length_data_geargroup30$YEAR, length_data_geargroup30$gear)
# 
# # count number of records that are removed if counted by individual gears 
# length_data_gear29 <- length_data_final |> 
#   group_by(YEAR, LAND_STANDARD_GEAR_NAME) |>
#   filter(n() < 30)|>
#   # ungroup() |> 
#   # group_by(YEAR, gear) |> 
#   summarise(count = n())
# print.data.frame(length_data_gear29)
# 
# # VS 
# 
# # count number of records that are removed if counted by gear groups 
# length_data_geargroup29 <- length_data_final |> 
#   group_by(YEAR, gear) |>
#   filter(n() < 30)|>
#   # ungroup() |> 
#   # group_by(YEAR, gear) |> 
#   summarise(count = n())
# print.data.frame(length_data_geargroup29)
# 

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
  select(YEAR, FINAL_DATE, ID, COUNTY, FL_CM, LAND_STANDARD_GEAR_NAME, gear) |> 
  # filter(gear == "Hook and Line") |> 
  mutate(ID = as.character(ID)) |> 
  select(-gear)
library(ggpubr)
# Create box plot across years
box_plot <- ggboxplot(length_data_glm, x = "LAND_STANDARD_GEAR_NAME", y = "FL_CM",
                      color = "LAND_STANDARD_GEAR_NAME",
                      ylab = "FL_CM", xlab = "LAND_STANDARD_GEAR_NAME")
box_plot
# Create density plot across years
density_plot <- ggdensity(length_data_glm, x = "FL_CM",
                          add = "mean", rug = TRUE,
                          color = "LAND_STANDARD_GEAR_NAME", fill = "LAND_STANDARD_GEAR_NAME",
                          ylab = "FL_CM", xlab = "Gear")
density_plot

#table(length_data_glm$LAND_STANDARD_GEAR_NAME, length_data_glm$YEAR)

# plot data
library(ggplot2)
abc4 = allgears_glm_plot <- length_data_glm |> 
  ggplot(aes(x = as.Date(FINAL_DATE), y = FL_CM)) +
  geom_point(aes(colour = LAND_STANDARD_GEAR_NAME, shape=LAND_STANDARD_GEAR_NAME), size = 1, alpha = 0.5) +
  scale_shape_manual(values=c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 13, 14))+
  geom_smooth(method = "lm", formula = "y ~ x", col = "black") +
  # facet_wrap(~ COUNTY_LANDED) +
  labs(x = "", y = "Length (cm)", colour = "", shape = "") +
  theme_bw() +
  theme(legend.position = "bottom", legend.text = element_text(size = 10),
        legend.box.spacing = unit(0, "npc"), panel.grid = element_blank()) +
  guides(colour = guide_legend(override.aes = list(size = 2)))

# # run again but with only sig different gears
# rodreelfishtraps <- length_data_glm |> 
#   filter(LAND_STANDARD_GEAR_NAME == c("LINES HAND", "POTS AND TRAPS; FISH", "ROD AND REEL"))
# 
# ggplot(rodreelfishtraps, aes(x = as.Date(FINAL_DATE), y = FL_CM)) +
#   geom_point(aes(colour = LAND_STANDARD_GEAR_NAME, shape = LAND_STANDARD_GEAR_NAME), size = 1, alpha = 0.5) +
#   geom_smooth(method = "lm", formula = "y ~ x", col = "black") +
#   # facet_wrap(~ COUNTY_LANDED) +
#   labs(x = "", y = "Length (cm)", colour = "", shape = "") +
#   theme_bw() +
#   theme(legend.position = "bottom", legend.text = element_text(size = 10),
#         legend.box.spacing = unit(0, "npc"), panel.grid = element_blank()) +
#   guides(colour = guide_legend(override.aes = list(size = 2)))
# 
# density_plot <- ggdensity(rodreelfishtraps, x = "FL_CM",
#                           add = "mean", rug = TRUE,
#                           color = "LAND_STANDARD_GEAR_NAME", fill = "LAND_STANDARD_GEAR_NAME",
#                           ylab = "FL_CM", xlab = "Gear")
# density_plot


# fit models
library(lmerTest)
# mod0 = lmer(FL_CM ~ COUNTY * scale(FINAL_DATE) + LAND_STANDARD_GEAR_NAME + (1 | YEAR) + (1 | ID),
#             data = length_data_glm, REML = FALSE)
# anova(mod0)
# 
# mod0.1 = lmer(FL_CM ~ COUNTY + scale(FINAL_DATE) + LAND_STANDARD_GEAR_NAME + (1 | YEAR) + (1 | ID),
#                data = length_data_glm, REML = FALSE)
# anova(mod0.1)
# 
# # Gaussian full model - not using county landed because it is effectively one island
# mod1 = lmer(FL_CM ~ COUNTY + LAND_STANDARD_GEAR_NAME + (1 | YEAR) + (1 | ID),
#             data = length_data_glm, REML = FALSE)
# anova(mod1)
# summary(mod1, correlation = FALSE)

# comparing length to date and gear in a linear model
mod1.1 = lmer(FL_CM ~ scale(FINAL_DATE) + LAND_STANDARD_GEAR_NAME + (1 | YEAR) + (1 | ID),
              data = length_data_glm, REML = FALSE)
anova(mod1.1) # both date and gear are significant in linear model

# comparing length to date and gear in a gamma full model
mod2 = glmer(FL_CM ~ scale(FINAL_DATE) + LAND_STANDARD_GEAR_NAME + (1 | YEAR) + (1 | ID),
             data = length_data_glm, family = Gamma(link=log))

# check which number is lower, lower number is better fitting model 
AIC(mod1.1, mod2) # glm is better fit

# gamma reduced model
mod3 = glmer(FL_CM ~ scale(FINAL_DATE) + (1 | YEAR) + (1 | ID),
             data = length_data_glm, family = Gamma(link=log))

mod4 = glmer(FL_CM ~ LAND_STANDARD_GEAR_NAME + (1 | YEAR) + (1 | ID),
             data = length_data_glm, family = Gamma(link=log))

# likelihood ratio test - compare gamma full model to reduced model to find p-value of value excluded from reduced model 

# gives p value of gear - significant p value shows one of the gears present is significantly different from the others
### 
anova(mod2, mod3)
# gives p value of date - significant p value shows the slope of the line of fit across time is significantly different from a 0(zero) slope 
### 
anova(mod2, mod4)

# pairwise comparisons (if needed)(needed for Hook and Line)
### 
mod_contr = emmeans::emmeans(object = mod2, pairwise ~ "LAND_STANDARD_GEAR_NAME", adjust = "tukey")
mod_contr

# cld provides gear groupings based on which gears are similar vs significantly different from each other 
### 
multcomp::cld(object = mod_contr$emmeans)

allgears_multcompcld <- multcomp::cld(object = mod_contr$emmeans)

length_data_fishcount <- length_data_final |> 
  group_by(LAND_STANDARD_GEAR_NAME) |>
  summarise(n_fish = n()) 
length_data_tripcount <- length_data_final |> 
  group_by(LAND_STANDARD_GEAR_NAME) |>
  summarise(n_distinct(ID)) 

allgears_multcompcld_fish <- full_join(allgears_multcompcld, length_data_fishcount, by = "LAND_STANDARD_GEAR_NAME")
allgears_multcompcld_trip <- full_join(allgears_multcompcld_fish, length_data_tripcount, by = "LAND_STANDARD_GEAR_NAME")

allgears_multcompcld_final <- allgears_multcompcld_trip |> 
  dplyr::rename("group" = ".group",
                "n_distinct_ID" = "n_distinct(ID)") |> 
  mutate(Percentage = (n_fish/sum(n_fish))*100) |> 
  arrange(desc(Percentage)) 
  
allgears_multcompcld_final$Percentage<-format(round(allgears_multcompcld_final$Percentage,2),nsmall=2)

tbl1 = flextable(allgears_multcompcld_final) |> autofit()

# print(allgears_multcompcld_final)
# write.csv(allgears_multcompcld_final, file = "~/SEFSC-SFD-CFB-TIP-Compositions/tools/output/stx_slps_glm_allgears.csv", row.names = FALSE)

# group <- data.frame(group = c('1','2', "3"))
# # sapply(sapply(as.character(group$group), function(i) grep(i, allgears_multcompcld_final$group)), function(i) sum(allgears_multcompcld_final$n_fish[i]))
# # 
# # sapply(sapply(as.character(group$group), function(i) grep(i, allgears_multcompcld_final$group)), function(i) sum(allgears_multcompcld_final$n_distinct_ID[i]))
# 
# allgear_glm_totals <- data.frame(group = c(1, 2, 3),
#                                  n_distinct_ID = (sapply(sapply(as.character(group$group), function(i) grep(i, allgears_multcompcld_final$group)), function(i) sum(allgears_multcompcld_final$n_distinct_ID[i]))),
#                                  n_fish = (sapply(sapply(as.character(group$group), function(i) grep(i, allgears_multcompcld_final$group)), function(i) sum(allgears_multcompcld_final$n_fish[i]))))
# 
# print(allgear_glm_totals)
# write.csv(allgear_glm_totals, file = "~/SEFSC-SFD-CFB-TIP-Compositions/tools/output/stx_slps_glm_allgears_totals.csv", row.names = FALSE)



## All Gears 2012 and after ####


# str(length_data_final)
library(ggplot2)
length_data_glm_2012 <- length_data_final |>
  filter(YEAR >= 2012) |> 
  select(YEAR, FINAL_DATE, ID, COUNTY, FL_CM, LAND_STANDARD_GEAR_NAME, gear) |> 
  # filter(gear == "Hook and Line") |> 
  mutate(ID = as.character(ID)) |> 
  select(-gear)
library(ggpubr)
# Create box plot across years
box_plot <- ggboxplot(length_data_glm_2012, x = "LAND_STANDARD_GEAR_NAME", y = "FL_CM",
                      color = "LAND_STANDARD_GEAR_NAME",
                      ylab = "FL_CM", xlab = "LAND_STANDARD_GEAR_NAME")
box_plot
# Create density plot across years
density_plot <- ggdensity(length_data_glm_2012, x = "FL_CM",
                          add = "mean", rug = TRUE,
                          color = "LAND_STANDARD_GEAR_NAME", fill = "LAND_STANDARD_GEAR_NAME",
                          ylab = "FL_CM", xlab = "Gear")
density_plot

#table(length_data_glm$LAND_STANDARD_GEAR_NAME, length_data_glm$YEAR)

# plot data
library(ggplot2)
abc5 = ggplot(length_data_glm_2012, aes(x = as.Date(FINAL_DATE), y = FL_CM)) +
  geom_point(aes(colour = LAND_STANDARD_GEAR_NAME, shape = LAND_STANDARD_GEAR_NAME), size = 1, alpha = 0.5) +
  geom_smooth(method = "lm", formula = "y ~ x", col = "black") +
  # facet_wrap(~ COUNTY_LANDED) +
  labs(x = "", y = "Length (cm)", colour = "", shape = "") +
  theme_bw() +
  theme(legend.position = "bottom", legend.text = element_text(size = 10),
        legend.box.spacing = unit(0, "npc"), panel.grid = element_blank()) +
  guides(colour = guide_legend(override.aes = list(size = 2)))

# # run again but with only sig different gears
# rodreelfishtraps <- length_data_glm |> 
#   filter(LAND_STANDARD_GEAR_NAME == c("LINES HAND", "POTS AND TRAPS; FISH", "ROD AND REEL"))
# 
# ggplot(rodreelfishtraps, aes(x = as.Date(FINAL_DATE), y = FL_CM)) +
#   geom_point(aes(colour = LAND_STANDARD_GEAR_NAME, shape = LAND_STANDARD_GEAR_NAME), size = 1, alpha = 0.5) +
#   geom_smooth(method = "lm", formula = "y ~ x", col = "black") +
#   # facet_wrap(~ COUNTY_LANDED) +
#   labs(x = "", y = "Length (cm)", colour = "", shape = "") +
#   theme_bw() +
#   theme(legend.position = "bottom", legend.text = element_text(size = 10),
#         legend.box.spacing = unit(0, "npc"), panel.grid = element_blank()) +
#   guides(colour = guide_legend(override.aes = list(size = 2)))
# 
# density_plot <- ggdensity(rodreelfishtraps, x = "FL_CM",
#                           add = "mean", rug = TRUE,
#                           color = "LAND_STANDARD_GEAR_NAME", fill = "LAND_STANDARD_GEAR_NAME",
#                           ylab = "FL_CM", xlab = "Gear")
# density_plot


# fit models
library(lmerTest)
# mod0 = lmer(FL_CM ~ COUNTY * scale(FINAL_DATE) + LAND_STANDARD_GEAR_NAME + (1 | YEAR) + (1 | ID),
#             data = length_data_glm, REML = FALSE)
# anova(mod0)
# 
# mod0.1 = lmer(FL_CM ~ COUNTY + scale(FINAL_DATE) + LAND_STANDARD_GEAR_NAME + (1 | YEAR) + (1 | ID),
#                data = length_data_glm, REML = FALSE)
# anova(mod0.1)
# 
# # Gaussian full model - not using county landed because it is effectively one island
# mod1 = lmer(FL_CM ~ COUNTY + LAND_STANDARD_GEAR_NAME + (1 | YEAR) + (1 | ID),
#             data = length_data_glm, REML = FALSE)
# anova(mod1)
# summary(mod1, correlation = FALSE)

# comparing length to date and gear in a linear model
mod1.1 = lmer(FL_CM ~ scale(FINAL_DATE) + LAND_STANDARD_GEAR_NAME + (1 | YEAR) + (1 | ID),
              data = length_data_glm_2012, REML = FALSE)
anova(mod1.1) # both date and gear are significant in linear model

# comparing length to date and gear in a gamma full model
mod2 = glmer(FL_CM ~ scale(FINAL_DATE) + LAND_STANDARD_GEAR_NAME + (1 | YEAR) + (1 | ID),
             data = length_data_glm_2012, family = Gamma(link=log))

# check which number is lower, lower number is better fitting model 
AIC(mod1.1, mod2) # glm is better fit

# gamma reduced model
mod3 = glmer(FL_CM ~ scale(FINAL_DATE) + (1 | YEAR) + (1 | ID),
             data = length_data_glm_2012, family = Gamma(link=log))

mod4 = glmer(FL_CM ~ LAND_STANDARD_GEAR_NAME + (1 | YEAR) + (1 | ID),
             data = length_data_glm_2012, family = Gamma(link=log))

# likelihood ratio test - compare gamma full model to reduced model to find p-value of value excluded from reduced model 

# gives p value of gear - significant p value shows one of the gears present is significantly different from the others
### 
anova(mod2, mod3)
# gives p value of date - significant p value shows the slope of the line of fit across time is significantly different from a 0(zero) slope 
### 
anova(mod2, mod4)

# pairwise comparisons (if needed)(needed for Hook and Line)
### 
mod_contr = emmeans::emmeans(object = mod2, pairwise ~ "LAND_STANDARD_GEAR_NAME", adjust = "tukey")
mod_contr

# cld provides gear groupings based on which gears are similar vs significantly different from each other 
### 
multcomp::cld(object = mod_contr$emmeans)

allgears_multcompcld_2012 <- multcomp::cld(object = mod_contr$emmeans)

length_data_fishcount <- length_data_glm_2012 |> 
  group_by(LAND_STANDARD_GEAR_NAME) |>
  summarise(n_fish = n()) 
length_data_tripcount <- length_data_glm_2012 |> 
  group_by(LAND_STANDARD_GEAR_NAME) |>
  summarise(n_distinct(ID)) 

allgears_multcompcld_fish_2012 <- full_join(allgears_multcompcld_2012, length_data_fishcount, by = "LAND_STANDARD_GEAR_NAME")
allgears_multcompcld_trip_2012 <- full_join(allgears_multcompcld_fish_2012, length_data_tripcount, by = "LAND_STANDARD_GEAR_NAME")

allgears_multcompcld_finaL_2012 <- allgears_multcompcld_trip_2012 |> 
  dplyr::rename("group" = ".group",
                "n_distinct_ID" = "n_distinct(ID)")
print(allgears_multcompcld_finaL_2012)

write.csv(allgears_multcompcld_finaL_2012, file = "~/SEFSC-SFD-CFB-TIP-Compositions/tools/output/stx_slps_2012glm_allgears.csv", row.names = FALSE)

group_2012 <- data.frame(group = c('1','2', "3"))
# sapply(sapply(as.character(group$group), function(i) grep(i, allgears_multcompcld_final$group)), function(i) sum(allgears_multcompcld_final$n_fish[i]))
# 
# sapply(sapply(as.character(group$group), function(i) grep(i, allgears_multcompcld_final$group)), function(i) sum(allgears_multcompcld_final$n_distinct_ID[i]))

allgear_glm_totals_2012 <- data.frame(group = c(1, 2, 3),
                                      n_distinct_ID = (sapply(sapply(as.character(group_2012$group), function(i) grep(i, allgears_multcompcld_finaL_2012$group)), function(i) sum(allgears_multcompcld_finaL_2012$n_distinct_ID[i]))),
                                      n_fish = (sapply(sapply(as.character(group_2012$group), function(i) grep(i, allgears_multcompcld_finaL_2012$group)), function(i) sum(allgears_multcompcld_finaL_2012$n_fish[i]))))

print(allgear_glm_totals_2012)
write.csv(allgear_glm_totals_2012, file = "~/SEFSC-SFD-CFB-TIP-Compositions/tools/output/stx_slps_2012glm_allgears_totals.csv", row.names = FALSE)


## Diving ####

# str(length_data_final)
library(ggplot2)
use_gear_dv <- length_data_final |>
  select(YEAR, FINAL_DATE, ID, COUNTY, FL_CM, LAND_STANDARD_GEAR_NAME, gear) |> 
  filter(gear == "Diving") |> 
  mutate(ID = as.character(ID)) |> 
  select(-gear)
library(ggpubr)
# Create box plot across years
box_plot <- ggboxplot(use_gear_dv, x = "LAND_STANDARD_GEAR_NAME", y = "FL_CM",
                      color = "LAND_STANDARD_GEAR_NAME",
                      ylab = "FL_CM", xlab = "LAND_STANDARD_GEAR_NAME")
box_plot
# Create density plot across years
density_plot <- ggdensity(use_gear_dv, x = "FL_CM",
                          add = "mean", rug = TRUE,
                          color = "LAND_STANDARD_GEAR_NAME", fill = "LAND_STANDARD_GEAR_NAME",
                          ylab = "FL_CM", xlab = "Gear")
density_plot

# plot data
library(ggplot2)
ggplot(use_gear_dv, aes(x = as.Date(FINAL_DATE), y = FL_CM)) +
  geom_point(aes(colour = LAND_STANDARD_GEAR_NAME, shape = LAND_STANDARD_GEAR_NAME), size = 1, alpha = 0.5) +
  geom_smooth(method = "lm", formula = "y ~ x", col = "black") +
  # facet_wrap(~ COUNTY_LANDED) +
  labs(x = "", y = "Length (cm)", colour = "", shape = "") +
  theme_bw() +
  theme(legend.position = "bottom", legend.text = element_text(size = 10),
        legend.box.spacing = unit(0, "npc"), panel.grid = element_blank()) +
  guides(colour = guide_legend(override.aes = list(size = 2)))


# fit models
library(lmerTest)

# comparing length to date and gear in a linear model
mod1.1 = lmer(FL_CM ~ scale(FINAL_DATE) + LAND_STANDARD_GEAR_NAME + (1 | YEAR) + (1 | ID),
              data = use_gear_dv, REML = FALSE)
anova(mod1.1) # both date and gear are significant in linear model

# comparing length to date and gear in a gamma full model
mod2 = glmer(FL_CM ~ scale(FINAL_DATE) + LAND_STANDARD_GEAR_NAME + (1 | YEAR) + (1 | ID),
             data = use_gear_dv, family = Gamma(link=log))

# check which number is lower, lower number is better fitting model 
AIC(mod1.1, mod2) # glm is better fit

# gamma reduced model
mod3 = glmer(FL_CM ~ scale(FINAL_DATE) + (1 | YEAR) + (1 | ID),
             data = use_gear_dv, family = Gamma(link=log))

mod4 = glmer(FL_CM ~ LAND_STANDARD_GEAR_NAME + (1 | YEAR) + (1 | ID),
             data = use_gear_dv, family = Gamma(link=log))

# likelihood ratio test - compare gamma full model to reduced model to find p-value of value excluded from reduced model 

# gives p value of gear - significant p value shows one of the gears present is significantly different from the others
### hook and line gear p value is significant 
anova(mod2, mod3)
# gives p value of date - significant p value shows the slope of the line of fit across time is significantly different from a 0(zero) slope 
### hook and line date p value is significant 
anova(mod2, mod4)

### no significant p values for time period or gears, further comparisons not needed 

## Net - LAND_STANDARD_GEAR_NAME ####

# str(length_data_final)
library(ggplot2)
# net land_standard_gear_name
use_gear_nsg <- length_data_final |>
  select(YEAR, FINAL_DATE, ID, COUNTY, FL_CM, LAND_STANDARD_GEAR_NAME, gear) |> 
  filter(gear == "Net") |> 
  mutate(ID = as.character(ID)) |> 
  select(-gear)
library(ggpubr)
# Create box plot across years
box_plot <- ggboxplot(use_gear_nsg, x = "LAND_STANDARD_GEAR_NAME", y = "FL_CM",
                      color = "LAND_STANDARD_GEAR_NAME",
                      ylab = "FL_CM", xlab = "LAND_STANDARD_GEAR_NAME")
box_plot
# Create density plot across years
density_plot <- ggdensity(use_gear_nsg, x = "FL_CM",
                          add = "mean", rug = TRUE,
                          color = "LAND_STANDARD_GEAR_NAME", fill = "LAND_STANDARD_GEAR_NAME",
                          ylab = "FL_CM", xlab = "Gear")
density_plot

# plot data
library(ggplot2)
ggplot(use_gear_nsg, aes(x = as.Date(FINAL_DATE), y = FL_CM)) +
  geom_point(aes(colour = LAND_STANDARD_GEAR_NAME, shape = LAND_STANDARD_GEAR_NAME), size = 1, alpha = 0.5) +
  geom_smooth(method = "lm", formula = "y ~ x", col = "black") +
  # facet_wrap(~ COUNTY_LANDED) +
  labs(x = "", y = "Length (cm)", colour = "", shape = "") +
  theme_bw() +
  theme(legend.position = "bottom", legend.text = element_text(size = 10),
        legend.box.spacing = unit(0, "npc"), panel.grid = element_blank()) +
  guides(colour = guide_legend(override.aes = list(size = 2)))


# fit models
library(lmerTest)

### both fixed linear and glm models have warning message because ENTANGLING NETS (GILL) UNSPECIFIED only occur in one year, preventing convergence across timeline 
# comparing length to date and gear in a liner model
mod1.1 = lmer(FL_CM ~ scale(FINAL_DATE) + LAND_STANDARD_GEAR_NAME + (1 | YEAR) + (1 | ID),
              data = use_gear_nsg, REML = FALSE)
anova(mod1.1)

# comparing length to date and gear in a gamma full model
mod2 = glmer(FL_CM ~ scale(FINAL_DATE) + LAND_STANDARD_GEAR_NAME + (1 | YEAR) + (1 | ID),
             data = use_gear_nsg, family = Gamma(link=log)) 

# check which number is lower, lower number is better fitting model 
AIC(mod1.1, mod2) # glm is better fit

# gamma reduced model
mod3 = glmer(FL_CM ~ scale(FINAL_DATE) + (1 | YEAR) + (1 | ID),
             data = use_gear_nsg, family = Gamma(link=log))

mod4 = glmer(FL_CM ~ LAND_STANDARD_GEAR_NAME + (1 | YEAR) + (1 | ID),
             data = use_gear_nsg, family = Gamma(link=log))

# likelihood ratio test - compare gamma full model to reduced model to find p-value of value excluded from reduced model 
# gives p value of gear - significant p value shows one of the gears present is significantly different from the others
### net (using LAND_STANDARD_GEAR_NAME) does not have any significantly different gears 
anova(mod2, mod3) 
# gives p value of date - significant p value shows the slope of the line of fit across time is significantly different from a 0(zero) slope
### There is no significantly different time in the Net grouping 
anova(mod2, mod4)
### no significant p values for time period or gears, further comparisons not needed 


## Net - LAND_GEAR_NAME ####

# str(length_data_final)
library(ggplot2)
# net land_standard_gear_name
use_gear_nlg <- length_data_final |>
  select(YEAR, FINAL_DATE, ID, COUNTY, FL_CM, LAND_GEAR_NAME, gear) |>
  filter(gear == "Net") |>
  mutate(ID = as.character(ID)) |>
  select(-gear)
library(ggpubr)
# Create box plot across years
box_plot <- ggboxplot(use_gear_nlg, x = "LAND_GEAR_NAME", y = "FL_CM",
                      color = "LAND_GEAR_NAME",
                      ylab = "FL_CM", xlab = "LAND_GEAR_NAME")
box_plot
# Create density plot across years
density_plot <- ggdensity(use_gear_nlg, x = "FL_CM",
                          add = "mean", rug = TRUE,
                          color = "LAND_GEAR_NAME", fill = "LAND_GEAR_NAME",
                          ylab = "FL_CM", xlab = "Gear")
density_plot

# plot data
library(ggplot2)
ggplot(use_gear_nlg, aes(x = as.Date(FINAL_DATE), y = FL_CM)) +
  geom_point(aes(colour = LAND_GEAR_NAME, shape = LAND_GEAR_NAME), size = 1, alpha = 0.5) +
  geom_smooth(method = "lm", formula = "y ~ x", col = "black") +
  # facet_wrap(~ COUNTY_LANDED) +
  labs(x = "", y = "Length (cm)", colour = "", shape = "") +
  theme_bw() +
  theme(legend.position = "bottom", legend.text = element_text(size = 10),
        legend.box.spacing = unit(0, "npc"), panel.grid = element_blank()) +
  guides(colour = guide_legend(override.aes = list(size = 2)))


# fit models
# library(lmerTest)

### both fixed linear and glm models have warning message because ENTANGLING NETS (GILL) UNSPECIFIED only occur in one year, preventing convergence across timeline 
# comparing length to date and gear in a liner model
mod1.1 = lmer(FL_CM ~ scale(FINAL_DATE) + LAND_GEAR_NAME + (1 | YEAR) + (1 | ID),
              data = use_gear_nlg, REML = FALSE)
anova(mod1.1)
# summary(mod1.1, correlation = FALSE)

# comparing length to date and gear in a gamma full model
mod2 = glmer(FL_CM ~ scale(FINAL_DATE) + LAND_GEAR_NAME + (1 | YEAR) + (1 | ID),
             data = use_gear_nlg, family = Gamma(link=log))
# anova(mod2) #no p value given for gamma model by itself

# check which number is lower, lower number is better fitting model
AIC(mod1.1, mod2)
# glm is better fit

# gamma reduced model
mod3 = glmer(FL_CM ~ scale(FINAL_DATE) + (1 | YEAR) + (1 | ID),
             data = use_gear_nlg, family = Gamma(link=log))

mod4 = glmer(FL_CM ~ LAND_GEAR_NAME + (1 | YEAR) + (1 | ID),
             data = use_gear_nlg, family = Gamma(link=log))

# likelihood ratio test - compare gamma full model to reduced model to find p-value of value excluded from reduced model
anova(mod2, mod3) #gives p value of gear - significant p value shows one of the gears present is significantly different from the others
anova(mod2, mod4) #gives p value of date - significant p value shows the slope of the line of fit across time is significantly different from a 0(zero) slope
### no significant p values for time period or gears, further comparisons not needed

## Trap ####

# str(length_data_final)
library(ggplot2) 
# net land_standard_gear_name
use_gear_tr <- length_data_final |>
  select(YEAR, FINAL_DATE, ID, COUNTY, FL_CM, LAND_STANDARD_GEAR_NAME, gear) |> 
  filter(gear == "Trap") |> 
  mutate(ID = as.character(ID)) |> 
  select(-gear)
library(ggpubr)
# Create box plot across years
box_plot <- ggboxplot(use_gear_tr, x = "LAND_STANDARD_GEAR_NAME", y = "FL_CM",
                      color = "LAND_STANDARD_GEAR_NAME",
                      ylab = "FL_CM", xlab = "LAND_STANDARD_GEAR_NAME")
box_plot
# Create density plot across years
density_plot <- ggdensity(use_gear_tr, x = "FL_CM",
                          add = "mean", rug = TRUE,
                          color = "LAND_STANDARD_GEAR_NAME", fill = "LAND_STANDARD_GEAR_NAME",
                          ylab = "FL_CM", xlab = "Gear")
density_plot

# plot data
library(ggplot2)
ggplot(use_gear_tr, aes(x = as.Date(FINAL_DATE), y = FL_CM)) +
  geom_point(aes(colour = LAND_STANDARD_GEAR_NAME, shape = LAND_STANDARD_GEAR_NAME), size = 1, alpha = 0.5) +
  geom_smooth(method = "lm", formula = "y ~ x", col = "black") +
  # facet_wrap(~ COUNTY_LANDED) +
  labs(x = "", y = "Length (cm)", colour = "", shape = "") +
  theme_bw() +
  theme(legend.position = "bottom", legend.text = element_text(size = 10),
        legend.box.spacing = unit(0, "npc"), panel.grid = element_blank()) +
  guides(colour = guide_legend(override.aes = list(size = 2)))


# fit models
library(lmerTest)

# comparing length to date and gear in a liner model
mod1.1 = lmer(FL_CM ~ scale(FINAL_DATE) + LAND_STANDARD_GEAR_NAME + (1 | YEAR) + (1 | ID),
              data = use_gear_tr, REML = FALSE)
anova(mod1.1)

# comparing length to date and gear in a gamma full model
mod2 = glmer(FL_CM ~ scale(FINAL_DATE) + LAND_STANDARD_GEAR_NAME + (1 | YEAR) + (1 | ID),
             data = use_gear_tr, family = Gamma(link=log))

# check which number is lower, lower number is better fitting model 
AIC(mod1.1, mod2) # glm is better fit

# gamma reduced model
mod3 = glmer(FL_CM ~ scale(FINAL_DATE) + (1 | YEAR) + (1 | ID),
             data = use_gear_tr, family = Gamma(link=log))

mod4 = glmer(FL_CM ~ LAND_STANDARD_GEAR_NAME + (1 | YEAR) + (1 | ID),
             data = use_gear_tr, family = Gamma(link=log))

# likelihood ratio test - compare gamma full model to reduced model to find p-value of value excluded from reduced model 
# gives p value of gear - significant p value shows one of the gears present is significantly different from the others
anova(mod2, mod3)
# gives p value of date - significant p value shows the slope of the line of fit across time is significantly different from a 0(zero) slope 
### There is no significantly different time in the Trap grouping 
anova(mod2, mod4)

# pairwise comparisons (if needed)(needed for Hook and Line)
### 
mod_contr = emmeans::emmeans(object = mod2, pairwise ~ "LAND_STANDARD_GEAR_NAME", adjust = "tukey")
mod_contr

# cld provides gear groupings based on which gears are similar vs significantly different from each other 
### 
multcomp::cld(object = mod_contr$emmeans)

# I took out box traps to confirm difference between fish trap and cmb traps and difference is still present 


# Gear Density Plots ####

# Filtered to years with 30 or more length records (regardless of gear) per year.

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
  geom_density(aes(color = gear), linewidth = 0.75)+
  scale_color_hue(labels=ycounts$n_labels)+
  # scale_color_hue(labels=c("Combined",ycounts$n_labels))+
  #scale_color_manual(values = gearcols, labels = c("Combined", counts$n_labels))+
  labs(color = "Gear Type", x = "Fork Length (cm)", title = paste0(county,  "\n (N = ", sum(ycounts$n), ")"))+
  # theme_minimal()
  theme(legend.title = element_text(size=14), 
        legend.text = element_text(size=12))+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=gear),
                   linetype="dashed")

abc2 = agr_den_allgears

# geardensity_plot <- length_data_final %>% group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
#   ggdensity(x = "FL_CM",
#                           add = "mean", rug = TRUE,
#                           color = "gear", fill = "gear",
#                           ylab = "FL_CM", xlab = "Gear")
# abc3 = geardensity_plot


    #  filter to 30 or more of same gear each year 
    # ygearcounts =length_data_final |> 
    #   group_by(YEAR, LAND_STANDARD_GEAR_NAME) |>
    #   filter(n() >= 30)|>
    #   ungroup() |> 
    #   tabyl(gear) %>%
    #   mutate(n_gearlabels = paste0(gear, " (n= ", n, ")" ))
    # 
    #  length_data_final |> 
    #   group_by(YEAR, LAND_STANDARD_GEAR_NAME) |>
    #   filter(n() >= 30)|>
    #   ungroup() |> 
    # ggplot(aes(FL_CM))+
    #  # geom_density( aes(color = "Combined"),lwd=1.5)+
    #   geom_density(aes(color = gear), size = 0.75)+
    #   scale_color_hue(labels=ygearcounts$n_gearlabels)+
    #  # scale_color_hue(labels=c("Combined",ycounts$n_labels))+
    #   #scale_color_manual(values = gearcols, labels = c("Combined", counts$n_labels))+
    #   labs(color = "Gear Type", x = "Fork Length (cm)", title = paste0(region,  "\n (N = ", sum(ygearcounts$n), ")"))+
    #   # theme_minimal()
    #   theme(legend.title = element_text(size=14), 
    #     legend.text = element_text(size=12))



# USVI TIP length comps by gear and interview type. Investigate if MARFIN and MRAG interview types can be used or should be dropped?
  

#table(tip_CR$INT_TYPE,useNA='always')
# 
# usvi <- length_data_final[length_data_final$STATE=='VIRGIN ISLANDS',]
# counts = usvi %>%
#   tabyl(INT_TYPE) %>%
#   mutate(n_labels = paste0(INT_TYPE, " (n= ", n, ")" ))
# 
# usvi %>%
#   ggplot(aes(FL_CM, color = INT_TYPE))+
#   geom_density(size = 0.75)+
#   scale_color_hue(labels=counts$n_labels)+
#   #  scale_color_manual(values = gearcols, labels = counts$n_labels)+
#   labs(color = "Interview Type", x = "Fork Length (cm)", title = paste0(usvi$STATE,  "\n (N = ", sum(counts$n), ")"))+
#   facet_wrap(~gear) +
#   # theme_minimal()
#   theme(legend.title = element_text(size=14), 
#         legend.text = element_text(size=12))

## Gear Groupings

# Filtered to minimum 30 fish per year

# 
# 
# length_data_final %>%  group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
#   ggplot(aes(FL_CM, color = gear))+
#   geom_density(size = 0.75)+
#   scale_color_hue(labels=ycounts$n_labels)+
#   #  scale_color_manual(values = gearcols, labels = counts$n_labels)+
#   labs(color = "Gear Type", x = "Fork Length (cm)", title = paste0(county,  "\n (N = ", sum(ycounts$n), ")"))+
#   # facet_wrap(~ISLAND,ncol=1) +
#   # theme_minimal()
#   theme(legend.title = element_text(size=14), 
#         legend.text = element_text(size=12))

### Diving ####
dv<- length_data_final[length_data_final$gear_short=='DV',]
counts = dv %>% group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
  tabyl(LAND_STANDARD_GEAR_NAME) %>%
  mutate(n_labels = paste0(LAND_STANDARD_GEAR_NAME, " (n= ", n, ")" ))

mudv <- ddply(dv, "LAND_STANDARD_GEAR_NAME", summarise, grp.mean=mean(FL_CM))
head(mudv)


dv %>% group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
  ggplot(aes(FL_CM, color = LAND_STANDARD_GEAR_NAME))+
  geom_density(size = 0.75)+
  # scale_color_manual( values = gearcols, labels = counts$n_labels)+
  scale_color_hue(labels = counts$n_labels)+
  labs(color = "Gear Type", x = "Fork Length (cm)", title = paste0(county,  "\n (N = ", sum(counts$n), ")"))+
  # facet_wrap(~ISLAND,ncol=1) +
  # theme_minimal()
  theme(legend.title = element_text(size=14), 
        legend.text = element_text(size=12))+
  geom_vline(data=mudv, aes(xintercept=grp.mean, color=LAND_STANDARD_GEAR_NAME),
             linetype="dashed")
abc5 = dv


### Traps ####
trap <- length_data_final[length_data_final$gear_short=='TR',]
counts = trap %>% group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
  tabyl(LAND_STANDARD_GEAR_NAME) %>%
  mutate(n_labels = paste0(LAND_STANDARD_GEAR_NAME, " (n= ", n, ")" ))

trap %>% group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
  ggplot(aes(FL_CM, color = LAND_STANDARD_GEAR_NAME))+
  geom_density(size = 0.75)+
  # scale_color_manual( values = gearcols, labels = counts$n_labels)+
  scale_color_hue(labels = counts$n_labels)+
  labs(color = "Gear Type", x = "Fork Length (cm)", title = paste0(county,  "\n (N = ", sum(counts$n), ")"))+
  # facet_wrap(~ISLAND,ncol=1) +
  # theme_minimal()
  theme(legend.title = element_text(size=14),
        legend.text = element_text(size=12))

### Nets ####
net <- length_data_final[length_data_final$gear_short=='NT',]
counts = net %>% group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
  tabyl(LAND_STANDARD_GEAR_NAME) %>%
  mutate(n_labels = paste0(LAND_STANDARD_GEAR_NAME, " (n= ", n, ")" ))

net %>% group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
  ggplot(aes(FL_CM,color = LAND_STANDARD_GEAR_NAME))+
  geom_density(size = 0.75)+
  # scale_color_manual( values = gearcols, labels = counts$n_labels)+
  scale_color_hue(labels = counts$n_labels)+
  labs(color = "Gear Type", x = "Fork Length (cm)", title = paste0(county,  "\n (N = ", sum(counts$n), ")"))+
  # facet_wrap(~ISLAND,ncol=1) +
  # theme_minimal()
  theme(legend.title = element_text(size=14), 
        legend.text = element_text(size=12))


net_LANDGEAR <- length_data_final[length_data_final$gear_short=='NT',]
counts = net_LANDGEAR %>% group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
  tabyl(LAND_GEAR_NAME) %>%
  mutate(n_labels = paste0(LAND_GEAR_NAME, " (n= ", n, ")" ))

net_LANDGEAR %>% group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
  ggplot(aes(FL_CM,color = LAND_GEAR_NAME))+
  geom_density(size = 0.75)+
  # scale_color_manual( values = gearcols, labels = counts$n_labels)+
  scale_color_hue(labels = counts$n_labels)+
  labs(color = "Gear Type", x = "Fork Length (cm)", title = paste0(county,  "\n (N = ", sum(counts$n), ")"))+
  # facet_wrap(~ISLAND,ncol=1) +
  # theme_minimal()
  theme(legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

### Other ####
ot<- length_data_final[length_data_final$gear_short=='OT',]
counts = ot %>% group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
  tabyl(LAND_STANDARD_GEAR_NAME) %>%
  mutate(n_labels = paste0(LAND_STANDARD_GEAR_NAME, " (n= ", n, ")" ))

ot %>% group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
  ggplot(aes(FL_CM, color = LAND_STANDARD_GEAR_NAME))+
  geom_density(size = 0.75)+
  # scale_color_manual( values = gearcols, labels = counts$n_labels)+
  scale_color_hue(labels = counts$n_labels)+
  labs(color = "Gear Type", x = "Fork Length (cm)", title = paste0(county,  "\n (N = ", sum(counts$n), ")"))+
  # facet_wrap(~ISLAND,ncol=1) +
  # theme_minimal()
  theme(legend.title = element_text(size=14), 
        legend.text = element_text(size=12))


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



DV <- length_data_final[length_data_final$gear_short=='DV',]
counts = DV %>% group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
  tabyl(LAND_STANDARD_GEAR_NAME) %>%
  mutate(n_labels = paste0(LAND_STANDARD_GEAR_NAME, " (n= ", n, ")" ))

DV %>%  group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
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


## Annual Density plots ####

# This will be split by final gear aggregations once these are decided, removes "other" gear types.


fleet_final <- length_data_final[length_data_final$fleet==1,]

fcounts = fleet_final %>%  group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
  tabyl(gear) %>%
  mutate(n_labels = paste0(gear, " (n= ", n, ")" ))

all_car <-
  fleet_final %>%  group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
  group_by(YEAR) %>%
  mutate(year_labs = paste0(YEAR, "\n n = ", n())) %>%
  ggplot(aes(FL_CM, color = gear))+
  geom_density(size = 0.75)+
  #scale_color_manual(values = gearcols, labels = counts$n_labels)+
  scale_color_hue(labels=fcounts$n_labels)+
  labs(color = "Gear Type", x = "Fork Length (cm)", title = paste0(county,  "\n (N = ", sum(fcounts$n), ")"))+
  facet_wrap(~year_labs)+
  # theme_minimal()
  theme(legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

all_car
export_fig_page(all_car)


# table(length_data_final$ISLAND)

SX <- length_data_final %>%
  filter(ISLAND=='ST CROIX')

vessel_sx <- distinct(SX, VESSEL_ID, LICENSE)  ##multiple vessels per license commonly

final_gears <- unique(SX$gear_short)  ##++this is where final length comps are generated ; could be aggregated above (e.g. via lookup table) & should be 1 here 

SX_comps <- list()  ##++this creates an empty list for for loop

# for loop generates binned length comps for each of the final gears. Stored in list. RUN:  View(comps[[1]]) to see in R
# view(SX_comps[[1]])

for(i in 1:length(final_gears)){
  SX_comps[[i]] <-  SX %>%
    filter(gear_short == final_gears[i]) %>%
    group_by(YEAR) %>%
    mutate(ln_fish = n(),
           ln_trips = length(unique(ID)),
           ln_dealers=length(unique(DEALER_CODE)),
           ln_vessels=length(unique(VESSEL_ID))) %>%
    ungroup() %>%
    group_by(YEAR, ln_fish, ln_trips, ln_dealers, ln_vessels, lbin)%>%
    summarise(freq = n() / unique(ln_fish)) %>%
    ungroup() %>%
    pivot_wider(id_cols = c(YEAR, ln_fish, ln_trips, ln_dealers, ln_vessels), 
                names_from = lbin, 
                values_from = freq, 
                values_fill = list(freq = 0)) %>%
    left_join(full_set) %>%
    select(comp_names)%>%
    replace(is.na(.), 0) %>%
    mutate(flag_n  = ifelse(ln_fish > 30, 0, 
                            ifelse(ln_fish < 15, 2, 1)))
  
  names(SX_comps)[[i]] <- paste0(final_gears[i], "_", bin_size, "cm") ##++tabs are named here ; could add _nom here if desired (instead of _lfd and _lfdw in main file)
  
}


# Stacked bar charts ####

#### Visual representation of gear distribution within groupings

# 
# counts =length_data_final %>%
#   tabyl(LAND_STANDARD_GEAR_NAME) %>%
#   mutate(n_labels = paste0(LAND_STANDARD_GEAR_NAME, " (n= ", n, ")" ))


length_data_final %>%  group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>% 
  ggplot(aes(fill = LAND_STANDARD_GEAR_NAME, y = FL_CM, x= gear))+
  geom_col(position = "fill", stat="identity")+
  scale_color_hue(labels=counts$n_labels)+
  labs(color = "Gear Type", x = "Fork Length (cm)", title = paste0(county,  "\n (N = ", sum(ycounts$n), ")"))+
  theme_minimal()+
  theme(legend.text = element_text(size = 7))
# guides(color = guide_legend(override.aes = list(size = 0.5))) 


# Gear distribution across time by gear groupings ####

## All gears over time ####

# create count of observed records by individual gear name

stx_count <- length_data_final %>% group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
  add_count(COUNTY) %>%
  mutate(COUNTYn = paste0(COUNTY, ' (', n, ')')) 

stx_gear_time <- subset(stx_count, select = -n) %>% 
  add_count(LAND_STANDARD_GEAR_NAME) %>%
  mutate(LAND_STANDARD_GEAR_NAMEn = paste0(LAND_STANDARD_GEAR_NAME, '(', n, ')')) 

# plot    

ggplot(data = stx_gear_time, aes(x = FINAL_DATE, y = LAND_STANDARD_GEAR_NAMEn , group = COUNTY ,color = COUNTYn )) + 
  facet_wrap(vars(COUNTY), ncol = 1) +
  geom_point(size = 2) + 
  labs(x = "Year", y = "STANDARDGEARNAME_1 (# obs)", 
       title = "Area-time distribution of landed gear STTJ", 
       color = "COUNTY (# obs)",
       subtitle = paste("N = ", nrow(stx_gear_time))) 
# theme(legend.position = "none")

## Diving Gear Distribution Over Time by Year ####


dv2<- length_data_final[length_data_final$gear_short=='DV',]
dv4<- aggregate(dv2$FL_CM, list(dv2$YEAR, dv2$LAND_STANDARD_GEAR_NAME), mean)
colnames(dv4) <- c('Year', 'LAND_STANDARD_GEAR_NAME', 'yearlyValue')
counts = dv2 %>%
  tabyl(LAND_STANDARD_GEAR_NAME) %>%
  mutate(n_labels = paste0(LAND_STANDARD_GEAR_NAME, " (n= ", n, ")" ))


dv4 %>%
  ggplot(aes(x = Year, y = yearlyValue, color = LAND_STANDARD_GEAR_NAME, group = 1))+
  geom_point(size = 2) +
  # scale_color_manual( values = gearcols, labels = counts$n_labels)+
  scale_color_hue(labels = counts$n_labels)+
  labs(color = "Gear Type", y = "Fork Length (cm)", x= "Year", title = paste0(county,  "\n (N = ", sum(counts$n), ")"))+
  # facet_wrap(~ISLAND,ncol=1) +
  # theme_minimal()
  theme(legend.title = element_text(size=14), 
        legend.text = element_text(size=12))


# display yearly values of gears in same group on same graph

# summary_hl2 <- hl2 %>% 
#     group_by(YEAR) %>% 
#     summarise(lower = min(FL_CM), upper = max(FL_CM), p = median(FL_CM))
# 
# ggplot(data = summary_hl2, mapping = aes(x = YEAR, y = p)) +
#     geom_pointrange(mapping = aes(ymin = lower, ymax = upper)) # doesn't separate by gear, can we do that? 

## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
# summarySE <- function(data=hl2, measurevar, groupvars=c("gear", "YEAR"), na.rm=FALSE,
#                       conf.interval=.95, .drop=TRUE) {
#     library(plyr)
# 
#     # New version of length which can handle NA's: if na.rm==T, don't count them
#     length2 <- function (x, na.rm=FALSE) {
#         if (na.rm) sum(!is.na(x))
#         else       length(x)
#     }
# 
#     # This does the summary. For each group's data frame, return a vector with
#     # N, mean, and sd
#     datac <- ddply(data, groupvars, .drop=.drop,
#       .fun = function(xx, col) {
#         c(N    = length2(xx[[col]], na.rm=na.rm),
#           mean = mean   (xx[[col]], na.rm=na.rm),
#           sd   = sd     (xx[[col]], na.rm=na.rm)
#         )
#       },
#       measurevar
#     )
# 
#     # Rename the "mean" column    
#     datac <- rename(datac, c("mean" = measurevar))
# 
#     datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
# 
#     # Confidence interval multiplier for standard error
#     # Calculate t-statistic for confidence interval: 
#     # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
#     ciMult <- qt(conf.interval/2 + .5, datac$N-1)
#     datac$ci <- datac$se * ciMult
# 
#     return(datac)
# }
# 
# hl2sum <- summarySE(data=hl2, measurevar="FL_CM", groupvars=c("gear","year"), na.rm=FALSE, conf.interval=.95)
# hl2sum <- summarySE(hl2, measurevar="FL_CM", groupvars=c("gear","year"))
# 
# ggplot(hl2, aes(x=YEAR, y=FL_CM, colour=gear)) + 
#     geom_errorbar(aes(ymin=FL_CM-se, ymax=FL_CM+se), width=.1) +
#     geom_line() +
#     geom_point()
# 
# ggplot(hl2, aes(x=YEAR, y=FL_CM, colour=gear, group=gear)) + 
#     geom_errorbar(aes(ymin=FL_CM-se, ymax=FL_CM+se), colour="black", width=.1, position=pd) +
#     geom_line(position=pd) +
#     geom_point(position=pd, size=3, shape=21, fill="white") + # 21 is filled circle
#     xlab("Year") +
#     ylab("Fork Length (cm)") +
#     scale_colour_hue(name="Gear type",    # Legend label, use darker colors
#                      breaks=gear,
#                      labels=gear,
#                      l=40) +                    # Use darker colors, lightness=40
#     ggtitle("Average ") +
#     expand_limits(y=0) +                        # Expand y range
#     scale_y_continuous(breaks=0:20*4) +         # Set tick every 4
#     theme_bw() +
#     theme(legend.justification=c(1,0),
#           legend.position=c(1,0))               # Position legend in bottom right


## Trap Gear Distribution Over Time by Year ####

trap <- length_data_final[length_data_final$gear_short=='TR',]
trap2<- aggregate(trap$FL_CM, list(trap$YEAR, trap$LAND_STANDARD_GEAR_NAME), mean)
colnames(trap2) <- c('Year', 'LAND_STANDARD_GEAR_NAME', 'yearlyValue')
counts = trap %>%
  tabyl(LAND_STANDARD_GEAR_NAME) %>%
  mutate(n_labels = paste0(LAND_STANDARD_GEAR_NAME, " (n= ", n, ")" ))

trap2 %>%
  ggplot(aes(x = Year, y = yearlyValue, color = LAND_STANDARD_GEAR_NAME, group = 1))+
  geom_point(size = 2) +
  # scale_color_manual( values = gearcols, labels = counts$n_labels)+
  scale_color_hue(labels = counts$n_labels)+
  labs(color = "Gear Type", y = "Fork Length (cm)", x= "Year", title = paste0(county,  "\n (N = ", sum(counts$n), ")"))+
  # facet_wrap(~ISLAND,ncol=1) +
  # theme_minimal()
  theme(legend.title = element_text(size=14), 
        legend.text = element_text(size=12))


## Net Gear Distribution Over Time by Year ####

net <- length_data_final[length_data_final$gear_short=='NT',]
net2<- aggregate(net$FL_CM, list(net$YEAR, net$LAND_STANDARD_GEAR_NAME), mean)
colnames(net2) <- c('Year', 'LAND_STANDARD_GEAR_NAME', 'yearlyValue')
counts = net %>%
  tabyl(LAND_STANDARD_GEAR_NAME) %>%
  mutate(n_labels = paste0(LAND_STANDARD_GEAR_NAME, " (n= ", n, ")" ))

net2 %>%
  ggplot(aes(x = Year, y = yearlyValue, color = LAND_STANDARD_GEAR_NAME, group = 1))+
  geom_point(size = 2) +
  # scale_color_manual( values = gearcols, labels = counts$n_labels)+
  scale_color_hue(labels = counts$n_labels)+
  labs(color = "Gear Type", y = "Fork Length (cm)", x= "Year", title = paste0(county,  "\n (N = ", sum(counts$n), ")"))+
  # facet_wrap(~ISLAND,ncol=1) +
  # theme_minimal()
  theme(legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

## Other Gear Distribution Over Time by Year ####

ot<- length_data_final[length_data_final$gear_short=='OT',]
ot2<- aggregate(ot$FL_CM, list(ot$YEAR, ot$LAND_STANDARD_GEAR_NAME), mean)
colnames(ot2) <- c('Year', 'LAND_STANDARD_GEAR_NAME', 'yearlyValue')
counts = ot %>%
  tabyl(LAND_STANDARD_GEAR_NAME) %>%
  mutate(n_labels = paste0(LAND_STANDARD_GEAR_NAME, " (n= ", n, ")" ))

ot2 %>%
  ggplot(aes(x = Year, y = yearlyValue, color = LAND_STANDARD_GEAR_NAME, group = 1))+
  geom_point(size = 2) +
  # scale_color_manual( values = gearcols, labels = counts$n_labels)+
  scale_color_hue(labels = counts$n_labels)+
  labs(color = "Gear Type", y = "Fork Length (cm)", x= "Year", title = paste0(county,  "\n (N = ", sum(counts$n), ")"))+
  # facet_wrap(~ISLAND,ncol=1) +
  # theme_minimal()
  theme(legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

# Cummulative Density Plots ####

## Aggregated CDF ####

counts =length_data_final %>%
  tabyl(gear) %>%
  mutate(n_labels = paste0(gear, " (n= ", n, ")" ))

length_data_final %>%
  
  ggplot(aes(FL_CM, color = gear))+
  stat_ecdf()+
  # scale_color_manual(values = gearcols, labels = counts$n_labels)+
  scale_color_hue(labels = counts$n_labels)+
  labs(color = "Gear Type", x = "Fork Length (cm)", title = paste0(county,  "\n (N = ", sum(counts$n), ")"))+
  theme_minimal()


## Management periods\*  ####

# **This section is optional and may be empty if there are not signifcant changes**


length_data_final %>%
  
  ggplot(aes(FL_CM, color = gear))+
  stat_ecdf()+
  # scale_color_manual(values = gearcols, labels = counts$n_labels)+
  scale_color_hue(labels = counts$n_labels)+
  labs(color = "Gear Type", x = "Fork Length (cm)", title = paste0(county,  "\n (N = ", sum(counts$n), ")"))+
  facet_wrap(~mgt_period) +
  theme_minimal()


## Annual CDF ####

length_data_final %>% 
  group_by(YEAR) %>%
  mutate(year_labs = paste0(YEAR, "\n n = ", n())) %>%
  
  ggplot(aes(FL_CM, color = gear))+
  stat_ecdf()+
  # scale_color_manual(values = gearcols, labels = counts$n_labels)+
  scale_color_hue(labels = counts$n_labels)+
  labs(color = "Gear Type", x = "Fork Length (cm)", title = paste0(county,  "\n (N = ", sum(counts$n), ")"))+
  facet_wrap(~year_labs)+
  theme_minimal()

# Island Density Plots ####

## Aggregated density plots ####


srcounts =length_data_final %>%
  tabyl(ISLAND) %>%
  mutate(n_labels = paste0(ISLAND, " (n= ", n, ")" ))

#srcounts <- srcounts %>% drop_na(SUBREGION)

length_data_final %>%
  ggplot(aes(FL_CM))+
  #geom_density( aes(color = "Combined"))+
  geom_density(aes(color = ISLAND))+
  #scale_color_manual(values = regcols, labels = c("Combined", srcounts$n_labels))+
  scale_color_hue(labels=srcounts$n_labels)+
  labs(color = "Island", x = "Fork Length (cm)", title = paste0(county,  "\n (N = ", sum(srcounts$n), ")"))+
  theme_minimal()

## Management periods\* ####

# **This section is optional and may be empty if there are not significant changes**


length_data_final %>%
  
  ggplot(aes(FL_CM, color = ISLAND))+
  geom_density()+
  #scale_color_manual(values = regcols, labels = srcounts$n_labels)+
  scale_color_hue(labels = srcounts$n_labels)+
  labs(color = "Island", x = "Fork Length (cm)", title = paste0(county,  "\n (N = ", sum(srcounts$n), ")"))+
  facet_wrap(~mgt_period) +
  theme_minimal()


# Annual Density plots ####

length_data_final %>% 
  group_by(YEAR) %>%
  mutate(year_labs = paste0(YEAR, "\n n = ", n())) %>%
  
  ggplot(aes(FL_CM, color = ISLAND))+
  geom_density()+
  #  scale_color_manual(values = regcols, labels = srcounts$n_labels)+
  scale_color_hue(labels = srcounts$n_labels)+
  labs(color = "Subregion", x = "Fork Length (cm)", title = paste0(county,  "\n (N = ", sum(srcounts$n), ")"))+
  facet_wrap(~year_labs)+
  theme_minimal()


final_gears <- unique(length_data_final$gear_short)  ##++this is where final length comps are generated ; could be aggregated above (e.g. via lookup table) & should be 1 here 

comps <- list()  ##++this creates an empty list for for loop

# for loop generates binned length comps for each of the final gears. Stored in list. RUN:  View(comps[[1]]) to see in R

for(i in 1:length(final_gears)){
  comps[[i]] <-  length_data_final %>%
    filter(gear_short == final_gears[i]) %>%
    group_by(YEAR) %>%
    mutate(ln_fish = n(),
           ln_trips = length(unique(ID)),
           ln_dealers=length(unique(DEALER_CODE)),
           ln_vessels=length(unique(VESSEL_ID))) %>%
    ungroup() %>%
    group_by(YEAR, ln_fish, ln_trips, ln_dealers, ln_vessels, lbin)%>%
    summarise(freq = n() / unique(ln_fish)) %>%
    ungroup() %>%
    pivot_wider(id_cols = c(YEAR, ln_fish, ln_trips, ln_dealers, ln_vessels), 
                names_from = lbin, 
                values_from = freq, 
                values_fill = list(freq = 0)) %>%
    left_join(full_set) %>%
    select(comp_names)%>%
    replace(is.na(.), 0) %>%
    mutate(flag_n  = ifelse(ln_fish > 30, 0, 
                            ifelse(ln_fish < 15, 2, 1)))
  
  names(comps)[[i]] <- paste0(final_gears[i], "_", bin_size, "cm") ##++tabs are named here ; could add _nom here if desired (instead of _lfd and _lfdw in main file)
  
  
}
# 
# write.xlsx(comps, file = paste0("./outputs/", sp, "_com_lfd_", sprintf('%02d', min_year %% 100), sprintf('%02d', max_year %% 100), "_", gsub("-", "", Sys.Date()), ".xlsx"))

