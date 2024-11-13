# TIP DATA FILTERING FOR SEDAR 57U
# CREATED BY ADYAN RIOS
# LAST EDITED BY ADYAN RIOS 8AUG2022

# Load libraries ####
librarian::shelf(here, tidyverse)

# SPECIFY INPUT FILE NAMES ----
# inData1  <- list.files(path = "Input/", pattern = "I1")
inData2  <- list.files(path = here("data", "sedar_57u_cr_lob_2021"), pattern = "I2")
inData3  <- list.files(path = here("data", "sedar_57u_cr_lob_2021"), pattern = "I3")
inData4  <- list.files(path = here("data", "sedar_57u_cr_lob_2021"), pattern = "I4")
inData5  <- list.files(path = here("data", "sedar_57u_cr_lob_2021"), pattern = "I5")

# SPECIFY OUTPUT FILE NAMES ----
outData1a <- here("data", "sedar_57u_cr_lob_2021", paste0("O1a_TIP_CONF_TABLES_",  format(Sys.time(),'%Y%m%d')))
outData2 <- here("data", "sedar_57u_cr_lob_2021", paste0("O2_TIP_SL57U_",  format(Sys.time(),'%Y%m%d')))

# READ IN DATA ----
# tipDat    <- read.csv(paste0("Input/",inData1),   stringsAsFactors = FALSE)
tipDat    <- readRDS(file = here("data", "raw", "com_tip_PR_VI_97648_97646_20220830.RDS"))
prMerge   <- read.csv(here("data", "sedar_57u_cr_lob_2021", inData2),   stringsAsFactors = FALSE)
viMerge   <- read.csv(here("data", "sedar_57u_cr_lob_2021", inData3),   stringsAsFactors = FALSE)
removeLW  <- read.csv(here("data", "sedar_57u_cr_lob_2021", inData4),   stringsAsFactors = FALSE)
gearMerge <- read.csv(here("data", "sedar_57u_cr_lob_2021", inData5),   stringsAsFactors = FALSE)

gearMerge <- gearMerge %>% mutate_all(na_if, "")

# CREATE VARIABLE FOR ISLAND ----
tipDat <- tipDat %>%
  mutate(ISL = case_when(STATE_LANDED == "PUERTO RICO" ~ "PR", 
                         STATE_LANDED == "VIRGIN ISLANDS" & COUNTY_LANDED %in% c("ST JOHN", "ST THOMAS") ~ "STT", 
                         STATE_LANDED == "VIRGIN ISLANDS" & COUNTY_LANDED == "ST CROIX" ~ "STX", 
                         TRUE ~ "NOT CODED"))

# FILTER TO JUST SPINY LOBSTER CARAPCE LENGTHS IN COMPLETE YEARS FOR CODED ISLANDS ----
tipSL <- tipDat %>%
  filter(as.numeric(OBS_STANDARD_SPECIES_CODE) %in% c(97648, 97646),
         LENGTH_TYPE1 == "CARAPACE LENGTH",
         !is.na(LENGTH1_MM),
         ISL != "NOT CODED",
         # !(ISL == "PR" & YEAR >= 2017),
         # YEAR <= 2017,
         YEAR <= 2021)

#CLEAN UP MEMORY----
# rm(tipDat)

# MERGE IN COAST ----
tipSL_coast <- tipSL %>%
  left_join(prMerge, by = c("STATE_LANDED", "COUNTY_LANDED")) %>%
  left_join(viMerge, by = c("STATE_LANDED", "AREANAME_1" = "AREANAME")) %>%
  rename(VI_COAST1 = VI_COAST) %>%
  left_join(viMerge, by = c("STATE_LANDED", "AREANAME_2" = "AREANAME")) %>%
  rename(VI_COAST2 = VI_COAST) %>%
  mutate(VI_COASTuse = ifelse(VI_COAST2 == VI_COAST1 | AREANAME_2 == "", VI_COAST1, "OTHER")) %>%
  left_join(viMerge, by = c("STATE_LANDED", "AREANAME_3" = "AREANAME")) %>%
  rename(VI_COAST3 = VI_COAST) %>%
  mutate(VI_COASTuse = ifelse(VI_COAST3 == VI_COASTuse | AREANAME_3 == "", VI_COAST1, "OTHER")) %>%
  left_join(viMerge, by = c("STATE_LANDED", "AREANAME_4" = "AREANAME")) %>%
  rename(VI_COAST4 = VI_COAST) %>%
  mutate(VI_COASTuse = ifelse(VI_COAST4 == VI_COASTuse | AREANAME_4 == "", VI_COAST1, "OTHER")) %>%
  mutate(COAST = case_when(STATE_LANDED == "PUERTO RICO" ~ PR_COAST,
                            STATE_LANDED == "VIRGIN ISLANDS" ~ VI_COASTuse))

# MERGE IN GEARS----
tipSL_coastGear <- tipSL_coast %>%
  mutate(STANDARDGEARNAME_1 = case_when(STANDARDGEARNAME_1 != "NOT CODED" ~ STANDARDGEARNAME_1),
         STANDARDGEARNAME_2 = case_when(STANDARDGEARNAME_2 != "NOT CODED" ~ STANDARDGEARNAME_2)) %>%
  left_join(., gearMerge, by = c( "ISL", "STANDARDGEARNAME_1", "STANDARDGEARNAME_2"))

# NEW GEAR TYPE
new_gears = tipSL_coastGear %>% select(ISL, YEAR, STANDARDGEARNAME_1, STANDARDGEARNAME_2, GEAR_TYPE) %>% distinct()
short_list = filter(new_gears, is.na(GEAR_TYPE))


# LOOK INTO CONFIDENTIALITY OF TRIPS----
tipSL_conf <- tipSL_coastGear %>%
  group_by(GEAR_TYPE,ISL,YEAR) %>%
  summarize(unique_id = length(unique(ID)),
            unique_county = length(unique(COUNTY_LANDED)),
            unique_dealer = length(unique(DEALER)),
            unique_dealer_code = length(unique(DEALER_CODE)),
            unique_license = length(unique(LICENSE)),
            unique_vessel = length(unique(VESSEL_ID)),
            max = max(unique_county, unique_dealer, unique_dealer_code, unique_license, unique_vessel)) %>%
  data.frame()

# SAVE SUMMARY OF CONFIDENTIALITY AND IDENTIFY YEAR GROUPS AS NECESSARY IN EXCEL PIVOT----
write.csv(tipSL_conf, paste0(outData1a, ".csv"), row.names = FALSE)

# PREPARE TIP FOR NON-CONFIDENTIAL PLOTS----
tipSL_nonConf <- tipSL_coastGear %>%
  mutate(NEW_YEAR = case_when(ISL == "PR" & GEAR_TYPE == "Diving" & YEAR %in% c(1987:1988) ~ "1987-1988",
                              ISL == "PR" & GEAR_TYPE == "Diving" & YEAR %in% c(1996:1997) ~ "1996-1997",
                              ISL == "PR" & GEAR_TYPE == "Pots and Traps" & YEAR %in% c(1988:1989) ~ "1988-1989",
                              ISL == "STT" & GEAR_TYPE == "Pots and Traps" & YEAR %in% c(1980:1983) ~ "1980-1983",
                              ISL == "STT" & GEAR_TYPE == "Pots and Traps" & YEAR %in% c(1993:1995) ~ "1993-1995",
                              ISL == "STT" & GEAR_TYPE == "Pots and Traps" & YEAR %in% c(2008:2009) ~ "2008-2009",
                              ISL == "STX" & GEAR_TYPE == "Diving" & YEAR %in% c(1981:1983) ~ "1981-1983",
                              ISL == "STX" & GEAR_TYPE == "Diving" & YEAR %in% c(1985:1987) ~ "1985-1987",
                              ISL == "STX" & GEAR_TYPE == "Diving" & YEAR %in% c(1995:1996) ~ "1995-1996",
                              ISL == "STX" & GEAR_TYPE == "Diving" & YEAR %in% c(2010:2011) ~ "2010-2011",
                              ISL == "STX" & GEAR_TYPE == "Pots and Traps" & YEAR %in% c(1981:1983) ~ "1981-1983",
                              ISL == "STX" & GEAR_TYPE == "Pots and Traps" & YEAR %in% c(1987:1990) ~ "1987-1990",
                              ISL == "STX" & GEAR_TYPE == "Pots and Traps" & YEAR %in% c(2000:2006) ~ "2000-2006",
                              ISL == "STX" & GEAR_TYPE == "Pots and Traps" & YEAR %in% c(2008:2009) ~ "2008-2009",
                              ISL == "STX" & GEAR_TYPE == "Pots and Traps" & YEAR %in% c(2010:2017) ~ "2010-2017",
                              TRUE ~ as.character(YEAR)))

# QUICK VIEW OF RAW DATA----
tipSL_nonConf %>%
  filter(OBS_WEIGHT_KG>0) %>% 
  ggplot(aes(x = LENGTH1_MM, y = OBS_WEIGHT_KG)) +
  geom_point() +
  facet_wrap(~ISL) + 
  ggtitle("Raw data by Island (OBS_WEIGHT_KG > 0)")

table(tipSL_nonConf$CONDITION_TYPE, tipSL_nonConf$ISL)
table(tipSL_nonConf$SEX_NAME, tipSL_nonConf$ISL)

# FLAG LENGTH MEASURMENT ERRORS----
tipSL_flag <- tipSL_nonConf %>%
  mutate(REMOVE = case_when(
    LENGTH1_MM > 250 ~ TRUE,
    ISL == "STX" & CONDITION_TYPE == "HARD SHELLS,LOBSTER" ~ TRUE,
    ISL == "PR" & SEX == "U" & CONDITION_TYPE == "ROUND (WHOLE)" & LENGTH1_MM > 200 & OBS_WEIGHT_KG > 0 ~ TRUE,
    TRUE ~ FALSE))

# QUICK VIEW OF FLAGGED DATA----
tipSL_flag %>%
  filter(OBS_WEIGHT_KG>0) %>% 
  ggplot(aes(x = LENGTH1_MM, y = OBS_WEIGHT_KG, color = REMOVE)) +
  geom_point() +
  facet_grid(~ISL) + 
  ggtitle("Flagged data by Island (OBS_WEIGHT_KG > 0)")

# QUICK TABLE OF FLAGGED DATA----
table(tipSL_flag$ISL, tipSL_flag$REMOVE == TRUE)
table(tipSL_flag$ISL, tipSL_flag$REMOVE == TRUE & tipSL_flag$GEAR_TYPE == "Diving")
table(tipSL_flag$ISL, tipSL_flag$REMOVE == TRUE & tipSL_flag$GEAR_TYPE == "Pots and Traps")

# A FEW MORE ISSUES IN PR TO CONSIDER
tipSL_flag %>%
  filter(OBS_WEIGHT_KG>0, ISL == "PR", LENGTH1_MM < 400, OBS_WEIGHT_KG < 5) %>%
  mutate(YEAR = factor(YEAR)) %>%
  ggplot(aes(x = LENGTH1_MM, y = OBS_WEIGHT_KG, shape = REMOVE, color = YEAR)) +
  geom_point() +
  ggtitle("Flagged data in PR (OBS_WEIGHT_KG > 0)\nSome additional data clusters of concern,\nbut not questionable enough to warrent further removals")

# REMOVE LENGTH MEASURMENT ERRORS----
tipSL_keep <- tipSL_flag %>%
  filter(REMOVE == FALSE)

# HOW MANY ENTRIES WERE REMOVED?----
length(tipSL_flag$LENGTH1_MM) - length(tipSL_keep$LENGTH1_MM) #322 (now 403)

# QUICK VIEW OF CLEAN DATA----
tipSL_keep %>%
  mutate(OBS_WEIGHT_KG = ifelse(is.na(OBS_WEIGHT_KG), 0, OBS_WEIGHT_KG)) %>% 
  ggplot(aes(x = LENGTH1_MM, y = OBS_WEIGHT_KG)) +
  geom_point() +
  facet_grid(ISL~GEAR_TYPE) +
  ggtitle("Retained Data")

# STILL SOME MEASUREMENT ERRORS IN PR 2008
#q999 <- quantile(tipSL_keep$LENGTH1_MM, .999) 
q999 <- 190
q999 #190 (now 184)
table(tipSL_keep$LENGTH1_MM > q999, tipSL_keep$ISL)
tipSL_drop <- filter(tipSL_keep, LENGTH1_MM > q999)
table(tipSL_drop$ISL, tipSL_drop$YEAR)
table(tipSL_drop$ISL, tipSL_drop$YEAR)/length(tipSL_drop$YEAR)
table(tipSL_drop$ISL, tipSL_drop$GEAR_TYPE)
# 50% OF THE TOP 15 OF MEASUREMENTS COME FROM 2008 IN PR
sum(tipSL_drop$LENGTH1_MM > q999 & tipSL_drop$ISL == "PR" & tipSL_drop$YEAR == 2008)/sum(tipSL_keep$ISL == "PR" & tipSL_keep$YEAR == 2008)
# CAUTION IN ASSESSMENT REMOVE PR 2008 ABOVE 190 (44 entires, 4%)

# QUANTITY ERRRORS
table(tipSL_flag$ISL, tipSL_flag$QUANTITY > 1 & tipSL_flag$REMOVE == FALSE)
table(tipSL_flag$REMOVE == FALSE, tipSL_flag$QUANTITY)
table(tipSL_flag$YEAR, tipSL_flag$QUANTITY)
table(tipSL_flag$ISL == "PR" & tipSL_flag$REMOVE == FALSE, tipSL_flag$QUANTITY)
table(tipSL_flag$ISL == "STT" & tipSL_flag$REMOVE == FALSE, tipSL_flag$QUANTITY)
table(tipSL_flag$ISL == "PR" & tipSL_flag$QUANTITY > 1 & tipSL_flag$REMOVE == FALSE, tipSL_flag$YEAR)
table(tipSL_flag$ISL == "STT" & tipSL_flag$QUANTITY > 1 & tipSL_flag$REMOVE == FALSE, tipSL_flag$YEAR)

# EXPORT DATA----
write.csv(tipSL_keep, paste0(outData2, ".csv"), row.names = FALSE)

# GUIDANCE USING THESE DATA
# 1. SUM OVER QUANTITY TO GET SAMPLE SIZES (AND TO USE THAT VARIABLE AS INTENDED)
# 2. KEEP ONLY LENGTH GREATER THAN OR EQUAL TO 88.9 MM
# 3. EXCLUDE PR 2008
# 4. EXCLUDE STT 1988

tipSL_keep %>%
  filter(!(ISL == "PR" & YEAR == 2008),
         !(ISL == "STT" & YEAR == 1988),
         GEAR_TYPE != "Other",
         !(ISL == "STT" & GEAR_TYPE == "Diving"),
         !(ISL == "STX" & GEAR_TYPE == "Pots and Traps"),
         LENGTH1_MM >= 88.9) %>%
  mutate(OBS_WEIGHT_KG = ifelse(is.na(OBS_WEIGHT_KG), 0, OBS_WEIGHT_KG)) %>% 
  ggplot(aes(x = LENGTH1_MM, y = OBS_WEIGHT_KG)) +
  geom_point() +
  facet_wrap(ISL~GEAR_TYPE) +
  ggtitle("Recommended Data
1. SUM OVER QUANTITY TO GET SAMPLE SIZES (AND TO USE THAT VARIABLE AS INTENDED)
2. KEEP ONLY LENGTH GREATER THAN OR EQUAL TO 88.9 MM
3. EXCLUDE PR 2008
4. EXCLUDE STT 1988")

######################################################
# ADDITIONAL PLOTS/TABLES EXPLORING MEASUREMENT ERRORS

# ODD STX ENTRIES----
tipSL_flag %>%
  filter(ISL == "STX", OBS_WEIGHT_KG>0) %>% 
  ggplot(aes(x = LENGTH1_MM, y = OBS_WEIGHT_KG, color = REMOVE)) +
  geom_point()

tipSL_flag %>%
  filter(ISL == "STX", OBS_WEIGHT_KG>0) %>% 
  ggplot(aes(x = LENGTH1_MM, y = OBS_WEIGHT_KG, color = REMOVE)) +
  geom_point() +
  facet_grid(~CONDITION_TYPE)

# ODD PR ENTIRES OVERALL----
tipSL_flag %>%
  filter(ISL == "PR", SEX == "U", OBS_WEIGHT_KG>0, OBS_WEIGHT_KG < 7.5) %>% 
  ggplot(aes(x = LENGTH1_MM, y = OBS_WEIGHT_KG, color = REMOVE)) +
  geom_point() 

# ODD PR ENTIRES IN 1990 (NOT REMOVED)----
tipSL_flag %>%
  filter(ISL == "PR", OBS_WEIGHT_KG>0, YEAR == 1990) %>% 
  ggplot(aes(x = LENGTH1_MM, y = OBS_WEIGHT_KG, color = REMOVE)) +
  geom_point() 

# ODD PR ENTRIES IN 2008
tipSL_keep %>%
  filter(ISL == "PR", YEAR %in% c(2008)) %>% 
  mutate(YEAR = factor(YEAR), OBS_WEIGHT_KG = ifelse(is.na(OBS_WEIGHT_KG), 0, OBS_WEIGHT_KG)) %>%
  ggplot(aes(x = LENGTH1_MM)) +
  geom_histogram() +
  facet_wrap(~YEAR)

#########################################################
#TABLES SHOWING THE NUMBER OF RECORDS FOR EACH ERROR TYPE

# UNSUALLY LARGE ENTIRES----
length(filter(tipSL_flag, LENGTH1_MM > 250)$ID) #308

# ODD STX ENTRIES----
length(filter(tipSL_flag, LENGTH1_MM <= 250 & 
                ISL == "STX" & 
                CONDITION_TYPE == "HARD SHELLS,LOBSTER")$ID) #9

# ODD PR ENTIRES----
length(filter(tipSL_flag, LENGTH1_MM <= 250 & 
                ISL == "PR" &
                SEX == "U" & 
                CONDITION_TYPE == "ROUND (WHOLE)" & 
                LENGTH1_MM > 200 & 
                OBS_WEIGHT_KG > 0)$ID) #5

# ODD PR ENTIRES IN 1990 (NOT REMOVED)----
length(filter(tipSL_flag, LENGTH1_MM <= 250 & 
                ISL == "PR" & 
                tipSL$SEX == "U" & 
                CONDITION_TYPE == "ROUND (WHOLE)" & 
                LENGTH1_MM > 100 & 
                OBS_WEIGHT_KG < 0.25 &
                tipSL$YEAR == 1990)$ID) #17
