
# Verify confidentiality of interviews by gear_group year and island
# Assume that if three interviews occur in a given day for a given strata
# then the strata is not confidential (zero's count for confidentiality counts)
inData5  <- list.files(path = here("data", "sedar_57u_cr_lob_2021"), pattern = "I5")
gearMerge <- read.csv(here("data", "sedar_57u_cr_lob_2021", inData5),   stringsAsFactors = FALSE)
gearMerge <- gearMerge %>% mutate_all(na_if, "")

n_tip    <- readRDS(file = here("data", "raw", "n_com_tip_PR_VI_20220830.RDS"))
table(n_tip$YEAR, n_tip$LANDING_AREA_STATE_CODE)

n_tip <- n_tip %>%
  mutate(ISL = case_when(STATE_LANDED == "PUERTO RICO" ~ "PR", 
                         STATE_LANDED == "VIRGIN ISLANDS" & COUNTY_LANDED %in% c("ST JOHN", "ST THOMAS") ~ "STT", 
                         STATE_LANDED == "VIRGIN ISLANDS" & COUNTY_LANDED == "ST CROIX" ~ "STX", 
                         TRUE ~ "NOT CODED"),
         YMD = format(INTERVIEW_DATE,"%Y-%m-%d"))
table(n_tip$YEAR, n_tip$ISL)

# MERGE IN GEARS----
n_tip_gear <- n_tip %>%
  mutate(STANDARDGEARNAME_1 = case_when(STANDARDGEARNAME_1 != "NOT CODED" ~ STANDARDGEARNAME_1),
         STANDARDGEARNAME_2 = case_when(STANDARDGEARNAME_2 != "NOT CODED" ~ STANDARDGEARNAME_2)) %>%
  left_join(., gearMerge, by = c( "ISL", "STANDARDGEARNAME_1", "STANDARDGEARNAME_2"))
table(n_tip_gear$GEAR_TYPE, n_tip_gear$ISL)

# Merge in gear type
new_gears = n_tip_gear %>% 
  filter(ISL != "NOT CODED") %>%
  select(ISL, STANDARDGEARNAME_1, STANDARDGEARNAME_2, GEAR_TYPE) %>% 
  distinct()
short_list = filter(new_gears, is.na(GEAR_TYPE))

# Caution: not all gears are classified (only the ones associated with lobster)
# Ignore other gears for now

# Confidentiality
conf_tip = n_tip_gear %>%
  filter(ISL != "NOT CODED",
         GEAR_TYPE %in% c("Pots and Traps", "Diving")) %>%
  group_by(ISL, GEAR_TYPE, YMD) %>%
  mutate(n_MDY = n_distinct(ID)) %>%
  group_by(ISL, GEAR_TYPE, YEAR) %>%
  summarise(CONF_LT_3 = max(n_MDY) < 3) %>%
  mutate(CONF_LT_3 = as.numeric(CONF_LT_3))

n_tip = n_tip_gear %>%
  filter(ISL != "NOT CODED",
         GEAR_TYPE %in% c("Pots and Traps", "Diving")) %>%
  group_by(ISL, GEAR_TYPE, YEAR) %>%
  summarize(n = n_distinct(ID))

write.csv(conf_tip, 
          here("data", "sedar_57u_cr_lob_2021", paste0("com_tip_year_isl_confidential_", format(Sys.time(),'%Y%m%d'), ".csv")), 
          row.names = FALSE)

days_tip = n_tip_gear %>%
  filter(ISL != "NOT CODED",
         GEAR_TYPE %in% c("Pots and Traps", "Diving")) %>%
  group_by(ISL, GEAR_TYPE, YEAR) %>%
  summarize(n = n_distinct(YMD))

write.csv(n_tip, 
          here("data", "sedar_57u_cr_lob_2021", paste0("com_tip_year_isl_interviews_", format(Sys.time(),'%Y%m%d'), ".csv")), 
          row.names = FALSE)

write.csv(days_tip, 
          here("data", "sedar_57u_cr_lob_2021", paste0("com_tip_year_isl_days_", format(Sys.time(),'%Y%m%d'), ".csv")), 
          row.names = FALSE)
test <- n_tip_gear %>%
  filter(ISL != "NOT CODED",
         GEAR_TYPE %in% c("Pots and Traps", "Diving"))%>%
  filter(ISL == "PR",
         YEAR == 1980,
         GEAR_TYPE == "Diving")
unique(test$INTERVIEW_DATE)

test2 <- tipSL_coastGear %>%
  filter(ISL != "NOT CODED",
         GEAR_TYPE %in% c("Pots and Traps", "Diving"))%>%
  filter(ISL == "PR",
         YEAR == 1980,
         GEAR_TYPE == "Diving")
unique(test2$INTERVIEW_DATE)
unique(test2$VESSEL_ID)
unique(test2$VESSEL_NAME)
unique(test2$INTERVIEW_COMMENT)


test3 <- tipSL_coastGear %>%
  filter(ISL != "NOT CODED",
         GEAR_TYPE %in% c("Pots and Traps", "Diving"))%>%
  filter(ISL == "PR",
         YEAR == 2010,
         GEAR_TYPE == "Pots and Traps")
unique(test3$INTERVIEW_COMMENT)
unique(test3$VESSEL_ID)
unique(test3$VESSEL_NAME)
unique(test3$PR_COAST)


