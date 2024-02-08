# explore land vs effort standard gear

#run first blocks of pr_yt_2022_figures

# included effort and land standard gear name
length_data_final2 <- join_length_dat %>%
  select(YEAR, INTERVIEW_DATE, FINAL_DATE, ID, OBS_ID, STATE = STATE_LANDED,
         COUNTY=COUNTY_LANDED, COUNTY_CODE, FL_CM, LENGTH_TYPE1, LENGTH_UNIT1,
         OBS_WEIGHT_KG, OBS_WEIGHT_UNIT, lbin, source, LAND_STANDARD_GEAR_NAME,
         LAND_GEAR_NAME, GEARNAME_1, mgt_period, ISLAND, INT_TYPE, fleet, 
         DEALER_CODE, VESSEL_ID, LICENSE) %>%  #STAT_AREA
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
# if land_standard_gear_name is not coded, replace with gearname_1 (effort gear)
length_full_gear <- length_data_final2 |> 
  mutate(gear = case_when(LAND_STANDARD_GEAR_NAME == "NOT CODED" ~ GEARNAME_1, 
                               TRUE ~ LAND_STANDARD_GEAR_NAME))

# filter to needed variables 
length_full_gear_glm <- length_full_gear |>
  select(YEAR, FINAL_DATE, ID, COUNTY, FL_CM, OBS_WEIGHT_KG, OBS_WEIGHT_UNIT, gear, GEAR_GROUP, LENGTH_UNIT1) |> 
  mutate(ID = as.character(ID))

# create yearly counts by gear by lengths
gant_data2 <- length_full_gear_glm %>% 
  group_by(gear) %>% 
  dplyr::mutate(n_ID = n_distinct(ID)) |> 
  dplyr::filter(n_ID >= 3) %>% ungroup %>%
  group_by(YEAR, gear) |>
  dplyr::summarize(n = n(), .groups = "drop") |> 
  mutate(YEAR = as.integer(YEAR))

# create graph to visualize data by year and gear
abc111 <- gant_data2 |>
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
        title = element_text(size = 15))

# create yearly counts of gear by IDs
gant_data_id2 <- length_full_gear_glm %>% 
  group_by(gear) %>% 
  dplyr::mutate(n_ID = n_distinct(ID)) |> 
  dplyr::filter(n_ID >= 3) %>% ungroup %>%
  group_by(YEAR, gear) |>
  dplyr::summarize(n_ID = n_distinct(ID), .groups = "drop") |> 
  mutate(YEAR = as.integer(YEAR))

# create graph to visualize yearly counts by ID
abc211 <- gant_data_id2 |>
  group_by(gear) |>
  dplyr::mutate(total_n = sum(n_ID)) |> 
  ungroup() |>   
  dplyr::mutate(gear = fct_reorder(gear, total_n)) %>%
  ggplot(aes(x = YEAR, y = gear, color = gear, size = n_ID)) +
  geom_point()  +
  labs(x = "Year", y = "", colour = "", shape = "", 
       title = paste(county, "Interviews"),
       caption = disclaimer) +
  theme_bw() + 
  theme(legend.position="null", text = element_text(size = 20), 
        title = element_text(size = 15))



# how do the number of weight units compare over time? 
# count per weight unit per year
weight_data <- length_full_gear_glm %>% 
  group_by(OBS_WEIGHT_UNIT) %>% 
  dplyr::mutate(n_ID = n_distinct(ID)) |> 
  dplyr::filter(n_ID >= 3) %>% ungroup %>%
  group_by(YEAR, OBS_WEIGHT_UNIT) |>
  dplyr::summarize(n = n(), .groups = "drop") |> 
  mutate(YEAR = as.integer(YEAR))
# graph 
weight_units <- weight_data |> 
  ggplot(aes(fill = OBS_WEIGHT_UNIT, x = YEAR, y = n))+
  geom_bar(stat = "identity", color = "black")+
  labs(x = "Year", y = "# Of Records", title = county) +
  theme_bw()+ 
  theme(legend.position="bottom", text = element_text(size = 20),
        title = element_text(size = 15))
 
# how do the number of length units compare over time
# count per length unit per year
lengthunit_data <- length_full_gear_glm %>% 
  group_by(LENGTH_UNIT1) %>% 
  dplyr::mutate(n_ID = n_distinct(ID)) |> 
  dplyr::filter(n_ID >= 3) %>% ungroup %>%
  group_by(YEAR, LENGTH_UNIT1) |>
  dplyr::summarize(n = n(), .groups = "drop") |> 
  mutate(YEAR = as.integer(YEAR))
# graph 
length_units <- lengthunit_data |> 
  ggplot(aes(fill = LENGTH_UNIT1, x = YEAR, y = n))+
  geom_bar(stat = "identity", color = "black")+
  labs(x = "Year", y = "# Of Records", title = county) +
  theme_bw()+ 
  theme(legend.position="bottom", text = element_text(size = 20),
        title = element_text(size = 15))


colnames(pr_yt)

table(pr_yt$YEAR, pr_yt$OBS_WEIGHT_UNIT)


table(pr_yt$YEAR, pr_yt$LENGTH_TYPE1)


table(pr_yt$COUNTY_LANDED)
