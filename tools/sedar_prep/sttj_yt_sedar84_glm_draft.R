# Set up library  ####
librarian::shelf(here, tidyverse, ROracle, keyring, dotenv, reshape, openxlsx, janitor, DT, pander, knitr, ggpubr, lmerTest)


# Read in data ####

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

# Read in initial gear groupings
TIP_gears <- read_csv(here("data", "CSVs", "tip_gears_yts_sttj_landstdgearname.csv"), show_col_types = FALSE)

# Filter and format data ####

# Filter to STTJ and Yellowtail Snapper
sttj_yt <- tip |>
  filter(COUNTY_LANDED %in% c("ST THOMAS", "ST JOHN"),
         OBS_STANDARD_SPECIES_CODE == "168907",
         FISHING_MODE == 'COMMERCIAL',
         LENGTH1_MM > 0) |>
  left_join(TIP_gears, by = join_by(LAND_STANDARD_GEAR_NAME)) |> 
  mutate(TEST_DATE = as.Date(ymd_hms(INTERVIEW_DATE)),
         FINAL_DATE = case_when(is.na(TEST_DATE) ~ INTERVIEW_DATE, 
                                TRUE ~ TEST_DATE))%>%
  select(YEAR, FINAL_DATE, ID, COUNTY_LANDED, LENGTH1_MM, LAND_STANDARD_GEAR_NAME, gear) #%>%  #STAT_AREA
  # filter(between(FL_CM , min_size, max_size),
  #        ISLAND != 'NOT CODED')



# Summary Stats ####

# By gear name
land_gear_name_summary <- sttj_yt |>
  group_by(LAND_STANDARD_GEAR_NAME) %>%
  summarise(
    count = n(),
    mean = mean(LENGTH1_MM, na.rm = TRUE),
    sd = sd(LENGTH1_MM, na.rm = TRUE),
    median = median(LENGTH1_MM, na.rm = TRUE),
    IQR = IQR(LENGTH1_MM, na.rm = TRUE)
  )


# By gear group
gear_group_summary <- sttj_yt |>
  group_by(gear) %>%
  summarise(
    count = n(),
    mean = mean(LENGTH1_MM, na.rm = TRUE),
    sd = sd(LENGTH1_MM, na.rm = TRUE),
    median = median(LENGTH1_MM, na.rm = TRUE),
    IQR = IQR(LENGTH1_MM, na.rm = TRUE)
  )


# Gear name within gear group plots ####

# Gears
unique(sttj_yt$gear) # "Hook and Line", "Trap", "Other", "Net"

use_gear <- sttj_yt |>
  filter(gear == "Net") |> 
  mutate(ID = as.character(ID)) |> 
  select(-gear)

# Create box plot across years
box_plot <- ggboxplot(use_gear, x = "LAND_STANDARD_GEAR_NAME", y = "LENGTH1_MM",
                 color = "LAND_STANDARD_GEAR_NAME",
                 ylab = "LENGTH1_MM", xlab = "LAND_STANDARD_GEAR_NAME")
box_plot
# Create density plot across years
density_plot <- ggdensity(use_gear, x = "LENGTH1_MM",
                          add = "mean", rug = TRUE,
                          color = "LAND_STANDARD_GEAR_NAME", fill = "LAND_STANDARD_GEAR_NAME",
                          ylab = "LENGTH1_MM", xlab = "Gear")
density_plot

# GLMs

library(lme4)

# str(use_gear)

data = use_gear  
  # mutate(LAND_STANDARD_GEAR_NAME = gsub(".*; ", "", LAND_STANDARD_GEAR_NAME))


# plot data
library(ggplot2)
ggplot(data, aes(x = as.Date(FINAL_DATE), y = LENGTH1_MM)) +
  geom_point(aes(colour = LAND_STANDARD_GEAR_NAME, shape = LAND_STANDARD_GEAR_NAME), size = 1, alpha = 0.5) +
  geom_smooth(method = "lm", formula = "y ~ x", col = "black") +
  # facet_wrap(~ COUNTY_LANDED) +
  labs(x = "", y = "Length (mm)", colour = "", shape = "") +
  theme_bw() +
  theme(legend.position = "bottom", legend.text = element_text(size = 10),
        legend.box.spacing = unit(0, "npc"), panel.grid = element_blank()) +
  guides(colour = guide_legend(override.aes = list(size = 2)))

# fit models
library(lmerTest)
mod0 = lmer(LENGTH1_MM ~ COUNTY_LANDED * scale(FINAL_DATE) + LAND_STANDARD_GEAR_NAME + (1 | YEAR) + (1 | ID),
            data = data, REML = FALSE)
anova(mod0)

mod0.1 = lmer(LENGTH1_MM ~ COUNTY_LANDED + scale(FINAL_DATE) + LAND_STANDARD_GEAR_NAME + (1 | YEAR) + (1 | ID),
               data = data, REML = FALSE)
anova(mod0.1)

# Gaussian full model - not using county landed because it is effectively one island
mod1 = lmer(LENGTH1_MM ~ COUNTY_LANDED + LAND_STANDARD_GEAR_NAME + (1 | YEAR) + (1 | ID),
            data = data, REML = FALSE)
anova(mod1)
summary(mod1, correlation = FALSE)

mod1.1 = lmer(LENGTH1_MM ~ scale(FINAL_DATE) + LAND_STANDARD_GEAR_NAME + (1 | YEAR) + (1 | ID),
            data = data, REML = FALSE)
anova(mod1.1)


# gamma full model
mod2 = glmer(LENGTH1_MM ~ scale(FINAL_DATE) + LAND_STANDARD_GEAR_NAME + (1 | YEAR) + (1 | ID),
             data = data, family = Gamma(link=log))

AIC(mod1, mod2) # glm is better fit

# gamma reduced model
mod3 = glmer(LENGTH1_MM ~ scale(FINAL_DATE) + (1 | YEAR) + (1 | ID),
             data = data, family = Gamma(link=log))

mod4 = glmer(LENGTH1_MM ~ LAND_STANDARD_GEAR_NAME + (1 | YEAR) + (1 | ID),
             data = data, family = Gamma(link=log))

# likelihood ratio test
anova(mod2, mod3) #gives p value of gear 
  # traps:p value 0.928, no sig diff  
  # hook and line: p value 0.00004939, sig diff
  # net: p value 0.9849, no sig diff 
anova(mod2, mod4) #gives p value of date
  # hook and line: p value 0.0001759, sig diff
anova(mod2) #no p value given


## pairwise comparisons (if needed)
mod_contr = emmeans::emmeans(object = mod2, pairwise ~ "LAND_STANDARD_GEAR_NAME", adjust = "tukey")
mod_contr
# relationship between the following are significantly different: 
## lines hand - rod and reel 0.0001
## line power troll other - rod and reel 0.0205
multcomp::cld(object = mod_contr$emmeans)
# hook and line groupings: 
# LAND_STANDARD_GEAR_NAME emmean      SE  df asymp.LCL asymp.UCL .group
# LINES POWER TROLL OTHER  5.710 0.05217 Inf     5.608     5.812  1    
# LINES HAND               5.753 0.01333 Inf     5.726     5.779  1    
# ELECTRIC (HAND)          5.769 0.06645 Inf     5.639     5.900  12   
# ELECTRIC OR HYDRAULIC    5.843 0.07349 Inf     5.699     5.987  12   
# ROD AND REEL             5.894 0.02896 Inf     5.837     5.951   2   



