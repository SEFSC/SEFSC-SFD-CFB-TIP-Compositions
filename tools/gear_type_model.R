# Fit GLMMs to size comp data (USVI commercial yellowtail snapper)
# Kyle Dettloff
# 11-16-2023

# read in data
tip = readRDS(file = "M:/SFD/SECM-SFD/SEFSC-SFD-CFB-TIP-Compositions-fis-dev-20231115/data/raw/com_tip_PR_VI_20231108.RDS")

# prepare data for modeling
library(dplyr)
data = tip %>% filter(STATE_LANDED == "VIRGIN ISLANDS", COUNTY_LANDED %in% c("ST CROIX", "ST THOMAS"), FISHING_MODE == "COMMERCIAL", LENGTH1_MM > 0,
                      LAND_STANDARD_SPECIES_CODE == 168907, grepl("TRAPS", LAND_STANDARD_GEAR_NAME)) %>%
  select(ID, INTERVIEW_DATE, YEAR, COUNTY_LANDED, LAND_STANDARD_GEAR_NAME, LENGTH1_MM) %>%
  rename(GEAR = LAND_STANDARD_GEAR_NAME) %>%
  mutate(ID = as.character(ID), INTERVIEW_DATE = as.numeric(as.Date(INTERVIEW_DATE)), GEAR = gsub(".*; ", "", GEAR))

# plot data
library(ggplot2)
ggplot(data, aes(x = as.Date(INTERVIEW_DATE), y = LENGTH1_MM)) +
  geom_point(aes(colour = GEAR, shape = GEAR), size = 1, alpha = 0.5) +
  geom_smooth(method = "lm", formula = "y ~ x", col = "black") +
  facet_wrap(~ COUNTY_LANDED) +
  labs(x = "", y = "Length (mm)", colour = "", shape = "") +
  theme_bw() +
  theme(legend.position = "bottom", legend.text = element_text(size = 10),
        legend.box.spacing = unit(0, "npc"), panel.grid = element_blank()) +
  guides(colour = guide_legend(override.aes = list(size = 2)))

# fit models
library(lmerTest)
 mod0 = lmer(LENGTH1_MM ~ COUNTY_LANDED * scale(INTERVIEW_DATE) + GEAR + (1 | YEAR) + (1 | ID),
             data = data, REML = FALSE)
 anova(mod0)
 
 mod0.1 = lmer(LENGTH1_MM ~ COUNTY_LANDED + scale(INTERVIEW_DATE) + GEAR + (1 | YEAR) + (1 | ID),
             data = data, REML = FALSE)
 anova(mod0.1)

# Gaussian full model
mod1 = lmer(LENGTH1_MM ~ COUNTY_LANDED + GEAR + (1 | YEAR) + (1 | ID),
             data = data, REML = FALSE)
anova(mod1)
summary(mod1, correlation = FALSE)

# gamma full model
mod2 = glmer(LENGTH1_MM ~ COUNTY_LANDED + GEAR + (1 | YEAR) + (1 | ID),
             data = data, family = Gamma(link=log))

# gamma reduced model
mod3 = glmer(LENGTH1_MM ~ COUNTY_LANDED + (1 | YEAR) + (1 | ID),
             data = data, family = Gamma(link=log))

# likelihood ratio test
anova(mod2, mod3) #gives p value of gear
anova(mod2) #no p value given
## pairwise comparisons (if needed)
install.packages('multcompView')
library(emmeans)
mod_contr = emmeans::emmeans(object = mod2, pairwise ~ "GEAR", adjust = "tukey")

multcomp::cld(object = mod_contr$emmeans)
