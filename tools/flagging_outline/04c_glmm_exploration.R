# 04c Comparing k to date and area in a gamma full model ####


# # If using multiple islands use "island", if just PR use "county_sampled"
# mod3 <- glmer(k ~ scale(interview_date) + county_sampled + (1 | year) + (1 | id),
#               data = tip_spp,
#               family = Gamma(link = log)
# )
#
# # pairwise comparisons - compares each gear to eachother and gives p value
# mod_contr <- emmeans::emmeans(object = mod3,
#                               pairwise ~ "gear",
#                               adjust = "tukey")
#
# # cld provides gear groupings based on which gears are
# # similar vs significantly different from each other
# allgears_multcompcld <- multcomp::cld(object = mod_contr$emmeans)