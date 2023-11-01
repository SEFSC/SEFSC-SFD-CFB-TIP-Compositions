# Plotting historical data


# Plot to see outliers 
plot(pr_04_sp901_filtered$AREAZIP,  pr_04_sp901_filtered$STLENGTH)
pr_04_STLmorethan1000 <- pr_04_sp901_filtered %>%
  filter(STLENGTH > 1000)
plot(pr_04_STLmorethan1000$AREAZIP,  pr_04_STLmorethan1000$STLENGTH) #all samples >10.000 are from same location sus?
unique(pr_04_STLmorethan1000$AREAZIP) # AREAZIP = 374


view(pr_04_sp901_filtered)
unique(pr_04_sp901_filtered$LENGTHTYPE) #FM is only length type

plot(pr_04_STLmorethan1000$INTDATE, pr_04_STLmorethan1000$STLENGTH) #all but 1 value above 10,000 was recorded on the same day sus?
unique(pr_04_STLmorethan1000$INTDATE) # 1/16/2004 and 8/10/2004

# Plot L vs W 
plot(pr_04_sp901_filtered$STLENGTH, pr_04_sp901_filtered$WEIGHT_B)
pr_04_sp901_reduced <- pr_04_sp901_filtered %>%
  filter(STLENGTH <= 300 & STLENGTH > 50,
         WEIGHT_B >= 500 & WEIGHT_B < 4000,
         WEIGHT_B !=0,
         STLENGTH !=0)

plot(pr_04_sp901_reduced$STLENGTH, pr_04_sp901_reduced$WEIGHT_B)

colnames(pr_04_sp901_filtered) #get column names
