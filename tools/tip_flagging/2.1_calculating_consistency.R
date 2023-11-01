# Calculations of reporting consistency across SEDAR species ####

load(file = "data/count_na.Rda")
str(count_na)
view(count_na)

# find percent na's
count_na_calc <- count_na %>%
  mutate(YS_percent_na = ((na_count_ys/130564)*100),
         HOG_percent_na = ((na_count_hog/8672)*100),
         LOB_percent_na = ((na_count_lob/103281)*100),
         QT_percent_na = ((na_count_qt/36626)*100),
         SP_percent_na = ((na_count_sp/50720)*100),
         RH_percent_na = ((na_count_rh/53335)*100))
str(count_na_calc)
view(count_na)
# find percent reported
percent_report <- count_na_calc %>%
  mutate(YS_percent_reported = (100-YS_percent_na),
         HOG_percent_reported = (100-HOG_percent_na),
         LOB_percent_reported = (100-LOB_percent_na),
         QT_percent_reported = (100-QT_percent_na),
         SP_percent_reported = (100-SP_percent_na),
         RH_percent_reported = (100-RH_percent_na))
  # format(digits = 2, width = 6, scientific = FALSE, drop0trailing = TRUE  # this turns them into characters from numeric, too soon

str(percent_report)
view(count_na)
colnames(percent_report)

# Reorder columns for easy reading 
organized_report <- percent_report[,c(1,2, 8, 14, 3, 9, 15, 4, 10, 16, 5, 11, 17, 6, 12, 18, 7, 13, 19)]

# convert to numeric 
# organized_report_named <- organized_report[,-1] # convert first column back into row names
# rownames(organized_report_named) <- organized_report[,1]
# sapply(organized_report_named, class) # find out what class each column is 
# organized_report_num1 <- transform(organized_report_named,
#                                    YS_percent_na = as.numeric(YS_percent_na))
# sapply(organized_report_named, class) # check class each column is 
# check classes: 
# str(organized_report)
# organized_report_numeric <- organized_report_named %>%
#   as.numeric(na_count_ys)
# 
# organized_report_numeric <- as.data.frame(sapply(organized_report_named, as.numeric)) #<- sapply is here
# 
# organized_report$na_count_ys <- as.numeric(organized_report$na_count_ys)
# 
# as.numeric(orgainized_report$na_count_ys)


# add total calculation 
final_report <- organized_report %>% # sum 
  mutate(overall_reported_na = (rowSums(across(c(na_count_hog, na_count_lob, na_count_qt, na_count_rh, na_count_sp, na_count_ys)))))

final_percent_na <- final_report %>%
  mutate(overall_percent_na = (overall_reported_na/383198)*100)

final_percent_report <- final_percent_na %>%
  mutate(overall_percent_reported = (100-overall_percent_na))

# make it pretty
final_percent_report_named <- final_percent_report[,-1] # convert first column back into row names
rownames(final_percent_report_named) <- final_percent_report[,1]

calculating_consistency <- final_percent_report_named[,c(19, 20, 21, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18)]%>%
  format(digits = 2, width = 6, scientific = FALSE, drop0trailing = TRUE) %>%
  rename(OVERALL_NA = overall_reported_na,
         OVERALL_PERCENT_NA = overall_percent_na,
         OVERALL_PERCENT_REPORTED = overall_percent_reported,
         YTS_NA = na_count_ys,
         YTS_PERCENT_NA = YS_percent_na,
         YTS_PERCENT_REPORTED = YS_percent_reported,
         HOG_NA = na_count_hog,
         HOG_PERCENT_NA = HOG_percent_na,
         HOG_PERCENT_REPORTED = HOG_percent_reported,
         LOB_NA = na_count_lob,
         LOB_PERCENT_NA = LOB_percent_na,
         LOB_PERCENT_REPORTED = LOB_percent_reported,
         QTF_NA = na_count_qt,
         QTF_PERCENT_NA = QT_percent_na,
         QTF_PERCENT_REPORTED = QT_percent_reported,
         SLP_NA = na_count_sp,
         SLP_PERCENT_NA = SP_percent_na,
         SLP_PERCENT_REPORTED = SP_percent_reported,
         REH_NA = na_count_rh,
         REH_PERCENT_NA = RH_percent_na,
         REH_PERCENT_REPORTED = RH_percent_reported)

# export 
save(count_na,file="calculating_consistency") # file is saved in data folder

write.csv(calculating_consistency, here("data", paste0("calculating_consistency_", format(Sys.Date(), "%Y%m%d"), ".csv")), row.names = TRUE)


# OVERALL stats
colSums(final_percent_report_named==0) # have to use this df cause still numeric
(colSums(final_percent_report_named==0)/nrow(final_percent_report_named))*100
# 10.81967% are never reported
# 31.80328% are always reported

