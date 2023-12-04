# muni code consolidation across projects

# libraries
librarian::shelf(here, tidyverse, ROracle, keyring, dotenv, lubridate, readr, stringr)

# load csvs
# edits before upload: remove accents on letters, convert degree to decimal coordinates - in process 
pr_area_comparisons_mrip <- read_csv("data/CSVs/pr_area_comparisons_mrip.csv")
pr_area_comparisons_tip <- read_csv("data/CSVs/pr_area_comparisons_tip.csv")
pr_area_comparisons_historical <- read_csv("data/CSVs/pr_area_comparisons_historical.csv")
pr_area_comparisons_mer <- read_csv("data/CSVs/pr_area_comparisons_mer.csv")
pr_mrip_mer_area_comparison_spatialjoin <- read_csv("data/CSVs/pr_mrip_mer_area_comparison_spatialjoin.csv")

# check characters of tables
# str(pr_area_comparisons_mer)

# make everything lower case
# mrip
pr_area_comparisons_mrip$MRIP_Site_Name<- tolower(pr_area_comparisons_mrip$MRIP_Site_Name)
pr_area_comparisons_mrip$MRIP_City<- tolower(pr_area_comparisons_mrip$MRIP_City)
pr_area_comparisons_mrip$MRIP_County <- tolower(pr_area_comparisons_mrip$MRIP_County)

# tip
pr_area_comparisons_tip$TIP_PLACE_NAME<- tolower(pr_area_comparisons_tip$TIP_PLACE_NAME)
pr_area_comparisons_tip$TIP_CNTY_NAME<- tolower(pr_area_comparisons_tip$TIP_CNTY_NAME)
pr_area_comparisons_tip$TIP_STATE_ID <- tolower(pr_area_comparisons_tip$TIP_STATE_ID)

# historical
pr_area_comparisons_historical$H_muni_name<- tolower(pr_area_comparisons_historical$H_muni_name)
pr_area_comparisons_historical$H_muni_cd<- tolower(pr_area_comparisons_historical$H_muni_cd)
pr_area_comparisons_historical$H_region <- tolower(pr_area_comparisons_historical$H_region)

# mer
pr_area_comparisons_mer$MER_Municipio<- tolower(pr_area_comparisons_mer$MER_Municipio)
pr_area_comparisons_mer$MER_Name<- tolower(pr_area_comparisons_mer$MER_Name)
pr_area_comparisons_mer$MER_PR_Region <- tolower(pr_area_comparisons_mer$MER_PR_Region)
pr_area_comparisons_mer$MER_code <- tolower(pr_area_comparisons_mer$MER_code)

#combo mer/mrip
pr_mrip_mer_area_comparison_spatialjoin$MER_Name <- tolower(pr_mrip_mer_area_comparison_spatialjoin$MER_Name)
pr_mrip_mer_area_comparison_spatialjoin$MER_Municipio <- tolower(pr_mrip_mer_area_comparison_spatialjoin$MER_Municipio)
pr_mrip_mer_area_comparison_spatialjoin$MRIP_Site_Name <- tolower(pr_mrip_mer_area_comparison_spatialjoin$MRIP_Site_Name)
pr_mrip_mer_area_comparison_spatialjoin$MRIP_County <- tolower(pr_mrip_mer_area_comparison_spatialjoin$MRIP_County)


# CONVERT DMS TO DECIMAL DEGREES - COMPLETED WITHIN CSV
# all longitudes should be negative

# remove direction (N,W) from MER and add (-) to long 
pr_area_comparisons_mer$MER_latitude <- str_sub(pr_area_comparisons_mer$MER_lat, end = -2)  
pr_area_comparisons_mer$MER_longitude <- str_sub(pr_area_comparisons_mer$MER_long, end = -2)  
pr_area_comparisons_mer$MER_longitude = as.numeric(as.character(pr_area_comparisons_mer$MER_longitude)) 
pr_area_comparisons_mer <- pr_area_comparisons_mer |> 
  mutate(MER_longitued_d = MER_longitude*(-1))  
  # rename(MER_latitude_d = MER_latitude)
colnames(pr_area_comparisons_mer)[24] = "MER_latitude_d"

# convert all lat column to numeric
pr_area_comparisons_mer$MER_latitude_d = as.numeric(as.character(pr_area_comparisons_mer$MER_latitude_d)) 

# create common muni and site column names
# pr_historical <- pr_area_comparisons_historical%>%
#   rename(MUNICIPIO = H_muni_name)
colnames(pr_area_comparisons_historical)[2] = "MUNICIPIO"

# pr_mer <- pr_area_comparisons_mer%>%
#   rename(#MUNICIPIO = MER_Municipio,
#          SITE_NAME = MER_Name)
# 
# pr_mrip <- pr_area_comparisons_mrip%>%
#   rename(#MUNICIPIO = MRIP_County,
#          SITE_NAME = MRIP_Site_Name)

# pr_tip <- pr_area_comparisons_tip%>%
#   rename(MUNICIPIO = TIP_CNTY_NAME)
colnames(pr_area_comparisons_tip)[4] = "MUNICIPIO"

# pr_spatialjoin <- pr_mrip_mer_area_comparison_spatialjoin%>%
#   rename(MUNICIPIO = MRIP_County)

# pair down to necessary columns 
pr_mer_skeleton <- pr_area_comparisons_mer[,c(2, 3, 24, 26)]
pr_mrip_skeleton <- pr_area_comparisons_mrip[,c(2, 6, 9, 10)]
spatialjoin_skeleton <- pr_mrip_mer_area_comparison_spatialjoin[, c(1, 2, 6, 19, 20, 37,  44, 45)]

# remove rows from MER that have na's as those locations are not used
pr_mer_clean <- na.omit(pr_mer_skeleton)

# # try full join
# # only one location with the same site name in both tables, figure out another way to cross reference

# merge to add mer locations not in the join
combo_merge_mer <- merge(spatialjoin_skeleton,pr_mer_clean, by = 'MER_Name', all = TRUE)

# clean columns of mer merge
combo_merge_mer_clean <- combo_merge_mer[, c(1, 2, 3, 6, 7, 8, 9, 10, 11)]


# merge to add mrip locations not in the join
combo_merge_mermrip <- merge(combo_merge_mer_clean,pr_mrip_skeleton, by = 'MRIP_Site_Name', all = TRUE)

# clean columns of mrip merge
combo_merge_mermrip_clean <- combo_merge_mermrip[, c(1, 2, 3, 4, 7, 8, 9, 10, 11, 12)]
write.csv(combo_merge_mermrip_clean, file = "tools/output/combo_merge_firstdraft.csv", row.names = FALSE)

# read in cleaned first draft (combined mer/mrip locations fixed)
combo_merge_firstdraft_edited <- read_csv("tools/output/combo_merge_firstdraft_edited.csv")

# create a new column for official site name
combo_merge <- combo_merge_firstdraft_edited |> 
  mutate(SITE_NAME = case_when(!is.na(MER_Name) ~ MER_Name, 
                               TRUE ~ MRIP_Site_Name),
         MUNICIPIO = case_when(!is.na(MER_Municipio) ~ MER_Municipio, 
                               TRUE ~ MRIP_County),
         GEN_LAT = case_when(!is.na(MER_latitude_d) ~ MER_latitude_d, 
                               TRUE ~ MRIP_Latitude.y),
         GEN_LONG = case_when(!is.na(MER_longitued_d) ~ MER_longitued_d, 
                               TRUE ~ MRIP_Longitude.y)) 
combo_merge_ID <- rowid_to_column(combo_merge, "SITE_ID") |> 
  select(SITE_ID, SITE_NAME, MUNICIPIO, GEN_LAT, GEN_LONG)


write.csv(combo_merge_ID, file = "tools/output/combo_merge_muni.csv", row.names = FALSE)

