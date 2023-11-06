# muni code consolidation across projects

# libraries
librarian::shelf(here, tidyverse, ROracle, keyring, dotenv, lubridate, readr, stringr)

# load csvs
# edits before upload: remove accents on letters, convert degree to decimal coordinates - in process 
pr_area_comparisons_mrip <- read_csv("data/CSVs/pr_area_comparisons_mrip.csv")
pr_area_comparisons_tip <- read_csv("data/CSVs/pr_area_comparisons_tip.csv")
pr_area_comparisons_historical <- read_csv("data/CSVs/pr_area_comparisons_historical.csv")
pr_area_comparisons_mer <- read_csv("data/CSVs/pr_area_comparisons_mer.csv")

# check characters of tables
str(pr_area_comparisons_mrip)

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

# CONVERT DMS TO DECIMAL DEGREES - COMPLETED WITHIN CSV
# all longitudes should be negative

# remove direction (N,W) from MER and add (-) to long 
pr_area_comparisons_mer$MER_lat <- str_sub(pr_area_comparisons_mer$MER_lat, end = -2)  
pr_area_comparisons_mer$MER_long <- str_sub(pr_area_comparisons_mer$MER_long, end = -2)  
pr_area_comparisons_mer$MER_long = as.numeric(as.character(pr_area_comparisons_mer$MER_long)) 
pr_area_comparisons_mer <- pr_area_comparisons_mer |> 
  mutate(MER_LONGITUDE = MER_long*(-1)) |> 
  rename(MER_LATITUDE = MER_lat)

# convert all lat column to numeric
pr_area_comparisons_mer$MER_LATITUDE = as.numeric(as.character(pr_area_comparisons_mer$MER_LATITUDE)) 

# create common muni and site column names
pr_historical <- pr_area_comparisons_historical%>%
  rename(MUNICIPIO = H_muni_name)

pr_mer <- pr_area_comparisons_mer%>%
  rename(#MUNICIPIO = MER_Municipio,
         SITE_NAME = MER_Name)

pr_mrip <- pr_area_comparisons_mrip%>%
  rename(#MUNICIPIO = MRIP_County,
         SITE_NAME = MRIP_Site_Name)

pr_tip <- pr_area_comparisons_tip%>%
  rename(MUNICIPIO = TIP_CNTY_NAME)

# pair down to necessary columns 
pr_mer_skeleton <- pr_mer[,c(1,2,3, 11, 12, 13, 24)]
pr_mrip_skeleton <- pr_mrip[,c(1,2, 4, 5, 6, 7, 8, 9, 10)]

# remove rows from MER that have na's as those locations are not used
pr_mer_clean <- na.omit(pr_mer_skeleton)

# try full join
mer_mrip_combo <- full_join(pr_mrip_skeleton, pr_mer_clean, by = 'SITE_NAME')
# only one location with the same site name in both tables, figure out another way to cross reference
# 


