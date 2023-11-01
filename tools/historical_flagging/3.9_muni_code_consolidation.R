# muni code consolidation across projects

# libraries
librarian::shelf(here, tidyverse, ROracle, keyring, dotenv, lubridate, readr)

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

# create common column name
pr_historical <- pr_area_comparisons_historical%>%
  rename(MUNICIPIO = H_muni_name)

pr_mer <- pr_area_comparisons_mer%>%
  rename(MUNICIPIO = MER_Municipio)

pr_mrip <- pr_area_comparisons_mrip%>%
  rename(MUNICIPIO = MRIP_City)

pr_tip <- pr_area_comparisons_tip%>%
  rename(MUNICIPIO = TIP_CNTY_NAME)

# pair down to necessary columns 
pr_mer_skeleton <- pr_mer[,c(1,2,3, 11, 12, 13, 22)]
pr_mrip_skeleton <- pr_mrip[,c(1,2, 4, 5, 5, 6, 7, 8, 9, 10)]

# try full join
mer_mrip_combo <- full_join(pr_mer, pr_mrip, by = 'MUNICIPIO')



