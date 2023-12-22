# Isolate unique muni codes 

# libraries
librarian::shelf(here, tidyverse, ROracle, keyring, dotenv, lubridate, readr, stringr)

# select all years in tip
# cr_tip_yr2(state_codes = 'PR', st_year = "1988", end_year = "2010")

com_tip_PR_88_10 <- readRDS("~/SEFSC-SFD-CFB-TIP-Compositions/data/raw/com_tip_PR_1988_2010_20231205.RDS")

# upload all yrs historically - before uploading i deleted any columns from csv that were completely NA
# look into differences between group A and B variables 
# location variables all changed to "AREAZIP" 
hist_88 <- read_csv("data/CSVs/hist_88.csv")
  str(hist_88)
hist_89 <- read_csv("data/CSVs/hist_89.csv")
  str(hist_89_num)
  hist_89_num <- hist_89 |> 
    mutate(AREACOUNTY = as.numeric(AREACOUNTY),
           AREAZIP = as.numeric(AREAZIP))
hist_90 <- read_csv("data/CSVs/hist_90.csv")
hist_91 <- read_csv("data/CSVs/hist_91.csv")
hist_92 <- read_csv("data/CSVs/hist_92.csv")
hist_93 <- read_csv("data/CSVs/hist_93.csv")
hist_94 <- read_csv("data/CSVs/hist_94.csv")
hist_95 <- read_csv("data/CSVs/hist_95.csv")
hist_96 <- read_csv("data/CSVs/hist_96.csv")
hist_97 <- read_csv("data/CSVs/hist_97.csv")
hist_98 <- read_csv("data/CSVs/hist_98.csv")
hist_99 <- read_csv("data/CSVs/hist_99.csv")
hist_00 <- read_csv("data/CSVs/hist_00.csv")
hist_01 <- read_csv("data/CSVs/hist_01.csv")
hist_02 <- read_csv("data/CSVs/hist_02.csv")
hist_03 <- read_csv("data/CSVs/hist_03.csv")
hist_04 <- read_csv("data/CSVs/hist_04.csv")
hist_05 <- read_csv("data/CSVs/hist_05.csv")
hist_06 <- read_csv("data/CSVs/hist_06.csv")
hist_07 <- read_csv("data/CSVs/hist_07.csv")
hist_08 <- read_csv("data/CSVs/hist_08.csv")
hist_09 <- read_csv("data/CSVs/hist_09.csv") # data format is different from previous year
hist_10 <- read_csv("data/CSVs/hist_10.csv")

# find unique areazip from each year

summary(hist_89)

hist_88_countCOUNTY <- hist_88 |> 
  group_by(AREACOUNTY) |>
  summarise(n_distinct(DATE)) 
colnames(hist_88_countCOUNTY)[2] = "DATE88"

hist_89_countCOUNTY <- hist_89_num |> 
  group_by(AREACOUNTY) |>
  summarise(n_distinct(DATE)) 
colnames(hist_89_countCOUNTY)[2] = "DATE89"

hist_89_countZIP <- hist_89_num |> 
  group_by(AREAZIP) |>
  summarise(n_distinct(DATE)) 
colnames(hist_89_countZIP)[2] = "DATE89"

hist_90_countCOUNTY <- hist_90 |> 
  group_by(AREACOUNTY) |>
  summarise(n_distinct(INT_NUM)) 
colnames(hist_90_countCOUNTY)[2] = "ID90"

hist_90_countZIP <- hist_90 |> 
  group_by(AREAZIP) |>
  summarise(n_distinct(INT_NUM))  
colnames(hist_90_countZIP)[2] = "ID90"

hist_91_countCOUNTY <- hist_91 |> 
  group_by(AREACOUNTY) |>
  summarise(n_distinct(INT_NUM))  
colnames(hist_91_countCOUNTY)[2] = "ID91"

hist_91_countZIP <- hist_91 |> 
  group_by(AREAZIP) |>
  summarise(n_distinct(INT_NUM))  
colnames(hist_91_countZIP)[2] = "ID91"

hist_92_countZIP <- hist_92 |> 
  group_by(AREAZIP) |>
  summarise(n_distinct(SEQUENCE))  
colnames(hist_92_countZIP)[2] = "ID92"

hist_93_countZIP <- hist_93 |> 
  group_by(AREAZIP) |>
  summarise(n_distinct(SEQUENCE))  
colnames(hist_93_countZIP)[2] = "ID93"

hist_94_countZIP <- hist_94 |> 
  group_by(AREAZIP) |>
  summarise(n_distinct(SEQUENCE))  
colnames(hist_94_countZIP)[2] = "ID94"

hist_95_countZIP <- hist_95 |> 
  group_by(AREAZIP) |>
  summarise(n_distinct(SEQUENCE))  
colnames(hist_95_countZIP)[2] = "ID95"

hist_96_countZIP <- hist_96 |> 
  group_by(AREAZIP) |>
  summarise(n_distinct(SEQUENCE))  
colnames(hist_96_countZIP)[2] = "ID96"

hist_97_countZIP <- hist_97 |> 
  group_by(AREAZIP) |>
  summarise(n_distinct(SEQUENCE))  
colnames(hist_97_countZIP)[2] = "ID97"

hist_98_countZIP <- hist_98 |> 
  group_by(AREAZIP) |>
  summarise(n_distinct(SEQUENCE))  
colnames(hist_98_countZIP)[2] = "ID98"

hist_99_countZIP <- hist_99 |> 
  group_by(AREAZIP) |>
  summarise(n_distinct(SEQUENCE))  
colnames(hist_99_countZIP)[2] = "ID99"

hist_00_countZIP <- hist_00 |> 
  group_by(AREAZIP) |>
  summarise(n_distinct(SEQUENCE)) 
colnames(hist_00_countZIP)[2] = "ID00" 

hist_01_countZIP <- hist_01 |> 
  group_by(AREAZIP) |>
  summarise(n_distinct(SEQUENCE))  
colnames(hist_01_countZIP)[2] = "ID01"

hist_02_countZIP <- hist_02 |> 
  group_by(AREAZIP) |>
  summarise(n_distinct(SEQUENCE))  
colnames(hist_02_countZIP)[2] = "ID02"

hist_03_countZIP <- hist_03 |> 
  group_by(AREAZIP) |>
  summarise(n_distinct(SEQUENCE))  
colnames(hist_03_countZIP)[2] = "ID03"

hist_04_countZIP <- hist_04 |> 
  group_by(AREAZIP) |>
  summarise(n_distinct(SEQUENCE))  
colnames(hist_04_countZIP)[2] = "ID04"

hist_05_countZIP <- hist_05 |> 
  group_by(AREAZIP) |>
  summarise(n_distinct(SEQUENCE))  
colnames(hist_05_countZIP)[2] = "ID05"

hist_06_countZIP <- hist_06 |> 
  group_by(AREAZIP) |>
  summarise(n_distinct(SEQUENCE))  
colnames(hist_06_countZIP)[2] = "ID06"

hist_07_countZIP <- hist_07 |> 
  group_by(AREAZIP) |>
  summarise(n_distinct(SEQUENCE))  
colnames(hist_07_countZIP)[2] = "ID07"

hist_08_countZIP <- hist_08 |> 
  group_by(AREAZIP) |>
  summarise(n_distinct(SEQUENCE))  
colnames(hist_08_countZIP)[2] = "ID08"

hist_09_countZIP <- hist_09 |> 
  group_by(AREAZIP) |>
  summarise(n_distinct(ID))  
colnames(hist_09_countZIP)[2] = "ID09"

hist_10_countZIP <- hist_10 |> 
  group_by(AREAZIP) |>
  summarise(n_distinct(ID))  
colnames(hist_10_countZIP)[2] = "ID10"

# join tables by AREAZIP

hist_areazip1 <- full_join(hist_89_countZIP, hist_90_countZIP, by = "AREAZIP")
hist_areazip2 <- full_join(hist_91_countZIP, hist_92_countZIP, by = "AREAZIP")
hist_areazip3 <- full_join(hist_93_countZIP, hist_94_countZIP, by = "AREAZIP")
hist_areazip4 <- full_join(hist_95_countZIP, hist_96_countZIP, by = "AREAZIP")
hist_areazip5 <- full_join(hist_97_countZIP, hist_98_countZIP, by = "AREAZIP")
hist_areazip6 <- full_join(hist_99_countZIP, hist_00_countZIP, by = "AREAZIP")
hist_areazip7 <- full_join(hist_01_countZIP, hist_02_countZIP, by = "AREAZIP")
hist_areazip8 <- full_join(hist_03_countZIP, hist_04_countZIP, by = "AREAZIP")
hist_areazip9 <- full_join(hist_05_countZIP, hist_06_countZIP, by = "AREAZIP")
hist_areazip10 <- full_join(hist_07_countZIP, hist_08_countZIP, by = "AREAZIP")
hist_areazip11 <- full_join(hist_09_countZIP, hist_10_countZIP, by = "AREAZIP")

hist_areazip12 <- full_join(hist_areazip1, hist_areazip2, by = "AREAZIP")
hist_areazip13 <- full_join(hist_areazip3, hist_areazip4, by = "AREAZIP")
hist_areazip14 <- full_join(hist_areazip5, hist_areazip6, by = "AREAZIP")
hist_areazip15 <- full_join(hist_areazip7, hist_areazip8, by = "AREAZIP")
hist_areazip16 <- full_join(hist_areazip9, hist_areazip10, by = "AREAZIP")


hist_areazip17 <- full_join(hist_areazip11, hist_areazip12, by = "AREAZIP")
hist_areazip18 <- full_join(hist_areazip13, hist_areazip14, by = "AREAZIP")
hist_areazip19 <- full_join(hist_areazip15, hist_areazip16, by = "AREAZIP")

hist_areazip20 <- full_join(hist_areazip17, hist_areazip18, by = "AREAZIP")

hist_areazip21 <- full_join(hist_areazip19, hist_areazip20, by = "AREAZIP")

write.csv(hist_areazip21, file = "tools/output/unique_areazip.csv", row.names = FALSE)

hist_areacounty1 <- full_join(hist_88_countCOUNTY, hist_89_countCOUNTY, by = "AREACOUNTY")
hist_areacounty2 <- full_join(hist_90_countCOUNTY, hist_91_countCOUNTY, by = "AREACOUNTY")

hist_areacounty3 <- full_join(hist_areacounty1, hist_areacounty2, by = "AREACOUNTY")

write.csv(hist_areacounty3, file = "tools/output/unique_areacounty.csv", row.names = FALSE)

# COMBINE AREAZIP AND AREACOUNTY INTO ONE LIST 

hist_88_countCOUNTY <- hist_88 |> 
  group_by(AREACOUNTY) |>
  summarise(n_distinct(DATE)) 
colnames(hist_88_countCOUNTY)[2] = "DATE88c"
colnames(hist_88_countCOUNTY)[1] = "AREAZIP"

hist_89_countCOUNTY <- hist_89_num |> 
  group_by(AREACOUNTY) |>
  summarise(n_distinct(DATE)) 
colnames(hist_89_countCOUNTY)[2] = "DATE89c"
colnames(hist_89_countCOUNTY)[1] = "AREAZIP"

hist_89_countZIP <- hist_89_num |> 
  group_by(AREAZIP) |>
  summarise(n_distinct(DATE)) 
colnames(hist_89_countZIP)[2] = "DATE89z"

hist_90_countCOUNTY <- hist_90 |> 
  group_by(AREACOUNTY) |>
  summarise(n_distinct(INT_NUM)) 
colnames(hist_90_countCOUNTY)[2] = "ID90c"
colnames(hist_90_countCOUNTY)[1] = "AREAZIP"

hist_90_countZIP <- hist_90 |> 
  group_by(AREAZIP) |>
  summarise(n_distinct(INT_NUM))  
colnames(hist_90_countZIP)[2] = "ID90z"

hist_91_countCOUNTY <- hist_91 |> 
  group_by(AREACOUNTY) |>
  summarise(n_distinct(INT_NUM))  
colnames(hist_91_countCOUNTY)[2] = "ID91c"
colnames(hist_91_countCOUNTY)[1] = "AREAZIP"

hist_91_countZIP <- hist_91 |> 
  group_by(AREAZIP) |>
  summarise(n_distinct(INT_NUM))  
colnames(hist_91_countZIP)[2] = "ID91z"

hist_areazip1 <- full_join(hist_89_countZIP, hist_90_countZIP, by = "AREAZIP")
hist_areazip2 <- full_join(hist_91_countZIP, hist_92_countZIP, by = "AREAZIP")
hist_areazip12 <- full_join(hist_areazip1, hist_areazip2, by = "AREAZIP")
hist_areazip17 <- full_join(hist_areazip11, hist_areazip12, by = "AREAZIP")
hist_areazip20 <- full_join(hist_areazip17, hist_areazip18, by = "AREAZIP")
hist_areazip22 <- full_join(hist_areazip19, hist_88_countCOUNTY, by = "AREAZIP")
hist_areazip23 <- full_join(hist_areazip20, hist_areazip22, by = "AREAZIP")

write.csv(hist_areazip23, file = "tools/output/unique_areacountyzip.csv", row.names = FALSE)


# tip dataset unique place_id
colnames(com_tip_PR_88_10)
# reporting_area_zip is historical landing zip 
# sample_area_zip is historical sampling zip
# site_location is historical sepcific zip 
PR_88_10 <- com_tip_PR_88_10 |> 
  select(ID, COUNTY_LANDED, LANDING_AREA_COUNTY_CODE, PLACE_LANDED, LANDING_AREA_PLACE_CODE, 
         COUNTY_SAMPLED, SAMPLE_AREA_COUNTY_CODE, PLACE_SAMPLED, SAMPLE_AREA_PLACE_CODE,
         SAMPLE_AREA_ZIP, REPORTING_AREA_ZIP, SITE_LOCATION)

PR_88_10_county_land <- PR_88_10 |> 
  group_by(LANDING_AREA_COUNTY_CODE) |>
  summarise(n_distinct(ID)) 
colnames(PR_88_10_county_land)[2] = "LANDING_AREA"
colnames(PR_88_10_county_land)[1] = "COUNTY_CODE"

PR_88_10_county_sample <- PR_88_10 |> 
  group_by(SAMPLE_AREA_COUNTY_CODE) |>
  summarise(n_distinct(ID)) 
colnames(PR_88_10_county_sample)[2] = "SAMPLE_AREA"
colnames(PR_88_10_county_sample)[1] = "COUNTY_CODE"

tip_county_code <- full_join(PR_88_10_county_sample, PR_88_10_county_land, by = "COUNTY_CODE")
write.csv(tip_county_code, file = "tools/output/unique_countycode_tip.csv", row.names = FALSE)

PR_88_10_place_land <- PR_88_10 |> 
  group_by(LANDING_AREA_PLACE_CODE) |>
  summarise(n_distinct(ID)) 
colnames(PR_88_10_place_land)[2] = "LANDING_AREA"
colnames(PR_88_10_place_land)[1] = "PLACE_CODE"

PR_88_10_place_sample <- PR_88_10 |> 
  group_by(SAMPLE_AREA_PLACE_CODE) |>
  summarise(n_distinct(ID)) 
colnames(PR_88_10_place_sample)[2] = "SAMPLE_AREA"
colnames(PR_88_10_place_sample)[1] = "PLACE_CODE"

tip_place_code <- full_join(PR_88_10_place_sample, PR_88_10_place_land, by = "PLACE_CODE")
write.csv(tip_place_code, file = "tools/output/unique_placecode_tip.csv", row.names = FALSE)
