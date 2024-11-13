
# PREPARING LENGTH DATA FOR SS

# REQUIRE FOLDER NAMED "data" THAT CONTAINS "O2_TIP_SL57_08_21_2018.csv"
# FOLDER "data" MUST BE IN SAME LOCATION WHERE THIS R SCRIPT IS SAVED
# R script is written assuming that it will be run from a fresh R process 
# with the working directory automatically set to the project directory.

# 1 LOAD PACKAGES -------------------------------------------------

  library(here)
  library(dplyr)
  library(tidyr)
  library(ggplot2)


# 2 READ IN DATA -------------------------------------------------

  dataIn <- read.csv(here("data", "sedar_57u_cr_lob_2021", "O2_TIP_SL57U_20220830.csv"), stringsAsFactors = FALSE)

# 3 FILTER OUT PROBLEM DATA -------------------------------------------------

# GUIDANCE USING THESE DATA
# 1. SUM OVER QUANTITY TO GET SAMPLE SIZES (USE THAT VARIABLE AS INTENDED)

  dataIsl <- uncount(dataIn, QUANTITY) %>%
    rename(Year = YEAR, Length = LENGTH1_MM) %>%
    mutate(lengthSS = round(Length))


# BIN DATA ----------------------------------------------------------------

  #GET BINS THAT START ON A QUARTER INCH
  #THIS MEANS SOMETIMES 6 mm AND SOMETIMES 7 mm BINS
  bottomBin = 51 #STARTING AT 2 INCHES
  lengths <- c(bottomBin:max(dataIsl$lengthSS))
  
  #PREP BINS
  qtr_in = seq(2,10.25,.25) #STARTING AT 2 INCHES
  qtr_in_mm = round(qtr_in*25.4)
  binSizes <- qtr_in_mm[2:34]-qtr_in_mm[1:33]
  binPrep <- data.frame(cbind(bin = qtr_in_mm[1:33], space = binSizes))
  binVector <- uncount(binPrep, space)$bin
  lengths <- seq(bottomBin, max(binVector), 1)
  bin <- binVector[1:length(lengths)]
  bin_merge <- data.frame(cbind(lengths, bin))
  binSize <- "mm_qtrINCH"
  
  # SPECIFY AND GET TABLES (constant bin size)
  # binSize = 0.25
  # bottomBin = 51 #everything under will be in this bin
  # bottom <- seq(bottomBin, max(dataIsl$lengthSS), binSize)
  # bin <- c(rep(bottomBin, bottomBin-1), sort(rep(bottom, binSize)))
  # lengths <- 1:length(bin)
  # bin_merge <- data.frame(cbind(lengths, bin))
  
  allCombo <- expand.grid(lengths = c(min(dataIsl$lengthSS):max(dataIsl$lengthSS)),
              Year = c(1980:2021),
              SEX = c("F", "M"),
              GEAR_TYPE = unique(dataIsl$GEAR_TYPE),
              ISL = c("PR", "STT", "STX"), stringsAsFactors = FALSE) %>%
    left_join(., bin_merge, by = "lengths")
  
  minKeep = 51
  
  dataIsl_binned <- dataIsl %>% 
    filter(SEX %in% c("F", "M"),
           Year %in% c(1980:2021),
           lengthSS >= minKeep) %>%
    full_join(., allCombo, by = c("ISL", "GEAR_TYPE", "SEX", "Year", "lengthSS" = "lengths")) %>%
    mutate(bin = ifelse(bin == bottomBin, minKeep, bin))
  
  #FORMAT FOR SS
  LengthsForSS <- dataIsl_binned %>%
    group_by(ISL, GEAR_TYPE, SEX, Year, bin) %>%
    summarize(n = sum(!is.na(Length))) %>%
    spread(bin, n, fill = 0)
  
  SampleSizeForSS <- dataIsl_binned %>%
    group_by(ISL, GEAR_TYPE, Year) %>%
    summarize(N = sum(!is.na(Length)),
              trips = sum(!is.na(unique(ID))))
  
  PropForSS <- dataIsl_binned %>%
    group_by(ISL, GEAR_TYPE, SEX, Year, bin) %>%
    summarize(n = sum(!is.na(Length))) %>%
    full_join(., SampleSizeForSS, by = c("ISL", "GEAR_TYPE", "Year")) %>%
    mutate(sqrt_N = sqrt(N),
           p = round(n/N, 4)) %>%
    select(ISL, GEAR_TYPE, SEX, Year, bin, sqrt_N, N, trips, p) %>%
    spread(bin, p, fill = 0) %>%
    filter(N > 0) 
    
  #write.csv(LengthsForSS, here("output", paste0(binSize, "LengthsForSS.csv")), row.names = FALSE)
  #write.csv(SampleSizeForSS, here("output", paste0(binSize, "SampleSizeForSS.csv")), row.names = FALSE)
  write.csv(PropForSS, here("data", "sedar_57u_cr_lob_2021", paste0(binSize, "_PropForSS_", format(Sys.time(),'%Y%m%d',),".csv")), row.names = FALSE)
  
  
  #SIMPLE SUMMARY
  dataIsl2 <- dataIsl %>% 
    filter(SEX %in% c("F", "M"),
           Year <= 2021) 
  
  dataIsl2Summary <- dataIsl2 %>%
    group_by(ISL, GEAR_TYPE, Year) %>%
    summarize(n = length(lengthSS),
              mean = mean(lengthSS),
              min = min(lengthSS))
  
  ggplot(dataIsl2Summary, aes(Year, min)) +
    geom_line() +
    facet_grid(ISL~GEAR_TYPE)
  
  dataIsl2 %>%
    mutate(Year = factor(Year)) %>%
    ggplot(aes(Year, lengthSS)) +
    geom_boxplot() +
    facet_grid(ISL~GEAR_TYPE) +
    theme(axis.text.x=element_text(angle=90,hjust=1, vjust=0.5))
  
  #2 INCHES IS 50.8 MM (KEEP 51)
  summary(dataIsl2$lengthSS < 51) #27
  
  quantile(dataIsl2$lengthSS, .0004)

  dataIslUse <- dataIsl2 %>%
    filter(lengthSS >= minKeep)
  
  dataIslUseSummary <- dataIslUse %>%
    group_by(ISL, Year) %>%
    summarize(n = length(lengthSS),
              mean = mean(lengthSS),
              sd = sd(lengthSS))
  
  write.csv(dataIslUseSummary, here("data", "sedar_57u_cr_lob_2021", paste0("dataSummary", format(Sys.time(),'%Y%m%d'),".csv")), row.names = FALSE)
  