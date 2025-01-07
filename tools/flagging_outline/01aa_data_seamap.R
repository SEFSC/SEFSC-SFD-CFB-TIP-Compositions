# 01aa_data_seamapc

# Load libraries ####
    librarian::shelf(here, tidyverse, measurements)

# specify settings 
    sedar <- "sedar91"
    pr <- "seamapc_pr_c_kg.csv"
    stt <- "seamapc_stt_c_kg.csv"
    stx <- "seamapc_stx_c_kg.csv"
  
# before import, i removed any columns that were unnecessary to length comp or had incompatable symbols

# import seamap-c data
    seamap_pr <- read.csv(here::here("data", sedar, "csv", pr))
    seamap_stt <- read.csv(here::here("data", sedar, "csv", stt))
    seamap_stx <- read.csv(here::here("data", sedar, "csv", stx))

# format the seamapc by each island

# PR   #### 
   colnames(seamap_pr)

## Prep raw data ####
    pr_format <- seamap_pr |>
# Standardize variable format
      janitor::clean_names() |>
      dplyr::mutate(
# Simplify and standardize variable names
        sampling_program = "SEAMAP-C",
        data_provider = "NMFS Miami",
        stock = "Caribbean",
        specimen_id = "097648",
        interview_date = as.Date(as.character(interview_date), format = "%d/%m/%y"), 
        month = format(as.Date(interview_date,format="%Y-%m-%d"), format = "%m"),
        day = format(as.Date(interview_date,format="%Y-%m-%d"), format = "%d"),
        year = format(as.Date(interview_date,format="%Y-%m-%d"), format = "%Y"),
# Create variable for fishery (rec or com)
        fishery = "COMMERCIAL",
      )  |> 
# select necessary variables 
      dplyr::select(sampling_program, 
                    data_provider, 
                    stock, 
                    island, 
                    specimen_id,
                    interview_date, 
                    month, 
                    day, 
                    year,
                    fishery, 
                    length1_cm,
                    obs_weight_g,
                    sex_name, 
                    )
  glimpse(pr_format)  
  
# STT ####
  
  colnames(seamap_stt)
  glimpse(seamap_stt)
  
## Prep raw data ####
  stt_format <- seamap_stt |>
# Standardize variable format
    janitor::clean_names() |>
    dplyr::mutate(
# Simplify and standardize variable names
      sampling_program = "SEAMAP-C",
      data_provider = "NMFS Miami",
      stock = "Caribbean",
      specimen_id = "097648",
      interview_date = as.Date(as.character(interview_date), format = "%d/%m/%y"), 
      month = format(as.Date(interview_date,format="%Y-%m-%d"), format = "%m"),
      day = format(as.Date(interview_date,format="%Y-%m-%d"), format = "%d"),
      year = format(as.Date(interview_date,format="%Y-%m-%d"), format = "%Y"),
# Create variable for fishery (rec or com)
      fishery = "COMMERCIAL",
    )  |> 
# select necessary variables 
    dplyr::select(sampling_program, 
                  data_provider, 
                  stock, 
                  island, 
                  specimen_id, 
                  interview_date, 
                  month, 
                  day, 
                  year,
                  fishery, 
                  length1_cm,
                  obs_weight_g,
                  sex_name, 
    )
  
  glimpse(stt_format)  
  
# STX ####
  
  colnames(seamap_stx)
  glimpse(seamap_stx)
## Prep raw data ####
  stx_format <- seamap_stx |>
# Standardize variable format
    janitor::clean_names() |>
    dplyr::mutate(
# Simplify and standardize variable names
      sampling_program = "SEAMAP-C",
      data_provider = "NMFS Miami",
      stock = "Caribbean",
      specimen_id = "097648",
      # interview_date = lubridate::dmy(interview_date),
      # date = interview_date, 
      interview_date = as.Date(as.character(interview_date), format = c("%d/%m/%Y", "%m/%d/%Y", "%Y-%m-%d"
                                                                        )),
      month = format(as.Date(interview_date,format="%Y-%m-%d"), format = "%m"),
      day = format(as.Date(interview_date,format="%Y-%m-%d"), format = "%d"),
      year = format(as.Date(interview_date,format="%Y-%m-%d"), format = "%Y"),
# Create variable for fishery (rec or com)
      fishery = "COMMERCIAL",
    )  |> 
# select necessary variables 
    dplyr::select(sampling_program, 
                  data_provider, 
                  stock, 
                  island, 
                  specimen_id, 
                  interview_date, 
                  # date, 
                  # interview_date_new,
                  month, 
                  day, 
                  year,
                  fishery, 
                  length1_cm,
                  obs_weight_g,
                  sex_name, 
    )
  
  glimpse(stx_format)  

# date error ???  
    
# combine SEAMAP data
  seamap_c <- rbind(pr_format, stt_format, stx_format)
  
  
# Save formatted tip_spp ####
  saveRDS(
    seamap_c,
    file = here::here(
      "data",
      sedar,
      "rds",
      paste0( "seamap_c_",
              format(Sys.time(), "%Y%m%d"), 
              ".rds"
      )
    )
  )
  