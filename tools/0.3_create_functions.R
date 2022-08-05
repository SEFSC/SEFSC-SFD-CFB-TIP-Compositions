#  Create reusable extraction functions ####

  # Function to read specific LANDING_AREA_STATE_CODE and OBS_STANDARD_SPECIES_CODE
    cr_tip_sp <- function(state_codes, sp_codes) {
        tip_sp <- dbGetQuery(con, 
                      paste0("SELECT * FROM ",
                             tip_view,
                             " WHERE LANDING_AREA_STATE_CODE IN (",
                             paste(shQuote(state_codes, type="sh"), collapse=", "),
                             ")",
                             " AND OBS_STANDARD_SPECIES_CODE IN (",
                             paste(sp_codes, collapse=", "),
                             ")"))
        tip_sp_file <- paste("com_tip",
                             paste(state_codes, collapse="_"),
                             paste(sp_codes, collapse="_"),
                             format(Sys.time(),'%Y%m%d'),
                             sep="_")
        saveRDS(tip_sp, here("data", "raw", paste0(tip_sp_file, ".RData")))
    }
  
  # Function to read specific LANDING_AREA_STATE_CODE ####
    cr_tip <- function(state_codes) {
      tip <- dbGetQuery(con, 
                 paste0("SELECT * FROM ",
                        tip_view,
                        " WHERE LANDING_AREA_STATE_CODE IN (",
                        paste(shQuote(state_codes, type="sh"), collapse=", "),
                        ")"))
      tip_file <- paste("com_tip",
                       paste(state_codes, collapse="_"),
                       format(Sys.time(),'%Y%m%d'),
                       sep="_")
      saveRDS(tip, here("data", "raw", paste0(tip_file, ".RData")))
    }
  
  

