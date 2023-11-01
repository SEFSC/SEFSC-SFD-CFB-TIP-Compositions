# CREATE FUNCTION for retrieving oracle data by year 

cr_tip_yr <- function(state_codes, year) {
  tip_sp <- dbGetQuery(con,
                       paste0("SELECT * FROM ",
                              tip_view,
                              " WHERE LANDING_AREA_STATE_CODE IN (",
                              paste(shQuote(state_codes, type="sh"), collapse=", "),
                              ")",
                              " AND YEAR IN (",
                              paste(year, collapse=", "),
                              ")"))
  tip_sp_file <- paste("com_tip",
                       paste(state_codes, collapse="_"),
                       paste(year, collapse="_"),
                       format(Sys.time(),'%Y%m%d'),
                       sep="_")
  saveRDS(tip_sp, file = here("data", "raw", paste0(tip_sp_file, ".RDS")))
}

# If you ever want to select year for a range of years, I would suggest doing the following:
  
  cr_tip_yr2 <- function(state_codes, st_year, end_year) {
    tip_sp <- dbGetQuery(con,
                         paste0("SELECT * FROM ",
                                tip_view,
                                " WHERE LANDING_AREA_STATE_CODE IN (",
                                paste(shQuote(state_codes, type="sh"), collapse=", "),
                                ")",
                                " AND YEAR BETWEEN ",
                                paste(st_year, "AND", end_year)))
                         tip_sp_file <- paste("com_tip",
                                              paste(state_codes, collapse="_"),
                                              st_year, end_year,
                                              format(Sys.time(),'%Y%m%d'),
                                              sep="_")
                         saveRDS(tip_sp, file = here("data", "raw", paste0(tip_sp_file, ".RDS")))
  }
  