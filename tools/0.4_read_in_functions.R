#  Create reusable read in functions ####

# Create function for finding most recent extraction
get_file_date <- function(file_location, file_pattern) {
  # Find out the date of the most recent extraction
  file_date <- max(as.numeric(gsub(
    ".*?([0-9]+).RDS*", "\\1",
    list.files(file_location,
      pattern = file_pattern
    )
  )))
  return(file_date)
}

# Create function to check if extraction needs to be updated
check_date <- function(max_date) {
  date_limit <- Sys.Date() - 14
  date_extraction <- (as.Date(paste0(max_date), format = "%Y%m%d"))
  date_diff <- (date_extraction + 14) - Sys.Date()
  ifelse(date_limit >= date_extraction,
    "Warning: Most recent extraction is over two weeks old.",
    paste0("Extraction update suggested in ", date_diff, " days.")
  )
}


format_for_r <- function(data) {
  data_use <- data |>
    dplyr::mutate(
      YEAR = as.numeric(YEAR),
      isl = dplyr::case_when(
        STATE_LANDED == "PUERTO RICO" ~ "pr",
        STATE_LANDED == "VIRGIN ISLANDS" &
          COUNTY_LANDED %in% c("ST JOHN", "ST THOMAS") ~ "sttj",
        STATE_LANDED == "VIRGIN ISLANDS" &
          COUNTY_LANDED == "ST CROIX" ~ "stx",
        .default = "not coded"
      )
    ) |>
    dplyr::mutate(
      OBS_STANDARD_SPECIES_CODE =
        tryCatch(as.numeric(OBS_STANDARD_SPECIES_CODE),
          error = function(e) {
            return(NA)
          }
        ),
      LENGTH1_CM = tryCatch(LENGTH1_MM / 10, error = function(e) {
        return(NA)
      })
    )
  return(data_use)
}
