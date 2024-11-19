# 02ccc_spp_size_price

# Price data in TIP - 
# Found in the TIP.landing table under column Price and 
# merged with the MV table via the mv.id = l.interview_id

# Function to read price data by LANDING_AREA_STATE_CODE  ####
cr_tip_land <- function(state_codes) {
  tip <- dbGetQuery(con, 
                    paste0("SELECT * FROM ",
                           tip_land,
                           " WHERE tmv.county_landed IN (",
                           paste(shQuote(state_codes, type="sh"), collapse=", "),
                           ")"))
  tip_file <- paste("com_tip_lan",
                    paste(state_codes, collapse="_"),
                    format(Sys.time(),'%Y%m%d'),
                    sep="_")
  saveRDS(tip, file = here("data", "raw", paste0(tip_file, ".RDS")))
}

cr_tip_land(state_codes = "ST CROIX")

# Price table
price_land = dbGetQuery(con, "SELECT 
        tmv.county_landed,
        tmv.year,
        itis_scientificname
  FROM sefsc.sedat_xref_species_nmfs_itis@SECPR_dblk.sfsc.noaa.gov")

# Species ITIS table
itis = dbGetQuery(con, "SELECT 
        tmv.county_landed,
        tmv.year,
        itis_scientificname
  FROM sefsc.sedat_xref_species_nmfs_itis@SECPR_dblk.sfsc.noaa.gov")

# SQL code 
# select tmv.county_landed, tmv.year,
# sum(count(distinct(tmv.id))) over(partition by tmv.county_landed, tmv.year) nmb_ins
# from tips.tip_mv@tips_dblk.sfsc.noaa.gov TMV
# JOIN TIPS.LANDING@TIPS_DBLK.SFSC.NOAA.GOV l on (tmv.id = l.interview_id)
# where tmv.agency_id = 32
# and l.price is not null
# and l.price <> 0
# and l.STANDARD_SPECIES_ID in (097648, 097646)
# group by tmv.county_landed, tmv.year
# order by tmv.county_landed, tmv.year;