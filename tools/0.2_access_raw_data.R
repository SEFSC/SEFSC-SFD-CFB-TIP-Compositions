# Access Oracle tables ####

  # TIP view name is protected because it contains database links
  # Ask the repo owner for the .env file
  load_dot_env(file = here("data", "tip.env"))
  dbname <- Sys.getenv("db")
  tip_view <- Sys.getenv("tip_view_name")
  
  con <- dbConnect(dbDriver("Oracle"), 
                   username = keyring::key_list(dbname)[1,2], 
                   password = keyring::key_get(dbname, keyring::key_list(dbname)[1,2]),
                   dbname = dbname)
  