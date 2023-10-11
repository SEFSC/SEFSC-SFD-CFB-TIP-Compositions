##Load packages
librarian::shelf(tidyverse, here)

# Create output folders if they do not exist yet ####
  if (!dir.exists(here("data"))){ dir.create(here("data")) }
  if (!dir.exists(here("data", "raw"))){ dir.create(here("data", "raw")) }
