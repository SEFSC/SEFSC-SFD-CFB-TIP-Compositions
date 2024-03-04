# read in conversion table
species_codes2 <- 
  read_csv("~/SEFSC-SFD-CFB-TIP-Compositions/data/CSVs/pr_species_codes_steve_conv.csv")
tip_ORGANIZED$OBS_STANDARD_SPECIES_CODE <- 
  str_remove(tip_ORGANIZED$OBS_STANDARD_SPECIES_CODE, "^0+")

# FIND UNIQUE SPECIES CODES AND # OF OCCURANCES ####

tip_unique_SPC <- tip_ORGANIZED %>%
  group_by(OBS_STANDARD_SPECIES_CODE) %>%
  tally()

tip_spc_rename <- tip_unique_SPC |>
  rename(
    COUNT_ORACLE = n,
    tip_spc = OBS_STANDARD_SPECIES_CODE
  )

tip_unique_SPC_NAMES <- tip_spc_rename %>%
  mutate(
    SPC_NAME =
      species_codes2$common[match(
        tip_spc_rename$tip_spc,
        species_codes2$sp_itis
      )]
  )


pr_hist_unique_spc <- pr_hist_date %>%
  group_by(SPECIES) %>% # if data 2000 to 2007, use SPECIES_B
  tally()
pr_hist_unique_spc_renamed <- pr_hist_unique_spc |>
  dplyr::rename(
    COUNT_HISTORICAL = n,
    hist_spc = SPECIES
  )

pr_hist_unique_SPC_NAMES <- pr_hist_unique_spc_renamed %>%
  mutate(
    SPC_NAME =
      species_codes2$common[match(
        pr_hist_unique_spc_renamed$hist_spc,
        species_codes2$sp_dner
      )]
  )

# compare species names
unique_species_merge2 <-
  merge(tip_unique_SPC_NAMES, pr_hist_unique_SPC_NAMES,
        by = "SPC_NAME",
        all = TRUE
  )

# COMPARE
SPC_comparison <- unique_species_merge %>%
  mutate(COMPARE = (COUNT_ORACLE - COUNT_HISTORICAL))

flextable(SPC_comparison) |>
  theme_box() %>%
  align(align = "center", part = "all") %>%
  fontsize(size = 8, part = "all") %>%
  autofit()