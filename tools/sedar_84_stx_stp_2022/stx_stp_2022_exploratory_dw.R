# exploratory work performed at data workshop that was not included in working paper or presentation

# QUICK LOOK INTO SEX
colnames(stx_slp)
unique(stx_slp$SEX_NAME)

stx_sex <- as.data.frame(table(stx_slp$SECONDARY_SEX, useNA='always'))
n_stx_sex = length(stx_slp$SEX_NAME)
sum_stx_sex <- stx_sex |>
  mutate(Percent = round((Freq/n_stx_sex*100), 2))

stx_slp_2012 <- stx_slp |>
   filter(YEAR>2011)
stx_sex2012 <- as.data.frame(table(stx_slp_2012$SEX_NAME, useNA='always'))

len_types <- as.data.frame(table(stx_slp$LENGTH_TYPE1, useNA='always'))

total_len_count <- len_types |>
  mutate(Percent = round((Freq/total_length*100), 2))
