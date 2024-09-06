# 02aa_spp_size_quantity ####
# investigate "quantity" variable - indicates number of individuals measured 
# noted in CSL EDA

# Load libraries ####
librarian::shelf(
  here, tidyverse, janitor, flextable, ggplot2
)

# Specify settings #### 
tip_spp_rds <- "prusvi_csl_spp_size_prep_20240906.rds" # rds from end of 02a script
spp_itis <- c("097648", "097646") # find on itis.gov
spp <- "csl"
isl <- c("pr", "stt", "stx")
print_spp <- "Caribbean Spiny Lobster"
print_isl <- "Puerto Rico - USVI"

# Read in formatted data ####
tip_spp <- readRDS(here::here("data", tip_spp_rds))

# View records that have "quantity" variable ####
tip_quantity <- tip_spp |> 
  filter(quantity > 1) |> 
  group_by(species_code, island, quantity) |> 
  summarise(
    .groups = "drop",
    interviews = n_distinct(id),
    records = n())

tip_quantity_total <- tip_quantity |> 
  summarise(
    .groups = "drop",
    all_interviews = sum(interviews),
    all_records = sum(records))

# create formatted table of quantity variables  
flextable(tip_quantity) |>
  theme_box() %>%
  align(align = "center", part = "all") %>%
  fontsize(size = 8, part = "all") %>%
  autofit() 

# Tabulate complete and incomplete length and weight pairs
count_lw_pairs <- tip_spp |>
  filter(quantity > 1) |> 
  dplyr::group_by(island, year, data_source, sector, length_type1, record_type) |>
  dplyr::summarize(
    .groups = "drop",
    records = n()
  )

# create formatted table of length/weight pair completion 
flextable(count_lw_pairs) |>
  theme_box() %>%
  align(align = "center", part = "all") %>%
  fontsize(size = 8, part = "all") %>%
  autofit() |>
  colformat_num(j = "year", big.mark = "")

# create variable counts 
tip_spp_count <- tip_spp |>
  filter(quantity > 1) |> 
  add_count(island) |>
  dplyr::mutate(islandn = paste0(island, " (", n, ")")) |>
  select(-n)

# Plot length values recorded by quantity ####
quantity_time <- tip_spp_count |>
  # filter(quantity > 1) |> 
  ggplot(aes(x = date, 
             y = length1_cm, 
             # color = as.factor(quantity)
             )
         ) +
  # facet_wrap(~islandn, ncol = 2) + 
  facet_wrap( ~ islandn) +
  geom_point(aes(colour = cut(quantity, c(-Inf, 5, 20, Inf)))) +
  scale_color_manual(name = "quantity",
                     values = c("(-Inf,5]" = "blue",
                                "(5,20]" = "green",
                                "(20, Inf]" = "red"),
                     labels = c("<= 5", "5 < quantity <= 20", "> 20")) +
  labs(
    x = "Year", y = "Length (cm)",
    title = "Quantity distribution of Lengths sampled",
    color = "Quantity Value",
    subtitle = paste("Total N = ", nrow(tip_spp))
  )
quantity_time
