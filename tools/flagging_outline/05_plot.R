# 05_plot

#' plot the filtered data included
#' 1 - gears across time
#' 2 - aggregated density of lengths 

# Plot gears used over time ####
gear_data <- tip_spp_count |>
  group_by(gear) %>%
  dplyr::mutate(n_id = n_distinct(id)) |>
  dplyr::filter(n_id >= 3) %>% ungroup %>%
  group_by(year, gearn) |>
  dplyr::summarize(n = n(), .groups = "drop") |>
  mutate(year = as.integer(year))

gear_by_yr <- gear_data |>
  group_by(gearn) |>
  dplyr::mutate(total_n = sum(n)) |>
  ungroup() |>
  dplyr::mutate(gearn = fct_reorder(gearn, total_n)) %>%
  ggplot(aes(x = year, y = gearn, color = gearn, size = n)) +
  geom_point() +
  labs(
    x = "Year", y = "", colour = "", shape = "",
    title = paste(isl, "Length Samples")
  ) +
  theme_bw() +
  theme(
    legend.position = "null", text = element_text(size = 15),
    title = element_text(size = 15)
  )


## Annual Density plots ####
### ALL GEARS ####
fleet_final <- length_data_1983_2022[length_data_1983_2022$fleet==1,]

fcounts = fleet_final %>%  group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
  tabyl(gear) %>%
  mutate(n_labels = paste0(gear, " (n= ", n, ")" ))

all_carSTTJ <-
  fleet_final %>%  group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
  group_by(YEAR) %>%
  dplyr::mutate(year_labs = paste0(YEAR, "\n n = ", n())) %>%
  ggplot(aes(FL_CM))+
  geom_density(linewidth = 0.75)+
  # geom_vline(data = fleet_final, aes(xintercept=mean(FL_CM)),
  # linetype="dashed", linewidth=1)+
  #scale_color_manual(values = gearcols, labels = counts$n_labels)+
  # scale_color_hue(labels=fcounts$n_labels)+
  labs(x = "Fork Length (cm)", title = paste0(county,  "\n (N = ", sum(fcounts$n), ")"))+
  facet_wrap(~year_labs, ncol = 7)
# theme_minimal()
# theme(legend.title = element_text(linewidth=14), 
# legend.text = element_text(linewidth=12))+

export_fig_page(all_carSTTJ) 
abc17 = all_carSTTJ

### TOP GEARS ####
fleet_final_gears <- length_data_gears[length_data_gears$fleet==1,]

fcounts = fleet_final_gears %>%  group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
  tabyl(gear) %>%
  mutate(n_labels = paste0(gear, " (n= ", n, ")" ))

all_car_gearsSTTJ <-
  fleet_final_gears %>%  group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
  group_by(YEAR) %>%
  dplyr::mutate(year_labs = paste0(YEAR, "\n n = ", n())) %>%
  ggplot(aes(FL_CM, color = gear))+
  geom_density(linewidth = 0.75)+
  #scale_color_manual(values = gearcols, labels = counts$n_labels)+
  scale_color_hue(labels=fcounts$n_labels)+
  labs(color = "Gear Type", x = "Fork Length (cm)", title = paste0(county,  "\n (N = ", sum(fcounts$n), ")"))+
  facet_wrap(~year_labs, ncol = 7)+
  # theme_minimal()
  guides(color=guide_legend(ncol = 2))+
  theme(legend.title = element_text(size=14), 
        legend.text = element_text(size=12),
        legend.position = "bottom")

export_fig_page(all_car_gearsSTTJ) 

abc18 = all_car_gearsSTTJ

# Aggregated cummulative density ####

counts =length_data_final %>%
  tabyl(gear) %>%
  mutate(n_labels = paste0(gear, " (n= ", n, ")" ))

abc20 = length_data_final %>%
  ggplot(aes(FL_CM))+
  stat_ecdf()+
  # scale_color_manual(values = gearcols, labels = counts$n_labels)+
  # scale_color_hue(labels = counts$n_labels)+
  labs(x = "Fork Length (cm)", title = paste0(county,  "\n (N = ", sum(counts$n), ")"))+
  theme_minimal()


