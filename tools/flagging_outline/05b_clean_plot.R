# 05_plot

#' plot the filtered data included
#' 1 - gears across time
#' 2 - aggregated density of lengths 


# Load libraries ####
librarian::shelf(here, tidyverse, flextable, ggplot2, janitor)

# Specify settings ####
tip_spp_rds <- "pr_yts_clean_filtered_20240711.rds" # rds from end of 05a script
clean_gear <- "pr_yts_clean_gear_list_20240328.rds" # gears representing >2%
spp <- "yts"
isl <- "pr"
print_isl <- "Puerto Rico"
break_year = 2012

# Read in formatted data ####
tip_spp <- readRDS(here::here("data", tip_spp_rds))
gear_2percent <- readRDS(here::here("data", clean_gear))

# Create count of observed records for each area  ####
tip_spp_count <- tip_spp |>
  add_count(county_landed) |>
  dplyr::mutate(county_landedn = paste0(county_landed, " (", n, ")")) |>
  select(-n) |>
  add_count(gear) |>
  dplyr::mutate(gearn = paste0(gear, " (", n, ")")) |>
  select(-n)|>
  add_count(island) |>
  dplyr::mutate(islandn = paste0(island, " (", n, ")")) |>
  select(-n)

# Plot gears by number of fish measured used over time ####
gear_data <- tip_spp_count |>
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
    title = paste(print_isl, "Length Samples")
  ) +
  theme_bw() +
  theme(
    legend.position = "null", text = element_text(size = 15),
    title = element_text(size = 15)
  )

abc1 = gear_by_yr

# Plot gears by number of unqiue interviews over time ####
gear_data_id <- tip_spp_count %>% 
  group_by(gear) %>% 
  dplyr::mutate(n_ID = n_distinct(id)) |> 
  dplyr::filter(n_ID >= 3) %>% ungroup %>%
  group_by(year, gear) |>
  dplyr::summarize(n_ID = n_distinct(id), .groups = "drop") |> 
  mutate(year = as.integer(year))

gear_by_id <- gear_data_id |>
  # filter(YEAR > 2011) |> 
  group_by(gear) |>
  dplyr::mutate(total_n = sum(n_ID)) |> 
  ungroup() |>   
  dplyr::mutate(gear = fct_reorder(gear, total_n)) %>%
  ggplot(aes(x = year, y = gear, color = gear, size = n_ID)) +
  geom_point()  +
  labs(x = "Year", y = "", colour = "", shape = "", 
       title = paste(print_isl, " Unique Interviews")) +
  theme_bw() + 
  theme(legend.position="null", text = element_text(size = 20), 
        title = element_text(size = 15))+
  geom_vline(xintercept = (break_year - 0.5),  
             color = "darkgrey", linewidth=1.5)

abc2 = gear_by_id

# Gear Density Plots ####

## Aggregated density plots ####
### overlay time periods 

full_mean = round(mean(tip_spp$length1_cm), 2)

tip_spp_break <- tip_spp |>
  filter(year >= break_year) 

truncated_mean = round(mean(tip_spp_break$length1_cm), 2)

agr_den_NOgears <- 
  ggplot() +
  geom_density(aes(length1_cm, 
                   color = "tip_spp"),
               linewidth = 1.0, 
               alpha = .2, 
               data = tip_spp) +
  geom_density(aes(length1_cm, 
                   color = "tip_spp_break"),
               linewidth = 1.0, 
               alpha = .2, 
               data = tip_spp_break) +
  geom_vline(data = tip_spp, 
             aes(xintercept=mean(length1_cm), 
                 color = "tip_spp"),
             linetype="dashed", linewidth=1) +
  geom_vline(data=tip_spp_break, 
             aes(xintercept=mean(length1_cm), 
                 color = "tip_spp_break"),
             linetype="dashed", linewidth=1) +
  labs(x = "Fork Length (cm)", 
       title = paste0(print_isl, 
                      " aggregated length density"))+
  guides(color=guide_legend(title="Time Series"))+
  scale_color_discrete(
    labels=c(paste0(min_year, "-", max_year), 
             paste0(break_year, "-", max_year)))+
  theme(legend.title = element_text(size=20), 
        legend.text = element_text(size=20),
        legend.position = "bottom",
        axis.text.x=element_text(size=20),
        axis.text.y=element_text(size=20),
        axis.title.x = element_text( size = 20),
        axis.title.y = element_text( size = 20),
        title = element_text(size = 20))

abc3 = agr_den_NOgears

### GEAR INDIVIDUALS ####
tip_spp_top_gears <- tip_spp %>% # filter to gears >2% reported
  filter(gear %in% gear_2percent$Gear) 

ycounts =tip_spp_top_gears %>% 
  tabyl("gear") %>%
  mutate(n_labels = paste0(gear, " (n= ", n, ")" ))

muv <- plyr::ddply(tip_spp_top_gears, 
                   "gear", 
                   summarise, 
                   grp.mean=mean(length1_cm))
head(muv)

agr_den_top_gears <- tip_spp_top_gears %>% 
  ggplot(aes(length1_cm))+
  geom_density(aes(color = gear),
               linewidth = 0.75)+
  scale_color_hue(labels=ycounts$n_labels)+
  labs(color = "Gear" , 
       x = "Fork Length (cm)", 
       title = paste0(print_isl, " relevant gears"))+ 
  #title = paste0(county,  "\n (N = ", sum(ycounts$n), ")"))+
  # theme_minimal()
  theme(legend.title = element_text(size=20), 
        legend.text = element_text(size=20),
        legend.position = "bottom",
        axis.text.x=element_text(size=20),
        axis.text.y=element_text(size=20),
        axis.title.x = element_text( size = 20),
        axis.title.y = element_text( size = 20),
        title = element_text(size = 20))+
  guides(color=guide_legend(ncol = 2))+
  geom_vline(data=muv, aes(xintercept=grp.mean, color=gear),
             linetype="dashed")

abc4 = agr_den_top_gears

### top gears after time break #### 

tip_spp_break_gears <- tip_spp_break %>% 
  filter(gear %in% c("LINES HAND", "POTS AND TRAPS; FISH",
                     "ROD AND REEL")) 

ycounts =tip_spp_break_gears %>% 
  tabyl("gear") %>%
  mutate(n_labels = paste0(gear, " (n= ", n, ")" ))

muv_break <- 
  plyr::ddply(tip_spp_break_gears, 
              "gear", summarise, grp.mean=mean(length1_cm))
head(muv_break)

agr_den_break <- tip_spp_break_gears %>% 
  ggplot(aes(length1_cm))+
  geom_density(aes(color = gear),linewidth = 0.75)+
  scale_color_hue(labels=ycounts$n_labels)+
  labs(color = "Gear" , x = "Fork Length (cm)", title = print_isl)+ 
  #title = paste0(print_isl,  "\n (N = ", sum(ycounts$n), ")"))+
  theme(legend.title = element_text(size=20), 
        legend.text = element_text(size=20),
        legend.position = "bottom",
        axis.text.x=element_text(size=20),
        axis.text.y=element_text(size=20),
        axis.title.x = element_text( size = 20),
        axis.title.y = element_text( size = 20),
        title = element_text(size = 20))+
  guides(color=guide_legend(ncol = 2))+
  geom_vline(data=muv12, aes(xintercept=grp.mean, color=gear),
             linetype="dashed")


abc5 = agr_den_break


## Annual Density plots ####

### top gears together ####
# ann_den_top_gears <- tip_spp|> 
#   filter(gear %in% gear_2percent$Gear)

# fcounts = ann_den_top_gears %>%  group_by(year) %>% filter(n() >= 30) %>% ungroup %>%
#   tabyl(gear) %>%
#   mutate(n_labels = paste0(gear, " (n= ", n, ")" ))

plot_ann_den <-
  tip_spp_top_gears %>%  group_by(year) %>% filter(n() >= 30) %>% ungroup %>%
  group_by(year) %>%
  dplyr::mutate(year_labs = paste0(year, "\n n = ", n())) %>%
  ggplot(aes(length1_cm))+
  geom_density(linewidth = 0.75)+
  # geom_vline(data = fleet_final, aes(xintercept=mean(FL_CM)),
  # linetype="dashed", linewidth=1)+
  #scale_color_manual(values = gearcols, labels = counts$n_labels)+
  # scale_color_hue(labels=fcounts$n_labels)+
  labs(x = "Fork Length (cm)", 
       title = paste0(print_isl,  "\n (N = ", sum(ycounts$n), ")"))+
  facet_wrap(~year_labs, ncol = 7)

abc6 = plot_ann_den

### top gears separated ####
# fcounts = ann_den_top_gears %>%  group_by(year) %>% filter(n() >= 30) %>% ungroup %>%
#   tabyl(gear) %>%
#   mutate(n_labels = paste0(gear, " (n= ", n, ")" ))

plot_ann_den_separate <-
  tip_spp_top_gears %>%  group_by(year) %>% filter(n() >= 30) %>% ungroup %>%
  group_by(year) %>%
  dplyr::mutate(year_labs = paste0(year, "\n n = ", n())) %>%
  ggplot(aes(length1_cm, color = gear))+
  geom_density(linewidth = 0.75)+
  #scale_color_manual(values = gearcols, labels = counts$n_labels)+
  scale_color_hue(labels=ycounts$n_labels)+
  labs(color = "Gear Type", 
       x = "Fork Length (cm)", 
       title = paste0(print_isl,  "\n (N = ", sum(ycounts$n), ")"))+
  facet_wrap(~year_labs, ncol = 7)+
  # theme_minimal()
  guides(color=guide_legend(ncol = 2))+
  theme(legend.title = element_text(size=14), 
        legend.text = element_text(size=12),
        legend.position = "bottom")

abc7 = all_car_gearsSTTJ

# Aggregated cumulative density ####

counts = tip_spp %>%
  tabyl(gear) %>%
  mutate(n_labels = paste0(gear, " (n= ", n, ")" ))

cumulative_den <- tip_spp %>%
  ggplot(aes(length1_cm))+
  stat_ecdf()+
  labs(x = "Fork Length (cm)",
       title = paste0(print_isl,  "\n (N = ", sum(counts$n), ")"))+
  theme_minimal()
 
abc20 = cumulative_den
