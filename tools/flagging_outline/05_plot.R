# 05_plot

#' plot the filtered data included
#' 1 - gears across time
#' 2 - aggregated density of lengths 


# Load libraries ####
librarian::shelf(here, tidyverse, flextable, ggplot2)

# Specify settings ####
tip_spp_rds <- "pr_yts_clean_tip_20240307.rds" # rds from end of 02 script
spp <- "yts"
isl <- "pr"
print_isl <- "Puerto Rico"
break_year = 2012

# Read in formatted data ####
tip_spp <- readRDS(here::here("data", tip_spp_rds))

# Plot gears by number of fish measured used over time ####
gear_data <- tip_spp |>
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

# Plot gears by number of unqiue interviews over time ####
gant_data_id <- tip_spp %>% 
  group_by(year, gear) |>
  dplyr::summarize(n_ID = n_distinct(id), .groups = "drop") |> 
  mutate(year = as.integer(year))+
  geom_vline(xintercept = (break_year - 0.5),  
             color = "darkgrey", linewidth=1.5)

gear_by_id <- gant_data_id |>
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

# Gear Density Plots ####

## Aggregated density plots ####
### overlay time periods 

full_mean = round(mean(tip_spp$length1_cm), 2)

tip_spp_2012_2022 <- tip_spp |>
  filter(year >= 2012) 

truncated_mean = round(mean(tip_spp_2012_2022$length1_cm), 2)

agr_den_NOgears <- 
  ggplot() +
  geom_density(aes(length1_cm, 
                   color = "tip_spp"),
               linewidth = 1.0, 
               alpha = .2, 
               data = tip_spp) +
  geom_density(aes(length1_cm, 
                   color = "tip_spp_2012_2022"),
               linewidth = 1.0, 
               alpha = .2, 
               data = tip_spp_2012_2022) +
  geom_vline(data = tip_spp, 
             aes(xintercept=mean(length1_cm), 
                 color = "tip_spp"),
             linetype="dashed", linewidth=1) +
  geom_vline(data=tip_spp_2012_2022, 
             aes(xintercept=mean(length1_cm), 
                 color = "tip_spp_2012_2022"),
             linetype="dashed", linewidth=1) +
  labs(x = "Fork Length (cm)", 
       title = paste0(print_isl, 
                      " aggregated length density"))+
  # scale_fill_discrete(name = "Time Series", labels = c("1983-2022", "2012-2022"))
  guides(color=guide_legend(title="Time Series"))+
  scale_color_discrete(labels=c('1983-2022', '2012-2022'))+
  theme(legend.title = element_text(size=20), 
        legend.text = element_text(size=20),
        legend.position = "bottom",
        axis.text.x=element_text(size=20),
        axis.text.y=element_text(size=20),
        axis.title.x = element_text( size = 20),
        axis.title.y = element_text( size = 20),
        title = element_text(size = 20))
# scale_fill_manual(name = "dataset", values = c(length_data_1983_2022 = "red", length_data_2012_2022 = "green"))

### GEAR INDIVIDUALS ####
tip_spp_top_gears <- tip_spp %>% # filter to gears >2% reported
  filter(gear %in% c("LINES HAND", "POTS AND TRAPS; FISH",
                     "HAUL SEINES", "BOTTOM LINE")) 

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

### 2012 top gears individuals #### 

length_data_gears_2012 <- length_data_glm_2012 %>% 
  filter(gear %in% c("LINES HAND", "POTS AND TRAPS; FISH",
                     "ROD AND REEL")) 

ycounts =length_data_gears_2012 %>% #group_by(YEAR) %>% filter(n() >= 30) %>% ungroup %>%
  tabyl("gear") %>%
  mutate(n_labels = paste0(gear, " (n= ", n, ")" ))

muv12 <- plyr::ddply(length_data_gears_2012, "gear", summarise, grp.mean=mean(FL_CM))
head(muv12)

agr_den_v12 <- length_data_gears_2012 %>% 
  # group_by(gear) %>% 
  # dplyr::mutate(n_ID = n_distinct(ID)) |> 
  # dplyr::filter(n_ID >= 3) %>% ungroup %>%
  # group_by(YEAR) %>% 
  # filter(n() >= 30) %>% ungroup %>%
  ggplot(aes(FL_CM))+
  # geom_density( aes(color = "Combined"),lwd=1.5)+
  geom_density(aes(color = gear),linewidth = 0.75)+
  scale_color_hue(labels=ycounts$n_labels)+
  # scale_color_hue(labels=c("Combined",ycounts$n_labels))+
  #scale_color_manual(values = gearcols, labels = c("Combined", counts$n_labels))+
  labs(color = "Gear" , x = "Fork Length (cm)", title = county)+ #title = paste0(county,  "\n (N = ", sum(ycounts$n), ")"))+
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
  geom_vline(data=muv12, aes(xintercept=grp.mean, color=gear),
             linetype="dashed")


abc19 = agr_den_v12


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

