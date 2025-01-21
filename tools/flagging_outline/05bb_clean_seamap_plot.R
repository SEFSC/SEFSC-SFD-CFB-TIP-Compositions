# 05bb_clean_seamap_plot

# Load libraries ####
    librarian::shelf(here, tidyverse, flextable, ggplot2, janitor)

# specify settings

# pr
  #   isl <- "pr"
  #   print_isl <- "Puerto Rico"
  # # date from end of 05a script 
  #   tip_date <- "20241119"
# stt
  #   isl <- "stt"
  #   print_isl <- "St. Thomas/St. John"
  # # date from end of 05a script
  #   tip_date <- "20241108"
# stx    
    isl <- "stx"
    print_isl <- "St. Croix"
  # date from end of 05a script
    tip_date <- "20241108"
    
    seamap_date <- "20250103"
    target_len <- "Carapace Length"
    print_spp <- "Caribbean Spiny Lobster"
    spp <- "csl"
    sedar <- "sedar91"

# Read in formatted data from 1aa and 5a ####
    seamap_rds <- paste0("seamap_c_", seamap_date, ".rds")
    seamap_c <- readRDS(here::here("data",  sedar, "rds", seamap_rds))
    
    tip_spp_rds <- paste0(isl, "_", spp, "_clean_filtered_", tip_date, ".rds" )
    tip_spp <- readRDS(here::here("data",  sedar, "rds", spp, isl, tip_spp_rds))
    
# filter seamap to isl    
    seamap_len <- seamap_c |> 
      filter(!is.na(length1_cm),
             island == isl)

# calculate mean of all lengths  
    seamap_mean = round(mean(seamap_len$length1_cm), 2)
    full_mean = round(mean(tip_spp$length1_cm), 2)
    
# create density plot of all lengths comparing the full and truncated time periods
    seamap_compare_agr <- 
      ggplot() +
      geom_density(aes(length1_cm, 
                       color = "tip_spp"),
                   linewidth = 1.0, 
                   alpha = .2, 
                   data = tip_spp) +
      geom_density(aes(length1_cm, 
                       color = "seamap_len"),
                   linewidth = 1.0, 
                   alpha = .2, 
                   data = seamap_len) +
      geom_vline(data = tip_spp, 
                 aes(xintercept=mean(length1_cm), 
                     color = "tip_spp"),
                 linetype="dashed", linewidth=1) +
      geom_vline(data=seamap_len, 
                 aes(xintercept=mean(length1_cm), 
                     color = "seamap_len"),
                 linetype="dashed", linewidth=1) +
      labs(x = paste0(target_len, " (cm)"), 
           title = paste0(print_isl, " ", print_spp,
                          " Aggregated Length Density"))+
      guides(color=guide_legend(title="Source-Mean"))+
      scale_color_discrete(
        labels=c(paste("SEAMAP-C", seamap_mean),
                 paste("Trip Interview Program", full_mean) 
                 ))+
      theme(legend.title = element_text(size=15), 
            legend.text = element_text(size=15),
            legend.position = "bottom",
            axis.text.x=element_text(size = 15),
            axis.text.y=element_text(size = 15),
            axis.title.x = element_text( size = 15),
            axis.title.y = element_text( size = 15),
            title = element_text(size = 12))
# view  
    seamap_compare_agr
# save  
    ggsave(filename = 
             here::here("data", sedar, "figure", spp, isl, "seamap_compare_agr.png"),
           width = 14, height = 8)

# save figure under alphanumeric identifier    
    abc30 = seamap_compare_agr 

    
# redo with just years available in SEAMAP ####
    
# filter tip to target years
    unique(seamap_len$year)
# specify seamap-c years
    # target_years <- "2021-2023" #pr
    # target_years <- "2022" #stt
    target_years <- "2022-2023" #stx
    
    tip_spp_yrs <- tip_spp |> 
      # dplyr::filter(year %in% c(2022, 2023)) #stx
      dplyr::filter(year %in% c(2022)) #stt
      # dplyr::filter(year %in% c(2021, 2022, 2023)) #pr
 

       
# calculate mean of target years  
    target_mean = round(mean(tip_spp_yrs$length1_cm), 2) 
    
# create density plot of all lengths comparing the full and truncated time periods
    seamap_compare_agr_target <- 
      ggplot() +
      geom_density(aes(length1_cm, 
                       color = "tip_spp_yrs"),
                   linewidth = 1.0, 
                   alpha = .2, 
                   data = tip_spp_yrs) +
      geom_density(aes(length1_cm, 
                       color = "seamap_len"),
                   linewidth = 1.0, 
                   alpha = .2, 
                   data = seamap_len) +
      geom_vline(data = tip_spp_yrs, 
                 aes(xintercept=mean(length1_cm), 
                     color = "tip_spp_yrs"),
                 linetype="dashed", linewidth=1) +
      geom_vline(data=seamap_len, 
                 aes(xintercept=mean(length1_cm), 
                     color = "seamap_len"),
                 linetype="dashed", linewidth=1) +
      labs(x = paste0(target_len, " (cm)"), 
           title = paste(print_isl, print_spp,
                          "Aggregated Length Density", target_years ))+
      guides(color=guide_legend(title="Source-Mean"))+
      scale_color_discrete(
        labels=c(paste("SEAMAP-C", seamap_mean),
                 paste("Trip Interview Program", target_mean) 
        ))+
      theme(legend.title = element_text(size=15), 
            legend.text = element_text(size=15),
            legend.position = "bottom",
            axis.text.x=element_text(size = 15),
            axis.text.y=element_text(size = 15),
            axis.title.x = element_text( size = 15),
            axis.title.y = element_text( size = 15),
            title = element_text(size = 12))
# view  
    seamap_compare_agr_target
  # save  
    ggsave(filename = 
             here::here("data", sedar, "figure", spp, isl, "seamap_compare_agr_target.png"),
           width = 14, height = 8)
    
# save figure under alphanumeric identifier    
    abc31 = seamap_compare_agr_target 
    