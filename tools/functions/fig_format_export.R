
##Define functions for figures
myTheme <- function(fig){
  fig + 
    #scale_y_continuous(breaks = scales::pretty_breaks(2), limits = c(0, NA)) +
    #scale_x_continuous(limits = c(0,35), breaks = seq(0,35,10))+
    theme(axis.ticks = element_line(colour = "black"),
          title = element_text(family = "Times",
                               size = 14),
          axis.title = element_text(family = "Times",
                                    size = 12),
          axis.text = element_text(family = "Times",
                                   size = 12,
                                   colour = "black"),
          axis.text.x = element_text(family = "Times"),
          legend.position="bottom",
          legend.text = element_text(size = 12,
                                     family = "Times"),
          legend.title = element_blank())
}

export_fig_page <- function(fig){
  ggsave(filename = paste0(deparse(substitute(fig)),".png"),
         plot = fig,
         device = "png",
         path =  "./figures",
         width = 8,
         height = 9)
}


export_fig <- function(fig){
  ggsave(filename = paste0(deparse(substitute(fig)),".png"),
         plot = fig,
         device = "png",
         path =  "./figures",
         width = 8,
         height = 5.8)
}

export_fig_small <- function(fig){
  ggsave(filename = paste0(deparse(substitute(fig)),".png"),
         plot = fig,
         device = "png",
         path =  "./figures",
         width = 5,
         height = 4)
}
