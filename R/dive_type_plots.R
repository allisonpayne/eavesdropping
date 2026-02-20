library(tidyverse)

dive_type_plot <- function(dive_data, focal_dive, context_dives){
  
  focal_start <- min(dive_data$Date[dive_data$dive_id == focal_dive])
  focal_date <- format(focal_start, "%Y-%m-%d")

  focal_dive_plot <- dive_data %>% 
    dplyr::filter(dive_id == focal_dive) %>% 
    ggplot(aes(Date, Depth)) +
    geom_line(linewidth = 0.3) +
    scale_y_reverse(limits = c(800, 0)) +
    theme_bw() +
    labs(title = paste("Dive =", focal_dive, "|", focal_date), 
         xlab = "Time")
  
  # context_dive_data <- dives %>% 
  #   filter(dive_id %in% (focal_dive + context_dives))
  
  context_dive_plot <- dive_data %>% 
    dplyr::filter(dive_id %in% (focal_dive + context_dives)) %>% 
    ggplot(aes(Date, 
               Depth, 
               linewidth = dive_id == focal_dive, 
               group = 1)) +
    geom_line() +
    scale_linewidth_manual(values = c(0.3, 1)) + 
    scale_y_reverse(limits = c(800, 0)) +
    theme_bw() + 
    theme(legend.position = "none", 
          xlab(NULL))
  
  combined_plot <- grid.arrange(focal_dive_plot, 
                                context_dive_plot, 
                                heights = c(2, 1))
  
  ggsave(paste0("output/dive_images/dive", focal_dive, ".png"), 
         plot = combined_plot,
         width = 7, 
         height = 10, 
         units = "in")
  
  #clean up
  gc()
}
