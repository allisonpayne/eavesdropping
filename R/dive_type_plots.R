dive_type_plot <- function(dive_data, focal_dive, context_dives){
  
  focal_dive_plot <- dive_data %>% 
    filter(dive_id == focal_dive) %>% 
    ggplot(aes(Date, Depth)) +
    geom_line(linewidth = 0.3) +
    scale_y_reverse() +
    theme_bw() +
    labs(title = paste("Dive =", focal_dive))
  
  context_dive_data <- dives %>% 
    filter(dive_id %in% (focal_dive + context))
  
  context_dive_plot <- dive_data %>% 
    filter(dive_id %in% (focal_dive + context_dives)) %>% 
    ggplot(aes(Date, 
               Depth, 
               linewidth = dive_id == focal_dive, 
               group = 1)) +
    geom_line() +
    scale_linewidth_manual(values = c(0.3, 1)) + 
    scale_y_reverse() +
    theme_bw() + 
    theme(legend.position = "None")
  
  combined_plot <- grid.arrange(focal_dive_plot, context_dive_plot)
  
  ggsave(paste0("output/dive_images/dive", focal_dive, ".png"), 
         plot = combined_plot,
         width = 7, 
         height = 5, 
         units = "in")
}
