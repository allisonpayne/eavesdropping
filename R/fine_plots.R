make_bout_plot <- function(bout, tpws, depth, non_recording, manual) {
  
  start <- manual$start[bout]
  end <- manual$end[bout]
  
  plot_start <- start - hours(1)
  plot_end <- end + hours(1)
  
  bout_date <- format(start, "%b %d")
  
  #filter depth data for the bout
  bout_depth <- depth %>% 
    filter(Date >= plot_start, 
           Date <= plot_end)
  
  #filter tpws
  bout_tpws <- tpws %>% 
    filter(bout.x == bout)
  
  #filter non-recording times
  bout_non_recording <- non_recording %>% 
    filter(gap_end >= plot_start, 
           gap_start <= plot_end)
  
  # Skip if no data
  if(nrow(bout_tpws) == 0) return(NULL)
  
  #plot
  tpwsplot <- ggplot(bout_tpws, aes(clicktime, MPP)) + 
    geom_rect(data = non_recording, 
              aes(xmin = gap_start,
                  xmax = gap_end, 
                  ymin = -Inf, 
                  ymax = Inf), 
              fill = "gray30", 
              alpha = 0.5, 
              inherit.aes = FALSE) +
    geom_point(color = "purple", alpha = 0.2, show.legend = NA) + 
    geom_boxplot(aes(group = dive_id), fill = NA) +
    # geom_line(aes(y = mean_MPP), color = "hotpink") +
    scale_x_datetime(limits = c(plot_start, plot_end), 
                     date_labels = "%H:M") +
    facet_wrap(~bout.x, scales = "free_y") + 
    theme_bw() +
    theme(axis.title.x = element_blank(), 
          axis.text.x = element_blank())
  
  depthplot <- ggplot(bout_depth, aes(Date, Depth)) +
    geom_line(linewidth = 0.3) +
    scale_y_reverse() +
    scale_x_datetime(limits = c(plot_start, plot_end), 
                     date_labels = "%H:%M") +
    theme_bw() + 
    labs(x = bout_date)
  
  cowplot::plot_grid(tpwsplot, depthplot, ncol = 1)
}
