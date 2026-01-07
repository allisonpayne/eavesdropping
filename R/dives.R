
get_dives <- function(depth, surface_threshold, bottom_percent) {
  depth %>% 
    mutate(
      surface = Depth < surface_threshold, 
      dive_change = surface & lag(!surface, default = TRUE), 
      dive_id = cumsum(dive_change)
    ) %>% 
    filter(!surface) %>%  # Remove surface intervals
    group_by(dive_id) %>% 
    mutate(max_depth = max(Depth, na.rm = TRUE), 
           bottom_thresh = max_depth * 0.80, #threshold at bottom 80% of the dive
           at_bottom = Depth >= bottom_thresh, 
           time_diff = as.numeric(difftime(Date, lag(Date), units = "secs")),
           depth_change = Depth - lag(Depth),
           direction = sign(depth_change),  # +1 = deeper, -1 = shallower
           direction_change = direction != lag(direction)  # Wiggle detected!
    ) %>%
    ungroup()
}

summarize_dives <- function(dives, min_duration_min = 5) {
  dives %>%
    group_by(dive_id) %>%
    summarize(
      start = min(Date),
      end = max(Date),
      duration_min = as.numeric(difftime(end, start, units = "mins")),
      max_depth = max(Depth),
      mean_depth = mean(Depth),
      n_records = n(),
      bottom_duration_min = sum(at_bottom, na.rm = TRUE) * mean(time_diff, na.rm = TRUE) / 60,
      n_bottom_wiggles = sum(at_bottom & direction_change, na.rm = TRUE),
      wiggles_per_min = n_bottom_wiggles / pmax(bottom_duration_min, 0.01),
      bottom_depth_range = ifelse(any(at_bottom),
                                  max(Depth[at_bottom]) - min(Depth[at_bottom]),
                                  NA),
      .groups = "drop") %>%
    filter(duration_min > min_duration_min)
}