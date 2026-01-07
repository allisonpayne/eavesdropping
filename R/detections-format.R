library(tidyverse)

acou <- read_csv(here::here("data/detections.csv"),
                 show_col_types = FALSE) %>% 
  mutate(start = as.POSIXct(start), 
         end = as.POSIXct(end))

page_start <- as.POSIXct("2025-02-21 06:00", tz = "US/Pacific")

window_hr <- 12

find_window <- function(time, start, win_hr) {
  n_window <- as.numeric(time - start, unit = "hours") %/% win_hr
  start + n_window * window_hr * 3600 + window_hr / 2 * 3600
}

acou_agg <- acou %>% 
  mutate(window = find_window(start, page_start, window_hr)) %>% 
  group_by(window) 
