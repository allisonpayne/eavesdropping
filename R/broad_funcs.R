find_window <- function(time, start, win_hr) {
  n_window <- as.numeric(time - start, unit = "hours") %/% win_hr
  start + n_window * window_hr * 3600 + window_hr / 2 * 3600
}

sum_odon_exposure <- function(data, win_start) {
  win_dur <- window_hr * 3600
  odon_encounters <- data %>% 
    filter(start < win_start + win_dur,
           end > win_start)
  if (nrow(odon_encounters) == 0) return(0)
  
  odon_hours <- odon_encounters %>% 
    mutate(start = pmax(start, win_start),
           end = pmin(end, win_start + win_dur)) %>% 
    summarize(encounter_hours = sum(as.numeric(end - start, unit = "hours"))) %>% 
    pull(encounter_hours)
  return(odon_hours)
}
