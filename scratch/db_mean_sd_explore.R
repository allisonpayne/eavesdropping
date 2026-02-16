db_dives_50 <- db_dives %>% 
  mutate(recording_id = cumsum(Time - lag(Time, default = Time[1]) > 1.01) + 1,
         end_calib = recordings$end_calib[recording_id],
         dive_id = dive_summ$dive_id[find_next(end_calib, dive_summ$end)]) %>%
  group_by(dive_id) %>% 
  #only take the middle 50% of the dive
  slice(floor(n() * 0.25):floor(n() * 0.75)) %>% 
  ungroup() 

rbind(
  filter(db_dives_50, 
         between(Time, 
                 lubridate::ymd_hms("2025-02-21 18:43:34", tz = "Etc/UTC"), 
                 lubridate::ymd_hms("2025-02-21 18:52:30", tz = "Etc/UTC"))) %>% 
    mutate(type = "ben_travel"),
  filter(db_dives_50, 
         between(Time, 
                 lubridate::ymd_hms("2025-03-23 09:10:03", tz = "Etc/UTC"), 
                 lubridate::ymd_hms("2025-03-23 09:29:24", tz = "Etc/UTC"))) %>% 
    mutate(type = "ben_forage"),
  filter(db_dives_50, 
         between(Time, 
                 lubridate::ymd_hms("2025-03-22 21:04:34", tz = "Etc/UTC"), 
                 lubridate::ymd_hms("2025-03-22 21:25:56", tz = "Etc/UTC"))) %>% 
    mutate(type = "ben_rest")
) %>% 
  group_by(type) %>% 
  summarize(mean(db_1000_3500),
            sd(db_1000_3500))
  

#get 10 of each category, use dive ID, see if the mean and sd holds up