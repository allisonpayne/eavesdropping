library(tidyverse)
source(here::here("R/find_next.R"))

clicks_0227_paths <- dir("data/sw/manual_clicks/", full.names = TRUE) %>% 
  #just get feb 27 for now
  str_subset("20250227")

# This will work once all the files have the same columns
# read_delim(clicks_0227, id = "file")

# This works with variable numbers of columns
clicks_0227 <- map_dfr(clicks_0227_paths, \(click_path) {
  read_delim(click_path) %>%
    mutate(filename = paste0(str_extract(click_path, "[0-9]{8}_[0-9]{6}"), ".WAV"), 
           click_path = click_path)
})

clicks_freq_adj <- clicks_0227 %>% 
  mutate(`Low Freq (Hz)` = 3000, 
         `High Freq (Hz)` = 30000) %>% 
  select(-c(filename, `Avg Power Density (dB FS/Hz)`, `Peak Freq (Hz)`)) 

clicks_freq_adj %>% 
  group_by(click_path) %>% 
  group_walk(\(dat, group) {
    write_delim(dat, 
                file = file.path(group$click_path), 
                delim = "\t")
  })

write_delim(clicks_freq_adj)

dive_summ <- read_rds(here::here("output/processed_dive_summaries.rds"))
dives <- read_rds(here::here("output/processed_dives.rds"))
recording <- read_rds(here::here("output/recording_calib.rds")) 
recording <- recording %>% 
  mutate(recording_id = c(1:nrow(recording)))

recording_0227 <- recording %>% 
  filter(filename %in% clicks_0227$filename) %>% 
  left_join(clicks_0227, by = "filename") %>% 
  rename(PeakFreq = `Peak Freq (Hz)`, 
         RL = `Avg Power Density (dB FS/Hz)`) %>% 
  mutate(dive_id = dive_summ$dive_id[find_next(end_calib, dive_summ$end)]) %>% 
  group_by(dive_id) %>% 
  mutate(click_start = start_calib + `Begin Time (s)`, 
         ici = as.numeric(difftime(lead(click_start), 
                                   click_start, 
                                   units = "secs"))) %>% 
  ungroup()



recording_0227 %>% 
  group_by(dive_id) %>%
  ggplot(aes(click_start, ici)) +
  geom_point() + 
  ylim(0, 2)

pf_plot <- recording_0227 %>%
  group_by(dive_id) %>% 
  summarize(max_peak_freq = quantile(PeakFreq, .95), 
            median_click = median(click_start), 
            n = n()) %>% 
  ggplot(aes(median_click, max_peak_freq)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(aes(weight = n), se = FALSE)


rl_plot <- recording_0227 %>% 
  group_by(dive_id) %>% 
  summarize(max_rl = quantile(RL, .95), 
            median_click = median(click_start)) %>% 
  ggplot(aes(median_click, max_rl)) +
  geom_point(alpha = 0.5) +
  geom_smooth(se = FALSE) 

cowplot::plot_grid(pf_plot, rl_plot, nrow = 2)
