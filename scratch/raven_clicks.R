library(tidyverse)
source(here::here("R/find_next.R"))
source(here::here("R/non_recording.R"))
theme_set(theme_bw())

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

dive_summ <- read_rds(here::here("output/processed_dive_summaries.rds"))
dives <- read_rds(here::here("output/processed_dives.rds"))
recording <- read_rds(here::here("output/recording_calib.rds")) 
recording <- recording %>% 
  mutate(recording_id = c(1:nrow(recording)))
non_recording <- calculate_non_recording(recording) %>% 
  filter(gap_end <= last(recording$end_calib))

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

recording_summ <- recording_0227 %>% 
  group_by(dive_id) %>% 
  summarize(max_rl = quantile(RL, .95), 
            mean_pf = mean(PeakFreq), 
            count = n(),
            median_click = median(click_start))

recording_0227 %>%
  group_by(dive_id) %>% 
  mutate(n_clicks = n()) %>% 
  ggplot(aes(click_start, PeakFreq, group = dive_id)) +
  geom_point(alpha = 0.2, fill = "gray") + 
  # geom_boxplot(alpha = 0.7) +
  geom_rect(data = non_recording, 
            aes(xmin = gap_start, 
                xmax = gap_end, 
                ymin = -Inf, 
                ymax = Inf), 
            fill = "gray30", 
            alpha = 0.5, 
            inherit.aes = FALSE) + 
  scale_x_datetime(limits = range(recording_summ$median_click))

pf_plot <- recording_summ %>%
  ggplot(aes(median_click, mean_pf)) +
  geom_point(alpha = 0.5) + 
  geom_rect(data = non_recording, 
            aes(xmin = gap_start, 
                xmax = gap_end, 
                ymin = -Inf, 
                ymax = Inf), 
            fill = "gray30", 
            alpha = 0.5, 
            inherit.aes = FALSE) + 
  geom_smooth(se = FALSE) +
  scale_x_datetime(limits = range(recording_summ$median_click))

rl_plot <- recording_summ %>% 
  ggplot(aes(median_click, max_rl)) +
  geom_point(color = "hotpink", size = 3) +
  geom_point(data = recording_0227, 
             aes(x = click_start, y = RL), 
             alpha = 0.2, 
             inherit.aes = FALSE) +
  geom_rect(data = non_recording, 
            aes(xmin = gap_start, 
                xmax = gap_end, 
                ymin = -Inf, 
                ymax = Inf), 
            fill = "gray30", 
            alpha = 0.5, 
            inherit.aes = FALSE) + 
  geom_smooth(se = FALSE) +
  scale_x_datetime(limits = range(recording_summ$median_click)) + 
  labs(x = "Time", 
       y = "Received level (dB)")

dive_plot <- dives %>% 
  filter(Date >= first(recording_summ$median_click), 
         Date <= last(recording_summ$median_click)) %>% 
  ggplot(aes(Date, Depth)) + 
  geom_path() + 
  scale_y_reverse() + 
  scale_x_datetime(limits = range(recording_summ$median_click)) %>% 
  labs(x = NULL)

cowplot::plot_grid(dive_plot, rl_plot, nrow = 2)


