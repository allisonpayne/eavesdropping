library(tidyverse)
source(here::here("R/dives.R"))

depth <- read_csv(here::here("data/23A1302/out-Archive.csv"), 
                  show_col_types = FALSE) %>% 
  drop_na(Depth) %>% 
  mutate(Date = as.POSIXct(Time, format = "%H:%M:%S %d-%b-%Y", tz = "UTC")) %>% 
  rename(no_zoc_depth = Depth,
         Depth = `Corrected Depth`)

recording <- read.csv(here::here("data/wav_timestamps.csv")) %>% 
  mutate(start = ymd_hms(start_time, tz = "UTC"),
         end = ymd_hms(end_time, tz = "UTC")) %>% 
  drop_na(end)

recording_start <- recording$start[1]
recording_end <- last(recording$end)

depth <- depth %>% 
  filter(between(Date, recording_start, recording_end))

dives <- get_dives(depth, 5, 80)


# estimate clock offset/drift
trigger_depth <- 40
asc_trigger <- depth$Date[lead(depth$Depth, default = 0) < trigger_depth &
                            depth$Depth >= trigger_depth]
nearest_trigger <- asc_trigger[
  sapply(recording$end, \(t) {
    which.min(abs(as.numeric(t - asc_trigger, unit = "secs")))
  })
]
recording_elapsed <- as.numeric(recording$end - depth$Date[1], unit = "secs")
trigger_error <- as.numeric(nearest_trigger - recording$end, unit = "secs")
drift_model <- lm(trigger_error ~ recording_elapsed)
drift_model
recording_calib <- recording %>% 
  mutate(elapsed = as.numeric(end - depth$Date[1], unit = "secs"),
         drift = predict(drift_model, newdata = recording),
         start_calib = start + drift,
         end_calib = end + drift) %>% 
  select(-elapsed)

p <- ggplot(depth, aes(Date, Depth)) +
  geom_rect(aes(xmin = start, xmax = end, ymin = 1, ymax = 999),
            recording_calib,
            fill = "orchid3",
            alpha = 0.5,
            inherit.aes = FALSE) +
  geom_rect(aes(xmin = start_calib, xmax = end_calib, ymin = 1, ymax = 999),
            recording_calib,
            fill = "forestgreen",
            alpha = 0.5,
            inherit.aes = FALSE) +
  geom_line(color = "cornflowerblue") +
  scale_y_reverse() +
  theme_minimal()
plotly::ggplotly(p)

saveRDS(recording_calib, here::here("output/recording_calib.rds"))
