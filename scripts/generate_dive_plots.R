library(gridExtra)
library(tidyverse)
source(here::here("R/dive_type_plots.R"))

dives <- read_rds(here::here("output/processed_dives.rds"))
recording <- readRDS(here::here("output/recording_calib.rds"))

recording_start <- recording$start_calib[1]
recording_end <- last(recording$end_calib)

#filter dives to only when we had recordings
dives <- dives %>% 
  filter(between(Date, recording_start, recording_end))

#Generate plots and save to outputs folder
walk(unique(last_dives$dive_id),
     \(fd) {
       dive_type_plot(dives, fd, -2:2)
       Sys.sleep(0.01)  # Small delay between plots to reduce likelihood of crashing partway through (due to memory issue)
     })