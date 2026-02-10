#Read and process the TDR data. Should have two outputs: 
#1. Depth profile. Each row is a depth record.
#2. Dive summary. Each row is a dive.

library(tidyverse)
source("R/dives.R")

depth <- read_csv(here::here("data/23A1302/out-Archive.csv"), 
                  show_col_types = FALSE) %>% 
  drop_na(Depth) %>% 
  mutate(Date = as.POSIXct(Time, format = "%H:%M:%S %d-%b-%Y", tz = "UTC")) %>% 
  rename(no_zoc_depth = Depth,
         Depth = `Corrected Depth`)

write_rds(x = depth, file = "output/processed_depth_profile.rds")

dives <- get_dives(depth, 5, 80)
dive_stats <- summarize_dives(dives)

write_rds(x = dives, file = "output/processed_dives.rds")
write_rds(x = dive_stats, file = "output/processed_dive_summaries.rds")
