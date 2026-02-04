library(momentuHMM)
library(tidyverse)
source(here::here("R/dives.R"))

depth <- read_csv(here::here("data/23A1302/out-Archive.csv"), 
                  show_col_types = FALSE) %>% 
  drop_na(Depth) %>% 
  mutate(Date = as.POSIXct(Time, format = "%H:%M:%S %d-%b-%Y", tz = "UTC")) %>% 
  rename(no_zoc_depth = Depth,
         Depth = `Corrected Depth`)

dives <- get_dives(depth, 5, 80) 
dive_stats <- summarize_dives(dives)

dive_prepped <- prepData(data = dive_stats, 
                         coordNames = NULL)

# Define initial parameters
nbStates <- 3  # number of hidden states

# Fit HMM
hmm_fit <- fitHMM(data = dive_prepped,
                  nbStates = nbStates,
                  dist = list(mean_depth = "gamma", 
                              n_bottom_wiggles = "gamma", 
                              bottom_duration_min = "gamma"), 
                  Par0 = list(
                    #travel, rest, forage
                    mean_depth = c(350, 400, 600, 20, 20, 20),
                    n_bottom_wiggles = c(50, 10, 150, 20, 20, 20), 
                    bottom_duration_min = c(10, 20, 15, 1, 1, 1)
                  ))  # initial parameter values


