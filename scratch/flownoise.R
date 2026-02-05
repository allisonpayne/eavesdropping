library(tidyverse)
library(cowplot)
source(here::here("R/dive_db.R"))
theme_set(theme_bw())

ben_travel <- read_csv(here::here("data/flownoise/benthic-travel.csv")) 
ben_forage <- read_csv(here::here("data/flownoise/benthic-forage.csv")) %>% 
  rename(Time = 1)
ben_rest <- read_csv(here::here("data/flownoise/benthic-rest.csv")) %>% 
  rename(Time = 1)
pel_forage <- read_csv(here::here("data/flownoise/pelagic-forage.csv")) %>% 
  rename(Time = 1)
pel_trav_u <- read_csv(here::here("data/flownoise/pelagic-u-travel.csv")) %>% 
  rename(Time = 1)
pel_rest <- read_csv(here::here("data/flownoise/pelagic-rest.csv")) %>% 
  rename(Time = 1)
pel_trav_v <- read_csv(here::here("data/flownoise/pelagic-v-travel.csv")) %>% 
  rename(Time = 1)

ben_travel_plot <- dive_db(ben_travel)
ben_forage_plot <- dive_db(ben_forage)
ben_rest_plot <- dive_db(ben_rest)
pel_forage_plot <- dive_db(pel_forage)
pel_u_travel_plot <- dive_db(pel_trav_u)
pel_v_travel_plot <- dive_db(pel_trav_v)
pel_rest_plot <- dive_db(pel_rest)

plot_grid(ben_travel_plot, ben_forage_plot, ben_rest_plot, 
          pel_forage_plot, pel_u_travel_plot, pel_v_travel_plot, pel_rest_plot)
