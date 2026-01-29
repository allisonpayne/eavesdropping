library(ggOceanMaps)
library(terra)
library(tidyverse)
library(aniMotum)
library(tidyterra)
theme_set(theme_bw())

pb25 <- read_csv(here::here("data/seal_locations.csv"),
                          show_col_types = FALSE) %>% 
  rename(Date = Date_es) %>% 
  mutate(trip = "PB") %>% 
  vect(geom = c("Longitude", "Latitude"), 
       crs = "epsg:4326")

pb_track <- as.lines(pb25)

pm25 <- read_csv(here::here("data/MoreDeployments/H391-PM2025/265936-Locations.csv"), 
                 show_col_types = FALSE) %>% 
  mutate(trip = "PM") %>% 
  select(Date, Latitude, Longitude, trip) %>% 
  vect(geom = c("Longitude", "Latitude"), 
       crs = "epsg:4326")

pm_track <- as.lines(pm25)

rod <- read_csv(here::here("data/MoreDeployments/H391-RoD/2023228_240257_aniMotum_crw_RSB.csv"), 
                show_col_types = FALSE) %>% 
  rename(Date = date, 
         Longitude = lon, 
         Latitude = lat) %>% 
  mutate(trip = "ROD") %>% 
  select(Date, Latitude, Longitude, trip) %>% 
  vect(geom = c("Longitude", "Latitude"), 
       crs = "epsg:4326")

rod_track <- as.lines(rod)

basemap(limits = c(-163, -121, 36.5, 51),
        bathymetry = TRUE,
        bathy.style = "rcb") +
  geom_spatvector(data = pm_track, color = "lightblue", linewidth = 1) +
  geom_spatvector(data = rod_track, color = "lightskyblue", linewidth = 1) +
  geom_spatvector(data = pb_track, color = "gold", linewidth = 1) +
  xlab("Longitude") + 
  ylab("Latitude") +
  theme(legend.position = "none")
