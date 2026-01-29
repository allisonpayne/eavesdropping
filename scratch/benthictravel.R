library(tidyverse)
library(ggOceanMaps)
library(terra)
library(tidyterra)
library(lubridate)
library(geosphere)

track <- read_csv(here::here("data/seal_locations.csv"),
                 show_col_types = FALSE)

benthic_start <- as.POSIXct("2025-03-02 16:00:00")
benthic_end <- as.POSIXct("2025-03-30 18:00:00")

track <- track %>% 
  filter(Date_es > benthic_start & Date_es < benthic_end) 

track_sf <- track %>% 
  vect(geom = c("Longitude", "Latitude"), 
       crs = "epsg:4326")

trackline <- as.lines(track_sf)

basemap(limits = c(-125.5, -124, 41.5, 43.5),
        bathymetry = TRUE,
        bathy.style = "rcb", 
        crs = 4326) +
  geom_spatvector(data = trackline, color = "gold", linewidth = 1) +
  geom_point(data = track, aes(Longitude, Latitude), color = "darkorange") +
  xlab("Longitude") + 
  ylab("Latitude") +
  theme(legend.position = "none")

#now I want to know approx how far she traveled every 6 hours i guess?
track_dist <- track %>% 
  mutate(datetime = ymd_hms(Date_es), 
         window = floor_date(datetime, "12 hours")) %>% 
  arrange(datetime)

window_dists <- track_dist %>% 
  group_by(window) %>% 
  arrange(datetime) %>% 
  mutate(distance = if(n() > 1) {
    c(0, sapply(2:n(), function(i) {
      distHaversine(
        cbind(Longitude[i-1], Latitude[i-1]),
        cbind(Longitude[i], Latitude[i])
    )}
    ))}
    else { 
      0
      }) %>% 
  summarize(total_dist_km = sum(distance) / 1000, 
                                n_obs = n(), 
                                first_time = min(datetime), 
                                last_time = max(datetime))

ggplot(window_dists, aes(window, total_dist_km)) + geom_point()  
