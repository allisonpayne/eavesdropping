library(tidyverse)
library(ggOceanMaps)
library(terra)
library(tidyterra)
library(lubridate)
library(aniMotum)
library(geosphere)
library(momentuHMM)


track <- read_csv(here::here("data/seal_locations.csv"),
                  show_col_types = FALSE) %>% 
  mutate(lc = c("G"), 
         id = c("H391")) %>% 
  format_data(date = "Date_es", coord = c("Longitude", "Latitude")) %>% 
  as.data.frame()

page_start <- as.POSIXct("2025-02-21 06:00", tz = "US/Pacific")

ssm_steps <- tibble(
  id = "H391", 
  date = seq(page_start, max(track$date), by = 3600 * 6)
)

fit <- fit_ssm(track, 
               vmax = 3, #max travel rate 
               model = "crw", #fits correlated random walk
               time.step = ssm_steps, #6 hour steps
               control = ssm_control(verbose = 0), 
) #turns off reports

reg_locs <- grab(fit, what = "predicted")

#now I want to know approx how far she traveled every 6 hours 
track_dist <- track %>% 
  mutate(datetime = ymd_hms(date), 
         window = floor_date(datetime, "6 hours")) %>% 
  arrange(datetime)

library(geosphere)

window_dists <- track_dist %>% 
  group_by(window) %>% 
  arrange(datetime) %>% 
  mutate(
    lon_prev = lag(lon),
    lat_prev = lag(lat),
    distance = ifelse(
      is.na(lon_prev),
      0,
      distHaversine(cbind(lon_prev, lat_prev), cbind(lon, lat))
    )
  ) %>% 
  summarize(
    total_dist_km = sum(distance, na.rm = TRUE) / 1000, 
    n_obs = n(), 
    first_time = min(datetime), 
    last_time = max(datetime)
  )

mean(window_dists$total_dist_km)
quantile(window_dists$total_dist_km)

# There's one clear outlier, remove it and interpolate
outlier_idx <- which(window_dists$total_dist_km > 50)
window_dists$total_dist_km[outlier_idx] <- mean(window_dists$total_dist_km[outlier_idx + c(-1, 1)])
window_dists$dist_sd <- (window_dists$total_dist_km - mean(window_dists$total_dist_km)) / sd(window_dists$total_dist_km)

ggplot(window_dists, aes(window, total_dist_km)) + geom_point()  

mean(window_dists$total_dist_km)
quantile(window_dists$total_dist_km)
