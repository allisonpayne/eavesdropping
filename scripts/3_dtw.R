library(tidyverse)
library(dtw)

db_dives <- read_rds(here::here("output/all_db_dives_processed.rds"))

#re-calculating the dive id now that they are combined
db_dives <- db_dives %>% 
  mutate(dive_id = cumsum(Time - lag(Time, default = Time[1]) > 1.01)) %>% 
  group_by(dive_id) %>% 
  #only take the middle 50% of the dive
  slice(floor(n() / 4):floor(n() * 3 / 4)) %>% 
  ungroup()

dive_ids <- unique(db_dives$dive_id)
n_dives <- length(dive_ids)
dive_list <- db_dives %>% 
  select(dive_id, db_1000_3500) %>% 
  group_split(dive_id)

dtw_dist_mtx <- matrix(0, nrow = n_dives, ncol = n_dives)
for (i in 1:(n_dives - 1)) {
  print(str_glue("{i} / {(n_dives - 1)}"))
  for (j in (i + 1):n_dives) {
    dtw_dist_mtx[i, j] <- dtw(dive_list[[i]]$db_1000_3500, 
                              dive_list[[j]]$db_1000_3500)$distance
    dtw_dist_mtx[j, i] <- dtw_dist_mtx[i, j]
  }
}

write_rds(x = dtw_dist_mtx, 
          file = "output/dtw_dist_mtx.rds")

rownames(dtw_dist_mtx) <- dive_ids
dtw_dist <- as.dist(dtw_dist_mtx)

#plotting, seeing if this worked
plot(hclust(dtw_dist), )
clust_tree <- hclust(dtw_dist)
clusts <- cutree(clust_tree, h = 6000)

#how does it look with the dives?

dives <- read_rds(here::here("output/processed_dives.rds"))
dive_summ <- read_rds(here::here("output/processed_dive_summaries.rds"))
recording <- read_rds(here::here("output/recording_calib.rds"))


dive_summ <- dive_summ %>%    
  group_by(dive_id) 

# Associate dives between acoustics and depth profiles
dive_clusts <- db_dives %>% 
  group_by(dive_id) %>% 
  summarize(start = first(Time),
            end = last(Time)) %>% 
  mutate(
    clust = clusts,
    recording_end = recording$end_calib[
      approx(recording$end,
             seq_along(recording$end),
             xout = end,
             method = "constant",
             f = 1)$y
    ],
    dive_id = approx(dive_summ$end,
                     dive_summ$dive_id,
                     xout = recording_end,
                     method = "constant",
                     f = 1)$y)

dives %>% 
  right_join(select(dive_clusts, dive_id, clust), by = "dive_id") %>% 
  group_by(dive_id) %>% 
  mutate(elapsed = as.numeric(Date - first(Date), unit = "secs")) %>% 
  ungroup() %>% 
  ggplot(aes(elapsed, Depth, group = dive_id)) +
  geom_line(alpha = 0.2) +
  scale_y_reverse() +
  facet_wrap(~clust, scales = "free") +
  theme_classic()

#cluster 1 
dives %>% 
  filter(dive_id == 333:337) %>% 
  ggplot(aes(Date, Depth)) +
  geom_line(linewidth = 1) +
  scale_y_reverse() +
  theme_bw()

db_dives %>% 
  filter(dive_id == 333:337) %>% 
  ggplot(aes(Time, db_1000_3500)) +
  geom_line(linewidth = .5) +
  scale_y_reverse() +
  theme_bw()

#cluster 2
dives %>% 
  filter(dive_id == 710:713) %>% 
  ggplot(aes(Date, Depth)) +
  geom_line(linewidth = 1) +
  scale_y_reverse() +
  theme_bw()

#cluster 3
dives %>% 
  filter(dive_id == 12) %>% 
  ggplot(aes(Date, Depth)) +
  geom_line(linewidth = 1) +
  scale_y_reverse() +
  theme_bw()

#cluster 4
dives %>% 
  filter(dive_id == 101:105) %>% 
  ggplot(aes(Date, Depth)) +
  geom_line(linewidth = 1) +
  scale_y_reverse() +
  theme_bw()

#cluster 5
dives %>% 
  filter(dive_id == 76:79) %>% 
  ggplot(aes(Date, Depth)) +
  geom_line(linewidth = 1) +
  scale_y_reverse() +
  theme_bw()

#cluster 6
dives %>% 
  filter(dive_id == 112:115) %>% 
  ggplot(aes(Date, Depth)) +
  geom_line(linewidth = 1) +
  scale_y_reverse() +
  theme_bw()

#cluster 7
dives %>% 
  filter(dive_id == 169:175) %>% 
  ggplot(aes(Date, Depth)) +
  geom_line(linewidth = 1) +
  scale_y_reverse() +
  theme_bw()
