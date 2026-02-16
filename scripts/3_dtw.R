library(tidyverse)
library(dtw)

db_dives <- read_rds(here::here("output/all_db_dives_processed.rds"))

#re-calculating the dive id now that they are combined
db_dives_75 <- db_dives %>% 
  mutate(dive_id = cumsum(Time - lag(Time, default = Time[1]) > 1.01)) %>% 
  group_by(dive_id) %>% 
  #only take the middle 80% of the dive
  slice(floor(n() * 0.125):floor(n() * 0.875)) %>% 
  ungroup()

dive_ids <- unique(db_dives_75$dive_id)
n_dives <- length(dive_ids)
dive_list <- db_dives_75 %>% 
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

rownames(dtw_dist_mtx) <- dive_ids
dtw_dist <- as.dist(dtw_dist_mtx)

write_rds(x = dtw_dist, 
          file = "output/dtw_dist_mtx_75.rds")

#how does it look with the dives?

dives <- read_rds(here::here("output/processed_dives.rds"))
dive_summ <- read_rds(here::here("output/processed_dive_summaries.rds"))
recording <- read_rds(here::here("output/recording_calib.rds"))
dtw_dist_75 <- read_rds(here::here("output/dtw_dist_mtx_75.rds"))
# REMOVE ME after rerunning the distance matrix code above
rownames(dtw_dist_75) <- dive_ids
dtw_dist_75 <- as.dist(dtw_dist_75)

rownames(dtw_dist_mtx) <- dive_ids
dtw_dist <- as.dist(dtw_dist_mtx)

clust_tree <- hclust(dtw_dist_75)
clusts <- cutree(clust_tree, k = 8)

# Associate dives between acoustics and depth profiles
dive_clusts <- db_dives_75 %>% 
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
             f = 1,
             rule = 2)$y
    ],
    dive_id = approx(dive_summ$end,
                     dive_summ$dive_id,
                     xout = recording_end,
                     method = "constant",
                     f = 1,
                     rule = 2)$y)

# drop second recording from within a dive (e.g. due to inversion)
dive_clusts <- dive_clusts %>% 
  group_by(dive_id) %>% 
  slice(1) %>% 
  ungroup()

# Run depth profile DTW
dive_profile_list <- dives %>% 
  semi_join(dive_clusts, by = "dive_id") %>% 
  group_by(dive_id) %>% 
  mutate(depth_std = Depth / max(Depth)) %>% 
  ungroup() %>% 
  select(dive_id, depth_std) %>% 
  group_split(dive_id)
n_profiles <- length(dive_profile_list)
dtw_depth_mtx <- matrix(0, nrow = n_profiles, ncol = n_profiles)
for (i in 1:(n_profiles - 1)) {
  print(str_glue("{i} / {(n_profiles - 1)}"))
  for (j in (i + 1):n_profiles) {
    dtw_depth_mtx[i, j] <- dtw(dive_profile_list[[i]]$depth_std, 
                               dive_profile_list[[j]]$depth_std)$distance
    dtw_depth_mtx[j, i] <- dtw_depth_mtx[i, j]
  }
}
rownames(dtw_depth_mtx) <- unique(dive_clusts$dive_id)
dtw_depth <- as.dist(dtw_depth_mtx)

# Standardize distance matrices, combine, and apply clustering
w <- c(0.5, 0.5) # equally weighted
dtw_depth_std <- dtw_depth / max(dtw_depth)
dtw_dist_75_std <- dtw_dist_75 / max(dtw_dist_75)
combined_dtw <- w[1] * dtw_depth_std + w[2] * dtw_dist_75_std
combined_tree <- hclust(combined_dtw)
clusts <- cutree(combined_tree, k = 12)

# Max wants to check this when DTW on depth profiles finish
dive_clusts$clust <- clusts

dives %>% 
  right_join(select(dive_clusts, dive_id, clust), 
             by = "dive_id",
             relationship = "many-to-one") %>% 
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

db_dives %>% 
  filter(dive_id == 710:713) %>% 
  ggplot(aes(Time, db_1000_3500)) +
  geom_line(linewidth = .5) +
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
