library(tidyverse)
library(dtw)

###read in the flow noise measurements for all dives and depth profiles
db_dives <- read_rds(here::here("output/all_db_dives_processed.rds"))
dives <- read_rds(here::here("output/processed_dives.rds"))

###assign dive ids 
dive_summ <- read_rds(here::here("output/processed_dive_summaries.rds"))
recordings <- read_rds(here::here("output/recording_calib.rds"))

find_next <- function(x, y) {
  # Find the index of the next value in y following x
  approx(y, 
         seq_along(y),
         method = "constant",
         f = 1, 
         rule = 2,
         xout = x)$y
}
db_dives_75 <- db_dives %>% 
  mutate(recording_id = cumsum(Time - lag(Time, default = Time[1]) > 1.01) + 1,
         end_calib = recordings$end_calib[recording_id],
         dive_id = dive_summ$dive_id[find_next(end_calib, dive_summ$end)]) %>%
  group_by(dive_id) %>% 
  #only take the middle 75% of the dive
  slice(floor(n() * 0.125):floor(n() * 0.875)) %>% 
  ungroup() 

###create distance matrix for flow noise using dynamic time warping
dive_ids <- unique(db_dives_75$dive_id)
dive_ids <- dive_ids[floor(seq(500, 2000, length.out = 50))] #DELETEME
n_dives <- length(dive_ids)
dive_list <- db_dives_75 %>% 
  filter(dive_id %in% dive_ids) %>% #DELETEME
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
dtw_dist_db <- as.dist(dtw_dist_mtx)

write_rds(x = dtw_dist_db, 
          file = "output/dtw_dist_mtx_75.rds")

###create distance matrix for dive shape using dynamic time warping
dive_profile_list <- dives %>% 
  semi_join(db_dives_75, by = "dive_id") %>% 
  filter(dive_id %in% dive_ids) %>% #DELETEME
  group_by(dive_id) %>% 
  mutate(depth_std = Depth / max(Depth)) %>% 
  ungroup() %>% 
  select(dive_id, depth_std) %>% 
  group_split(dive_id)
dtw_depth_mtx <- matrix(0, nrow = n_dives, ncol = n_dives)
for (i in 1:(n_dives - 1)) {
  print(str_glue("{i} / {(n_dives - 1)}"))
  for (j in (i + 1):n_dives) {
    dtw_depth_mtx[i, j] <- dtw(dive_profile_list[[i]]$depth_std, 
                               dive_profile_list[[j]]$depth_std)$distance
    dtw_depth_mtx[j, i] <- dtw_depth_mtx[i, j]
  }
}
rownames(dtw_depth_mtx) <- dive_ids
dtw_depth <- as.dist(dtw_depth_mtx)

write_rds(x = dtw_depth, 
          file = "output/dtw_dist_mtx_depth.rds")

###standardize and combine the distance matrices 
w <- c(0.8, 0.2) # equally weighted
dtw_depth_std <- dtw_depth / max(dtw_depth)
dtw_dist_db_std <- dtw_dist_db / max(dtw_dist_db)
combined_dtw <- w[1] * dtw_depth_std + w[2] * dtw_dist_db_std
combined_tree <- hclust(combined_dtw)
clusts <- cutree(combined_tree, k = 8)
dive_clust <- dive_summ %>% 
  filter(dive_id %in% dive_ids) %>% #DELETEME
  mutate(clust = clusts)

# and plot
dives %>% 
  right_join(select(dive_clust, dive_id, clust), 
             by = "dive_id",
             relationship = "many-to-one") %>% 
  group_by(dive_id) %>% 
  mutate(elapsed = as.numeric(Date - first(Date), unit = "secs"),
         elapsed_std = elapsed / max(elapsed),
         depth_std = Depth / max(Depth)) %>% 
  ungroup() %>% 
  ggplot(aes(elapsed_std, depth_std, group = dive_id)) +
  geom_line(alpha = 0.2) +
  scale_y_reverse() +
  facet_wrap(~clust) +
  theme_classic()
db_dives_75 %>% 
  right_join(select(dive_clust, dive_id, clust), 
             by = "dive_id",
             relationship = "many-to-one") %>% 
  group_by(dive_id) %>% 
  mutate(elapsed = as.numeric(Time - first(Time), unit = "secs"),
         elapsed_std = elapsed / max(elapsed)) %>% 
  ungroup() %>% 
  ggplot(aes(elapsed_std, db_1000_3500, group = dive_id)) +
  geom_line(alpha = 0.2) +
  facet_wrap(~clust) +
  theme_classic()
db_dives_75 %>% 
  right_join(select(dive_clust, dive_id, clust), 
             by = "dive_id",
             relationship = "many-to-one") %>% 
  group_by(dive_id) %>% 
  mutate(elapsed = as.numeric(Time - first(Time), unit = "secs"),
         elapsed_std = elapsed / max(elapsed)) %>% 
  ungroup() %>%
  group_by(dive_id, clust, epoch = elapsed %/% 10) %>% 
  summarize(db_mean = mean(db_1000_3500),
            db_sd = sd(db_1000_3500),
            elapsed_std = first(elapsed_std),
            .groups = "drop") %>% 
  ggplot(aes(elapsed_std, group = dive_id)) +
  geom_line(aes(y = db_mean), color = "firebrick", alpha = 0.2) +
  # geom_line(aes(y = db_sd), color = "cornflowerblue") +
  facet_wrap(~clust) +
  theme_classic()


###visualize with clustering

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
