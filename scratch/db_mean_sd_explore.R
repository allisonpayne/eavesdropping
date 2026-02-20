db_dives_50 <- db_dives %>% 
  mutate(recording_id = cumsum(Time - lag(Time, default = Time[1]) > 1.01) + 1,
         end_calib = recordings$end_calib[recording_id],
         dive_id = dive_summ$dive_id[find_next(end_calib, dive_summ$end)]) %>%
  group_by(dive_id) %>% 
  #only take the middle 40% of the dive
  slice(floor(n() * 0.3):floor(n() * 0.7)) %>% 
  ungroup() 


ben_travel_ids <- c(70, 76, 77, 80) #removed 74
ben_rest_ids <- c(81, 846, 889, 911, 970, 1040, 1139, 1253) 
ben_forage_ids <- c(1267, 1823, 1293, 1775, 1567)

pel_travel_v_ids <- c(89, 217, 256, 295, 345, 405, 473, 585)
pel_travel_u_ids <- c(119, 140, 223, 367, 490, 654)
pel_forage_ids <- c(203, 246, 335, 358, 410, 515, 560, 671) #removed 1198
pel_rest_ids <- c(278, 378, 458, 571, 599, 779)


benthic <- rbind(
  filter(db_dives_50, 
         dive_id %in% ben_travel_ids) %>% 
    mutate(type = "ben_travel"),
  filter(db_dives_50, 
         dive_id %in% ben_forage_ids) %>% 
    mutate(type = "ben_forage"),
  filter(db_dives_50, 
         dive_id %in% ben_rest_ids) %>% 
    mutate(type = "ben_rest")
) %>% 
  group_by(dive_id, type) %>% 
  summarize(db_mean = mean(db_1000_3500),
            db_sd = sd(db_1000_3500), 
            .groups = "drop")

ggplot(benthic, aes(db_mean, db_sd, color = type)) + 
  geom_point()
  

#plotting
dives %>% 
  filter(dive_id == 80) %>% 
  ggplot(aes(Date, Depth)) +
  geom_line(linewidth = 1) +
  scale_y_reverse() +
  theme_bw()


db_dives_50 %>% 
  filter(dive_id %in% ben_rest_ids) %>% 
  ggplot(aes(Time, db_1000_3500)) +
  geom_line() + 
  ylim(-72, -42) +
  facet_wrap(~ dive_id, scales = "free_x") +
  ggtitle("Benthic rest")

db_dives_50 %>% 
  filter(dive_id %in% ben_travel_ids) %>% 
  ggplot(aes(Time, db_1000_3500)) +
  geom_line() + 
  ylim(-72, -42) +
  facet_wrap(~ dive_id, scales = "free_x") +
  ggtitle("Benthic travel")

db_dives_50 %>% 
  filter(dive_id %in% ben_forage_ids) %>% 
  ggplot(aes(Time, db_1000_3500)) +
  geom_line() + 
  ylim(-72, -42) +
  facet_wrap(~ dive_id, scales = "free_x") +
  ggtitle("Benthic forage")

#observations
#the noise around the seal's flow noise varies substantially (like, 10 db of 
#background noise difference on the benthic rest dives). 

pelagic <- rbind(
  filter(db_dives_50, 
         dive_id %in% pel_travel_v_ids) %>% 
    mutate(type = "pel_v_travel"),
  filter(db_dives_50, 
         dive_id %in% pel_travel_u_ids) %>% 
    mutate(type = "pel_u_travel"),
  filter(db_dives_50, 
         dive_id %in% pel_forage_ids) %>% 
    mutate(type = "pel_forage"),
  filter(db_dives_50, 
         dive_id %in% pel_rest_ids) %>% 
    mutate(type = "pel_rest")
) %>% 
  group_by(dive_id, type) %>% 
  summarize(db_mean = mean(db_1000_3500),
            db_sd = sd(db_1000_3500), 
            .groups = "drop")

ggplot(pelagic, aes(db_mean, db_sd, color = type)) + 
  geom_point()

db_dives_50 %>% 
  filter(dive_id %in% pel_forage_ids) %>% 
  ggplot(aes(Time, db_1000_3500)) +
  geom_line() + 
  ylim(-72, -42) +
  facet_wrap(~ dive_id, scales = "free_x") +
  ggtitle("Pelagic forage")

db_dives_50 %>% 
  filter(dive_id %in% pel_travel_v_ids) %>% 
  ggplot(aes(Time, db_1000_3500)) +
  geom_line() + 
  ylim(-72, -42) +
  facet_wrap(~ dive_id, scales = "free_x") +
  ggtitle("Pelagic Travel v")

dives <- read_rds("output/processed_dives.rds")
dives %>% 
  filter(dive_id %in% pel_forage_ids) %>% 
  ggplot(aes(Date, Depth)) +
  geom_line() + 
  facet_wrap(~ dive_id, scales = "free_x") +
  scale_y_reverse() +
  ggtitle("Pelagic forage")

dives %>% 
  filter(dive_id %in% pel_travel_v_ids) %>% 
  ggplot(aes(Date, Depth)) +
  geom_line() + 
  facet_wrap(~ dive_id, scales = "free_x") +
  scale_y_reverse() +
  ggtitle("Pelagic travel v")

pel_travel_v_ids <- c(89, 217, 256, 295, 345, 405, 473, 585)
pel_travel_u_ids <- c(119, 140, 223, 367, 490, 654)
pel_forage_ids <- c(203, 246, 335, 358, 410, 515, 560, 671) #removed 1198
pel_rest_ids <- c(278, 378, 458, 571, 599, 779)

# data frame of typed dives
standardized_profiles <- list(pel_travel_v = pel_travel_v_ids,
     pel_travel_u = pel_travel_u_ids,
     pel_forage = pel_forage_ids,
     pel_rest = pel_rest_ids) %>% 
  map2(names(.), \(.ids, .type) tibble(type = .type, dive_id = .ids)) %>% 
  bind_rows() %>% 
  mutate(profile = map(dive_id, \(.id) select(filter(dives, dive_id == .id), Date, Depth))) %>% 
  unnest(profile) %>% 
  group_by(type, dive_id) %>% 
  reframe(t = seq(0, 1, length.out = 1e3),
          d = approx(seq(0, 1, length.out = n()), 
                     Depth / max(Depth), 
                     xout = seq(0, 1, length.out = 1e3))$y)
# differences appear clear
ggplot(standardized_profiles, aes(t, d)) +
  geom_line(aes(group = dive_id), alpha = 0.5) +
  scale_y_reverse() +
  facet_wrap(~type) +
  theme_classic()
# cluster with dtw distance
library(dendextend)
library(mclust)
library(dtw)
profile_mtx <- standardized_profiles %>% 
  pivot_wider(names_from = t, values_from = d) %>% 
  select(-type, -dive_id) %>% 
  as.matrix() 
profile_dist <- dtw::dtwDist(profile_mtx)
profile_tree <- hclust(as.dist(profile_dist))
profile_dend <- as.dendrogram(profile_tree)
types_in_order <- standardized_profiles %>% 
  pivot_wider(names_from = t, values_from = d) %>% 
  pull(type)
labels(profile_dend) <- types_in_order[labels(profile_dend)]
plot(profile_dend)
clusts <- cutree(profile_dend, k = 4)
adjustedRandIndex(types_in_order, clusts)

step_patterns <- list(
  symmetric1 = symmetric1,
  symmetric2 = symmetric2,
  asymmetric = asymmetric
)
linkage_methods <- c("ward.D2", "complete", "average")
window_sizes <- c(0, 5, 10)  # 0 = no window

results <- expand_grid(
  step = names(step_patterns),
  method = linkage_methods,
  window = window_sizes
)

results$ARI <- NA

for(i in 1:nrow(results)) {
  print(str_glue("{i}/{nrow(results)}"))
  dtw_dist <- dtwDist(
    profile_mtx, 
    step.pattern = step_patterns[[results$step[i]]],
    window.type = if(results$window[i] > 0) "sakoechiba" else "none",
    window.size = results$window[i]
  )
  
  hc <- hclust(as.dist(dtw_dist), method = results$method[i])
  pred <- cutree(hc, k = 3)
  
  results$ARI[i] <- adjustedRandIndex(types_in_order, pred)
}

optimized_dist <- dtwDist(
  profile_mtx, 
  step.pattern = asymmetric,
  window.type = "none",
  window.size = 0
)
profile_tree <- hclust(as.dist(optimized_dist))
profile_dend <- as.dendrogram(profile_tree)
labels(profile_dend) <- types_in_order[labels(profile_dend)]
plot(profile_dend)

# PSD
library(psd)
benthic_flow_noise <- rbind(
  filter(db_dives_50, 
         dive_id %in% ben_travel_ids) %>% 
    mutate(type = "ben_travel"),
  filter(db_dives_50, 
         dive_id %in% ben_forage_ids) %>% 
    mutate(type = "ben_forage"),
  filter(db_dives_50, 
         dive_id %in% ben_rest_ids) %>% 
    mutate(type = "ben_rest")
)
benthic_psd <- benthic_flow_noise %>% 
  group_by(dive_id, type) %>% 
  group_modify(\(.db, .key) {
    db_ts <- ts(.db$db_1000_3500, 
                start = as.numeric(.db$Time[1]), 
                frequency = 1)
    db_ps <- pspectrum(
      db_ts,
      ntap.init = 10,
      niter = 5
    )
    tibble(freq = db_ps$freq, spec = db_ps$spec)
  }) %>% 
  ungroup()

ggplot(benthic_psd, aes(freq, spec, group = dive_id, color = type)) +
  geom_line()

psd_mtx <- benthic_psd %>% 
  group_by(dive_id, type) %>% 
  reframe(new_freq = seq(0, 0.5, length.out = 200),
          spec = approx(freq, spec, xout = new_freq)$y,
          freq = 1:200) %>% 
  pivot_wider(names_from = freq, 
              values_from = spec, 
              id_cols = c(dive_id, type)) %>% 
  select(-dive_id, -type) %>% 
  as.matrix()

psd_dist <- dtwDist(psd_mtx)
psd_tree <- hclust(as.dist(psd_dist))
psd_dend <- as.dendrogram(psd_tree)
labels(psd_dend) <- types_in_order[labels(psd_dend)]
plot(psd_dend)

psd_summary <- benthic_psd %>% 
  filter(freq <= 0.2) %>% 
  group_by(dive_id, type) %>% 
  summarize(spec_slope = coef(lm(spec ~ freq))[2],
            spec_int = coef(lm(spec ~ freq))[1],
            .groups = "drop")
ggplot(psd_summary, aes(spec_int, spec_slope, color = type)) +
  geom_point(shape = 21)
  
library(e1071)
benthic_fuzzy <- cmeans(
  x = select(psd_summary, spec_int, spec_slope),
  centers = 3,
  iter.max = 100,
  verbose = FALSE,
  dist = "euclidean",
  method = "cmeans",
  m = 2
)

colnames(benthic_fuzzy$membership) <- c("ben_rest", "ben_forage", "ben_travel")[order(benthic_fuzzy$centers[, "spec_int"])]
psd_summary %>% 
  mutate(actual_membership = map2_dbl(row_number(), 
                                      type, 
                                      \(.i, .t) benthic_fuzzy$membership[.i, .t])) %>% 
  view()

