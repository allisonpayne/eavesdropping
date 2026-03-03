library(tidyverse)
library(e1071)
library(psd)
library(dtw)

db_dives <- read_rds(here::here("output/all_db_dives_processed.rds"))
recording <- read_rds(here::here("output/recording_calib.rds"))
dive_summ <- read_rds(here::here("output/processed_dive_summaries.rds"))
dive_types <- read_csv(here::here("data/manual-dive-type.csv"))
benthic_ids <- filter(dive_types, Benthic_Pelagic == "Benthic") %>% 
  select(diveID)

find_next <- function(x, y) {
  # Find the index of the next value in y following x
  approx(y, 
         seq_along(y),
         method = "constant",
         f = 1, 
         rule = 2,
         xout = x)$y
}

# Just pick out the bottom
depth <- readRDS("output/processed_dives.rds")
benthic_depth <- depth %>% 
  semi_join(benthic_ids, by = c(dive_id = "diveID")) %>% 
  group_by(dive_id) %>% 
  filter(Depth >= max(Depth) - 5) %>% 
  summarize(benthos_begin = first(Date),
            benthos_end = last(Date))

db_dives_50 <- db_dives %>% 
  mutate(recording_id = cumsum(Time - lag(Time, default = Time[1]) > 1.01) + 1,
         end_calib = recording$end_calib[recording_id],
         dive_id = dive_summ$dive_id[find_next(end_calib, dive_summ$end)]) %>%
  left_join(benthic_depth, by = "dive_id") %>% 
  filter(Time >= benthos_begin,
         Time <= benthos_end)
  # group_by(dive_id) %>% 
  # cut 5 minutes from beginning and end
  # slice(floor(n() * 0.2):floor(n() * 0.8)) %>%
  # ungroup() 

benthic_flow_noise <- filter(db_dives_50, dive_id %in% benthic_ids$diveID)

benthic_psd <- benthic_flow_noise %>% 
  group_by(dive_id) %>% 
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

benthic_psd %>% 
  group_by(dive_id) %>% 
  summarize(spec_int = sum(spec * 0.5 / n())) %>% 
  ggplot(aes(spec_int)) +
  geom_density()
ggplot(benthic_psd, aes(freq, spec, group = dive_id)) +
  geom_line(alpha = 0.1) +
  scale_y_log10()

psd_summary <- benthic_psd %>% 
  filter(freq <= 0.1) %>% 
  group_by(dive_id) %>% 
  summarize(spec_slope = coef(lm(log(spec) ~ freq))[2],
            spec_int = coef(lm(log(spec) ~ freq))[1],
            .groups = "drop")
ggplot(psd_summary, aes(spec_int, spec_slope)) +
  geom_hex()

#but it doesn't keep the dive id...? the number of dive ids and the 
#number of items that were clustered are off by 2
benthic_fuzzy <- cmeans(
  x = select(psd_summary, spec_int, spec_slope),
  centers = 3,
  iter.max = 100,
  verbose = FALSE,
  dist = "euclidean",
  method = "cmeans",
  m = 2
)

cluster_names <- c("rest", "forage", "travel")[order(benthic_fuzzy$centers[,1])]
benthic_membership <- cbind(
  psd_summary,
  set_names(as_tibble(benthic_fuzzy[["membership"]]), cluster_names)
) %>% 
  mutate(cluster = factor(benthic_fuzzy$cluster,
                          labels = cluster_names),
         confidence = pmax(forage, travel, rest))

centers <- as_tibble(benthic_fuzzy$centers) %>% 
  mutate(cluster = cluster_names)
ggplot(benthic_membership, aes(spec_int, spec_slope)) +
  geom_hex() +
  geom_point(data = centers,
             shape = 21,
             size = 3,
             color = "white",
             fill = "#8AC51B") +
  scale_fill_distiller(palette = "RdPu") +
  theme_classic()

benthic_membership %>% 
  left_join(select(dive_summ, dive_id, start), by = "dive_id") %>% 
  arrange(start) %>% 
  ggplot(aes(start, cluster)) +
  geom_line(aes(group = 1)) +
  geom_point(aes(color = confidence)) +
  scale_color_distiller(palette = "RdPu") +
  theme_classic()

benthic_membership %>% 
  filter(cluster == "travel",
         confidence >= 0.95) %>% 
  slice_sample(n = 10)

depth <- readRDS("output/processed_dives.rds")
benthic_depth <- depth %>% 
  semi_join(benthic_ids, by = c(dive_id = "diveID"))

db_dives_50 %>% 
  filter(dive_id == 1196) %>% 
  ggplot(aes(Time, db_1000_3500)) +
  geom_line() +
  scale_y_continuous(limits = c(-75, -40))
.db <- filter(db_dives_50, dive_id == 1196)
db_ts <- ts(.db$db_1000_3500, 
            start = as.numeric(.db$Time[1]), 
            frequency = 1)
db_ps <- pspectrum(
  db_ts,
  ntap.init = 10,
  niter = 5
)
plot(db_ps$freq, db_ps$spec, type = "l", xlim = c(0, 0.5))
abline(lm(db_ps$spec[1:113] ~ db_ps$freq[1:113], ))
summary(lm(db_ps$spec[1:113] ~ db_ps$freq[1:113], ))


