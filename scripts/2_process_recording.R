#Read and process the acoustic data. Should have two outputs: 
#3. Recording summary. Each row is a recording.
#4. Acoustic profile. Each row is 1s of the LTSA.

library(tidyverse)
source(here::here("R/dive_db.R"))

recording <- read.csv(here::here("data/wav_timestamps.csv")) %>% 
  mutate(start = ymd_hms(start_time, tz = "UTC"),
         end = ymd_hms(end_time, tz = "UTC")) %>% 
  drop_na(end)

write_rds(x = recording, file = "output/recording_summary.rds")

#read in the daily ltsa files 10 at a time (otherwise we run out of memory)
db_dives_1_10 <- dir(here::here("data/flownoise/full_ltsa"), 
                     full.names = TRUE)[c(1:10)] %>% 
  read_csv()
write_rds(x = db_dives_1_10, file = "output/ltsa_1_10.rds")

db_dives_11_20 <- dir(here::here("data/flownoise/full_ltsa"), 
                     full.names = TRUE)[c(11:20)] %>% 
  read_csv()
write_rds(x = db_dives_11_20, file = "output/ltsa_11_20.rds")

db_dives_21_30 <- dir(here::here("data/flownoise/full_ltsa"), 
                      full.names = TRUE)[c(21:30)] %>% 
  read_csv()
write_rds(x = db_dives_21_30, file = "output/ltsa_21_30.rds")

db_dives_31_38 <- dir(here::here("data/flownoise/full_ltsa"), 
                      full.names = TRUE)[c(31:38)] %>% 
  read_csv()
write_rds(x = db_dives_31_38, file = "output/ltsa_31_38.rds")

#read in each of the full rds files and process them
db_dives_1_10 <- read_rds(here::here("output/ltsa_1_10.rds"))
db_dives_1_10_processed <- db_dives_1_10 %>% 
  rename(Time = 1) %>% 
  select(1, `1000`:`3500`) %>% 
  pivot_longer(-1, names_to = "freq", values_to = "db") %>% 
  mutate(freq = parse_number(freq),
         pressure = 10^(db / 10)) %>% 
  group_by(Time) %>% 
  summarize(pressure_1000_3500 = sum(pressure)) %>% 
  mutate(db_1000_3500 = 10 * log10(pressure_1000_3500), 
         dive_id = cumsum(Time - lag(Time, default = Time[1]) > 1.01))
write_rds(x = db_dives_1_10_processed, 
          file = "output/db_dives_1_10_processed.rds")

db_dives_11_20 <- read_rds(here::here("output/ltsa_11_20.rds"))
db_dives_11_20_processed <- db_dives_11_20 %>% 
  rename(Time = 1) %>% 
  select(1, `1000`:`3500`) %>% 
  pivot_longer(-1, names_to = "freq", values_to = "db") %>% 
  mutate(freq = parse_number(freq),
         pressure = 10^(db / 10)) %>% 
  group_by(Time) %>% 
  summarize(pressure_1000_3500 = sum(pressure)) %>% 
  mutate(db_1000_3500 = 10 * log10(pressure_1000_3500), 
         dive_id = cumsum(Time - lag(Time, default = Time[1]) > 1.01))
write_rds(x = db_dives_11_20_processed, 
          file = "output/db_dives_11_20_processed.rds")

db_dives_21_30 <- read_rds(here::here("output/ltsa_21_30.rds"))
db_dives_21_30_processed <- db_dives_21_30 %>% 
  rename(Time = 1) %>% 
  select(1, `1000`:`3500`) %>% 
  pivot_longer(-1, names_to = "freq", values_to = "db") %>% 
  mutate(freq = parse_number(freq),
         pressure = 10^(db / 10)) %>% 
  group_by(Time) %>% 
  summarize(pressure_1000_3500 = sum(pressure)) %>% 
  mutate(db_1000_3500 = 10 * log10(pressure_1000_3500), 
         dive_id = cumsum(Time - lag(Time, default = Time[1]) > 1.01))
write_rds(x = db_dives_21_30_processed, 
          file = "output/db_dives_21_30_processed.rds")

db_dives_31_38 <- read_rds(here::here("output/ltsa_31_38.rds"))
db_dives_31_38_processed <- db_dives_31_38 %>% 
  rename(Time = 1) %>% 
  select(1, `1000`:`3500`) %>% 
  pivot_longer(-1, names_to = "freq", values_to = "db") %>% 
  mutate(freq = parse_number(freq),
         pressure = 10^(db / 10)) %>% 
  group_by(Time) %>% 
  summarize(pressure_1000_3500 = sum(pressure)) %>% 
  mutate(db_1000_3500 = 10 * log10(pressure_1000_3500), 
         dive_id = cumsum(Time - lag(Time, default = Time[1]) > 1.01))
write_rds(x = db_dives_31_38_processed, 
          file = "output/db_dives_31_38_processed.rds")

#combine all the processed files
db_dives_1_10_processed <- read_rds(here::here("output/db_dives_1_10_processed.rds"))
db_dives_11_20_processed <- read_rds(here::here("output/db_dives_11_20_processed.rds"))
db_dives_21_30_processed <- read_rds(here::here("output/db_dives_21_30_processed.rds"))
db_dives_31_38_processed <- read_rds(here::here("output/db_dives_31_38_processed.rds"))

all_db_dives_processed <- rbind(db_dives_1_10_processed, 
                            db_dives_11_20_processed, 
                            db_dives_21_30_processed, 
                            db_dives_31_38_processed)

write_rds(x = all_db_dives_processed, 
          file = "output/all_db_dives_processed.rds")

#looks normal 
all_db_dives_processed %>% ggplot(aes(Time)) + geom_histogram()
