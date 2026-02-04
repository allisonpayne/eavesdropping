library(tidyverse)
theme_set(theme_bw())

audio_travel <- read_csv(here::here("data/flownoise/benthic-travel.csv")) 

dive_db <- function(data) {
  data_longer <- pivot_longer(data, -1, names_to = "freq", values_to = "db") %>% 
    mutate(freq = parse_number(freq)) %>% 
    filter(between(freq, 1000, 3500)) %>% 
    mutate(pressure = 10^(db / 10)) %>% 
    group_by(Time) %>% 
    summarize(pressure_1000_3500 = sum(pressure)) %>% 
    mutate(db_1000_3500 = 10 * log10(pressure_1000_3500))
  
  ggplot(data_longer, aes(Time, db_1000_3500)) +
    geom_line() + 
    ylim(-75, -40)
}

dive_db(audio_travel)


#pivot longer so that the variable isn't encoded as column names

travel_longer <- pivot_longer(audio_travel, -1, names_to = "freq", values_to = "db") %>% 
  mutate(freq = parse_number(freq)) %>% 
  filter(between(freq, 1000, 3500)) %>% 
  mutate(pressure = 10^(db / 10)) %>% 
  group_by(Time) %>% 
  summarize(pressure_1000_3500 = sum(pressure)) %>% 
  mutate(db_1000_3500 = 10 * log10(pressure_1000_3500))

ggplot(travel_longer, aes(Time, db_1000_3500)) +
  geom_line() + 
  ylim(-75, -40)

audio_forage <- read_csv(here::here("data/flownoise/benthic-forage.csv")) %>% 
  rename(Time = 1)

#pivot longer so that the variable isn't encoded as column names

forage_longer <- pivot_longer(audio_forage, -1, names_to = "freq", values_to = "db") %>% 
  mutate(freq = parse_number(freq)) %>% 
  filter(between(freq, 1000, 3500)) %>% 
  mutate(pressure = 10^(db / 10)) %>% 
  group_by(Time) %>% 
  summarize(pressure_1000_3500 = sum(pressure)) %>% 
  mutate(db_1000_3500 = 10 * log10(pressure_1000_3500))

ggplot(forage_longer, aes(Time, db_1000_3500)) +
  geom_line()


audio_rest <- read_csv(here::here("data/flownoise/benthic-rest.csv")) %>% 
  rename(Time = 1)

#pivot longer so that the variable isn't encoded as column names

rest_longer <- pivot_longer(audio_rest, -1, names_to = "freq", values_to = "db") %>% 
  mutate(freq = parse_number(freq)) %>% 
  filter(between(freq, 1000, 3500)) %>% 
  mutate(pressure = 10^(db / 10)) %>% 
  group_by(Time) %>% 
  summarize(pressure_1000_3500 = sum(pressure)) %>% 
  mutate(db_1000_3500 = 10 * log10(pressure_1000_3500))

ggplot(rest_longer, aes(Time, db_1000_3500)) +
  geom_line()

audio_pel_for <- read_csv(here::here("data/flownoise/pelagic-forage.csv")) %>% 
  rename(Time = 1)

#pivot longer so that the variable isn't encoded as column names

pelfor_longer <- pivot_longer(audio_pel_for, -1, names_to = "freq", values_to = "db") %>% 
  mutate(freq = parse_number(freq)) %>% 
  filter(between(freq, 1000, 3500)) %>% 
  mutate(pressure = 10^(db / 10)) %>% 
  group_by(Time) %>% 
  summarize(pressure_1000_3500 = sum(pressure)) %>% 
  mutate(db_1000_3500 = 10 * log10(pressure_1000_3500))

ggplot(pelfor_longer, aes(Time, db_1000_3500)) +
  geom_line()

audio_pel_utrav <- read_csv(here::here("data/flownoise/pelagic-u-travel.csv")) %>% 
  rename(Time = 1)

#pivot longer so that the variable isn't encoded as column names

pelutrav_longer <- pivot_longer(audio_pel_utrav, -1, names_to = "freq", values_to = "db") %>% 
  mutate(freq = parse_number(freq)) %>% 
  filter(between(freq, 1000, 3500)) %>% 
  mutate(pressure = 10^(db / 10)) %>% 
  group_by(Time) %>% 
  summarize(pressure_1000_3500 = sum(pressure)) %>% 
  mutate(db_1000_3500 = 10 * log10(pressure_1000_3500))

ggplot(pelutrav_longer, aes(Time, db_1000_3500)) +
  geom_line()

audio_pel_vtrav <- read_csv(here::here("data/flownoise/pelagic-v-travel.csv")) %>% 
  rename(Time = 1)

#pivot longer so that the variable isn't encoded as column names

pelvtrav_longer <- pivot_longer(audio_pel_vtrav, -1, names_to = "freq", values_to = "db") %>% 
  mutate(freq = parse_number(freq)) %>% 
  filter(between(freq, 1000, 3500)) %>% 
  mutate(pressure = 10^(db / 10)) %>% 
  group_by(Time) %>% 
  summarize(pressure_1000_3500 = sum(pressure)) %>% 
  mutate(db_1000_3500 = 10 * log10(pressure_1000_3500))

ggplot(pelvtrav_longer, aes(Time, db_1000_3500)) +
  geom_line()
audio_fn <- audio %>% 
  select(Time, `1000`, `1100`) %>% 
  group_by(Time) %>% 
  #converting to sound pressure 
  mutate(`1000` = 10^(`1000`/10), 
         `1100` = 10^(`1100`/10)) %>% 
  summarize(s = sum(`1000`, `1100`)) %>% 
  ungroup() %>% 
  summarize(mean(s), sd(s))

audio_pel_rest <- read_csv(here::here("data/flownoise/pelagic-rest.csv")) %>% 
  rename(Time = 1)

#pivot longer so that the variable isn't encoded as column names

pelrest_longer <- pivot_longer(audio_pel_rest, -1, names_to = "freq", values_to = "db") %>% 
  mutate(freq = parse_number(freq)) %>% 
  filter(between(freq, 1000, 3500)) %>% 
  mutate(pressure = 10^(db / 10)) %>% 
  group_by(Time) %>% 
  summarize(pressure_1000_3500 = sum(pressure)) %>% 
  mutate(db_1000_3500 = 10 * log10(pressure_1000_3500))

ggplot(pelrest_longer, aes(Time, db_1000_3500)) +
  geom_line()
audio_fn <- audio %>% 
  select(Time, `1000`, `1100`) %>% 
  group_by(Time) %>% 
  #converting to sound pressure 
  mutate(`1000` = 10^(`1000`/10), 
         `1100` = 10^(`1100`/10)) %>% 
  summarize(s = sum(`1000`, `1100`)) %>% 
  ungroup() %>% 
  summarize(mean(s), sd(s))



