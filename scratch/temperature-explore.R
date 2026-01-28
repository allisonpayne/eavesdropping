library(tidyverse)

sst <- read_csv(here::here("data/23A1302/out-SST.csv"))

sst <- sst %>% 
  mutate(
    datetime = as.POSIXct(Date, 
               format = "%H:%M:%S %d-%b-%Y", 
               tz = "UTC")
  ) %>% 
  filter(datetime < "2025-04-01 06:00:00")

ggplot(sst, aes(datetime, Temperature)) + 
  geom_point() + 
  ylab("Sea surface temperature (C)") + 
  theme_bw()

#mixed layer temperature
mlt <- read_csv(here::here("data/23A1302/out-MixLayer.csv"))

mlt <- mlt %>% 
  mutate(datetime = as.POSIXct(Date, 
                        format = "%H:%M:%S %d-%b-%Y", 
                        tz = "UTC")) %>% 
  filter(datetime < "2025-04-01 06:00:00")

ggplot(mlt, aes(datetime, MLTave)) + 
  geom_point() + 
  ylab("Average mixed layer temperature (C)") + 
  theme_bw()

ggplot(mlt, aes(datetime, TempMin)) + 
  geom_point() + 
  ylab("Minimum temperature (C)") + 
  theme_bw()

ggplot(mlt, aes(MLTave, SSTAve)) + 
  geom_point()
