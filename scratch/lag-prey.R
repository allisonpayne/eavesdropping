library(tidyverse)
cctd <- read_csv("~/Downloads/Bizzarro et al. California Current Trophic Database (CCTD)/CCTD.csv")
lags <- filter(cctd, 
               predator_sci_name %in% c(
                 "Lagenorhynchus obliquidens",
                 ""
               ))

tibble()
