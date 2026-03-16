library(momentuHMM)
library(tidyverse)
source(here::here("R/dives.R"))

depth <- read_csv(here::here("data/23A1302/out-Archive.csv"), 
                  show_col_types = FALSE) %>% 
  drop_na(Depth) %>% 
  mutate(Date = as.POSIXct(Time, format = "%H:%M:%S %d-%b-%Y", tz = "UTC")) %>% 
  rename(no_zoc_depth = Depth,
         Depth = `Corrected Depth`)

dives <- get_dives(depth, 5, 80) 
dive_stats <- summarize_dives(dives) %>% 
  mutate(ID = 1)

dive_type <- read_csv(here::here("data/manual-dive-type.csv")) %>% 
  select(-c(AcousticsUsed, Notes)) %>% 
  mutate(ID = 1,
         Benthic_Pelagic_int = unclass(factor(Benthic_Pelagic, 
                                              levels = c("Benthic", 
                                                         "Pelagic"))),
         Rest_Forage_Travel_int = unclass(factor(Rest_Forage_Travel, 
                                                 levels = c("Rest", 
                                                            "Forage",
                                                            "Travel"))))

dive_prepped <- prepData(data = dive_type, 
                         coordNames = NULL)

##### 6 State Model #####
# Define initial parameters
nbStates <- 6  # number of hidden states

# Fit HMM
# order of initial states is BR, BF, BT, PR,PF, PT
one <- 0.99
zero <- function(k) (1 - one) / (k - 1)
cat_pars <- list(
  Benthic_Pelagic_int = rep(c(one, zero(2)), each = 3),
  Rest_Forage_Travel_int = c(
    # P(Rest)   for states BR, BF, BT, PR, PF, PT
    one,     zero(3), zero(3), one,     zero(3), zero(3),
    # P(Forage) for states BR, BF, BT, PR, PF, PT
    zero(3), one,     zero(3), zero(3), one,     zero(3)
    # P(Travel) implied: high for BT and PT, low elsewhere
  )
)

hmm_fit6 <- fitHMM(data = dive_prepped,
                   nbStates = nbStates,
                   dist = list(Benthic_Pelagic_int = "cat2", 
                               Rest_Forage_Travel_int = "cat3"), 
                   Par0 = cat_pars)

hmm_fit6

##### 3 state model #####
# Define initial parameters
nbStates <- 3  # number of hidden states

# Fit HMM
# order of initial states is BR, BF, BT, PR,PF, PT
one <- 0.99
zero <- function(k) (1 - one) / (k - 1)
cat_pars <- list(
  Rest_Forage_Travel_int = c(
    # P(R) for states R, F, T
    one, zero(3), zero(3),
    # P(F) for states R, F, T
    zero(3), one, zero(3)
    # P(T) implied
  )
)

hmm_fit3 <- fitHMM(data = dive_prepped,
                   nbStates = nbStates,
                   dist = list(Rest_Forage_Travel_int = "cat3"), 
                   Par0 = cat_pars,
                   stateNames = c("Rest", "Forage", "Travel"))
hmm_fit3

#version with sw click presence absence

clicks_by_dive <- read_csv(here::here("data/sw/clicks_by_dive.csv"))

hmm_fit_sw <- fitHMM(data = dive_prepped,
                   nbStates = nbStates,
                   formula = 
                   dist = list(Rest_Forage_Travel_int = "cat3"), 
                   Par0 = cat_pars,
                   stateNames = c("Rest", "Forage", "Travel"))
