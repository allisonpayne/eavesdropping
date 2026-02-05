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
dive_stats <- summarize_dives(dives)

dive_prepped <- prepData(data = dive_stats, 
                         coordNames = NULL)

# Define initial parameters
nbStates <- 3  # number of hidden states

# Fit HMM
hmm_fit <- fitHMM(data = dive_prepped,
                  nbStates = nbStates,
                  dist = list(mean_depth = "gamma", 
                              n_bottom_wiggles = "gamma", 
                              bottom_duration_min = "gamma"), 
                  Par0 = list(
                    #travel, rest, forage
                    mean_depth = c(350, 400, 600, 20, 20, 20),
                    n_bottom_wiggles = c(50, 10, 150, 20, 20, 20), 
                    bottom_duration_min = c(10, 20, 15, 1, 1, 1)
                  ))  # initial parameter values

calculate_prominence <- function(x, peaks_matrix) {
  # peaks_matrix is the output from pracma::findpeaks()
  # Columns: peak_value, peak_location, peak_start, peak_end
  
  if (is.null(peaks_matrix) || nrow(peaks_matrix) == 0) {
    return(numeric(0))
  }
  
  n_peaks <- nrow(peaks_matrix)
  prominences <- numeric(n_peaks)
  
  for (i in 1:n_peaks) {
    peak_height <- peaks_matrix[i, 1]
    peak_loc <- peaks_matrix[i, 2]
    peak_start <- peaks_matrix[i, 3]
    peak_end <- peaks_matrix[i, 4]
    
    # Find the highest valley on the left side
    if (peak_start > 1) {
      left_min <- min(x[peak_start:peak_loc])
    } else {
      left_min <- x[1]
    }
    
    # Find the highest valley on the right side
    if (peak_end < length(x)) {
      right_min <- min(x[peak_loc:peak_end])
    } else {
      right_min <- x[length(x)]
    }
    
    # Prominence is the peak height minus the higher of the two valleys
    key_col <- max(left_min, right_min)
    prominences[i] <- peak_height - key_col
  }
  
  return(prominences)
}
foo <- filter(dives, dive_id == 72)
foo_peaks <- pracma::findpeaks(-foo$Depth, zero = "+")
peak_prom <- calculate_prominence(-foo$Depth, foo_peaks)
is_wiggle <- peak_prom >= 1.0
wiggles <- slice(foo, foo_peaks[is_wiggle, 2])
ggplot(foo, aes(Date, Depth)) +
  geom_line() +
  geom_point(data = wiggles, col = "red") +
  scale_y_reverse() +
  scale_x_datetime(date_labels = "%m-%d %H:%M")

# PSD of two dives - putative forage and travel
library(tuneR)
library(seewave)

wav <- readWave("/Users/allisonpayne/Local Documents/Science/Eavesdropping/Flow noise/BenthicTravel_20250221_184334.WAV")
signal <- wav@left
n <- length(signal)
start <- floor(0.25 * n)
end   <- floor(0.75 * n)
signal_mid <- signal[start:end]
signal_bp <- bwfilter(signal_mid,
                      f = fs,
                      from = 1000,
                      to = 2000,
                      bandpass = TRUE,
                      n = 4)   # filter order
fs <- wav@samp.rate
divespec <- meanspec(signal_bp, f = fs, plot = FALSE)
divespec_df <- data.frame(freq = divespec[, 1], amp = divespec[, 2])
ggplot(divespec_df, aes(freq, amp)) + 
  geom_line() +
  xlim()
