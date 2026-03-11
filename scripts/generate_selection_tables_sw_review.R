library(tidyverse)

input_csv  <- here::here("data/sw/DB100_clicks_with_audiofiles.csv")
output_dir <- here::here("data/sw/raven/")
max_peak_freq <- 20   # only keep clicks with peak_freq_Hz <= this value

# Load and filter data
message("Reading: ", input_csv)
clicks <- read.csv(input_csv, stringsAsFactors = FALSE)
clicks <- clicks[clicks$peak_freq_Hz <= max_peak_freq, ]
message(sprintf("%d click(s) retained after filtering (peak_freq_Hz <= %g)", nrow(clicks), max_peak_freq))

# Create output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  message("Created output directory: ", output_dir)
}

# Split by audio file and write one selection table per file
audio_files <- unique(clicks$audio_file)
message(sprintf("Writing selection tables for %d audio file(s)...", length(audio_files)))

for (af in audio_files) {
  
  # Subset clicks for this audio file
  sub <- clicks[clicks$audio_file == af, ]
  n   <- nrow(sub)
  
  # Build the selection table -- preset values
  # We are artificially creating "boxes" that are offset by 1.5 around each click  
  table_out <- data.frame(
    "Selection"      = seq_len(n),
    "View"           = "Spectrogram 1",
    "Channel"        = 1L,
    "Begin Time (s)" = round(sub$offset_seconds - 0.025, 4),
    "End Time (s)"   = round(sub$offset_seconds + 0.025, 4),
    "Low Freq (Hz)"  = 1000,
    "High Freq (Hz)" = 40000,
    check.names      = FALSE,
    stringsAsFactors = FALSE
  )
  
  # Build output filename: e.g. 20250227_030627.Table.1.selections.txt
  base_name <- sub("\\.WAV$", "", af, ignore.case = TRUE)
  out_file  <- file.path(output_dir, paste0(base_name, ".Table.1.selections.txt"))
  
  # Write tab-delimited file (Raven Pro format)
  write.table(table_out,
              file      = out_file,
              sep       = "\t",
              row.names = FALSE,
              quote     = FALSE)
}

message("Done! ", length(audio_files), " selection table(s) written to: ", output_dir)
