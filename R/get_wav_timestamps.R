library(tuneR)
library(tidyverse)
library(lubridate)

wav_directory <- "/Volumes/Extreme SSD/h391"
output_directory <- "/Users/allisonpayne/Local Documents/Github/Manuscripts/eavesdropping/data"

#list of wav files 
wav_files <- list.files(wav_directory, pattern = "\\.wav$", 
                        full.names = TRUE,
                        ignore.case = TRUE)

#extract start time from the file name
extract_start_time <- function(filepath) {
  filename <- basename(filepath)
  timestamp_str <- str_extract(filename, "\\d{8}_\\d{6}")
  
  #extract timestamp
  if (is.na(timestamp_str)) {
    warning(paste("could not parse timestamp from:", filename))
    return(NA)
  }
  
  #parse timestamp
  start_time <- ymd_hms(timestamp_str, tz = "UTC")
  return(start_time)
}
  
  #get wav file duration in seconds
  get_wav_duration <- function(filepath) {
    tryCatch({
      wav <- readWave(filepath, header = TRUE)
      duration_sec <- wav$samples / wav$sample.rate
      return(duration_sec)
    }, error = function(e) {
      warning(paste("error reading wav file:", filepath, e$message))
      return(NA)
    })
  }
  
    #process files 
    results <- tibble(
      filepath = wav_files, 
      filename = basename(wav_files)
    ) %>% 
      mutate(
        start_time = map(filepath, extract_start_time), 
        duration_sec = map_dbl(filepath, get_wav_duration)
      ) %>% 
      unnest(start_time) %>% 
      mutate(
        end_time = start_time + seconds(duration_sec), 
        duration_min = duration_sec / 60
      ) %>% 
      select(filename, start_time, end_time, duration_sec, duration_min)
    
    print(results)
    
    
    #export to csv
    output_file <- file.path(output_directory, "wav_timestamps.csv")
    write_csv(results, output_file)
    