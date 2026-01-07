#function to find gaps between recordings
calculate_non_recording <- function(recording_df) {
  if(nrow(recording_df) == 0) return(tibble(gap_start = as.POSIXct(character()), 
                                            gap_end = as.POSIXct(character())))
  non_recording <- tibble()
  
  #calculate gaps 
  if(nrow(recording_df) > 1) {
    for(i in 1:(nrow(recording_df) - 1)) {
      if(recording_df$end[i] < recording_df$start[i + 1]) {
        non_recording <- bind_rows(non_recording, 
                                   tibble(gap_start = recording_df$end[i], 
                                          gap_end = recording_df$start[i + 1]))
      }
    }
  }
  return(non_recording)
}
