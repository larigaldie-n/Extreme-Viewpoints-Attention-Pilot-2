extract_dwell_times <- function(dataset, size, start_coordinate, shift_coordinate, x_left, x_right, y_size)
{
  dwell_time <- c()
  
  pb <- txtProgressBar(min = -1, max = size-1, style = 3, width = 50, char = "=")
  
  for (j in (seq_len(size) - 1))
  {
    d_match <- dataset %>%
      mutate(Match = ifelse(Corrected.Gaze.X >= x_left & Corrected.Gaze.X <= x_right & Corrected.Gaze.Y >= floor(start_coordinate + shift_coordinate * j) & Corrected.Gaze.Y <= floor(start_coordinate + shift_coordinate * j) + y_size & !is.na(Corrected.Gaze.Y), TRUE, FALSE))
    match_first <- (d_match %>%
      filter(Match == TRUE) %>% first())[["Timestamp"]]
    match_last <- (d_match %>%
      filter(Match == TRUE) %>% last())[["Timestamp"]]
    d_match <- d_match %>% filter(Timestamp <= match_last,
                                  Timestamp >= match_first)
      acc <- 0
      ts <- NA
      
      for(i in seq_len(nrow(d_match)))
      {
        if(d_match[[i, "Match"]] == TRUE)
        {
          if(!is.na(ts))
          {
            acc <- acc + (d_match[[i, "Timestamp"]] - ts)
          }
          else
          {
            ## Some hits will be missed because samples are not continuous.
            ## The refresh rate is 120Hz (one image every 8.333ms), so on
            ## average, we can consider that any period of viewing started 
            ## (1/2)*8.333 earlier, and ended (1/2)*8.333 later. iMotions does
            ## this correction for fixation durations, but we have to do it
            ## manually since we consider all gaze data. This also allows us to
            ## not have a 0ms time for a single quick gaze (saccade) in the area
            acc <- acc + 8.333
          }
          ts <- d_match[[i, "Timestamp"]]
        }
        else
        {
          ts <- NA
        }
      }
      dwell_time <- c(dwell_time, acc)
    setTxtProgressBar(pb, j)
  }
  return(dwell_time)
}

datasets_merge <- function()
{
  if (!dir.exists(file.path("final_data"))) {dir.create(file.path("final_data"))}
  start_coordinate <- 1
  shift_coordinate <- 296
  x_left <- 248
  x_right <- 1673
  y_shift <- 260
  
  ET_files         <- list.files(file.path("intermediate_data", "ET"), pattern="*.csv", full.names = TRUE)
  d_eye_tracking   <- lapply(ET_files, read_csv, comment="#", show_col_types = FALSE)
  ET_files_names <- basename(ET_files)
  responses_files  <- list.files(file.path("raw_data", "responses"), pattern="*.csv", full.names = TRUE)
  d_responses      <- lapply(responses_files, read_csv, comment="#", show_col_types = FALSE)
  
  for (i in seq_len(length(d_eye_tracking)))
  {
    cat(paste(" File:", ET_files_names[[i]]))
    d_responses[[i]]$Dwell.Time <- extract_dwell_times(d_eye_tracking[[i]], max(d_responses[[i]]$Order), start_coordinate, shift_coordinate, x_left, x_right, y_shift)
    write.csv(d_responses[[i]], file = file.path("final_data", ET_files_names[i]), row.names = FALSE)
  }
  cat("\nDone!")
}
