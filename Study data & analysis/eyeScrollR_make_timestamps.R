timestamp_start <- list()
timestamp_stop <- list()

for(i in seq_len(length(datasets)))
{
  ## A few subjects had smooth scrolling on, so we need to remove a few ms of
  ## data after each scroll to ensure data quality
  if(grepl("smooth", ET_files[[i]]))
  {
    wheeldown_timestamps <- (datasets[[i]] %>%
      filter(grepl("MOUSEWHEEL", Data, fixed = TRUE)))[["Timestamp"]]
    rep_idx <- rep(FALSE, nrow(datasets[[i]]))
    for(ts in wheeldown_timestamps)
    {
      rep_idx <- rep_idx | (datasets[[i]]$Timestamp >= ts & datasets[[i]]$Timestamp < (ts+125))
    }
    datasets[[i]]$Fixation.X[rep_idx] <- NA
    datasets[[i]]$Fixation.Y[rep_idx] <- NA
  }
  data <- datasets[[i]] %>% filter(grepl("WM_LBUTTONUP", Data, fixed = TRUE))
  diff <- c()
  for(j in seq_len(nrow(data)-1))
  {
    diff <- c(diff, data[[j+1, "Timestamp"]] - data[[j, "Timestamp"]])
  }
  max_diff_idx <- which(diff == max(diff))
  timestamp_start[[ET_files_names[[i]]]] <- data[[max_diff_idx, "Timestamp"]]
  timestamp_stop[[ET_files_names[[i]]]] <- data[[max_diff_idx+1, "Timestamp"]]
}
