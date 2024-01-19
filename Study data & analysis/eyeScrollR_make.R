eyeScrollR_make <- function()
{
  if (!dir.exists(file.path("intermediate_data", "ET"))) {dir.create(file.path("intermediate_data", "ET"), recursive = TRUE)}
  ET_files <- list.files(path=file.path("raw_data", "ET_raw"), pattern="*.csv", full.names = TRUE, recursive = TRUE)
  ET_files_names <- basename(ET_files)
  datasets <- lapply(ET_files, read_csv, comment="#", show_col_types = FALSE)
  datasets <- lapply(datasets,
                     function(x) {names(x) <- make.names(names(x))
                     return(x)
                     }
  )
  
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
      datasets[[i]]$Gaze.X[rep_idx] <- NA
      datasets[[i]]$Gaze.Y[rep_idx] <- NA
    }
    # data <- datasets[[i]] %>% filter(grepl("WM_LBUTTONUP", Data, fixed = TRUE))
    # diff <- c()
    # for(j in seq_len(nrow(data)-1))
    # {
    #   diff <- c(diff, data[[j+1, "Timestamp"]] - data[[j, "Timestamp"]])
    # }
    # max_diff_idx <- which(diff == max(diff))
    # timestamp_start[[ET_files_names[[i]]]] <- data[[max_diff_idx, "Timestamp"]]
    # timestamp_stop[[ET_files_names[[i]]]] <- data[[max_diff_idx+1, "Timestamp"]]
  }
  
  img_height <- 29913
  img_width <- 1920
  
  source("eyeScrollR_make_timestamps.R")
  
  calib_img <- readPNG("calibration_image.png")
  calibration <- scroll_calibration_auto(calib_img, 100)
  
  for (i in seq_len(length(datasets)))
  {
    d_ET <- eye_scroll_correct(eyes_data = datasets[[i]],
                               timestamp_start = timestamp_start[[ET_files_names[[i]]]],
                               timestamp_stop = max(datasets[[i]]$Timestamp),
                               image_width = img_width,
                               image_height = img_height,
                               calibration = calibration,
                               scroll_lag = get_scroll_lag(refresh_rate = 60, n_frame = 2))
      
    d_click <- d_ET %>%
      filter(grepl("BUTTONDOWN", Data, fixed = TRUE))
    d_click <- d_click %>%
      mutate(X = unlist(strsplit(Data, ";"))[seq(from=2, by=6, length.out=nrow(d_click))], Y = unlist(strsplit(Data, ";"))[seq(from=3, by=6, length.out=nrow(d_click))])
    d_click <- d_click %>%
      mutate(X.Coord = as.numeric(unlist(strsplit(X, ":"))[seq(from=2, by=2, length.out=nrow(d_click))]), Y.Coord = as.numeric(unlist(strsplit(Y, ":"))[seq(from=2, by=2, length.out=nrow(d_click))])+Scroll-calibration$top_left_y) %>%
      ## Button coordinates 
      filter(Y.Coord>29700, Y.Coord<29820, X.Coord>780, X.Coord<1135)
    
    d_ET <- d_ET %>% filter(Timestamp < d_click$Timestamp[1]) %>%
      select(Timestamp, Corrected.Gaze.X, Corrected.Gaze.Y, Fixation.Start, Fixation.End, Fixation.Duration, Corrected.Fixation.X, Corrected.Fixation.Y) %>%
      filter(!is.na(Corrected.Gaze.Y) | !is.na(Corrected.Fixation.Y))
    
    write_csv(d_ET, file = file.path("intermediate_data", "ET", ET_files_names[i]), na = "")
  }
}
