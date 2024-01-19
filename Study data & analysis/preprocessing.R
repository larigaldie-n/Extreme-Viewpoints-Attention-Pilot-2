exclude <- function()
{
  if (!file.exists("exclusion_log.txt")) {file.create("exclusion_log.txt")}
  if (!dir.exists(file.path("excluded_data", "ET_raw", "smooth"))) {dir.create(file.path("excluded_data", "ET_raw", "smooth"), recursive = TRUE)}
  if (!dir.exists(file.path("excluded_data", "responses"))) {dir.create(file.path("excluded_data", "responses"))}
  ET_files <- list.files(path=file.path("raw_data", "ET_raw"), pattern="*.csv", full.names = TRUE, recursive = TRUE)
  lapply(ET_files, function(x) {
    filename <- basename(x)
    path <- x
    response_file  <- file.path("raw_data", "responses", filename)
    
    bindata <- setdiff(strsplit(readBin(x, character(), n = 1), ',', fixed=TRUE)[[1]], "")
    index <- which(bindata == "\r\n#Gaze Calibration") + 1
    calibration_result <- tolower(bindata[index])
    
    if(!(file.exists(response_file)))
    {
      if(grepl("smooth", x, fixed = TRUE))
      {
        path <- file.path("excluded_data", "ET_raw", "smooth", filename)
      }
      else
      {
        path <- file.path("excluded_data", "ET_raw", filename)
      }
      cat(paste(filename, "Reason: No associated response file", sep=", "), file="exclusion_log.txt", sep="\n", append=TRUE)
    }
    else if(calibration_result == "poor")
    {
      if(grepl("smooth", x, fixed = TRUE))
      {
        path <- file.path("excluded_data", "ET_raw", "smooth", filename)
      }
      else
      {
        path <- file.path("excluded_data", "ET_raw", filename)
      }
      cat(paste(filename, "Reason: Poor calibration", sep=", "), file="exclusion_log.txt", sep="\n", append=TRUE)
      file.rename(response_file, file.path("excluded_data", "responses", filename))
    }
    else
    {
      res <- read_csv(response_file, show_col_types = FALSE)
      if(!(is.element("Agreement", names(res))))
      {
        if(grepl("smooth", x, fixed = TRUE))
        {
          path <- file.path("excluded_data", "ET_raw", "smooth", filename)
        }
        else
        {
          path <- file.path("excluded_data", "ET_raw", filename)
        }
        cat(paste(filename, "Reason: Responses absent from the response file (participant did not finish study)", sep=", "), file="exclusion_log.txt", sep="\n", append=TRUE)
        file.rename(response_file, file.path("excluded_data", "responses", filename))
      }
      else
      {
        d_ET <- read_csv(x, comment = "#", show_col_types = FALSE)
        max_ts <- max(d_ET$Timestamp)
        d_freq <- d_ET %>% filter(!is.na(Gaze.X))
        
        ## Careful analysis of the datasets showed that only space strokes after the first 2 minutes were done during the period of interest
        d_keys <- d_ET %>% filter(Timestamp > 120000, grepl("Space", Data, fixed = TRUE))
        
        # Get clicks on the right edge of the screen (scroll bar)
        d_click <- d_ET %>%
          filter(Timestamp > 60000, grepl("BUTTONDOWN", Data, fixed = TRUE))
        d_click <- d_click %>%
          mutate(X = unlist(strsplit(Data, ";"))[seq(from=2, by=6, length.out=nrow(d_click))])
        d_click <- d_click %>%
          mutate(X.Coord = as.numeric(unlist(strsplit(X, ":"))[seq(from=2, by=2, length.out=nrow(d_click))])) %>%
          filter(X.Coord > 1900)
        
        
        if(nrow(d_freq)/(max_ts/1000)<60)
        {
          if(grepl("smooth", x, fixed = TRUE))
          {
            path <- file.path("excluded_data", "ET_raw", "smooth", filename)
          }
          else
          {
            path <- file.path("excluded_data", "ET_raw", filename)
          }
          cat(paste(filename, "Reason: low frequency of data", nrow(d_ET)/(max_ts/1000), sep=", "), file="exclusion_log.txt", sep="\n", append=TRUE)
          file.rename(response_file, file.path("excluded_data", "responses", filename))
        }
        else if(nrow(d_keys)>0)
        {
          if(grepl("smooth", x, fixed = TRUE))
          {
            path <- file.path("excluded_data", "ET_raw", "smooth", filename)
          }
          else
          {
            path <- file.path("excluded_data", "ET_raw", filename)
          }
          cat(paste(filename, "Reason: Did not respect instructions (key strokes messing with mouse scrolling)", sep=", "), file="exclusion_log.txt", sep="\n", append=TRUE)
          file.rename(response_file, file.path("excluded_data", "responses", filename))
        }
        else if(nrow(d_click)>0)
        {
          if(grepl("smooth", x, fixed = TRUE))
          {
            path <- file.path("excluded_data", "ET_raw", "smooth", filename)
          }
          else
          {
            path <- file.path("excluded_data", "ET_raw", filename)
          }
          cat(paste(filename, "Reason: Did not respect instructions (used the right scroller)", sep=", "), file="exclusion_log.txt", sep="\n", append=TRUE)
          file.rename(response_file, file.path("excluded_data", "responses", filename))
        }
        
      }
    }
    file.rename(x, path)
  })
  
  responses_files  <- list.files(path=file.path("raw_data", "responses"), pattern="*.csv", full.names = TRUE, recursive = TRUE)
  
  lapply(responses_files, function(x) {
    filename <- basename(x)
    path <- x
    ET_files  <- basename(list.files(path=file.path("raw_data", "ET_raw"), pattern="*.csv", recursive = TRUE))
    if(!(is.element(filename, ET_files)))
    {
      path <- file.path("excluded_data", "responses", filename)
      cat(paste(filename, "Reason: No associated eye-tracker file", sep=", "), file="exclusion_log.txt", sep="\n", append=TRUE)
    }
    file.rename(x, path)
  })
  
  return(invisible())
}
