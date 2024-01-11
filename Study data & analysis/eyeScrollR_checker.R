eyeScrollR_checker <- function()
{
  if (!dir.exists(file.path("draw_check", "ET_check"))) {dir.create(file.path("draw_check", "ET_check"), recursive = TRUE)}
  ET_files         <- list.files(file.path("intermediate_data", "ET"), pattern="*.csv", full.names = TRUE)
  ET_files_names   <- basename(ET_files)
  d_eye_tracking   <- lapply(ET_files, read_csv, comment="#", show_col_types = FALSE)
  
  for (i in seq_len(length(d_eye_tracking)))
  {
    d_img <- d_eye_tracking[[i]] %>%
      filter(!is.na(Corrected.Fixation.Y)) %>%
      slice(sample(n(), 50)) %>%
      select(Corrected.Fixation.X, Corrected.Fixation.Y, Fixation.Start, Fixation.End)
    d_img$Timestamp <- round((d_img$Fixation.Start + d_img$Fixation.End) / 2)
    minutes <- (d_img$Timestamp %/% 1000) %/% 60
    seconds <- (d_img$Timestamp %/% 1000) - minutes * 60
    frames <- round((d_img$Timestamp - minutes * 60 * 1000 - seconds * 1000) / 1000 * 30)
    d_img$Timestamp_check <- paste0(minutes, "m ", seconds, "s", frames)
    d_img$img <- ifelse(d_img$Corrected.Fixation.Y <28800, "top", "bottom")
    d_img$Corrected.Fixation.Y.img <- ifelse(d_img$Corrected.Fixation.Y < 28800, d_img$Corrected.Fixation.Y, d_img$Corrected.Fixation.Y - 28800)
    write.csv(d_img, file = file.path("draw_check", "ET_check", ET_files_names[i]), row.names = FALSE)
  }
  file.copy(file.path("raw_data", "responses", ET_files_names), file.path("draw_check", "order_files"))
}
