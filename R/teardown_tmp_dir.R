#' @title Teardown Temp Directory
#' @description Deletes files and removes directory
#' @param save_path Path to directory that's being deleted
teardown_tmp_dir <- function(save_path) {
  current_files <- list.files(path=save_path, pattern=".png")
  lapply(current_files, unlink)
  unlink(save_path, recursive=TRUE)
}
