#' @title Teardown Temp Directory
#' @description Deletes files and removes directory
#' @param save_path Path to directory that's being deleted

unlink(TMP_DIR, recursive=TRUE)
