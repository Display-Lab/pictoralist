#' @title Setup Temp Directory
#' @description Adds temporary directory to save ggplot objects
#'
setup_tmp_dir <- function() {
  tmp_dir <- tempdir()
  return(dirname(tmp_dir))
}
