#' @title Read Data
#' @description Read data from path
#' @importFrom readr read_csv
#' @export
read_data <- function(data_path) {
  if(is.null(data_path)){
    stop(simpleError("Path to data is NULL."))
  }
  if(!file.exists(data_path)){
    stop("Path to data non-existant: ", data_path)
  }
  readr::read_csv(data_path)
}
