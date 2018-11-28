#' @title Main
#' @description The application entry function with all the side effects.
#' @param spek_path Path to spek json. Use NULL to indicate read from stdin
#' @param data_path Path to performance data.
#' @export
main <- function(spek_path = NULL, data_path = NULL) {
  # Read spek
  spek <- read_spek(spek_path)

  # Don't print the return value
  invisible(NULL)
}