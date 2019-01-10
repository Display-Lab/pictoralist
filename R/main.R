#' @title Main
#' @description The application entry function with all the side effects.
#' @param spek_path Path to spek json. Use NULL to indicate read from stdin
#' @param data_path Path to performance data.
#' @export
main <- function(spek_path = NULL, data_path = NULL) {
  # Read spek
  spek <- read_spek(spek_path)

  # Parse spek
  candidates <- parse_spek_candidates(spek)
  promoted   <- parse_promoted_candidates(candidates)
  ascribees <- parse_ascribees(promoted)

  table_spec <- parse_table_spec(spek)

  # Read data
  data <- read_data(data_path)

  # Load templates
  templates <- load_templates()

  # Don't print the return value
  invisible(NULL)
}

