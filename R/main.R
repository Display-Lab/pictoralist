#' @title Main
#' @description The application entry function with all the side effects.
#' @param spek_path Path to spek json. Use NULL to indicate read from stdin
#' @param data_path Path to performance data.
#' @importFrom extrafont loadfonts
#' @importFrom extrafont font_import
#' @importFrom spekex read_spek
#' @export
main <- function(spek_path = NULL, data_path = NULL) {
  # Read spek
  spek <- spekex::read_spek(spek_path)

  # Parse spek
  candidates <- parse_spek_candidates(spek)
  promoted   <- parse_promoted_candidates(candidates)

  # Read data
  data <- read_data(data_path)

  # Load templates
  templates <- load_templates()

  # Load fonts
  extrafont::font_import(paths=system.file("montserrat",package="pictoralist"), prompt=FALSE)
  extrafont::loadfonts(device = "postscript", quiet=T)

  # Produce plots
  figures <- produce_plots(promoted, templates, data, spek)

  # Write plots to disk. TODO: issue12

  # Don't print the return value
  invisible(NULL)
}

