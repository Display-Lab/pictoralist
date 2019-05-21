#' @export
.onLoad <- function(libname, pkgname) {
  suppressWarnings(
    extrafont::font_import(paths=system.file("montserrat",package="pictoralist"), prompt=FALSE)
  )
  suppressWarnings( extrafont::loadfonts(device = "postscript", quiet=T) )
}
