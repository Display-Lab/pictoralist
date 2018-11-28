#' @title Read Spek
#' @description Read json-ld spek
#' @importFrom jsonlite fromJSON
#' @importFrom jsonld jsonld_expand
#' @importFrom readr read_file
read_spek <- function(spek_path = NULL){
  if(is.null(spek_path)){
    path <- stdin()
  } else {
    path <- spek_path
  }

  spek_str <- readr::read_file(path)
  expanded <- jsonld::jsonld_expand(spek_str )
  converted <- jsonlite::fromJSON(expanded, simplifyDataFrame = F)
  return(converted[[1]])
}
