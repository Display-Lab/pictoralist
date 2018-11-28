#' @title Read Spek
#' @description Read json-ld spek
#' @importFrom jsonlite fromJSON
#' @importFrom jsonld jsonld_expand
#' @importFrom readr read_file
read_spek <- function(spek_path = NULL){
  input <- resolve_spek_input(spek_path)
  spek_str <- readr::read_file(input)

  expanded <- jsonld::jsonld_expand(spek_str )
  converted <- jsonlite::fromJSON(expanded, simplifyDataFrame = F)
  return(converted[[1]])
}

resolve_spek_input <- function(spek_path){
  if(is.null(spek_path)){
    res <- stdin()
  } else {
    res <- spek_path
  }
  return(res)
}
