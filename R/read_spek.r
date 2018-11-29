#' @title Read Spek
#' @description Read json-ld spek
#' @importFrom jsonlite fromJSON
#' @importFrom jsonld jsonld_expand
#' @importFrom readr read_file
read_spek <- function(spek_path = NULL){
  input <- resolve_spek_input(spek_path)

  expanded <- jsonld::jsonld_expand(input)
  converted <- jsonlite::fromJSON(expanded, simplifyDataFrame = F)
  return(converted)
}

resolve_spek_input <- function(spek_path){
  if(is.null(spek_path)){
    res <- paste0( readLines('stdin') , collapse=" ")
  } else {
    res <- readr::read_file(spek_path)
  }
  return(res)
}
