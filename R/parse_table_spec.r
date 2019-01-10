#' @title Parse Table Spec
#' @description Get column specification for input data from spek
#' @param spek List of spek components.
#' @return List of table specifications
#' @importFrom dplyr first
#' @export
parse_table_spec <- function(spek){
  table_idx <- which(sapply(spek, 'getElement', '@type') == "http://www.w3.org/ns/csvw#Table")

  # Return empty list if no table is present
  if(length(table_idx) == 0){ return(list()) }

  table <- spek[[dplyr::first(table_idx)]]
  table_schema <- dplyr::first(getElement(table, PT$TABLE_SCHEMA_URI), default=list())

  result <- resolve_all_bnodes(table_schema, spek)
  return(result)
}

