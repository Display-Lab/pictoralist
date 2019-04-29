#' @title Spek Extractor Convenience Functions
#' @description

get_id_col_from_spek <- function(spek) {
  column_list <- get_column_list(spek)
  column_names <- sapply(column_list, FUN=get_name_of_column)
  column_uses <- sapply(column_list, FUN=get_use_of_column)

  return(column_names[which(column_uses == "identifier")])
}

get_value_or_numerator_col_from_spek <- function(spek) {
  column_list <- get_column_list(spek)
  column_names <- sapply(column_list, FUN=get_name_of_column)
  column_uses <- sapply(column_list, FUN=get_use_of_column)

  return(column_names[which(column_uses == "value" | column_uses == "numerator")])
}

get_column_names_by_use <- function(spek, use){
  column_list <- get_column_list(spek)
  column_names <- sapply(column_list, FUN=get_name_of_column)
  column_uses <- sapply(column_list, FUN=get_use_of_column)

  return(column_names[which(column_uses == use)])
}

get_column_list <- function(spek) {
  spek[[PT$INPUT_TABLE_URI]][[1]][[PT$TABLE_SCHEMA_URI]][[1]][[PT$COLUMN_URI]]
}

get_name_of_column <- function(column_specification) {
  column_specification[[PT$COLUMN_NAME_URI]][[1]][['@value']]
}

get_use_of_column <- function(column_specification) {
  column_specification[[PT$COLUMN_USE_URI]][[1]][['@value']]
}
