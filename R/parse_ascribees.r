#' @title Parse Promoted Candidates
#' @description return subset of candidates that are promoted candidates
#' @param candidates list of candidates
#' @export
parse_ascribees <- function(candidates){
  ascribees_list <- sapply(candidates, 'getElement', PT$ASCRIBEE_URI)
  ascribees <- unique(unlist(ascribees_list))
  as.character(ascribees)
}
