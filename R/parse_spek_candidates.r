#' @title Parse Spek Candidates
#' @description Get list of templates out of the spek
#' @param spek List of spek components.
#' @return List of candidates
#' @export
#'
parse_spek_candidates <- function(spek){
  candidate_idxs <- which(sapply(spek, 'getElement', '@type') == PT$CANDIDATE_URI)
  candidates <- spek[candidate_idxs]
}
