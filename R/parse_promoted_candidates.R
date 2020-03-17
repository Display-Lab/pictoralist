#' @title Parse Promoted Candidates
#' @description return subset of candidates that are promoted candidates
#' @param candidates list of candidates
#' @export
parse_promoted_candidates <- function(candidates){
  promoted_by <- sapply(candidates, 'getElement', PT$PROMOTED_URI)
  promoted_idxs <- which(!sapply(promoted_by, 'is.null'))
  candidates[promoted_idxs]
}
