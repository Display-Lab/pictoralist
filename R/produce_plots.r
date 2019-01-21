#' @title Produce Plots
#' @description Produce the ggplots for the promoted candidates.
#' @param promoted List of promoted candidates.
#' @param data Performance data.
#' @param table_spec Table specification.
#' @return List of ggplots
#' @export
produce_plots <- function(promoted, templates, data, table_spec){
  # Strip recipient id and template id from promoted candidates
  p_ids <- lapply(promoted, FUN=`strip_performer_id`)
  t_envs <- lapply(promoted, FUN=`lookup_template`, templates=templates)

  result <- mapply(FUN=`run_template`, p_ids, t_envs, MoreArgs = list(data=data, spec=table_spec), SIMPLIFY = F)
  return(result)
}

#' @title Run Template
#' @description Generate ggplot from data for the recipient
run_template <- function(p_id, t_env, data, spec){
  result <- t_env$run(p_id, data, spec)
  return(result)
}

#' @title Strip Performer Id
#' @description Given a template node and list of template environments, return matching env
strip_performer_id <- function(x){
  anc_performer <- unlist(getElement(x, PT$ANC_PERFORMER_URI))
  p_id <- getElement(anc_performer, "@value")
  sub(PT$APP_BASE_URI, '', p_id)
}

#' @title Lookup Template
#' @description Given a template node and list of template environments, return matching env
lookup_template <- function(x, templates){
  anc_template <- unlist(getElement(x, PT$ANC_TEMPLATE_URI))
  value <- getElement(anc_template, "@value")
  t_id <- sub(PT$APP_BASE_URI,"", value)
  getElement(templates, t_id)
}


