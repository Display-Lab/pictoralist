#' @title Produce Plots
#' @description Produce the ggplots for the promoted candidates.
#' @param promoted List of promoted candidates.
#' @param data Performance data.
#' @param spek List representation of client spek.
#' @param templates List of template environments. Each should have name attribute and run() function
#' @return List of ggplots
#' @export
produce_plots <- function(promoted, templates, data, spek){
  # Extract recipient id and template id from promoted candidates
  p_ids <- lapply(promoted, FUN=`extract_performer_id`)
  t_envs <- lapply(promoted, FUN=`extract_template_id`, templates=templates)
  result <- mapply(FUN=`run_template`, p_ids, t_envs,
                   MoreArgs = list(data=data, spek=spek), SIMPLIFY = F)
  result[sapply(result, is.null)] <- NULL # Removes invalid templates (NULL)
  return(result)
}

#' @title Run Template
#' @param p_id performer id.  Passed to template as recipient.
#' @description Generate ggplot from data for the recipient
run_template <- function(p_id, t_env, data, spek){
  invalid_templates <- tryCatch(
    {
      result <- t_env$run(p_id, data, spek)
      return(result)
    },
    error=function(x) {
      temp_name <- get0("template_name", t_env, ifnotfound="name not found.")
      message(paste("Invalid template environment.",
                    "Template name:", temp_name, ". Performer id:", p_id, sep=' '))
    }
  )
}

#' @title Extract Performer Id
#' @description Given a candidate, get the value of the Ancestor Performer id.
extract_performer_id <- function(x){
  anc_performer <- unlist(getElement(x, PT$ANC_PERFORMER_URI))
  p_id <- getElement(anc_performer, "@value")
  p_id
}

#' @title Extract Template Id
#' @description Given a candidate, get the value of the Ancestor Template id.
extract_template_id <- function(x, templates){
  anc_template <- unlist(getElement(x, PT$ANC_TEMPLATE_URI))
  value <- getElement(anc_template, "@value")
  t_id <- sub(PT$APP_BASE_URI,"", value)
  getElement(templates, t_id)
}
