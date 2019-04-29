#' @title Load Templates
#' @description Load templates from file(s)
#' @export
load_templates <- function(){
  template_paths <- list.files(PT$DEFAULT_TEMPLATE_LOCATION, "\\.[rR]$", full.names=TRUE)
  envs <- lapply(template_paths, source_template)
  names(envs) <- sapply(envs, getElement, "template_name")
  return(envs)
}

#' @title Load Templates
#' @describeIn load_templates
#' @description Load individual template into it's own environment
#' @return Environment in which the template was sourced.
#' @export
source_template <- function(path){
  anno_env <- new.env(parent = .BaseNamespaceEnv)
  source(path, local = anno_env)
  anno_env$template_name <- sub("\\.[rR]$", "", basename(path))
  return(anno_env)
}
