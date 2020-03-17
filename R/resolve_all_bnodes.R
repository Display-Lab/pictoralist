#' @title Resolve All Bnodes
#' @description Get blank node definition recursively
#' @param node List to be resolved.
#' @param graph List of spek components.
#' @return node with contents of all blank nodes recursively resolved
resolve_all_bnodes <- function(node, graph=list()){
  #if node is not a list, return
  if(!is.list(node)){return(node)}
  # Resolve this node if it is a blank node reference
  if(is_bnode_ref(node)){ node <- resolve_bnode(node,graph) }
  # Call resolve all children replacing values with resolved values
  lapply(node, FUN="resolve_all_bnodes", graph=graph)
}

#' @title Resolve Bnode
#' @description Get blank node definition
#' @describeIn Resolve All Bnodes
#' @param node List to be resolved.
#' @param graph List of spek components.
#' @return node with contents of blank node
resolve_bnode <- function(node, graph){
  id <- getElement(node, '@id')
  idx <- which(sapply(graph, 'getElement', '@id') == id)
  if(length(idx) == 0){  return(node)  }
  graph[[idx]]
}

#' @title Is BNode Ref
#' @description Answers the question, "is the given list just a blank node reference?"
#' @describeIn Resolve All Bnodes
#' @param node list. R representation of a rdf node
#' @return boolean
is_bnode_ref <- function(node){
  id <- getElement(node,"@id")
  if(length(node)==1 && !is.null(id)){
    return(TRUE)
  }else{
    return(FALSE)
  }
}
