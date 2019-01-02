# Wrap package constants up into their own env

PT <- new.env()

PT$CANDIDATE_URI <- "http://example.com/cpo#cpo_0000053"
PT$ANC_TEMPLATE_URI <- "http://example.com/slowmo#AncestorTemplate"
PT$ASCRIBEE_URI <- "http://example.com/slowmo#AncestorPerformer"
PT$PROMOTED_URI <- "http://example.com/slowmo#promoted_by"

PT$DEFAULT_TEMPLATE_LOCATION <- system.file("templates", package="pictoralist")
