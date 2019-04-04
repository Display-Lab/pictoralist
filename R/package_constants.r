# Wrap package constants up into their own env

PT <- new.env()

PT$CANDIDATE_URI     <- "http://example.com/cpo#cpo_0000053"
PT$ANC_TEMPLATE_URI  <- "http://example.com/slowmo#AncestorTemplate"
PT$ANC_PERFORMER_URI <- "http://example.com/slowmo#AncestorPerformer"
PT$ASCRIBEE_URI      <- "http://example.com/slowmo#AncestorPerformer"
PT$PROMOTED_URI      <- "http://example.com/slowmo#promoted_by"
PT$INPUT_TABLE_URI   <- "http://example.com/slowmo#InputTable"
PT$COLUMN_USE_URI    <- "http://example.com/slowmo#ColumnUse"
PT$TABLE_SCHEMA_URI  <- "http://www.w3.org/ns/csvw#tableSchema"
PT$COLUMN_URI        <- "http://www.w3.org/ns/csvw#columns"
PT$COLUMN_NAME_URI   <- "http://www.w3.org/ns/csvw#name"
PT$COLUMN_TITLES_URI <- "http://www.w3.org/ns/csvw#titles"
PT$COLUMN_TYPE_URI   <- "http://www.w3.org/ns/csvw#datatype"
PT$CSV_TABLE_URI     <- "http://www.w3.org/ns/csvw#Table"

PT$DEFAULT_TEMPLATE_LOCATION <- system.file("templates", package="pictoralist")

PT$APP_BASE_URI <- "http://example.com/app#"
