# Wrap package constants up into their own env

#' @export
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

PT$APP_BASE_URI <- "http://example.com/app#"

# Style-guide colors
PT$DL_GREEN        <- "#108A00"
PT$DL_LIGHT_BLUE   <- "#0174BB"
PT$DL_RED          <- "#853754"
PT$DL_CYAN         <- "#00B5AF"
PT$DL_BLACK        <- "#000000"
PT$DL_GRAY         <- "#878A8F"
PT$DL_BLUE         <- "#00274C"
PT$DL_ORANGE       <- "#BA5827"
PT$DL_FILL         <- "#FFFFFF"
PT$DL_LIGHT_BORDER <- "#e7edee" # used in TopPerformerGraph

# Style-guide font
PT$DL_FONT <- "Montserrat"

# Template dimensions
PT$DL_WIDTH <- 10
PT$DL_HEIGHT <- 10
