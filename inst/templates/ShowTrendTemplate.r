# Show Trend Template
library(ggplot2)
library(dplyr)

run <- function(recip, data, table_spec){
  plot_data <- make_plot_data(recip, data, table_spec)
  make_plot(recip, plot_data, table_spec)
}

make_plot_data <- function(recip, data, table_spec){
  # Trim data to recipient
  id_colname <- first(use_colnames(table_spec, 'identifier'))
  data %>% filter_at(.vars=id_colname, .vars_predicate = all_vars(.== recip))
}

make_plot <- function(recip, plot_data, table_spec){
  # Symbolize column names
  time_colname <- first(use_colnames(table_spec, 'time'))
  value_colname <- first(use_colnames(table_spec, 'value'))
  sx <- sym(time_colname)
  sy <- sym(value_colname)

  ggplot(plot_data, aes(x=!!sx,y=!!sy)) + geom_line() + display_lab_base_theme()
}

display_lab_base_theme <- function(){
  theme_classic()

}

use_colnames <- function(spec, use){
  COLS_URI <- 'http://www.w3.org/ns/csvw#columns'
  USE_URI <- 'http://example.com/slowmo#use'
  NAME_URI <- 'http://www.w3.org/ns/csvw#name'

  columns <- getElement(spec, COLS_URI)
  uses <- unlist(sapply(columns, `getElement`, 'http://example.com/slowmo#use'))
  matching_cols <- columns[which(uses == use)]

  unlist(sapply(matching_cols, `getElement`, NAME_URI))
}
