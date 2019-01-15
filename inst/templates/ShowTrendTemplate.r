# Show Trend Template
library(ggplot2)
library(dplyr)

run <- function(recip, data, table_spec){
  plot_data <- make_plot_data(recip, data, table_spec)
  make_plot(recip, plot_data)
}

make_plot_data <- function(recip, data, table_spec){
  # What is the colname of the time?
  time_colname <- first(use_colnames(table_spec, 'time'))
  print(time_colname)
  # What is the colname of the value?
  value_colname <- first(use_colnames(table_spec, 'value'))
  print(value_colname)
  # What is the identifier column name
  id_colname <- first(use_colnames(table_spec, 'identifier'))
  print(id_colname)

  browser()
  # Trim data to recipient
  data %>% filter(id==recip)
}

make_plot <- function(recip, plot_data){
  ggplot(plot_data, aes(x=timepoint,y=performance)) + geom_line()
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
