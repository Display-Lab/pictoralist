# Show Trend Template
library(ggplot2)
library(dplyr)

# Constants
DL_GRAY <- "#878A8F"
DL_BLUE <- "#00274C"
DL_FILL <- "#FFFFFF"

# Entry point that Pictoralist will call
run <- function(recipient, data, spek){
  plot_data <- make_plot_data(recipient, data, spek)
  make_plot(recipient, plot_data, spek)
}

#--Supporting functions--------------------------------------------------------#

make_plot_data <- function(recip, data, spek){
  # Trim data to recipient
  # TODO: issue13:
  #  id_colname <- pictoralist::extract
  sy <- sym(value_colname)

  # Get measure goal if available
  goal <- 10

  ymin <- 0
  y_upper_lim <- max(plot_data[[value_colname]]) * 1.2
  xmin <- 0
  x_upper_lim <- max(plot_data[[time_colname]]) * 1.10
  label_nudge = y_upper_lim * .055

  ggplot(plot_data, aes(x=!!sx, y=!!sy)) +
    display_lab_base_theme() +
    display_lab_goal_line(value = goal, xlim=x_upper_lim) +
    display_lab_line() +
    display_lab_vert_callout(aes(label=!!sy), nudge=label_nudge) +
    scale_y_continuous(limits=c(ymin,y_upper_lim)) +
    scale_x_continuous(limits=c(xmin,x_upper_lim))
}

display_lab_goal_line <- function(value, xlim){
  list(
    geom_hline(yintercept=value, linetype=2, color=DL_GRAY),
    annotate(geom="text", x=0.98*xlim, y = value*1.03, label="GOAL")
  )

}

display_lab_base_theme <- function(){
  theme_classic(base_family = 'montserrat') +
    theme( axis.ticks.y = element_blank(),
           axis.text.y = element_text(hjust=0))
}

display_lab_line <- function(mapping=NULL){
  list(
    geom_line(mapping=mapping, size=2, lineend="round", color=DL_BLUE),
    geom_point(mapping=mapping, size=2.5, stroke=3, shape="circle filled",
             color=DL_BLUE, fill=DL_FILL)
  )
}

display_lab_vert_callout <- function(mapping=NULL, nudge){
  geom_label(mapping=mapping, nudge_y=nudge, fill = DL_BLUE, color="#FFFFFF")
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
