# Show Trend Template
library(ggplot2)
library(dplyr)
library(pictoralist)

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
  identity_colname <- get_column_names_by_use(spek, "identifier")
  sym_ident <- sym(identity_colname)

  data %>%
    filter(!!sym_ident == recip)
}

make_plot <- function(recip, plot_data, spek){
  value_colname <- get_column_names_by_use(spek, "numerator")
  time_colname <- get_column_names_by_use(spek, "time")
  sy <- sym(value_colname)
  sx <- sym(time_colname)

  # Get measure goal if available

  ymin <- 0
  y_upper_lim <- max(plot_data[[value_colname]]) * 1.2
  label_nudge = y_upper_lim * .055

  goal <- max(plot_data[[value_colname]]) * 0.9

  ggplot(plot_data, aes(x=!!sx, y=!!sy)) +
    display_lab_base_theme() +
    display_lab_goal_line(value = goal) +
    display_lab_line() +
    display_lab_vert_callout(aes(label=!!sy), nudge=label_nudge) +
    scale_y_continuous(limits=c(ymin,y_upper_lim))
}

display_lab_goal_line <- function(value){
  geom_hline(yintercept=value, linetype=2, color=DL_GRAY)
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
  columns <- getElement(spec, PT$COLUMN_URI)
  uses <- unlist(sapply(columns, `getElement`, PT$COLUMN_USE_URI))
  matching_cols <- columns[which(uses == use)]

  unlist(sapply(matching_cols, `getElement`, PT$COLUMN_NAME_URI))
}
