library(ggplot2)
library(dplyr)
library(grid)
library(pictoralist)
library(rlang)

run <- function(recipient, data, spek){
  plot_data <- make_plot_data(recipient, data, spek)
  plot <- make_plot(recipient, plot_data, spek)
  return(plot)
}

make_plot <- function(recipient, data, spek){
  # Strings that should be extracted from the spek
  performer_colname <- 'performer'
  numer_colname <- 'passed'
  denom_colname <- 'non_excluded'
  comparator_name <- 'GOAL'
  comparator_value <- 0.80
  measure_title <- "Post Operative\nNausea and Vomiting"
  measure_scale_title <- "Percent of Cases Passed"

  # define column symbols for tidy eval
  numer <- rlang::sym(numer_colname)
  denom <- rlang::sym(denom_colname)
  performer <- rlang::sym(performer_colname)

  breaks_y <- c(0.20, 0.4, 0.6, 0.8, 1.0)
  labels_y <- c("20%", "40%", "60%", "80%", "100%")


  col_graph <- ggplot(data=data, aes(x=performer, y=rate)) +
    single_bar_theme() +
    geom_col(position = "dodge", width=0.2, fill=PT$DL_BLUE) +
    geom_text(mapping = aes(label=performance_label), nudge_y = -0.10, color=PT$DL_FILL,
              family=PT$DL_FONT) +
    scale_y_continuous(limits=c(0,1.1), expand=c(0,0), breaks=breaks_y, labels = labels_y) +
    scale_fill_manual(values = c(PT$DL_LIGHT_BLUE, PT$DL_BLUE)) +
    ggtitle(measure_title) +
    ylab(measure_scale_title)

  col_graph +
    geom_hline(yintercept = comparator_value,
               linetype = "dashed",
               color = PT$DL_GRAY) +
    geom_text(aes(y=comparator_value,label=comparator_name),
              x = 1.5,
              nudge_y=0.07, color=PT$DL_BLUE, size=3,
              family=PT$DL_FONT)
}

make_plot_data <- function(recipient, data, spek){
  # Strings that should be extracted from the spek
  performer_colname <- 'performer'
  numer_colname <- 'passed'
  denom_colname <- 'non_excluded'
  comparator_name <- 'GOAL'
  comparator_value <- 0.80

  # define column symbols for tidy eval
  numer <- rlang::sym(numer_colname)
  denom <- rlang::sym(denom_colname)
  performer <- rlang::sym(performer_colname)

  # Trim data to single performer
  # calculate rate
  mdf <- data %>%
    filter( !!performer == recipient) %>%
    mutate(rate = !!numer / !!denom,
           performance_label = paste0(!!numer, "/", !!denom)) %>%
    mutate(label_nudge = ifelse(rate > 0.20, -0.10 , 0.15),
           label_point_shape = ifelse(rate > 0.20, NA, 23))

  # Handle time if present: filter to max time point
  time_colname <- 'month'
  if( !is.na(time_colname) ){
    time <- rlang::sym(time_colname)
    mdf <- mdf %>% filter(!!time == max(!!time))
  }

  return(mdf)
}

# Removes grid and provides correct axis style
# (missing y-axis ticks on actual axis)
single_bar_theme <- function(){
  theme_classic() +
    theme(axis.ticks=element_blank(),
          axis.text = element_text(color=PT$DL_BLUE),
          axis.title.x = element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank(),
          legend.position = "none",
          text = element_text(family=PT$DL_FONT))
}
