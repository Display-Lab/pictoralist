library(ggplot2)
library(dplyr)
library(grid)
library(lubridate)
library(pictoralist)

run <- function(recipient, data, spek){
  color_palette <- c(PT$DL_RED, PT$DL_LIGHT_BLUE, PT$DL_CYAN, PT$DL_ORANGE)

  # Synthetic input data
  ids <- c(rep("a",4), rep("b",4),rep("c",4),rep("d",4))
  numerators <- c(100, 100, 90, 90, 100, 90, 70, 65, 50,60,88,85,30,30,40,60)
  denominators <- rep(100,16)
  ## Munge date char vector to date vector
  date_strings <- rep(c("2012-01-01", "2012-02-02", "2012-03-03", "2012-04-04"), 4)
  date_list <- lapply(date_strings, FUN=ymd)
  date_arr <- do.call("c", date_list)
  performance_labels <- c("100/100","100/100","90/100", "90/100")

  achievable_benchmark_line = 0.8

  #Group colors to ids (recipient is "a")
  recipient <- "a"
  non_recipients <- unique(ids[ids != "a"])
  modified_palette <- list()
  modified_palette[non_recipients] <- color_palette
  modified_palette[[recipient]] <- PT$DL_BLUE
  modified_palette <- unlist(modified_palette)

  # Removes grid and provides correct axis style
  # (missing y-axis ticks on actual axis)
  single_line_theme <- function(){
    theme_classic() +
      theme(axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.text = element_text(color=PT$DL_BLUE),
            axis.title.y=element_blank(),
            panel.background=element_blank(),
            panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            plot.background=element_blank(),
            legend.position = "none",
            text = element_text(family=PT$DL_FONT))
  }
  # Calculations for label on recipient
  show_label <- ifelse((ids == 'a'), performance_labels, NA)
  show_arrow <- as.factor(ifelse((ids == 'a'), "show", "noshow"))
  # Avoids connectoin between one path to the next (lengths[4] -> lengths[5])
  ## Assemble components into input data
  df <- data.frame(id=ids, numer=numerators, denom=denominators, date=date_arr)

  # Calculate additional columns data
  df$lengths <- df$numer/df$denom
  df$labels <- mapply(paste, df$numer, df$denom, MoreArgs = list(sep="/"))
  df$is_recipient <- df$id == "a"
  df$recipient_label <- rep("not_recipient", nrow(df))
  df[df$is_recipient, "recipient_label"] <- "recipient"

  # Gets date interval and adds to max for GOAL geom_text()
  min_date <- min(df$date)
  max_date <- max(df$date)
  days_interval <- max_date - min_date
  goal_offset <- floor(days_interval/10)

  # y axis labels
  breaks_y <- c(0.20, 0.4, 0.6, 0.8, 1.0)
  labels_y <- c("20%", "40%", "60%", "80%", "100%")

  col_graph <- ggplot(data=df, aes(x=date, y=lengths)) +
    single_line_theme() +
    geom_hline(yintercept = achievable_benchmark_line,
               linetype = "dashed",
               color = PT$DL_GRAY) +
    geom_point(mapping = aes(y = lengths + 0.07, shape=show_arrow),
               size=4, color= PT$DL_BLUE) +
    geom_line(mapping=aes(x=date, y=lengths, color=id),
              size=1,
              lineend="round") +
    geom_point(mapping=aes(y=lengths, color=id),
               size=2, fill=PT$DL_FILL,
               shape=21, stroke=1.2) +
    scale_y_continuous(limits=c(0,1.15), expand=c(0,0),
                       breaks=breaks_y, labels = labels_y) +
    scale_x_date(date_labels = "%b", expand=c(0.1,0)) +
    scale_shape_manual(values = c("show"=18, "noshow"=NA), guide = FALSE) +
    scale_color_manual(values = modified_palette,
                       guide = guide_legend(title=NULL)) +
    geom_label(mapping = aes(label=show_label),
               nudge_y = 0.1, fill=PT$DL_BLUE,
               color=PT$DL_FILL, label.r = unit(0, "lines"), label.size=0,
               family=PT$DL_FONT) +
    theme(legend.position="bottom")

  col_graph + geom_text(mapping = aes(label="GOAL", y=achievable_benchmark_line - 0.04,
                                      x=max_date + goal_offset),
                        size=3, color=PT$DL_BLUE, family=PT$DL_FONT)
}

