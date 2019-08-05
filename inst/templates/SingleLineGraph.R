library(ggplot2)
library(dplyr)
library(grid)
library(lubridate)
library(pictoralist)

run <- function(recipient, data, spek){
  # Dummy value
  achievable_benchmark_line <- 0.5
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
  # x-axis ordered by month
  performer <- data %>%
    filter(sta6a == recipient) %>%
    select(sta6a, report_month, documented, total)

  numerator <- performer$documented
  denominator <- performer$total
  performance <- numerator/denominator
  performance_labels <- paste(numerator, denominator, sep="/")
  dates <- ymd(performer$report_month)

  df <- data.frame(lengths = performance,
                   labels = performance_labels,
                   dates = floor_date(x=dates, unit="month"))

  # Gets date interval and adds to max for GOAL geom_text()
  min_date <- min(df$dates)
  max_date <- max(df$dates)

  days_interval <- max_date - min_date
  goal_offset <- floor(days_interval/10)

  # y axis labels
  breaks_y <- c(0.20, 0.4, 0.6, 0.8, 1.0)
  labels_y <- c("20%", "40%", "60%", "80%", "100%")

  col_graph <- ggplot(data=df, aes(x=dates, y=lengths, label=labels)) +
    single_line_theme() +
    geom_hline(yintercept = achievable_benchmark_line,
               linetype = "dashed",
               color = PT$DL_GRAY) +
    geom_line(mapping=aes(y=lengths),
              size=1,
              lineend="round",
              color=PT$DL_BLUE) +
    geom_point(mapping = aes(y = lengths + 0.07),
               size=4, color= PT$DL_BLUE, shape=18) +
    geom_label(mapping = aes(label=performance_labels),
               nudge_y = 0.1, fill=PT$DL_BLUE,
               color=PT$DL_FILL, label.r = unit(0, "lines"),
               family=PT$DL_FONT, label.size=0) +
    geom_point(mapping = aes(y = lengths),
               size=2, color=PT$DL_BLUE, fill=PT$DL_FILL,
               shape=21, stroke=1.2) +
    scale_y_continuous(limits=c(0,1.15), expand=c(0,0),
                       breaks=breaks_y, labels = labels_y) +
    scale_x_date(date_labels = "%b", expand = expand_scale(add = 12))

  col_graph + geom_text(mapping = aes(label="GOAL", y=achievable_benchmark_line - 0.04,
                                      x=max_date + goal_offset),
                        size=3, color=PT$DL_BLUE, family=PT$DL_FONT)
}
