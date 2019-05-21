library(ggplot2)
library(dplyr)
library(grid)
library(lubridate)
library(pictoralist)

run <- function(recipient, data, spek){
  # Dummy input data
  t1 <- "2012-01-26"
  t2 <- "2012-02-26"
  t3 <- "2012-03-26"
  t4 <- "2012-04-26"

  bar1 <- 11/20
  bar2 <- 17/30
  bar3 <- 23/25
  bar4 <- 19/33

  # Converts year/month/date to abbreviated month names
  date1 <- ymd(t1)
  date2 <- ymd(t2)
  date3 <- ymd(t3)
  date4 <- ymd(t4)
  dates <- c(date1, date2, date3, date4)

  # y axis labels
  breaks_y <- c(0.20, 0.4, 0.6, 0.8, 1.0)
  labels_y <- c("20%", "40%", "60%", "80%", "100%")

  # Removes grid and provides correct axis style
  # (missing y-axis ticks on actual axis)
  single_bar_theme <- function(){
    theme_classic() +
      theme(axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.text = element_text(color=PT$DL_BLUE),
            axis.title.y=element_blank(),
            panel.background=element_blank(),
            panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            plot.background=element_blank())
  }

  # Creates annotated performance on each bar
  histogram_ratios <- function(mapping=NULL){
    geom_label(mapping=mapping, data=histogram_nums, stroke=3, nudge_y = -0.05)
  }

  df <- data.frame(lengths = c(bar1, bar2, bar3, bar4),
                   labels = c("11/20", "17/30", "23/25", "19/33"),
                   dates = floor_date(x=dates, unit="month"))


  bar_graph <- ggplot(data=df, aes(x=dates, y=lengths, label=labels)) +
    single_bar_theme() +
    geom_bar(fill=PT$DL_BLUE, stat="identity") +
    geom_text(nudge_y = rep(-0.05,4), color=PT$DL_FILL) +
    scale_x_date(date_labels = "%b") +
    scale_y_continuous(limits=c(0,1.1), expand=c(0,0),
                       breaks=breaks_y, labels = labels_y)

  # Creates achievable benchmark line with correct dashed legend
  benchmarks <- c(.70, .60, .80, .75)

  bar_graph +
    geom_line(mapping=aes(y=benchmarks, linetype="GOAL"),
              size=1,
              lineend="round",
              color=PT$DL_GRAY) +
    theme(legend.position = c(0.9,0.95), legend.title = element_blank(),
          legend.key.size =  unit(0.5, "in"),
          legend.text = element_text(color=PT$DL_BLUE)) +
    scale_linetype_manual(values = c("GOAL" = 2))
}
