library(ggplot2)
library(dplyr)
library(grid)
library(lubridate)


DL_GRAY <- "#878A8F"
DL_BLUE <- "#00274C"
DL_LIGHT_BLUE <- "#0174BB"
DL_FILL <- "#FFFFFF"

# Dummy input data (performers/performance)
# Dummy input data
t1 <- "2012-01-26"
t2 <- "2012-02-26"
t3 <- "2012-03-26"
t4 <- "2012-04-26"

achievable_benchmark_line = 0.5

# Converts year/month/date to abbreviated month names
date1 <- ymd(t1)
date2 <- ymd(t2)
date3 <- ymd(t3)
date4 <- ymd(t4)
# Removes grid and provides correct axis style
# (missing y-axis ticks on actual axis)
single_line_theme <- function(){
  theme_classic() +
    theme(axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.text = element_text(color=DL_BLUE),
          axis.title.y=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank(),
          legend.position = "none")
}
# x-axis ordered by month
performance <- c(0.25, 0.25, 0.4, 0.5)
performance_labels <- c("25/100","25/100","40/100", "50/100")
dates <- c(date1, date2, date3, date4)

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
             color = DL_GRAY) +
  geom_label(mapping = aes(label=performance_labels),
             nudge_y = 0.1, fill=DL_BLUE,
             color=DL_FILL, label.r = unit(0, "lines")) +
  geom_point(mapping = aes(y = lengths + 0.07),
             size=4, color= DL_BLUE, shape=18) +
  geom_line(mapping=aes(y=lengths),
            size=1,
            lineend="round",
            color=DL_BLUE) +
  geom_point(mapping = aes(y = lengths),
             size=2, color=DL_BLUE, fill=DL_FILL,
             shape=21, stroke=1.2) +
  scale_y_continuous(limits=c(0,1.15), expand=c(0,0),
                     breaks=breaks_y, labels = labels_y) +
  scale_x_date(date_labels = "%b", expand=c(0.1,0))

col_graph + geom_text(mapping = aes(label="GOAL", y=achievable_benchmark_line - 0.04,
                                    x=max_date + goal_offset),
                      size=3, color=DL_BLUE)

