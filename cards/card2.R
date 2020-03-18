library(ggplot2)
library(dplyr)
library(grid)
library(lubridate)
library(pictoralist)

# Copy of SingleBar template

run <- function(recipient, data, spek){
  # Creates achievable benchmark line with correct dashed legend
  benchmarks <- c(.90, .90, .90, .90)

  # Avoid issue of goal line being superimposed over performance_labels with print_pos
  plot_data <- data %>%
    filter(id == recipient) %>%
    mutate(rate=numer/denom,
           perf_label=paste(numer, denom, sep="/"),
           callout_below=rate < benchmarks,
           callout_y=ifelse(callout_below,rate-0.05, rate+0.05),
           callout_color=ifelse(callout_below, pictoralist::PT$DL_FILL, pictoralist::PT$DL_BLUE))


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
            plot.background=element_blank(),
            text = element_text(family=PT$DL_FONT))
  }

  # Creates annotated performance on each bar
  histogram_ratios <- function(mapping=NULL){
    geom_label(mapping=mapping, data=histogram_nums, stroke=3, nudge_y = -0.05)
  }

  # y axis labels
  breaks_y <- c(0.20, 0.4, 0.6, 0.8, 1.0)
  labels_y <- c("20%", "40%", "60%", "80%", "100%")

  plot <- ggplot(data=plot_data, aes(x=time, y=rate, label=perf_label)) +
    single_bar_theme() +
    geom_bar(fill=PT$DL_BLUE, stat="identity") +
    geom_text(aes(y=callout_y, color= callout_color), family= PT$DL_FONT) +
    scale_x_date(date_labels = "%b") +
    scale_y_continuous(limits=c(0,1.1), expand=c(0,0),
                       breaks=breaks_y, labels = labels_y) +
    geom_line(mapping=aes(y=benchmarks, linetype="GOAL"),
              size=1, lineend="round", color=PT$DL_GRAY) +
    scale_linetype_manual(values = c("GOAL" = 2)) +
    scale_color_discrete(guide="none") +
    theme(legend.title = element_blank(),
          legend.key.size =  unit(0.5, "in"),
          legend.text = element_text(color=PT$DL_BLUE, size=10))

  plot
}
