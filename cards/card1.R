library(ggplot2)
library(dplyr)
library(grid)
library(lubridate)
library(pictoralist)

# Copied from SingleLineGraph

run <- function(recipient, data, spek){
  # Geenrate benchmark performance by mean of all performers
  benchmark_perf <- data %>%
    group_by(time) %>%
    summarize(id='benchmark', perf_color=PT$DL_CYAN, arrow_shape=as.integer(NA),
              perf_label=NA, numer=mean(numer), denom=mean(denom),
              percentage=floor(100*numer/denom))

  plot_data <- data %>%
    mutate(percentage= floor(100*numer/denom)) %>%
    filter(id == recipient) %>%
    mutate(perf_color=PT$DL_BLUE, arrow_shape=as.integer(18),
           perf_label=paste(numer,denom,sep="/")) %>%
    rbind(benchmark_perf) %>%
    mutate(rate=numer/denom)

  achievable_benchmark_line = 0.8

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

  # y axis labels
  breaks_y <- c(0.20, 0.4, 0.6, 0.8, 1.0)
  labels_y <- c("20%", "40%", "60%", "80%", "100%")

  # Plot benchmark last
  figure <- ggplot(data=plot_data, aes(x=time, y=rate)) +
    single_line_theme() +
    geom_point(mapping=aes(color=perf_color), size=2, fill=PT$DL_FILL,
               shape=21, stroke=1.2)  +
    geom_point(mapping = aes(y = rate + 0.07, shape=arrow_shape),
               size=4, color= PT$DL_BLUE) +
    geom_line(mapping=aes(color=perf_color), size=1, lineend="round") +
   scale_y_continuous(limits=c(0,1.15), expand=c(0,0),
                      breaks=breaks_y, labels = labels_y) +
   scale_x_date(date_labels = "%b", expand=c(0.1,0)) +
   scale_shape_identity(guide = FALSE) +
   scale_color_identity(labels=c("You", "Peer Ave."), guide = guide_legend(title=NULL)) +
   geom_label(mapping = aes(label=perf_label),
              nudge_y = 0.1, fill=PT$DL_BLUE,
              color=PT$DL_FILL, label.r = unit(0, "lines"), label.size=0,
              family=PT$DL_FONT) +
   theme(legend.position="bottom")
  figure
}

