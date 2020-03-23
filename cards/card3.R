library(ggplot2)
library(dplyr)
library(tidyr)
library(pictoralist)

# Create Pie Chart (Top Performer)
run <- function(recipient, data, spek){
  # Plotting attribues for ring position, width, and color
  #  associated with the observation (numerator, denominator, gap)
  plotting_attrs <- tibble(obs=c("numer","gap","denom"),
                            ring=c(50,50,58),
                            width=c(16,16,6),
                            fill_color=c(PT$DL_BLUE, PT$DL_LIGHT_BORDER, PT$DL_LIGHT_BORDER))

  # if the data is time series, drop all but the max time observed.
  tmax_data <- data %>%
    filter(time == max(time))

  # Trim to recipient and merge with plotting attributes
  plot_data <- tmax_data %>%
    filter(id == recipient) %>%
    mutate(gap = denom - numer) %>%
    select(-time) %>%
    pivot_longer(cols=c(numer,denom,gap), names_to = "obs") %>%
    left_join(plotting_attrs)

  # Create single performance label
  numerator <- plot_data %>% filter(obs=="numer") %>% pull(value)
  denominator <- plot_data %>% filter(obs=="denom") %>% pull(value)
  perf_label=paste(floor(100*numerator/denominator), "%",sep="")

  # calculate the benchmark rate
  benchmark_rate <- tmax_data %>% summarize(rate=mean(numer)/mean(denom)) %>% pull(rate)
  # scale benchmark to observed denominator and invert to accomodate polor coordiate direction=-1
  benchmark <- denominator * (1-benchmark_rate)

  #Removes everything except circle and annotations
  top_performer_theme <- function(){
    theme_classic() +
      theme(axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            legend.position="none",
            panel.background=element_blank(),
            panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            plot.background=element_blank(),
            text = element_text(family=PT$DL_FONT))
  }

  # Scale is 0,100
  figure <-ggplot(plot_data, aes(x=ring, y=value, fill=fill_color, width=width)) +
    geom_col() +
    scale_fill_identity() +
    scale_x_continuous(limits=c(0,100)) +
    geom_segment(aes(x=40, y = benchmark, xend = 63, yend = benchmark),
                 linetype="dashed", color=PT$DL_GRAY) +
    coord_polar(theta="y", direction=-1) +
    dl_annotate("text", x=10, y=denominator/2, label=perf_label, size=9, color=PT$DL_BLUE, fontface=2) +
    dl_annotate("text", x=8, y=0, label="PONV 03", size=2.5, color=PT$DL_BLUE) +
    dl_annotate("text", x=20, y=0, label=paste(numerator, denominator, sep="/"),
                size=4, color=PT$DL_BLUE, family=PT$DL_FONT) +
    dl_annotate("text", x=70, y=benchmark, label="PEER AVE", size=2, color=PT$DL_BLUE) +
    top_performer_theme()
  figure
}

