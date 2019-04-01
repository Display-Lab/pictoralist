library(ggplot2)
library(dplyr)
library(pictoralist)

# Create Pie Chart (Top Performer)
run <- function(recip, data, spek){
  DL_LIGHT_BORDER <- "#e7edee" #takes alpha into account
  DL_GRAY <- "#878A8F"
  DL_BLUE <- "#00274C"
  DL_FILL <- "#FFFFFF"
  color_set <- c(DL_BLUE, DL_LIGHT_BORDER, DL_LIGHT_BORDER)
  percentage <- "90%"
  goal <- 17/20

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
            plot.background=element_blank())
  }

  # Background listed twice for small section left uncompleted
  df <- data.frame(
    group = c("background", "performance", "background"),
    value = c(20, 18, 2),
    ring = c(58, 50, 50),
    width = c(6,16,16)
  )

  # Scale is 0,100
  cols <- ggplot(df, aes(x=ring, y=value, fill=group, width=width)) +
    geom_col(position=position_fill(), fill=color_set) +
    scale_x_continuous(limits=c(0,100))

  # The y-coordinate is scaled from 0 to 1
  pie_chart <- cols +
    coord_polar(theta="y", direction=-1) +
    top_performer_theme() +
    annotate("text", x=10, y=0, label=percentage, size=10, color=DL_BLUE) +
    annotate("text", x=5, y=.5, label="COUNSEL RATE", size=3, color=DL_BLUE) +
    annotate("text", x=25, y=.5, label="18/20", size=4, color=DL_BLUE) +
    annotate("text", x=70, y=goal, label="GOAL", size=3, color=DL_BLUE) +
    annotate("text", x=100, y=0, label="Congratulations!", size=6, color=DL_BLUE) +
    annotate("text", x=85, y=0, label="YOU ARE A TOP PERFORMER", size=3, color=DL_BLUE) +
    geom_segment(aes(x=40, y = goal, xend = 63, yend = goal), linetype="dashed", color=DL_GRAY)

  pie_chart
}
