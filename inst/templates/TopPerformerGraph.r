library(ggplot2)
library(dplyr)
library(pictoralist)

# Create Pie Chart (Top Performer)
run <- function(recip, data, spek){
  color_set <- c(PT$DL_BLUE, PT$DL_LIGHT_BORDER, PT$DL_LIGHT_BORDER)

  recip_data <- filter(data, data$practice == recip)
  denom_colname <- 'total_scripts'
  numer_colname <- 'high_dose_scripts'
  data_denom <- sum(recip_data[denom_colname])
  data_numer <- sum(recip_data[numer_colname])

  percentage <- paste(floor(100*(data_numer / data_denom)), "%", sep="")
  # Blocked by Display-Lab/bit-stomach#37
  goal <- .85

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

  # Background listed twice for small section left uncompleted
  df <- data.frame(
    id = recip,
    group = c("background", "performance", "background"),
    value = c(data_denom, data_numer, data_denom - data_numer),
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
    dl_annotate("text", x=10, y=0, label=percentage, size=9, color=PT$DL_BLUE, fontface=2) +
    dl_annotate("text", x=7, y=.5, label="COUNSEL RATE", size=2.5, color=PT$DL_BLUE) +
    dl_annotate("text", x=25, y=.5, label=paste(data_numer, data_denom, sep="/"), size=4, color=PT$DL_BLUE, family=PT$DL_FONT) +
    dl_annotate("text", x=70, y=goal, label="GOAL", size=2, color=PT$DL_BLUE) +
    dl_annotate("text", x=100, y=0, label="Congratulations!", size=5, color=PT$DL_BLUE) +
    dl_annotate("text", x=85, y=0, label="YOU ARE A TOP PERFORMER", size=2.5, color=PT$DL_BLUE) +
    geom_segment(aes(x=40, y = goal, xend = 63, yend = goal), linetype="dashed", color=PT$DL_GRAY)

  pie_chart
}

