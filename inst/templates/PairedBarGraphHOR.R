library(ggplot2)
library(dplyr)

DL_GRAY <- "#878A8F"
DL_BLUE <- "#00274C"
DL_LIGHT_BLUE <- "#0174BB"
DL_FILL <- "#FFFFFF"
DL_RED <- "#853754"
DL_CYAN <- "#00B5AF"
DL_ORANGE <- "#BA5827"

recipient_id <- "a"

bar_width <- 0.5

make_plot <- function(plot_data, recipient_id, goal) {
  pal_names <- c(recipient_id, "b")
  palette <- c(DL_BLUE, DL_LIGHT_BLUE)
  names(palette) <- pal_names

  plot_data %>% ggplot(aes(x=categories, y=rate, fill=id)) +
    single_bar_theme() +
    geom_col(position="dodge", width=bar_width) +
    scale_y_continuous(limits=c(0,1.15), expand=c(0,0),
                       breaks=breaks_y, labels = labels_y) +
    geom_hline(yintercept = achievable_benchmark_line,
               linetype = "dashed",
               color = DL_GRAY) +
    geom_text(mapping = aes(label="GOAL", y=achievable_benchmark_line + 0.05,
                            x=num_categories + 0.55),
              size=3, color=DL_BLUE) +
    geom_text(mapping = aes(x=categories,
                            y=0.10,
                            label=recipient_labs),
              color=DL_FILL, nudge_x=-0.25*bar_width) +
    geom_text(mapping = aes(x=categories,
                            y=0.10,
                            label=nonrecipient_labs),
              color=DL_FILL, nudge_x=0.25*bar_width) +
    scale_fill_manual(values = palette,
                      guide=FALSE) +
    coord_flip()
}

# Synthetic input data
ids <- c(rep("a",4), rep("b", 4))
numerators <- c(91, 93, 95, 97, 94, 96, 90, 98)
denominators <- rep(100,length(ids))
recipient_performance_labels <- ifelse(ids=="a", paste(numerators,
                                                       '/',
                                                       denominators,
                                                       sep=""), NA)
nonrecipient_performance_labels <- ifelse(ids!="a", paste(numerators,
                                                          '/',
                                                          denominators,
                                                          sep=""), NA)
categories <- rep(c("category1", "foo", "bar", "baz"), length(unique(ids)))
df <- data.frame(id=ids,
                 rate=numerators/denominators,
                 categories=categories)
df$recipient_labs <- recipient_performance_labels
df$nonrecipient_labs <- nonrecipient_performance_labels

achievable_benchmark_line = 0.90

# y axis labels
breaks_y <- c(0.20, 0.4, 0.6, 0.8, 1.0)
labels_y <- c("20%", "40%", "60%", "80%", "100%")

# Gets position for GOAL geom_text()
num_categories <- length(unique(categories))
goal_offset <- achievable_benchmark_line

# Removes grid and provides correct axis style
# (missing y-axis ticks on actual axis)
single_bar_theme <- function(){
  theme_classic() +
    theme(axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.text = element_text(color=DL_BLUE),
          axis.title.y=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank())
}

make_plot(df, recipient_id, achievable_benchmark_line)

