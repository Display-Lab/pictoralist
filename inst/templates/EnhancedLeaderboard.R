library(ggplot2)
library(dplyr)
library(pictoralist)

# Synthetic input data
ids <- c("XDNU OBGYN CLINIC", "VH OB GYN CLINIC",
         "DF FAMILY MEDICINE", "BHG OB GYN CLINIC",
         "EAA OB GYN CLINIC", "FOO BAR MEDICINE",
         "BAZ OB GYN CLINIC")
numerators <- c(60,49,37,30,26,19,15)
denominators <- c(60,50,40,38,36,32,28)
declined <- denominators - numerators

counsel_rate <- floor(100*(numerators/denominators))
declined_small <- ifelse(counsel_rate >= 85 & counsel_rate != 100, declined, NA)
show_line <- ifelse(counsel_rate >= 85 & counsel_rate != 100, "show", "noshow")
declined_large <- ifelse(counsel_rate < 85, declined, NA)


# Plots data
make_plot <- function(plot_data) {
  plot_data %>% ggplot(aes(x=id, label=id)) +
    leaderboard_theme() +
    geom_col(mapping=aes(y=denom), fill=PT$DL_LIGHT_BLUE) +
    geom_text(mapping=aes(y=denom - largest*0.07, label=declined_label),
              color=PT$DL_FILL, family=PT$DL_FONT) +
    geom_col(mapping=aes(y=numer), fill=PT$DL_BLUE) +
    geom_text(mapping=aes(y=largest*0.07, label=numer),
              color=PT$DL_FILL, family=PT$DL_FONT) +
    geom_segment(mapping=aes(y=numer + 0.5*declined_label,
                             yend=denom*0.85, xend=id, color=show_line)) +
    geom_text(mapping=aes(y=denom*0.80, label=declined_small),
              color=PT$DL_FILL, family=PT$DL_FONT) +
    scale_y_continuous(expand=c(0.1,0)) +
    geom_text(mapping=aes(y=largest*-0.12, label=rate_label, fontface="bold"),
              color=PT$DL_BLUE, family=PT$DL_FONT) +
    scale_color_manual(values = c("show"=PT$DL_FILL, "noshow"=NA)) +
    coord_flip()
}

# Removes grid and provides correct axis style
# (missing y-axis ticks on actual axis)
leaderboard_theme <- function(){
  theme_classic() +
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_text(color=PT$DL_BLUE),
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
# Avoids connection between one path to the next (lengths[4] -> lengths[5])
## Assemble components into input data
run <- function(recipient, data, spek){
  df <- data.frame(id=ids, numer=numerators, denom=denominators)

  # Calculate additional columns data
  df$rate_label <- mapply(paste, counsel_rate, "%")
  df$accepted_label <- numerators
  df$declined_label <- denominators - numerators
  df$declined_small <- declined_small
  df$largest <- max(denominators)
  df$show_line <- show_line
  df$id = reorder(df$id, counsel_rate)

  # reorders data into descending order
  df <- df %>% arrange(100 - counsel_rate)

  make_plot(df)
}

