library(ggplot2)
library(dplyr)
library(pictoralist)

# Synthetic input data
#ids <- c("XDNU OBGYN CLINIC", "VH OB GYN CLINIC",
#         "DF FAMILY MEDICINE", "BHG OB GYN CLINIC",
#         "EAA OB GYN CLINIC", "FOO BAR MEDICINE",
#         "BAZ OB GYN CLINIC")
#numerators <- c(40,57,38,51,48,31,10)
#denominators <- c(40,63,49,68,65,63,65)
delta <- c(1,-1,2,-1,0,-1,0)
rank <- c(1,2,3,4,5,6,7)
#counsel_rate <- floor(100*(numerators/denominators))

calculate_delta_label <- function(delta) {
  ifelse(delta == 0, NA, sprintf("%+i", delta))
}

#"UP RANK"=24, "DOWN RANK"=25, "NO CHANGE"=23
calculate_delta_shape <- function(delta) {
  ifelse(delta == 0, 23, ifelse(delta > 0, 24, 25))
}

# Creates list of green, red, gray arrows
calculate_delta_color <- function(delta) {
  ifelse(delta==0, PT$DL_GRAY, ifelse(delta > 0, PT$DL_GREEN, PT$DL_RED))
}

# Plots data
make_plot <- function(plot_data) {
  plot_data %>% ggplot(aes(y=rank)) +
    leaderboard_theme() +
    geom_text(x=1, aes(y=max(rank)-rank, label=rank),
              check_overlap=TRUE, color=PT$DL_BLUE, hjust=0, size=3,
              family=PT$DL_FONT) +
    geom_point(x=1.5, aes(y=max(rank)-rank, shape=delta_shape),
               fill=delta_color, stroke=0, size=4) +
    geom_text(x=2.1, aes(y=max(rank)-rank, label=delta_label),
              check_overlap=TRUE, color=PT$DL_BLUE, hjust=1, size=3, fontface="bold",
              family=PT$DL_FONT) +
    geom_text(x=2.5, aes(y=max(rank)-rank, label=id),
              check_overlap=TRUE, color=PT$DL_BLUE, hjust=0, size=3,
              family=PT$DL_FONT) +
    geom_text(x=7.5, aes(y=max(rank)-rank, label=rate_label, fontface="bold"),
              check_overlap=TRUE, color=PT$DL_BLUE, hjust=1, size=3,
              family=PT$DL_FONT) +
    geom_text(x=9, aes(y=max(rank)-rank, label=count_label),
              check_overlap=TRUE, color=PT$DL_BLUE, hjust=1, size=3,
              family=PT$DL_FONT) +
    geom_text(x=1, aes(y=max(rank), label="RANK"),
              color=PT$DL_BLUE, size=3, hjust=0,
              family=PT$DL_FONT) +
    geom_text(x=2.5, aes(y=max(rank), label="CLINIC NAME"),
              color=PT$DL_BLUE, size=3, hjust=0,
              family=PT$DL_FONT) +
    geom_text(x=7.5, aes(y=max(rank), label="COUNSEL RATE"),
              color=PT$DL_BLUE, size=3, hjust=1,
              family=PT$DL_FONT) +
    geom_text(x=9, aes(y=max(rank), label="PATIENTS"),
              color=PT$DL_BLUE, size=3, hjust=1,
              family=PT$DL_FONT) +
    scale_x_continuous(limits=c(1,9)) +
    geom_hline(mapping=aes(yintercept=max(rank) - 0.3), color=PT$DL_BLUE) +
    geom_hline(mapping=aes(yintercept=-1), color=PT$DL_BLUE) +
    geom_text(x=1.5, aes(y=-1.3, label="UP RANK"),
              color=PT$DL_BLUE, size=3, hjust=0,
              family=PT$DL_FONT) +
    geom_point(x=1, aes(y=-1.3),
               shape=24, fill=PT$DL_GREEN, stroke=0, size=4) +
    geom_text(x=4.5, aes(y=-1.3, label="DOWN RANK"),
              color=PT$DL_BLUE, size=3, hjust=0,
              family=PT$DL_FONT) +
    geom_point(x=4, aes(y=-1.3),
               shape=25, fill=PT$DL_RED, stroke=0, size=4) +
    geom_text(x=7.5, aes(y=-1.3, label="NO CHANGE"),
              color=PT$DL_BLUE, size=3, hjust=0,
              family=PT$DL_FONT) +
    geom_point(x=7, aes(y=-1.3),
               shape=23, fill=PT$DL_GRAY, stroke=0, size=4) +
    scale_shape_identity()
}

# Removes grid and provides correct axis style
# (missing y-axis ticks on actual axis)
leaderboard_theme <- function(){
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

# Still contains static delta_labels and static ranks
delta_label <- calculate_delta_label(delta)
delta_shape <- calculate_delta_shape(delta)
delta_color <- calculate_delta_color(delta)

run <- function(recipient, data, spek){
  # Avoids connection between one path to the next (lengths[4] -> lengths[5])
  ## Assemble components into input data
  denom_colname <- 'total_quantity'
  numer_colname <- 'total_scripts'
  top_performers <- data %>%
    group_by(practice) %>%
    summarise(total_scripts = sum(total_scripts), total_quantity = sum(total_quantity)) %>%
    mutate(percentage = floor(100*total_scripts/total_quantity)) %>%
    arrange(desc(total_scripts/total_quantity)) %>%
    select(practice, percentage, total_scripts, total_quantity) %>%
    head(7)

  counsel_rate <- top_performers$percentage
  numerators <- top_performers$total_scripts
  denominators <- top_performers$total_quantity
  ids <- top_performers$practice
  df <- data.frame(id=ids,
                   rank=rank,
                   delta_shape=delta_shape,
                   delta_label=delta_label,
                   delta_color=delta_color)

  # Calculate additional columns data
  df$rate_label <- mapply(paste, counsel_rate, "%")
  df$count_label <- mapply(paste, numerators, denominators, MoreArgs = list(sep="/"))
  df$id = reorder(df$id, counsel_rate)

  # reorders data into descending order
  df <- df %>% arrange(100 - counsel_rate)
  make_plot(df)
}

