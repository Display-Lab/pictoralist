library(ggplot2)
library(dplyr)
library(grid)
library(lubridate)
library(pictoralist)

run <- function(recipient, data, spek){
  color_palette <- c(PT$DL_RED, PT$DL_LIGHT_BLUE, PT$DL_CYAN, PT$DL_ORANGE)

  performers <- data %>%
    group_by(sta6a) %>%
    summarise(documented = sum(documented), total = sum(total)) %>%
    mutate(percentage = floor(100*documented / total)) %>%
    arrange(desc(percentage)) %>%
    head(4)

  ids <- performers$sta6a
  # Edge case recipient isn't a top performer
  if(!(recipient %in% ids)) {
    ids <- head(ids,3)
    ids <- append(ids, recipient)
  }

  # Get id, performer (p), numerator (n), denominator (d)
  repeated_ids <- c(rep(ids[1],4), rep(ids[2],4), rep(ids[3],4), rep(ids[4], 4))
  p1 <- data %>%
    filter(sta6a == ids[1]) %>%
    head(4)
  n1 <- p1$documented
  d1 <- p1$total

  p2 <- data %>%
    filter(sta6a == ids[2]) %>%
    head(4)
  n2 <- p2$documented
  d2 <- p2$total

  p3 <- data %>%
    filter(sta6a == ids[3]) %>%
    head(4)
  n3 <- p3$documented
  d3 <- p3$total

  p4 <- data %>%
    filter(sta6a == ids[4]) %>%
    head(4)
  n4 <- p4$documented
  d4 <- p4$total

  ## Munge date char vector to date vector
  date_strings <- c(rep(p1$report_month, 4))
  date_list <- lapply(date_strings, FUN=ymd)
  date_arr <- do.call("c", date_list)

  ## Munge numerator/ denominator to single vectors
  numerators <- c(n1, n2, n3, n4)
  denominators <- c(d1, d2, d3, d4)

  performance_labels <- paste(numerators, denominators, sep="/")

  achievable_benchmark_line = 0.8
  ids <- repeated_ids

  #Group colors to ids
  non_recipients <- unique(ids[ids != recipient])
  modified_palette <- list()
  modified_palette[non_recipients] <- color_palette
  modified_palette[recipient] <- PT$DL_BLUE
  modified_palette <- unlist(modified_palette)

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
  # Calculations for label on recipient
  show_label <- ifelse((ids == recipient), performance_labels, NA)
  show_arrow <- as.factor(ifelse((ids == recipient), "show", "noshow"))
  # Avoids connectoin between one path to the next (lengths[4] -> lengths[5])
  ## Assemble components into input data
  df <- data.frame(id=ids, numer=numerators, denom=denominators, date=date_arr)

  # Calculate additional columns data
  df$lengths <- df$numer/df$denom
  df$labels <- mapply(paste, df$numer, df$denom, MoreArgs = list(sep="/"))
  df$is_recipient <- df$id == recipient
  df$recipient_label <- rep("not_recipient", nrow(df))
  df[df$is_recipient, "recipient_label"] <- "recipient"

  # Gets date interval and adds to max for GOAL geom_text()
  min_date <- min(df$date)
  max_date <- max(df$date)
  days_interval <- max_date - min_date
  goal_offset <- floor(days_interval/10)

  # y axis labels
  breaks_y <- c(0.20, 0.4, 0.6, 0.8, 1.0)
  labels_y <- c("20%", "40%", "60%", "80%", "100%")

  col_graph <- ggplot(data=df, aes(x=date, y=lengths)) +
    single_line_theme() +
    geom_hline(yintercept = achievable_benchmark_line,
               linetype = "dashed",
               color = PT$DL_GRAY) +
    geom_point(mapping = aes(y = lengths + 0.07, shape=show_arrow),
               size=4, color= PT$DL_BLUE) +
    geom_line(mapping=aes(x=date, y=lengths, color=id),
              size=1,
              lineend="round") +
    geom_point(mapping=aes(y=lengths, color=id),
               size=2, fill=PT$DL_FILL,
               shape=21, stroke=1.2) +
    scale_y_continuous(limits=c(0,1.15), expand=c(0,0),
                       breaks=breaks_y, labels = labels_y) +
    scale_x_date(date_labels = "%b", expand=c(0.1,0)) +
    scale_shape_manual(values = c("show"=18, "noshow"=NA), guide = FALSE) +
    scale_color_manual(values = modified_palette,
                       guide = guide_legend(title=NULL)) +
    geom_label(mapping = aes(label=show_label),
               nudge_y = 0.1, fill=PT$DL_BLUE,
               color=PT$DL_FILL, label.r = unit(0, "lines"), label.size=0,
               family=PT$DL_FONT) +
    theme(legend.position="bottom")

  col_graph + geom_text(mapping = aes(label="GOAL", y=achievable_benchmark_line - 0.04,
                                      x=max_date + goal_offset),
                        size=3, color=PT$DL_BLUE, family=PT$DL_FONT)
}

