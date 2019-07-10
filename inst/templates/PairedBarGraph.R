library(ggplot2)
library(dplyr)
library(grid)
library(lubridate)
library(pictoralist)

make_plot_data <- function(input_data, recipient_id) {
  average_df <- data.frame(id=rep("average", 4),
                           numer=rep(NA, 4),
                           denom=rep(NA,4),
                           date=unique(input_data$date))
  input_data %>% rbind(average_df) %>%
    mutate(recipient = id==recipient_id,
           rate = (numer/denom),
           date = floor_date(x=date, unit="month")) %>%
    group_by(date) %>%
    mutate(rate = case_when(
      id == "average" ~ mean(rate[!recipient], na.rm=TRUE),
      TRUE ~ rate
    )) %>%
    select(id, date, rate) %>%
    filter(id == recipient_id | id == "average")
}

make_plot <- function(plot_data, recipient_id, goal) {
  # y axis labels
  breaks_y <- c(0.20, 0.4, 0.6, 0.8, 1.0)
  labels_y <- c("20%", "40%", "60%", "80%", "100%")

  # Hard-coded for now
  achievable_benchmark_line = 0.7

  # Gets date interval and adds to max for GOAL geom_text()
  min_date <- min(plot_data$date)
  max_date <- max(plot_data$date)
  days_interval <- max_date - min_date
  goal_offset <- floor(days_interval/5)

  pal_names <- c(recipient_id, "average")
  palette <- c(PT$DL_BLUE, PT$DL_LIGHT_BLUE)
  names(palette) <- pal_names

  plot_data %>% ggplot(aes(x=date, y=rate, fill=id)) +
    single_bar_theme() +
    geom_col(position="dodge") +
    scale_x_date(date_labels = "%b") +
    scale_y_continuous(limits=c(0,1.15), expand=c(0,0),
                       breaks=breaks_y, labels = labels_y) +
    geom_hline(yintercept = achievable_benchmark_line,
               linetype = "dashed",
               color = PT$DL_GRAY) +
    geom_text(mapping = aes(label="GOAL", y=achievable_benchmark_line - 0.04,
                            x=max_date + goal_offset, family=PT$DL_FONT),
              size=3, color=PT$DL_BLUE) +
    scale_fill_manual(values = palette,
                      guide=FALSE)
}

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

run <- function(recipient, data, spek){
  performers <- data %>%
    group_by(sta6a) %>%
    summarise(documented = sum(documented), total = sum(total)) %>%
    mutate(percentage = floor(100*documented / total)) %>%
    arrange(desc(percentage)) %>%
    head(2)

  ids <- performers$sta6a

  p1 <- data %>%
    filter(sta6a == recipient) %>%
    head(4)
  n1 <- p1$documented
  d1 <- p1$total

  p2 <- data %>%
    filter(sta6a == ids[1]) %>%
    head(4)
  n2 <- p2$documented
  d2 <- p2$total

  p3 <- data %>%
    filter(sta6a == ids[2]) %>%
    head(4)
  n3 <- p3$documented
  d3 <- p3$total

  # Input data
  ids <- c(rep(recipient,4), rep(ids[1], 4), rep(ids[2], 4))
  numerators <- c(n1, n2, n3)
  denominators <- c(d1, d2, d3)

  ## Munge date char vector to date vector
  date_strings <- rep(c(p1$report_month), 3)
  date_list <- lapply(date_strings, FUN=ymd)
  date_arr <- do.call("c", date_list)

  input_data <- data.frame(id=ids,
                           numer=numerators,
                           denom=denominators,
                           date=date_arr)

  plot_data <- make_plot_data(input_data, recipient)

  make_plot(plot_data, recipient, achievable_benchmark_line)
}

