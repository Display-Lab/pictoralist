library(ggplot2)
library(dplyr)
library(grid)
library(lubridate)
library(pictoralist)

recipient_id= "a"

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
                            x=max_date + goal_offset),
              size=3, color=PT$DL_BLUE) +
    scale_fill_manual(values = palette,
                      guide=FALSE)
}

# Synthetic input data
ids <- c(rep("a",4), rep("b", 4), rep("c", 4))
numerators <- c(60, 80, 70, 40, 70, 60, 75, 65, rep(20,4))
denominators <- rep(100,length(ids))
## Munge date char vector to date vector
date_strings <- rep(c("2012-01-01", "2012-02-02", "2012-03-03", "2012-04-04"), length(unique(ids)))
date_list <- lapply(date_strings, FUN=ymd)
date_arr <- do.call("c", date_list)

input_data <- data.frame(id=ids,
                         numer=numerators,
                         denom=denominators,
                         date=date_arr)

achievable_benchmark_line = 0.7

# y axis labels
breaks_y <- c(0.20, 0.4, 0.6, 0.8, 1.0)
labels_y <- c("20%", "40%", "60%", "80%", "100%")

# Gets date interval and adds to max for GOAL geom_text()
min_date <- min(input_data$date)
max_date <- max(input_data$date)
days_interval <- max_date - min_date
goal_offset <- floor(days_interval/5)

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
          plot.background=element_blank())
}

run <- function(recipient, data, spek){
  plot_data <- make_plot_data(input_data, recipient_id)

  make_plot(plot_data, recipient_id, achievable_benchmark_line)
}

