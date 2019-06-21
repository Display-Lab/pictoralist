library(ggplot2)
library(dplyr)
library(grid)
library(pictoralist)


###--------------------------------NOT BEING USED------------
# Dummy input data (performers/performance)
p1 <- "XDNU WHC OB GYN CLINIC"
p2 <- "VH OB GYN CLINIC"
p3 <- "DF FAMILY MEDICINE"
p4 <- "BHC OB GYN CLINIC"
p5 <- "EAA OB GYN CLINIC"
p6 <- "ABC OB GYN CLINIC"
p7 <- "UM MEDICINE"
p8 <- "EX AMPLE CLINIC"
p9 <- "EX AM MEDICINE"
p10 <- "DNXU OB GYN CLINIC"
p11 <- "FAM MEDICINE"
p12 <- "STANDARD OB GYN CLINIC"
p13 <- "FOO MEDICINE"
p14 <- "BAR BAZ OB GYN CLINIC"

#performers = c(p1, p2, p3, p4, p5, p6, p7,
#               p8, p9, p10, p11, p12, p13, p14)
#performance = c(0.20, 0.20, 0.20, 0.20, 0.40, 0.60, 0.70,
#                0.80, 0.85, 0.90, 0.94, 0.97, 0.99, 1.00)
# Below 20 annotation arrow
#recipient_id = "XDNU WHC OB GYN CLINIC"
# Above 20 annotation inside bar graph
recipient_id = "FOO MEDICINE"

###--------------------------------NOT BEING USED------------^

achievable_benchmark_line = 0.50

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
          legend.position = "none",
          text = element_text(family=PT$DL_FONT))
}
# gets performers orderd in descending order by percentage ()
run <- function(recipient, data, spek){
  denom_colname <- 'total_quantity'
  numer_colname <- 'total_scripts'

  top_performers <- data %>%
    group_by(practice) %>%
    summarise(total_scripts = sum(total_scripts),
              total_quantity = sum(total_quantity)) %>%
    mutate(percentage = round(total_scripts/total_quantity, digits=2)) %>%
    arrange(desc(total_scripts/total_quantity)) %>%
    select(practice, percentage) %>%
    head(14)

  # If recipient not in top 14, remove last elem and add recipient
  if(!(recipient %in% top_performers$practice)) {
    recip_data <- filter(data, data$practice == recipient)
    data_denom <- sum(recip_data[denom_colname])
    data_numer <- sum(recip_data[numer_colname])
    top_performers <- top_performers %>% head(13) %>%
      rbind(c(recipient, round(data_numer/data_denom, digits=2)))
  }
  performers <- as.character(pull(top_performers, practice))
  performance <- as.numeric(pull(top_performers, percentage))
  performance_labels <- paste(performance*100,100,sep="/")

  ordered_performers <- reorder(performers, performance)
  role = (ordered_performers == recipient)
  above_twenty <- ifelse((performance > .20 & role), performance_labels, NA)
  below_twenty <- ifelse((performance <= .20 & role), performance_labels, NA)
  below_twenty_arrow <- as.factor(ifelse((performance <= .20 & role), "show", "noshow"))

  df <- data.frame(lengths = performance,
                   performers = ordered_performers,
                   role = role,
                   above_twenty = above_twenty,
                   below_twenty = below_twenty,
                   below_twenty_arrow = below_twenty_arrow)


  breaks_y <- c(0.20, 0.4, 0.6, 0.8, 1.0)
  labels_y <- c("20%", "40%", "60%", "80%", "100%")

  col_graph <- ggplot(data=df, aes(x=performers, y=lengths)) +
    single_bar_theme() +
    geom_col(mapping = aes(fill=role), position = "dodge", width=0.8) +
    geom_text(mapping = aes(label=above_twenty), nudge_y = -0.10, color=PT$DL_FILL,
              family=PT$DL_FONT) +
    geom_label(mapping = aes(label=below_twenty),
               nudge_y = 0.15, fill=PT$DL_BLUE, color=PT$DL_FILL,
               label.r = unit(0, "lines"),
               family=PT$DL_FONT) +
    geom_point(mapping = aes(y = lengths + 0.07, shape=below_twenty_arrow), size=2.5, fill=PT$DL_BLUE, color=PT$DL_BLUE) +
    scale_y_continuous(limits=c(0,1.1), expand=c(0,0), breaks=breaks_y, labels = labels_y) +
    scale_x_discrete(df$performers, expand=expand_scale(add=c(0.65,2))) +
    scale_fill_manual(values = c(PT$DL_LIGHT_BLUE, PT$DL_BLUE)) +
    scale_shape_manual(values = c("show"=23, "noshow"=NA)) +
    coord_flip()

  col_graph +
    geom_hline(yintercept = achievable_benchmark_line,
               linetype = "dashed",
               color = PT$DL_GRAY) +
    geom_text(aes(15,achievable_benchmark_line,label="GOAL"),
              nudge_y=0.07, color=PT$DL_BLUE, size=3,
              family=PT$DL_FONT)
}

