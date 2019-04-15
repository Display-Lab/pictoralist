library(ggplot2)
library(dplyr)
library(grid)

DL_GRAY <- "#878A8F"
DL_BLUE <- "#00274C"
DL_LIGHT_BLUE <- "#0174BB"
DL_FILL <- "#FFFFFF"

# Dummy input data (performers/performance)
p1 <- "1"
p2 <- "2"
p3 <- "3"
p4 <- "4"
p5 <- "5"
p6 <- "6"
p7 <- "7"
p8 <- "8"
p9 <- "9"
p10 <- "10"
p11 <- "11"
p12 <- "12"
p13 <- "13"
p14 <- "14"

performers = c(p1, p2, p3, p4, p5, p6, p7,
               p8, p9, p10, p11, p12, p13, p14)
performance = c(0.15, 0.20, 0.20, 0.20, 0.40, 0.60, 0.70,
                0.80, 0.85, 0.90, 0.94, 0.97, 0.99, 1.00)
performance_labels = c("15/100","20/100","20/100",
                       "20/100","40/100","60/100",
                       "70/100","80/100","85/100",
                       "90/100","94/100","97/100",
                       "99/100","100/100")
achievable_benchmark_line = 0.75

# Below 20 annotation arrow
#recipient_id = "XDNU WHC OB GYN CLINIC"
# Above 20 annotation inside bar graph
recipient_id = "5"

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
          plot.background=element_blank(),
          legend.position = "none")
}
# expects performers to be an ordered facter by performance (descending)
ordered_performers <- reorder(performers, 1-performance)
role = (ordered_performers == recipient_id)
show_label <- ifelse((role), performance_labels, NA)
show_you <- ifelse((role), "YOU", NA)
show_arrow <- as.factor(ifelse((role), "show", "noshow"))

df <- data.frame(lengths = performance,
                 performers = ordered_performers,
                 role = role,
                 show_label = show_label,
                 show_arrow = show_arrow)


breaks_y <- c(0.20, 0.4, 0.6, 0.8, 1.0)
labels_y <- c("20%", "40%", "60%", "80%", "100%")

col_graph <- ggplot(data=df, aes(x=performers, y=lengths)) +
  single_bar_theme() +
  geom_col(mapping = aes(fill=role),
           position = "dodge", width=0.8) +
  geom_hline(yintercept = achievable_benchmark_line,
             linetype = "dashed",
             color = DL_GRAY) +
  geom_label(mapping = aes(label=show_label),
             nudge_y = 0.08, fill=DL_BLUE,
             color=DL_FILL, label.r = unit(0, "lines")) +
  geom_text(mapping = aes(label=show_you),
            nudge_y = 0.14, fill=DL_BLUE,
            size=3) +
  geom_point(mapping = aes(y = lengths + 0.05, shape=show_arrow),
             size=4, color=DL_BLUE) +
  scale_y_continuous(limits=c(0,1.15), expand=c(0,0), breaks=breaks_y, labels = labels_y) +
  scale_x_discrete(df$performers, expand=expand_scale(add=c(0.65,2))) +
  scale_fill_manual(values = c(DL_LIGHT_BLUE, DL_BLUE)) +
  scale_shape_manual(values = c("show"=18, "noshow"=NA))

col_graph +
  geom_text(aes(15,achievable_benchmark_line,label="GOAL"),
            nudge_y=-0.05, color=DL_BLUE, size=3)

