# Show Gap Template
library(ggplot2)

run <- function(recip, data, colspec){
  plot_data <- make_plot_data(recip, data, colspec)
  make_plot(recip, plot_data)
}

make_plot_data <- function(recip, data, colspec){
  data.frame(x=c(1,2,3), y=c(1,2,3))
}

make_plot <- function(recip, plot_data){
  ggplot(plot_data, aes(x=x,y=y)) + geom_line()
}
