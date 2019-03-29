# Show Gap Template
library(ggplot2)

# Constants
DL_GRAY <- "#878A8F"
DL_BLUE <- "#00274C"
DL_FILL <- "#FFFFFF"

# Entry point that Pictoralist will call
run <- function(recip, data, colspec){
  plot_data <- make_plot_data(recip, data, colspec)
  make_plot(recip, plot_data)
}

#--Supporting functions--------------------------------------------------------#
make_plot_data <- function(recip, data, colspec){
  data.frame(x=c(1,2,3), y=c(1,2,3))
}

make_plot <- function(recip, plot_data){
  ggplot(plot_data, aes(x=x,y=y)) + geom_line()
}
