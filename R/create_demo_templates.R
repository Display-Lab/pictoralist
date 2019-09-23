#' @title Create Demo Templates
#' @description saves .png files of each template using synthetic data
library(readr)
library(dplyr)
library(ggplot2)

create_demo_templates <- function() {
  # Comparison bar graph (horizontal) template
  create_template("ComparisonBarGraphHOR", "demo4")
  # Comparison bar graph (vertical) template
  create_template("ComparisonBarGraphVERT", "d5")
  # Comparison line graph
  create_template("ComparisonLineGraph","d1")
  # Enhanced leaderboard
  create_template("EnhancedLeaderboard","d1")
  # IUD Graph
  create_template("IUDGraph","demo")
  # Leaderboard
  create_template("Leaderboard","d1")
  # Paired bar graph (vertical)
  create_template("PairedBarGraph","d1")
  # Paired bar graph (horizontal)
  create_template("PairedBarGraphHOR","d1")
  # Single bar template
  create_template("SingleBarTemplate","d1")
  # Single line graph
  create_template("SingleLineGraph","d1")
  # Top Performer template
  create_template("TopPerformerGraph", "demo")
}

# Creates template from given input "name"
create_template <- function(name, recipient) {
  source(paste("inst/templates/", name, ".R", sep=""))
  data <- read_csv(paste("inst/templates/",name,".csv", sep=""))
  thumb <- run(recipient, data, list())
  ggsave(paste(name,".png", sep=""), thumb, device = "png", height = 5, width=5)
}
