#!/usr/bin/env Rscript

# Render all cards
library(rmarkdown)

from_base_files <- list.files(path="cards", pattern = "\\.Rmd$", full.names = T)
from_cards_files <- list.files(path=".", pattern = "\\.Rmd$")
files <- c(from_base_files, from_cards_files)

for (f in files){
  render(f)
}


