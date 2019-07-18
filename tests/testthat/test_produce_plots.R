library(ggplot2)
library(tibble)

context('Produce plots')

# Dummy templates
foo <- new.env()
foo$run <- function(data, spek){ ggplot2::ggplot() }
bar <- new.env()
bar$run <- function(data, spek){ ggplot2::ggplot() }
dummy_templates <- list('foo'=foo, 'bar'=bar)

va_data <- read_data(spekex::get_data_path("va"))
va_spek <- spekex::read_spek(spekex::get_spek_path("va"))

templates <- load_templates()



test_that("retuns a list of ggplot objects with mtxdata",{
  mtx_data <- read_data(spekex::get_data_path("mtx"))
  mtx_spek <- spekex::read_spek(spekex::get_spek_path("mtx"))
  promoted <- "E87746"
  mtx_templates <- c(templates$TopPerformerGraph, templates$IUDGraph)

  result <- produce_plots(promoted, mtx_templates, mtx_data, mtx_spek)
})
