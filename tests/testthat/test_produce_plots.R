library(ggplot2)
library(tibble)

context('Produce plots')

# Dummy templates
foo <- new.env()
foo$run <- function(data, spek){ ggplot2::ggplot() }
bar <- new.env()
bar$run <- function(data, spek){ ggplot2::ggplot() }
dummy_templates <- list('foo'=foo, 'bar'=bar)

test_that("retuns a list of ggplot objects",{
  skip("pending")
  dummy_data <- tibble('id'=c(a,b,c), 'val'=c(10,20,30))
  dummy_promoted <- list()
  dummy_templates <- list()
  result <- produce_plots(dummy_promoted, dummy_templates, dummy_data, dummy_spek)
})
