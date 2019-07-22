library(ggplot2)
library(tibble)

context('Save Plots')

# Dummy templates
foo <- ggplot2::ggplot()
bar <- ggplot2::ggplot()
dummy_templates <- list('foo'=foo, 'bar'=bar)
performer_id <- "baz"

# Displaylab templates
templates <- load_templates()

test_that("templates are saved in save_path directory with .png format", {
  save_path <- getwd()
  save_plots(dummy_templates, save_path, performer_id)

  current_files <- list.files(path=save_path, pattern=".png")
  expected_files <- c("baz-bar.png", "baz-foo.png")
  do_match <- current_files == expected_files
  expect_true(all(do_match))
  file.remove(current_files)
})

test_that("templates are saved in current directory by default", {
  save_path <- NA
  save_plots(dummy_templates, save_path, performer_id)

  current_files <- list.files(path=getwd(), pattern=".png")
  expected_files <- c("baz-bar.png", "baz-foo.png")
  do_match <- current_files == expected_files
  expect_true(all(do_match))
  file.remove(current_files)
})

test_that("templates successfully save displaylab templates", {
  skip("current data examples (va/mtx) will not produce a complete list of templates")
  save_path <- NA
  save_plots()
})

