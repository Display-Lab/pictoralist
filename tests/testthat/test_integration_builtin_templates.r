context("Integration test of baked in templates")


test_that("Baked in templates work with mtx data",{
  mtx_data <- read_data(spekex::get_data_path("mtx"))
  mtx_spek <- spekex::read_spek(spekex::get_spek_path("mtx"))

  templates <- load_templates()

  results <- lapply(templates, FUN=function(t, recip, data, spek){t$run(recip, data, spek)},
                    recip = "E87746", data=mtx_data, spek=mtx_spek)
  is_ggplot <- sapply(results, function(x){"ggplot" %in% class(x)})
  expect_true(all(is_ggplot))
})
