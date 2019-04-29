context("Integration test of baked in templates")


test_that("Baked in templates work with mtx data",{
  mtx_data <- gen_mtx_behavior_data()
  mtx_spek <- gen_mtx_behavior_spek()

  templates <- load_templates()

  results <- lapply(templates, FUN=function(t, recip, data, spek){t$run(recip, data, spek)},
                    recip = "E87746", data=mtx_data, spek=mtx_spek)

})
