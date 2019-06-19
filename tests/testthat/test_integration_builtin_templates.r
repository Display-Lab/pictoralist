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

test_that("Data provided is used in baked in Top Performer Template", {
  mtx_data <- read_data(spekex::get_data_path("mtx"))
  mtx_spek <- spekex::read_spek(spekex::get_spek_path("mtx"))

  templates <- load_templates()

  denom_colname <- 'total_scripts'
  numer_colname <- 'high_dose_scripts'

  recip_data <- filter(mtx_data, mtx_data$practice == "E87746")
  recip_data_zero <- filter(mtx_data, mtx_data$practice == "A81001")
  data_denom <- sum(recip_data[denom_colname])
  data_numer <- sum(recip_data[numer_colname])

  tpg_env <- templates$TopPerformerGraph
  result <- tpg_env$run("E87746", mtx_data, mtx_spek)
  result_zero <- tpg_env$run("A81001", mtx_data, mtx_spek)

  template_denom <- result$data$value[1]
  template_recip <- result$data$id[1]
  template_recip_zero <- result_zero$data$id[1]
  expect_true(template_denom == data_denom)
  expect_true(template_recip == "E87746")
  expect_true(template_recip_zero == "A81001")
})

test_that("Data provided is used in baked in IUD Graph Template", {
  mtx_data <- read_data(spekex::get_data_path("mtx"))
  mtx_spek <- spekex::read_spek(spekex::get_spek_path("mtx"))

  templates <- load_templates()

  denom_colname <- 'total_scripts'
  numer_colname <- 'high_dose_scripts'
  recip_data <- filter(mtx_data, mtx_data$practice == "E84076")
  data_denom <- sum(recip_data[denom_colname])
  data_numer <- sum(recip_data[numer_colname])

  iud_env <- templates$IUDGraph
  result <- iud_env$run("E84076", mtx_data, mtx_spek)

  template_recip <- result$data$id[1]
  template_numer <- result$data$numer[1]
  template_denom <- result$data$denom[1]

  expect_true(template_recip == "E84076")
  expect_true(template_numer == data_numer)
  expect_true(template_denom == data_denom)
})


