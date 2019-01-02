context("Read Data")

test_that("Throw error on NULL data path.", {
  expect_error(read_data(NULL), "Path to data is NULL" )
})

test_that("Throw error on non-existant path.", {
  expect_error(read_data('/bad/path'), "Path to data non-existant" )
})
