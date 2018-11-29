library(mockery)
context("Resolve spek input")

DEMO_INPUT <- '{"foo":"bar"}'

test_that("reads lines from stdin given no args", {
  input_mock <- mock(DEMO_INPUT)
  stub(resolve_spek_input,'readLines',input_mock)

  # Check that the output is as expected
  expect_equal( resolve_spek_input(NULL), DEMO_INPUT)

  # Check that readLines was called with stdin
  arg <- mock_args(input_mock)[[1]][[1]]
  expect_identical(arg, 'stdin')
})

test_that("reads lines from filename arg", {
  input_mock <- mock(DEMO_INPUT)
  stub(resolve_spek_input,'readr::read_file',input_mock)

  res <- resolve_spek_input('foo')

  arg <- mock_args(input_mock)[[1]][[1]]
  expect_identical(arg, 'foo')
})
