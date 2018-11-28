context("Resolve spek input")

test_that("returns stdin given null", {
  expect_identical( resolve_spek_input(NULL), stdin())
})

test_that("returns character string given string", {
  expect_equal( resolve_spek_input("foo"), "foo")
})
