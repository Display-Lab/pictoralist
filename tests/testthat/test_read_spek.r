require(mockery)
context("Read Spek")

JSON_EXAMPLE <- '{
  "@graph" : [{
    "@id" : "http://example.com/app#Alice",
    "@type" : "http://example.com/slowmo#ascribee",
    "RO_0000091" : [ "small_gap", "negative_trend", "negative_gap", "mastery_unknown" ]
  }, {
    "@id" : "http://example.com/app#Bob",
    "@type" : "http://example.com/slowmo#ascribee",
    "RO_0000091" : [ "mastery_present", "positive_trend" ]
  }]
}'

MALFORMED_JSON <- '{ "foo": 4, "bar": 5'


test_that("returns a list from a spek", {
  # Mock the resolve spec input function
  stub(read_spek, 'resolve_spek_input', JSON_EXAMPLE )

  res <- read_spek()
  expect_type(res, "list")
})

test_that("throws parsing error if input is not JSON", {
  stub(read_spek, 'resolve_spek_input', MALFORMED_JSON )
  expect_error(read_spek())
})

