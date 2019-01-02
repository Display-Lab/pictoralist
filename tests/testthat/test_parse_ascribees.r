context('Parse Ascribees')

CANDIDATES <- list(
    list(
      `@id` = "http://example.com/app/candidate1",
      `@type` = "http://example.com/cpo#cpo_0000053",
      `http://example.com/slowmo#AncestorPerformer` = list(list(`@value` = "http://example.com/app#Alice")),
      `http://example.com/slowmo#AncestorTemplate` = list( list(`@value` = "https://inferences.es/app/onto#ShowTrendTemplate") )
    ),
    list(
      `@id` = "http://example.com/app/candidate2",
      `@type` = "http://example.com/cpo#cpo_0000053",
      `http://example.com/slowmo#AncestorPerformer` = list(list(`@value` = "http://example.com/app#Bob")),
      `http://example.com/slowmo#AncestorTemplate` = list( list(`@value` = "https://inferences.es/app/onto#ShowTrendTemplate") )
    ),
    list(
      `@id` = "http://example.com/app/candidate3",
      `@type` = "http://example.com/cpo#cpo_0000053",
      `http://example.com/slowmo#AncestorPerformer` = list(list(`@value` = "http://example.com/app#Carol")),
      `http://example.com/slowmo#AncestorTemplate` = list( list(`@value` = "https://inferences.es/app/onto#ShowTrendTemplate") )
    ),
    list(
      `@id` = "http://example.com/app/candidate4",
      `@type` = "http://example.com/cpo#cpo_0000053",
      `http://example.com/slowmo#AncestorPerformer` = list(list(`@value` = "http://example.com/app#Alice")),
      `http://example.com/slowmo#AncestorTemplate` = list( list(`@value` = "https://inferences.es/app/onto#ShowTrendTemplate") )
    )
  )

EMPTY_CANDIDATES <- list()

UNIQUE_ASCRIBEES <- c("http://example.com/app#Alice", "http://example.com/app#Bob", "http://example.com/app#Carol")

test_that("returns empty list when no ancestor performers present.", {
  res <- parse_ascribees(EMPTY_CANDIDATES)
  expect_length(res, 0)
  expect_type(res, "character")
})

test_that("returns list of unique performers present.", {
  res <- parse_ascribees(CANDIDATES)

  expect_type(res, "character")
  expect_equal(length(res), length(UNIQUE_ASCRIBEES))
  expect_setequal(res, UNIQUE_ASCRIBEES)
})
