context("Get Promoted Candidates")

CANDIDATES_NONE_PROMOTED <- list(
    list(
      `@id` = "http://example.com/app/candidate1",
      `@type` = "http://example.com/cpo#cpo_0000053",
      `http://example.com/slowmo#AncestorPerformer` = list(list(`@value` = "http://example.com/app#Alice")),
      `http://example.com/slowmo#AncestorTemplate` = list( list(`@value` = "https://inferences.es/app/onto#ShowTrendTemplate") )
    ),
    list(
      `@id` = "http://example.com/app/candidate2",
      `@type` = "http://example.com/cpo#cpo_0000053",
      `http://example.com/slowmo#AncestorPerformer` = list(list(`@value` = "http://example.com/app#Carol")),
      `http://example.com/slowmo#AncestorTemplate` = list( list(`@value` = "https://inferences.es/app/onto#ShowTrendTemplate") )
    )
  )

CANDIDATES_PROMOTED <- list(
    list(
      `@id` = "http://example.com/app/candidate1",
      `@type` = "http://example.com/cpo#cpo_0000053",
      `http://example.com/slowmo#AncestorPerformer` = list(list(`@value` = "http://example.com/app#Alice")),
      `http://example.com/slowmo#AncestorTemplate` = list( list(`@value` = "https://inferences.es/app/onto#ShowTrendTemplate") ),
      `http://example.com/slowmo#promoted_by` = list(list(`@id` = "http://example.com/slowmo#default_esteemer_criteria"))
    ),
    list(
      `@id` = "http://example.com/app/candidate2",
      `@type` = "http://example.com/cpo#cpo_0000053",
      `http://example.com/slowmo#AncestorPerformer` = list(list(`@value` = "http://example.com/app#Carol")),
      `http://example.com/slowmo#AncestorTemplate` = list( list(`@value` = "https://inferences.es/app/onto#ShowTrendTemplate") ),
      `http://example.com/slowmo#promoted_by` = list(list(`@id` = "http://example.com/slowmo#default_esteemer_criteria"))
    )
  )

CANDIDATES_MIX <- c(CANDIDATES_NONE_PROMOTED, CANDIDATES_PROMOTED)

test_that("returns empty list when no promoted candidates present.", {
  res <- get_promoted_candidates(CANDIDATES_NONE_PROMOTED)

  expect_length(res, 0)
  expect_type(res, "list")
})

test_that("returns only promoted candiates from mix of promoted and not.", {
  res <- get_promoted_candidates(CANDIDATES_MIX)
  res_ids <- sapply(res, getElement, "@id")
  promoted_ids <- sapply(CANDIDATES_PROMOTED, getElement, "@id")

  expect_type(res, "list")
  expect_length(res, length(CANDIDATES_PROMOTED))
  expect_setequal(res_ids, promoted_ids)
})
