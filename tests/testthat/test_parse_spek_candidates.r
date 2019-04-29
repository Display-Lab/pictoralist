context('Parse Spek Templates')

SPEK_WITH_CANDIDATES <- list(
    list(
      `@id` = "http://example.com/app#Aaron",
      `@type` = "http://purl.obolibrary.org/obo/psdo_0000085",
      `http://purl.obolibrary.org/obo/RO_0000091` = list( list(`@value` = "mastery_unknown") )
    ),
    list(
      `@id` = "http://example.com/app/candidate1",
      `@type` = "http://example.com/cpo#cpo_0000053",
      `http://example.com/slowmo#AncestorPerformer` = list(list(`@value` = "http://example.com/app#Alice")),
      `http://example.com/slowmo#AncestorTemplate` = list( list(`@value` = "https://inferences.es/app/onto#ShowTrendTemplate") ),
      `http://schema.org/name` = list(list(`@value` = "trend figure"))
    ),
    list(
      `@id` = "http://example.com/app/candidate2",
      `@type` = "http://example.com/cpo#cpo_0000053",
      `http://example.com/slowmo#AncestorPerformer` = list(list(`@value` = "http://example.com/app#Carol")),
      `http://example.com/slowmo#AncestorTemplate` = list( list(`@value` = "https://inferences.es/app/onto#ShowTrendTemplate") ),
      `http://purl.obolibrary.org/obo/RO_0000091` = list(list(`@value` = "mastery_present")),
      `http://schema.org/name` = list(list(`@value` = "trend figure")),
      `http://example.com/slowmo#uses_intervention_property` = list(list(`@value` = "show_trend"))
    )
  )

SPEK_SANS_CANDIDATES <- list(
  list(
    `@id` = "http://example.com/app#Bob",
    `@type` = "http://purl.obolibrary.org/obo/psdo_0000085",
    `http://purl.obolibrary.org/obo/RO_0000091` = list(
      list(`@value` = "mastery_present"),
      list(`@value` = "positive_trend")
    ) ),
  list(
    `@id` = "http://example.com/app#Alice",
    `@type` = "http://purl.obolibrary.org/obo/psdo_0000085",
    `http://purl.obolibrary.org/obo/RO_0000091` = list(
      list(`@value` = "small_gap"),
      list(`@value` = "mastery_unknown")
    ) )
)

test_that("return a list of candidates from a spek with candidates", {
  res <- parse_spek_candidates(SPEK_WITH_CANDIDATES)

  expect_length(res, 2)
  expect_type(res, "list")
})

test_that("return an empty list from a spek without candidates", {
  res <- parse_spek_candidates(SPEK_SANS_CANDIDATES)

  expect_length(res, 0)
  expect_type(res, "list")
})
