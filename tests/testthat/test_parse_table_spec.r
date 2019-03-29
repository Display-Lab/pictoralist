context("Parse Table Spec")

TABLE_NODE <- list(
  `@id` = "_:b4",
  `@type` = "http://www.w3.org/ns/csvw#Table",
  `http://www.w3.org/ns/csvw#tableSchema` = list( list(`@id` = "_:b7")),
  `http://purl.org/dc/terms/title` = list( list(`@value` = "Mock Performance Data")))

OTHER_NODES <- list(
  list(`@id` = "_:b1",
    `http://www.w3.org/ns/csvw#datatype` = list(list(`@value` = "integer")),
    `http://purl.org/dc/terms/description` = list(list(`@value` = "Time at which performance was measured.")),
    `http://www.w3.org/ns/csvw#name` = list(list(`@value` = "timepoint")),
    `http://www.w3.org/ns/csvw#titles` = list(list(`@value` = "Time")),
    `http://example.com/slowmo#ColumnUse` = list(list(`@value` = "time"))),
  list(`@id` = "_:b3",
    `http://www.w3.org/ns/csvw#datatype` = list( list(`@value` = "string")),
    `http://purl.org/dc/terms/description` = list( list(`@value` = "Performer unique ID")),
    `http://www.w3.org/ns/csvw#name` = list( list(`@value` = "id")),
    `http://www.w3.org/ns/csvw#titles` = list( list(`@value` = "Name")),
    `http://example.com/slowmo#ColumnUse` = list( list(`@value` = "identifier"))),
  list(`@id` = "_:b7",
    `http://www.w3.org/ns/csvw#columns` = list( list(`@id` = "_:b1"),
                                                list(`@id` = "_:b3"),
                                                list(`@id` = "_:b8"))),
  list(`@id` = "_:b8",
    `http://www.w3.org/ns/csvw#datatype` = list( list(`@value` = "integer")),
    `http://purl.org/dc/terms/description` = list( list(`@value` = "Demonstration performance value")),
    `http://www.w3.org/ns/csvw#name` = list(list(`@value` = "performance")),
    `http://www.w3.org/ns/csvw#titles` = list(list(`@value` = "Performance")),
    `http://example.com/slowmo#ColumnUse` = list(list(`@value` = "value")))
)

test_that("All column specs are available.",{
  spek <- c(list(TABLE_NODE), OTHER_NODES)
  result <- parse_table_spec(spek)

  expect_length(result$`http://www.w3.org/ns/csvw#columns`, 3)
  col_spec_lengths <- sapply(result$`http://www.w3.org/ns/csvw#columns`, "length")
  expect_true(all(col_spec_lengths == 6))
})

test_that("returns empty list if table is missing from spek.", {
  spek <- OTHER_NODES
  result <- parse_table_spec(spek)
  expect_length(result, 0)
})

test_that("returns empty list if table schema is missing from table.", {
  table_sans_schema <- TABLE_NODE[c(1,2,4)]
  spek <- c(list(table_sans_schema), OTHER_NODES)

  result <- parse_table_spec(spek)
  expect_length(result, 0)
})
