library(ggplot2)
library(tibble)

context('Produce plots')

templates <- load_templates()

# Candidate Nodes for mtx data
CANDI_1 <- list( `@id` = "http://example.com/app/candidate1",
                 `@type` = "http://example.com/cpo#cpo_0000053")
CANDI_1[PT$ANC_PERFORMER_URI] <- list(list(`@value` = paste0(PT$APP_BASE_URI,"E87746")))
CANDI_1[PT$ANC_TEMPLATE_URI]  <- list( list(`@value` = paste0(PT$APP_BASE_URI,"TopPerformerGraph")))
CANDI_1[PT$PROMOTED_URI] <- list(list(`@id` = "http://example.com/slowmo#default_esteemer_criteria"))

CANDI_2 <- list( `@id` = "http://example.com/app/candidate2",
                 `@type` = "http://example.com/cpo#cpo_0000053")
CANDI_2[PT$ANC_PERFORMER_URI] <- list(list(`@value` = paste0(PT$APP_BASE_URI,"E87746")))
CANDI_2[PT$ANC_TEMPLATE_URI]  <- list( list(`@value` = paste0(PT$APP_BASE_URI,"IUDGraph")))
CANDI_2[PT$PROMOTED_URI] <- list(list(`@id` = "http://example.com/slowmo#default_esteemer_criteria"))

MTX_PROMOTED_CANDIDATES <- list(CANDI_1, CANDI_2)

# Candidate nodes for va data
CANDI_3 <- list( `@id` = "http://example.com/app/candidate3",
                 `@type` = "http://example.com/cpo#cpo_0000053")
CANDI_3[PT$ANC_PERFORMER_URI] <- list(list(`@value` = paste0(PT$APP_BASE_URI,"6559AA")))
CANDI_3[PT$ANC_TEMPLATE_URI]  <- list( list(`@value` = paste0(PT$APP_BASE_URI,"SingleLineGraph")))
CANDI_3[PT$PROMOTED_URI] <- list(list(`@id` = "http://example.com/slowmo#default_esteemer_criteria"))

CANDI_4 <- list( `@id` = "http://example.com/app/candidate4",
                 `@type` = "http://example.com/cpo#cpo_0000053")
CANDI_4[PT$ANC_PERFORMER_URI] <- list(list(`@value` = paste0(PT$APP_BASE_URI,"6559AA")))
CANDI_4[PT$ANC_TEMPLATE_URI]  <- list( list(`@value` = paste0(PT$APP_BASE_URI,"ComparisonLineGraph")))
CANDI_4[PT$PROMOTED_URI] <- list(list(`@id` = "http://example.com/slowmo#default_esteemer_criteria"))

VA_PROMOTED_CANDIDATES <- list(CANDI_3, CANDI_4)

# Candidate with invalid template
INV_CANDI <- list( `@id` = "http://example.com/app/candidate2",
                 `@type` = "http://example.com/cpo#cpo_0000053")
INV_CANDI[PT$ANC_PERFORMER_URI] <- list(list(`@value` = paste0(PT$APP_BASE_URI,"E87746")))
INV_CANDI[PT$ANC_TEMPLATE_URI]  <- list( list(`@value` = paste0(PT$APP_BASE_URI,"foo")))
INV_CANDI[PT$PROMOTED_URI] <- list(list(`@id` = "http://example.com/slowmo#default_esteemer_criteria"))

ONE_INV_CANDIDATE <- list(INV_CANDI, CANDI_3)

test_that("produce_plots returns a list of ggplot objects with mtxdata", {
  mtx_data <- read_data(spekex::get_data_path("mtx"))
  mtx_spek <- spekex::read_spek(spekex::get_spek_path("mtx"))
  template_idxs <- match(c("TopPerformerGraph", "IUDGraph"), names(templates))
  mtx_templates <- templates[template_idxs]

  result <- produce_plots(MTX_PROMOTED_CANDIDATES, mtx_templates, mtx_data, mtx_spek)

  expect_s3_class(result[[1]], "ggplot")
  expect_true(length(result) == 2)
})

test_that("produce_plots returns a list of ggplot objects with vadata", {
  va_data <- read_data(spekex::get_data_path("va"))
  va_spek <- spekex::read_spek(spekex::get_spek_path("va"))
  template_idxs <- match(c("SingleLineGraph", "ComparisonLineGraph"), names(templates))
  va_templates <- templates[template_idxs]

  result <- produce_plots(VA_PROMOTED_CANDIDATES, va_templates, va_data, va_spek)

  expect_s3_class(result[[1]], "ggplot")
  expect_true(length(result) == 2)
})

test_that("produce_plots emits error when template is not found", {
  va_data <- read_data(spekex::get_data_path("va"))
  va_spek <- spekex::read_spek(spekex::get_spek_path("va"))
  template_idxs <- match(c("SingleLineGraph", "foo"), names(templates))
  va_templates <- templates[template_idxs]

  result <- produce_plots(ONE_INV_CANDIDATE, va_templates, va_data, va_spek)

  expect_true(length(result) == 1)
})
