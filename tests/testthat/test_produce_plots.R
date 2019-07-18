library(ggplot2)
library(tibble)

context('Produce plots')

templates <- load_templates()

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

PROMOTED_CANDIDATES <- list(CANDI_1, CANDI_2 )

test_that("retuns a list of ggplot objects with mtxdata",{
  mtx_data <- read_data(spekex::get_data_path("mtx"))
  mtx_spek <- spekex::read_spek(spekex::get_spek_path("mtx"))
  template_idxs <- match(c("TopPerformerGraph", "IUDGraph"), names(templates))
  mtx_templates <- templates[template_idxs]

  result <- produce_plots(PROMOTED_CANDIDATES, mtx_templates, mtx_data, mtx_spek)
})
