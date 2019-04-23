context("Load templates")

DUMMY_TEMPLATE <- "\nlibrary(ggplot2)\nrun<-function(recip, data, spek){gglot()}"

test_that('sourced template adds template name to environment',{
  template_path <- tempfile(fileext=".r")
  cat(DUMMY_TEMPLATE, file=template_path)

  result <- source_template(template_path)

  expect_true(grepl(result$template_name, basename(template_path)))
})
