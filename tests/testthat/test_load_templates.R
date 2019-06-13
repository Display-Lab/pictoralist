context("Load templates")

DUMMY_TEMPLATE <- "\nlibrary(ggplot2)\nrun<-function(recip, data, spek){gglot()}"

test_that('sourced template adds template name to environment',{
  template_path <- tempfile(fileext=".r")
  cat(DUMMY_TEMPLATE, file=template_path)

  result <- source_template(template_path)

  expect_true(grepl(result$template_name, basename(template_path)))
})

test_that("load template actually loads all templates bundled in package", {
  template_path <- system.file("templates", package="pictoralist")
  template_names <- list.files(path=template_path,"^.*(\\.[rR$])", full.names=FALSE)
  # Removes the '.r' or '.R'
  template_names <- sub("\\.[rR]", "", template_names)
  template_num <- length(template_names)

  template_envs <- load_templates()

  env_names <- names(template_envs)
  env_num <- length(env_names)

  expect_setequal(template_names, env_names)
  expect_equal(template_num, env_num)

  # Check that all environments contain exactly one function matching pattern "run"
  result <- sapply(template_envs, function(x){length(lsf.str(x,pattern="run")) == 1})
  expect_true(all(result))
})
