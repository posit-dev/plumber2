test_that("plumber blocks all gets called and that returns overwrite", {
  pa <- api("annotations/plumber.R")

  expect_true(pa$get_data("first"))
  expect_true(pa$get_data("second"))
})
