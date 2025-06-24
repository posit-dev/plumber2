test_that("plumber blocks all gets called and that returns overwrite", {
  papi <- api("annotations/plumber.R")

  expect_true(papi$get_data("first"))
  expect_true(papi$get_data("second"))
})
