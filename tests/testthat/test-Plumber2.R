test_that("Plumber2 gets initialized with correct values", {
  papi <- Plumber2$new()
  expect_null(papi$.__enclos_env__$private$HEADER_ROUTER)
  expect_null(papi$.__enclos_env__$private$REQUEST_ROUTER)

  papi <- Plumber2$new(max_request_size = 1024, shared_secret = "test")
  expect_equal(papi$header_router$routes, c("max_size", "shared_secret"))
})

test_that("Plumber2 prints info", {
  papi <- Plumber2$new()

  expect_snapshot(papi$format())

  papi$start(block = FALSE, showcase = FALSE, silent = TRUE)

  expect_snapshot(papi$format())

  papi$stop()
})

test_that("Plumber2 attaches doc route at startup", {
  papi <- Plumber2$new()

  expect_length(papi$request_router$routes, 0)

  papi$start(block = FALSE, showcase = FALSE, silent = TRUE)

  expect_equal(papi$request_router$routes, "openapi")

  papi$stop()

  expect_length(papi$request_router$routes, 0)
})
