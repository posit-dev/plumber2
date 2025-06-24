test_that("Assets can be served and excluded", {
  papi <- api("annotations/assets.R")

  req <- fiery::fake_request("http://127.0.0.1:8080/assets/assets.R")
  res <- papi$test_request(req)
  expect_equal(res$body, c(file = fs::path_abs("assets.R", "./annotations")))

  req <- fiery::fake_request("http://127.0.0.1:8080/assets/not_a_file.txt")
  res <- papi$test_request(req)
  expect_equal(res$status, 404L)

  papi$ignite(block = FALSE, showcase = FALSE, silent = TRUE)
  on.exit(papi$extinguish())
  req <- reqres::Request$new(fiery::fake_request("http://example.com"))
  res <- req$forward("http://127.0.0.1:8080/statics/global_api.R")
  promise_impl <- attr(res, "promise_impl", exact = TRUE)
  while (promise_impl$status() == "pending") {
    later::run_now()
    Sys.sleep(0.01)
  }
  expect_equal(req$respond()$status, 200L)

  res <- req$forward("http://127.0.0.1:8080/statics/assets.R")
  promise_impl <- attr(res, "promise_impl", exact = TRUE)
  while (promise_impl$status() == "pending") {
    later::run_now()
    Sys.sleep(0.01)
  }
  expect_equal(req$respond()$status, 404L)
})
