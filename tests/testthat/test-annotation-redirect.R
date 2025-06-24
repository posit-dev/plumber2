test_that("redirect works", {
  papi <- api("annotations/redirect.R")

  req <- fiery::fake_request("http://127.0.0.1:8080/redir/test")
  res <- papi$test_header(req)
  expect_equal(res$status, 308L)
  expect_equal(res$headers$location, "/newdir/test")

  req <- fiery::fake_request("http://127.0.0.1:8080/broken/path/to/something")
  res <- papi$test_header(req)
  expect_equal(res$status, 307L)
  expect_equal(res$headers$location, "/temp/path/to/something")

  req <- fiery::fake_request("http://127.0.0.1:8080/remote")
  res <- papi$test_header(req)
  expect_equal(res$status, 307L)
  expect_equal(res$headers$location, "http://example.com")
})

test_that("forward works", {
  papi <- api("annotations/redirect.R")
  papi2 <- api(port = 9876) |>
    api_get("/", function() "hello", get_serializers("text"))
  papi2$ignite(block = FALSE, showcase = FALSE, silent = TRUE)
  on.exit(papi2$extinguish())

  req <- fiery::fake_request("http://127.0.0.1:8080/proxy")
  res <- papi$test_request(req)
  promise_impl <- attr(res, "promise_impl", exact = TRUE)
  while (promise_impl$status() == "pending") {
    later::run_now()
    Sys.sleep(0.01)
  }
  res <- private(promise_impl, "value")
  expect_equal(res$status, 200L)
  expect_equal(res$headers$`content-type`, "text/plain")
  expect_equal(rawToChar(res$body), "hello")
})
