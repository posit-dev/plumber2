test_that("redirect works", {
  pa <- api("annotations/redirect.R")

  req <- fiery::fake_request("http://127.0.0.1:8080/redir/test")
  res <- pa$test_header(req)
  expect_equal(res$status, 308L)
  expect_equal(res$headers$location, "/newdir/test")

  req <- fiery::fake_request("http://127.0.0.1:8080/broken/path/to/something")
  res <- pa$test_header(req)
  expect_equal(res$status, 307L)
  expect_equal(res$headers$location, "/temp/path/to/something")

  req <- fiery::fake_request("http://127.0.0.1:8080/remote")
  res <- pa$test_header(req)
  expect_equal(res$status, 307L)
  expect_equal(res$headers$location, "http://example.com")
})

test_that("forward works", {
  pa <- api("annotations/redirect.R")
  pa2 <- api(port = 9876) |>
    api_get("/", function() "hello", get_serializers("text"))
  pa2$ignite(block = FALSE, showcase = FALSE, silent = TRUE)
  on.exit(pa2$extinguish())

  req <- fiery::fake_request("http://127.0.0.1:8080/proxy")
  res <- pa$test_request(req)
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
