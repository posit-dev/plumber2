test_that("serial message handler works", {
  pa <- api("annotations/serial_message.R")
  pa$test_message(fiery::fake_request("http://127.0.0.1:8080"), FALSE, "hi")
  expect_equal(pa$get_data("message"), "hi")
})

test_that("async message handler works", {
  pa <- api("annotations/async_message.R")
  pa$test_message(
    fiery::fake_request("http://127.0.0.1:8080"),
    FALSE,
    "async hi"
  )
  while (is.null(pa$get_data("async_message"))) {
    later::run_now()
    Sys.sleep(0.01)
  }
  expect_equal(pa$get_data("async_message"), "async hi")
})
