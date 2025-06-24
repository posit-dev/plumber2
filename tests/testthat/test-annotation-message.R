test_that("serial message handler works", {
  papi <- api("annotations/serial_message.R")
  papi$test_message(fiery::fake_request("http://127.0.0.1:8080"), FALSE, "hi")
  expect_equal(papi$get_data("message"), "hi")
})

test_that("async message handler works", {
  papi <- api("annotations/async_message.R")
  papi$test_message(fiery::fake_request("http://127.0.0.1:8080"), FALSE, "async hi")
  while(is.null(papi$get_data("async_message"))) {
    later::run_now()
    Sys.sleep(0.01)
  }
  expect_equal(papi$get_data("async_message"), "async hi")
})
