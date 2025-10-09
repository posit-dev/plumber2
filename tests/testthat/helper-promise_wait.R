on_ci <- isTRUE(as.logical(Sys.getenv("CI")))

# Block until all pending later tasks have executed
# wait_for_it <- function(timeout = if (on_ci) 60 else 30) {
wait_for_it <- function(p = NULL, timeout = if (on_ci) 60 else 30) {
  start <- Sys.time()
  err <- NULL
  if (!is.null(p)) {
    promises::catch(p, function(reason) err <<- reason)
  }
  while (!later::loop_empty()) {
    if (difftime(Sys.time(), start, units = "secs") > timeout) {
      stop("Waited too long")
    }
    later::run_now()
    Sys.sleep(0.01)
  }
  if (!is.null(err)) {
    withRestarts(
      stop(err),
      continue_test = function(e) NULL
    )
  }
}
# Block until the promise is resolved/rejected. If resolved, return the value.
# If rejected, throw (yes throw, not return) the error.
extract <- function(promise) {
  promise_value <- NULL
  f <- function(value) promise_value <<- value
  promise <- promises::then(promise, f)
  while (is.null(promise_value)) {
    wait_for_it(promise)
  }
  promise_value
}
