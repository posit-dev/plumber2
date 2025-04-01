#' Router control flow
#'
#' In plumber2 your API can have multiple middleware that a request passes
#' through. At any point can you short-circuit the remaining middleware by
#' returning `Break`, which instructs plumber2 to return the response as is.
#' Returning `Next` indicates the opposite, ie that the request should be
#' allowed to pass on to the next middleware in the chain. A handler function
#' that doesn't return either of these are assumed to return a value that should
#' be set to the response body and implicitely continue to the next middleware.
#'
#' @export
#'
Next <- structure(TRUE, class = "plumber_control")
#' @rdname Next
#' @export
#'
Break <- structure(FALSE, class = "plumber_control")

is_plumber_control <- function(x) inherits(x, "plumber_control")

#' @export
print.plumber_control <- function(x, ...) {
  if (x) {
    cli::cli_text("Move on to the next handler in the stack")
  } else {
    cli::cli_text("Stop prcessing and return the response")
  }
}

#' @rdname Next
#' @export
should_break <- function(x) is_plumber_control(x) && !x
