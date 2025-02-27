Next <- structure(TRUE, class = "plumber_control")
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

should_break <- function(x) is_plumber_control(x) && !x
