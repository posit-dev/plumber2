#' Launch the API
#'
#' This function starts the api with the settings it has defined.
#'
#' @param api A plumber2 api object to launch or stop
#' @param block Should the console be blocked while running (alternative is
#' to run in the background). Defaults to `FALSE` in interactive sessions and
#' `TRUE` otherwise.
#' @param showcase Should the default browser open up at the server address.
#' If `TRUE` then a browser opens at the root of the api, unless the api
#' contains OpenAPI documentation in which case it will open at that
#' location. If a string the string is used as a path to add to the root
#' before opening.
#' @param ... Arguments passed on to the `start` handler
#' @param silent Should startup messaging by silenced
#'
#' @return These functions return the `api` object allowing for easy chaining
#' with the pipe, even though they will often be the last part of the chain
#'
#' @export
#'
api_run <- function(
  api,
  block = !is_interactive(),
  showcase = is_interactive(),
  ...,
  silent = FALSE
) {
  api$ignite(block = block, showcase = showcase, ..., silent = silent)
  invisible(api)
}
#' @rdname api_run
#' @export
api_stop <- function(api) {
  api$extinguish()
  invisible(api)
}
