#' Launch the API
#'
#' This function starts the api with the settings it has defined.
#'
#' @param api A plumber2 api object to launch or stop
#' @param host,port Host and port to run the api on. If not provided the host
#' and port used during the creation of the Plumber2 api will be used
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
#' @examples
#' pa <- api() |>
#'   api_get("/", function() {
#'     list(msg = "Hello World")
#'   }) |>
#'   api_on("start", function(...) {
#'     cat("I'm alive")
#'   })
#'
#' # Start the server
#' pa |> api_run(block = FALSE)
#'
#' # Stop it again
#' pa |> api_stop()
#'
api_run <- function(
  api,
  host = NULL,
  port = NULL,
  block = !is_interactive(),
  showcase = is_interactive(),
  ...,
  silent = FALSE
) {
  if (!is.null(host) || !is.null(port)) {
    old_host <- api$host
    old_port <- api$port
    api$host <- ifelse(!is.null(host), host, old_host)
    api$port <- ifelse(!is.null(port), port, old_port)
    on.exit(
      {
        api$host <- old_host
        api$port <- old_port
      },
      add = TRUE
    )
  }
  api$ignite(block = block, showcase = showcase, ..., silent = silent)
  invisible(api)
}
#' @rdname api_run
#' @export
api_stop <- function(api) {
  api$extinguish()
  invisible(api)
}
