#' Create a new plumber API, optionally based on one or more plumber files
#'
#' This is the main way to create a new Plumber object that encapsulates your
#' full api.
#'
#' @param ... plumber files or directories containing plumber files to be parsed
#' in the given order. The order of parsing determines the final order of the
#' routes in the stack
#' @param host A string that is a valid IPv4 address that is owned by this
#' server
#' @param port A number or integer that indicates the server port that should be
#' listened on. Note that on most Unix-like systems including Linux and macOS,
#' port numbers smaller than 1024 require root privileges.
#' @param env The environment the files should be parsed in
#'
#' @return A Plumber object
#'
#' @export
#'
api <- function(..., host = get_opts("host", "127.0.0.1"), port = get_opts("port", 8080), env = caller_env()) {
  api <- Plumber$new(host, port)
  locations <- list2(...)
  lapply(locations, function(loc) {
    if (fs::is_dir(loc)) {
      loc <- fs::dir_ls(loc)
    }
    for (file in loc) {
      api$parse_file(file, env = env)
    }
  })
  api
}
#' @rdname api
#' @param x An object to test for whether it is a plumber api
#' @export
#'
is_plumber_api <- function(x) inherits(x, "Plumber")
