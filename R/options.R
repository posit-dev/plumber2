#' Retrieve options for creating a plumber2 api
#'
#' You can provide options for your plumber2 api which will be picked up when
#' you create the API with [api()]. Options can be set either through the
#' internal [options()] functionality, or by setting environment variables. In
#' the former case, the name of the option must be prefixed with `"plumber2."`,
#' in the latter case the variable name must be in upper case and prefixed with
#' `"PLUMBER2_"`. If the option is stored as an environment variable then the
#' value is cast to the type giving in `default`. See the docs for [api()] for
#' the default values of the different options.
#'
#' @param x The name of the option
#' @param default The default value, if `x` is not set
#'
#' @return For `get_opts` The value of `x`, if any, or `default`. For
#' `all_opts()` a named list of all the options that are set
#'
#' @export
#'
#' @examples
#' # Using `options()`
#' old_opts <- options(plumber2.port = 9889L)
#' get_opts("port")
#' options(old_opts)
#'
#' # Using environment variables
#' old_env <- Sys.getenv("PLUMBER2_PORT")
#' Sys.setenv(PLUMBER2_PORT = 9889)
#'
#' ## If no default is provided the return value is a string
#' get_opts("port")
#'
#' ## Provide a default to hint at the options type
#' get_opts("port", 8080L)
#'
#' Sys.setenv(PLUMBER2_PORT = old_env)
#'
get_opts <- function(x, default = NULL) {
  getOption(paste0("plumber2.", x), default = {
    env_name <- toupper(paste0("PLUMBER2_", x))
    res <- Sys.getenv(env_name)
    if (res == "") {
      res <- default
    } else {
      if (!is.null(default) && is.atomic(default)) {
        mode(res) <- mode(default)
      }
    }
    res
  })
}

#' @rdname get_opts
#' @export
all_opts <- function() {
  compact(list(
    host = get_opts("host"),
    port = get_opts("port"),
    docType = get_opts("docType"),
    docPath = get_opts("docPath"),
    rejectMissingMethods = get_opts("rejectMissingMethods"),
    ignoreTrailingSlash = get_opts("ignoreTrailingSlash"),
    maxRequestSize = get_opts("maxRequestSize"),
    compressionLimit = get_opts("compressionLimit"),
    async = get_opts("async"),
    roxygenPrefix = get_opts("roxygenPrefix")
  ))
}
