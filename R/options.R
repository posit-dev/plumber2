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
#' # plumber2 options
#' The following options are currently recognized by plumber2. They are all read
#' at creation time and have a parallel argument in [api()] where you can also
#' see their default values. This means that changing an option after
#' creation/during running will have no effect.
#'
#' * **host**: The address to serve the server from
#' * **port**: The port to use for the server
#' * **docType**: The ui to use for serving OpenAPI documentation
#' * **docPath**: The path to serve the documentation from
#' * **rejectMissingMethods**: Should requests to paths that doesn't have a
#' handler for the specific method automatically be rejected with a 405 Method
#' Not Allowed response
#' * **ignoreTrailingSlash**: Should the trailing slash of a path be ignored
#' when adding handlers and handling requests
#' * **maxRequestSize**: The maximum allowed size of request bodies
#' * **sharedSecret**: A shared secret the request must contain to be permitted
#' * **compressionLimit**: The threshold for response size before automatic
#' compression is used
#' * **async**: The default async engine to use
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
    sharedSecret = get_opts("sharedSecret"),
    compressionLimit = get_opts("compressionLimit"),
    async = get_opts("async"),
    roxygenPrefix = get_opts("roxygenPrefix")
  ))
}
