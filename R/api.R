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
#' @param doc_type The type of API documentation to generate. Can be either
#' `"rapidoc"` (the default), `"redoc"`, `"swagger"`, or `NULL` (equating to not
#' generating API docs)
#' @param doc_path The URL path to serve the api documentation from
#' @param reject_missing_methods Should requests to paths that doesn't
#' have a handler for the specific method automatically be rejected with a
#' 405 Method Not Allowed response with the correct Allow header informing
#' the client of the implemented methods. Assigning a handler to `"any"` for
#' the same path at a later point will overwrite this functionality. Be
#' aware that setting this to `TRUE` will prevent the request from falling
#' through to other routes that might have a matching method and path. This
#' setting anly affects handlers on the request router.
#' @param ignore_trailing_slash Logical. Should the trailing slash of a path
#' be ignored when adding handlers and handling requests. Setting this will
#' not change the request or the path associated with but just ensure that
#' both `path/to/ressource` and `path/to/ressource/` ends up in the same
#' handler.
#' @param max_request_size Sets a maximum size of request bodies. Setting this
#' will add a handler to the header router that automatically rejects requests
#' based on their `Content-Length` header
#' @param shared_secret Assigns a shared secret to the api. Setting this will
#' add a handler to the header router that automatically rejects requests if
#' their `Plumber-Shared-Secret` header doesn't contain the same value. Be aware
#' that this type of authentication is very weak. Never put the shared secret in
#' plain text but rely on e.g. the keyring package for storage. Even so, if
#' requests are send over HTTP (not HTTPS) then anyone can read the secret and
#' use it
#' @param env The parent environment to the environment the files should be
#' evaluated in. Each file will be evaluated in it's own environment so they
#' don't interfere with each other
#'
#' @return A Plumber object
#'
#' @export
#'
api <- function(
  ...,
  host = get_opts("host", "127.0.0.1"),
  port = get_opts("port", 8080),
  doc_type = get_opts("docs", "rapidoc"),
  doc_path = get_opts("apiPath", "__docs__"),
  reject_missing_methods = get_opts("methodNotAllowed", FALSE),
  ignore_trailing_slash = get_opts("trailingSlash"),
  max_request_size = get_opts("maxRequestSize"),
  shared_secret = get_opts("sharedSecret"),
  env = caller_env()
) {
  api <- Plumber$new(
    host = host,
    port = port,
    doc_type = doc_type,
    doc_path = doc_path,
    reject_missing_methods = reject_missing_methods,
    ignore_trailing_slash = ignore_trailing_slash,
    max_request_size = max_request_size,
    shared_secret = shared_secret
  )
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
