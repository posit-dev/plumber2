#' Create a new plumber API, optionally based on one or more plumber files
#'
#' This is the main way to create a new Plumber2 object that encapsulates your
#' full api. It is also possible to add files to the API after creation using
#' `api_parse()`
#'
#' @param ... plumber files or directories containing plumber files to be parsed
#' in the given order. The order of parsing determines the final order of the
#' routes in the stack. If `...` contains a `_server.yml` file then all other
#' files in `...` will be ignored and the `_server.yml` file will be used as the
#' basis for the API
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
#' @param compression_limit The size threshold in bytes for trying to
#' compress the response body (it is still dependant on content negotiation)
#' @param default_async The default evaluator to use for async request handling
#' @param env The parent environment to the environment the files should be
#' evaluated in. Each file will be evaluated in it's own environment so they
#' don't interfere with each other
#'
#' @return A [Plumber2] object
#'
#' @export
#'
api <- function(
  ...,
  host = get_opts("host", "127.0.0.1"),
  port = get_opts("port", 8080),
  doc_type = get_opts("docType", "rapidoc"),
  doc_path = get_opts("docPath", "__docs__"),
  reject_missing_methods = get_opts("rejectMissingMethods", FALSE),
  ignore_trailing_slash = get_opts("ignoreTrailingSlash", TRUE),
  max_request_size = get_opts("maxRequestSize"),
  shared_secret = get_opts("sharedSecret"),
  compression_limit = get_opts("compressionLimit", 1e3),
  default_async = get_opts("async", "future"),
  env = caller_env()
) {
  locations <- dots_to_plumber_files(...)
  if (isTRUE(is_plumber2_server_yml(locations))) {
    server_yml <- yaml::read_yaml(locations)
    if (!is.null(server_yml$constructor)) {
      api <- source(
        fs::path(
          fs::path_dir(locations),
          server_yml$constructor
        ),
        verbose = FALSE
      )
      if (!is_plumber_api(api)) {
        cli::cli_abort(
          "The constructor file in {.file {locations}} did not produce a plumber2 API"
        )
      }
    } else {
      api <- Plumber2$new(
        host = server_yml$options$host %||% host,
        port = server_yml$options$port %||% port,
        doc_type = server_yml$options$docType %||% doc_type,
        doc_path = server_yml$options$docPath %||% doc_path,
        reject_missing_methods = server_yml$options$rejectMissingMethods %||%
          reject_missing_methods,
        ignore_trailing_slash = server_yml$options$ignoreTrailingSlash %||%
          ignore_trailing_slash,
        max_request_size = server_yml$options$maxRequestSize %||%
          max_request_size,
        shared_secret = shared_secret,
        compression_limit = server_yml$options$compressionLimit %||%
          compression_limit,
        default_async = server_yml$options$default_async %||% default_async,
        env = env
      )
    }
    locations <- fs::path(
      fs::path_dir(locations),
      server_yml$routes
    )
  } else {
    api <- Plumber2$new(
      host = host,
      port = port,
      doc_type = doc_type,
      doc_path = doc_path,
      reject_missing_methods = reject_missing_methods,
      ignore_trailing_slash = ignore_trailing_slash,
      max_request_size = max_request_size,
      shared_secret = shared_secret,
      compression_limit = compression_limit,
      default_async = default_async,
      env = env
    )
  }
  api_parse(api, locations)
}
#' @rdname api
#' @param x An object to test for whether it is a plumber api
#' @export
#'
is_plumber_api <- function(x) inherits(x, "Plumber2")

#' @rdname api
#' @param api A plumber2 api object to parse files into
#' @export
api_parse <- function(api, ...) {
  locations <- dots_to_plumber_files(..., prefer_yml = FALSE)
  for (loc in locations) {
    api$parse_file(loc)
  }
  api
}

dots_to_plumber_files <- function(..., prefer_yml = TRUE, call = caller_env()) {
  locations <- unlist(lapply(list2(...), function(loc) {
    if (length(loc) == 0) return(NULL)
    if (fs::is_dir(loc)) {
      loc <- fs::dir_ls(loc, all = TRUE, recurse = TRUE)
      server_yml <- is_plumber2_server_yml(loc)
      if (prefer_yml && any(server_yml)) {
        loc <- loc[server_yml]
      } else {
        loc <- loc[fs::path_ext(loc) %in% c("R", "r")]
      }
    }
    loc
  }))
  if (length(locations) == 0) return(character())
  if (!all(fs::file_exists(locations))) {
    cli::cli_abort("{.arg ...} must point to existing files", call = call)
  }
  server_yml <- is_plumber2_server_yml(locations)
  if (prefer_yml && any(server_yml)) {
    if (sum(server_yml) != 1) {
      cli::cli_abort(
        "You can at most use one {.file _server.yml} file to specify your API",
        call = call
      )
    }
    if (length(locations) != 1) {
      cli::cli_warn(
        "{.file _server.yml} found. Ignoring all other files provided in {.arg ...}",
        call = call
      )
    }
    locations[server_yml]
  } else {
    if (any(server_yml)) {
      cli::cli_warn(
        "Ignoring {.file _server.yml} files in {.arg ...}",
        call = call
      )
    }
    locations[!server_yml]
  }
}

# For use by connect etc
launch_server <- function(settings, host = NULL, port = NULL) {
  papi <- api(settings)
  if (!is.null(host)) {
    papi$host <- host
  }
  if (!is.null(port)) {
    papi$port <- port
  }
  api_run(papi)
}
