#' The Plumber Class
#'
#' @description
#' This class encapsulates all of the logic of a plumber2 api, and is what gets
#' passed around in the functional api of plumber2. The Plumber class is a
#' subclass of the [fiery::Fire] class. Please consult the documentation for
#' this for additional information on what this type of server is capable of.
#' Note that the Plumber objects are reference objects, meaning that any change
#' to it will change all instances of the object.
#'
#' ## Initialization
#' A new 'Plumber'-object is initialized using the `new()` method on the
#' generator:
#'
#' \tabular{l}{
#'  `api <- Plumber$new()`
#' }
#'
#' However, most users will use the functional api of the package and thus
#' construct one using [api()]
#'
#' ## Copying
#' As `Plumber` objects are using reference semantics new copies of an api cannot
#' be made simply be assigning it to a new variable. If a true copy of a `Plumber`
#' object is desired, use the `clone()` method.
#'
#' @importFrom R6 R6Class
#' @importFrom fiery Fire
#' @importFrom jsonlite write_json
#' @importFrom routr RouteStack Route openapi_route
#' @importFrom stringi stri_replace_all_regex
#'
#' @export
#'
Plumber <- R6Class(
  "Plumber",
  inherit = Fire,
  public = list(
    #' @description Create a new `Plumber` api
    #' @param host A string overriding the default host
    #' @param port An port number overriding the default port
    #' @param doc_type The type of API documentation to generate. Can be either
    #' `"rapidoc"` (the default), `"redoc"`, `"swagger"`, or `NULL` (equating to
    #' not generating API docs)
    #' @param doc_path The URL path to serve the api documentation from
    #' @param reject_missing_methods Should requests to paths that doesn't
    #' have a handler for the specific method automatically be rejected with a
    #' 405 Method Not Allowed response with the correct Allow header informing
    #' the client of the implemented methods. Assigning a handler to `"any"` for
    #' the same path at a later point will overwrite this functionality. Be
    #' aware that setting this to `TRUE` will prevent the request from falling
    #' through to other routes that might have a matching method and path. This
    #' setting anly affects handlers on the request router.
    #' @param ignore_trailing_slash One of `"no"`, `"redirect"`, or `"remap"`.
    #' If `"no"` then the router consider the URL paths `path/to/ressource` and
    #' `path/to/ressource/` as different and they will end in different handlers.
    #' If `"redirect"` then any request that is made to a path with a trailing
    #' slash is send a `308 Permanent Redirect` response instructing the request
    #' to be redirected to the path without a slash. If `"remap"` then the
    #' trailing slash is silently removed from the request path before searching
    #' for handlers in the different routes of the stack. For the two last
    #' options all routes added to the stack will have the terminal slash
    #' removed from their handler paths
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
    #' @return A `Plumber` object
    initialize = function(
      host = get_opts("host", "127.0.0.1"),
      port = get_opts("port", 8080),
      doc_type = get_opts("docs", "redoc"),
      doc_path = get_opts("apiPath", "__docs__"),
      reject_missing_methods = get_opts("methodNotAllowed", FALSE),
      ignore_trailing_slash = get_opts("trailingSlash"),
      max_request_size = get_opts("maxRequestSize"),
      shared_secret = get_opts("sharedSecret")
    ) {
      super$initialize(host, port)

      if (!is.null(doc_type)) {
        private$DOC_TYPE <- arg_match0(
          doc_type,
          c("rapidoc", "redoc", "swagger")
        )
      }
      check_string(doc_path)
      private$DOC_PATH <- doc_path
      check_bool(reject_missing_methods)
      private$REJECT_MISSING_METHODS <- reject_missing_methods
      ignore_trailing_slash <- ignore_trailing_slash %||% "redirect"
      if (is.logical(ignore_trailing_slash)) {
        ignore_trailing_slash <- if (ignore_trailing_slash) "redirect" else "no"
      }
      private$IGNORE_TRAILING_SLASH <- arg_match0(
        ignore_trailing_slash,
        c("no", "redirect", "remap")
      )
      check_number_decimal(max_request_size, allow_null = TRUE)
      check_string(shared_secret, allow_null = TRUE)

      header_route <- create_header_route(
        max_request_size,
        shared_secret,
        private$IGNORE_TRAILING_SLASH
      )
      if (length(header_route$routes) != 0) {
        private$HEADER_ROUTER <- header_route
      }
    },
    #' @description Human readable description of the api object
    #' @param ... ignored
    #' @return A character vector
    format = function(...) {
      cli::cli_fmt({
        cli::cli_rule("A plumber server")
        cli::cli_text("Serving on http://{self$host}:{self$port}")
        cli::cli_text(
          "Currently {if (self$is_running()) cli::col_green('running') else cli::col_red('not running')}"
        )
      })
    },
    #' @description Begin running the server. Will trigger the `start` event
    #' @param block Should the console be blocked while running (alternative is
    #' to run in the background)
    #' @param showcase Should the default browser open up at the server address.
    #' If `TRUE` then a browser opens at the root of the api, unless the api
    #' contains OpenAPI documentation in which case it will open at that
    #' location. If a string the string is used as a path to add to the root
    #' before opening.
    #' @param ... Arguments passed on to the `start` handler
    #' @param silent Should startup messaging by silenced
    ignite = function(
      block = FALSE,
      showcase = is_interactive(),
      ...,
      silent = FALSE
    ) {
      if (length(private$OPENAPI) != 0 && !is.null(private$DOC_TYPE)) {
        openapi_file <- tempfile(fileext = ".json")
        write_json(private$OPENAPI, openapi_file, auto_unbox = TRUE)
        api_route <- openapi_route(
          openapi_file,
          root = private$DOC_PATH,
          ui = private$DOC_TYPE
        )
        self$request_router$add_route(api_route, "openapi")

        self$on(
          "end",
          function(...) {
            on.exit(self$off("__plumber_cleanup__"))
            self$request_router$remove_route("openapi")
            unlink(openapi_file)
          },
          id = "__plumber_cleanup__"
        )

        if (isTRUE(showcase)) {
          showcase <- sub("/?$", "/", private$DOC_PATH)
        }
      }

      if (!silent)
        cli::cli_text(
          "Plumber server started at http://{self$host}:{self$port}"
        )
      super$ignite(block = block, showcase = showcase, ..., silent = TRUE)
    },
    #' @description Add a new route to either the request or header router
    #' @param name The name of the route to add. If a route is already present
    #' with this name then the provided route (if any) is merged into it
    #' @param route The route to add. If `NULL` a new empty route will be
    #' created
    #' @param header Logical. Should the route be added to the header router?
    #' @param after The location to place the new route on the stack. `NULL`
    #' will place it at the end. Will not have an effect if a route with the
    #' given name already exists.
    add_route = function(name, route = NULL, header = FALSE, after = NULL) {
      route <- route %||% Route$new()
      if (header) {
        router <- self$header_router
      } else {
        router <- self$request_router
      }
      if (!router$has_route(name)) {
        router$add_route(route, name, after)
      } else if (!route$empty) {
        router$get_route(name)$merge(route)
      }
    },
    #' @description Add a handler to a request. See [api_request_handlers] for
    #' detailed information
    #' @param method The HTTP method to attach the handler to
    #' @param path A string giving the path the handler responds to.
    #' @param handler A handler function to call when a request is matched to
    #' the path
    #' @param serializers A named list of serializers that can be used to format
    #' the response before sending it back to the client. Which one is selected
    #' is based on the request `Accept` header
    #' @param parsers A named list of parsers that can be used to parse the
    #' request body before passing it in as the `body` argument. Which one is
    #' selected is based on the request `Content-Type` header
    #' @param use_strict_serializer By default, if a serializer that respects
    #' the requests `Accept` header cannot be found, then the first of the
    #' provided ones are used. Setting this to `TRUE` will instead send back a
    #' `406 Not Acceptable` response
    #' @param download Should the response mark itself for download instead of
    #' being shown inline? Setting this to `TRUE` will set the
    #' `Content-Disposition` header in the response to `attachment`. Setting it
    #' to a string is equivalent to setting it to `TRUE` but will in addition
    #' also set the default filename of the download to the string value
    #' @param route The route this handler should be added to. Defaults to the
    #' last route in the stack. If the route does not exist it will be created
    #' as the last route in the stack.
    #' @param header Logical. Should the handler be added to the header router
    #'
    request_handler = function(
      method,
      path,
      handler,
      serializers,
      parsers = NULL,
      use_strict_serializer = FALSE,
      download = FALSE,
      route = NULL,
      header = FALSE
    ) {
      method <- arg_match0(
        tolower(method),
        c(
          "get",
          "head",
          "post",
          "put",
          "delete",
          "connect",
          "options",
          "trace",
          "patch",
          "any",
          "all"
        )
      )
      if (method == "any") method <- "all"
      check_bool(header)
      check_string(path)
      check_string(route, allow_null = TRUE)

      # Substitute the plumber style path arg for a routr style
      path <- stri_replace_all_regex(path, "<(.+?)(:.+?)?>", ":$1")

      if (header) {
        router <- self$header_router
      } else {
        router <- self$request_router
      }

      if (is.null(route)) {
        if (length(router$routes) == 0) {
          route <- "default"
        } else {
          route <- router$routes[length(route$routes)]
        }
      }
      if (!router$has_route(route)) {
        cli::cli_inform(
          "Creating {.field {route}} route in {if (header) 'header' else 'request'} router"
        )
        self$add_route(route, header = header)
      }
      route <- router$get_route(route)

      route$add_handler(
        method,
        path,
        create_plumber_request_handler(
          handler,
          serializers,
          parsers,
          use_strict_serializer,
          download
        ),
        reject_missing_methods = !header && private$REJECT_MISSING_METHODS
      )
    },
    #' @description Add a handler to a WebSocket message. See [api_message] for
    #' detailed information
    #' @param handler A function conforming to the specifications laid out in
    #' [api_message()]
    message_handler = function(handler) {
      handler <- create_plumber_message_handler(handler)
      self$on("message", handler)
    },
    #' @description Parses a plumber file and updates the app according to it
    #' @param file The path to a file to parse
    #' @param env The environment to evaluate the content of the file in
    parse_file = function(file, env = caller_env()) {
      parsed <- parse_plumber_file(file, env)
      if (!parsed$route[[1]]$empty) {
        if (!self$request_router$has_route(names(parsed$route))) {
          self$add_route(names(parsed$route))
        }
        self$request_router$get_route(names(
          parsed$route
        ))$merge_route(parsed$route[[1]])
      }
      if (!parsed$header_route[[1]]$empty) {
        if (!self$header_router$has_route(names(parsed$header_route))) {
          self$add_route(names(parsed$header_route), header = TRUE)
        }
        self$header_router$get_route(names(
          parsed$header_route
        ))$merge_route(parsed$header_route[[1]])
      }
      for (asset in parsed$asset_routes) {
        self$serve_static(
          at = asset$at,
          path = asset$path,
          use_index = asset$use_index,
          fallthrough = asset$fallthrough,
          html_charset = asset$html_charset,
          headers = asset$headers,
          validation = asset$validation
        )
        for (ex in asset$except) {
          self$exclude_static(paste0(asset$at, ex))
        }
      }
      for (handler in parsed$message_handlers) {
        self$message_handler(handler)
      }
      self$add_api_doc(parsed$api)
      parsed$modifier(self)
    },
    #' @description Add a (partial) OpenAPI spec to the api docs
    #' @param doc A list with the OpenAPI documentation
    #' @param overwrite Logical. Should already existing documentation be
    #' removed or should it be merged together with `doc`
    #' @param subset A character vector giving the path to the subset of the
    #' docs to assign `doc` to
    add_api_doc = function(doc, overwrite = FALSE, subset = NULL) {
      check_character(subset, allow_null = TRUE)
      if (!(is_list(doc) && is_named2(doc))) {
        stop_input_type(doc, "a named list")
      }

      doc <- subset_to_list(subset, doc)
      if (overwrite) {
        if (is.null(overwrite)) {
          private$OPENAPI <- list()
        } else if (list_has_subset(private$OPENAPI, subset)) {
          private$OPENAPI[[subset]] <- list()
        }
      }
      private$OPENAPI <- utils::modifyList(private$OPENAPI, doc)
    }
  ),
  active = list(
    #' @field request_router The router handling requests
    request_router = function() {
      if (is.null(private$REQUEST_ROUTER)) {
        private$REQUEST_ROUTER <- RouteStack$new(
          ignore_trailing_slash = private$IGNORE_TRAILING_SLASH
        )
        private$REQUEST_ROUTER$attach_to <- "request"
        self$attach(private$REQUEST_ROUTER)
      }
      private$REQUEST_ROUTER
    },
    #' @field header_router The router handling partial requests (the request
    #' will pass through this router prior to reading in the body)
    header_router = function() {
      if (is.null(private$HEADER_ROUTER)) {
        private$HEADER_ROUTER <- RouteStack$new(
          ignore_trailing_slash = private$IGNORE_TRAILING_SLASH
        )
        private$HEADER_ROUTER$attach_to <- "header"
        self$attach(private$HEADER_ROUTER)
      }
      private$HEADER_ROUTER
    },
    #' @field doc_type The type of API documentation to generate. Can be either
    #' `"rapidoc"` (the default), `"redoc"`, `"swagger"`, or `NULL` (equating to
    #' not generating API docs)
    doc_type = function(value) {
      if (missing(value)) return(private$DOC_TYPE)
      if (!is.null(value)) {
        value <- arg_match0(value, c("rapidoc", "redoc", "swagger"))
      }
      private$DOC_TYPE <- value
    },
    #' @field doc_path The URL path to serve the api documentation from
    doc_path = function(value) {
      if (missing(value)) return(private$DOC_PATH)
      check_string(value)
      private$DOC_PATH <- value
    }
  ),
  private = list(
    OPENAPI = list(openapi = "3.0.0", info = list(title = "", description = "")),
    REQUEST_ROUTER = NULL,
    HEADER_ROUTER = NULL,
    MESSAGE_ROUTER = NULL,
    DOC_TYPE = NULL,
    DOC_PATH = NULL,
    REJECT_MISSING_METHODS = NULL,
    IGNORE_TRAILING_SLASH = NULL
  )
)

create_plumber_request_handler <- function(
  handler,
  serializers = NULL,
  parsers = NULL,
  use_strict_serializer = FALSE,
  download = FALSE,
  call = caller_env()
) {
  check_function(handler)
  if (!"..." %in% fn_fmls_names(handler)) {
    fn_fmls(handler) <- c(fn_fmls(handler), "..." = missing_arg())
  }
  if (
    !is.null(serializers) && !(is_list(serializers) && is_named(serializers))
  ) {
    stop_input_type(serializers, "a named list", allow_null = TRUE, call = call)
  }
  if (length(serializers) == 0) serializers <- NULL
  if (!is.null(parsers) && !(is_list(parsers) && is_named(parsers))) {
    stop_input_type(parsers, "a named list", allow_null = TRUE, call = call)
  }
  if (length(parsers) == 0) parsers <- NULL
  check_bool(use_strict_serializer, call = call)
  if (is_bool(download)) {
    dl_file <- NULL
  } else if (is_string(download)) {
    dl_file <- download
    download <- TRUE
  } else {
    stop_input_type(download, "a boolean or a string", call = call)
  }

  function(request, response, keys, server, id, ...) {
    # Default the response to 200 if it is 404 (the default) as we hit an endpoint
    if (response$status == 404L) response$status <- 200L

    # Parse body unless parser is set to 'none'
    if (!is.null(parsers)) {
      success <- request$parse(!!!parsers)
      if (!success) return(Break)
    }
    # Add serializers for the finalizing route
    success <- response$set_formatter(
      !!!serializers,
      default = if (use_strict_serializer) NULL else names(serializers)[1]
    )
    if (!success) {
      # Shortcircuit evaluation if we cannot serve the requested content type
      return(Break)
    }

    # Mark body as download if requested
    if (download) {
      response$as_download(dl_file)
    }

    is_clean <- FALSE

    # Set up formatter - currently only used for devices
    info <- init_formatter(response$formatter)

    on.exit(
      {
        if (!is_clean) clean_formatter(response$formatter, info)
      },
      add = TRUE
    )

    # Call the handler with all available data
    result <- inject(handler(
      !!!keys,
      request = request,
      response = response,
      server = server,
      client_id = id,
      query = request$query,
      body = request$body
    ))

    # If the handler returns a ggplot and a device serializer is in effect we render it
    if (!is.null(info) && inherits(result, "ggplot")) {
      plot(result)
    }

    # Cache break signal as it may get overwritten below
    signals_break <- should_break(result)

    # Overwrite result with closing value if any
    result <- close_formatter(response$formatter, info) %||% result
    is_clean <- TRUE

    # Check if return value is of a type that should be added as body
    if (
      !is_plumber_control(result) &&
        !is.null(result) &&
        !inherits(result, "Response")
    ) {
      response$body <- result
    }

    # If an explicit break is returned forward that signal
    if (signals_break) {
      Break
    } else {
      Next
    }
  }
}

create_plumber_message_handler <- function(handler) {
  check_function(handler)
  if (!"..." %in% fn_fmls_names(handler)) {
    fn_fmls(handler) <- c(fn_fmls(handler), "..." = missing_arg())
  }
  function(server, id, binary, message, request, ...) {
    response <- handler(
      message = message,
      server = server,
      client_id = id,
      request = request
    )
    if (is.raw(response) || is_string(response)) {
      server$send(response, id)
    }
  }
}

subset_to_list <- function(x, end_value = list()) {
  if (length(x) == 0) {
    end_value
  } else {
    list2(
      !!x[1] := subset_to_list(x[-1])
    )
  }
}
list_has_subset <- function(x, subset) {
  if (length(subset) == 0) return(TRUE)
  x <- x[[subset[1]]]
  if (is.null(x)) return(FALSE)
  list_has_subset(x, subset[-1])
}
