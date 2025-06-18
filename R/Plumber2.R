#' The Plumber2 Class
#'
#' @description
#' This class encapsulates all of the logic of a plumber2 api, and is what gets
#' passed around in the functional api of plumber2. The `Plumber2` class is a
#' subclass of the [fiery::Fire] class. Please consult the documentation for
#' this for additional information on what this type of server is capable of.
#' Note that the `Plumber2` objects are reference objects, meaning that any
#' change to it will change all instances of the object.
#'
#' ## Initialization
#' A new `Plumber2`-object is initialized using the `new()` method on the
#' generator:
#'
#' \tabular{l}{
#'  `api <- Plumber2$new()`
#' }
#'
#' However, most users will use the functional api of the package and thus
#' construct one using [api()]
#'
#' ## Copying
#' As `Plumber2` objects are using reference semantics new copies of an api cannot
#' be made simply be assigning it to a new variable. If a true copy of a `Plumber2`
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
Plumber2 <- R6Class(
  "Plumber2",
  inherit = Fire,
  public = list(
    #' @description Create a new `Plumber2` api
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
    #' @param ignore_trailing_slash Logical. Should the trailing slash of a path
    #' be ignored when adding handlers and handling requests. Setting this will
    #' not change the request or the path associated with but just ensure that
    #' both `path/to/resource` and `path/to/resource/` ends up in the same
    #' handler. This setting will only affect routes that are created automatically.
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
    #' @param default_async The default evaluator to use for async request
    #' handling
    #' @param env An environment that will be used as the default execution
    #' environment for the API
    #' @return A `Plumber2` object
    initialize = function(
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
      super$initialize(host, port)

      if (!is.null(doc_type)) {
        private$DOC_TYPE <- arg_match0(
          doc_type,
          c("rapidoc", "redoc", "swagger", "")
        )
      }
      check_string(doc_path)
      private$DOC_PATH <- doc_path
      check_bool(reject_missing_methods)
      private$REJECT_MISSING_METHODS <- reject_missing_methods
      check_bool(ignore_trailing_slash)
      private$IGNORE_TRAILING_SLASH <- ignore_trailing_slash
      check_number_decimal(max_request_size, allow_null = TRUE)
      check_string(shared_secret, allow_null = TRUE)
      self$compression_limit <- compression_limit

      header_router <- create_header_router(
        max_request_size,
        shared_secret
      )
      if (length(header_router$routes) != 0) {
        private$HEADER_ROUTER <- header_router
      }

      private$ASYNC_EVALUATER <- get_async(default_async)

      check_environment(env)
      private$PARENT_ENV <- env
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
      if (
        length(private$OPENAPI) != 0 &&
          !is.null(private$DOC_TYPE) &&
          private$DOC_TYPE != ""
      ) {
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

      if (!silent) {
        cli::cli_text(
          "plumber2 server started at http://{self$host}:{self$port}"
        )
      }
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
      route <- route %||%
        Route$new(ignore_trailing_slash = private$IGNORE_TRAILING_SLASH)
      if (header) {
        router <- self$header_router
      } else {
        router <- self$request_router
      }
      if (!router$has_route(name)) {
        router$add_route(route, name, after)
      } else if (!route$empty) {
        router$get_route(name)$merge_route(route)
      }

      invisible(self)
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
    #' @param async If `FALSE` create a regular handler. If `TRUE`, use the
    #' default async evaluator to create an async handler. If a string, the
    #' async evaluator registered to that name is used. If a function is
    #' provided then this is used as the async evaluator
    #' @param doc OpenAPI documentation for the handler. Will be added to the
    #' `paths$<handler_path>$<handler_method>` portion of the API.
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
      async = FALSE,
      doc = NULL,
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
      if (method == "any") {
        method <- "all"
      }
      check_bool(header)
      check_string(path)
      check_string(route, allow_null = TRUE)

      # Augment docs with path info
      path_info <- parse_path(path)
      doc <- doc %||% list(parameters = list())
      doc_path_param <- vapply(
        doc$parameters,
        function(p) p$`in` == "path",
        logical(1)
      )
      doc$parameters <- c(
        combine_parameters(
          path_info$params,
          doc$parameters[doc_path_param],
          from_block = FALSE
        ),
        doc$parameters[!doc_path_param]
      )
      operation_id <- paste0(path_info$path, "-", method)
      doc$parameters <- lapply(doc$parameters, function(par) {
        par$operationId <- par$operationId %||% operation_id
        par
      })

      # Substitute the plumber style path arg for a routr style
      path <- as_routr_path(path)

      if (header) {
        router <- self$header_router
      } else {
        router <- self$request_router
      }

      if (is.null(route)) {
        if (length(router$routes) == 0) {
          route <- "default"
        } else {
          route <- router$routes[length(router$routes)]
        }
      }
      if (!router$has_route(route)) {
        cli::cli_inform(
          "Creating {.field {route}} route in {if (header) 'header' else 'request'} router"
        )
        self$add_route(route, header = header)
      }
      route <- router$get_route(route)

      if (isTRUE(async)) {
        async <- private$ASYNC_EVALUATER
      } else if (isFALSE(async)) {
        async <- NULL
      }

      route$add_handler(
        method,
        path,
        create_request_handler(
          handler,
          serializers,
          parsers,
          use_strict_serializer,
          download,
          doc,
          get_async(async)
        ),
        reject_missing_methods = !header && private$REJECT_MISSING_METHODS
      )
      if (!header) {
        doc$parameters <- doc$parameters %||% list()
        self$add_api_doc(doc, subset = c("paths", path_info$path, method))
      }

      invisible(self)
    },
    #' @description Add a handler to a WebSocket message. See [api_message] for
    #' detailed information
    #' @param handler A function conforming to the specifications laid out in
    #' [api_message()]
    #' @param async If `FALSE` create a regular handler. If `TRUE`, use the
    #' default async evaluator to create an async handler. If a string, the
    #' async evaluator registered to that name is used. If a function is
    #' provided then this is used as the async evaluator
    message_handler = function(handler, async = FALSE) {
      if (isTRUE(async)) {
        async <- private$ASYNC_EVALUATER
      } else if (isFALSE(async)) {
        async <- NULL
      }
      handler <- create_message_handler(handler, get_async(async))
      self$on("message", handler)

      invisible(self)
    },
    #' @description Add a redirect to the header router. Depending on the value
    #' of `permanent` it will respond with a 307 Temporary Redirect or 308
    #' Permanent Redirect. `from` and `to` can contain path parameters and
    #' wildcards which will be matched between the two to construct the correct
    #' redirect path.
    #' @param method The HTTP method the redirect should respond to
    #' @param from The path the redirect should respond to
    #' @param to The path/URL to redirect the incoming request towards. After
    #' resolving any path parameters and wildcards it will be used in the
    #' `Location` header
    #' @param permanent Logical. Is the redirect considered permanent or
    #' temporary? Determines the type of redirct status code to use
    redirect = function(method, from, to, permanent = TRUE) {
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
      if (method == "any") {
        method <- "all"
      }
      check_string(from)
      check_string(to)
      check_bool(permanent)
      self$header_router$add_redirect(method, from, to, permanent)

      invisible(self)
    },
    #' @description Parses a plumber file and updates the app according to it
    #' @param file The path to a file to parse
    #' @param env The parent environment to the environment the file should be
    #' evaluated in. If `NULL` the environment provided at construction will be
    #' used
    parse_file = function(file, env = NULL) {
      eval_env <- new.env(parent = env %||% private$PARENT_ENV)
      parsed <- parse_plumber_file(file, env = eval_env)

      api <- self

      for (block in parsed$blocks) {
        api <- apply_plumber2_block(block, api, parsed$route)
      }

      invisible(api)
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

      invisible(self)
    },
    #' @description Add a shiny app to an api. See [api_shiny()] for detailed
    #' information
    #' @param path The path to serve the app from
    #' @param app A shiny app object
    #'
    add_shiny = function(path, app) {
      check_installed("callr")
      check_installed("shiny")
      if (!shiny::is.shiny.appobj(app)) {
        stop_input_type(app, "a shiny app object")
      }
      port <- self$get_data("shiny_port") %||% 53000L
      self$set_data("shiny_port", port + 1L)
      proc_name <- paste0("shiny_reverse_proxy_", port)

      shiny_proxy <- firestorm::ReverseProxy$new(
        paste0("http://127.0.0.1:", port),
        path
      )
      self$attach(shiny_proxy)

      self$on("start", function(...) {
        proc <- callr::r_bg(
          function(app, port) {
            shiny::runApp(
              app,
              port = port,
              launch.browser = FALSE,
              host = "127.0.0.1",
              workerId = "",
              quiet = TRUE,
              display.mode = "normal",
              test.mode = FALSE
            )
          },
          args = list(app = app, port = port)
        )
        self$set_data(proc_name, proc)
      })
      self$on("end", function(...) {
        proc <- self$get_data(proc_name)
        if (inherits(proc, "r_process")) proc$kill()
      })

      invisible(self)
    },
    #' @description Add a reverse proxy from a path to a given URL. See
    #' [api_forward()] for more details
    #' @param path The root to forward from
    #' @param url The url to forward to
    #' @param continue Should the response be passed through the standard route
    #' before being send to the client
    #'
    forward = function(path, url, continue = FALSE) {
      revprox <- firestorm::ReverseProxy$new(url, path)
      self$attach(revprox)

      invisible(self)
    }
  ),
  active = list(
    #' @field request_router The router handling requests
    request_router = function() {
      if (is.null(private$REQUEST_ROUTER)) {
        private$REQUEST_ROUTER <- RouteStack$new()
        private$REQUEST_ROUTER$attach_to <- "request"
        self$attach(private$REQUEST_ROUTER)
      }
      private$REQUEST_ROUTER
    },
    #' @field header_router The router handling partial requests (the request
    #' will pass through this router prior to reading in the body)
    header_router = function() {
      if (is.null(private$HEADER_ROUTER)) {
        private$HEADER_ROUTER <- RouteStack$new()
        private$HEADER_ROUTER$attach_to <- "header"
        self$attach(private$HEADER_ROUTER)
      }
      private$HEADER_ROUTER
    },
    #' @field doc_type The type of API documentation to generate. Can be either
    #' `"rapidoc"` (the default), `"redoc"`, `"swagger"`, or `NULL` (equating to
    #' not generating API docs)
    doc_type = function(value) {
      if (missing(value)) {
        return(private$DOC_TYPE)
      }
      if (!is.null(value)) {
        value <- arg_match0(value, c("rapidoc", "redoc", "swagger"))
      }
      private$DOC_TYPE <- value
    },
    #' @field doc_path The URL path to serve the api documentation from
    doc_path = function(value) {
      if (missing(value)) {
        return(private$DOC_PATH)
      }
      check_string(value)
      private$DOC_PATH <- value
    }
  ),
  private = list(
    OPENAPI = list(
      openapi = "3.0.0",
      info = list(title = "", description = "")
    ),
    REQUEST_ROUTER = NULL,
    HEADER_ROUTER = NULL,
    MESSAGE_ROUTER = NULL,
    DOC_TYPE = "rapidoc",
    DOC_PATH = "__docs__",
    REJECT_MISSING_METHODS = FALSE,
    IGNORE_TRAILING_SLASH = TRUE,
    ASYNC_EVALUATER = NULL,
    PARENT_ENV = NULL
  )
)

subset_to_list <- function(x, end_value = list()) {
  if (length(x) == 0) {
    end_value
  } else {
    list2(
      !!x[1] := subset_to_list(x[-1], end_value)
    )
  }
}
list_has_subset <- function(x, subset) {
  if (length(subset) == 0) {
    return(TRUE)
  }
  x <- x[[subset[1]]]
  if (is.null(x)) {
    return(FALSE)
  }
  list_has_subset(x, subset[-1])
}
