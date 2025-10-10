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
#' @importFrom firestorm ReverseProxy
#' @importFrom rapidoc rapidoc_spec
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
      default_async = get_opts("async", "mirai"),
      env = caller_env()
    ) {
      super$initialize(host, port)
      self$query_delim <- ","

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

      private$SESSION_FRAMEWORK <- "plumber2"
      private$SESSION_FRAMEWORK_VERSION <- utils::packageVersion("plumber2")
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
        openapi <- private$OPENAPI
        openapi$servers <- c(
          openapi$servers,
          list(list(url = ""))
        )
        write_json(openapi, openapi_file, auto_unbox = TRUE)
        api_route <- inject(openapi_route(
          openapi_file,
          root = private$DOC_PATH,
          ui = private$DOC_TYPE,
          !!!private$DOC_ARGS
        ))
        logo <- system.file("help", "figures", "logo.svg", package = "plumber2")
        if (logo == "") logo <- system.file("man", "figures", "logo.svg", package = "plumber2")
        api_route$add_handler(
          "get",
          paste0(sub("/?$", "/", private$DOC_PATH), "logo.svg"),
          function(request, response, keys, ...) {
            response$file <- logo
            response$status <- 200L
            FALSE
          }
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
    #' @param root The root path to serve this route from.
    add_route = function(
      name,
      route = NULL,
      header = FALSE,
      after = NULL,
      root = ""
    ) {
      route <- route %||%
        Route$new(ignore_trailing_slash = private$IGNORE_TRAILING_SLASH)
      route$root <- paste0(root, route$root)
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
    #' @param then A function to call at the completion of an async handler
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
      serializers = NULL,
      parsers = NULL,
      use_strict_serializer = FALSE,
      download = FALSE,
      async = FALSE,
      then = NULL,
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
      doc$operationId <- doc$operationId %||%
        paste0(path_info$path, "-", method)

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
          get_async(async),
          then
        ),
        reject_missing_methods = !header && private$REJECT_MISSING_METHODS
      )
      if (!(header || is.null(doc) || inherits(doc, "plumber_noDoc"))) {
        doc$parameters <- doc$parameters %||% list()
        true_path <- gsub("//", "/", paste0(route$root, path_info$path))
        self$add_api_doc(doc, subset = c("paths", true_path, method))
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
    #' @param then A function to call at the completion of an async handler
    message_handler = function(handler, async = FALSE, then = NULL) {
      if (isTRUE(async)) {
        async <- private$ASYNC_EVALUATER
      } else if (isFALSE(async)) {
        async <- NULL
      }
      handler <- create_message_handler(handler, get_async(async), then)
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
      self$header_router$add_redirect(
        method,
        as_routr_path(from),
        as_routr_path(to),
        permanent
      )

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
        api <- apply_plumber2_block(block, api, parsed$route, parsed$root)
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
        if (is.null(subset)) {
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
    #' @param except Subpaths to `path` that should not be forwarded to the
    #' shiny app. Be sure it doesn't contains paths that the shiny app needs
    #'
    add_shiny = function(path, app, except = NULL) {
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
          function(app, port, .__fiery_id_fun__) {
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
          args = list(
            app = app,
            port = port,
            .__fiery_id_fun__ = private$client_id
          )
        )
        self$set_data(proc_name, proc)
      })
      self$on("end", function(...) {
        proc <- self$get_data(proc_name)
        if (inherits(proc, "r_process")) proc$kill()
      })

      invisible(self)
    },
    #' @description Render and serve a Quarto or Rmarkdown document from an
    #' endpoint. See [api_report()] for more information.
    #'
    #' @param path The base path to serve the report from. Additional endpoints
    #' will be created in addition to this.
    #' @param report The path to the report to serve
    #' @param ... Further arguments to `quarto::quarto_render()` or
    #' `rmarkdown::render()`
    #' @param doc An [openapi_operation()] documentation for the report. Only
    #' `query` parameters will be used and a request body will be generated from
    #' this for the POST methods.
    #' @param max_age The maximum age in seconds to keep a rendered report
    #' before initiating a re-render
    #' @param async Should rendering happen asynchronously (using mirai)
    #' @param finalize An optional function to run before sending the response
    #' back. The function will receive the request as the first argument, the
    #' response as the second, and the server as the third.
    #' @param continue A logical that defines whether the response is returned
    #' directly after rendering or should be made available to subsequent routes
    #' @param cache_dir The location of the render cache. By default a temporary
    #' folder is created for it.
    #' @param cache_by_id Should caching be scoped by the user id. If the
    #' rendering is dependent on user-level access to different data this is
    #' necessary to avoid data leakage.
    #' @param route The route this handler should be added to. Defaults to the
    #' last route in the stack. If the route does not exist it will be created
    #' as the last route in the stack.
    #'
    add_report = function(
      path,
      report,
      ...,
      doc = NULL,
      max_age = Inf,
      async = TRUE,
      finalize = NULL,
      continue = FALSE,
      cache_dir = tempfile(pattern = "plumber2_report"),
      cache_by_id = FALSE,
      route = NULL
    ) {
      if (!is_bool(async) && !identical(async, "mirai")) {
        cli::cli_warn("report serving can only be done with the mirai backend")
        async <- TRUE
      }

      info <- routr::report_info(report)
      paths <- c(
        path,
        paste0(sub("/?$", "/", path), info$format),
        unique(paste0(sub("/?$", ".", path), info$ext))
      )
      formats <- c(
        list(info$formats[!duplicated(info$mime_types)]),
        as.list(info$formats),
        as.list(info$formats[!duplicated(info$mime_types)])
      )
      response <- c(
        list(unique(info$mime_types)),
        as.list(info$mime_types),
        as.list(unique(info$mime_types))
      )
      if (!is.null(doc$parameters)) {
        doc$parameters <- set_names(
          doc$parameters,
          vapply(doc$parameters, `[[`, character(1), "name")
        )
      }
      get_doc <- openapi_operation(
        summary = doc$summary,
        description = doc$description,
        operation_id = doc$operationId %||% character(),
        parameters = lapply(
          names(info$query_params),
          function(param) {
            if (length(doc$parameters[[param]]$schema) == 0) {
              schema <- openapi_schema(info$query_params[[param]])
            } else {
              schema <- doc$parameters[[param]]$schema
            }
            openapi_parameter(
              name = param,
              location = "query",
              required = FALSE,
              schema = schema
            )
          }
        ),
        responses = doc$response %||% list(),
        tags = doc$tags %||% character()
      )
      post_doc <- get_doc
      delete_doc <- get_doc
      post_doc$requestBody <- openapi_request_body(
        content = openapi_content(
          "application/json" = openapi_schema(
            I("object"),
            properties = set_names(
              lapply(get_doc$parameters, `[[`, "schema"),
              vapply(get_doc$parameters, `[[`, character(1), "name")
            )
          )
        )
      )
      post_doc$parameters <- NULL
      delete_doc$parameters <- NULL

      type_caster <- create_type_casters(doc)

      rr <- routr::report_route(
        path,
        file = report,
        ...,
        max_age = max_age,
        async = async,
        finalize = finalize,
        ignore_trailing_slash = private$IGNORE_TRAILING_SLASH,
        continue = continue,
        cache_dir = cache_dir,
        cache_by_id = cache_by_id,
        param_caster = type_caster
      )

      if (is.null(route)) {
        if (length(self$request_router$routes) == 0) {
          route <- "default"
        } else {
          route <- self$request_router$routes[length(
            self$request_router$routes
          )]
        }
      }
      if (!self$request_router$has_route(route)) {
        cli::cli_inform(
          "Creating {.field {route}} route in request router"
        )
        self$add_route(route)
      }
      route <- self$request_router$get_route(route)
      route$merge_route(rr)
      if (!inherits(doc, "plumber_noDoc")) {
        paths <- gsub("/+", "/", paste0(route$root, "/", paths))
        for (i in seq_along(paths)) {
          format <- paste0(
            "*",
            formats[[i]],
            "* (",
            response[[i]],
            ")"
          )
          if (length(format) > 1) {
            pg_summary <- cli::format_inline(
              "Render and serve \"{info$title}\""
            )
            pg_description <- cli::format_inline(
              "This path allows you to access *{info$title}* in one of the
              following formats: {format} through content negotiation.",
              keep_whitespace = FALSE
            )
            d_summary <- cli::format_inline(
              "Clear render cache of \"{info$title}\""
            )
            d_description <- cli::format_inline(
              "This will delete *all* cached versions of the rendered report."
            )
          } else {
            pg_summary <- cli::format_inline(
              "Render and serve \"{info$title}\" as {formats[[i]]}"
            )
            pg_description <- cli::format_inline(
              "This path allows you to access *{info$title}* in the {format} format."
            )
            d_summary <- cli::format_inline(
              "Clear render cache of the {formats[[i]]} version of \"{info$title}\""
            )
            d_description <- cli::format_inline(
              "This will delete cached versions of the {format} format."
            )
          }
          get_doc$responses <- post_doc$responses <- list(
            "200" = openapi_response(
              description = "The rendered report",
              content = openapi_content(
                !!!set_names(
                  rep_along(
                    response[[i]],
                    list(set_names(list(), character()))
                  ),
                  response[[i]]
                )
              )
            ),
            "400" = openapi_response(
              description = "Supplied parameters were not acceptable"
            ),
            "500" = openapi_response(
              description = "Issues during rendering"
            )
          )
          delete_doc$responses <- list(
            "204" = openapi_response(
              description = "The render cache was cleared"
            )
          )
          if (i == 1) {
            get_doc$summary <- post_doc$summary <- get_doc$summary %||%
              pg_summary
            get_doc$description <- post_doc$description <- get_doc$description %||%
              pg_description
          } else {
            get_doc$summary <- post_doc$summary <- pg_summary
            get_doc$description <- post_doc$description <- pg_description
          }
          delete_doc$summary <- d_summary
          delete_doc$description <- d_description
          path_doc <- openapi_path(
            get = get_doc,
            post = post_doc,
            delete = delete_doc
          )
          self$add_api_doc(path_doc, subset = c("paths", paths[i]))
        }
      }

      invisible(self)
    },
    #' @description Add a reverse proxy from a path to a given URL. See
    #' [api_forward()] for more details
    #' @param path The root to forward from
    #' @param url The url to forward to
    #' @param except Subpaths to `path` that should be exempt from forwarding
    #'
    forward = function(path, url, except = NULL) {
      revprox <- firestorm::ReverseProxy$new(url, path, except)
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
    },
    #' @field doc_args Further arguments to the documentation UI
    doc_args = function(value) {
      if (missing(value)) {
        return(private$DOC_ARGS)
      }
      if (!is_bare_list(value)) {
        stop_input_type(value, "a bare list")
      }
      private$DOC_ARGS <- modifyList(private$DOC_ARGS, value)
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
    DOC_ARGS = list(
      slots = "<img slot=\"logo\" src=\"./logo.svg\" width=36px style=\"margin-left:7px\"/>",
      heading_text = paste0("plumber2 ", packageVersion("plumber2")),
      primary_color = "#FF81D2",
      text_color = "#B4FFE4",
      bg_color = "#212121"
    ),
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
