Plumber <- R6::R6Class(
  "Plumber",
  inherit = fiery::Fire,
  public = list(
    initialize = function(host = get_opts("host", "127.0.0.1"), port = get_opts("port", 8080)) {
      super$initialize(host, port)
      header_route <- create_header_route()
      if (length(header_route$routes) != 0) {
        private$HEADER_ROUTER <- header_route
      }
    },
    format = function(...) {
      cli::cli_fmt({
        cli::cli_rule("A plumber server")
        cli::cli_text("Serving on http://{self$host}:{self$port}")
        cli::cli_text("Currently {if (self$is_running()) cli::col_green('running') else cli::col_red('not running')}")
      })
    },
    ignite = function(block = FALSE, showcase = is_interactive(), ..., silent = FALSE) {
      if (length(private$OPENAPI) != 0) {
        openapi_file <- tempfile(fileext = ".json")
        jsonlite::write_json(private$OPENAPI, openapi_file, auto_unbox = TRUE)
        docs_path <- get_opts("apiPath", "__docs__")
        doc_type <- get_opts("docs", "redoc")
        api_route <- routr::openapi_route(openapi_file, root = docs_path, ui = doc_type)
        self$request_router$add_route(api_route, "openapi")

        self$on("end", function(...) {
          on.exit(self$off("__plumber_cleanup__"))
          self$request_router$remove_route("openapi")
          unlink(openapi_file)
        }, id = "__plumber_cleanup__")

        if (isTRUE(showcase)) {
          showcase <- sub("/?$", "/", docs_path)
        }
      }

      if (!silent) cli::cli_text("Plumber server started at http://{self$host}:{self$port}")
      super$ignite(block = block, showcase = showcase, ..., silent = TRUE)
    },
    add_route = function(name, header = FALSE, after = NULL) {
      if (header) {
        router <- self$header_router
      } else {
        router <- self$request_router
      }
      router$add_route(routr::Route$new(), name, after)
    },
    request_handler = function(method, path, handler, serializers, parsers = NULL,
                               use_strict_serializer = FALSE, download = FALSE,
                               route = NULL, header = FALSE) {
      method <- arg_match0(method, c("get", "head", "post", "put", "delete", "connect", "options", "trace", "patch", "any", "all"))
      if (method == "any") method <- "all"
      check_bool(header)
      check_string(path)
      check_string(route, allow_null = TRUE)

      # Substitute the plumber style path arg for a routr style
      path <- stringi::stri_replace_all_regex(path, "<(.+?)(:.+?)?>", ":$1")

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
        cli::cli_inform("Creating {.field {route}} route in {if (header) 'header' else 'request'} router")
        self$add_route(route, header)
      }
      route <- router$get_route(route)

      route$add_handler(method, path, create_plumber_request_handler(handler, serializers, parsers, use_strict_serializer, download))
    },
    message_handler = function(handler) {
      handler <- create_plumber_message_handler(handler)
      self$on("message", handler)
    },
    parse_file = function(file, env = caller_env()) {
      parsed <- parse_plumber_file(file, env)
      if (!parsed$route[[1]]$empty) {
        if (!self$request_router$has_route(names(parsed$route))) {
          self$add_route(names(parsed$route))
        }
        self$request_router$get_route(names(parsed$route))$merge_route(parsed$route[[1]])
      }
      if (!parsed$header_route[[1]]$empty) {
        if (!self$header_router$has_route(names(parsed$header_route))) {
          self$add_route(names(parsed$header_route))
        }
        self$header_router$get_route(names(parsed$header_route))$merge_route(parsed$header_route[[1]])
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
      self$add_api_spec(parsed$api)
      parsed$modifier(self)
    },
    add_api_spec = function(spec, overwrite = FALSE, subset = NULL) {
      check_character(subset, allow_null = TRUE)
      if (!(is_list(spec) && is_named2(spec))) {
        stop_input_type(spec, "a named list")
      }

      spec <- subset_to_list(subset, spec)
      if (overwrite) {
        private$OPENAPI <- list()
      }
      private$OPENAPI <- modifyList(private$OPENAPI, spec)
    }
  ),
  active = list(
    request_router = function() {
      if (is.null(private$REQUEST_ROUTER)) {
        private$REQUEST_ROUTER <- routr::RouteStack$new()
        private$REQUEST_ROUTER$attach_to <- "request"
        self$attach(private$REQUEST_ROUTER)
      }
      private$REQUEST_ROUTER
    },
    header_router = function() {
      if (is.null(private$HEADER_ROUTER)) {
        private$HEADER_ROUTER <- routr::RouteStack$new()
        private$HEADER_ROUTER$attach_to <- "header"
        self$attach(private$HEADER_ROUTER)
      }
      private$HEADER_ROUTER
    }
  ),
  private = list(
    OPENAPI = list(),
    REQUEST_ROUTER = NULL,
    HEADER_ROUTER = NULL,
    MESSAGE_ROUTER = NULL
  )
)

create_plumber_request_handler <- function(handler, serializers = NULL, parsers = NULL, use_strict_serializer = FALSE, download = FALSE, call = caller_env()) {
  check_function(handler)
  if (!"..." %in% fn_fmls_names(handler)) {
    fn_fmls(handler) <- c(fn_fmls(handler), "..." = missing_arg())
  }
  if (!is.null(serializers) && !(is_list(serializers) && is_named(serializers))) {
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
    success <- response$set_formatter(!!!serializers, default = if (strict_serializer) NULL else names(serializers)[1])
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

    on.exit({
      if (!is_clean) clean_formatter(response$formatter, info)
    }, add = TRUE)

    # Call the handler with all available data
    result <- inject(handler(!!!keys, request = request, response = response, server = server, client_id = id, query = request$query, body = request$body))

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
    if (!is_plumber_control(result) && !is.null(result) && !inherits(result, "Response")) {
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
    response <- handler(message = message, server = server, client_id = id, request = request)
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
