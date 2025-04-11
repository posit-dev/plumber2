create_request_handler <- function(
  handler,
  serializers = NULL,
  parsers = NULL,
  use_strict_serializer = FALSE,
  download = FALSE,
  doc = NULL,
  async = NULL,
  call = caller_env()
) {
  # Input checks
  check_function(handler)
  ## Add ... to formals so we never error on unknown arguments
  if (!"..." %in% fn_fmls_names(handler)) {
    fn_fmls(handler) <- c(fn_fmls(handler), "..." = missing_arg())
  }
  if (
    !is.null(serializers) && !(is_list(serializers) && is_named2(serializers))
  ) {
    stop_input_type(serializers, "a named list", allow_null = TRUE, call = call)
  }
  if (length(serializers) == 0) serializers <- NULL
  if (!is.null(parsers) && !(is_list(parsers) && is_named2(parsers))) {
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
  check_function(async, allow_null = TRUE)

  # Create type casters based on doc
  type_casters <- create_type_casters(doc)
  body_parser <- function(request) {
    # Parse body unless parser is set to 'none'
    if (!is.null(parsers)) {
      request$parse(!!!parsers)
    }
    type_casters$body(request$body, request$headers$Content_Type)
  }

  if (is.null(async)) {
    create_sequential_request_handler(
      handler,
      serializers,
      use_strict_serializer,
      type_casters,
      body_parser,
      download,
      dl_file
    )
  } else {
    create_async_request_handler(
      handler,
      async,
      serializers,
      use_strict_serializer,
      type_casters,
      body_parser,
      download,
      dl_file
    )
  }
}

create_sequential_request_handler <- function(
  handler,
  serializers,
  use_strict_serializer,
  type_casters,
  body_parser,
  download,
  dl_file
) {
  function(request, response, keys, server, id, arg_list = list(), ...) {
    # Default the response to 200 if it is 404 (the default) as we hit an endpoint
    if (response$status == 404L) response$status <- 200L

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

    # Call the handler with all available data. If formatter is a device
    # serialiser with_formatter() will set up the correct promise domain. If not
    # it is a no-op
    result <- with_formatter(
      inject(handler(
        !!!type_casters$path(keys),
        request = request,
        response = response,
        server = server,
        client_id = id,
        query = type_casters$query(request$query),
        body = body_parser(request),
        !!!arg_list,
        ...
      )),
      response$formatter,
      info
    )

    if (promises::is.promising(result)) {
      is_clean <- TRUE # Don't close device
      promises::then(result, function(result) {
        is_clean <- FALSE
        on.exit(
          {
            if (!is_clean) clean_formatter(response$formatter, info)
          },
          add = TRUE
        )
        finish_request(result, response, info)
      })
    } else {
      finish_request(result, response, info)
    }
  }
}

finish_request <- function(result, response, info, call = caller_env()) {
  # If the handler returns a ggplot and a device serializer is in effect we render it
  if (!is.null(info) && inherits(result, "ggplot")) {
    plot(result)
  }

  # Cache break signal as it may get overwritten below
  signals_break <- should_break(result)

  # Overwrite result with closing value if any
  result <- close_formatter(response$formatter, info) %||% result
  call$is_clean <- TRUE

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

create_async_request_handler <- function(
  handler,
  async,
  serializers,
  use_strict_serializer,
  type_casters,
  body_parser,
  download,
  dl_file
) {
  envir <- list2env(list(
    handler = handler,
    query = NULL,
    body = NULL
  ))
  if (any(c("request", "response", "server") %in% fn_fmls_names(handler))) {
    cli::cli_abort(c(
      "async handlers cannot access {.arg request}, {.arg response}, or {.arg server}",
      i = "remove all of these arguments from the handler definition"
    ))
  }
  has_query <- "query" %in% fn_fmls_names(handler)
  has_body <- "body" %in% fn_fmls_names(handler)

  function(request, response, keys, server, id, arg_list = list(), ...) {
    # Default the response to 200 if it is 404 (the default) as we hit an endpoint
    if (response$status == 404L) response$status <- 200L

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

    # Collect all variables - minimise the amount of data send to async
    envir$formatter <- response$formatter
    envir$keys <- type_casters$path(keys)
    envir$id <- id
    if (has_query) envir$query <- type_casters$query(request$query)
    if (has_body) envir$body <- body_parser(request)
    envir$dots <- list2(!!!arg_list, ...)

    result <- async(async_request_call, envir = envir)

    promises::then(
      result,
      function(result) {
        response$body <- result$result
        result$continue
      }
    )
  }
}

async_request_call <- quote({
  is_clean <- FALSE

  # Set up formatter - currently only used for devices
  info <- plumber2::init_formatter(formatter)

  on.exit(
    {
      if (!is_clean) plumber2::clean_formatter(formatter, info)
    },
    add = TRUE
  )

  # Call the handler with all available data. We don't need a promise domain for
  # device serialisers since it happens sequentially in the other process
  result <- rlang::inject(handler(
    !!!keys,
    client_id = id,
    query = query,
    body = body,
    !!!dots
  ))

  # Complete rehash of the above then() block

  # If the handler returns a ggplot and a device serializer is in effect we render it
  if (!is.null(info) && inherits(result, "ggplot")) {
    graphics::plot(result)
  }

  # Cache break signal as it may get overwritten below
  signals_break <- plumber2::should_break(result)

  # Overwrite result with closing value if any (equivalent to %||%)
  result2 <- plumber2::close_formatter(formatter, info)
  is_clean <- TRUE
  if (!is.null(result2)) result <- result2

  # Return the result and continue signal so it can be used in the then()
  list(
    result = result,
    continue = if (signals_break) plumber2::Break else plumber2::Next
  )
})
