parse_file <- function(path, env = globalenv()) {
  file <- readLines(path)
  file <- sub("^#([^\\*])", "##\\1", file)
  file <- sub("^#\\*", "#'", file)
  tmp_file <- tempfile()
  writeLines(file, tmp_file)
  blocks <- roxygen2::parse_file(tmp_file, srcref_path = path)
  route <- routr::route()
  blocks <- lapply(blocks, parse_block, route = route, env = env)
  route <- set_names(list(route), fs::path_file(fs::path_ext_remove(path)))
  # Find the asset routes from the files
  statics <- blocks[vapply(blocks, function(x) inherits(x, "AssetRoute"), logical(1))]
  if (length(statics) != 0) {
    route <- c(
      set_names(statics, vapply(statics, function(s) s$at, character(1))),
      route
    )
  }
  apis <- blocks[vapply(blocks, is_bare_list, logical(1))]
  globals <- vapply(apis, function(x) !is.null(x$openapi), logical(1))
  paths <- Reduce(modifyList, apis[!globals])
  globals <- Reduce(modifyList, apis[globals])

  list(
    routes = route,
    api = c(globals, paths),
    mod = blocks[vapply(blocks, is_function, logical(1))]
  )
}

parse_block <- function(block, route, env = globalenv()) {
  call <- eval_bare(block$call, env = env)
  tags <- vapply(block$tags, `[[`, character(1), "tag")
  values <- lapply(block$tags, `[[`, "raw")
  if (any(tags == "assets")) {
    parse_asset_block(call, block, tags, values, route, env)
  } else if (any(tags == "statics")) {
    parse_static_block(call, block, tags, values, route, env)
  } else if (any(tags == "plumber")) {
    parse_plumber_block(call, tags)
  } else if (any(tags %in% c("get", "head", "post", "put", "delete", "connect", "options", "trace", "patch"))) {
    parse_handler_block(call, block, tags, values, route, env)
  } else {
    parse_global_api(tags, values)
  }
}

parse_plumber_block <- function(call, tags) {
  check_function(call)
  if (length(fn_fmls(call)) != 1) {
    cli::cli_abort("plumber modifiers must be unary functions")
  }
  call
}

parse_handler_block <- function(handler, block, tags, values, route, env) {
  check_function(handler)
  if (!"..." %in% fn_fmls_names(handler)) {
    cli::cli_abort(c(
      "All handlers must accept {.arg ...}",
      i = "please fix the handler defined in {.file {block$file}}, line {block$line}"
    ))
  }
  methods <- which(tags %in% c("get", "head", "post", "put", "delete", "connect", "options", "trace", "patch"))

  serializers <- which(tags == "serializer")
  if (length(serializers) != 0) {
    serializers <- trimws(unlist(values[serializers]))
  } else {
    serializers <- NULL
  }
  serializers <- get_serializers(serializers, env)

  parsers <- which(tags == "parser")
  if (length(parsers) != 0) {
    parsers <- trimws(values[parsers])
  } else {
    parsers <- NULL
  }
  parsers <- get_parsers(parsers, env)

  download <- which(tags == "download")
  dl_file <- NULL
  if (length(download) == 1) {
    dl_file <- values[[download]]
  }
  download <- length(download) == 1

  strict_serializer <- any(tags == "serializerStrict")

  for (i in methods) {
    path <- trimws(values[[i]])
    # Get all plumber style path args
    path_args <- stringi::stri_extract_all_regex(path, "<.+?>")[[1]]
    # Extract the name of the arg
    arg_names <- stringi::stri_match_first_regex(path_args, "<(.+?)(:|>)")[,2]
    # Extract the optional type of the arg
    arg_types <- stringi::stri_match_first_regex(path_args, ":(.+)>")[,2]
    # Substitute the plumber style path arg for a routr style
    path <- stringi::stri_replace_all_regex(path, "<(.+?)(:.+?)?>", ":$1")

    route$add_handler(tags[i], path, function(request, response, keys, server, id, ...) {
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
    })
  }
  parse_block_api(tags, values, names(parsers), names(serializers))
}

parse_static_block <- function(call, block, tags, values, route, env) {
  if (sum(tags == "statics") != 1) {
    cli::cli_abort("Only one {.field @statics} tag allowed per block")
  }
  if (!(is.null(call) || identical(call, list()))) {
    cli::cli_warn("Expression ignored for {.field @statics} blocks")
  }
  extra_tags <- setdiff(tags, c("statics", "except", "backref"))
  if (length(extra_tags) != 0) {
    cli::cli_warn("Ignoring {.field {paste0('@', extra_tags)}} tag{?s} when parsing {.field @statics} tag")
  }
  mapping <- trimws(strsplit(values[[which(tags == "statics")]], " ", fixed = TRUE)[[1]])
  if (length(mapping) == 1) {
    mapping <- c(mapping, "/")
  }
  except <- which(tags == "except")
  routr::asset_route(mapping[2], mapping[1], except = unlist(values[except]))
}

parse_asset_block <- function(call, block, tags, values, route, env) {
  if (sum(tags == "assets") != 1) {
    cli::cli_abort("Only one {.field @assets} tag allowed per block")
  }
  if (!(is.null(call) || identical(call, list()))) {
    cli::cli_warn("Expression ignored for {.field @assets} blocks")
  }
  extra_tags <- setdiff(tags, c("assets", "backref"))
  if (length(extra_tags) != 0) {
    cli::cli_warn("Ignoring {.field {paste0('@', extra_tags)}} tag{?s} when parsing {.field @assets} tag")
  }
  mapping <- trimws(strsplit(values[[which(tags == "assets")]], " ", fixed = TRUE)[[1]])
  if (length(mapping) == 1) {
    mapping <- c(mapping, "/")
  }
  route$merge_route(
    routr::ressource_route(!!mapping[2] := mapping[1])
  )
  NULL
}


# Roxygen extension ============================================================


