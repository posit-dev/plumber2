#' Parse a plumber file
#'
#' This function takes care of parsing an annotated plumber file and creating
#' one or more routes, API specs, and a modifier function to be called on the
#' plumber app after the routes have been added. This function does not attach
#' the parsed data to a plumber api, and it is rarely necessary to call it
#' directly.
#'
#' @param path The path to the file to parse
#' @param ignore_trailing_slash Logical. Should the trailing slash of a path
#' be ignored when adding handlers and handling requests. Setting this will
#' not change the request or the path associated with but just ensure that
#' both `path/to/resource` and `path/to/resource/` ends up in the same
#' handler.
#' @param default_async The async evaluator to use by default when the `@async`
#' tag is used without further argument
#' @param env The environment to evaluate the code and annotations in
#'
#' @return A list containing:
#'
#' * `route` The main route handling requests according to the parsed file, as a
#'   named list of length one
#' * `header_route` The route to be attached to header events (fires before the
#'   body has been recieved and can be used to prematurely reject requests based
#'   on their headers), as a named list of length one
#' * `asset_routes` All the asset routes created by `@static` blocks as a named
#'   list
#' * `message_handlers` All the websocket message handlers created by `@message`
#'   blocks, as a list
#' * `api` A list giving the OpenAPI spec as parsed from the file
#' * `modifier` A single function chaining all the functions from `@plumber`
#'   blocks together
#'
#' @export
#' @keywords internal
#'
parse_plumber_file <- function(
  path,
  ignore_trailing_slash,
  default_async,
  env = caller_env()
) {
  check_string(path)
  check_environment(env)
  if (!fs::file_exists(path)) {
    cli::cli_abort("{.arg path} must point to an existing file", call = env)
  }

  source(path, local = env, verbose = FALSE)

  file <- readLines(path)
  file <- sub("^#([^\\*])", "##\\1", file)
  file <- sub("^#\\*", "#'", file)
  tmp_file <- tempfile()
  on.exit(unlink(tmp_file), add = TRUE)
  writeLines(file, tmp_file)
  blocks <- roxygen2::parse_file(tmp_file, srcref_path = path)
  route <- routr::Route$new(ignore_trailing_slash = ignore_trailing_slash)
  header_route <- routr::Route$new(
    ignore_trailing_slash = ignore_trailing_slash
  )
  blocks <- lapply(
    blocks,
    parse_block,
    route = route,
    header_route = header_route,
    default_async = default_async,
    env = env
  )

  # TODO: Use routeName tag if present
  route_name <- fs::path_file(fs::path_ext_remove(path))

  route <- set_names(list(route), route_name)
  header_route <- set_names(list(header_route), route_name)
  # Find the asset routes from the file
  asset_routes <- blocks[vapply(
    blocks,
    function(x) inherits(x, "AssetRoute"),
    logical(1)
  )]
  if (length(asset_routes) != 0) {
    asset_routes <- set_names(
      asset_routes,
      vapply(asset_routes, function(s) s$at, character(1))
    )
  }
  apis <- blocks[vapply(blocks, is_bare_list, logical(1))]
  globals <- vapply(apis, function(x) !is.null(x$openapi), logical(1))
  paths <- Reduce(utils::modifyList, apis[!globals])
  globals <- Reduce(utils::modifyList, apis[globals])

  modifiers <- blocks[vapply(blocks, inherits, logical(1), "plumber_call")]
  modifier <- function(api) {
    for (mod in modifiers) {
      api <- mod(api)
      if (!is_plumber_api(api)) {
        cli::cli_abort("All modifiers must return the plumber api")
      }
    }
  }

  redirects <- blocks[vapply(blocks, inherits, logical(1), "plumber_redirect")]
  redirects <- unlist(redirects, recursive = FALSE)

  list(
    route = route,
    header_route = header_route,
    asset_routes = asset_routes,
    message_handlers = blocks[vapply(
      blocks,
      inherits,
      logical(1),
      "message_call"
    )],
    redirects = redirects,
    api = c(globals, paths),
    modifiers = modifier
  )
}

parse_block <- function(
  block,
  route,
  header_route,
  default_async,
  env = caller_env()
) {
  call <- eval_bare(block$call, env = env)
  tags <- vapply(block$tags, `[[`, character(1), "tag")
  values <- lapply(block$tags, `[[`, "raw")
  if (any(tags == "assets")) {
    parse_asset_block(call, block, tags, values, route, env)
  } else if (any(tags == "statics")) {
    parse_static_block(call, block, tags, values, env)
  } else if (any(tags == "message")) {
    parse_message_block(call, block, tags, values, default_async)
  } else if (any(tags == "redirect")) {
    parse_redirect_block(call, block, tags, values)
  } else if (any(tags == "plumber")) {
    parse_plumber_block(call, tags)
  } else if (
    any(
      tags %in%
        c(
          "get",
          "head",
          "post",
          "put",
          "delete",
          "connect",
          "options",
          "trace",
          "patch"
        )
    )
  ) {
    parse_handler_block(
      call,
      block,
      tags,
      values,
      route,
      header_route,
      default_async,
      env
    )
  } else {
    parse_global_api(tags, values, env)
  }
}

# ---- Helpers for specific block types ----------------------------------------

parse_plumber_block <- function(call, tags) {
  check_function(call)
  if (length(fn_fmls(call)) != 1) {
    cli::cli_abort("plumber modifiers must be unary functions")
  }
  structure(call, class = "plumber_call")
}

parse_handler_block <- function(
  handler,
  block,
  tags,
  values,
  route,
  header_route,
  default_async,
  env
) {
  methods <- which(
    tags %in%
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
        "any"
      )
  )

  serializers <- which(tags == "serializer")
  if (length(serializers) != 0) {
    serializers <- trimws(unlist(values[serializers]))
  } else {
    serializers <- NULL
  }
  serializers <- get_serializers_internal(serializers, env)

  parsers <- which(tags == "parser")
  if (length(parsers) != 0) {
    parsers <- trimws(values[parsers])
  } else {
    parsers <- NULL
  }
  parsers <- get_parsers_internal(parsers, env)

  download <- which(tags == "download")
  if (length(download) != 0) {
    download <- values[[download[1]]] %||% TRUE
  } else {
    download <- FALSE
  }

  if ("async" %in% tags) {
    async <- trimws(values[[which(tags == "async")[1]]])
    if (async == "") async <- default_async
  } else {
    async <- NULL
  }

  strict_serializer <- any(tags == "serializerStrict")
  if (any(tags == "header")) route <- header_route

  doc <- parse_block_api(tags, values, names(parsers), names(serializers))

  for (i in methods) {
    method <- tags[i]
    if (method == "any") method <- "all"
    path <- as_routr_path(trimws(values[[i]]))
    oapi_path <- as_openapi_path(trimws(values[[i]]))

    route$add_handler(
      method,
      path,
      create_request_handler(
        handler,
        serializers = serializers,
        parsers = parsers,
        use_strict_serializer = strict_serializer,
        download = download,
        doc = doc$paths[[oapi_path]][[method]],
        async = get_async(async)
      )
    )
  }

  doc
}

parse_static_block <- function(call, block, tags, values, env) {
  if (sum(tags == "statics") != 1) {
    cli::cli_abort("Only one {.field @statics} tag allowed per block")
  }
  if (!(is.null(call) || identical(call, list()))) {
    cli::cli_warn("Expression ignored for {.field @statics} blocks")
  }
  extra_tags <- setdiff(tags, c("statics", "except", "backref"))
  if (length(extra_tags) != 0) {
    cli::cli_warn(
      "Ignoring {.field {paste0('@', extra_tags)}} tag{?s} when parsing {.field @statics} tag"
    )
  }
  mapping <- trimws(strsplit(
    values[[which(tags == "statics")]],
    " ",
    fixed = TRUE
  )[[1]])
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
    cli::cli_warn(
      "Ignoring {.field {paste0('@', extra_tags)}} tag{?s} when parsing {.field @assets} tag"
    )
  }
  mapping <- trimws(strsplit(
    values[[which(tags == "assets")]],
    " ",
    fixed = TRUE
  )[[1]])
  if (length(mapping) == 1) {
    mapping <- c(mapping, "/")
  }
  route$merge_route(
    routr::ressource_route(!!mapping[2] := mapping[1])
  )
  NULL
}

parse_message_block <- function(call, block, tags, values, default_async) {
  check_function(call)
  if (!"..." %in% fn_fmls_names(call)) {
    fn_fmls(call) <- c(fn_fmls(call), "..." = missing_arg())
  }
  if ("async" %in% tags) {
    async <- trimws(values[[which(tags == "async")[1]]])
    if (async == "") async <- default_async
  } else {
    async <- NULL
  }
  structure(
    create_message_handler(call, async = get_async(async)),
    class = "message_call"
  )
}

parse_redirect_block <- function(call, block, tags, values) {
  res <- lapply(values[tags == "redirect"], function(x) {
    x <- stringi::stri_split_fixed(x, " ", n = 3)[[1]]
    if (length(x) != 3) {
      cli::cli_warn(c(
        "Malformed {.field @redirect} tag",
        i = "The format must conform to: <method> <from path> <to path>"
      ))
      return(NULL)
    }
    is_permanent <- grepl("^\\!", x[1])
    x[1] <- sub("!", "", x[1], fixed = TRUE)
    list(
      method = x[1],
      from = x[2],
      to = x[3],
      permanent = is_permanent
    )
  })
  class(res) <- "plumber2_redirect"
}
