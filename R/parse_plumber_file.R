#' Parse a plumber file
#'
#' This function takes care of parsing an annotated plumber file and creating
#' one or more routes, API specs, and a modifier function to be called on the
#' plumber app after the routes have been added. This function does not attach
#' the parsed data to a plumber api, and it is rarely necessary to call it
#' directly.
#'
#' @param path The path to the file to parse
#' @param env The environment to evaluate the code and annotations in
#'
#' @return A list containing:
#'
#' * `route` The main route handling requests according to the parsed file, as a
#'   named list of length one
#' * `header_route` The route to be attached to header events (fires before the
#'   body has been received and can be used to prematurely reject requests based
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
#' @importFrom roxygen2 parse_file block_has_tags block_get_tag_value
#' @keywords internal
#'
#' @examplesIf file.exists("path/to/my/plumber/file.R")
#' # Parse a plumber file
#' parse_plumber_file("path/to/my/plumber/file.R")
#'
parse_plumber_file <- function(
  path,
  env = caller_env()
) {
  check_string(path)
  check_environment(env)
  if (!fs::file_exists(path)) {
    cli::cli_abort("{.arg path} must point to an existing file", call = env)
  }

  source(path, local = env, verbose = FALSE)
  wd <- fs::path_dir(path)

  file <- readLines(path)
  if (
    !(trimws(file[[1]]) == "#' @roxygenPrefix" ||
      isTRUE(get_opts("roxygenPrefix")))
  ) {
    file <- sub("^#([^\\*])", "##\\1", file)
  }
  file <- sub("^#\\*", "#'", file)
  is_string <- grepl("^\".*\"$", file)
  file[is_string] <- paste0("{", file[is_string], "}")
  tmp_file <- tempfile()
  on.exit(unlink(tmp_file), add = TRUE)
  writeLines(file, tmp_file)
  # We use parse_file instead of parse_text so we can add srcref
  blocks <- parse_file(tmp_file, srcref_path = path)

  route_name <- if (block_has_tags(blocks[[1]], "routeName")) {
    block_get_tag_value(blocks[[1]], "routeName")
  } else {
    fs::path_file(fs::path_ext_remove(path))
  }
  root <- if (block_has_tags(blocks[[1]], "root")) {
    block_get_tag_value(blocks[[1]], "root")
  } else {
    NULL
  }

  blocks <- lapply(
    blocks,
    parse_block,
    env = env,
    file_dir = wd
  )

  then_blocks <- vapply(blocks, inherits, logical(1), "plumber2_then_block")
  index <- rle(then_blocks)
  prior <- cumsum(index$lengths)[which(index$values) - 1]
  then_calls <- split(
    blocks[then_blocks],
    rep(prior, index$lengths[index$values])
  )
  for (i in seq_along(prior)) {
    pr <- prior[i]
    if (pr == 0 || is.null(blocks[[pr]]$async) || isFALSE(blocks[[pr]]$async)) {
      cli::cli_abort(
        "A {.field @then} block must follow an {.field @async} block or another {.field @then} block"
      )
    }
    blocks[[pr]]$then <- then_calls[[i]]
  }

  list(
    blocks = blocks[!then_blocks],
    route = route_name,
    root = root
  )
}

#' @importFrom roxygen2 block_has_tags
parse_block <- function(
  block,
  env = caller_env(),
  file_dir = "."
) {
  call <- eval_bare(block$call, env = env)
  tags <- vapply(block$tags, `[[`, character(1), "tag")
  values <- lapply(block$tags, `[[`, "raw")
  block <- if (block_has_tags(block, "assets")) {
    parse_asset_block(call, tags, values, env, file_dir)
  } else if (block_has_tags(block, "statics")) {
    parse_static_block(call, tags, values, env, file_dir)
  } else if (block_has_tags(block, "authenticator")) {
    parse_authenticator_block(call, tags, values, env)
  } else if (block_has_tags(block, "message")) {
    parse_message_block(call, tags, values, env)
  } else if (block_has_tags(block, "then")) {
    parse_then_block(call, tags, values, env)
  } else if (block_has_tags(block, "redirect")) {
    parse_redirect_block(call, tags, values, env)
  } else if (block_has_tags(block, "shiny")) {
    parse_shiny_block(call, tags, values, env)
  } else if (block_has_tags(block, "forward")) {
    parse_forward_block(call, tags, values, env)
  } else if (block_has_tags(block, "report")) {
    parse_report_block(call, tags, values, env, file_dir)
  } else if (block_has_tags(block, "plumber")) {
    parse_plumber_block(call, tags, values, env)
  } else if (
    block_has_tags(
      block,
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
  ) {
    parse_handler_block(call, tags, values, env)
  } else if (identical(call, "_API")) {
    parse_api_block(call, tags, values, env)
  } else {
    structure(list(), class = "plumber2_empty_block")
  }
  for (tag in tags) {
    if (is_extension_tag(tag)) {
      block <- parse_extension(tag, block, call, tags, values, env)
    }
  }
  block
}

# ---- Helpers for specific block types ----------------------------------------

parse_api_block <- function(call, tags, values, env) {
  structure(
    list(doc = parse_global_api(tags, values, env)),
    class = "plumber2_api_block"
  )
}

parse_plumber_block <- function(call, tags, values, env) {
  check_function(call)
  if (length(fn_fmls(call)) != 1) {
    cli::cli_abort("plumber modifiers must be unary functions")
  }
  structure(list(call = call), class = "plumber2_call_block")
}

parse_handler_block <- function(call, tags, values, env) {
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
    download <- trimws(values[[download[1]]]) %||% TRUE
  } else {
    download <- FALSE
  }

  if ("async" %in% tags) {
    async <- trimws(values[[which(tags == "async")[1]]])
    if (async == "") async <- TRUE
  } else {
    async <- FALSE
  }

  strict_serializer <- any(tags == "serializerStrict")

  doc <- parse_block_api(tags, values, names(parsers), names(serializers))

  endpoints <- lapply(methods, function(i) {
    method <- tags[i]
    if (method == "any") {
      method <- "all"
    }

    list(
      method = method,
      path = trimws(values[[i]])
    )
  })

  structure(
    list(
      endpoints = endpoints,
      handler = call,
      serializers = serializers,
      parsers = parsers,
      use_strict_serializer = strict_serializer,
      download = download,
      doc = doc,
      async = async,
      header = any(tags == "header"),
      auth = parse_auth_tags(tags, values)
    ),
    class = "plumber2_handler_block"
  )
}

parse_static_block <- function(call, tags, values, env, file_dir) {
  if (sum(tags == "statics") != 1) {
    cli::cli_abort("Only one {.field @statics} tag allowed per block")
  }
  if (!(is.null(call) || identical(call, list()))) {
    cli::cli_warn("Expression ignored for {.field @statics} blocks")
  }
  mapping <- trimws(strsplit(
    values[[which(tags == "statics")]],
    " ",
    fixed = TRUE
  )[[1]])
  if (length(mapping) == 1) {
    mapping <- c(mapping, "/")
  }
  mapping[1] <- fs::path_abs(mapping[1], file_dir)
  except <- which(tags == "except")
  structure(
    list(
      asset = routr::asset_route(
        mapping[2],
        mapping[1],
        except = unlist(values[except])
      ),
      endpoints = list(list(method = "get", path = mapping[2]))
    ),
    class = "plumber2_static_block"
  )
}

parse_asset_block <- function(call, tags, values, env, file_dir) {
  if (sum(tags == "assets") != 1) {
    cli::cli_abort("Only one {.field @assets} tag allowed per block")
  }
  if (!(is.null(call) || identical(call, list()))) {
    cli::cli_warn("Expression ignored for {.field @assets} blocks")
  }
  mapping <- trimws(strsplit(
    values[[which(tags == "assets")]],
    " ",
    fixed = TRUE
  )[[1]])
  if (length(mapping) == 1) {
    mapping <- c(mapping, "/")
  }
  mapping[1] <- fs::path_abs(mapping[1], file_dir)
  path <- mapping[2]
  if (grepl("/$", path)) {
    path <- paste0(path, "*")
  }
  structure(
    list(
      route = routr::resource_route(!!mapping[2] := mapping[1]),
      header = FALSE,
      endpoints = list(list(method = "get", path = path)),
      auth = parse_auth_tags(tags, values)
    ),
    class = "plumber2_route_block"
  )
}

parse_authenticator_block <- function(call, tags, values, env) {
  if (!is.function(call) || fireproof::is_auth(call)) {
    stop_input_type(call, "an {.cls Auth} subclass object or a function")
  }
  name <- trimws(values[[which(tags == "authenticator")[1]]])
  structure(
    list(
      auth = call,
      name = name
    ),
    class = "plumber2_authenticator_block"
  )
}

parse_message_block <- function(call, tags, values, env) {
  check_function(call)
  if (!"..." %in% fn_fmls_names(call)) {
    fn_fmls(call) <- c(fn_fmls(call), "..." = missing_arg())
  }
  if ("async" %in% tags) {
    async <- trimws(values[[which(tags == "async")[1]]])
    if (async == "") async <- TRUE
  } else {
    async <- FALSE
  }
  structure(
    list(
      handler = call,
      async = async
    ),
    class = "plumber2_message_block"
  )
}

parse_then_block <- function(call, tags, values, env) {
  check_function(call)
  if (!"..." %in% fn_fmls_names(call)) {
    fn_fmls(call) <- c(fn_fmls(call), "..." = missing_arg())
  }
  structure(
    call,
    class = "plumber2_then_block"
  )
}

parse_redirect_block <- function(call, tags, values, env) {
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
  structure(
    list(redirects = res),
    class = "plumber2_redirect_block"
  )
}

parse_shiny_block <- function(call, tags, values, env) {
  if (sum(tags == "shiny") != 1) {
    cli::cli_abort("Only one {.field @shiny} tag allowed per block")
  }
  check_installed("shiny")
  if (!shiny::is.shiny.appobj(call)) {
    stop_input_type(call, "a shiny app object")
  }
  except <- which(tags == "except")
  structure(
    list(
      shiny_app = call,
      path = values[[which(tags == "shiny")]],
      except = unlist(values[except]),
      auth = parse_auth_tags(tags, values)
    ),
    class = "plumber2_proxy_block"
  )
}

parse_forward_block <- function(call, tags, values, env) {
  res <- lapply(values[tags == "forward"], function(x) {
    x <- stringi::stri_split_fixed(x, " ", n = 2)[[1]]
    if (length(x) != 2) {
      cli::cli_warn(c(
        "Malformed {.field @forward} tag",
        i = "The format must conform to: <from path> <to url>"
      ))
      return(NULL)
    }
    list(
      path = x[1],
      url = x[2]
    )
  })
  res <- res[lengths(res) != 0]

  except <- which(tags == "except")
  structure(
    list(
      path = vapply(res, `[[`, character(1), "path"),
      url = vapply(res, `[[`, character(1), "url"),
      except = unlist(values[except]),
      auth = parse_auth_tags(tags, values)
    ),
    class = "plumber2_proxy_block"
  )
}

parse_report_block <- function(call, tags, values, env, file_dir) {
  if (sum(tags == "report") != 1) {
    cli::cli_abort("Only one {.field @report} tag allowed per block")
  }
  x <- trimws(values[[which(tags == "report")]])
  call <- fs::path_abs(call, file_dir)

  info <- routr::report_info(call)

  paths <- c(
    x,
    paste0(sub("/?$", "/", x), info$format),
    unique(paste0(sub("/?$", ".", x), info$ext))
  )

  endpoints <- unlist(
    lapply(paths, function(x) {
      list(list(method = "get", path = x), list(method = "post", path = x))
    }),
    recursive = FALSE
  )

  doc <- list(
    paths = parse_block_api(tags, values, character(0), character(0))
  )

  structure(
    list(
      path = x,
      report = call,
      doc = doc,
      endpoints = endpoints,
      auth = parse_auth_tags(tags, values)
    ),
    class = "plumber2_report_block"
  )
}

# ---- Methods for applying block info -----------------------------------------

#' Generic for applying information from a plumber2 block to an api
#'
#' In order to facilitate extensibility of the plumber2 file syntax you can
#' provide your own methods for how to apply information from a plumber2 block
#' to an api.
#'
#' @param block The block that was parsed
#' @param api The [Plumber2] api object to apply it to
#' @param route_name The name of the route the plumber2 file is associated with.
#' Either the name of the file or the value of the `@routeName` tag
#' @param root The root given by the potential `@root` tag in the file. If no
#' `@root` tag is present this value will be null. The value represents the root
#' path for every endpoint defined in the file and should be prepended to any
#' URL path you use.
#' @param ... ignored
#'
#' @return `api`, modified
#'
#' @seealso [add_plumber2_tag()]
#'
#' @export
#' @keywords internal
#'
#' @examples
#' # Add a method for a fictional "hello_block" that makes the api say hello when
#' # it starts
#' apply_plumber2_block.hello_block <- function(block, api, route_name, root, ...) {
#'   api$on("start", function(...) {
#'     message("Hello")
#'   })
#'   api
#' }
#'
apply_plumber2_block <- function(block, api, route_name, root, ...) {
  UseMethod("apply_plumber2_block")
}

#' @export
apply_plumber2_block.plumber2_proxy_block <- function(
  block,
  api,
  route_name,
  root,
  ...
) {
  if (!is.null(block$shiny_app)) {
    api$add_shiny(
      paste0(root, block$path),
      block$shiny_app,
      except = block$except,
      auth_flow = !!block$auth$flow,
      auth_scope = block$auth$scope
    )
  } else if (!is.null(block$url)) {
    for (i in seq_along(block$path)) {
      api$forward(
        paste0(root, block$path[i]),
        block$url[i],
        except = block$except,
        auth_flow = !!block$auth$flow,
        auth_scope = block$auth$scope
      )
    }
  }
  api
}
#' @export
apply_plumber2_block.plumber2_redirect_block <- function(
  block,
  api,
  route_name,
  root,
  ...
) {
  for (redirect in block$redirects) {
    api$redirect(
      redirect$method,
      paste0(root, redirect$from),
      redirect$to,
      redirect$permanent
    )
  }
  api
}
#' @export
apply_plumber2_block.plumber2_message_block <- function(
  block,
  api,
  route_name,
  root,
  ...
) {
  api$message_handler(block$handler, block$async, block$then)
  api
}
#' @export
apply_plumber2_block.plumber2_call_block <- function(
  block,
  api,
  route_name,
  root,
  ...
) {
  maybe_new <- block$call(api)
  if (is_plumber_api(maybe_new)) maybe_new else api
}
#' @export
apply_plumber2_block.plumber2_route_block <- function(
  block,
  api,
  route_name,
  root,
  ...
) {
  api$add_route(route_name, block$route, block$header, root = root)
  if (!is.null(block$doc)) {
    api$add_api_doc(block$doc)
  }
  if (!is.null(block$auth)) {
    for (i in seq_along(block$endpoints)) {
      for (path in block$endpoints[[i]]$path) {
        api$add_authentication(
          method = block$endpoints[[i]]$method,
          path = paste0(root, path),
          auth_flow = !!block$auth$flow,
          auth_scope = block$auth$scope,
          add_doc = FALSE
        )
      }
    }
  }
  api
}
#' @export
apply_plumber2_block.plumber2_static_block <- function(
  block,
  api,
  route_name,
  root,
  ...
) {
  api$serve_static(
    at = paste0(root, block$asset$at),
    path = block$asset$path,
    use_index = block$asset$use_index,
    fallthrough = block$asset$fallthrough,
    html_charset = block$asset$html_charset,
    headers = block$asset$headers,
    validation = block$asset$validation
  )
  for (ex in block$asset$except) {
    api$exclude_static(paste0(root, block$asset$at, ex))
  }
  api
}
#' @export
apply_plumber2_block.plumber2_handler_block <- function(
  block,
  api,
  route_name,
  root,
  ...
) {
  if (!is.null(root)) {
    if (block$header) {
      router <- api$header_router
    } else {
      router <- api$request_router
    }
    if (!router$has_route(route_name)) {
      api$add_route(route_name, header = block$header, root = root)
    } else {
      route <- router$get_route(route_name)
      if (route$root == "/") {
        route$root <- root
      } else {
        cli::cli_warn(
          "Ignoring {.field @root {route_name}} as the route already has a root set",
          .frequency = "once",
          .frequency_id = paste0(route_name, "-", root)
        )
      }
    }
  }
  for (endpoint in block$endpoints) {
    oapi_path <- as_openapi_path(endpoint$path)
    endpoint_doc <- block$doc[[oapi_path]][[endpoint$method]]
    api$request_handler(
      method = endpoint$method,
      path = endpoint$path,
      handler = block$handler,
      serializers = block$serializers,
      parsers = block$parsers,
      use_strict_serialize = block$use_strict_serializer,
      auth_flow = !!block$auth$flow,
      auth_scope = block$auth$scope,
      download = block$download,
      async = block$async,
      then = block$then,
      doc = endpoint_doc,
      route = route_name,
      header = block$header
    )
  }
  api
}
#' @export
apply_plumber2_block.plumber2_report_block <- function(
  block,
  api,
  route_name,
  root,
  ...
) {
  api$add_report(
    block$path,
    block$report,
    doc = block$doc,
    auth_flow = !!block$auth$flow,
    auth_scope = block$auth$scope
  )
  api
}
#' @export
apply_plumber2_block.plumber2_authenticator_block <- function(
  block,
  api,
  route_name,
  root,
  ...
) {
  api$add_authenticator(
    block$auth,
    block$name
  )
  api
}
#' @export
apply_plumber2_block.plumber2_api_block <- function(
  block,
  api,
  route_name,
  root,
  ...
) {
  api$add_api_doc(block$doc)
  api
}
#' @export
apply_plumber2_block.plumber2_empty_block <- function(
  block,
  api,
  route_name,
  root,
  ...
) {
  api
}

#' @export
apply_plumber2_block.plumber2_cors_block <- function(
  block,
  api,
  route_name,
  root,
  ...
) {
  NextMethod()
  for (i in seq_along(block$endpoints)) {
    for (path in block$endpoints[[i]]$path) {
      api <- api_security_cors(
        api,
        paste0(root, path),
        block$cors,
        methods = block$endpoints[[i]]$method
      )
    }
  }
  api
}

#' @export
apply_plumber2_block.plumber2_rip_block <- function(
  block,
  api,
  route_name,
  root,
  ...
) {
  NextMethod()
  for (i in seq_along(block$endpoints)) {
    if (block$endpoints[[i]]$method == "get") {
      for (path in block$endpoints[[i]]$path) {
        api <- api_security_resource_isolation(
          api,
          paste0(root, path),
          block$rip
        )
      }
    }
  }
  api
}


# Helpers ----------------------------------------------------------------

parse_auth_tags <- function(tags, values) {
  if ("auth" %in% tags) {
    flow <- trimws(values[[which(tags == "auth")[1]]])
    flow <- parse_quo(flow, empty_env())
    if ("authScope" %||% tags) {
      scope <- unlist(values[tags == "authScope"])
      scope <- unlist(trimws(unlist(strsplit(scope, ","))))
    } else {
      scope <- NULL
    }
    list(
      flow = flow,
      scope = scope
    )
  } else {
    NULL
  }
}
