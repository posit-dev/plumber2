plumber <- function(location = NULL, host = get_opts("host", "127.0.0.1"), port = get_opts("port", 8080), env = globalenv()) {
  app <- fiery::Fire$new(host = host, port = port)
  router <- routr::RouteStack$new()
  settings <- new.env(parent = emptyenv())
  settings$api <- list()
  modifiers <- list()

  if (!is.null(location)) {
    loc_dirs <- fs::is_dir(location)
    if (any(loc_dirs)) {
      # FIXME: Doesn't preserve the order of input
      dirs <- location[loc_dirs]
      location <- location[!loc_dirs]
      location <- c(location, fs::dir_ls(dirs, regexp = "\\.(r|R)$"))
    }
    if (any(!fs::file_exists(location))) {
      cli::cli_abort(c(
        "The following provided files does not exist",
        set_names(location[!fs::file_exists(location)], "*")
      ))
    }
    for (file in location) {
      parsed_route <- parse_file(file, env = env)
      lapply(names(parsed_route$routes), function(name) {
        router$add_route(parsed_route$routes[[name]], name)
      })
      settings$api <- modifyList(settings$api, parsed_route$api)
      modifiers <- c(modifiers, parsed_route$mod)
    }
  }

  # FIXME: Any asset_routes added to router after this point will not have effect
  app$attach(router, "plumber_request_route")

  header_route <- create_header_route()
  if (!is.null(header_route)) {
    app$attach(header_route, "plumber_header_route")
  }

  app <- structure(
    list(app = app, settings = settings),
    class = "plumber_server"
  )

  for (mod in modifiers) {
    app <- mod(app)
    if (!inherits(app, "plumber_server")) {
      cli::cli_abort("All modifiers must return the plumber server")
    }
  }

  app
}

#' @export
print.plumber_server <- function(x, ...) {
  cli::cli_rule("A plumber server")
  cli::cli_text("Serving on http://{x$app$host}:{x$app$port}")
  cli::cli_text("Currently {if (x$app$is_running()) cli::col_green('running') else cli::col_red('not running')}")
}

#' @export
server_start <- function(server, block = TRUE, showcase = is_interactive(), ..., silent = FALSE) {
  openapi_file <- tempfile(fileext = ".json")
  jsonlite::write_json(server$api, openapi_file, auto_unbox = TRUE)
  api_route <- routr::openapi_route(openapi_file, ui = "swagger")
  router <- server$router
  router$add_route(api_route, "openapi")

  server$app$on("end", function(server, ...) {
    on.exit(server$off("__plumber_cleanup__"))
    router$remove_route("openapi")
    unlink(openapi_file)
  }, id = "__plumber_cleanup__")

  if (!silent) cli::cli_text("Plumber server started at http://{server$app$host}:{server$app$port}")
  server$app$ignite(block = block, showcase = FALSE, ..., silent = TRUE)
  invisible(server)
}
#' @export
server_stop <- function(server) {
  server$app$extinguish()
  invisible(server)
}
