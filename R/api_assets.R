#' Serve ressources from your file system
#'
#' plumber2 provides two ways to serve files from your server. One
#' (`api_assets`) goes through R and gives you all the power you expect to
#' further modify and work with the response. The other (api_statics) never hits
#' the R process and as a result is blazing fast. However this comes with the
#' price of very limited freedom to modify the response or even do basic
#' authentication. Each has their place.
#'
#' @param api A plumber2 api object to add the rossource serving to
#' @param at The path to serve the ressources from
#' @param path The location on the file system to map `at` to
#' @param route The name of the route in the header router to add the asset
#' route to. Defaults to the last route in the stack. If the route does not
#' exist it will be created as the last route in the stack
#' @inheritParams routr::ressource_route
#' @inheritParams routr::asset_route
#'
#' @return These functions return the `api` object allowing for easy chaining
#' with the pipe
#'
#' @export
#'
api_assets <- function(
  api,
  at,
  path,
  default_file = "index.html",
  default_ext = "html",
  finalize = NULL,
  continue = FALSE,
  route = NULL
) {
  asset_route <- routr::ressource_route(
    !!at := path,
    default_file = default_file,
    default_ext = default_ext,
    finalize = finalize,
    continue = continue
  )
  if (is.null(route)) {
    if (length(api$header_router$routes) == 0) {
      route <- "default"
    } else {
      route <- api$header_router$routes[length(api$header_router$routes)]
    }
  }
  if (!api$header_router$has_route(route)) {
    cli::cli_inform(
      "Creating {.field {route}} route in header router"
    )
    api$add_route(route, header = TRUE)
  }
  api$add_route(route, asset_route, header = TRUE)
  api
}

#' @rdname api_assets
#' @export
api_statics <- function(
  api,
  at,
  path,
  use_index = TRUE,
  fallthrough = FALSE,
  html_charset = "utf-8",
  headers = list(),
  validation = NULL,
  except = NULL
) {
  api$serve_static(
    at = at,
    path = path,
    use_index = use_index,
    fallthrough = fallthrough,
    html_charset = html_charset,
    headers = headers,
    validation = validation
  )
  for (ex in except) {
    api$exclude_static(paste0(at, ex))
  }
  api
}
