#' Create a _server.yml file to describe your API
#'
#' While you can manually create a plumber2 API by calling [api()], you will
#' often need to deploy the api somewhere else. To facilitate this you can
#' create a `_server.yml` that encapsulates all of your settings and plumber
#' files. If you call [api()] with a path to such a file the API will be
#' constructed according to its content.
#'
#' @param ... path to files and/or directories that contain annotated plumber
#' files to be used by your API
#' @param path The folder to place the generated `_server.yml` file in
#' @param constructor The path to a file that creates a plumber2 API object. Can
#' be omitted in which case an API object will be created for you
#' @param freeze_opt Logical specifying whether any options you currently have
#' locally (either as environment variables or R options) should be written to
#' the `_server.yml` file. Shared secret will never be written to the file and
#' you must find a different way to move that to your deployment server.
#'
#' @export
#'
create_server_yml <- function(
  ...,
  path = ".",
  constructor = NULL,
  freeze_opt = TRUE
) {
  routes <- unlist(list(...))
  if (!is.null(routes)) {
    check_character(routes)
    if (any(!fs::file_exists(routes))) {
      cli::cli_abort("{.arg ...} must point to existing files or directories")
    }
    routes <- fs::path_real(routes)
    if (!all(fs::path_has_parent(routes, path))) {
      cli::cli_abort(
        "{.arg ...} must all point to files placed in subfolders relative to {.arg path}"
      )
    }
    routes <- fs::path_rel(routes, path)
  }
  if (
    !is.null(constructor) &&
      (!fs::is_file(constructor) || !fs::path_ext(constructor) %in% c("R", "r"))
  ) {
    cli::cli_abort("{.arg constructor} must point to an existing R file")
  }
  settings <- list(
    engine = "plumber2",
    routes = routes,
    constructor = constructor,
    options = list()
  )

  if (freeze_opt) {
    settings$options <- all_opts()
  }
  yaml::write_yaml(settings, fs::path_join(c(path, "_server.yml")))
}

is_plumber2_server_yml <- function(path) {
  vapply(path, function(p) {
    if (!grepl("^_server.ya?ml$", fs::path_file(p))) {
      return(FALSE)
    }
    isTRUE(tolower(yaml::read_yaml(p)$engine) == "plumber2")
  }, logical(1))
}
