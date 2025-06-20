#' Load up an API distributed with a package
#'
#' Packages can included one or more api specification(s) by storing the
#' annotated route files and/or `_server.yml` file in subfolders of
#' `./inst/plumber2`. The name of the subfolder will be the name of the api
#'
#' @param package The name of the package that provides the api. If `NULL` then
#' a list of available apis across all installed packages is returned
#' @param name The name of the api. If `NULL` then a list of available apis in
#' the given package is returned
#' @inheritDotParams api
#'
#' @return If `package` or `name` is `NULL` then a data frame providing
#' available apis filtered on either package or name (if any is provided) is
#' returned. Otherwise a [Plumber2] object representing the api is returned
#'
#' @export
#'
#' @examples
#' # Load one of the plumber2 examples
#' api_package("plumber2", "quickstart")
#'
#' # List all available apis
#' api_package()
#'
api_package <- function(package = NULL, name = NULL, ...) {
  if (is.null(package)) {
    list_apis(name = name)
  } else if (is.null(name)) {
    list_apis(package)
  } else {
    path <- system.file("plumber2", name, package = package)
    if (path == "") {
      cli::cli_abort("No api named {.val {name}} found in {.pkg {package}}")
    }
    api(path, ...)
  }
}

list_apis <- function(package = NULL, name = NULL) {
  if (is.null(package)) {
    do.call(
      rbind,
      lapply(unname(utils::installed.packages()[, "Package"]), list_apis)
    )
  } else {
    path <- system.file("plumber2", package = package)
    if (path == "") {
      return(character())
    }
    apis <- data.frame(
      package = package,
      api = list.dirs(path, full.names = FALSE, recursive = FALSE)
    )
    if (!is.null(name)) {
      apis <- apis[apis$api == NAME, ]
    }
    apis
  }
}
