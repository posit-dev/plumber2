.onLoad <- function(lib, pkg) {
  run_on_load()
}

# Define %||% for R < 4.4
if (!exists("%||%", envir = baseenv())) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}
