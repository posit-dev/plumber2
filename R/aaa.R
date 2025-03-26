registry <- new.env(parent = emptyenv())

compact <- function(x) {
  if (!is_bare_list(x)) return(x)
  x[lengths(x) != 0]
}

as_openapi_path <- function(x) {
  stringi::stri_replace_all_regex(
    x,
    "<(.+?)(:.+?)?>",
    "{$1}"
  )
}

as_routr_path <- function(x, call = caller_env()) {
  path <- stringi::stri_replace_all_regex(
    x,
    "<(.+?)(:.+?)?>",
    ":$1"
  )
  params <- sub(":", "", stringi::stri_extract_all_regex(x, ":[^/]+")[[1]])

  protected_names <- c(
    "request",
    "response",
    "server",
    "client_id",
    "query",
    "body"
  )
  if (any(params %in% protected_names)) {
    cli::cli_abort(c(
      "Path parameters must not take the name of any of the arguments that are passed to the handler",
      i = "Rename any parameters named {.or {.val {protected_names}}}"
    ))
  }
  path
}
