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

as_routr_path <- function(x) {
  stringi::stri_replace_all_regex(
    x,
    "<(.+?)(:.+?)?>",
    ":$1"
  )
}
