create_header_route <- function(max_size, shared_secret, ignore_trailing_slash = "no") {
  stack <- routr::RouteStack$new(ignore_trailing_slash = ignore_trailing_slash)
  stack$attach_to <- "header"
  if (!is.null(max_size)) {
    stack$add_route(routr::sizelimit_route(max_size), "max_size")
  }
  if (!is.null(shared_secret)) {
    stack$add_route(routr::shared_secret_route(
      shared_secret,
      "Plumber-Shared-Secret"
    ))
  }
  stack
}
