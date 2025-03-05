create_header_route <- function() {
  stack <- routr::RouteStack$new()
  stack$attach_to <- "header"
  max_size <- get_opts("maxRequestSize")
  if (!is.null(max_size)) {
    stack$add_route(routr::sizelimit_route(max_size), "max_size")
  }
  shared_secret <- get_opts("sharedSecret")
  if (!is.null(shared_secret)) {
    stack$add_route(routr::shared_secret_route(
      shared_secret,
      "Plumber-Shared-Secret"
    ))
  }
  stack
}
