create_header_router <- function(max_size, shared_secret) {
  stack <- routr::RouteStack$new()
  stack$attach_to <- "header"
  if (!is.null(max_size)) {
    stack$add_route(routr::sizelimit_route(max_size), "max_size")
  }
  if (!is.null(shared_secret) && shared_secret != "") {
    stack$add_route(routr::shared_secret_route(
      shared_secret,
      "Plumber-Shared-Secret"
    ), "shared_secret")
  }
  stack
}
