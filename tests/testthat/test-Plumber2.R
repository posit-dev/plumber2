test_that("Plumber2 gets initialized with correct values", {
  papi <- Plumber2$new()
  expect_null(papi$.__enclos_env__$private$HEADER_ROUTER)
  expect_null(papi$.__enclos_env__$private$REQUEST_ROUTER)

  papi <- Plumber2$new(max_request_size = 1024, shared_secret = "test")
  expect_equal(papi$header_router$routes, c("max_size", "shared_secret"))
})

test_that("Plumber2 prints info", {
  papi <- Plumber2$new()

  expect_snapshot(papi$format())

  papi$start(block = FALSE, showcase = FALSE, silent = TRUE)

  expect_snapshot(papi$format())

  papi$stop()
})

test_that("Plumber2 attaches doc route at startup", {
  papi <- Plumber2$new()

  expect_length(papi$request_router$routes, 0)

  papi$start(block = FALSE, showcase = FALSE, silent = TRUE)

  expect_equal(papi$request_router$routes, "openapi")

  papi$stop()

  expect_length(papi$request_router$routes, 0)
})

test_that("add_route works correctly", {
  papi <- Plumber2$new()

  # Add route to request router
  papi$add_route("test_route")
  expect_true(papi$request_router$has_route("test_route"))

  # Add route to header router
  papi$add_route("test_header_route", header = TRUE)
  expect_true(papi$header_router$has_route("test_header_route"))

  # Add route with custom route object
  custom_route <- routr::Route$new()
  papi$add_route("custom_route", custom_route)
  expect_true(papi$request_router$has_route("custom_route"))

  # Test merging into existing route
  existing_route <- papi$request_router$get_route("test_route")
  papi$add_route("test_route", routr::Route$new())
  expect_identical(papi$request_router$get_route("test_route"), existing_route)
})

test_that("request_handler adds handlers correctly", {
  papi <- Plumber2$new()

  # Basic handler
  handler <- function(req, res) "test"
  papi$request_handler(
    method = "GET",
    path = "/test",
    handler = handler,
    serializers = get_serializers()
  )

  expect_length(papi$request_router$routes, 1)
  expect_equal(papi$request_router$routes, "default")

  # Handler with custom route
  papi$request_handler(
    method = "POST",
    path = "/custom",
    handler = handler,
    serializers = get_serializers(),
    route = "custom_route"
  )

  expect_length(papi$request_router$routes, 2)
  expect_true("custom_route" %in% papi$request_router$routes)

  # Handler with header router
  papi$request_handler(
    method = "GET",
    path = "/header",
    handler = handler,
    serializers = get_serializers(),
    header = TRUE
  )

  expect_length(papi$header_router$routes, 1)
  expect_equal(papi$header_router$routes, "default")
})

test_that("redirect adds redirects correctly", {
  papi <- Plumber2$new()

  # Add redirect
  papi$redirect("GET", "/old", "/new")

  # Check if redirect is added
  res <- papi$test_header(fiery::fake_request("http://example.com/old"))
  expect_equal(res$status, 308L)
  expect_equal(res$headers$location, "/new")
})

test_that("parse_file loads file content", {
  # Parse the file
  papi <- Plumber2$new()
  papi$parse_file("fixtures/minimal_api.R")

  # Check if route is added
  expect_length(papi$request_router$routes, 1)
})

test_that("add_api_doc adds docs correctly", {
  papi <- Plumber2$new()

  # Add API docs
  test_doc <- list(info = list(title = "Test API", version = "1.0.0"))
  papi$add_api_doc(test_doc)

  # Check if docs are added
  openapi <- papi$.__enclos_env__$private$OPENAPI
  expect_equal(openapi$info$title, "Test API")
  expect_equal(openapi$info$version, "1.0.0")

  # Test with subset
  test_subset <- list(description = "Test path")
  papi$add_api_doc(test_subset, subset = c("paths", "/test", "get"))

  # Check if subset is added
  openapi <- papi$.__enclos_env__$private$OPENAPI
  expect_equal(openapi$paths$`/test`$get$description, "Test path")

  # Test with overwrite
  new_doc <- list(info = list(title = "New Title"))
  papi$add_api_doc(new_doc, overwrite = TRUE)

  # Check if docs are overwritten
  openapi <- papi$.__enclos_env__$private$OPENAPI
  expect_equal(openapi$info$title, "New Title")
  expect_null(openapi$info$version)
})

test_that("add_shiny adds shiny app correctly", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("callr")

  papi <- Plumber2$new()

  # Create a simple shiny app
  shiny_app <- shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::textOutput("text")
    ),
    server = function(input, output) {
      output$text <- shiny::renderText("Hello from Shiny!")
    }
  )

  # Add shiny app to plumber
  papi$add_shiny("/shiny", shiny_app)

  # Check if shiny proxy is attached
  attached <- papi$plugins
  proxy_found <- FALSE
  for (plugin in attached) {
    if (inherits(plugin, "ReverseProxy")) {
      proxy_found <- TRUE
      break
    }
  }
  expect_true(proxy_found)
})

test_that("forward adds reverse proxy correctly", {
  papi <- Plumber2$new()

  # Add forward
  papi$forward("/api", "http://example.com")

  # Check if reverse proxy is attached
  attached <- papi$plugins
  proxy_found <- FALSE
  for (plugin in attached) {
    if (inherits(plugin, "ReverseProxy")) {
      proxy_found <- TRUE
      break
    }
  }
  expect_true(proxy_found)
})

test_that("active bindings work correctly", {
  papi <- Plumber2$new()

  # Test doc_type getter
  expect_equal(papi$doc_type, "rapidoc")

  # Test doc_type setter
  papi$doc_type <- "redoc"
  expect_equal(papi$doc_type, "redoc")

  # Test doc_type with invalid value
  expect_error(papi$doc_type <- "invalid")

  # Test doc_path getter
  expect_equal(papi$doc_path, "__docs__")

  # Test doc_path setter
  papi$doc_path <- "api-docs"
  expect_equal(papi$doc_path, "api-docs")
})

test_that("utility functions work correctly", {
  # Test subset_to_list
  result <- subset_to_list(c("a", "b", "c"), "value")
  expect_equal(result, list(a = list(b = list(c = "value"))))

  # Test empty subset
  result <- subset_to_list(character(), "value")
  expect_equal(result, "value")

  # Test list_has_subset
  test_list <- list(a = list(b = list(c = "value")))
  expect_true(list_has_subset(test_list, c("a", "b", "c")))
  expect_true(list_has_subset(test_list, c("a", "b")))
  expect_true(list_has_subset(test_list, c("a")))
  expect_true(list_has_subset(test_list, character()))
  expect_false(list_has_subset(test_list, c("a", "d")))
  expect_false(list_has_subset(test_list, c("d")))
})
