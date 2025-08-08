test_that("is_plumber_api correctly identifies Plumber2 objects", {
  # Test with Plumber2 object
  pa <- Plumber2$new()
  expect_true(is_plumber_api(pa))

  # Test with non-Plumber2 object
  expect_false(is_plumber_api(list()))
  expect_false(is_plumber_api(NULL))
  expect_false(is_plumber_api("not an api"))
})

test_that("api function creates Plumber2 object with default parameters", {
  # Test with no parameters
  pa <- api()
  expect_s3_class(pa, "Plumber2")
  expect_equal(pa$host, "127.0.0.1")
  expect_equal(pa$port, 8080)
  expect_equal(pa$doc_type, "rapidoc")
  expect_equal(pa$doc_path, "__docs__")
})

test_that("api function creates Plumber2 object with custom parameters", {
  # Test with custom parameters
  pa <- api(
    host = "0.0.0.0",
    port = 8000,
    doc_type = "swagger",
    doc_path = "api-docs",
    compression_limit = 2000
  )

  expect_s3_class(pa, "Plumber2")
  expect_equal(pa$host, "0.0.0.0")
  expect_equal(pa$port, 8000)
  expect_equal(pa$doc_type, "swagger")
  expect_equal(pa$doc_path, "api-docs")
  expect_equal(pa$compression_limit, 2000)
})

test_that("api function parses plumber files correctly", {
  # Test with existing file
  file_path <- "fixtures/minimal_api.R"
  pa <- api(file_path)

  expect_s3_class(pa, "Plumber2")
  expect_length(pa$request_router$routes, 1)
})

test_that("api function parses _server.yml files correctly", {
  # Test with _server.yml
  file_path <- "fixtures/server1/_server.yml"
  pa <- api(file_path)

  expect_s3_class(pa, "Plumber2")
  expect_equal(pa$host, "0.0.0.0")
  expect_equal(pa$port, 8000)
  expect_equal(pa$doc_type, "swagger")
  expect_equal(pa$doc_path, "api-docs")
  expect_equal(pa$compression_limit, 2000)
  expect_length(pa$request_router$routes, 1)
})

test_that("api function parses _server.yml with constructor correctly", {
  # Test with _server.yml that has a constructor
  file_path <- "fixtures/server2/_server.yml"
  pa <- api(file_path)

  expect_s3_class(pa, "Plumber2")
  expect_equal(pa$host, "0.0.0.0")
  expect_equal(pa$port, 8001)

  # Check if the API doc from constructor is preserved
  openapi <- pa$.__enclos_env__$private$OPENAPI
  expect_equal(openapi$info$title, "Constructor API")
  expect_equal(openapi$info$version, "1.0.0")
})

test_that("api function handles constructor errors correctly", {
  # Create a bad constructor file
  tmp_dir <- tempdir()
  bad_constructor <- file.path(tmp_dir, "bad_constructor.R")
  server_yml <- file.path(tmp_dir, "bad_server.yml")

  on.exit({
    unlink(bad_constructor)
    unlink(server_yml)
  })

  # Write files
  writeLines("list()", bad_constructor)
  writeLines(
    paste0(
      "constructor: ",
      basename(bad_constructor),
      "\n",
      "routes:\n",
      "  - minimal_api.R"
    ),
    server_yml
  )

  # Test with bad constructor
  expect_snapshot(api(server_yml), error = TRUE)
})

test_that("api_parse correctly parses plumber files", {
  pa <- Plumber2$new()

  # Parse a single file
  result <- api_parse(pa, "fixtures/minimal_api.R")

  expect_s3_class(result, "Plumber2")
  expect_length(result$request_router$routes, 1)

  # Parse multiple files
  pa <- Plumber2$new()
  result <- api_parse(
    pa,
    "fixtures/server3/api_with_low_order.R",
    "fixtures/server3/api_with_high_order.R"
  )

  expect_s3_class(result, "Plumber2")
  expect_length(result$request_router$routes, 2)
})

test_that("api_parse respects @routeOrder", {
  pa <- Plumber2$new()

  # Parse files in wrong order but with correct priority
  result <- api_parse(
    pa,
    "fixtures/server3/api_with_high_order.R",
    "fixtures/server3/api_with_low_order.R"
  )

  # Routes should be ordered by @routeOrder, not input order
  expect_s3_class(result, "Plumber2")
  expect_length(result$request_router$routes, 2)

  # Create a simple way to check the route order
  expect_equal(
    result$request_router$routes,
    c("api_with_low_order", "api_with_high_order")
  )
})

test_that("dots_to_plumber_files correctly processes file paths", {
  # Test with no input
  result <- dots_to_plumber_files()
  expect_equal(result, character())

  # Test with single file
  result <- dots_to_plumber_files("fixtures/minimal_api.R")
  expect_equal(result, "fixtures/minimal_api.R")

  # Test with multiple files
  result <- dots_to_plumber_files(
    "fixtures/minimal_api.R",
    "fixtures/server3/api_with_high_order.R"
  )
  expect_equal(length(result), 2)
  expect_true(all(
    c("fixtures/minimal_api.R", "fixtures/server3/api_with_high_order.R") %in%
      result
  ))

  # Test with directory
  result <- dots_to_plumber_files("fixtures/server3")
  expect_true(all(endsWith(result, ".R")))

  # Test preferring server.yml
  result <- dots_to_plumber_files("fixtures/server2", prefer_yml = TRUE)
  expect_equal(unname(result), "fixtures/server2/_server.yml")

  # Test not preferring server.yml
  result <- dots_to_plumber_files("fixtures/server2", prefer_yml = FALSE)
  expect_true(all(endsWith(result, ".R")))
  expect_true(!"fixtures/server2/_server.yml" %in% result)
})

test_that("dots_to_plumber_files handles errors correctly", {
  # Test error for non-existent files
  expect_snapshot(dots_to_plumber_files("non_existent_file.R"), error = TRUE)

  # Check for warning when providing server.yml files without preferring them
  expect_snapshot(
    dots_to_plumber_files("fixtures/server1/_server.yml", prefer_yml = FALSE)
  )

  # Check for error when providing multiple server.yml files and preferring them
  expect_snapshot(
    dots_to_plumber_files("fixtures"),
    error = TRUE
  )

  # Check for warning when providing other files along with server.yml
  expect_snapshot(
    dots_to_plumber_files(
      "fixtures/server1/_server.yml",
      "fixtures/minimal_api.R"
    )
  )
})
