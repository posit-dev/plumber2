test_that("openapi function creates valid OpenAPI object", {
  # Test with minimal parameters
  result <- openapi(
    info = openapi_info(title = "Test API", version = "1.0.0")
  )

  expect_s3_class(result, "plumber2_openapi")
  expect_equal(result$openapi, "3.0.0")
  expect_equal(result$info$title, "Test API")
  expect_equal(result$info$version, "1.0.0")
  expect_equal(result$paths, list())

  # Test with all parameters
  result <- openapi(
    openapi = "3.0.1",
    info = openapi_info(title = "Test API", version = "1.0.0"),
    paths = list("/test" = list()),
    tags = list(openapi_tag(name = "test"))
  )

  expect_equal(result$openapi, "3.0.1")
  expect_equal(names(result$paths), "/test")
  expect_equal(result$tags[[1]]$name, "test")

  # Test error for invalid paths
  expect_snapshot(openapi(info = openapi_info(title = "Test", version = "1.0.0"), paths = "invalid"),
                 error = TRUE)

  # Test error for missing required parameters
  expect_snapshot(openapi(), error = TRUE)
})

test_that("openapi_info function creates valid info object", {
  # Test with minimal parameters
  result <- openapi_info(title = "Test API", version = "1.0.0")

  expect_equal(result$title, "Test API")
  expect_equal(result$version, "1.0.0")
  expect_null(result$description)

  # Test with all parameters
  result <- openapi_info(
    title = "Test API",
    description = "API description",
    terms_of_service = "https://example.com/terms",
    contact = openapi_contact(name = "API Support"),
    license = openapi_license(name = "MIT"),
    version = "1.0.0"
  )

  expect_equal(result$title, "Test API")
  expect_equal(result$description, "API description")
  expect_equal(result$termsOfService, "https://example.com/terms")
  expect_equal(result$contact$name, "API Support")
  expect_equal(result$license$name, "MIT")
  expect_equal(result$version, "1.0.0")

  # Test error for missing required parameters
  expect_snapshot(openapi_info(), error = TRUE)
})

test_that("openapi_contact function creates valid contact object", {
  # Test with minimal parameters
  result <- openapi_contact()
  expect_equal(result, structure(list(), names = character(0)))

  # Test with all parameters
  result <- openapi_contact(
    name = "API Support",
    url = "https://example.com/support",
    email = "support@example.com"
  )

  expect_equal(result$name, "API Support")
  expect_equal(result$url, "https://example.com/support")
  expect_equal(result$email, "support@example.com")
})

test_that("openapi_license function creates valid license object", {
  # Test with minimal parameters
  result <- openapi_license()
  expect_equal(result, structure(list(), names = character(0)))

  # Test with name only
  result <- openapi_license(name = "MIT")
  expect_equal(result$name, "MIT")
  expect_null(result$url)

  # Test with all parameters
  result <- openapi_license(
    name = "MIT",
    url = "https://opensource.org/licenses/MIT"
  )

  expect_equal(result$name, "MIT")
  expect_equal(result$url, "https://opensource.org/licenses/MIT")

  # Test error when url is provided without name
  expect_snapshot(openapi_license(url = "https://example.com"), error = TRUE)
})

test_that("openapi_path function creates valid path object", {
  # Test with minimal parameters
  result <- openapi_path()
  expect_equal(result, structure(list(), names = character(0)))

  # Test with summary and description
  result <- openapi_path(
    summary = "Test path",
    description = "Test path description"
  )

  expect_equal(result$summary, "Test path")
  expect_equal(result$description, "Test path description")

  # Test with HTTP methods
  get_op <- openapi_operation(summary = "Get operation")
  post_op <- openapi_operation(summary = "Post operation")

  result <- openapi_path(
    get = get_op,
    post = post_op
  )

  expect_equal(result$get, get_op)
  expect_equal(result$post, post_op)
})

test_that("openapi_operation function creates valid operation object", {
  # Test with minimal parameters
  result <- openapi_operation()
  expect_equal(result, structure(list(), names = character(0)))

  # Test with all parameters
  result <- openapi_operation(
    summary = "Test operation",
    description = "Test operation description",
    operation_id = "testOperation",
    parameters = list(openapi_parameter(name = "param1", location = "query")),
    request_body = openapi_request_body(
      content = openapi_content("application/json" = openapi_schema(character()))
    ),
    responses = list("200" = openapi_response(description = "Success")),
    tags = c("tag1", "tag2")
  )

  expect_equal(result$summary, "Test operation")
  expect_equal(result$description, "Test operation description")
  expect_equal(result$operationId, "testOperation")
  expect_equal(length(result$parameters), 1)
  expect_type(result$requestBody, "list")
  expect_equal(names(result$responses), "200")
  expect_equal(result$tags, list("tag1", "tag2"))

  # Test error for non-named responses
  expect_snapshot(openapi_operation(responses = list("response")), error = TRUE)
})

test_that("openapi_parameter function creates valid parameter object", {
  # Test with minimal parameters
  expect_snapshot(openapi_parameter(), error = TRUE)

  # Test with required parameters
  result <- openapi_parameter(name = "param1", location = "query")

  expect_equal(result$name, "param1")
  expect_equal(result$`in`, "query")
  expect_true(is.list(result$schema))

  # Test with all parameters
  result <- openapi_parameter(
    name = "param1",
    location = "query",
    description = "Test parameter",
    required = TRUE,
    schema = openapi_schema(character())
  )

  expect_equal(result$name, "param1")
  expect_equal(result$`in`, "query")
  expect_equal(result$description, "Test parameter")
  expect_true(result$required)
  expect_equal(result$schema$type, "string")

  # Test path parameter is always required
  result <- openapi_parameter(name = "param1", location = "path")
  expect_true(result$required)

  # Test error when path parameter is not required
  expect_snapshot(openapi_parameter(name = "param1", location = "path", required = FALSE),
                  error = TRUE)

  # Test error when both schema and content are provided
  expect_snapshot(
    openapi_parameter(
      name = "param1",
      location = "query",
      schema = openapi_schema(character()),
      content = openapi_content("text/plain" = openapi_schema(character()))
    ),
    error = TRUE
  )
})

test_that("openapi_header function creates valid header object", {
  # Test with minimal parameters
  result <- openapi_header()
  expect_equal(result, structure(list(), names = character(0)))

  # Test with all parameters
  result <- openapi_header(
    description = "Test header",
    schema = openapi_schema(character())
  )

  expect_equal(result$description, "Test header")
  expect_equal(result$schema$type, "string")
})

test_that("openapi_schema function creates valid schema object for AsIs class", {
  # Test with AsIs class
  result <- openapi_schema(I("integer"))

  expect_equal(result$type, "integer")
  expect_null(result$default)

  # Test with default value
  result <- openapi_schema(I("string"), default = "test")

  expect_equal(result$type, "string")
  expect_equal(result$default, "test")
})

test_that("openapi_schema function creates valid schema object for numeric class", {
  # Test with integer
  result <- openapi_schema(1L)

  expect_equal(result$type, "integer")
  expect_null(result$default)

  # Test with double
  result <- openapi_schema(1.0)

  expect_equal(result$type, "number")
  expect_null(result$default)

  # Test with bounds
  result <- openapi_schema(1L, min = 0, max = 10)

  expect_equal(result$type, "integer")
  expect_equal(result$minimum, 0)
  expect_equal(result$maximum, 10)
})

test_that("openapi_schema function creates valid schema object for character class", {
  result <- openapi_schema("test")

  expect_equal(result$type, "string")
  expect_null(result$default)

  # Test with default value
  result <- openapi_schema("test", default = "default")

  expect_equal(result$type, "string")
  expect_equal(result$default, "default")
})

test_that("openapi_schema function creates valid schema object for raw class", {
  result <- openapi_schema(raw(0))

  expect_equal(result$type, "string")
  expect_equal(result$format, "binary")
  expect_null(result$default)
})

test_that("openapi_schema function creates valid schema object for Date class", {
  result <- openapi_schema(Sys.Date())

  expect_equal(result$type, "string")
  expect_equal(result$format, "date")
  expect_null(result$default)
})

test_that("openapi_schema function creates valid schema object for POSIXt class", {
  result <- openapi_schema(Sys.time())

  expect_equal(result$type, "string")
  expect_equal(result$format, "date-time")
  expect_null(result$default)
})

test_that("openapi_schema function creates valid schema object for factor class", {
  f <- factor(c("a", "b", "c"))
  result <- openapi_schema(f)

  expect_equal(result$type, "string")
  expect_equal(result$enum, c("a", "b", "c"))
  expect_null(result$default)
})

test_that("openapi_schema function creates valid schema object for list class", {
  # Test with unnamed list (array)
  result <- openapi_schema(list(1L))

  expect_equal(result$type, "array")
  expect_equal(result$items$type, "integer")

  # Test with named list (object)
  result <- openapi_schema(list(
    integer_field = 1L,
    string_field = "test"
  ), required = c("integer_field"))

  expect_equal(result$type, "object")
  expect_equal(result$properties$integer_field$type, "integer")
  expect_equal(result$properties$string_field$type, "string")
  expect_equal(result$required, "integer_field")

  # Test error for unnamed list with multiple elements
  expect_snapshot(openapi_schema(list(1, "test")), error = TRUE)

  # Test error for invalid required fields
  expect_snapshot(
    openapi_schema(
      list(field1 = 1),
      required = c("field2")
    ),
    error = TRUE
  )
})

test_that("openapi_content function creates valid content object", {
  # Test with no parameters
  result <- openapi_content()
  expect_equal(result, list())

  # Test with valid parameters
  result <- openapi_content(
    "application/json" = openapi_schema(character()),
    "text/plain" = openapi_schema(character())
  )

  expect_equal(names(result), c("application/json", "text/plain"))
  expect_equal(result$`application/json`$schema$type, "string")
  expect_equal(result$`text/plain`$schema$type, "string")

  # Test error for invalid mime type names
  expect_snapshot(
    openapi_content(
      invalid_name = openapi_schema(character())
    ),
    error = TRUE
  )
})

test_that("openapi_request_body function creates valid request body object", {
  # Test with minimal parameters
  result <- openapi_request_body()
  expect_equal(result, structure(list(), names = character(0)))

  # Test with valid parameters
  result <- openapi_request_body(
    description = "Test request body",
    content = openapi_content("application/json" = openapi_schema(character())),
    required = TRUE
  )

  expect_equal(result$description, "Test request body")
  expect_equal(names(result$content), "application/json")
  expect_true(result$required)

  # Test error when description or required are provided without content
  expect_snapshot(
    openapi_request_body(description = "Test request body"),
    error = TRUE
  )

  expect_snapshot(
    openapi_request_body(required = TRUE),
    error = TRUE
  )
})

test_that("openapi_response function creates valid response object", {
  # Test with minimal parameters
  expect_snapshot(openapi_response(), error = TRUE)

  # Test with required parameters
  result <- openapi_response(description = "Test response")

  expect_equal(result$description, "Test response")
  expect_null(result$content)

  # Test with all parameters
  result <- openapi_response(
    description = "Test response",
    content = openapi_content("application/json" = openapi_schema(character())),
    headers = list(
      "X-Test-Header" = openapi_header(description = "Test header")
    )
  )

  expect_equal(result$description, "Test response")
  expect_equal(names(result$content), "application/json")
  expect_equal(names(result$headers), "X-Test-Header")

  # Test error for non-named headers
  expect_snapshot(
    openapi_response(
      description = "Test response",
      headers = list("header")
    ),
    error = TRUE
  )
})

test_that("openapi_tag function creates valid tag object", {
  # Test with minimal parameters
  result <- openapi_tag()
  expect_equal(result, structure(list(), names = character(0)))

  # Test with name only
  result <- openapi_tag(name = "test")

  expect_equal(result$name, "test")
  expect_null(result$description)

  # Test with all parameters
  result <- openapi_tag(
    name = "test",
    description = "Test tag"
  )

  expect_equal(result$name, "test")
  expect_equal(result$description, "Test tag")

  # Test error when description is provided without name
  expect_snapshot(openapi_tag(description = "Test tag"), error = TRUE)
})

test_that("require_input function validates input presence", {
  # Test valid input
  expect_no_error(require_input(test = "value"))

  # Test multiple valid inputs
  expect_no_error(require_input(test1 = "value1", test2 = "value2"))

  # Test error for missing input
  expect_snapshot(require_input(test = character()), error = TRUE)

  # Test error for multiple missing inputs
  expect_snapshot(require_input(test1 = character(), test2 = character()), error = TRUE)

  # Test mixed valid and invalid inputs
  expect_snapshot(require_input(valid = "value", invalid = character()), error = TRUE)
})
