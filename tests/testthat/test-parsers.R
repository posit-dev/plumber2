test_that("register_parser registers a parser correctly", {
  # Create a mock parser function
  mock_parser <- function() {
    function(raw, directives) paste0("mock:", rawToChar(raw))
  }

  # Register the parser
  register_parser("mock", mock_parser, "text/mock")

  # Check if it's registered correctly
  parsers <- show_registered_parsers()
  expect_true("mock" %in% parsers$name)
  expect_equal(parsers$mime_types[parsers$name == "mock"][[1]], "text/mock")

  # Check error conditions
  expect_snapshot(
    register_parser("mock/invalid", mock_parser, "text/mock"),
    error = TRUE
  )
  expect_snapshot(
    register_parser("...", mock_parser, "text/mock"),
    error = TRUE
  )
  expect_snapshot(
    register_parser("none", mock_parser, "text/mock"),
    error = TRUE
  )
})

test_that("show_registered_parsers returns correct format", {
  result <- show_registered_parsers()

  # Check structure
  expect_s3_class(result, "data.frame")
  expect_true(all(
    c("name", "mime_types", "default") %in% names(result)
  ))

  # Check content
  expect_true("json" %in% result$name)
  expect_true(any(vapply(
    result$mime_types[result$name == "json"],
    function(x) "application/json" %in% x,
    logical(1)
  )))
})

test_that("get_parsers returns correct parsers", {
  # Default parsers
  parsers <- get_parsers()
  expect_type(parsers, "list")
  expect_true(all(vapply(parsers, is.function, logical(1))))

  # Named parsers
  json_parser <- get_parsers("json")
  expect_true("application/json" %in% names(json_parser))

  # Multiple parsers
  multi_parsers <- get_parsers(c("json", "csv"))
  expect_true("application/json" %in% names(multi_parsers))
  expect_true("text/csv" %in% names(multi_parsers))

  # Error conditions
  expect_snapshot(
    get_parsers("nonexistent"),
    error = TRUE
  )
  expect_snapshot(
    get_parsers(list(c("...", "..."))),
    error = TRUE
  )
})

test_that("parse_csv parses correctly", {
  # Create parser
  parser <- parse_csv()
  expect_type(parser, "closure")

  # Test with a CSV string
  csv_string <- "a,b\n1,x\n2,y\n3,z"
  csv_raw <- charToRaw(csv_string)
  result <- parser(csv_raw, list())

  expect_s3_class(result, "data.frame")
  expect_equal(dim(result), c(3, 2))
  expect_equal(colnames(result), c("a", "b"))
  expect_equal(result$a, c(1, 2, 3))
  expect_equal(result$b, c("x", "y", "z"))
})

test_that("parse_octet passes through raw data unchanged", {
  # Create parser
  parser <- parse_octet()
  expect_type(parser, "closure")

  # Test with some raw data
  test_raw <- charToRaw("test data")
  result <- parser(test_raw, list())

  expect_equal(result, test_raw)
})

test_that("parse_rds parses correctly", {
  # Create parser
  parser <- parse_rds()
  expect_type(parser, "closure")

  # Test with various objects
  obj <- list(a = 1, b = "test")
  obj_raw <- serialize(obj, NULL)
  result <- parser(obj_raw, list())

  expect_equal(result, obj)
})

test_that("parse_text parses correctly", {
  # Create parser
  parser <- parse_text()
  expect_type(parser, "closure")

  # Test with a simple text string
  text_string <- "Hello, world!"
  text_raw <- charToRaw(text_string)
  result <- parser(text_raw, list())

  expect_equal(result, text_string)
})

test_that("parse_tsv parses correctly", {
  # Create parser
  parser <- parse_tsv()
  expect_type(parser, "closure")

  # Test with a TSV string
  tsv_string <- "a\tb\n1\tx\n2\ty\n3\tz"
  tsv_raw <- charToRaw(tsv_string)
  result <- parser(tsv_raw, list())

  expect_s3_class(result, "data.frame")
  expect_equal(dim(result), c(3, 2))
  expect_equal(colnames(result), c("a", "b"))
  expect_equal(result$a, c(1, 2, 3))
  expect_equal(result$b, c("x", "y", "z"))
})

test_that("parse_yaml parses correctly", {
  # Create parser
  parser <- parse_yaml()
  expect_type(parser, "closure")

  # Test with a YAML string
  yaml_string <- "a: 1\nb: test\nc:\n  d: true"
  yaml_raw <- charToRaw(yaml_string)
  result <- parser(yaml_raw, list())

  expect_equal(result, list(a = 1, b = "test", c = list(d = TRUE)))
})

test_that("parse_multipart parses correctly", {
  skip_if_not_installed("webutils")

  # Create an actual multipart/form-data raw payload
  boundary <- "------------------------abcdef1234567890"

  # Create the multipart body parts
  part1 <- paste0(
    "--", boundary, "\r\n",
    "Content-Disposition: form-data; name=\"text_field\"\r\n",
    "Content-Type: text/plain\r\n",
    "\r\n",
    "test data\r\n"
  )

  part2 <- paste0(
    "--", boundary, "\r\n",
    "Content-Disposition: form-data; name=\"csv_field\"; filename=\"test.csv\"\r\n",
    "Content-Type: text/csv\r\n",
    "\r\n",
    "a,b\r\n1,x\r\n",
    "--", boundary, "--\r\n"
  )

  # Combine parts into a complete multipart body
  multipart_body <- paste0(part1, part2)
  multipart_raw <- charToRaw(multipart_body)

  # Create parser
  parser <- parse_multipart()
  expect_type(parser, "closure")

  # Parse the multipart data
  result <- parser(multipart_raw, list(boundary = boundary))

  # Verify the result structure
  expect_type(result, "list")
  expect_length(result, 2)

  # Check first part (text field)
  expect_equal(attr(result[[1]], "name"), "text_field")
  expect_equal(attr(result[[1]], "content_type"), "text/plain")
  expect_equal(as.vector(result[[1]]), "test data")

  # Check second part (CSV file)
  expect_equal(attr(result[[2]], "name"), "csv_field")
  expect_equal(attr(result[[2]], "filename"), "test.csv")
  expect_equal(attr(result[[2]], "content_type"), "text/csv")
  expect_s3_class(result[[2]], "data.frame")
  expect_equal(colnames(result[[2]]), c("a", "b"))
})
