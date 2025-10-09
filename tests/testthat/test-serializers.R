test_that("register_serializer registers a serializer correctly", {
  # Create a mock serializer function
  mock_serializer <- function() {
    function(x) paste0("mock:", x)
  }

  # Register the serializer
  register_serializer("mock", mock_serializer, "text/mock")

  # Check if it's registered correctly
  serializers <- show_registered_serializers()
  expect_true("mock" %in% serializers$name)
  expect_equal(serializers$mime_type[serializers$name == "mock"], "text/mock")

  # Check error conditions
  expect_snapshot(
    register_serializer("mock/invalid", mock_serializer, "text/mock"),
    error = TRUE
  )
  expect_snapshot(
    register_serializer("...", mock_serializer, "text/mock"),
    error = TRUE
  )
  expect_snapshot(
    register_serializer("none", mock_serializer, "text/mock"),
    error = TRUE
  )
})

test_that("show_registered_serializers returns correct format", {
  result <- show_registered_serializers()

  # Check structure
  expect_s3_class(result, "data.frame")
  expect_true(all(
    c("name", "mime_type", "graphic", "default") %in% names(result)
  ))

  # Check content
  expect_true("json" %in% result$name)
  expect_true("application/json" %in% result$mime_type)
})

test_that("get_serializers returns correct serializers", {
  # Default serializers
  serializers <- get_serializers()
  expect_type(serializers, "list")
  expect_true(all(vapply(serializers, is.function, logical(1))))

  # Named serializers
  json_serializer <- get_serializers("json")
  expect_length(json_serializer, 1)
  expect_equal(names(json_serializer), "application/json")

  # Multiple serializers
  multi_serializers <- get_serializers(c("json", "csv"))
  expect_length(multi_serializers, 2)
  expect_equal(
    sort(names(multi_serializers)),
    sort(c("application/json", "text/csv"))
  )

  # Error conditions
  expect_snapshot(
    get_serializers("nonexistent"),
    error = TRUE
  )
  expect_snapshot(get_serializers(list(c("...", "..."))), error = TRUE)
})

test_that("format_csv serializes correctly", {
  # Create formatter
  formatter <- format_csv()
  expect_type(formatter, "closure")

  # Test with a data frame
  df <- data.frame(a = 1:3, b = c("x", "y", "z"))
  result <- formatter(df)
  expect_type(result, "character")
  expect_true(grepl("a,b", result))
  expect_true(grepl("1,x", result))
})

test_that("format_tsv serializes correctly", {
  # Create formatter
  formatter <- format_tsv()
  expect_type(formatter, "closure")

  # Test with a data frame
  df <- data.frame(a = 1:3, b = c("x", "y", "z"))
  result <- formatter(df)
  expect_type(result, "character")
  expect_true(grepl("a\tb", result))
  expect_true(grepl("1\tx", result))
})

test_that("format_rds serializes correctly", {
  # Create formatter
  formatter <- format_rds()
  expect_type(formatter, "closure")

  # Test with various objects
  obj <- list(a = 1, b = "test")
  result <- formatter(obj)
  expect_type(result, "raw")

  # Test deserialization
  expect_equal(unserialize(result), obj)
})

test_that("format_yaml serializes correctly", {
  # Create formatter
  formatter <- format_yaml()
  expect_type(formatter, "closure")

  # Test with a list
  obj <- list(a = 1, b = "test", c = list(d = TRUE))
  result <- formatter(obj)
  expect_type(result, "character")
  expect_true(grepl("a: 1", result))
  expect_true(grepl("b: test", result))
})

test_that("format_format serializes correctly", {
  # Create formatter
  formatter <- format_format()
  expect_type(formatter, "closure")

  # Test with a vector
  vec <- c(1, 2, 3)
  result <- formatter(vec)
  expect_type(result, "character")
  expect_equal(result, "1\n2\n3")

  # Test with custom separator
  formatter2 <- format_format(sep = ", ")
  result2 <- formatter2(vec)
  expect_equal(result2, "1, 2, 3")
})

test_that("format_print serializes correctly", {
  # Create formatter
  formatter <- format_print()
  expect_type(formatter, "closure")

  # Test with a data frame
  df <- data.frame(a = 1:3, b = c("x", "y", "z"))
  result <- formatter(df)
  expect_type(result, "character")
  expect_true(grepl("a b", result[1]))
})

test_that("format_cat serializes correctly", {
  # Create formatter
  formatter <- format_cat()
  expect_type(formatter, "closure")

  # Test with a character vector
  vec <- c("line1", "line2", "line3")
  result <- formatter(vec)
  expect_type(result, "character")
  expect_equal(result, "line1 line2 line3\n")
})

test_that("device_formatter creates valid formatter", {
  # Mock device functions
  mock_device_open <- function(filename, ...) {
    # Just create an empty file
    file.create(filename)
    1 # Return a device ID
  }

  mock_device_close <- function() {
    # Do nothing
  }

  # Create formatter constructor
  constructor <- device_formatter(mock_device_open, mock_device_close)
  expect_s3_class(constructor, "device_constructor")

  # Create formatter
  formatter <- constructor()
  expect_s3_class(formatter, "device_formatter")

  # Check formatter has appropriate attributes
  expect_type(attr(formatter, "init"), "closure")
  expect_type(attr(formatter, "close"), "closure")
  expect_type(attr(formatter, "clean"), "closure")
  expect_type(attr(formatter, "with"), "closure")

  # Test error conditions
  bad_device <- function() {} # No filename parameter
  expect_snapshot(device_formatter(bad_device), error = TRUE)
})

test_that("formatter orchestration functions work correctly", {
  # Create a mock device formatter
  mock_device_open <- function(filename, ...) {
    # Just create an empty file
    file.create(filename)
    1 # Return a device ID
  }

  constructor <- device_formatter(mock_device_open)
  formatter <- constructor()

  # Test init_formatter
  info <- init_formatter(formatter)
  expect_type(info, "list")
  expect_true(file.exists(info$path))

  # Test with_formatter
  result <- with_formatter(42, formatter, info)
  expect_equal(result, 42)

  # Test close_formatter
  output <- close_formatter(formatter, info)
  expect_type(output, "raw")
  expect_false(file.exists(info$path)) # File should be removed

  # Test with non-device formatter
  regular_formatter <- function(x) x
  expect_null(init_formatter(regular_formatter))
  expect_null(close_formatter(regular_formatter, NULL))
  expect_null(clean_formatter(regular_formatter, NULL))
  expect_equal(with_formatter(42, regular_formatter, NULL), 42)
})
