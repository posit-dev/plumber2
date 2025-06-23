test_that("docs get generated correctly", {
  papi <- api("annotations/request_handlers.R")
  doc <- private(papi, "OPENAPI")
  expect_named(
    doc$paths,
    c("/hello/{test}/", "/hello/", "/plot/", "/type/{param}/")
  )
  expect_equal(doc$paths[[1]]$get$summary, "A get endpoint")
  expect_equal(
    doc$paths[[1]]$get$description,
    "description\n\nmore description"
  )
  expect_equal(doc$paths[[1]]$get$operationId, "/hello/{test}/-get")
  expect_equal(doc$paths[[1]]$get$tags, list("my_tag"))

  # Parameters
  expect_length(doc$paths[[1]]$get$parameters, 3)
  expect_equal(doc$paths[[1]]$get$parameters[[1]]$name, "test")
  expect_equal(doc$paths[[1]]$get$parameters[[1]]$`in`, "path")
  expect_equal(doc$paths[[1]]$get$parameters[[1]]$description, "test")
  expect_true(doc$paths[[1]]$get$parameters[[1]]$required)
  expect_equal(doc$paths[[1]]$get$parameters[[1]]$schema, list(type = "string"))
  expect_equal(doc$paths[[1]]$get$parameters[[1]]$style, "simple")
  expect_equal(doc$paths[[1]]$get$parameters[[2]]$name, "test2")
  expect_equal(doc$paths[[1]]$get$parameters[[2]]$`in`, "query")
  expect_equal(doc$paths[[1]]$get$parameters[[2]]$description, "test2 and more")
  expect_false(doc$paths[[1]]$get$parameters[[2]]$required)
  expect_equal(
    doc$paths[[1]]$get$parameters[[2]]$schema,
    list(type = "array", items = list(type = "integer"))
  )
  expect_equal(doc$paths[[1]]$get$parameters[[2]]$style, "form")
  expect_equal(doc$paths[[1]]$get$parameters[[3]]$name, "test3")
  expect_equal(doc$paths[[1]]$get$parameters[[3]]$`in`, "query")
  expect_equal(doc$paths[[1]]$get$parameters[[3]]$description, "test3")
  expect_true(doc$paths[[1]]$get$parameters[[3]]$required)
  expect_equal(doc$paths[[1]]$get$parameters[[3]]$schema, list(type = "number"))
  expect_equal(doc$paths[[1]]$get$parameters[[3]]$style, "form")

  # Response
  expect_equal(doc$paths[[1]]$get$responses$`200`$description, "test4")
  expect_equal(doc$paths[[1]]$get$responses$`404`$description, "test5")
  expect_named(
    doc$paths[[1]]$get$responses$`200`$content,
    c(
      "text/csv",
      "application/json",
      "text/html",
      "application/rds",
      "text/tab-separated-values",
      "text/xml",
      "text/plain",
      "text/yaml"
    )
  )
  expect_equal(
    doc$paths[[1]]$get$responses$`200`$content[[1]]$schema,
    list(
      type = "object",
      properties = list(
        mpg = list(type = "array", items = list(type = "number")),
        cyl = list(type = "array", items = list(type = "integer")),
        disp = list(type = "array", items = list(type = "number"))
      ),
      required = character()
    )
  )

  # body
  expect_true(all(
    names(doc$paths[[2]]$post$requestBody$content) %in%
      c(registry$parsers$json$types, registry$parsers$yaml$types)
  ))
  expect_equal(
    doc$paths[[2]]$post$requestBody$content[[1]]$schema,
    list(
      type = structure("object", class = "AsIs"),
      properties = list(
        test = list(
          type = "integer",
          description = "an integer"
        ),
        test2 = list(
          type = "array",
          items = list(type = "string"),
          description = "an array of strings"
        )
      )
    )
  )
  expect_equal(
    doc$paths[[4]]$post$parameters[[1]],
    list(
      name = "param",
      `in` = "path",
      description = "",
      required = TRUE,
      schema = list(type = "string", enum = c("a", "b", "c")),
      style = "simple"
    )
  )
  expect_equal(
    doc$paths[[4]]$post$parameters[[2]],
    list(
      name = "required",
      `in` = "query",
      description = "",
      required = TRUE,
      schema = list(type = "string"),
      style = "form"
    )
  )
  expect_equal(
    doc$paths[[4]]$post$parameters[[3]],
    list(
      name = "range",
      `in` = "query",
      description = "",
      required = FALSE,
      schema = list(type = "integer", minimum = 2, maximum = 9),
      style = "form"
    )
  )
  expect_equal(
    doc$paths[[4]]$post$parameters[[4]],
    list(
      name = "default",
      `in` = "query",
      description = "",
      required = FALSE,
      schema = list(
        type = "string",
        default = "test"
      ),
      style = "form"
    )
  )
  expect_equal(
    doc$paths[[4]]$post$parameters[[5]],
    list(
      name = "array",
      `in` = "query",
      description = "",
      required = FALSE,
      schema = list(type = "array", items = list(type = "string")),
      style = "form"
    )
  )
  expect_equal(
    doc$paths[[4]]$post$parameters[[6]],
    list(
      name = "regex",
      `in` = "query",
      description = "",
      required = FALSE,
      schema = list(type = "string", pattern = "\\d-\\d{2}"),
      style = "form"
    )
  )

  expect_equal(
    doc$paths[[4]]$post$requestBody$content[[1]]$schema,
    list(
      type = structure("object", class = "AsIs"),
      properties = list(
        str = list(type = "string", description = ""),
        upper = list(
          type = "number",
          maximum = 10,
          default = 5,
          description = ""
        ),
        today = list(type = "string", format = "date", description = ""),
        now = list(type = "string", format = "date-time", description = ""),
        data = list(type = "string", format = "byte", description = ""),
        flag = list(type = "boolean", description = "")
      )
    )
  )
})

test_that("handlers gets constructed correctly", {
  papi <- api("annotations/request_handlers.R")
  papi |> api_logger(function(event, message, request, ...) NULL)

  req <- fiery::fake_request("http://127.0.0.1:8080/hello/you")
  res <- papi$test_request(req)

  expect_equal(res$status, 200L)
  expect_equal(
    res$headers$`content-disposition`,
    "attachment; filename=\"testfile\""
  )
  expect_equal(res$headers$`content-type`, "text/csv")
  expect_equal(res$body, readr::format_csv(mtcars[, 1:3], col_names = FALSE))

  req <- fiery::fake_request(
    "http://127.0.0.1:8080/hello/you",
    headers = list("Accept" = "application/dart")
  )
  res <- papi$test_request(req)
  expect_equal(res$status, 406L)
  expect_equal(res$headers$`content-type`, "application/problem+json")
  expect_equal(
    jsonlite::fromJSON(res$body)$detail,
    "Only text/csv, application/json, text/html, application/rds, text/tab-separated-values, text/xml, text/plain, or text/yaml content types supported."
  )

  good_body <- list(test = 2, test2 = letters[1:4])
  bad_body <- list(test = "fail", test2 = letters[1:4])

  req <- fiery::fake_request(
    "http://127.0.0.1:8080/hello/",
    "post",
    content = jsonlite::toJSON(good_body),
    headers = list("Content_Type" = "application/json")
  )
  res <- papi$test_request(req)
  expect_equal(res$headers$`content-type`, "application/json")
  expect_equal(res$body, unclass(jsonlite::toJSON(good_body)))

  req <- fiery::fake_request(
    "http://127.0.0.1:8080/hello/",
    "post",
    content = jsonlite::toJSON(bad_body),
    headers = list("Content_Type" = "application/json")
  )
  res <- papi$test_request(req)
  expect_equal(res$status, 400L)
  expect_equal(res$headers$`content-type`, "application/problem+json")
  expect_equal(
    jsonlite::fromJSON(res$body)$detail,
    "`test` must match the type {\"type\":\"integer\",\"description\":\"an integer\"}"
  )

  req <- fiery::fake_request("http://127.0.0.1:8080/plot/")
  res <- papi$test_request(req)
  expect_true(promises::is.promise(res))
  res <- extract(res)
  expect_equal(res$status, 200L)
  expect_equal(res$headers$`content-type`, "image/svg+xml")
  svg_file <- tempfile(fileext = ".svg")
  writeLines(rawToChar(res$body), svg_file, sep = "\n")
  expect_snapshot_file(svg_file, "async_svg")

  req <- fiery::fake_request("http://127.0.0.1:8080/header/")
  res <- papi$test_header(req)
  expect_equal(res$status, 406L)

  req <- fiery::fake_request("http://127.0.0.1:8080/type/a/", "post")
  res <- papi$test_request(req)
  expect_equal(
    jsonlite::fromJSON(res$body)$detail,
    "`required` is a required query parameter but is missing"
  )

  req <- fiery::fake_request("http://127.0.0.1:8080/type/d/", "post")
  res <- papi$test_request(req)
  expect_equal(
    jsonlite::fromJSON(res$body)$detail,
    "`param` must be one of \"a\", \"b\", or \"c\""
  )

  req <- fiery::fake_request(
    "http://127.0.0.1:8080/type/a/?required=test",
    "post"
  )
  res <- papi$test_request(req)
  expect_equal(
    unserialize(res$body),
    list(
      param = factor("a", levels = c("a", "b", "c")),
      query = list(required = "test", default = "test"),
      body = NULL
    )
  )
})
