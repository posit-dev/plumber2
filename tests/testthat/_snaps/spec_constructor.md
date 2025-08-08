# openapi function creates valid OpenAPI object

    Code
      openapi(info = openapi_info(title = "Test", version = "1.0.0"), paths = "invalid")
    Condition
      Error in `openapi()`:
      ! `paths` must be a named list

---

    Code
      openapi()
    Condition
      Error in `openapi_info()`:
      ! `title` and `version` must be provided

# openapi_info function creates valid info object

    Code
      openapi_info()
    Condition
      Error in `openapi_info()`:
      ! `title` and `version` must be provided

# openapi_license function creates valid license object

    Code
      openapi_license(url = "https://example.com")
    Condition
      Error in `openapi_license()`:
      ! `name` must be provided

# openapi_operation function creates valid operation object

    Code
      openapi_operation(responses = list("response"))
    Condition
      Error in `openapi_operation()`:
      ! `responses` must be a named list

# openapi_parameter function creates valid parameter object

    Code
      openapi_parameter()
    Condition
      Error in `openapi_parameter()`:
      ! `name` must be provided

---

    Code
      openapi_parameter(name = "param1", location = "path", required = FALSE)
    Condition
      Error in `openapi_parameter()`:
      ! path parameters have to be required

---

    Code
      openapi_parameter(name = "param1", location = "query", schema = openapi_schema(
        character()), content = openapi_content(`text/plain` = openapi_schema(
        character())))
    Condition
      Error in `openapi_parameter()`:
      ! Only one of `schema` and `content` must be used

# openapi_schema function creates valid schema object for list class

    Code
      openapi_schema(list(1, "test"))
    Condition
      Error in `openapi_schema()`:
      ! Un-named lists must only contain a single element

---

    Code
      openapi_schema(list(field1 = 1), required = c("field2"))
    Condition
      Error in `openapi_schema()`:
      ! `required` must all be elements of `x`

# openapi_content function creates valid content object

    Code
      openapi_content(invalid_name = openapi_schema(character()))
    Condition
      Error in `openapi_content()`:
      ! All arguments to `openapi_content()` must be named with a mime type

# openapi_request_body function creates valid request body object

    Code
      openapi_request_body(description = "Test request body")
    Condition
      Error in `openapi_request_body()`:
      ! `content` must be provided

---

    Code
      openapi_request_body(required = TRUE)
    Condition
      Error in `openapi_request_body()`:
      ! `content` must be provided

# openapi_response function creates valid response object

    Code
      openapi_response()
    Condition
      Error in `openapi_response()`:
      ! `description` must be provided

---

    Code
      openapi_response(description = "Test response", headers = list("header"))
    Condition
      Error in `openapi_response()`:
      ! `headers` must be a named list

# openapi_tag function creates valid tag object

    Code
      openapi_tag(description = "Test tag")
    Condition
      Error in `openapi_tag()`:
      ! `name` must be provided

# require_input function validates input presence

    Code
      require_input(test = character())
    Condition
      Error:
      ! `test` must be provided

---

    Code
      require_input(test1 = character(), test2 = character())
    Condition
      Error:
      ! `test1` and `test2` must be provided

---

    Code
      require_input(valid = "value", invalid = character())
    Condition
      Error:
      ! `invalid` must be provided

