# Construct OpenAPI specifications

These helper functions aid in constructing OpenAPI compliant
specifications for your API. The return simple lists and you may thus
forego these helpers and instead construct it all manually (or import it
from a json or yaml file). The purpose of these helpers is mainly in
basic input checking and for documenting the structure. Read more about
the spec at <https://spec.openapis.org/oas/v3.0.0.html>

## Usage

``` r
openapi(
  openapi = "3.0.0",
  info = openapi_info(),
  paths = list(),
  tags = list()
)

openapi_info(
  title = character(),
  description = character(),
  terms_of_service = character(),
  contact = openapi_contact(),
  license = openapi_license(),
  version = character()
)

openapi_contact(name = character(), url = character(), email = character())

openapi_license(name = character(), url = character())

openapi_path(
  summary = character(),
  description = character(),
  get = openapi_operation(),
  put = openapi_operation(),
  post = openapi_operation(),
  delete = openapi_operation(),
  options = openapi_operation(),
  head = openapi_operation(),
  patch = openapi_operation(),
  trace = openapi_operation(),
  parameters = list()
)

openapi_operation(
  summary = character(),
  description = character(),
  operation_id = character(),
  parameters = list(),
  request_body = openapi_request_body(),
  responses = list(),
  tags = character()
)

openapi_parameter(
  name = character(),
  location = c("path", "query", "header", "cookie"),
  description = character(),
  required = logical(),
  schema = openapi_schema(),
  content = openapi_content(),
  ...
)

openapi_header(description = character(), schema = openapi_schema())

openapi_schema(x, default = NULL, min = NULL, max = NULL, ..., required = NULL)

openapi_content(...)

openapi_request_body(
  description = character(),
  content = openapi_content(),
  required = logical()
)

openapi_response(
  description = character(),
  content = openapi_content(),
  headers = list()
)

openapi_tag(name = character(), description = character())
```

## Arguments

- openapi:

  The OpenAPI version the spec adheres to. The helpers assume 3.0.0 so
  this is also the default value

- info:

  A list as constructed by `openapi_info()`

- paths:

  A named list. The names correspond to endpoints and the elements are
  lists as constructed by `openapi_path()`

- tags:

  For `openapi()` a list with elements corresponding to the value
  constructed by `openapi_tag()`. For `openapi_operation()` a character
  vector or a list of strings

- title:

  A string giving the title of the API

- description:

  A longer description of the respective element. May use markdown

- terms_of_service:

  A URL to the terms of service for the API

- contact:

  A list as constructed by `openapi_contact()`

- license:

  A list as constructed by `openapi_license()`

- version:

  A string giving the version of the API

- name:

  The name of the contact, license, parameter, or tag

- url:

  The URL pointing to the contact or license information

- email:

  An email address for the contact

- summary:

  A one-sentence summary of the path or operation

- get, put, post, delete, options, head, patch, trace:

  A list describing the specific HTTP method when requested for the
  path, as constructed by `openapi_operation()`

- parameters:

  A list of parameters that apply to the path and/or operation. If this
  is given in `openapi_path()` it is inherited by all its operations.

- operation_id:

  A unique string that identifies this operation in the API

- request_body:

  A list as constructed by `openapi_request_body()`

- responses:

  A named list with the name corresponding to the response code and the
  elements being lists as constructed by `openapi_response()`

- location:

  Where this parameter is coming from. Either `"path"`, `"query"`,
  `"header"`, or `"cookie"`.

- required:

  For `openapi_parameter` a boolean indicating if this is a required
  parameter (`"path"` parameters are always required). For
  `openapi_schema()` a character vector naming the required properties
  of an object.

- schema:

  A description of the data as constructed by `openapi_schema`

- content:

  A list as constructed by `openapi_content()`.

- ...:

  Further named arguments to be added to the element. For
  `openapi_content()` named elements as constructed by
  `openapi_schema()`

- x:

  An R object corresponding to the type of the schema. Supported types
  are:

  - `integer`: Will signal `type: integer`

  - `numeric`: Will signal `type: number`

  - `character`: Will signal `type: string`

  - `factor`: Will signal `type: string` and `enum` set the factor
    levels

  - `raw`: Will signal `type:string; format: binary`

  - `Date`: Will signal `type:string; format: date`

  - `POSIXt`: Will signal `type:string; format: date-time`

  - `list`: If unnamed it must be a one-length list and will signal
    `type: array` and `items` set to the schema of its element. If named
    it will signal `type: object` and `properties` set to the schema of
    each element.

  - `AsIs`: Will signal a `type` equivalent to the value of the input
    (must be a string)

- default:

  A default value for the parameter. Must be reconsilable with the type
  of `x`

- min, max:

  Bounds for the value of the parameter

- headers:

  A named list with names corresponding to headers and elements as
  constructed by `openapi_header()`

## Value

A list

## Examples

``` r
# Create docs for an API with a single endpoint
doc <- openapi(
  info = openapi_info(
    title = "My awesome api",
    version = "1.0.0"
  ),
  paths = list(
    "/hello/{name}" = openapi_path(
      get = openapi_operation(
        summary = "Get a greeting",
        parameters = list(
          openapi_parameter(
            name = "name",
            location = "path",
            description = "Your name",
            schema = openapi_schema(character())
          )
        ),
        responses = list(
          "200" = openapi_response(
            description = "a kind message",
            content = openapi_content(
              "text/plain" = openapi_schema(character())
            )
          )
        )
      )
    )
  )
)

# Add it to an api
api() |>
  api_doc_add(doc)
#> ── A plumber server ────────────────────────────────────────────────────────────
#> Serving on http://127.0.0.1:8080
#> Currently not running

```
