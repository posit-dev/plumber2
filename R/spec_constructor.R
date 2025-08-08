#' Construct OpenAPI specifications
#'
#' These helper functions aid in constructing OpenAPI compliant specifications
#' for your API. The return simple lists and you may thus forego these helpers
#' and instead construct it all manually (or import it from a json or yaml
#' file). The purpose of these helpers is mainly in basic input checking and for
#' documenting the structure. Read more about the spec at
#' <https://spec.openapis.org/oas/v3.0.0.html>
#'
#' @param openapi The OpenAPI version the spec adheres to. The helpers assume
#' 3.0.0 so this is also the default value
#' @param info A list as constructed by `openapi_info()`
#' @param paths A named list. The names correspond to endpoints and the elements
#' are lists as constructed by `openapi_path()`
#' @param tags For `openapi()` a list with elements corresponding to the value
#' constructed by `openapi_tag()`. For `openapi_operation()` a character vector
#' or a list of strings
#'
#' @return A list
#'
#' @export
#'
#' @examples
#' # Create docs for an API with a single endpoint
#' doc <- openapi(
#'   info = openapi_info(
#'     title = "My awesome api",
#'     version = "1.0.0"
#'   ),
#'   paths = list(
#'     "/hello/{name}" = openapi_path(
#'       get = openapi_operation(
#'         summary = "Get a greeting",
#'         parameters = list(
#'           openapi_parameter(
#'             name = "name",
#'             location = "path",
#'             description = "Your name",
#'             schema = openapi_schema(character())
#'           )
#'         ),
#'         responses = list(
#'           "200" = openapi_response(
#'             description = "a kind message",
#'             content = openapi_content(
#'               "text/plain" = openapi_schema(character())
#'             )
#'           )
#'         )
#'       )
#'     )
#'   )
#' )
#'
#' # Add it to an api
#' api() |>
#'   api_doc_add(doc)
#'
#'
openapi <- function(
  openapi = "3.0.0",
  info = openapi_info(),
  paths = list(),
  tags = list()
) {
  require_input(openapi = openapi, info = info)
  if (!(is_list(paths) && is_named2(paths))) {
    cli::cli_abort("{.arg paths} must be a named list")
  }
  structure(
    c(
      compact(list(
        openapi = openapi,
        info = info,
        tags = tags
      )),
      list(paths = paths)
    ),
    class = "plumber2_openapi"
  )
}

#' @rdname openapi
#' @export
#'
#' @param title A string giving the title of the API
#' @param description A longer description of the respective element. May use
#' markdown
#' @param terms_of_service A URL to the terms of service for the API
#' @param contact A list as constructed by `openapi_contact()`
#' @param license A list as constructed by `openapi_license()`
#' @param version A string giving the version of the API
#'
openapi_info <- function(
  title = character(),
  description = character(),
  terms_of_service = character(),
  contact = openapi_contact(),
  license = openapi_license(),
  version = character()
) {
  require_input(title = title, version = version)
  compact(list(
    title = title,
    description = description,
    termsOfService = terms_of_service,
    contact = contact,
    license = license,
    version = version
  ))
}

#' @rdname openapi
#' @export
#'
#' @param name The name of the contact, license, parameter, or tag
#' @param url The URL pointing to the contact or license information
#' @param email An email address for the contact
#'
openapi_contact <- function(
  name = character(),
  url = character(),
  email = character()
) {
  compact(list(
    name = name,
    url = url,
    email = email
  ))
}

#' @rdname openapi
#' @export
#'
openapi_license <- function(
  name = character(),
  url = character()
) {
  if (length(url) != 0) {
    require_input(name = name)
  }
  compact(list(
    name = name,
    url = url
  ))
}

#' @rdname openapi
#' @export
#'
#' @param summary A one-sentence summary of the path or operation
#' @param get,put,post,delete,options,head,patch,trace A list describing the
#' specific HTTP method when requested for the path, as constructed by
#' `openapi_operation()`
#' @param parameters A list of parameters that apply to the path and/or
#' operation. If this is given in `openapi_path()` it is inherited by all its
#' operations.
#'
openapi_path <- function(
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
) {
  compact(list(
    summary = summary,
    description = description,
    get = get,
    put = put,
    post = post,
    delete = delete,
    options = options,
    head = head,
    patch = patch,
    trace = trace,
    parameters = parameters
  ))
}

#' @rdname openapi
#' @export
#'
#' @param operation_id A unique string that identifies this operation in the API
#' @param request_body A list as constructed by `openapi_request_body()`
#' @param responses A named list with the name corresponding to the response
#' code and the elements being lists as constructed by `openapi_response()`
#'
openapi_operation <- function(
  summary = character(),
  description = character(),
  operation_id = character(),
  parameters = list(),
  request_body = openapi_request_body(),
  responses = list(),
  tags = character()
) {
  if (!(is_list(responses) && is_named2(responses))) {
    cli::cli_abort("{.arg responses} must be a named list")
  }

  op <- compact(list(
    summary = summary,
    description = description,
    operationId = operation_id,
    parameters = parameters,
    requestBody = request_body,
    responses = responses,
    tags = as.list(tags)
  ))
  if (length(op) != 0 && is.null(op$response)) {
    op$response <- list()
  }
  op
}

#' @rdname openapi
#' @export
#'
#' @param location Where this parameter is coming from. Either `"path"`,
#' `"query"`, `"header"`, or `"cookie"`.
#' @param required For `openapi_parameter` a boolean indicating if this is a
#' required parameter (`"path"` parameters are always required). For
#' `openapi_schema()` a character vector naming the required properties of an
#' object.
#' @param schema A description of the data as constructed by `openapi_schema`
#' @param content A list as constructed by `openapi_content()`.
#' @param ... Further named arguments to be added to the element. For
#' `openapi_content()` named elements as constructed by `openapi_schema()`
#'
openapi_parameter <- function(
  name = character(),
  location = c("path", "query", "header", "cookie"),
  description = character(),
  required = logical(),
  schema = openapi_schema(),
  content = openapi_content(),
  ...
) {
  require_input(name = name, location = location)
  location <- arg_match(location)
  if (location == "path") {
    if (isFALSE(required)) {
      cli::cli_abort("path parameters have to be required")
    }
    required <- TRUE
  }
  if (length(schema) != 0 && length(content) != 0) {
    cli::cli_abort("Only one of {.arg schema} and {.arg content} must be used")
  }
  res <- compact(list(
    name = name,
    `in` = location,
    description = description,
    required = required,
    schema = schema,
    content = content,
    ...
  ))
  if (is.null(res$schema) && is.null(res$content)) {
    res$schema <- list()
  }
  res
}

#' @rdname openapi
#' @export
#'
openapi_header <- function(
  description = character(),
  schema = openapi_schema()
) {
  compact(list(
    description = description,
    schema = schema
  ))
}

#' @rdname openapi
#' @export
#'
#' @param x An R object corresponding to the type of the schema. Supported types
#' are:
#' * `integer`: Will signal `type: integer`
#' * `numeric`: Will signal `type: number`
#' * `character`: Will signal `type: string`
#' * `factor`: Will signal `type: string` and `enum` set the factor levels
#' * `raw`: Will signal `type:string; format: binary`
#' * `Date`: Will signal `type:string; format: date`
#' * `POSIXt`: Will signal `type:string; format: date-time`
#' * `list`: If unnamed it must be a one-length list and will signal
#'   `type: array` and `items` set to the schema of its element. If named it
#'   will signal `type: object` and `properties` set to the schema of each
#'   element.
#' * `AsIs`: Will signal a `type` equivalent to the value of the input (must be
#'   a string)
#' @param default A default value for the parameter. Must be reconsilable with
#' the type of `x`
#' @param min,max Bounds for the value of the parameter
openapi_schema <- function(
  x,
  default = NULL,
  min = NULL,
  max = NULL,
  ...,
  required = NULL
) {
  UseMethod("openapi_schema")
}
#' @export
openapi_schema.NULL <- function(x, ...) {
  list()
}
#' @export
openapi_schema.AsIs <- function(x, default = NULL, ...) {
  check_string(x)
  class(x) <- setdiff(class(x), "AsIs")
  compact(list(
    type = x,
    default = default,
    ...
  ))
}
#' @export
openapi_schema.numeric <- function(
  x,
  default = NULL,
  min = NULL,
  max = NULL,
  ...
) {
  type <- switch(typeof(x), integer = "integer", double = "number")
  compact(list(
    type = type,
    default = default,
    minimum = min,
    maximum = max
  ))
}
#' @export
openapi_schema.character <- function(x, default = NULL, ...) {
  compact(list(
    type = "string",
    default = default
  ))
}
#' @export
openapi_schema.raw <- function(x, default = NULL, ...) {
  compact(list(
    type = "string",
    format = "binary",
    default = default
  ))
}
#' @export
openapi_schema.Date <- function(x, default = NULL, ...) {
  compact(list(
    type = "string",
    format = "date",
    default = default
  ))
}
#' @export
openapi_schema.POSIXt <- function(x, default = NULL, ...) {
  compact(list(
    type = "string",
    format = "date-time",
    default = default
  ))
}
#' @export
openapi_schema.factor <- function(x, default = NULL, ...) {
  compact(list(
    type = "string",
    enum = levels(x),
    default = default
  ))
}
#' @export
openapi_schema.list <- function(x, default = NULL, ..., required = NULL) {
  if (is_named(x)) {
    if (!all(required %in% names(x))) {
      cli::cli_abort("{.arg required} must all be elements of {.arg x}")
    }
    compact(list(
      type = "object",
      properties = lapply(x, openapi_schema),
      required = required,
      default = default
    ))
  } else if (length(x) == 1) {
    compact(list(
      type = "array",
      items = openapi_schema(x[[1]]),
      default = default
    ))
  } else {
    cli::cli_abort("Un-named lists must only contain a single element")
  }
}

#' @rdname openapi
#' @export
#'
openapi_content <- function(...) {
  content <- list2(...)
  if (length(content) == 0) {
    return(content)
  }
  if (
    !is_named(content) ||
      !all(stringi::stri_detect_regex(names(content), "^.*/.*$"))
  ) {
    cli::cli_abort(
      "All arguments to {.fun openapi_content} must be named with a mime type"
    )
  }
  lapply(content, function(x) {
    list(schema = x)
  })
}

#' @rdname openapi
#' @export
#'
openapi_request_body <- function(
  description = character(),
  content = openapi_content(),
  required = logical()
) {
  if (
    (length(description) != 0 || length(required) != 0) && length(content) == 0
  ) {
    cli::cli_abort("{.arg content} must be provided")
  }
  compact(list(
    description = description,
    content = content,
    required = required
  ))
}

#' @rdname openapi
#' @export
#'
#' @param headers A named list with names corresponding to headers and elements
#' as constructed by `openapi_header()`
openapi_response <- function(
  description = character(),
  content = openapi_content(),
  headers = list()
) {
  require_input(description = description)
  if (!(is_list(headers) && is_named2(headers))) {
    cli::cli_abort("{.arg headers} must be a named list")
  }
  compact(list(
    description = description,
    content = content,
    headers = headers
  ))
}

#' @rdname openapi
#' @export
#'
openapi_tag <- function(name = character(), description = character()) {
  if (length(description) != 0 && length(name) == 0) {
    cli::cli_abort("{.arg name} must be provided")
  }
  compact(list(
    name = name,
    description = description
  ))
}

require_input <- function(..., call = caller_env()) {
  inputs <- list2(...)
  missing <- lengths(inputs) == 0
  if (any(missing)) {
    cli::cli_abort(
      "{.arg {names(inputs)[missing]}} must be provided",
      call = call
    )
  }
}
