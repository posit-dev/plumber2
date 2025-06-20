#' Configure your API for serving documentation for itself
#'
#' The [OpenAPI standard](https://www.openapis.org) offers a way to describe the
#' various endpoints of your api in machine- and human-readable way. On top of
#' this, various solutions have been build to generate online documentation of
#' the API based on a provided OpenAPI spec. plumber2 offers support for
#' [RapiDoc](https://rapidocweb.com), [Redoc](https://redocly.com/redoc), and
#' [Swagger](https://swagger.io) as a UI frontend for the documentation and will
#' also generate the spec for you based on the tags in parsed files. If you are
#' creating your API programmatically or you wish to add to the autogenerated
#' docs you can add docs manually, either when adding a handler (using the `doc`
#' argument), or with the `api_doc_add()` function
#'
#' # Using annotation
#' When using annotated route files documentation is automatically generated
#' based on the annotation. The following tags will contribute to documentation:
#'
#' * `@title`
#' * `@description`
#' * `@details`
#' * `@tos`
#' * `@license`
#' * `@contact`
#' * `@tag`
#' * `@param`
#' * `@query`
#' * `@body`
#' * `@response`
#' * `@parsers`
#' * `@serializers`
#'
#' Documentation is only generated for annotations related to global
#' documentation (a block followed by the `"_API"` sentinel), request handlers
#' (a block including one of `@get`, `@head`, `@post`, `@put`, `@delete`,
#' `@connect`, `@options`, `@trace`, `@patch`, or `@any`), or report generation
#' (a block including `@report`)
#'
#' @param api A plumber2 api object to add docs or doc settings to
#' @inheritParams api
#' @param doc A list with the OpenAPI documentation, usually constrcuted
#' with [one of the helper functions][openapi]
#' @param overwrite Logical. Should already existing documentation be
#' removed or should it be merged together with `doc`
#' @param subset A character vector giving the path to the subset of the
#' docs to assign `doc` to
#'
#' @return These functions return the `api` object allowing for easy chaining
#' with the pipe
#'
#' @rdname api_docs
#' @name api_docs
#'
#' @examples
#' # Serve the docs from a different path
#' api() |>
#'   api_doc_setting(doc_path = "__man__")
#'
#' # Add documentation to the api programmatically
#' api() |>
#'   api_doc_add(openapi(
#'     info = openapi_info(
#'       title = "My awesome api",
#'       version = "1.0.0"
#'     )
#'   ))
#'
#' # Add documentation to a subset of the docs
#' api() |>
#'   api_doc_add(
#'     openapi_operation(
#'       summary = "Get the current date",
#'       responses = list(
#'         "200" = openapi_response(
#'           description = "Current Date",
#'           content = openapi_content(
#'             "text/plain" = openapi_schema(character())
#'           )
#'         )
#'       )
#'     ),
#'     subset = c("paths", "/date", "get")
#'   )
#'
NULL

#' @rdname api_docs
#' @export
#'
api_doc_setting <- function(api, doc_type, doc_path) {
  if (!missing(doc_type)) {
    api$doc_type <- doc_type
  }
  if (!missing(doc_path)) {
    api$doc_path <- doc_path
  }
  api
}

#' @rdname api_docs
#' @export
#'
api_doc_add <- function(api, doc, overwrite = FALSE, subset = NULL) {
  api$add_api_doc(doc, overwrite, subset)
  api
}
