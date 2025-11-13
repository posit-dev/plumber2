# Add a handler for a request header

These handlers are called before the request body has been received and
lets you preemptively reject requests before receiving their full
content. If the handler does not return
[Next](https://plumber2.posit.co/reference/Next.md) then the request
will be returned at once. Most of your logic, however, will be in the
main handlers and you are asked to consult the
[api_request_handlers](https://plumber2.posit.co/reference/api_request_handlers.md)
docs for in-depth details on how to use request handlers in general.

## Usage

``` r
api_get_header(
  api,
  path,
  handler,
  serializers = NULL,
  parsers = NULL,
  use_strict_serializer = FALSE,
  download = FALSE,
  async = FALSE,
  then = NULL,
  route = NULL
)

api_head_header(
  api,
  path,
  handler,
  serializers = NULL,
  parsers = NULL,
  use_strict_serializer = FALSE,
  download = FALSE,
  async = FALSE,
  then = NULL,
  route = NULL
)

api_post_header(
  api,
  path,
  handler,
  serializers = NULL,
  parsers = NULL,
  use_strict_serializer = FALSE,
  download = FALSE,
  async = FALSE,
  then = NULL,
  route = NULL
)

api_put_header(
  api,
  path,
  handler,
  serializers = NULL,
  parsers = NULL,
  use_strict_serializer = FALSE,
  download = FALSE,
  async = FALSE,
  then = NULL,
  route = NULL
)

api_delete_header(
  api,
  path,
  handler,
  serializers = NULL,
  parsers = NULL,
  use_strict_serializer = FALSE,
  download = FALSE,
  async = FALSE,
  then = NULL,
  route = NULL
)

api_connect_header(
  api,
  path,
  handler,
  serializers = NULL,
  parsers = NULL,
  use_strict_serializer = FALSE,
  download = FALSE,
  async = FALSE,
  then = NULL,
  route = NULL
)

api_options_header(
  api,
  path,
  handler,
  serializers = NULL,
  parsers = NULL,
  use_strict_serializer = FALSE,
  download = FALSE,
  async = FALSE,
  then = NULL,
  route = NULL
)

api_trace_header(
  api,
  path,
  handler,
  serializers = NULL,
  parsers = NULL,
  use_strict_serializer = FALSE,
  download = FALSE,
  async = FALSE,
  then = NULL,
  route = NULL
)

api_patch_header(
  api,
  path,
  handler,
  serializers = NULL,
  parsers = NULL,
  use_strict_serializer = FALSE,
  download = FALSE,
  async = FALSE,
  then = NULL,
  route = NULL
)

api_any_header(
  api,
  path,
  handler,
  serializers = NULL,
  parsers = NULL,
  use_strict_serializer = FALSE,
  download = FALSE,
  async = FALSE,
  then = NULL,
  route = NULL
)
```

## Arguments

- api:

  A plumber2 api object to add the handler to

- path:

  A string giving the path the handler responds to. See Details

- handler:

  A handler function to call when a request is matched to the path

- serializers:

  A named list of serializers that can be used to format the response
  before sending it back to the client. Which one is selected is based
  on the request `Accept` header. See
  [`get_serializers()`](https://plumber2.posit.co/reference/register_serializer.md)
  for a helper to construct this

- parsers:

  A named list of parsers that can be used to parse the request body
  before passing it in as the `body` argument. Which one is selected is
  based on the request `Content-Type` header. See
  [`get_parsers()`](https://plumber2.posit.co/reference/register_parser.md)
  for a helper to construct this

- use_strict_serializer:

  By default, if a serializer that respects the requests `Accept` header
  cannot be found, then the first of the provided ones are used. Setting
  this to `TRUE` will instead send back a `406 Not Acceptable` response

- download:

  Should the response mark itself for download instead of being shown
  inline? Setting this to `TRUE` will set the `Content-Disposition`
  header in the response to `attachment`. Setting it to a string is
  equivalent to setting it to `TRUE` but will in addition also set the
  default filename of the download to the string value

- async:

  If `FALSE` create a regular handler. If `TRUE`, use the default async
  evaluator to create an async handler. If a string, the async evaluator
  registered to that name is used. If a function is provided then this
  is used as the async evaluator. See the *Async* section for more
  detail

- then:

  A list of function to be called once the async handler is done. The
  functions will be chained using
  [`promises::then()`](https://rstudio.github.io/promises/reference/then.html).
  See the *Async* section for more detail

- route:

  The route this handler should be added to. Defaults to the last route
  in the stack. If the route does not exist it will be created as the
  last route in the stack

## Value

These functions return the `api` object allowing for easy chaining with
the pipe

## Using annotation

Adding request header handler is done in the same way as for [standard
request
handlers](https://plumber2.posit.co/reference/api_request_handlers.md).
The only difference is that you include a `@header` tag as well. It is
not normal to document header requests as they usually exist as internal
controls. You can add `@noDoc` to avoid generating OpenAPI docs for the
handler

    #* A header handler authorizing users
    #*
    #* @get /*
    #*
    #* @header
    #* @noDoc
    function(client_id, response) {
      if (user_is_allowed(username)) {
        Next
      } else {
        response$status <- 404L
        Break
      }
    }

## See also

Other Request Handlers:
[`api_request_handlers`](https://plumber2.posit.co/reference/api_request_handlers.md)

## Examples

``` r
# Simple size limit (better to use build-in functionality)
api() |>
  api_post_header(
    "/*",
    function(request, response) {
      if (request$get_header("content-type") > 1024) {
        response$status <- 413L
        Break
      } else {
        Next
      }
    }
  )
#> Creating default route in header router
#> ── A plumber server ────────────────────────────────────────────────────────────
#> Serving on http://127.0.0.1:8080
#> Currently not running

```
