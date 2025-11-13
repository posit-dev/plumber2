# The Plumber2 Class

This class encapsulates all of the logic of a plumber2 api, and is what
gets passed around in the functional api of plumber2. The `Plumber2`
class is a subclass of the
[fiery::Fire](https://fiery.data-imaginist.com/reference/Fire.html)
class. Please consult the documentation for this for additional
information on what this type of server is capable of. Note that the
`Plumber2` objects are reference objects, meaning that any change to it
will change all instances of the object.

### Initialization

A new `Plumber2`-object is initialized using the `new()` method on the
generator:

|                         |
|-------------------------|
| `api <- Plumber2$new()` |

However, most users will use the functional api of the package and thus
construct one using
[`api()`](https://plumber2.posit.co/reference/api.md)

### Copying

As `Plumber2` objects are using reference semantics new copies of an api
cannot be made simply be assigning it to a new variable. If a true copy
of a `Plumber2` object is desired, use the `clone()` method.

## Super class

[`fiery::Fire`](https://fiery.data-imaginist.com/reference/Fire.html)
-\> `Plumber2`

## Active bindings

- `request_router`:

  The router handling requests

- `header_router`:

  The router handling partial requests (the request will pass through
  this router prior to reading in the body)

- `doc_type`:

  The type of API documentation to generate. Can be either `"rapidoc"`
  (the default), `"redoc"`, `"swagger"`, or `NULL` (equating to not
  generating API docs)

- `doc_path`:

  The URL path to serve the api documentation from

- `doc_args`:

  Further arguments to the documentation UI

## Methods

### Public methods

- [`Plumber2$new()`](#method-Plumber2-new)

- [`Plumber2$format()`](#method-Plumber2-format)

- [`Plumber2$ignite()`](#method-Plumber2-ignite)

- [`Plumber2$add_route()`](#method-Plumber2-add_route)

- [`Plumber2$request_handler()`](#method-Plumber2-request_handler)

- [`Plumber2$message_handler()`](#method-Plumber2-message_handler)

- [`Plumber2$redirect()`](#method-Plumber2-redirect)

- [`Plumber2$parse_file()`](#method-Plumber2-parse_file)

- [`Plumber2$add_api_doc()`](#method-Plumber2-add_api_doc)

- [`Plumber2$add_shiny()`](#method-Plumber2-add_shiny)

- [`Plumber2$add_report()`](#method-Plumber2-add_report)

- [`Plumber2$forward()`](#method-Plumber2-forward)

- [`Plumber2$add_auth_guard()`](#method-Plumber2-add_auth_guard)

- [`Plumber2$add_auth()`](#method-Plumber2-add_auth)

- [`Plumber2$clone()`](#method-Plumber2-clone)

Inherited methods

- [`fiery::Fire$async()`](https://fiery.data-imaginist.com/reference/Fire.html#method-async)
- [`fiery::Fire$attach()`](https://fiery.data-imaginist.com/reference/Fire.html#method-attach)
- [`fiery::Fire$close_ws_con()`](https://fiery.data-imaginist.com/reference/Fire.html#method-close_ws_con)
- [`fiery::Fire$delay()`](https://fiery.data-imaginist.com/reference/Fire.html#method-delay)
- [`fiery::Fire$exclude_static()`](https://fiery.data-imaginist.com/reference/Fire.html#method-exclude_static)
- [`fiery::Fire$extinguish()`](https://fiery.data-imaginist.com/reference/Fire.html#method-extinguish)
- [`fiery::Fire$get_data()`](https://fiery.data-imaginist.com/reference/Fire.html#method-get_data)
- [`fiery::Fire$has_plugin()`](https://fiery.data-imaginist.com/reference/Fire.html#method-has_plugin)
- [`fiery::Fire$header()`](https://fiery.data-imaginist.com/reference/Fire.html#method-header)
- [`fiery::Fire$is_running()`](https://fiery.data-imaginist.com/reference/Fire.html#method-is_running)
- [`fiery::Fire$log()`](https://fiery.data-imaginist.com/reference/Fire.html#method-log)
- [`fiery::Fire$off()`](https://fiery.data-imaginist.com/reference/Fire.html#method-off)
- [`fiery::Fire$on()`](https://fiery.data-imaginist.com/reference/Fire.html#method-on)
- [`fiery::Fire$reignite()`](https://fiery.data-imaginist.com/reference/Fire.html#method-reignite)
- [`fiery::Fire$remove_async()`](https://fiery.data-imaginist.com/reference/Fire.html#method-remove_async)
- [`fiery::Fire$remove_data()`](https://fiery.data-imaginist.com/reference/Fire.html#method-remove_data)
- [`fiery::Fire$remove_delay()`](https://fiery.data-imaginist.com/reference/Fire.html#method-remove_delay)
- [`fiery::Fire$remove_time()`](https://fiery.data-imaginist.com/reference/Fire.html#method-remove_time)
- [`fiery::Fire$resume()`](https://fiery.data-imaginist.com/reference/Fire.html#method-resume)
- [`fiery::Fire$safe_call()`](https://fiery.data-imaginist.com/reference/Fire.html#method-safe_call)
- [`fiery::Fire$send()`](https://fiery.data-imaginist.com/reference/Fire.html#method-send)
- [`fiery::Fire$serve_static()`](https://fiery.data-imaginist.com/reference/Fire.html#method-serve_static)
- [`fiery::Fire$set_client_id_converter()`](https://fiery.data-imaginist.com/reference/Fire.html#method-set_client_id_converter)
- [`fiery::Fire$set_data()`](https://fiery.data-imaginist.com/reference/Fire.html#method-set_data)
- [`fiery::Fire$set_logger()`](https://fiery.data-imaginist.com/reference/Fire.html#method-set_logger)
- [`fiery::Fire$start()`](https://fiery.data-imaginist.com/reference/Fire.html#method-start)
- [`fiery::Fire$stop()`](https://fiery.data-imaginist.com/reference/Fire.html#method-stop)
- [`fiery::Fire$test_header()`](https://fiery.data-imaginist.com/reference/Fire.html#method-test_header)
- [`fiery::Fire$test_message()`](https://fiery.data-imaginist.com/reference/Fire.html#method-test_message)
- [`fiery::Fire$test_request()`](https://fiery.data-imaginist.com/reference/Fire.html#method-test_request)
- [`fiery::Fire$test_websocket()`](https://fiery.data-imaginist.com/reference/Fire.html#method-test_websocket)
- [`fiery::Fire$time()`](https://fiery.data-imaginist.com/reference/Fire.html#method-time)
- [`fiery::Fire$trigger()`](https://fiery.data-imaginist.com/reference/Fire.html#method-trigger)

------------------------------------------------------------------------

### Method `new()`

Create a new `Plumber2` api

#### Usage

    Plumber2$new(
      host = get_opts("host", "127.0.0.1"),
      port = get_opts("port", 8080),
      doc_type = get_opts("docType", "rapidoc"),
      doc_path = get_opts("docPath", "__docs__"),
      reject_missing_methods = get_opts("rejectMissingMethods", FALSE),
      ignore_trailing_slash = get_opts("ignoreTrailingSlash", TRUE),
      max_request_size = get_opts("maxRequestSize"),
      shared_secret = get_opts("sharedSecret"),
      compression_limit = get_opts("compressionLimit", 1000),
      default_async = get_opts("async", "mirai"),
      env = caller_env()
    )

#### Arguments

- `host`:

  A string overriding the default host

- `port`:

  An port number overriding the default port

- `doc_type`:

  The type of API documentation to generate. Can be either `"rapidoc"`
  (the default), `"redoc"`, `"swagger"`, or `NULL` (equating to not
  generating API docs)

- `doc_path`:

  The URL path to serve the api documentation from

- `reject_missing_methods`:

  Should requests to paths that doesn't have a handler for the specific
  method automatically be rejected with a 405 Method Not Allowed
  response with the correct Allow header informing the client of the
  implemented methods. Assigning a handler to `"any"` for the same path
  at a later point will overwrite this functionality. Be aware that
  setting this to `TRUE` will prevent the request from falling through
  to other routes that might have a matching method and path. This
  setting only affects handlers on the request router.

- `ignore_trailing_slash`:

  Logical. Should the trailing slash of a path be ignored when adding
  handlers and handling requests. Setting this will not change the
  request or the path associated with but just ensure that both
  `path/to/resource` and `path/to/resource/` ends up in the same
  handler. This setting will only affect routes that are created
  automatically.

- `max_request_size`:

  Sets a maximum size of request bodies. Setting this will add a handler
  to the header router that automatically rejects requests based on
  their `Content-Length` header

- `shared_secret`:

  Assigns a shared secret to the api. Setting this will add a handler to
  the header router that automatically rejects requests if their
  `Plumber-Shared-Secret` header doesn't contain the same value. Be
  aware that this type of authentication is very weak. Never put the
  shared secret in plain text but rely on e.g. the keyring package for
  storage. Even so, if requests are send over HTTP (not HTTPS) then
  anyone can read the secret and use it

- `compression_limit`:

  The size threshold in bytes for trying to compress the response body
  (it is still dependant on content negotiation)

- `default_async`:

  The default evaluator to use for async request handling

- `env`:

  An environment that will be used as the default execution environment
  for the API

#### Returns

A `Plumber2` object

------------------------------------------------------------------------

### Method [`format()`](https://rdrr.io/r/base/format.html)

Human readable description of the api object

#### Usage

    Plumber2$format(...)

#### Arguments

- `...`:

  ignored

#### Returns

A character vector

------------------------------------------------------------------------

### Method `ignite()`

Begin running the server. Will trigger the `start` event

#### Usage

    Plumber2$ignite(
      block = FALSE,
      showcase = is_interactive(),
      ...,
      silent = FALSE
    )

#### Arguments

- `block`:

  Should the console be blocked while running (alternative is to run in
  the background)

- `showcase`:

  Should the default browser open up at the server address. If `TRUE`
  then a browser opens at the root of the api, unless the api contains
  OpenAPI documentation in which case it will open at that location. If
  a string the string is used as a path to add to the root before
  opening.

- `...`:

  Arguments passed on to the `start` handler

- `silent`:

  Should startup messaging by silenced

------------------------------------------------------------------------

### Method `add_route()`

Add a new route to either the request or header router

#### Usage

    Plumber2$add_route(name, route = NULL, header = FALSE, after = NULL, root = "")

#### Arguments

- `name`:

  The name of the route to add. If a route is already present with this
  name then the provided route (if any) is merged into it

- `route`:

  The route to add. If `NULL` a new empty route will be created

- `header`:

  Logical. Should the route be added to the header router?

- `after`:

  The location to place the new route on the stack. `NULL` will place it
  at the end. Will not have an effect if a route with the given name
  already exists.

- `root`:

  The root path to serve this route from.

------------------------------------------------------------------------

### Method `request_handler()`

Add a handler to a request. See
[api_request_handlers](https://plumber2.posit.co/reference/api_request_handlers.md)
for detailed information

#### Usage

    Plumber2$request_handler(
      method,
      path,
      handler,
      serializers,
      parsers = NULL,
      use_strict_serializer = FALSE,
      auth_flow = NULL,
      auth_scope = NULL,
      download = FALSE,
      async = FALSE,
      then = NULL,
      doc = NULL,
      route = NULL,
      header = FALSE
    )

#### Arguments

- `method`:

  The HTTP method to attach the handler to

- `path`:

  A string giving the path the handler responds to.

- `handler`:

  A handler function to call when a request is matched to the path

- `serializers`:

  A named list of serializers that can be used to format the response
  before sending it back to the client. Which one is selected is based
  on the request `Accept` header

- `parsers`:

  A named list of parsers that can be used to parse the request body
  before passing it in as the `body` argument. Which one is selected is
  based on the request `Content-Type` header

- `use_strict_serializer`:

  By default, if a serializer that respects the requests `Accept` header
  cannot be found, then the first of the provided ones are used. Setting
  this to `TRUE` will instead send back a `406 Not Acceptable` response

- `auth_flow`:

  The authentication flow the request must be validated by to be allowed
  into the handler, provided as a logical expression of authenticator
  names

- `auth_scope`:

  The scope required to access this handler given a successful
  authentication. Unless your authenticators provide scopes this should
  be `NULL`

- `download`:

  Should the response mark itself for download instead of being shown
  inline? Setting this to `TRUE` will set the `Content-Disposition`
  header in the response to `attachment`. Setting it to a string is
  equivalent to setting it to `TRUE` but will in addition also set the
  default filename of the download to the string value

- `async`:

  If `FALSE` create a regular handler. If `TRUE`, use the default async
  evaluator to create an async handler. If a string, the async evaluator
  registered to that name is used. If a function is provided then this
  is used as the async evaluator

- `then`:

  A function to call at the completion of an async handler

- `doc`:

  OpenAPI documentation for the handler. Will be added to the
  `paths$<handler_path>$<handler_method>` portion of the API.

- `route`:

  The route this handler should be added to. Defaults to the last route
  in the stack. If the route does not exist it will be created as the
  last route in the stack.

- `header`:

  Logical. Should the handler be added to the header router

------------------------------------------------------------------------

### Method `message_handler()`

Add a handler to a WebSocket message. See
[api_message](https://plumber2.posit.co/reference/api_message.md) for
detailed information

#### Usage

    Plumber2$message_handler(handler, async = FALSE, then = NULL)

#### Arguments

- `handler`:

  A function conforming to the specifications laid out in
  [`api_message()`](https://plumber2.posit.co/reference/api_message.md)

- `async`:

  If `FALSE` create a regular handler. If `TRUE`, use the default async
  evaluator to create an async handler. If a string, the async evaluator
  registered to that name is used. If a function is provided then this
  is used as the async evaluator

- `then`:

  A function to call at the completion of an async handler

------------------------------------------------------------------------

### Method `redirect()`

Add a redirect to the header router. Depending on the value of
`permanent` it will respond with a 307 Temporary Redirect or 308
Permanent Redirect. `from` and `to` can contain path parameters and
wildcards which will be matched between the two to construct the correct
redirect path.

#### Usage

    Plumber2$redirect(method, from, to, permanent = TRUE)

#### Arguments

- `method`:

  The HTTP method the redirect should respond to

- `from`:

  The path the redirect should respond to

- `to`:

  The path/URL to redirect the incoming request towards. After resolving
  any path parameters and wildcards it will be used in the `Location`
  header

- `permanent`:

  Logical. Is the redirect considered permanent or temporary? Determines
  the type of redirect status code to use

------------------------------------------------------------------------

### Method `parse_file()`

Parses a plumber file and updates the app according to it

#### Usage

    Plumber2$parse_file(file, env = NULL)

#### Arguments

- `file`:

  The path to a file to parse

- `env`:

  The parent environment to the environment the file should be evaluated
  in. If `NULL` the environment provided at construction will be used

------------------------------------------------------------------------

### Method `add_api_doc()`

Add a (partial) OpenAPI spec to the api docs

#### Usage

    Plumber2$add_api_doc(doc, overwrite = FALSE, subset = NULL)

#### Arguments

- `doc`:

  A list with the OpenAPI documentation

- `overwrite`:

  Logical. Should already existing documentation be removed or should it
  be merged together with `doc`

- `subset`:

  A character vector giving the path to the subset of the docs to assign
  `doc` to

------------------------------------------------------------------------

### Method `add_shiny()`

Add a shiny app to an api. See
[`api_shiny()`](https://plumber2.posit.co/reference/api_shiny.md) for
detailed information

#### Usage

    Plumber2$add_shiny(
      path,
      app,
      except = NULL,
      auth_flow = NULL,
      auth_scope = NULL
    )

#### Arguments

- `path`:

  The path to serve the app from

- `app`:

  A shiny app object

- `except`:

  Subpaths to `path` that should not be forwarded to the shiny app. Be
  sure it doesn't contains paths that the shiny app needs

- `auth_flow`:

  The authentication flow the request must be validated by to be allowed
  into the handler, provided as a logical expression of authenticator
  names

- `auth_scope`:

  The scope required to access this handler given a successful
  authentication. Unless your authenticators provide scopes this should
  be `NULL`

------------------------------------------------------------------------

### Method `add_report()`

Render and serve a Quarto or Rmarkdown document from an endpoint. See
[`api_report()`](https://plumber2.posit.co/reference/api_report.md) for
more information.

#### Usage

    Plumber2$add_report(
      path,
      report,
      ...,
      doc = NULL,
      max_age = Inf,
      async = TRUE,
      finalize = NULL,
      continue = FALSE,
      cache_dir = tempfile(pattern = "plumber2_report"),
      cache_by_id = FALSE,
      auth_flow = NULL,
      auth_scope = NULL,
      route = NULL
    )

#### Arguments

- `path`:

  The base path to serve the report from. Additional endpoints will be
  created in addition to this.

- `report`:

  The path to the report to serve

- `...`:

  Further arguments to
  [`quarto::quarto_render()`](https://quarto-dev.github.io/quarto-r/reference/quarto_render.html)
  or
  [`rmarkdown::render()`](https://pkgs.rstudio.com/rmarkdown/reference/render.html)

- `doc`:

  An
  [`openapi_operation()`](https://plumber2.posit.co/reference/openapi.md)
  documentation for the report. Only `query` parameters will be used and
  a request body will be generated from this for the POST methods.

- `max_age`:

  The maximum age in seconds to keep a rendered report before initiating
  a re-render

- `async`:

  Should rendering happen asynchronously (using mirai)

- `finalize`:

  An optional function to run before sending the response back. The
  function will receive the request as the first argument, the response
  as the second, and the server as the third.

- `continue`:

  A logical that defines whether the response is returned directly after
  rendering or should be made available to subsequent routes

- `cache_dir`:

  The location of the render cache. By default a temporary folder is
  created for it.

- `cache_by_id`:

  Should caching be scoped by the user id. If the rendering is dependent
  on user-level access to different data this is necessary to avoid data
  leakage.

- `auth_flow`:

  The authentication flow the request must be validated by to be allowed
  into the handler, provided as a logical expression of authenticator
  names

- `auth_scope`:

  The scope required to access this handler given a successful
  authentication. Unless your authenticators provide scopes this should
  be `NULL`

- `route`:

  The route this handler should be added to. Defaults to the last route
  in the stack. If the route does not exist it will be created as the
  last route in the stack.

------------------------------------------------------------------------

### Method `forward()`

Add a reverse proxy from a path to a given URL. See
[`api_forward()`](https://plumber2.posit.co/reference/api_forward.md)
for more details

#### Usage

    Plumber2$forward(path, url, except = NULL, auth_flow = NULL, auth_scope = NULL)

#### Arguments

- `path`:

  The root to forward from

- `url`:

  The url to forward to

- `except`:

  Subpaths to `path` that should be exempt from forwarding

- `auth_flow`:

  The authentication flow the request must be validated by to be allowed
  into the handler, provided as a logical expression of authenticator
  names

- `auth_scope`:

  The scope required to access this handler given a successful
  authentication. Unless your authenticators provide scopes this should
  be `NULL`

------------------------------------------------------------------------

### Method `add_auth_guard()`

Adds an auth guard to your API which can then be referenced in auth
flows.

#### Usage

    Plumber2$add_auth_guard(guard, name = NULL)

#### Arguments

- `guard`:

  An [Guard](https://thomasp85.github.io/fireproof/reference/Guard.html)
  subclass object defining the scheme

- `name`:

  The name to use for referencing the scheme in an auth flow

------------------------------------------------------------------------

### Method `add_auth()`

Add an auth flow to an endpoint

#### Usage

    Plumber2$add_auth(method, path, auth_flow, auth_scope = NULL, add_doc = TRUE)

#### Arguments

- `method`:

  The HTTP method to add auth to

- `path`:

  A string giving the path to be authenticated

- `auth_flow`:

  A logical expression giving the auth flow the client must pass to get
  access to the resource

- `auth_scope`:

  The scope requirements of the resource

- `add_doc`:

  Should OpenAPI documentation be added for the authentication

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Plumber2$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
