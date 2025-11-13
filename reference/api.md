# Create a new plumber API, optionally based on one or more plumber files

This is the main way to create a new
[Plumber2](https://plumber2.posit.co/reference/Plumber2.md) object that
encapsulates your full api. It is also possible to add files to the API
after creation using `api_parse()`

## Usage

``` r
api(
  ...,
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

is_plumber_api(x)

api_parse(api, ...)
```

## Arguments

- ...:

  plumber files or directories containing plumber files to be parsed in
  the given order. The order of parsing determines the final order of
  the routes in the stack. If `...` contains a `_server.yml` file then
  all other files in `...` will be ignored and the `_server.yml` file
  will be used as the basis for the API

- host:

  A string that is a valid IPv4 address that is owned by this server

- port:

  A number or integer that indicates the server port that should be
  listened on. Note that on most Unix-like systems including Linux and
  macOS, port numbers smaller than 1024 require root privileges.

- doc_type:

  The type of API documentation to generate. Can be either `"rapidoc"`
  (the default), `"redoc"`, `"swagger"`, or `NULL` (equating to not
  generating API docs)

- doc_path:

  The URL path to serve the api documentation from

- reject_missing_methods:

  Should requests to paths that doesn't have a handler for the specific
  method automatically be rejected with a 405 Method Not Allowed
  response with the correct Allow header informing the client of the
  implemented methods. Assigning a handler to `"any"` for the same path
  at a later point will overwrite this functionality. Be aware that
  setting this to `TRUE` will prevent the request from falling through
  to other routes that might have a matching method and path. This
  setting anly affects handlers on the request router.

- ignore_trailing_slash:

  Logical. Should the trailing slash of a path be ignored when adding
  handlers and handling requests. Setting this will not change the
  request or the path associated with but just ensure that both
  `path/to/resource` and `path/to/resource/` ends up in the same
  handler.

- max_request_size:

  Sets a maximum size of request bodies. Setting this will add a handler
  to the header router that automatically rejects requests based on
  their `Content-Length` header

- shared_secret:

  Assigns a shared secret to the api. Setting this will add a handler to
  the header router that automatically rejects requests if their
  `Plumber-Shared-Secret` header doesn't contain the same value. Be
  aware that this type of authentication is very weak. Never put the
  shared secret in plain text but rely on e.g. the keyring package for
  storage. Even so, if requests are send over HTTP (not HTTPS) then
  anyone can read the secret and use it

- compression_limit:

  The size threshold in bytes for trying to compress the response body
  (it is still dependant on content negotiation)

- default_async:

  The default evaluator to use for async request handling

- env:

  The parent environment to the environment the files should be
  evaluated in. Each file will be evaluated in it's own environment so
  they don't interfere with each other

- x:

  An object to test for whether it is a plumber api

- api:

  A plumber2 api object to parse files into

## Value

A [Plumber2](https://plumber2.posit.co/reference/Plumber2.md) object

## See also

[`api_package()`](https://plumber2.posit.co/reference/api_package.md)
for creating an api based on files distributed with a package

[`get_opts()`](https://plumber2.posit.co/reference/get_opts.md) for how
to set default options

## Examples

``` r
# When creating an API programmatically you'll usually initialise the object
# without pointing to any route files or a _server.yml file
pa <- api()

# You can pass it a directory and it will load up all recognised files it
# contains
example_dir <- system.file("plumber2", "quickstart", package = "plumber2")
pa <- api(example_dir)
#> Creating main route in request router

# Or you can pass files directly
pa <- api(list.files(example_dir, full.names = TRUE)[1])
#> Creating main route in request router
```
