# Load up an API distributed with a package

Packages can included one or more api specification(s) by storing the
annotated route files and/or `_server.yml` file in subfolders of
`./inst/plumber2`. The name of the subfolder will be the name of the api

## Usage

``` r
api_package(package = NULL, name = NULL, ...)
```

## Arguments

- package:

  The name of the package that provides the api. If `NULL` then a list
  of available apis across all installed packages is returned

- name:

  The name of the api. If `NULL` then a list of available apis in the
  given package is returned

- ...:

  Arguments passed on to
  [`api`](https://plumber2.posit.co/reference/api.md)

  `host`

  :   A string that is a valid IPv4 address that is owned by this server

  `port`

  :   A number or integer that indicates the server port that should be
      listened on. Note that on most Unix-like systems including Linux
      and macOS, port numbers smaller than 1024 require root privileges.

  `doc_type`

  :   The type of API documentation to generate. Can be either
      `"rapidoc"` (the default), `"redoc"`, `"swagger"`, or `NULL`
      (equating to not generating API docs)

  `doc_path`

  :   The URL path to serve the api documentation from

  `reject_missing_methods`

  :   Should requests to paths that doesn't have a handler for the
      specific method automatically be rejected with a 405 Method Not
      Allowed response with the correct Allow header informing the
      client of the implemented methods. Assigning a handler to `"any"`
      for the same path at a later point will overwrite this
      functionality. Be aware that setting this to `TRUE` will prevent
      the request from falling through to other routes that might have a
      matching method and path. This setting anly affects handlers on
      the request router.

  `ignore_trailing_slash`

  :   Logical. Should the trailing slash of a path be ignored when
      adding handlers and handling requests. Setting this will not
      change the request or the path associated with but just ensure
      that both `path/to/resource` and `path/to/resource/` ends up in
      the same handler.

  `max_request_size`

  :   Sets a maximum size of request bodies. Setting this will add a
      handler to the header router that automatically rejects requests
      based on their `Content-Length` header

  `shared_secret`

  :   Assigns a shared secret to the api. Setting this will add a
      handler to the header router that automatically rejects requests
      if their `Plumber-Shared-Secret` header doesn't contain the same
      value. Be aware that this type of authentication is very weak.
      Never put the shared secret in plain text but rely on e.g. the
      keyring package for storage. Even so, if requests are send over
      HTTP (not HTTPS) then anyone can read the secret and use it

  `compression_limit`

  :   The size threshold in bytes for trying to compress the response
      body (it is still dependant on content negotiation)

  `default_async`

  :   The default evaluator to use for async request handling

  `env`

  :   The parent environment to the environment the files should be
      evaluated in. Each file will be evaluated in it's own environment
      so they don't interfere with each other

## Value

If `package` or `name` is `NULL` then a data frame providing available
apis filtered on either package or name (if any is provided) is
returned. Otherwise a
[Plumber2](https://plumber2.posit.co/reference/Plumber2.md) object
representing the api is returned

## Examples

``` r
# Load one of the plumber2 examples
api_package("plumber2", "quickstart")
#> Creating main route in request router
#> ── A plumber server ────────────────────────────────────────────────────────────
#> Serving on http://127.0.0.1:8080
#> Currently not running

# List all available apis
api_package()
#>    package        api
#> 1 plumber2 quickstart
```
