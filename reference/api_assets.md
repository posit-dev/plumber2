# Serve resources from your file system

plumber2 provides two ways to serve files from your server. One
(`api_assets`) goes through R and gives you all the power you expect to
further modify and work with the response. The other (api_statics) never
hits the R process and as a result is blazing fast. However this comes
with the price of very limited freedom to modify the response or even do
basic authentication. Each has their place.

## Usage

``` r
api_assets(
  api,
  at,
  path,
  default_file = "index.html",
  default_ext = "html",
  finalize = NULL,
  continue = FALSE,
  auth_flow = NULL,
  auth_scope = NULL,
  route = NULL
)

api_statics(
  api,
  at,
  path,
  use_index = TRUE,
  fallthrough = FALSE,
  html_charset = "utf-8",
  headers = list(),
  validation = NULL,
  except = NULL
)
```

## Arguments

- api:

  A plumber2 api object to add the rossource serving to

- at:

  The path to serve the resources from

- path:

  The location on the file system to map `at` to

- default_file:

  The default file to look for if the path does not map to a file
  directly (see Details)

- default_ext:

  The default file extension to add to the file if a file cannot be
  found at the provided path and the path does not have an extension
  (see Details)

- finalize:

  An optional function to run if a file is found. The function will
  receive the request as the first argument, the response as the second,
  and anything passed on through `...` in the `dispatch` method. Any
  return value from the function is discarded. The function must accept
  `...`

- continue:

  A logical that should be returned if a file is found. Defaults to
  `FALSE` indicating that the response should be send unmodified.

- auth_flow:

  A logical expression giving the authentication flow the client must
  pass to get access to the resource.

- auth_scope:

  The scope requirements of the resource

- route:

  The name of the route in the header router to add the asset route to.
  Defaults to the last route in the stack. If the route does not exist
  it will be created as the last route in the stack

- use_index:

  Should an `index.html` file be served if present when a client
  requests the folder

- fallthrough:

  Should requests that doesn't match a file enter the request loop or
  have a 404 response send directly

- html_charset:

  The charset to report when serving html files

- headers:

  A list of headers to add to the response. Will be combined with the
  global headers of the app

- validation:

  An optional validation pattern. Presently, the only type of validation
  supported is an exact string match of a header. For example, if
  `validation` is `'"abc" = "xyz"'`, then HTTP requests must have a
  header named `abc` (case-insensitive) with the value `xyz`
  (case-sensitive). If a request does not have a matching header, than
  httpuv will give a 403 Forbidden response. If the `character(0)` (the
  default), then no validation check will be performed.

- except:

  One or more url paths that should be excluded from the route. Requests
  matching these will enter the standard router dispatch. The paths are
  interpreted as subpaths to `at`, e.g. the final path to exclude will
  be `at`+`exclude` (see example)

## Value

These functions return the `api` object allowing for easy chaining with
the pipe

## Using annotation

When using annotated route files the functionality of `api_assets()` can
be achieved like this:

    #* @assets my_wd/ ./
    NULL

When using annotated route files the functionality of `api_statics()`
can be achieved like this:

    #* @statics my_docs/ ~/
    #* @except my_secret_folder/
    NULL

## Examples

``` r
# Add asset serving through routr route
api() |>
  api_assets("my_wd/", "./")
#> Creating default route in header router
#> ── A plumber server ────────────────────────────────────────────────────────────
#> Serving on http://127.0.0.1:8080
#> Currently not running

# Add asset serving directly
api() |>
  api_statics("my_docs", "~/", except = "my_secret_folder/")
#> ── A plumber server ────────────────────────────────────────────────────────────
#> Serving on http://127.0.0.1:8080
#> Currently not running
```
