# Set up CORS for a path in your plumber2 API

This function adds Cross-Origin Resource Sharing (CORS) to a path in
your API. The function can be called multiple times to set up CORS for
multiple paths, potentially with different settings for each path. CORS
is a complex specification and more can be read about it at the
[CORS](https://rdrr.io/pkg/firesafety/man/CORS.html) plugin
documentation.

## Usage

``` r
api_security_cors(
  api,
  path = "/*",
  origin = "*",
  methods = c("get", "head", "put", "patch", "post", "delete"),
  allowed_headers = NULL,
  exposed_headers = NULL,
  allow_credentials = FALSE,
  max_age = NULL
)
```

## Arguments

- api:

  A plumber2 api object to add the plugin to

- path:

  The path that the policy should apply to. routr path syntax applies,
  meaning that wilcards and path parameters are allowed.

- origin:

  The origin allowed for the path. Can be one of:

  - A boolean. If `TRUE` then all origins are permitted and the
    preflight response will have the `Access-Control-Allow-Origin`
    header reflect the origin of the request. If `FALSE` then all
    origins are denied

  - The string `"*"` which will allow all origins and set
    `Access-Control-Allow-Origin` to `*`. This is different than setting
    it to `TRUE` because `*` instructs browsers that any origin is
    allowed and it may use this information when searching the cache

  - A character vector giving allowed origins. If the request origin
    matches any of these then the `Access-Control-Allow-Origin` header
    in the response will reflect the origin of the request

  - A function taking the request and returning `TRUE` if the origin is
    permitted and `FALSE` if it is not. If permitted the
    `Access-Control-Allow-Origin` header will reflect the request origin

- methods:

  The HTTP methods allowed for the `path`

- allowed_headers:

  A character vector of request headers allowed when making the request.
  If the request contains headers not permitted, then the response will
  be blocked by the browser. `NULL` will allow any header by reflecting
  the `Access-Control-Request-Headers` header value from the request
  into the `Access-Control-Allow-Headers` header in the response.

- exposed_headers:

  A character vector of response headers that should be made available
  to the client upon a succesful request

- allow_credentials:

  A boolean indicating whether credentials are allowed in the request.
  Credentials are cookies or HTTP authentication headers, which are
  normally stripped from `fetch()` requests by the browser. If this is
  `TRUE` then `origin` cannot be `*` according to the spec

- max_age:

  The duration browsers are allowed to keep the preflight response in
  the cache

## Value

This functions return the `api` object allowing for easy chaining with
the pipe

## Using annotation

To add CORS to a path you can add `@cors <origin>` to a handler
annotation. `<origin>` must be one or more URLs or `*`, separated by
comma (meaning it is not possible to provide a function using the
annotation). This will add CORS to all endpoints described in the block.
The annotation doesn't allow setting `allowed_headers`,
`exposed_headers`, `allow_credentials` or `max_age` and the default
values will be used.

    #* A handler for /user/<username>
    #*
    #* @param username:string The name of the user to provide information on
    #*
    #* @get /user/<username>
    #*
    #* @response 200:{name:string, age:integer, hobbies:[string]} Important
    #* information about the user such as their name, age, and hobbies
    #*
    #* @cors https://example.com, https://another-site.com
    #*
    function(username) {
      find_user_in_db(username)
    }

## See also

Other security features:
[`api_security_headers()`](https://plumber2.posit.co/reference/api_security_headers.md),
[`api_security_resource_isolation()`](https://plumber2.posit.co/reference/api_security_resource_isolation.md)

## Examples

``` r
# Set up cors for your asset/ path for the https://examples.com origin

api() |>
  api_security_cors(
    path = "asset/*",
    origin = "https://examples.com"
  )
#> ── A plumber server ────────────────────────────────────────────────────────────
#> Serving on http://127.0.0.1:8080
#> Currently not running
```
