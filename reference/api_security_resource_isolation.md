# Set up resource isolation for a path

This function adds resource isolation to a path in your API. The
function can be called multiple times to set up resource isolation for
multiple paths, potentially with different settings for each path. You
can read in depth about resource isolation at the
[ResourceIsolation](https://rdrr.io/pkg/firesafety/man/ResourceIsolation.html)
plugin documentation.

## Usage

``` r
api_security_resource_isolation(
  api,
  path = "/*",
  allowed_site = "same-site",
  forbidden_navigation = c("object", "embed"),
  allow_cors = TRUE
)
```

## Arguments

- api:

  A plumber2 api object to add the plugin to

- path:

  The path that the policy should apply to. routr path syntax applies,
  meaning that wilcards and path parameters are allowed.

- allowed_site:

  The allowance level to permit. Either `cross-site`, `same-site`, or
  `same-origin`.

- forbidden_navigation:

  A vector of destinations not allowed for navigational requests. See
  the [`Sec-Fetch-Dest`
  documentation](https://developer.mozilla.org/en-US/docs/Web/HTTP/Reference/Headers/Sec-Fetch-Dest)
  for a description of possible values. The special value `"all"` is
  also permitted which is the equivalent of passing all values.

- allow_cors:

  Should `Sec-Fetch-Mode: cors` requests be allowed

## Value

This functions return the `api` object allowing for easy chaining with
the pipe

## Using annotation

To add resource isolation to a path you can add `@rip <allowed_site>` to
a handler annotation. This will add resource isolation to all endpoints
described in the block. The annotation doesn't allow setting
`forbidden_navigation` or `allow_cors` and the default values will be
used.

    #* A handler for /user/<username>
    #*
    #* @param username:string The name of the user to provide information on
    #*
    #* @get /user/<username>
    #*
    #* @response 200:{name:string, age:integer, hobbies:[string]} Important
    #* information about the user such as their name, age, and hobbies
    #*
    #* @rip same-origin
    #*
    function(username) {
      find_user_in_db(username)
    }

## See also

Other security features:
[`api_security_cors()`](https://plumber2.posit.co/reference/api_security_cors.md),
[`api_security_headers()`](https://plumber2.posit.co/reference/api_security_headers.md)

## Examples

``` r
# Set up resource isolation for everything inside a user path
api() |>
  api_security_resource_isolation(
    path = "<user>/*"
  )
#> ── A plumber server ────────────────────────────────────────────────────────────
#> Serving on http://127.0.0.1:8080
#> Currently not running

```
