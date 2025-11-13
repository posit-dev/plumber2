# Add auth to an endpoint

This function adds auth to a specific method + path. It does so by
defining an auth flow which the request must pass in order to proceed,
as well as an optional vector of scopes required. The flow is given as a
logical expression of
[guards](https://plumber2.posit.co/reference/api_auth_guard.md) it must
satisfy. If you have registered two guards, `auth1` and `auth2`, then a
flow could be `auth1 && auth2` to require that both guards must be
passed to gain access. Alternatively you could use `auth1 || auth2` to
require that just one of them are passed. Flows can be arbitrarily
complex with nesting etc, but the OpenAPI spec has limits to what it can
describe so if you want to have an OpenAPI compliant api you must limit
yourself to at most two levels of nesting with the outer level being
`||` (ie. `(auth1 && auth2) || (auth3 && auth4)` is ok, but
`(auth1 || auth2) && (auth3 || auth4)` is not due to the outer level
being `&&`, and `(auth1 && auth2) || (auth3 && (auth4 || auth5))` is not
allowed because it has 3 nesting levels). This is only a limitation of
OpenAPI and plumber2 itself can handle all of the above. If scope is
given the scope of the user after successful authentication must contain
*all* of the provided scopes. While this function allows you to add
authentication to a path directly, it is often more convenient to add it
along with the resource or functionality you want to protect. To that
end, many functions such as
[`api_get()`](https://plumber2.posit.co/reference/api_request_handlers.md)
and [`api_report()`](https://plumber2.posit.co/reference/api_report.md)
also takes `auth_flow` and `auth_scope` as input and if given will add
auth to the relevant endpoint.

## Usage

``` r
api_auth(api, method, path, auth_flow, auth_scope = NULL, add_doc = TRUE)
```

## Arguments

- api:

  A plumber2 api object to add authentication to

- method:

  The HTTP method to add authentication to

- path:

  A string giving the path to be authenticated

- auth_flow:

  A logical expression giving the authentication flow the client must
  pass to get access to the resource.

- auth_scope:

  The scope requirements of the resource

- add_doc:

  Should OpenAPI documentation be added for the authentication

## Value

This functions return the `api` object allowing for easy chaining with
the pipe

## Examples

``` r
# We are not adding the guards here - only the auth flow
# We assume the guards `oauth`, `basic`, and `key` will be added
# later
api() |>
  api_datastore(storr::driver_environment()) |>
  api_auth(
    method = "get",
    path = "/user/<username>",
    auth_flow = oauth || (basic && key)
  )
#> ── A plumber server ────────────────────────────────────────────────────────────
#> Serving on http://127.0.0.1:8080
#> Currently not running
```
