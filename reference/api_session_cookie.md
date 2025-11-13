# Turn on session cookie data storage for your API

If you need to keep data between requests, but don't want to store it
server-side (see
[`api_datastore()`](https://plumber2.posit.co/reference/api_datastore.md))
you can instead pass it back and forth as an encrypted session cookie.
This function sets it up on your api and after it's use you can now
access and set session data in the request and response `$session`
field. Be aware that session data is send back and forth with all
requests and should thus be kept minimal to avoid congestion on your
server.

## Usage

``` r
api_session_cookie(
  api,
  key,
  name = "reqres",
  expires = NULL,
  max_age = NULL,
  path = NULL,
  secure = NULL,
  same_site = NULL
)
```

## Arguments

- api:

  A plumber2 api object to add the session cookie setup to

- key:

  A 32-bit secret key as a hex encoded string or a raw vector to use for
  encrypting the session cookie. A valid key can be generated using
  [`reqres::random_key()`](https://reqres.data-imaginist.com/reference/random_key.html).
  NEVER STORE THE KEY IN PLAIN TEXT. Optimally use the keyring package
  to store it

- name:

  The name of the cookie

- expires:

  A POSIXct object given the expiration time of the cookie

- max_age:

  The number of seconds to elapse before the cookie expires

- path:

  The URL path this cookie is related to

- secure:

  Should the cookie only be send over https

- same_site:

  Either `"Lax"`, `"Strict"`, or `"None"` indicating how the cookie can
  be send during cross-site requests. If this is set to `"None"` then
  `secure` *must* also be set to `TRUE`

## Value

These functions return the `api` object allowing for easy chaining with
the pipe

## Using annotation

Session cookie setup doesn't have a dedicated annotation tag, but you
can set it up in a `@plumber` block

    #* @plumber
    function(api) {
      api |>
        api_session_cookie(keyring::key_get("my_secret_plumber_key"))
    }

## Examples

``` r
key <- reqres::random_key()

api() |>
  api_session_cookie(key, secure = TRUE) |>
  api_get("/", function(request) {
    if (isTRUE(request$session$foo)) {
      msg <- "You've been here before"
    } else {
      msg <- "You must be new here"
      request$session$foo <- TRUE
    }
    list(
      msg = msg
    )
  })
#> Creating default route in request router
#> ── A plumber server ────────────────────────────────────────────────────────────
#> Serving on http://127.0.0.1:8080
#> Currently not running
```
