# Add an auth guard to your API

This function adds an auth guard to your API. Notably, this does not
turn on auth for any of your handlers but makes it available for
reference in an auth flow. To use it, reference it in the `auth_flow`
argument of functions supporting it. Guards are defined using the
various `guard_*()` constructors in the fireproof package. Refer to
these for further documentation

## Usage

``` r
api_auth_guard(api, guard, name = NULL)
```

## Arguments

- api:

  A plumber2 api object to add the authenticator to

- guard:

  A [Guard](https://thomasp85.github.io/fireproof/reference/Guard.html)
  subclass object defining an authentication scheme

- name:

  The name to use for referencing the guard in an authentication flow

## Value

This functions return the `api` object allowing for easy chaining with
the pipe

## Using annotation

To add a guard to your api defined in an annotated file use the
`@authGuard` tag:

    #* @authGuard BasicAuth
    fireproof::guard_basic(...)

The tag parameter (`BasicAuth`) provides the name for the guard

## Examples

``` r
guard <- fireproof::guard_key(
  key_name = "plumber2-key",
  validate = "MY_VERY_SECRET_KEY"
)

api() |>
  api_datastore(storr::driver_environment()) |>
  api_auth_guard(guard, "cookie_key")
#> ── A plumber server ────────────────────────────────────────────────────────────
#> Serving on http://127.0.0.1:8080
#> Currently not running
```
