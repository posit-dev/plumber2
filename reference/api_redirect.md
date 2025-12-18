# Redirect request to another resource

While it is optimal that an API remains stable over its lifetime it is
often not fully attainable. In order to direct requests for resources
that has been moved to the new location you can add a redirect that
ensures a smooth transition for clients still using the old path.
Depending on the value of `permanent` the redirect will respond with a
`307 Temporary Redirect` or `308 Permanent Redirect`. `from` and `to`
can contain path parameters and wildcards which will be matched between
the two to construct the correct redirect path. Further, `to` can either
be a path to the same server or a fully qualified URL to redirect
requests to another server alltogether.

## Usage

``` r
api_redirect(api, method, from, to, permanent = TRUE)
```

## Arguments

- api:

  A plumber2 api object to add the redirect to

- method:

  The HTTP method the redirect should respond to

- from:

  The path the redirect should respond to

- to:

  The path/URL to redirect the incoming request towards. After resolving
  any path parameters and wildcards it will be used in the `Location`
  header

- permanent:

  Logical. Is the redirect considered permanent or temporary? Determines
  the type of redirct status code to use

## Value

This functions return the `api` object allowing for easy chaining with
the pipe

## Using annotation

You can specify redirects in an annotated plumber file using the
`@redirect` tag. Preceed the method with a `!` to mark the redirect as
permanent

    #* @redirect !get /old/data/* /new/data/*
    #* @redirect any /unstable/endpoint /stable/endpoint
    NULL

## Examples

``` r
api() |>
  api_redirect("get", "/old/data/*", "/new/data/*")
#> ── A plumber server ────────────────────────────────────────────────────────────
#> Serving on http://127.0.0.1:8080
#> Currently not running
```
