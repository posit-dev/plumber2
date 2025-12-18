# Set up a plumber2 api to act as a reverse proxy

You can set up your plumber2 api to act as reverse proxy and forward all
requests to a specific path (and it's subpaths) to a different URL. In
contrast to
[`api_shiny()`](https://plumber2.posit.co/reference/api_shiny.md),
`api_forward()` is not responsible for launching whatever service is
being proxied so this should be handled elsewhere. The `path` will be
stripped from the request before being forwarded to the url, meaning
that if you set up a proxy on `my/proxy/` to `http://example.com`, then
a request for `my/proxy/user/thomas` will end at
`http://example.com/user/thomas`. Proxying is most useful when
forwarding to internal servers though you are free to forward to public
URLs as well. However, for the later you'd usually use a redirect
instead (via
[`api_redirect()`](https://plumber2.posit.co/reference/api_redirect.md))

## Usage

``` r
api_forward(api, path, url, except = NULL, auth_flow = NULL, auth_scope = NULL)
```

## Arguments

- api:

  A plumber2 api to add the shiny app to

- path:

  The path to serve the shiny app from

- url:

  The url to forward to

- except:

  Subpaths to `path` that should be exempt from forwarding

- auth_flow:

  A logical expression giving the authentication flow the client must
  pass to get access to the resource.

- auth_scope:

  The scope requirements of the resource

## Value

This functions return the `api` object allowing for easy chaining with
the pipe

## Using annotation

You can set up a reverse proxy in your annotated route file using the
`@forward` tag

    #* @forward /proxy http://127.0.0.1:56789
    NULL

## Examples

``` r
# Serve wikipedia directly from your app
api() |>
  api_forward("my_wiki/", "https://www.wikipedia.org")
#> ── A plumber server ────────────────────────────────────────────────────────────
#> Serving on http://127.0.0.1:8080
#> Currently not running
```
