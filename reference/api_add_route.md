# Add a new route to either the request or header router

This function allows explicit creation of routes or addition/merging of
a predefined routr::Route into the router of the api. A new route can
also be created with the `route` argument when [adding a
handler](https://plumber2.posit.co/reference/api_request_handlers.md).
However, that way will always add new routes to the end of the stack,
whereas using `api_add_route()` allows you full control of the
placement.

## Usage

``` r
api_add_route(api, name, route = NULL, header = FALSE, after = NULL, root = "")
```

## Arguments

- api:

  A plumber2 api object to add the route to

- name:

  The name of the route to add. If a route is already present with this
  name then the provided route (if any) is merged into it

- route:

  The route to add. If `NULL` a new empty route will be created

- header:

  Logical. Should the route be added to the header router?

- after:

  The location to place the new route on the stack. `NULL` will place it
  at the end. Will not have an effect if a route with the given name
  already exists.

- root:

  The root path to serve this route from.

## Value

This functions return the `api` object allowing for easy chaining with
the pipe

## Using annotation

There is no direct equivalent to this when using annotated route files.
However you can name your route in a file by adding `@routeName <name>`
to the first block of the file like so.

    #* @routeName my_route
    NULL

All relevant blocks in the file will then be added to this route, even
if the route already exist. In that way you can split the definition of
a single route out among multiple files if needed.

## Examples

``` r
# Add a new route and use it for a handler
api() |>
  api_add_route("logger_route") |>
  api_any(
    "/*",
    function() {
      cat("I just handled a request!")
    },
    route = "logger_route"
  )
#> ── A plumber server ────────────────────────────────────────────────────────────
#> Serving on http://127.0.0.1:8080
#> Currently not running

```
