# Serve a Shiny app from a plumber2 api

You can serve one or more shiny apps as part of a plumber2 api. The
shiny app launches in a background process and the api will work as a
reverse proxy to forward requests to `path` to the process and relay the
response to the client. The shiny app is started along with the api and
shut down once the api is stopped. This functionality requires the shiny
and callr packages to be installed. Be aware that all requests to
subpaths of `path` will be forwarded to the shiny process, and thus not
end up in your normal route

## Usage

``` r
api_shiny(api, path, app, except = NULL)
```

## Arguments

- api:

  A plumber2 api to add the shiny app to

- path:

  The path to serve the shiny app from

- app:

  A shiny app object

- except:

  Subpaths to `path` that should not be forwarded to the shiny app. Be
  sure it doesn't contains paths that the shiny app needs

## Value

This functions return the `api` object allowing for easy chaining with
the pipe

## Using annotation

A shiny app can be served using an annotated route file by using the
`@shiny` tag and proceeding the annotation block with the shiny app
object

    #* @shiny /my_app/
    shiny::shinyAppDir("./shiny")

## Examples

``` r
blank_shiny <- shiny::shinyApp(
  ui = shiny::fluidPage(),
  server = shiny::shinyServer(function(...) {})
)

api() |>
  api_shiny("my_app/", blank_shiny)
#> ── A plumber server ────────────────────────────────────────────────────────────
#> Serving on http://127.0.0.1:8080
#> Currently not running
```
