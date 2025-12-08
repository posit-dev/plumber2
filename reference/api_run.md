# Launch the API

This function starts the api with the settings it has defined.

## Usage

``` r
api_run(
  api,
  host = NULL,
  port = NULL,
  block = !is_interactive(),
  showcase = is_interactive(),
  ...,
  silent = FALSE
)

api_stop(api)
```

## Arguments

- api:

  A plumber2 api object to launch or stop

- host, port:

  Host and port to run the api on. If not provided the host and port
  used during the creation of the Plumber2 api will be used

- block:

  Should the console be blocked while running (alternative is to run in
  the background). Defaults to `FALSE` in interactive sessions and
  `TRUE` otherwise. Note that while setting `block = FALSE` will allow
  you to continue interacting with the main session you will not be able
  to call the api from within the session since it is still executing in
  the same thread.

- showcase:

  Should the default browser open up at the server address. If `TRUE`
  then a browser opens at the root of the api, unless the api contains
  OpenAPI documentation in which case it will open at that location. If
  a string the string is used as a path to add to the root before
  opening.

- ...:

  Arguments passed on to the `start` handler

- silent:

  Should startup messaging by silenced

## Value

These functions return the `api` object allowing for easy chaining with
the pipe, even though they will often be the last part of the chain

## Examples

``` r
pa <- api() |>
  api_get("/", function() {
    list(msg = "Hello World")
  }) |>
  api_on("start", function(...) {
    cat("I'm alive")
  })
#> Creating default route in request router

# Start the server
pa |> api_run(block = FALSE)
#> plumber2 server started at http://127.0.0.1:8080
#> I'm alive

# Stop it again
pa |> api_stop()
```
