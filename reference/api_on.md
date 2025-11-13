# Add a handler to an event

During the life cycle of a plumber API various events will be fired,
either automatically or manually. See the [article on events in
fiery](https://fiery.data-imaginist.com/articles/events.html) for a full
overview. `api_on()` allows you to add handlers that are called when
specific events fire. `api_off()` can be used to remove the handler if
necessary

## Usage

``` r
api_on(api, event, handler, id = NULL)

api_off(api, id)
```

## Arguments

- api:

  A plumber2 api object to launch or stop

- event:

  A string naming the event to listen for

- handler:

  A function to call when `event` fires

- id:

  A string uniquely identifying the handler. If `NULL` a random id will
  be generated making it impossible to remove the handler again

## Value

These functions return the `api` object allowing for easy chaining with
the pipe

## Using annotation

Event handler setup doesn't have a dedicated annotation tag, but you can
set it up in a `@plumber` block

    #* @plumber
    function(api) {
      api |>
        api_on("cycle-end", function(server) {
          server$log("message", "tick-tock")
        })
    }

## Examples

``` r
# Add a small console log to show the api is alive
pa <- api() |>
  api_on("cycle-end", function(server) {
    server$log("message", "tick-tock")
  }, id = "lifesign")

# Remove it again
pa |>
  api_off("lifesign")
#> ── A plumber server ────────────────────────────────────────────────────────────
#> Serving on http://127.0.0.1:8080
#> Currently not running
```
