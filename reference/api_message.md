# Add a handler to a WebSocket message

WebSockets is a bidirectional communication channel that can be
established at the request of the client. While websocket communication
is not really part of a standard REST api, it has many uses and can
easily be used together with one.

## Usage

``` r
api_message(api, handler, async = NULL, then = NULL)
```

## Arguments

- api:

  A plumber2 api object to add the handler to

- handler:

  A function conforming to the specifications laid out in Details

- async:

  If `FALSE` create a regular handler. If `TRUE`, use the default async
  evaluator to create an async handler. If a string, the async evaluator
  registered to that name is used. If a function is provided then this
  is used as the async evaluator. See the *Async* section for more
  detail

- then:

  A list of function to be called once the async handler is done. The
  functions will be chained using
  [`promises::then()`](https://rstudio.github.io/promises/reference/then.html).
  See the *Async* section for more detail

## Value

This functions return the `api` object allowing for easy chaining with
the pipe

## Details

A handler for a websocket message is much simpler than for requests in
general since it doesn't have to concern itself with methods, paths, and
responses. Any message handler registered will get called in sequence
when a websocket message is received from a client. Still, a few
expectations apply

### Handler Arguments

The handler can take any of the following arguments:

- `message`: Either a raw vector if the message received is in binary
  form or a single string, giving the message sent from the client

- `server`: The
  [Plumber2](https://plumber2.posit.co/reference/Plumber2.md) object
  representing your server implementation

- `client_id`: A string uniquely identifying the session the request
  comes from

- `request`: The request that was initially used to establish the
  websocket connection with the client as a
  [reqres::Request](https://reqres.data-imaginist.com/reference/Request.html)
  object

### Handler Return Value

It is not expected that a websocket message sends a response and thus
the handler is not required to do anything like that. However, if the
handler returns either a raw vector or a single string it is taken as a
signal to send this back to the client. Any other return value is
silently ignored.

## Async

You can handle websocket messages asynchronously if needed. Like with
[request
handlers](https://plumber2.posit.co/reference/api_request_handlers.md)
you can either do it manually by creating and returning a promise inside
the handler, or by letting plumber2 convert your handler to an async
handler using the `async` argument. Due to the nature of promises a
handler being converted to a promise can't take `request` and `server`
arguments, so if you need to manipulate these you need to use `then`
(more on this shortly). The same conventions about return value holds
for async message handlers as for regular ones.

### Async chaining

Because you can't manipulate `request` or `server` in the async handler
it may be needed to add operations to perform once the async handler has
finished. This can be done through the `then` argument. This takes a
list of functions to chain to the promise using
[`promises::then()`](https://rstudio.github.io/promises/reference/then.html).
Before the `then` chain is executed the return value of the async
handler will be send back to the client if it is a string or a raw
vector. Each `then` call will receive the same arguments as a standard
message handler as well as `result` which will hold the return value of
the previous handler in the chain. For the first `then` call `result`
will be whatever the main async handler returned. The return value of
the last call in the chain will be silently ignored.

## Using annotation

A websocket message handler can be added to an API in an annotated route
file by using the `@message` tag

    #* @message
    function(message) {
      if (message == "Hello") {
        return("Hello, you...")
      }
    }

You can create async handlers with `then` chaining using annotation,
through the `@async` and `@then` tags

    #* @message
    #* @async
    function(message) {
      if (message == "Hello") {
        return("Hello, you...")
      }
    }
    #* @then
    function(server) {
      server$log("message", "websocket message received")
    }

## Examples

``` r
api() |>
  api_message(
    function(message) {
      if (message == "Hello") {
        return("Hello, you...")
      }
    }
  )
#> ── A plumber server ────────────────────────────────────────────────────────────
#> Serving on http://127.0.0.1:8080
#> Currently not running
```
