# Register an async evaluator

plumber supports async request handling in two ways. Either manual by
returning a promise from the handler, or automatic through the `@async`
tag / `async` argument in [the handler
functions](https://plumber2.posit.co/reference/api_request_handlers.md).
The default evaluator is controlled by the `plumber2.async` option or
the `PLUMBER2_ASYNC` environment variable.

## Usage

``` r
register_async(name, fun, dependency = NULL)

show_registered_async()

get_async(name = NULL, ...)
```

## Arguments

- name:

  The name of the evaluator

- fun:

  A function that, upon calling it returns an evaluator taking an `expr`
  and `envir` argument. See the [async
  evaluator](https://plumber2.posit.co/reference/async_evaluators.md)
  functions for examples

- dependency:

  Package dependencies for the evaluator.

- ...:

  Arguments passed on to the async function creator

## Examples

``` r
# Register an async evaluator based on future (the provided mirai backend is
# superior in every way so this is for illustrative purpose)
future_async <- function(...) {
  function(expr, envir) {
    promises::future_promise(
      expr = expr,
      envir = envir,
      substitute = FALSE,
      ...
    )
  }
}
register_async("future", future_async, c("promises", "future"))
```
