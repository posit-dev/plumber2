# Async evaluators provided by plumber

These functions support async request handling. You can register your
own as well using
[`register_async()`](https://plumber2.posit.co/reference/register_async.md).

## Usage

``` r
mirai_async(...)
```

## Arguments

- ...:

  Further argument passed on to the internal async function. See Details
  for information on which function handles the formatting internally in
  each async evaluator

## Value

A function taking `expr` and `envir`. The former is the expression to
evaluate and the latter is an environment with additional variables that
should be made available during evaluation

## Provided evaluators

- `mirai_async()` uses
  [`mirai::mirai()`](https://mirai.r-lib.org/reference/mirai.html). It
  is registered as `"mirai"`. Be aware that for this evaluator to be
  performant you should start up multiple persistent background
  processes. See
  [`mirai::daemons()`](https://mirai.r-lib.org/reference/daemons.html).

## Examples

``` r
# Use the default mirai backend by setting `async = TRUE` with a handler

pa <- api() |>
  api_get("/hello/<name:string>", function(name) {
    list(
      msg = paste0("Hello ", name, "!")
    )
  }, async = TRUE)
#> Creating default route in request router

```
