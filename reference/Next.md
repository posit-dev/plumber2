# Router control flow

In plumber2 your API can have multiple middleware that a request passes
through. At any point can you short-circuit the remaining middleware by
returning `Break`, which instructs plumber2 to return the response as
is. Returning `Next` indicates the opposite, ie that the request should
be allowed to pass on to the next middleware in the chain. A handler
function that doesn't return either of these are assumed to return a
value that should be set to the response body and implicitely continue
to the next middleware.

## Usage

``` r
Next

Break

should_break(x)
```

## Arguments

- x:

  An object to test

## Value

A boolean value

## Examples

``` r
# should_break() only returns TRUE with Break

should_break(10)
#> [1] FALSE

should_break(FALSE)
#> [1] FALSE

should_break(Next)
#> [1] FALSE

should_break(Break)
#> [1] TRUE
```
