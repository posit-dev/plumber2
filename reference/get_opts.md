# Retrieve options for creating a plumber2 api

You can provide options for your plumber2 api which will be picked up
when you create the API with
[`api()`](https://plumber2.posit.co/reference/api.md). Options can be
set either through the internal
[`options()`](https://rdrr.io/r/base/options.html) functionality, or by
setting environment variables. In the former case, the name of the
option must be prefixed with `"plumber2."`, in the latter case the
variable name must be in upper case and prefixed with `"PLUMBER2_"`. If
the option is stored as an environment variable then the value is cast
to the type giving in `default`. See the docs for
[`api()`](https://plumber2.posit.co/reference/api.md) for the default
values of the different options.

## Usage

``` r
get_opts(x, default = NULL)

all_opts()
```

## Arguments

- x:

  The name of the option

- default:

  The default value, if `x` is not set

## Value

For `get_opts` The value of `x`, if any, or `default`. For `all_opts()`
a named list of all the options that are set

## Examples

``` r
# Using `options()`
old_opts <- options(plumber2.port = 9889L)
get_opts("port")
#> [1] 9889
options(old_opts)

# Using environment variables
old_env <- Sys.getenv("PLUMBER2_PORT")
Sys.setenv(PLUMBER2_PORT = 9889)

## If no default is provided the return value is a string
get_opts("port")
#> [1] "9889"

## Provide a default to hint at the options type
get_opts("port", 8080L)
#> [1] 9889

Sys.setenv(PLUMBER2_PORT = old_env)
```
