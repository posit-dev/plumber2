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

## plumber2 options

The following options are currently recognized by plumber2. They are all
read at creation time and have a parallel argument in
[`api()`](https://plumber2.posit.co/reference/api.md) where you can also
see their default values. This means that changing an option after
creation/during running will have no effect.

- **host**: The address to serve the server from

- **port**: The port to use for the server

- **docType**: The ui to use for serving OpenAPI documentation

- **docPath**: The path to serve the documentation from

- **rejectMissingMethods**: Should requests to paths that doesn't have a
  handler for the specific method automatically be rejected with a 405
  Method Not Allowed response

- **ignoreTrailingSlash**: Should the trailing slash of a path be
  ignored when adding handlers and handling requests

- **maxRequestSize**: The maximum allowed size of request bodies

- **sharedSecret**: A shared secret the request must contain to be
  permitted

- **compressionLimit**: The threshold for response size before automatic
  compression is used

- **async**: The default async engine to use

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
