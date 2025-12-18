# Parse a plumber file

This function takes care of parsing an annotated plumber file and
creating one or more routes, API specs, and a modifier function to be
called on the plumber app after the routes have been added. This
function does not attach the parsed data to a plumber api, and it is
rarely necessary to call it directly.

## Usage

``` r
parse_plumber_file(path, env = caller_env())
```

## Arguments

- path:

  The path to the file to parse

- env:

  The environment to evaluate the code and annotations in

## Value

A list containing:

- `route` The main route handling requests according to the parsed file,
  as a named list of length one

- `header_route` The route to be attached to header events (fires before
  the body has been recieved and can be used to prematurely reject
  requests based on their headers), as a named list of length one

- `asset_routes` All the asset routes created by `@static` blocks as a
  named list

- `message_handlers` All the websocket message handlers created by
  `@message` blocks, as a list

- `api` A list giving the OpenAPI spec as parsed from the file

- `modifier` A single function chaining all the functions from
  `@plumber` blocks together

## Examples

``` r
if (FALSE) {
# Parse a plumber file
parse_plumber_file("path/to/my/plumber/file.R")
}
```
