# Generic for applying information from a plumber2 block to an api

In order to facilitate extensibility of the plumber2 file syntax you can
provide your own methods for how to apply information from a plumber2
block to an api.

## Usage

``` r
apply_plumber2_block(block, api, route_name, root, ...)
```

## Arguments

- block:

  The block that was parsed

- api:

  The [Plumber2](https://plumber2.posit.co/reference/Plumber2.md) api
  object to apply it to

- route_name:

  The name of the route the plumber2 file is associated with. Either the
  name of the file or the value of the `@routeName` tag

- root:

  The root given by the potential `@root` tag in the file. If no `@root`
  tag is present this value will be null. The value represents the root
  path for every endpoint defined in the file and should be prepended to
  any URL path you use.

- ...:

  ignored

## Value

`api`, modified

## See also

[`add_plumber2_tag()`](https://plumber2.posit.co/reference/add_plumber2_tag.md)

## Examples

``` r
if (FALSE) {
# Add a method for a fictional "hello_block" that makes the api say hello when
# it starts
apply_plumber2_block.hello_block <- function(block, api, route_name, root, ...) {
  api$on("start", function(...) {
    message("Hello")
  })
  api
}
}
```
