# Add a tag extension to plumber2

Package authors can extend plumber2 with their own functionalities. If
they wish to add a new tag to be used when writing annotated plumber2
routes they can use this function. If so, it should be called when the
package is loaded.

## Usage

``` r
add_plumber2_tag(tag, handler)
```

## Arguments

- tag:

  The name of the tag

- handler:

  A handler function for the tag. See *Details*

## Value

This function is called for its side effects

## Details

The `handler` argument must be a function with the arguments `block`,
`call`, `tags`, `values`, and `env`. `block` is a list with the
currently parsed information from the block. You can add or modify the
values within to suit your need as well as subclass it. You should not
remove any values as others might need them. `call` is the parsed value
of whatever expression was beneath the plumber2 block. `tags` is a
character vector of all the tags in the block, and `values` is a list of
all the values associated with the tags (that is, whatever comes after
the tag in the block). The values are unparsed. You should assume that
all tags not relevant for your extension has already been handled and
incorporated into `block`. The function must return a modified version
of `block`. If you add a subclass to `block` you should make sure that a
method for
[`apply_plumber2_block()`](https://plumber2.posit.co/reference/apply_plumber2_block.md)
for the subclass exists.

## See also

[`apply_plumber2_block()`](https://plumber2.posit.co/reference/apply_plumber2_block.md)

## Examples

``` r
if (FALSE) {
# Add a tag that says hello when used
add_plumber2_tag("hello", function(block, call, tags, values, env) {
  message("Hello")
  class(block) <- c("hello_block", class(block))
  block
})

}
```
