# Register or fetch a parser

plumber2 comes with many parsers that should cover almost all standard
use cases. Still you might want to provide some of your own, which this
function facilitates.

## Usage

``` r
register_parser(name, fun, mime_types, default = TRUE)

show_registered_parsers()

get_parsers(parsers = NULL)
```

## Arguments

- name:

  The name to register the parser function to. If already present the
  current parser will be overwritten by the one provided by you

- fun:

  A function that, when called, returns a binary function that can parse
  a request body. The first argument takes a raw vector with the binary
  encoding of the request body, the second argument takes any additional
  directives given by the requests `Content-Type` header

- mime_types:

  One or more mime types that this parser can handle. The mime types are
  allowed to contain wildcards, e.g. `"text/*"`

- default:

  Should this parser be part of the default set of parsers

- parsers:

  Parsers to collect. This can either be a character vector of names of
  registered parsers or a list. If it is a list then the following
  expectations apply:

  - Any unnamed elements containing a character vector will be
    considered as names of registered parsers constructed with default
    values. The special value `"..."` will fetch all the parsers that
    are otherwise not specified in the call

  - Any element containing a function are considered as a provided
    parser and the element must be named by the mime type the parser
    understands (wildcards allowed)

  - Any remaining named elements will be considered names of registered
    parsers that should be constructed with the arguments given in the
    element

## Value

For `get_parsers` a named list of parser functions named by their mime
types. The order given in `parsers` is preserved.

## Details

If you want to register your own parser, then the function you register
must be a factory function, i.e. a function returning a function. The
returned function must accept two arguments, the first being a raw
vector corresponding to the request body, the second being the parsed
directives from the request `Content-Type` header. All arguments to the
factory function should be optional.

## See also

[parsers](https://plumber2.posit.co/reference/parsers.md)

[`register_serializer()`](https://plumber2.posit.co/reference/register_serializer.md)

## Examples

``` r
if (FALSE) {
# Register a parser that splits at a character and converts to number
register_parser("comma", function(delim = ",") {
  function(raw, directive) {
    as.numeric(strsplit(rawToChar(raw), delim)[[1]])
  }
}, mime_types = "text/plain", default = FALSE)
}
```
