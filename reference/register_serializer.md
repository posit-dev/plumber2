# Register or fetch a serializer

plumber2 comes with many serializers that should cover almost all
standard use cases. Still you might want to provide some of your own,
which this function facilitates.

## Usage

``` r
register_serializer(name, fun, mime_type, default = TRUE)

show_registered_serializers()

get_serializers(serializers = NULL)
```

## Arguments

- name:

  The name to register the serializer function to. If already present
  the current serializer will be overwritten by the one provided by you

- fun:

  A function that, when called, returns a unary function that can
  serialize a response body to the mime type defined in `mime_type`

- mime_type:

  The format this serializer creates. You should take care to ensure
  that the value provided is a standard mime type for the format

- default:

  Should this serializer be part of the default set of serializers

- serializers:

  Serializers to collect. This can either be a character vector of names
  of registered serializers or a list. If it is a list then the
  following expectations apply:

  - Any unnamed elements containing a character vector will be
    considered as names of registered serializers constructed with
    default values. The special value `"..."` will fetch all the
    serializers that are otherwise not specified in the call.

  - Any element containing a function are considered as a provided
    serializer and the element must be named by the mime type the
    serializer understands

  - Any remaining named elements will be considered names of registered
    serializers that should be constructed with the arguments given in
    the element

## Value

For `get_serializers` a named list of serializer functions named by
their mime type. The order given in `serializers` is preserved.

## Details

If you want to register your own serializer, then the function you
register must be a factory function, i.e. a function returning a
function. The returned function must accept a single argument which is
the response body. All arguments to the factory function should be
optional.

## Note

Using the `...` will provide remaining graphics serializers if a
graphics serializer is explicitely requested elsewhere. Otherwise it
will provide the remaining non-graphics serializers. A warning is thrown
if a mix of graphics and non-graphics serializers are requested.

## See also

[serializers](https://plumber2.posit.co/reference/serializers.md)

`register_serializer()`

## Examples

``` r
if (FALSE) {
# Add a serializer that deparses the value
register_serializer("deparse", function(...) {
  function(x) {
    deparse(x, ...)
  }
}, mime_type = "text/plain")
}
```
