# Formatter orchestration

These functions are for internal use and only exported to ease async
evaluation

## Usage

``` r
init_formatter(formatter)

close_formatter(formatter, info)

clean_formatter(formatter, info)

with_formatter(expr, formatter, info)
```

## Arguments

- formatter:

  A serializer function

- info:

  A structure returned by `init_formatter()`

- expr:

  An expression to evaluate in the context of the formatter

## Value

`init_formatter()` returns a opaque structure capturing information used
by the other functions. `close_formatter()` may return a value that
should be used as response body. `with_formatter()` returns the result
of `expr`. `clean_formatter()` is called for it's side effects and
should only be called if `close_formatter()` never evaluated.
