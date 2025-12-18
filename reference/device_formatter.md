# Create a graphics device formatter

This internal function facilitates creating a formatter that uses a
specific device for rendering.

## Usage

``` r
device_formatter(dev_open, dev_close = grDevices::dev.off())
```

## Arguments

- dev_open:

  The function that opens the device

- dev_close:

  The function closing the device. Usually this would be
  [`grDevices::dev.off()`](https://rdrr.io/r/grDevices/dev.html)

## Value

A device formatter function

## Examples

``` r
if (FALSE) {
# Create a png formatter using the default png device
device_formatter(png)
}
```
