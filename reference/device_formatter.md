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
# Create a png formatter using the default png device
device_formatter(png)
#> function (...) 
#> {
#>     provided_args <- names(enquos(...))
#>     dev_args <- fn_fmls_names(dev_open)
#>     extra_args <- setdiff(provided_args, dev_args)
#>     if (length(extra_args) != 0 && !"..." %in% dev_args) {
#>         cli::cli_abort("Provided arguments does not match arguments in {.fun {dev_name}}")
#>     }
#>     init_dev <- function() {
#>         output_file <- tempfile()
#>         dev_open(filename = output_file, ...)
#>         dev_id <- grDevices::dev.cur()
#>         list(path = output_file, dev = dev_id)
#>     }
#>     close_dev <- function(info) {
#>         grDevices::dev.set(info$dev)
#>         grDevices::dev.off()
#>         if (!file.exists(info$path)) {
#>             return(NULL)
#>         }
#>         con <- file(info$path, "rb")
#>         on.exit({
#>             close(con)
#>             unlink(info$path)
#>         }, add = TRUE)
#>         readBin(con, "raw", file.info(info$path)$size)
#>     }
#>     clean_dev <- function(info) {
#>         grDevices::dev.set(info$dev)
#>         grDevices::dev.off()
#>         unlink(info$path)
#>     }
#>     with_dev <- function(x, info) {
#>         promises::with_promise_domain(create_graphics_device_promise_domain(info$dev), 
#>             x)
#>     }
#>     structure(identity, init = init_dev, close = close_dev, clean = clean_dev, 
#>         with = with_dev, class = "device_formatter")
#> }
#> <bytecode: 0x557020b9c1a8>
#> <environment: 0x5570246cd918>
#> attr(,"class")
#> [1] "device_constructor"
```
