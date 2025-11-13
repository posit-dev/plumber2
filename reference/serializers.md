# Serializer functions provided by plumber2

These functions cover a large area of potential response body formats.
They are all registered to their standard mime type but users may want
to use them to register them to alternative types if they know it makes
sense.

## Usage

``` r
format_csv(...)

format_tsv(...)

format_rds(version = "3", ascii = FALSE, ...)

format_geojson(...)

format_feather(...)

format_parquet(...)

format_yaml(...)

format_htmlwidget(...)

format_format(..., sep = "\n")

format_print(..., sep = "\n")

format_cat(..., sep = "\n")

format_unboxed(...)

format_png(...)

format_jpeg(...)

format_tiff(...)

format_svg(...)

format_bmp(...)

format_pdf(...)
```

## Arguments

- ...:

  Further argument passed on to the internal formatting function. See
  Details for information on which function handles the formatting
  internally in each serializer

- version:

  the workspace format version to use. `NULL` specifies the current
  default version (3). The only other supported value is 2, the default
  from R 1.4.0 to R 3.5.0.

- ascii:

  a logical. If `TRUE` or `NA`, an ASCII representation is written;
  otherwise (default) a binary one. See also the comments in the help
  for [`save`](https://rdrr.io/r/base/save.html).

- sep:

  The separator between multiple elements

## Value

A function accepting the response body

## Provided serializers

- `format_csv()` uses
  [`readr::format_csv()`](https://readr.tidyverse.org/reference/format_delim.html)
  for formatting. It is registered as `"csv"` to the mime type
  `text/csv`

- `format_tsv()` uses
  [`readr::format_tsv()`](https://readr.tidyverse.org/reference/format_delim.html)
  for formatting. It is registered as `"tsv"` to the mime type
  `text/tsv`

- `format_rds()` uses
  [`serialize()`](https://rdrr.io/r/base/serialize.html) for formatting.
  It is registered as `"rds"` to the mime type `application/rds`

- `format_geojson()` uses
  [`geojsonsf::sfc_geojson()`](https://rdrr.io/pkg/geojsonsf/man/sfc_geojson.html)
  or
  [`geojsonsf::sf_geojson()`](https://rdrr.io/pkg/geojsonsf/man/sf_geojson.html)
  for formatting depending on the class of the response body. It is
  registered as `"geojson"` to the mime type `application/geo+json`

- `format_feather()` uses
  [`arrow::write_feather()`](https://arrow.apache.org/docs/r/reference/write_feather.html)
  for formatting. It is registered as `"feather"` to the mime type
  `application/vnd.apache.arrow.file`

- `format_parquet()` uses
  [`nanoparquet::write_parquet()`](https://nanoparquet.r-lib.org/reference/write_parquet.html)
  for formatting. It is registered as `"parquet"` to the mime type
  `application/vnd.apache.parquet`

- `format_yaml()` uses
  [`yaml::as.yaml()`](https://rdrr.io/pkg/yaml/man/as.yaml.html) for
  formatting. It is registered as `"yaml"` to the mime type `text/yaml`

- `format_htmlwidget()` uses
  [`htmlwidgets::saveWidget()`](https://rdrr.io/pkg/htmlwidgets/man/saveWidget.html)
  for formatting. It is registered as `"htmlwidget"` to the mime type
  `text/html`

- `format_format()` uses
  [`format()`](https://rdrr.io/r/base/format.html) for formatting. It is
  registered as `"format"` to the mime type `text/plain`

- `format_print()` uses [`print()`](https://rdrr.io/r/base/print.html)
  for formatting. It is registered as `"print"` to the mime type
  `text/plain`

- `format_cat()` uses [`cat()`](https://rdrr.io/r/base/cat.html) for
  formatting. It is registered as `"cat"` to the mime type `text/plain`

- `format_unboxed()` uses
  [`reqres::format_json()`](https://reqres.data-imaginist.com/reference/formatters.html)
  with `auto_unbox = TRUE` for formatting. It is registered as
  `"unboxedJSON"` to the mime type `application/json`

### Additional registered serializers

- [`reqres::format_json()`](https://reqres.data-imaginist.com/reference/formatters.html)
  is registered as "`json`" to the mime type `application/json`

- [`reqres::format_html()`](https://reqres.data-imaginist.com/reference/formatters.html)
  is registered as "`html`" to the mime type `text/html`

- [`reqres::format_xml()`](https://reqres.data-imaginist.com/reference/formatters.html)
  is registered as "`xml`" to the mime type `text/xml`

- [`reqres::format_plain()`](https://reqres.data-imaginist.com/reference/formatters.html)
  is registered as "`text`" to the mime type `text/plain`

## Provided graphics serializers

Serializing graphic output is special because it requires operations
before and after the handler is executed. Further, handlers creating
graphics are expected to do so through side-effects (i.e. call to
graphics rendering) or by returning a ggplot2 object. If you want to
create your own graphics serializer you should use
[`device_formatter()`](https://plumber2.posit.co/reference/device_formatter.md)
for constructing it.

- `format_png()` uses
  [`ragg::agg_png()`](https://ragg.r-lib.org/reference/agg_png.html) for
  rendering. It is registered as `"png"` to the mime type `image/png`

- `format_jpeg()` uses
  [`ragg::agg_jpeg()`](https://ragg.r-lib.org/reference/agg_jpeg.html)
  for rendering. It is registered as `"jpeg"` to the mime type
  `image/jpeg`

- `format_tiff()` uses
  [`ragg::agg_tiff()`](https://ragg.r-lib.org/reference/agg_tiff.html)
  for rendering. It is registered as `"tiff"` to the mime type
  `image/tiff`

- `format_svg()` uses
  [`svglite::svglite()`](https://svglite.r-lib.org/reference/svglite.html)
  for rendering. It is registered as `"svg"` to the mime type
  `image/svg+xml`

- `format_bmp()` uses
  [`grDevices::bmp()`](https://rdrr.io/r/grDevices/png.html) for
  rendering. It is registered as `"bmp"` to the mime type `image/bmp`

- `format_pdf()` uses
  [`grDevices::pdf()`](https://rdrr.io/r/grDevices/pdf.html) for
  rendering. It is registered as `"pdf"` to the mime type
  `application/pdf`

## See also

[`register_serializer()`](https://plumber2.posit.co/reference/register_serializer.md)

## Examples

``` r
# You can use serializers directly when adding handlers
pa <- api() |>
  api_get("/hello/<name:string>", function(name) {
    list(
      msg = paste0("Hello ", name, "!")
    )
  }, serializers = list("application/json" = format_unboxed()))
#> Creating default route in request router
```
