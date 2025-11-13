# Parser functions provided by plumber2

These functions cover a large area of potential request body formats.
They are all registered to their standard mime types but users may want
to use them to register them to alternative types if they know it makes
sense.

## Usage

``` r
parse_csv(...)

parse_octet()

parse_rds(...)

parse_feather(...)

parse_parquet(...)

parse_text(multiple = FALSE)

parse_tsv(...)

parse_yaml(...)

parse_geojson(...)

parse_multipart(parsers = get_parsers())
```

## Arguments

- ...:

  Further argument passed on to the internal parsing function. See
  Details for information on which function handles the parsing
  internally in each parser

- multiple:

  logical: should the conversion be to a single character string or
  multiple individual characters?

- parsers:

  A list of parsers to use for parsing the parts of the body

## Value

A function accepting a raw vector along with a `directives` argument
that provides further directives from the `Content-Type` to be passed
along

## Provided parsers

- `parse_csv()` uses
  [`readr::read_csv()`](https://readr.tidyverse.org/reference/read_delim.html)
  for parsing. It is registered as `"csv"` for the mime types
  `application/csv`, `application/x-csv`, `text/csv`, and `text/x-csv`

- `parse_multipart` uses
  [`webutils::parse_multipart()`](https://jeroen.r-universe.dev/webutils/reference/parse_multipart.html)
  for the initial parsing. It then goes through each part and tries to
  find a parser that matches the content type (either given directly or
  guessed from the file extension provided). If a parser is not found it
  leaves the value as a raw vector. It is registered as "`multi`" for
  the mime type `multipart/*`

- `parse_octet()` passes the raw data through unchanged. It is
  registered as `"octet"` for the mime type `application/octet-stream`

- `parse_rds()` uses
  [`unserialize()`](https://rdrr.io/r/base/serialize.html) for parsing.
  It is registered as `"rds"` for the mime type `application/rds`

- `parse_feather()` uses
  [`arrow::read_feather()`](https://arrow.apache.org/docs/r/reference/read_feather.html)
  for parsing. It is registered as `"feather"` for the mime types
  `application/vnd.apache.arrow.file` and `application/feather`

- `parse_parquet()` uses
  [`arrow::read_parquet()`](https://arrow.apache.org/docs/r/reference/read_parquet.html)
  for parsing. It is registered as `"parquet"` for the mime type
  `application/vnd.apache.parquet`

- `parse_text()` uses
  [`rawToChar()`](https://rdrr.io/r/base/rawConversion.html) for
  parsing. It is registered as `"text"` for the mime types `text/plain`
  and `text/*`

- `parse_tsv()` uses
  [`readr::read_tsv()`](https://readr.tidyverse.org/reference/read_delim.html)
  for parsing. It is registered as `"tsv"` for the mime types
  `application/tab-separated-values` and `text/tab-separated-values`

- `parse_yaml()` uses
  [`yaml::yaml.load()`](https://rdrr.io/pkg/yaml/man/yaml.load.html) for
  parsing. It is registered as `"yaml"` for the mime types
  `text/vnd.yaml`, `application/yaml`, `application/x-yaml`,
  `text/yaml`, and `text/x-yaml`

- `parse_geojson()` uses
  [`geojsonsf::geojson_sf()`](https://rdrr.io/pkg/geojsonsf/man/geojson_sf.html)
  for parsing. It is registered as `"geojson"` for the mime types
  `application/geo+json` and `application/vdn.geo+json`

### Additional registered parsers

- [`reqres::parse_json()`](https://reqres.data-imaginist.com/reference/parsers.html)
  is registered as "`json`" for the mime types `application/json` and
  `text/json`

- [`reqres::parse_queryform()`](https://reqres.data-imaginist.com/reference/parsers.html)
  is registered as "`form`" for the mime type
  `application/x-www-form-urlencoded`

- [`reqres::parse_xml()`](https://reqres.data-imaginist.com/reference/parsers.html)
  is registered as "`xml`" for the mime types `application/xml` and
  `text/xml`

- [`reqres::parse_html()`](https://reqres.data-imaginist.com/reference/parsers.html)
  is registered as "`html`" for the mime type `text/html`

## See also

[`register_parser()`](https://plumber2.posit.co/reference/register_parser.md)

## Examples

``` r
# You can use parsers directly when adding handlers
pa <- api() |>
  api_post("/hello/<name:string>", function(name, body) {
    list(
      msg = paste0("Hello ", name, "!")
    )
  }, parsers = list("text/csv" = parse_csv()))
#> Creating default route in request router
```
